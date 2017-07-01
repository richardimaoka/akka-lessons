package my.stream

import java.io.{IOException, InputStream}
import java.util.concurrent._

import akka.actor.ActorSystem
import akka.stream.Attributes.{InputBuffer, inputBuffer}
import akka.stream.scaladsl.{Keep, Sink, Source, StreamConverters}
import akka.stream.stage.{AsyncCallback, GraphStageLogic, GraphStageWithMaterializedValue, InHandler}
import akka.stream.testkit._
import akka.stream.testkit.scaladsl.TestSource
import akka.stream.{ActorMaterializerSettings, _}
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import my.stream.InputStreamSinkStage._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.control.NoStackTrace

object InputStreamSinkStage {

  sealed trait AdapterToStageMessage
  case object ReadElementAcknowledgement extends AdapterToStageMessage
  case object Close extends AdapterToStageMessage

  sealed trait StreamToAdapterMessage
  case class Data(data: ByteString) extends StreamToAdapterMessage
  case object Finished extends StreamToAdapterMessage
  case object Initialized extends StreamToAdapterMessage
  case class Failed(cause: Throwable) extends StreamToAdapterMessage

  sealed trait StageWithCallback {
    def wakeUp(msg: AdapterToStageMessage): Unit
  }

  def asInputStream(readTimeout: FiniteDuration = 5.seconds): Sink[ByteString, InputStream] =
    Sink.fromGraph(new InputStreamSinkStage(readTimeout))
}

/**
 * Creates a Sink which when materialized will return an [[InputStream]] which it is possible
 * to read the values produced by the stream this Sink is attached to.
 *
 * This Sink is intended for inter-operation with legacy APIs since it is inherently blocking.
 *
 * You can configure the default dispatcher for this Source by changing the `akka.stream.blocking-io-dispatcher` or
 * set it for a given Source by using [[akka.stream.ActorAttributes]].
 *
 * The [[InputStream]] will be closed when the stream flowing into this [[Sink]] completes, and
 * closing the [[InputStream]] will cancel this [[Sink]].
 *
 * @param readTimeout the max time the read operation on the materialized InputStream should block
 */
class InputStreamSinkStage(readTimeout: FiniteDuration) extends GraphStageWithMaterializedValue[SinkShape[ByteString], InputStream] {

  val in = Inlet[ByteString]("InputStreamSink.in")
  override def initialAttributes: Attributes = Attributes.name("InputStreamSinkStage")
  override val shape: SinkShape[ByteString] = SinkShape.of(in)

  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, InputStream) = {
    // This is just the buffer size, which is used by the actual dataQueue later
    val maxBuffer = inheritedAttributes.getAttribute(classOf[InputBuffer], InputBuffer(16, 16)).max
    require(maxBuffer > 0, "Buffer size must be greater than 0")

    /**
     * See how InputStreamAdapter's dataQueue: BlockingQueue[ByteString] uses it to
     * block the materialized Java OutputStream.write() method
     *
     * dataQueue.put() blocks if this exact Queue is full
     *
     * Interesting - the type parameter is StreamToAdapterMessage, not ByteString,
     * not only ByteString but also Failed, Finished, and Initialized status (?) messages are added
     */
    val dataQueue = new LinkedBlockingDeque[StreamToAdapterMessage](maxBuffer + 2)

    val logic = new GraphStageLogic(shape) with StageWithCallback with InHandler {

      var completionSignalled = false

      // getAsyncCallback returns an AsyncCallback instance,
      // which allows external threads call the body of getAsyncCallback{} via AsyncCallback.invoke()
      private val callback: AsyncCallback[AdapterToStageMessage] =
        getAsyncCallback {
          case ReadElementAcknowledgement ⇒ sendPullIfAllowed()
          case Close                      ⇒ completeStage()
        }

      /**
       * Used in OutputStreamAdapter's sendToStage(), which is called from flush() and close()
       */
      override def wakeUp(msg: AdapterToStageMessage): Unit = callback.invoke(msg)

      private def sendPullIfAllowed(): Unit = {
        println("sendPullIfAllowed()")
        if (dataQueue.remainingCapacity() > 1 && !hasBeenPulled(in)){
          println("pull(in) from sendPullIfAllowed()")
          pull(in)
        }
      }

      override def preStart() = {
        println("preStart(), calling dataQueue.add(Initialized) and pull(in)")
        dataQueue.add(Initialized)
        pull(in)
      }

      def onPush(): Unit = {
        //1 is buffer for Finished or Failed callback
        require(dataQueue.remainingCapacity() > 1)
        val elem = grab(in)
        println(s"onPush(${elem})")
        dataQueue.add(Data(elem))
        if (dataQueue.remainingCapacity() > 1) sendPullIfAllowed()
      }

      override def onUpstreamFinish(): Unit = {
        dataQueue.add(Finished)
        completionSignalled = true
        completeStage()
      }

      override def onUpstreamFailure(ex: Throwable): Unit = {
        dataQueue.add(Failed(ex))
        completionSignalled = true
        failStage(ex)
      }

      override def postStop(): Unit = {
        if (!completionSignalled) dataQueue.add(Failed(new AbruptStageTerminationException(this)))
      }

      setHandler(in, this)

    }

    (logic, new InputStreamAdapter(dataQueue, logic.wakeUp, readTimeout))
  }
}

/**
 * InputStreamAdapter that interacts with InputStreamSinkStage
 */
class InputStreamAdapter(
                          sharedBuffer: BlockingQueue[StreamToAdapterMessage],
                          sendToStage:  (AdapterToStageMessage) ⇒ Unit,
                          readTimeout:  FiniteDuration) extends InputStream {

  var isInitialized = false
  var isActive = true
  var isStageAlive = true
  val subscriberClosedException = new IOException("Reactive stream is terminated, no reads are possible")
  var detachedChunk: Option[ByteString] = None

  @scala.throws(classOf[IOException])
  private[this] def executeIfNotClosed[T](f: () ⇒ T): T =
    if (isActive) {
      waitIfNotInitialized()
      f()
    } else throw subscriberClosedException

  @scala.throws(classOf[IOException])
  override def read(): Int = {
    val a = Array[Byte](1)
    if (read(a, 0, 1) != -1) a(0) & 0xff
    else -1
  }

  @scala.throws(classOf[IOException])
  override def read(a: Array[Byte]): Int = read(a, 0, a.length)

  @scala.throws(classOf[IOException])
  override def read(a: Array[Byte], begin: Int, length: Int): Int = {
    require(a.length > 0, "array size must be >= 0")
    require(begin >= 0, "begin must be >= 0")
    require(length > 0, "length must be > 0")
    require(begin + length <= a.length, "begin + length must be smaller or equal to the array length")

    executeIfNotClosed(() ⇒
      if (isStageAlive) {
        detachedChunk match {
          case None ⇒
            try {
              /**
               * sharedBuffer: BlockingQueue[StreamToAdapterMessage]
               * So this must be a blocking call
               */
              sharedBuffer.poll(readTimeout.toMillis, TimeUnit.MILLISECONDS) match {
                case Data(data) ⇒
                  detachedChunk = Some(data)
                  readBytes(a, begin, length)
                case Finished ⇒
                  isStageAlive = false
                  -1
                case Failed(ex) ⇒
                  isStageAlive = false
                  throw new IOException(ex)
                case null        ⇒ throw new IOException("Timeout on waiting for new data")
                case Initialized ⇒ throw new IllegalStateException("message 'Initialized' must come first")
              }
            } catch {
              case ex: InterruptedException ⇒ throw new IOException(ex)
            }
          case Some(data) ⇒
            readBytes(a, begin, length)
        }
      } else -1)
  }

  private[this] def readBytes(a: Array[Byte], begin: Int, length: Int): Int = {
    require(detachedChunk.nonEmpty, "Chunk must be pulled from shared buffer")
    val availableInChunk = detachedChunk.get.size
    val readBytes = getData(a, begin, length, 0)
    if (readBytes >= availableInChunk) sendToStage(ReadElementAcknowledgement)
    readBytes
  }

  @scala.throws(classOf[IOException])
  override def close(): Unit = {
    executeIfNotClosed(() ⇒ {
      // at this point Subscriber may be already terminated
      if (isStageAlive) sendToStage(Close)
      isActive = false
    })
  }

  @tailrec
  private[this] def getData(arr: Array[Byte], begin: Int, length: Int,
                            gotBytes: Int): Int = {
    grabDataChunk() match {
      case Some(data) ⇒
        val size = data.size
        if (size <= length) {
          data.copyToArray(arr, begin, size)
          detachedChunk = None
          if (size == length)
            gotBytes + size
          else
            getData(arr, begin + size, length - size, gotBytes + size)
        } else {
          data.copyToArray(arr, begin, length)
          detachedChunk = Some(data.drop(length))
          gotBytes + length
        }
      case None ⇒ gotBytes
    }
  }

  private[this] def waitIfNotInitialized(): Unit = {
    if (!isInitialized) {
      sharedBuffer.poll(readTimeout.toMillis, TimeUnit.MILLISECONDS) match {
        case Initialized ⇒ isInitialized = true
        case null        ⇒ throw new IOException(s"Timeout after $readTimeout waiting for Initialized message from stage")
        case entry       ⇒ require(false, s"First message must be Initialized notification, got $entry")
      }
    }
  }

  private[this] def grabDataChunk(): Option[ByteString] = {
    detachedChunk match {
      case None ⇒
        sharedBuffer.poll() match {
          case Data(data) ⇒
            detachedChunk = Some(data)
            detachedChunk
          case Finished ⇒
            isStageAlive = false
            None
          case _ ⇒ None
        }
      case Some(_) ⇒ detachedChunk
    }
  }
}


class InputStreamSinkSpec extends TestKit(ActorSystem("InputStreamSinkSpec")) with WordSpecLike with Matchers with BeforeAndAfterAll {
  import system.dispatcher

  val UnboundedMailboxConfig = ConfigFactory.parseString("""akka.actor.default-mailbox.mailbox-type = "akka.dispatch.UnboundedMailbox"""")

  val settings = ActorMaterializerSettings(system).withDispatcher("akka.actor.default-dispatcher")
  implicit val materializer = ActorMaterializer(settings)

  val timeout = 300.milliseconds
  def randomByteString(size: Int): ByteString = {
    val a = new Array[Byte](size)
    ThreadLocalRandom.current().nextBytes(a)
    ByteString(a)
  }

  val byteString = randomByteString(3)
  val byteArray = byteString.toArray

  def readN(is: InputStream, n: Int): (Int, ByteString) = {
    val buf = new Array[Byte](n)
    val r = is.read(buf)
    (r, ByteString.fromArray(buf, 0, r))
  }
  def testSink(probe: TestProbe) = TestSinkStage(new InputStreamSinkStage(timeout), probe)

  "InputStreamSink" must {
    println("\nread bytes from InputStream" + "----------------------------------------------------")
    "read bytes from InputStream" in {
      val inputStream = Source.single(byteString).runWith(InputStreamSinkStage.asInputStream())
      readN(inputStream, byteString.size) should ===((byteString.size, byteString))
      inputStream.close()
    }

    "read bytes correctly if requested by InputStream not in chunk size" in {
      println("\nread bytes correctly if requested by InputStream not in chunk size" + "----------------------------------------------------")
      val sinkProbe = TestProbe()
      val byteString2 = randomByteString(3)
      val inputStream = Source(byteString :: byteString2 :: Nil)
        .runWith(testSink(sinkProbe))

      /**
       * Interesting features using TestSinkStage
       */
      sinkProbe.expectMsgAllOf(GraphStageMessages.Push, GraphStageMessages.Push)

      println()
      println(s"byteString  = ${byteString}")
      println(s"byteString2 = ${byteString2}")

      readN(inputStream, 2) should ===((2, byteString.take(2)))
      println(s"byteString.take(2) = ${byteString.take(2)}")

      readN(inputStream, 2) should ===((2, byteString.drop(2) ++ byteString2.take(1)))
      println(s"byteString.drop(2) ++ byteString2.take(1) = ${byteString.drop(2) ++ byteString2.take(1)}")

      readN(inputStream, 2) should ===((2, byteString2.drop(1)))
      println(s"byteString2.drop(1) = ${byteString2.drop(1)}")

      inputStream.close()
    }

    "returns less than was expected when the data source has provided some but not enough data" in  {
      println("\nreturns less than was expected when the data source has provided some but not enough data"  + "----------------------------------------------------")
      val inputStream = Source.single(byteString).runWith(InputStreamSinkStage.asInputStream())

      val arr = new Array[Byte](byteString.size + 1)
      inputStream.read(arr) should ===(arr.size - 1) //total number of bytes read in arr, arr.size - 1 == byteString.size
      ByteString(arr) should ===(byteString :+ 0)

      inputStream.close()
    }

    "block read until get requested number of bytes from upstream" in {
      println("\nblock read until get requested number of bytes from upstream" + "----------------------------------------------------")
      val (probe, inputStream) = TestSource.probe[ByteString].toMat(InputStreamSinkStage.asInputStream())(Keep.both).run()
      val f = Future(inputStream.read(new Array[Byte](byteString.size)))

      the[Exception] thrownBy Await.result(f, timeout) shouldBe a[TimeoutException]
      probe.sendNext(byteString)
      Await.result(f, remainingOrDefault) should ===(byteString.size)

      probe.sendComplete()
      inputStream.read() should ===(-1)
      inputStream.close()
    }

    "fill up buffer by default" in {
      println("\nfill up buffer by default"  + "----------------------------------------------------")
      val byteString2 = randomByteString(3)
      val inputStream = Source(byteString :: byteString2 :: Nil).runWith(InputStreamSinkStage.asInputStream())

      readN(inputStream, 3) should ===((3, byteString))
      readN(inputStream, 3) should ===((3, byteString2))

      inputStream.close()
    }

    "throw error when reactive stream is closed" in {
      println("\nthrow error when reactive stream is closed"  + "----------------------------------------------------")
      val (probe, inputStream) = TestSource.probe[ByteString].toMat(InputStreamSinkStage.asInputStream())(Keep.both).run()
      probe.sendNext(byteString)
      inputStream.close()
      probe.expectCancellation()
      the[Exception] thrownBy inputStream.read() shouldBe a[IOException]
    }

    "return all data when upstream is completed" in {
      println("\nreturn all data when upstream is completed"  + "----------------------------------------------------")
      val sinkProbe = TestProbe()

      val (probe, inputStream) = TestSource.probe[ByteString]
        .toMat(testSink(sinkProbe))(Keep.both).run()
      val bytes = randomByteString(1)

      probe.sendNext(bytes)
      sinkProbe.expectMsg(GraphStageMessages.Push)

      probe.sendComplete()
      sinkProbe.expectMsg(GraphStageMessages.UpstreamFinish)

      readN(inputStream, 3) should ===((1, bytes))
    }

    "work when read chunks smaller than stream chunks" in {
      println("\nwork when read chunks smaller than stream chunks"  + "----------------------------------------------------")
      val bytes = randomByteString(10)
      val inputStream = Source.single(bytes).runWith(InputStreamSinkStage.asInputStream())

      for (expect ← bytes.sliding(3, 3))
        readN(inputStream, 3) should ===((expect.size, expect))

      inputStream.close()
    }

    "throw exception when call read with wrong parameters" in {
      println("\nthrow exception when call read with wrong parameters"  + "----------------------------------------------------")
      val inputStream = Source.single(byteString).runWith(InputStreamSinkStage.asInputStream())
      val buf = new Array[Byte](3)
      an[IllegalArgumentException] shouldBe thrownBy(inputStream.read(buf, -1, 2))
      an[IllegalArgumentException] shouldBe thrownBy(inputStream.read(buf, 0, 5))
      an[IllegalArgumentException] shouldBe thrownBy(inputStream.read(new Array[Byte](0), 0, 1))
      an[IllegalArgumentException] shouldBe thrownBy(inputStream.read(buf, 0, 0))
      inputStream.close()
    }

    "successfully read several chunks at once" in {
      println("\nsuccessfully read several chunks at once"  + "----------------------------------------------------")
      val bytes = List.fill(4)(randomByteString(4))
      val sinkProbe = TestProbe()
      val inputStream = Source[ByteString](bytes)
        .runWith(testSink(sinkProbe))

      //need to wait while all elements arrive to sink
      bytes foreach { _ ⇒ sinkProbe.expectMsg(GraphStageMessages.Push) }

      for (i ← 0 to 1)
        readN(inputStream, 8) should ===((8, bytes(i * 2) ++ bytes(i * 2 + 1)))

      inputStream.close()
    }

    "work when read chunks bigger than stream chunks" in {
      println("\nwork when read chunks bigger than stream chunks"  + "----------------------------------------------------")
      val bytes1 = randomByteString(10)
      val bytes2 = randomByteString(10)
      val sinkProbe = TestProbe()
      val inputStream = Source(bytes1 :: bytes2 :: Nil).runWith(testSink(sinkProbe))

      //need to wait while both elements arrive to sink
      sinkProbe.expectMsgAllOf(GraphStageMessages.Push, GraphStageMessages.Push)

      readN(inputStream, 15) should ===((15, bytes1 ++ bytes2.take(5)))
      readN(inputStream, 15) should ===((5, bytes2.drop(5)))

      inputStream.close()
    }

    "return -1 when read after stream is completed" in {
      println("\nreturn -1 when read after stream is completed"  + "----------------------------------------------------")
      val inputStream = Source.single(byteString).runWith(InputStreamSinkStage.asInputStream())

      readN(inputStream, byteString.size) should ===((byteString.size, byteString))
      inputStream.read() should ===(-1)

      inputStream.close()
    }

    "return IOException when stream is failed" in {
      println("\nreturn IOException when stream is failed" + "----------------------------------------------------")
      val sinkProbe = TestProbe()
      val (probe, inputStream) = TestSource.probe[ByteString].toMat(testSink(sinkProbe))(Keep.both).run()
      val ex = new RuntimeException("Stream failed.") with NoStackTrace

      probe.sendNext(byteString)
      sinkProbe.expectMsg(GraphStageMessages.Push)

      readN(inputStream, byteString.size) should ===((byteString.size, byteString))

      probe.sendError(ex)
      sinkProbe.expectMsg(GraphStageMessages.Failure(ex))
      val e = intercept[IOException] { Await.result(Future(inputStream.read()), timeout) }
      e.getCause should ===(ex)
    }

    //    "use dedicated default-blocking-io-dispatcher by default" in {
    //      val sys = ActorSystem("dispatcher-testing", UnboundedMailboxConfig)
    //      val materializer = ActorMaterializer()(sys)
    //      try {
    //        TestSource.probe[ByteString].runWith(InputStreamSinkStage.asInputStream())(materializer)
    //        materializer.asInstanceOf[PhasedFusingActorMaterializer].supervisor.tell(StreamSupervisor.GetChildren, testActor)
    //        val ref = expectMsgType[Children].children.find(_.path.toString contains "inputStreamSink").get
    //        assertDispatcher(ref, "akka.stream.default-blocking-io-dispatcher")
    //      } finally shutdown(sys)
    //    }

    "work when more bytes pulled from InputStream than available" in {
      println("\nwork when more bytes pulled from InputStream than available"  + "----------------------------------------------------")
      val inputStream = Source.single(byteString).runWith(InputStreamSinkStage.asInputStream())

      readN(inputStream, byteString.size * 2) should ===((byteString.size, byteString))
      inputStream.read() should ===(-1)

      inputStream.close()
    }

    "read next byte as an int from InputStream" in {
      println("\nread next byte as an int from InputStream"  + "----------------------------------------------------")
      val bytes = ByteString(0, 100, 200, 255)
      val inputStream = Source.single(bytes).runWith(InputStreamSinkStage.asInputStream())
      List.fill(5)(inputStream.read()) should ===(List(0, 100, 200, 255, -1))
      inputStream.close()
    }

    "fail to materialize with zero sized input buffer" in {
      println("\nfail to materialize with zero sized input buffer" + "----------------------------------------------------")
      an[IllegalArgumentException] shouldBe thrownBy {
        Source.single(byteString)
          .runWith(StreamConverters.asInputStream(timeout).withAttributes(inputBuffer(0, 0)))
        /*
         With Source.single we test the code path in which the sink
         itself throws an exception when being materialized. If
         Source.empty is used, the same exception is thrown by
         Materializer.
         */
      }
    }

    "throw from inputstream read if terminated abruptly" in {
      println("\nthrow from inputstream read if terminated abruptly"  + "----------------------------------------------------")
      val mat = ActorMaterializer()
      val probe = TestPublisher.probe[ByteString]()
      val inputStream = Source.fromPublisher(probe).runWith(InputStreamSinkStage.asInputStream())(mat)
      mat.shutdown()

      intercept[IOException] {
        inputStream.read()
      }
    }
  }

}
