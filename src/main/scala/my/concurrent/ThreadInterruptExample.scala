package my.concurrent

import scala.util.Random


class InterruptableRunnable extends Runnable {
  override def run(): Unit = {
    try{
      Thread.sleep(1000)
      println(s"Hello I am a new runnable on ${Thread.currentThread()}")
    }
    catch {
      case e: InterruptedException =>
        println(e, s"I am interrupted on ${Thread.currentThread()} interrupted=${Thread.interrupted()}")
    }
  }
}

class InterruptableRunnable2 extends Runnable {

  var sum : Long = 0
  val random: Random = new Random()

  def heavyCrunch(): Unit = {
    for(i <- 1 to 10000)
      sum = sum + random.nextInt(100)
  }

  override def run(): Unit = {
    try {
      while (true) {
        heavyCrunch()
        if (Thread.interrupted()) {
          println(s"thread.interrupted = ${Thread.interrupted()}")
          throw new InterruptedException
        }
      }
    }
    catch {
      case e: InterruptedException =>
        println(s"Exception = ${e}, I am interrupted on ${Thread.currentThread()} interrupted=${Thread.interrupted()}")
    }
  }
}

object ThreadInterruptExample {
  def main(args: Array[String]): Unit = {

    val thread1 : Thread = new Thread(new InterruptableRunnable())
    thread1.start()
    thread1.interrupt()

    val thread2 : Thread = new Thread(new HelloRunnable())
    thread2.start()
    thread2.interrupt()

    val thread3 : Thread = new Thread(new InterruptableRunnable2())
    thread3.start()
    thread3.interrupt()

    println("main finished")
  }
}

