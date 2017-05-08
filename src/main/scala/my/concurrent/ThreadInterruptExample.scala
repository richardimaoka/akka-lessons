package my.concurrent

class HelloRunnable extends Runnable {
  override def run(): Unit = {
    Thread.sleep(1000)
    println(s"Hello I am a new runnable on ${Thread.currentThread()}")
  }
}

object ThreadInterruptExample {
  def main(args: Array[String]): Unit = {

    val thread1 : Thread = new Thread(new HelloRunnable())
    thread1.start()
    thread1.interrupt()

    println("main finished")
  }
}

