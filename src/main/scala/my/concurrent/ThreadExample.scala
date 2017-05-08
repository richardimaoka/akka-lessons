package my.concurrent

class HelloRunnable extends Runnable {
  override def run(): Unit = {
    Thread.sleep(1000)
    println(s"Hello I am a new runnable on ${Thread.currentThread()}")
  }
}

class HelloThread extends Thread {
  override def run(): Unit = {
    Thread.sleep(500)
    println(s"Hello I am a new thread ${Thread.currentThread()}")
  }
}

object ThreadExample {
  def main(args: Array[String]): Unit = {

    val thread1 : Thread = new Thread(new HelloRunnable())
    thread1.start()

    val thread2 : Thread = new HelloThread()
    thread2.start()

//    thread1.join()
//    thread2.join()
    println("main finished")
  }
}
