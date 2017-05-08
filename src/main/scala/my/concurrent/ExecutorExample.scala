package my.concurrent

import java.io.IOException
import java.net.{ServerSocket, Socket}
import java.util.concurrent.{Executor, ExecutorService, Executors, TimeUnit}

/****************************************************************************************
* https://docs.oracle.com/javase/tutorial/essential/concurrency/exinter.html
*****************************************************************************************/

class WeeRunnable extends Runnable {
  override def run: Unit = {
    println(this, Thread.currentThread(), "i am here")
  }
}

object ExecutorExample {
  def main(args: Array[String]) {
    val thread: Thread = new Thread(new WeeRunnable)
    thread.start()

    //ExecutorService implements Executor, so it is also a type of Executor,
    //but to call shutdown() it needs to be ExecutorService
    val executor: ExecutorService = Executors.newFixedThreadPool(10)
    executor.execute(new WeeRunnable)

    Thread.sleep(1200)
    executor.shutdown() //Without this the app hangs
  }
}


/****************************************************************************************
* https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ExecutorService.html
*****************************************************************************************/

class NetworkService(port: Int, poolSize: Int) extends Runnable {
  val serverSocket: ServerSocket = new ServerSocket(port)
  val pool: ExecutorService = Executors.newFixedThreadPool(poolSize)


  override def run(): Unit = { // run the service
    try {
      for(x <- 1 to 100) {
        pool.execute(new Handler(serverSocket.accept()));
      }
    } catch {
      case ex: IOException => pool.shutdown();
    }
  }

}

class Handler(socket: Socket) extends Runnable {
  override def run(): Unit = {
    // read and service request on socket
  }
}

object NetworkService {
  def shutdownAndAwaitTermination(pool: ExecutorService) {
    pool.shutdown(); // Disable new tasks from being submitted
    try {
      // Wait a while for existing tasks to terminate
      if (!pool.awaitTermination(60, TimeUnit.SECONDS)) {
        pool.shutdownNow(); // Cancel currently executing tasks
        // Wait a while for tasks to respond to being cancelled
        if (!pool.awaitTermination(60, TimeUnit.SECONDS))
          System.err.println("Pool did not terminate");
      }
    } catch (InterruptedException ie) {
      // (Re-)Cancel if current thread also interrupted
      pool.shutdownNow();
      // Preserve interrupt status
      Thread.currentThread().interrupt();
    }
  }
}

