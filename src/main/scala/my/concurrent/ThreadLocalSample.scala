package my.concurrent

import java.util.concurrent.atomic.AtomicInteger

object ThreadId {
  // Atomic integer containing the next thread ID to be assigned
  private val nextId: AtomicInteger = new AtomicInteger(0)

  // Thread local variable containing each thread's ID
  private val threadId: ThreadLocal[Int] = new ThreadLocal[Int] {
    override def initialValue(): Int = {
      ThreadId.nextId.getAndIncrement()
    }
  }

  // Returns the current thread's unique ID, assigning it if necessary
  def get() = threadId.get()
}

class ThreadIdRunnable(threadLocal: ThreadLocal[Int]) extends Runnable {
  override def run(): Unit = {
    println(s"${threadLocal.get()} on ${Thread.currentThread()}")
    threadLocal.set(threadLocal.get() + 1)
    Thread.sleep(100)
    println(s"${threadLocal.get()} on ${Thread.currentThread()}")
    threadLocal.set(threadLocal.get() + 1)
    Thread.sleep(100)
    println(s"${threadLocal.get()} on ${Thread.currentThread()}")
    threadLocal.set(threadLocal.get() + 1)
    Thread.sleep(100)
    println(s"${threadLocal.get()} on ${Thread.currentThread()}")
  }
}

object  ThreadLocalSample {
  def main(args: Array[String]): Unit = {
    val tl = new ThreadLocal[Int]
    tl.set(1000)
    println(tl.get())
    val thread1 = new Thread(new ThreadIdRunnable(tl))
    val thread2 = new Thread(new ThreadIdRunnable(tl))

    thread1.start()
    thread2.start()
    thread1.join(1000)
    thread2.join(1000)
  }
}
