package my.concurrent;

/**
 * Memory inconsistency errors can be avoided by volatile.
 * Atomic variables still do not avoid the problem.
 *
 * However ..... it is too difficult to reproduct a memory inconsistency error
 * with small line of codes. Probably stably reproducing it is nearly impossible...
 *
 * The below actually doesn't create memory inconsistency errors as far as I tested.
 */
public class MemoryInconsistency {

  static class MyThread extends Thread {
    CounterThreadUnsafe counter;
    int threadId;

    MyThread(int threadId, CounterThreadUnsafe counter){
      this.threadId = threadId;
      this.counter = counter;
    }

    public void run() {
      try{
        for(int i = 0; i < 100; i++) {
          int value = counter.value();
          System.out.println("Thread(" + String.format("%2d",threadId) + ") found counter = " + value);

          Thread.sleep(500);
        }
      }
      catch(InterruptedException e){
        System.out.println("Exception in Thread(" + threadId + ") " + e);
      }
    }
  }

  public static void main(String[] args) throws InterruptedException {
    CounterThreadUnsafe counter = new CounterThreadUnsafe();
    int upTo = 16;

    Thread[] arr = new Thread[upTo];

    for (int i = 1; i < upTo; i++ ){
      Thread t = new MyThread(i, counter);
      arr[i] = t;
      t.start();
    }

    Thread.sleep(100);
    System.out.println();

    for(int i = 0; i < 100; i++){
      //counter.increment();
      counter.c = i;
      Thread.sleep(500);
      System.out.println();
    }

    for (int i = 1; i < upTo; i++ ){
      arr[i].join();
    }
  }
}
