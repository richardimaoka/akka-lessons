package my.concurrent;

public class CounterMemoryInconcsistency {

  static class MyThread extends Thread {
    CounterThreadUnsafe counter;
    int threadId;

    MyThread(int threadId, CounterThreadUnsafe counter){
      this.threadId = threadId;
      this.counter = counter;
    }

    public void run() {
      try{
        Thread.sleep(threadId);
        counter.increment();
        int value = counter.value();

        if(value != threadId)
          System.out.println("Thread(" + threadId+ ") found counter = " + value);
      }
      catch(InterruptedException e){
        System.out.println("Exception in Thread(" + threadId + ") " + e);
      }
    }
  }

  public static void main(String[] args) throws InterruptedException {
    CounterThreadUnsafe counter = new CounterThreadUnsafe();
    int upTo = 3000;

    Thread[] arr = new Thread[upTo];

    for (int i = 1; i < upTo; i++ ){
      Thread t = new MyThread(i, counter);
      arr[i] = t;
      t.start();
    }

    for (int i = 1; i < upTo; i++ ){
      arr[i].join();
    }

    System.out.println("The resulting counter = " + counter.value() + ", expected = " + (upTo - 1) /* - 1 as counter starts from 0 */ );
  }
}
