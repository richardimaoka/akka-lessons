package my.concurrent;

public class ThreadInterferencePerformance {

  public static void test(CounterInterface c){
    int loopUpTo = 10000000;
    c.setZero();

    Thread t1 = new Thread(new Runnable() {
      public void run() {
        for(int i=0; i<loopUpTo; i++)
          c.increment();
      }
    });

    Thread t2 = new Thread(new Runnable() {
      public void run() {
        for(int i=0; i<loopUpTo; i++)
          c.increment();
      }
    });

    try{
      long timeBefore = System.currentTimeMillis();
      t1.start();
      t2.start();
      t1.join();
      t2.join();
      long timeAfter = System.currentTimeMillis();

      System.out.println(
        (timeAfter - timeBefore) + " milliseconds taken by "  + c.getClass() +
        ", result = " + c.value() + ", expected = " + loopUpTo*2
      );
    }
    catch(InterruptedException e) {
      System.out.println(e);
    }
  }

  public static void testForThreads(CounterInterface c, int threadsUpTo){
    try{
      int loopUpTo = 10000000;
      c.setZero();

      Thread[] ts = new Thread[threadsUpTo];
      for(int i=0; i<threadsUpTo;i++){
        Thread t = new Thread(new Runnable() {
          public void run() {
            for(int i=0; i<loopUpTo; i++)
              c.increment();
          }
        });
        
        ts[i] = t;      
      }
  
      long timeBefore = System.currentTimeMillis();
  
      for(int i=0; i<threadsUpTo;i++)
        ts[i].start();
      for(int i=0; i<threadsUpTo;i++)
        ts[i].join();
  
      long timeAfter = System.currentTimeMillis();
    
      System.out.println(
        (timeAfter - timeBefore) + " milliseconds taken for " + threadsUpTo + " number of threads by "  + c.getClass() +
          ", result = " + c.value() + ", expected = " + loopUpTo*threadsUpTo
      );
    }
    catch(InterruptedException e) {
      System.out.println(e);
    }
  }

  public static void main(String[] args) {
    CounterAtomic        c1 = new CounterAtomic();
    CounterReentrantLock c2 = new CounterReentrantLock();
    CounterSync          c3 = new CounterSync();

    test(c3);
    test(c3);
    test(c3);
    test(c3);
    test(c3);
    System.out.println();
    test(c1);
    test(c1);
    test(c1);
    test(c1);
    test(c1);
    System.out.println();
    test(c2);
    test(c2);
    test(c2);
    test(c2);
    test(c2);
    System.out.println();
    test(c3);
    test(c3);
    test(c3);
    test(c3);
    test(c3);
    System.out.println();

    testForThreads(c1, 2);
    testForThreads(c1, 3);
    testForThreads(c1, 4);
    testForThreads(c1, 5);
    testForThreads(c1, 6);
    testForThreads(c1, 7);
    testForThreads(c1, 8);
    System.out.println();

    testForThreads(c2, 2);
    testForThreads(c2, 3);
    testForThreads(c2, 4);
    testForThreads(c2, 5);
    testForThreads(c2, 6);
    testForThreads(c2, 7);
    testForThreads(c2, 8);
    System.out.println();

    testForThreads(c3, 2);
    testForThreads(c3, 3);
    testForThreads(c3, 4);
    testForThreads(c3, 5);
    testForThreads(c3, 6);
    testForThreads(c3, 7);
    testForThreads(c3, 8);
    System.out.println();
  }
}
