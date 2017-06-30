package my.concurrent;


/**
 * Thread interference -> 1. avoided by atomic access
 * because it is interleaving access to the shared data piece from multiple threads
 *
 * Also it can be avoided by:
 *
 *   2. sync method/object (synchronized method works like atomic, no interleaving invocation)
 *   3. or volatile, (Reads/writes are atomic for all variables declared volatile (including long and double variables))
 *
 * both of which establishes happens-before/after
 *
 *
 *
 * HOWEVER the below example is NOT actually thread-safe, because
 * ++ and -- operations are NOT atomic even with volatile int
 *
 * https://docs.oracle.com/javase/tutorial/essential/concurrency/atomic.html
 * Reads and writes are atomic for all variables declared volatile (including long and double variables).
 *
 * ++ and -- are not actually single write operation

 */
public class ThreadInterferenceVolatile {

  public static void main(String[] args) {
    try {
      CounterVolatileUnsafe c = new CounterVolatileUnsafe();
      int loopUpTo = 250000;

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

      t1.start();
      t2.start();
      t1.join();
      t2.join();

      System.out.println( "Result = " + c.value() + ", expected = " + loopUpTo*2 );
    }
    catch(InterruptedException e) {
      System.out.println(e);
    }
  }
}
