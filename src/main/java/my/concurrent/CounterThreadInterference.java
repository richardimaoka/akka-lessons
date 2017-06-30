package my.concurrent;


/**
 * Thread interference -> avoided by atomic access
 * because it is interleaving access to the shared data piece from multiple threads
 *
 * Also it can be avoided by:
 *
 *   sync method/object (synchronized method works like atomic, no interleaving invocation)
 *   or volatile, (Reads/writes are atomic for all variables declared volatile (including long and double variables))
 *
 * both of which establishes happens-before/after
 */
public class CounterThreadInterference {

  public static void main(String[] args) {
    try {
      CounterThreadUnsafe c = new CounterThreadUnsafe();
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
