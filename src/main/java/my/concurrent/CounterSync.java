package my.concurrent;

/**
 *  it automatically establishes a happens-before relationship with any subsequent invocation
 *  of a synchronized method for the same object
 */
public class CounterSync {
  private int c = 0;

  public synchronized void increment() {
    c++;
  }

  public synchronized void decrement() {
    c--;
  }

  public synchronized int value() {
    return c;
  }

}
