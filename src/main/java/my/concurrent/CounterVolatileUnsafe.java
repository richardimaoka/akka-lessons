package my.concurrent;

/**
 * This is NOT actually thread-safe, because
 * ++ and -- operations are NOT atomic even with volatile int
 *
 * https://docs.oracle.com/javase/tutorial/essential/concurrency/atomic.html
 * Reads and writes are atomic for all variables declared volatile (including long and double variables).
 *
 * ++ and -- are not actually single write operation
 */
public class CounterVolatileUnsafe implements CounterInterface {
  private volatile int c = 0;

  public void setZero() { c = 0; }

  public void increment() {
    c++;
  }

  public void decrement() {
    c--;
  }

  public int value() {
    return c;
  }

}
