package my.concurrent;

public class CounterThreadUnsafe {
  public int c = 0;

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
