package my.concurrent;

import java.util.concurrent.locks.ReentrantLock;

public class CounterReentrantLock implements CounterInterface {
  int c = 0;
  ReentrantLock lock = new ReentrantLock();

  public void setZero() { c = 0; }

  public void increment() {
    lock.lock();
    try {
      c++;
    } finally {
      lock.unlock();
    }
  }

  public void decrement() {
    lock.lock();
    try {
      c--;
    } finally {
      lock.unlock();
    }
  }

  public int value() {
    return c;
  }

}
