package my.concurrent

import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import java.util.Random


class Friend(val name: String, val lock: Lock = new ReentrantLock()) {

  //try to get locks of both bowee(myLock) and bower(yourLock)
  def impendingBow(bower: Friend): Boolean = {
    var myLock: Boolean = false
    var yourLock: Boolean = false
    try {
      myLock = lock.tryLock()
      yourLock = bower.lock.tryLock()
    } finally {
      if (!(myLock && yourLock)) {
        if (myLock) {
          lock.unlock()
        }
        if (yourLock) {
          bower.lock.unlock()
        }
      }
    }
    myLock && yourLock
  }

  def bow(bower: Friend ): Unit = {
    if (impendingBow(bower)) /*try to get locks of both bowee(this) and bower*/ {
      try {
        System.out.format("%s: %s has"
          + " bowed to me!%n",
          this.name, bower.name)
        bower.bowBack(this);
      } finally {
        lock.unlock()
        bower.lock.unlock()
      }
    } else {
      System.out.format("%s: %s started"
        + " to bow to me, but saw that"
        + " I was already bowing to"
        + " him.%n",
        this.name, bower.name)
    }
  }

  def bowBack(bower: Friend): Unit = {
    System.out.format("%s: %s has" + " bowed back to me!%n", this.name, bower.name);
  }
}

class BowLoop(val bower: Friend, val bowee: Friend) extends Runnable {
  override def run(): Unit = {
    val random: Random  = new Random()
    for(x <- 1 to 100)
    {
      try {
        Thread.sleep(random.nextInt(10));
      } catch {
        case e: InterruptedException =>
          println(e)
      }
      bowee.bow(bower)
    }
  }
}

object Safelock {
  def main(args: Array[String]) {
    val alphonse: Friend  = new Friend("Alphonse")
    val gaston: Friend = new Friend("Gaston")
    new Thread(new BowLoop(alphonse, gaston)).start()
    new Thread(new BowLoop(gaston, alphonse)).start()
  }
}