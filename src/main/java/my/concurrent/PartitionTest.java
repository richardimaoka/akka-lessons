package my.concurrent;

import akka.stream.javadsl.Partition;
import akka.stream.javadsl.Source;

public class PartitionTest {

  public static void main(String[] args) {

    //Source.single("hello").via(Partition.create(10, t -> 20));
  }
}
