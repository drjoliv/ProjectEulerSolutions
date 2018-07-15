package drjoliv.euler;

import drjoliv.jfunc.data.list.FList;
import drjoliv.jfunc.nums.Longs;
import drjoliv.jfunc.function.F0;
import drjoliv.jfunc.nums.Numbers;

public class Euler10 implements F0<String> {

  public static long euler10() {
    FList<Long> p = Numbers.primes
      .takeWhile(i -> i < 2000000L);
    return Longs.sum(p);
  }

  @Override
  public String call() {
    return String.valueOf(euler10());
  }
}
