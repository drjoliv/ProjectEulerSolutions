package drjoliv.euler;

import drjoliv.jfunc.function.F0;
import drjoliv.jfunc.nums.Numbers;

public class Euler07 implements F0<String> {
  public static long euler07() {
    return Numbers.primes.get(10000);
  }

  @Override
  public String call() {
    return String.valueOf(euler07());
  }
}
