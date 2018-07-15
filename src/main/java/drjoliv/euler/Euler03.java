package drjoliv.euler;

import drjoliv.jfunc.function.F0;

import drjoliv.jfunc.data.list.FList;
import drjoliv.jfunc.nums.Numbers;

public final class Euler03 implements F0<String> {

  public static long euler03() {
   FList<Long> primes = Numbers.primeFactors(600851475143L);
   return primes.maximum(Long::compare);
  }

  @Override
  public String call() {
    return String.valueOf(euler03());
  }
}
