package drjoliv.euler;

import static drjoliv.jfunc.nums.Longs.*;

import drjoliv.jfunc.function.F0;
import drjoliv.jfunc.nums.Longs;

public class Euler05 implements F0<String>{
  public static long euler05() {
    return range(1L,20L).foldr(Longs::lcm,1L);
  }

  @Override
  public String call() {
    return String.valueOf(euler05());
  }
}
