package drjoliv.euler;

import static drjoliv.jfunc.nums.Longs.range;

import drjoliv.jfunc.data.list.FList;
import drjoliv.jfunc.data.list.Functions;
import drjoliv.jfunc.function.F0;
import drjoliv.jfunc.function.F1;
import drjoliv.jfunc.nums.Longs;

public class Euler06 implements F0<String> {

  public static long euler06() {
    F1<FList<Long>, Long> sqrAndSum = F1.compose(Longs::sum, Functions.map(i -> i * i));
    F1<FList<Long>, Long> sumAndSqu = F1.compose(i -> i * i, Longs::sum);
    Long l = sqrAndSum.call(range(1L,10L));
    Long ll = sumAndSqu.call(range(1L,10L));
    return ll - l;
  }

  @Override
  public String call() {
    return String.valueOf(euler06());
  }

}
