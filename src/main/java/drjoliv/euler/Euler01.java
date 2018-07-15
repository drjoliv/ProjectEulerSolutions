package drjoliv.euler;

import static drjoliv.jfunc.nums.Integers.range;

import drjoliv.jfunc.data.list.FList;
import drjoliv.jfunc.function.F0;
import drjoliv.jfunc.nums.Integers;

public class Euler01 implements F0<String> {

  public static int euler01() {
    FList<Integer> list = range(1,999)
                  .filter(i -> i % 3 == 0 || i % 5 == 0);
    return list.foldr(Integers::add, 0);
  }

  @Override
  public String call() {
    return String.valueOf(euler01());
  }
}
