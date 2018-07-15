package drjoliv.euler;

import drjoliv.jfunc.data.list.FList;
import drjoliv.jfunc.function.F0;
import drjoliv.jfunc.nums.Integers;

public class Euler02 implements F0<String> {
  public static int euler02() {
    FList<Integer> list = FList.sequence(1,2, Integers::add)
      .filter(i -> i % 2 == 0)
      .takeWhile(i -> i < 4000000);

    return list.foldr(Integers::add,0);
  }

  @Override
  public String call() {
    return String.valueOf(euler02());
  }
}
