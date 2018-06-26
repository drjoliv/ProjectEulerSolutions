package drjoliv.euler;

import drjoliv.fjava.adt.FList;
import drjoliv.fjava.nums.Numbers;

public class Euler02 {
  public static int euler02() {
    FList<Integer> list = FList.sequence(0,1, Numbers::add)
      .filter(i -> i % 2 == 0)
      .takeWhile(i -> i < 4000000);

    return list.foldr(Numbers::add,0);
  }
}
