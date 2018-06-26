package drjoliv.euler;

import drjoliv.fjava.adt.FList;
import static drjoliv.fjava.adt.FList.*;

import drjoliv.fjava.nums.Numbers;
import static drjoliv.fjava.nums.Numbers.*;

public class Euler01 {
  public static int euler01() {
    FList<Integer> list = range(1,999)
                  .filter(i -> i % 3 == 0 || i % 5 == 0);
    return list.foldr(Numbers::add, 0);
  }
}
