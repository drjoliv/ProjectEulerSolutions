package drjoliv.euler;

import drjoliv.jfunc.data.list.FList;
import drjoliv.jfunc.function.F0;

import static drjoliv.jfunc.data.list.FList.*;
import static drjoliv.jfunc.data.list.Functions.*;
import static drjoliv.jfunc.nums.Integers.*;

public class Euler09 implements F0<String> {
  public static long euler09() {
    FList<Integer> list = 
      start(1)
      .For( a        -> range(a + 1, 1000)
          ,(a, b)    -> flist(1000 - b - a)
          ,(a, b, c) -> guard(a*a + b*b == c*c)
          .semi(flist(a*b*c)));
    return list.head();
  }

  @Override
  public String call() {
    return String.valueOf(euler09());
  }

}
