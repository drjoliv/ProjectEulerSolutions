package drjoliv.euler;

import static drjoliv.jfunc.data.list.FList.flist;
import static drjoliv.jfunc.data.list.Functions.guard;
import static drjoliv.jfunc.data.list.Functions.lastEq;
import static drjoliv.jfunc.data.list.Functions.sizeOf;
import static drjoliv.jfunc.nums.Integers.range;

import drjoliv.jfunc.contorl.CaseOf;
import drjoliv.jfunc.data.list.FList;
import drjoliv.jfunc.function.F0;
import drjoliv.jfunc.show.Show;

public class Euler04 implements F0<String> {

  public static <A> boolean isPalindrome(FList<A> chars) {
    return chars.visit(n -> true
          , (h,t) -> {
            return CaseOf.caseOf(t)
              .of(FList::isEmpty, () -> true)
              .of(sizeOf(1),      () -> h == t.head())
              .of(lastEq(h),      l  ->  isPalindrome(l.init()))
              .otherwise(() -> false);
          });
  }

  public static int euler04() {
    Show<Integer> s = Show.showInt();
    FList<Integer> l =
      range(100, 999)
      .For(a    -> range(a, 999)
         ,(a,b) -> { 
           int p = a * b;
           return guard(isPalindrome(s.show(p))).semi(flist(p));
         });
    return l.maximum(Integer::compare);
  }

  @Override
  public String call() {
    return String.valueOf(euler04());
  }
}
