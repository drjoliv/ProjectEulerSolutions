package drjoliv.euler;

import drjoliv.jfunc.contorl.Try;
import drjoliv.jfunc.function.F0;
import drjoliv.jfunc.function.F1;
import drjoliv.jfunc.io.IO;
import drjoliv.jfunc.nums.Longs;

import static drjoliv.jfunc.show.Show.*;

public class Euler08 implements F0<String> {
  public static long euler08() throws Exception {
    F1<String,Long> go = showStr()
      .then(fc  -> fc.map(c -> Character.getNumericValue(c)))
      .then(fi  -> fi.map(Integer::longValue))
      .then(fl  -> fl.window(13))
      .then(ffl -> ffl.map(Longs::product))
      .then(f   -> f.maximum(Long::compare));

    Try<Long> stream = Try.with(() -> Euler08.class.getResource("euler08.txt").openStream())
      .bind(IO.readFileFromStream)
      .map(go);
    
    return stream.get();
  }

  @Override
  public String call() {
    try {
        return String.valueOf(euler08());
      } catch (Exception e) {
        e.printStackTrace();
        return "fail";
      }
    }
}
