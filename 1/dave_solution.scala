// Advent of code, problem 1
// http://adventofcode.com/2016/day/1/answer

// Complex number library.  Add dep in build.sbt, or this in ammonite:
// load.ivy("org.scalanlp" %% "breeze" % "latest.integration")

// Input
val in1 = "L1, L1, L1, L1, L1"

// Solve it, 1-liner(ish)
val parser = "([LR])(\\d+)".r; val pos = in1.split(", ").foldLeft(Complex.i, Complex.zero) {case ((dir, pos), parser(d, len)) => val newDir = dir * (if (d == "L") Complex.i else -Complex.i); (newDir, pos + newDir * len.toInt) }._2; println(pos.re().abs + pos.im().abs) 


//
// Better explanation of what's happening there.
//

// Shitty simple way to pattern-match the string into turn direction and distance
val parser = "([LR])(\\d+)".r; 

// Split input string into Seq of individual moves (Turn direction + distance)
val moves = in1.split(", ")

// Strategy: Do a foldLeft on the above.  Fold carryover state is "Direction we're facing (expressed as a complex number)
// and current position (also as a complex number)".  Fold initial state is "Facing north (towards 0+i), standing on origin
// (0+0i)"  At each fold step, we first update our direction faced (which is a multiplication of the prior direction
// by +/-i), then update our position to "old position + (new direction * step distance)".

val pos = moves
  .foldLeft(Complex.i, Complex.zero) {
    case ((dir, pos), parser(d, len)) => 
      val newDir = dir * (if (d == "L") Complex.i else -Complex.i); 
      (newDir, pos + (newDir * len.toInt))
  }._2;
  
pos.re().abs + pos.im().abs 
