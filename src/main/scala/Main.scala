package org.sdj.turtlesloth


object Main {

  def main(args: Array[String]) = {
    (1 to 5000).foreach {(i) =>
      println(Program.fromSource("f = function() { v = \"%s\"; print(v);} f();".format(i)).completion.output)
    }
  }
}