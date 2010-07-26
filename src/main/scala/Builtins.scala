package org.sdj.turtlesloth.builtins
import org.sdj.turtlesloth._

object Print extends Method {
  override def resolve(args: List[Any], state: State) = {
    val string = args.head.toString + "\n";

    (null, state.copy(outputStrings = string :: state.outputStrings))
  }
}

object DefaultGlobals {
  lazy val get = Map("print" -> Print)
}