package org.sdj.turtlesloth.builtins
import org.sdj.turtlesloth._

object Print extends Method {
  override def resolve(args: List[Any], state: State) = {
    val string = args.head

    (null, state.copy(output = state.output + "%s\n".format(string)))
  }
}

object DefaultGlobals {
  lazy val get = Map("print" -> Print)
}