# TurtleSloth
TurtleSloth is a JavaScript interpreter that aims for flexibility and clarity of code.  It is written in Scala, and is purely functional.

### How can an interpreter for an imperative language be purely functional?
The interpreter takes javascript source, parses it, and generating a lazy stream of immutable objects each representing a state.  If the program terminates, the stream will end.  If not, the stream will continue forever.

### What's the point?
For fun, to see if it's possible, and see what kind of cool introspection is possible with this sort of interpreter.  Eventually, to explore techniques of static analysis and partial evaluation.

### How complete is it?
Right now, it's barely been started.  See the specs for the current status.

### Will it be fast?
No.