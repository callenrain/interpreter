A Metacircular Racket Evaluator
===============================
Justin Cosentino && Callen Rain
May 12, 2013

Background
----------

Over the past four weeks we have built an interpreter for a large subset of Racket. Not only is our interpreter able to run generic racket code, but it is also able to run the interpreter within itself. Although it took a significant amount of time, our meta-circular interpreter became a "meta-meta-meta-meta interpreter" and was able to compute the result of (+ 1 3). The interpreter was constructed in a "bottom-up" manner, meaning that we first constructed the lowest-level components and then used them as a foundation for the higher-level components. 

We first developed our interpreter's environment abstraction, which was used when evaluating Racket expressions. In order to construct these environments, we first implemented the binding abstraction, allowing the joining of a symbol to a value. Next we implemented frames, or lists of bindings. These frames make up a mutable list within the environment and contain all of our symbol to value bindings. From here we implemented the evaluator, which evaluates a Racket expression using the symbols found in the context of a given environment. This included both self-evaluating expression, such as intergers and booleans, as well as various procedure applications. These procedure applications include define, set!, and a number of primitives. Various special forms have been implemented, such as begin, cond, if, lambda, let, and let\*. 

Additional features have been added to this interpreted, including trace, untrace, quasiquote and unquote. 

Files
-----
<dl>
  <dt>interpreter.rkt</dt>
  <dd>Code containing all functionality of the interpreter.</dd>
  <dt>i-tests.rkt</dt>
  <dd>Code used for unit testing the interpreter.</dd>
</dl>

Usage
-----
To run the interpreter:
"""$ racket
> (require "interpreter.rkt")
> (repl)"""

To run the interpreter within the interpreter:
'''$ racket
> (require "interpreter.rkt")
> (repl)
INTERPRETER> (include "interpreter.rkt")
INTERPRETER> (repl)
INTERPRETER> (include "interpreter.rkt")
INTERPRETER> (repl)
...'''

