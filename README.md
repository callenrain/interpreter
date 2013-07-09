interpreter
===========

CPSC 037 Interpreter Project
Callen Rain && Justin Cosentino
May 12, 2013
-------------------------------------------------------------------------------
Over the past four weeks we have built an interpreter for a large subset of
Racket. This interpreter is able to run a majority of the code we have written 
with this course, excluding the streams and graphics assignments. Not only is 
our interpreter able to run this code, but it is also able to run the
interpreter within itself. Although it took a significant amount of time, our
meta-circular interpreter became a "meta-meta-meta-meta interpreter" and was 
able to compute the result of (+ 1 3). The interpreter was constructed in a 
"bottom-up" manner, meaning that we first constructed the lowest-level 
components and then used them as a foundation for the higher-level components. 

We first developed our interpreter's environment abstraction, which were used
when evaluating Racket expressions. In order to construct these environments, 
we first implemented the binding abstraction, allowing the joining of a symbol
to a value. Next we implemented frames, or lists of bindings. These frames make
up a mutable list within the environment and contain all of our symbol to value
bindings. From here we implemented the evaluator, which evaluates a Racket 
expression using the symbols found in the context of a given environment. This 
included both self-evaluating expression, such as intergers and booleans, as
well as various procedure applications. These procedure applications include
define, set!, and a number of primitives. At this point we built the
functionality for special forms, which are functions that each have different
forms and thus must be handled differently. The special forms included in this
interpreter are begin, cond, and if. We then implemented lambda, let, and let* 
procedures. Finally, we added the necessary components to the interpreter that
allow it to be meta-circular. 

A few special features were added to our interpreter. We implemented both 
extension A and B, allowing users to trace/untrace functions as well as utilize
quasiquote and unquote. 

Fortunately there were no sections of the interpreter that did not work, and we
were able to implement all the required features. 
