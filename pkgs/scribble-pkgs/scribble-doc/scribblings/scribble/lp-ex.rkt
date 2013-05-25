#lang scribble/lp

Literate programs have chunks of code, like this one:

@chunk[<f>
       (define (f x)
         <fs-body>)]

and this one:

@chunk[<fs-body>
       (* x x)]

that, when assembled, produce a complete program, in this case:

@racketblock[(define (f x)
               (* x x))]
