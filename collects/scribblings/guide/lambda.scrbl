#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "guide:lambda"]{Procedures: @scheme[lambda] and @scheme[case-lambda]}

A @scheme[lambda] expression creates a procedure. In the simplest
case, a @scheme[lambda] expression has the form

@specform[
(lambda (arg-id ...)
  body-expr ...+)
]

The @scheme[...+] in this syntactic sketch means ``one or more
repetitions of the preceeding element.'' Specifically, it means one or
more @scheme[_body-expr]s.

A @scheme[lambda] form with @math{n} @scheme[_arg-id]s accepts
@math{n} arguments:

@interaction[
((lambda (x) x)
 1)
((lambda (x y) (+ x y)) 
 1 2)
((lambda (x y) (+ x y)) 
 1)
]

A @scheme[lambda] expression can also have the form

@specform[
(lambda rest-id
  body-expr ...+)
]

That is, a @scheme[lambda] expression can have a single
@scheme[_rest-id] that is not surrounded by parentheses. The resulting
procedure accepts any number of arguments, and the arguments are put
into a list bound to @scheme[_rest-id].

@examples[
((lambda x x) 
 1 2 3)
((lambda x x))
((lambda x (car x))
 1 2 3)
]

Combining thes two styles, a @scheme[lambda] expression can have the
form

@specform[
(lambda (arg-id ...+ . rest-id)
  body-expr ...+)
]

The result is a procedure that requires at least as many arguments as
@scheme[_arg-id]s, and also accepts any number of additional
arguments.

@examples[
((lambda (a b . x) (cons (/ a b) x))
 1 2 3 4)
((lambda (a b . x) (cons (/ a b) x))
 1 2)
((lambda (a b . x) (cons (/ a b) x)) 
 1)
]

Support for optional and keyword arguments lead to even more
possibilities:

@itemize{

 @item{Instead of just an @scheme[_arg-id], an form argument can be
       @scheme[[_arg-id _default-expr]], which means that the argument
       is optional. When the argument is not supplied,
       @scheme[_default-expr] produces the default value. The
       @scheme[_default-expr] can refer to any preceding
       @scheme[_arg-id], and every following @scheme[_arg-id] must
       have a default as well.

       @examples[
         ((lambda (x [y 5]) (list x y))
          1 2)
         ((lambda (x [y 5]) (list x y)) 
          1)
         ((lambda (x [y (+ x 1)]) (list x y))
          1)
         (lambda ([x 5] y) (list x y))
       ]}

 @item{Instead of just an @scheme[_arg-id], an form argument can be
       @scheme[(code:line _keyword _arg-id)], which indicates a
       by-keyword argument instead of a by-position argument. The
       position of the keyword--identifier pair in the argument list
       does not matter for matching with arguments in an application,
       because it will be matched to an argument value by keyword
       insteda of by position.

       @examples[
         ((lambda (x #:second y) (list x y))
          1 #:second 2)
         ((lambda (x #:second y) (list x y)) 
          #:second 2 1)
         ((lambda (#:second y x) (list x y)) 
          1 #:second 2)
         ((lambda (x #:second y) (list x y)) 
          1 2)
         ((lambda (x #:second y) (list x y))
          #:second 2)
       ]}

 @item{The previous two possibilities can be combined to specify a
       by-keyword argument with a default value.

       @examples[
         ((lambda (x #:second [y 5]) (list x y))
          1 #:second 2)
         ((lambda (x #:second [y 5]) (list x y))
          1)
       ]}

}

The @scheme[case-lambda] form creates a procedure that can have
completely different behaviors depending on the number of arguments
that are supplied. A case-lambda expression has the form

@specform[
(case-lambda
  [formals body-expr ...+]
  ...)
]

where each @scheme[_formals _body-expr ...+] is anlogous to
@scheme[(lambda _formals _body-expr ...+)]. That is, a
@scheme[_formals] can be @scheme[(_arg-id ...)], @scheme[_rest-id], or 
@scheme[(_arg-id ... . _rest-id)].

Applying a procedure produced by @scheme[case-lambda] is like applying
a @scheme[lambda] for the first case that matches the number of given
arguments.

@examples[
((case-lambda [() 10][(x y) (+ x y)])
 1 2)
((case-lambda [() 10][(x y) (+ x y)]))
((case-lambda [() 10][(x y) (+ x y)])
 1)
]

A @scheme[case-lambda] procedure cannot directly support optional or
keyword arguments.

@refdetails["mz:lambda"]{procedure expressions}
