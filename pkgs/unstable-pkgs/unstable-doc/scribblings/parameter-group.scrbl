#lang scribble/manual

@(require scribble/eval "utils.rkt" (for-label racket unstable/parameter-group))

@title{Parameter Groups}
@unstable[@author+email["Neil Toronto" "ntoronto@racket-lang.org"]]

@defmodule[unstable/parameter-group]

Parameter groups are parameter-like @italic{views} that represent multiple parameters.

@(define evaluator (make-base-eval))

@examples[#:eval evaluator
                 (require unstable/parameter-group)
                 (define param1 (make-parameter 1))
                 (define param2 (make-parameter 2))
                 (define-parameter-group params (param1 param2))
                 (params)
                 (parameterize/group ([params  (params-value 10 20)])
                   (list (param1) (param2)))
                 (params)
                 (params (params-value 100 200))
                 (list (param1) (param2))]

Use parameter groups to conveniently set multiple parameters.
For example, the @racketmodname[plot #:indirect] library uses parameter groups
to save and restore appearance-controlling parameters when it must draw plots within a thunk.

@defproc[(parameter-group? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a parameter group.
}

@defform/subs[(define-parameter-group name (param-or-group-expr ...) options)
              ([options code:blank
                        (code:line #:struct struct-name)])
              #:contracts ([param-or-group-expr (or/c parameter? parameter-group?)])]{
Defines a new parameter group.

If @racket[struct-name] is not given, @racket[define-parameter-group] defines a new struct @racket[<name>-value] to hold the values of parameters.

If @racket[struct-name] is given, it must have a constructor @racket[(struct-name param-or-group-expr ...)] that accepts as many arguments as there are parameters in the group, and a @racket[struct-name] match expander that accepts as many patterns as there are parameters.

@examples[#:eval evaluator
                 (struct two-params (p1 p2) #:transparent)
                 (define-parameter-group params* (param1 param2) #:struct two-params)
                 (params*)]
}

@defform[(parameterize/group ([param-or-group-expr  value-expr] ...)
           body-expr ...+)
         #:contracts ([param-or-group-expr (or/c parameter? parameter-group?)])]{
Corresponds to @racket[parameterize], but can parameterize parameter groups as well as parameters.
}

@defform[(parameterize*/group ([param-or-group-expr  value-expr] ...)
           body-expr ...+)
         #:contracts ([param-or-group-expr (or/c parameter? parameter-group?)])]{
Corresponds to @racket[parameterize*], but can parameterize parameter groups as well as parameters.
}


@close-eval[evaluator]
