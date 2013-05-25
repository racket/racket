#lang scribble/manual
@(require scribble/eval)

@interaction[
 (+ 1 2)
 (code:line (+ 1 2) (code:comment "three"))
 (eval:alts (+ 1 2) 5)
 (eval:result @bold{example})
 (eval:alts (+ 1 2) (eval:result @bold{same}))
 (eval:alts (+ 1 2) (eval:result @elem{really the same} "Again..."))
 (eval:alts (+ 1 2) (eval:result @bold{still the same} "!" "error: too many repeats"))
 (eval:alts (+ 1 2) (eval:results (list @racketresult[1] @elem{2} "3") "counting"))
]

