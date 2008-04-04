
#lang scheme/base
(require scheme/contract
         syntax/stx
         "deriv-c.ss")

(provide (all-from-out "deriv-c.ss"))

#|

(define (?? c) (or/c c false/c))

(define (stx? x)
  (or (syntax? x)
      (and (pair? x) (stx? (car x)) (stx? (cdr x)))
      (null? x)))

(define (stx-list-like? x)
  (let ([x (stx->list x)])
    (and x (andmap syntax? x))))

(define syntax/f (?? syntax?))
(define syntaxes/c stx-list-like?)
(define syntaxes/f (?? syntaxes/c))
(define resolves/c (listof identifier?))

(define localaction/c
  (or/c local-expansion? local-expansion/expr? local-lift?
        local-lift-end? local-bind?))

(provide/contract
 (struct node
         ([z1 any/c]
          [z2 any/c]))
 (struct (deriv node)
         ([z1 syntax?]
          [z2 syntax/f]))
 (struct (lift-deriv deriv)
         ([z1 syntax?]
          [z2 syntax/f]
          [first deriv?]
          [lift-stx syntax?]
          [second deriv?]))
 (struct (mrule deriv)
         ([z1 syntax?]
          [z2 syntax/f]
          [transformation transformation?]
          [next (?? deriv?)]))
 (struct (lift/let-deriv deriv)
         ([z1 syntax?]
          [z2 syntax/f]
          [first deriv?]
          [lift-stx syntax?]
          [second deriv?]))
 (struct (transformation node)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [me1 (?? syntax?)]
          [locals (?? (listof localaction/c))]
          [me2 (?? syntax?)]
          [?2 (?? exn?)]
          [seq number?]))
 (struct (local-expansion node)
         ([z1 syntax?]
          [z2 syntax/f]
          [me1 syntax?]
          [me2 syntax/f]
          [for-stx? boolean?]
          [inner deriv?]))
 (struct (local-expansion/expr node)
         ([z1 syntax?]
          [z2 syntax/f]
          [me1 syntax?]
          [me2 syntax/f]
          [for-stx? boolean?]
          [opaque any/c]
          [inner deriv?]))
 (struct local-lift
         ([expr syntax?]
          [id identifier?]))
 (struct local-lift-end
         ([decl syntax?]))
 (struct local-bind
         ([bindrhs bind-syntaxes?]))
 (struct (base deriv)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (prule base)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (p:variable prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (p:module prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [one-body-form? boolean?]
          [mb (?? deriv?)]
          [?2 (?? exn?)]
          [body (?? deriv?)]))
 (struct (p:#%module-begin prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [pass1 (?? (listof modrule?))]
          [pass2 (?? (listof modrule?))]
          [?2 (?? exn?)]))
 (struct (p:define-syntaxes prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [rhs (?? deriv?)]
          [?2 (?? exn?)]))
 (struct (p:define-values prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [rhs (?? deriv?)]))
 (struct (p:#%expression prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [inner (?? deriv?)]))
 (struct (p:if prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [full? boolean?]
          [test (?? deriv?)]
          [then (?? deriv?)]
          [else (?? deriv?)]))
 (struct (p:wcm prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [key (?? deriv?)]
          [mark (?? deriv?)]
          [body (?? deriv?)]))
 (struct (p:set! prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [id-resolves (?? resolves/c)]
          [rhs (?? deriv?)]))
 (struct (p:set!-macro prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [deriv (?? deriv?)]))
 (struct (p:#%app prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [tagged-stx syntax/f]
          [lderiv (?? lderiv?)]))
 (struct (p:begin prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [lderiv (?? lderiv?)]))
 (struct (p:begin0 prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [first (?? deriv?)]
          [lderiv (?? lderiv?)]))
 (struct (p:lambda prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [renames any/c] ;; fixme
          [body (?? bderiv?)]))
 (struct (p:case-lambda prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [renames+bodies (listof clc?)]))
 (struct (p:let-values prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [renames any/c] ;; fixme
          [rhss (?? (listof deriv?))]
          [body (?? bderiv?)]))
 (struct (p:letrec-values prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [renames any/c] ;; fixme
          [rhss (?? (listof deriv?))]
          [body (?? bderiv?)]))
 (struct (p:letrec-syntaxes+values prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [srenames any/c] ;; fixme
          [sbindrhss (?? (listof bind-syntaxes?))]
          [vrenames any/c] ;; fixme
          [vrhss (?? (listof deriv?))]
          [body (?? bderiv?)]))
 (struct (p::STOP prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (p:stop p::STOP)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (p:unknown p::STOP)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (p:#%top p::STOP)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [tagged-stx syntax/f]))
 (struct (p:#%datum p::STOP)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [tagged-stx syntax/f]))
 (struct (p:quote p::STOP)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (p:quote-syntax p::STOP)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (p:require p::STOP)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (p:require-for-syntax p::STOP)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (p:require-for-template p::STOP)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (p:provide p::STOP)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]))
 (struct (p:rename prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [renames any/c]
          [inner (?? deriv?)]))
 (struct (p:synth prule)
         ([z1 syntax?]
          [z2 syntax/f]
          [resolves resolves/c]
          [?1 (?? exn?)]
          [subterms (?? (listof subitem?))]
          [?2 (?? exn?)]))

 (struct (lderiv node)
         ([z1 stx?]
          [z2 syntaxes/f]
          [?1 (?? exn?)]
          [derivs (?? (listof deriv?))]))
 (struct (bderiv node)
         ([z1 stx?]
          [z2 syntaxes/f]
          [pass1 (?? (listof (or/c b:error? brule?)))]
          [trans (symbols 'list 'letrec)]
          [pass2 (?? lderiv?)]))

 (struct b:error
         ([?1 exn?]))
 (struct brule
         ([renames any/c]))
 (struct (b:expr brule)
         ([renames any/c]
          [head deriv?]))
 (struct (b:splice brule)
         ([renames any/c]
          [head deriv?]
          [?1 (?? exn?)]
          [tail (?? stx?)]
          [?2 (?? exn?)]))
 (struct (b:defvals brule)
         ([renames any/c]
          [head deriv?]
          [?1 (?? exn?)]))
 (struct (b:defstx brule)
         ([renames any/c]
          [head deriv?]
          [?1 (?? exn?)]
          [bindrhs (?? bind-syntaxes?)]))

 (struct bind-syntaxes
         ([rhs deriv?]
          [?1 (?? exn?)]))

 (struct clc
         ([?1 (?? exn?)]
          [renames any/c]
          [body (?? bderiv?)]))

 (struct modrule ())
 (struct (mod:cons modrule)
         ([head deriv?]))
 (struct (mod:prim modrule)
         ([head deriv?]
          [prim (?? deriv?)]))
 (struct (mod:skip modrule) ())
 (struct (mod:splice modrule)
         ([head deriv?]
          [?1 (?? exn?)]
          [tail (?? stx?)]))
 (struct (mod:lift modrule)
         ([head deriv?]
          [tail syntaxes/c]))
 (struct (mod:lift-end modrule)
         ([tail syntaxes/c]))

 (struct subitem ())
 (struct (s:subterm subitem)
         ([path any/c]
          [deriv deriv?]))
 (struct (s:rename subitem)
         ([path any/c]
          [before syntax?]
          [after syntax?])))
|#
