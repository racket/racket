#lang racket/base
(require "../gentest-framework.rkt")
(provide proto:kernel-forms
         proto:kernel-contexts)

(define-tests proto:kernel-forms "Kernel forms"
  [#:suite
   "Atomic expressions"
   (testK "required variable"
          null
          #:no-steps)
   (testK "datum (number)"
          1
          [#:steps (tag-datum (#%datum . 1))
                   (macro '1)]
          #:no-hidden-steps)
   (testK "datum (boolean)"
          #f
          [#:steps (tag-datum (#%datum . #f))
                   (macro '#f)]
          #:no-hidden-steps)
   (testK "datum (explicit)"
          (#%datum . 5)
          [#:steps (macro '5)]
          #:no-hidden-steps)
   (testK "#%top (implicit)"
          unbound-variable
          [#:steps (tag-top (#%top . unbound-variable))]
          #:no-hidden-steps)
   (testK "#%top (explicit)"
          (#%top . unbound-variable)
          #:no-steps)
   (testK "quote"
          (quote mumble)
          #:no-steps)
   (testK "#%require"
          (#%require mzscheme)
          #:no-steps)
   (testK "require for-syntax"
          (#%require (for-syntax mzscheme))
          #:no-steps)
   (testK "require for-template"
          (#%require (for-template mzscheme))
          #:no-steps)]

  [#:suite
   "Definitions"
   (testK "define-values"
          (define-values (x) 'a)
          #:no-steps)
   (testK "define-syntaxes"
          (define-syntaxes (x) 'a)
          #:no-steps)]

  [#:suite
   "Simple expressions"
   (testK "if"
          (if 'a 'b 'c)
          #:no-steps)
   (testK "wcm"
          (with-continuation-mark 'a 'b 'c)
          #:no-steps)
   (testK "set!"
          (set! x 'a)
          #:no-steps)]

  [#:suite
   "Sequence-containing expressions"
   (testK "begin"
          (begin 'a 'b)
          #:no-steps)
   (testK "begin0 (single)"
          (begin0 'a)
          #:no-steps)
   (testK "begin0 (multiple)"
          (begin0 'a 'b 'c)
          #:no-steps)
   (testK "#%app (implicit)"
          (+ '1 '2)
          [#:steps (tag-app (#%app + '1 '2))]
          #:no-hidden-steps)
   (testK "#%app (implicit)"
          (+ '1 '2)
          [#:steps (tag-app (#%app + '1 '2))])
   (testK "#%app (explicit)"
          (#%app + '1 '2 '3)
          #:no-steps)]

  [#:suite
   "Binding forms"
   (testK "lambda (simple)"
          (lambda (x) x)
          [#:steps (rename-lambda (lambda (x) x))]
          #:same-hidden-steps)
   (testK "lambda (rest args)"
          (lambda (x . y) y)
          [#:steps (rename-lambda (lambda (x . y) y))]
          #:same-hidden-steps)
   (testK "lambda (multi)"
          (lambda (x) 'a 'b)
          [#:steps (rename-lambda (lambda (x) 'a 'b))]
          #:same-hidden-steps)
   (testK "letrec-values"
          (letrec-values ([(x) 'a]) x)
          [#:steps (rename-letrec-values (letrec-values ([(x) 'a]) x))]
          #:same-hidden-steps)
   (testK "letrec-values"
          (letrec-values ([(x) 'a] [(y) 'b]) y)
          [#:steps
           (rename-letrec-values
            (letrec-values ([(x) 'a] [(y) 'b]) y))]
          #:same-hidden-steps)
   (testK "case-lambda"
          (case-lambda [(x) x] [(x y) y])
          [#:steps
           (rename-case-lambda (case-lambda [(x) x] [(x y) y]))
           (rename-case-lambda (case-lambda [(x) x] [(x y) y]))]
          #:same-hidden-steps)
   (testK "let-values"
          (let-values ([(x) 'a]) x)
          [#:steps (rename-let-values (let-values ([(x) 'a]) x))]
          #:same-hidden-steps)]

  [#:suite
   "Internal definitions within #%stratified-body"
   (testK "internal begin (empty)"
          (#%stratified-body (begin) 'a)
          [#:steps (splice-block (#%stratified-body 'a))
                   (macro 'a)]
          [#:hidden-steps (splice-block (#%stratified-body 'a))])
   (testK "internal begin (solo)"
          (#%stratified-body (begin 'b))
          [#:steps (splice-block (#%stratified-body 'b))
                   (macro 'b)]
          [#:hidden-steps (splice-block (#%stratified-body 'b))])
   (testK "internal begin"
          (#%stratified-body (begin 'a) 'b)
          [#:steps (splice-block (#%stratified-body 'a 'b))
                   (macro (begin 'a 'b))]
          [#:hidden-steps (splice-block (#%stratified-body 'a 'b))])
   (testK "internal define-values"
          (#%stratified-body (define-values (x) 'a) 'b)
          [#:steps (block->letrec (#%stratified-body (letrec-values ([(x) 'a]) (#%stratified-body 'b))))
                   (rename-letrec-values (#%stratified-body (letrec-values ([(x) 'a]) (#%stratified-body 'b))))
                   (macro (#%stratified-body (letrec-values ([(x) 'a]) 'b)))
                   (macro (letrec-values ([(x) 'a]) 'b))]
          [#:hidden-steps
           (block->letrec (#%stratified-body (letrec-values ([(x) 'a]) (#%stratified-body 'b))))
           (rename-letrec-values (#%stratified-body (letrec-values ([(x) 'a]) (#%stratified-body 'b))))])
   (testK "internal define-values in begin"
          (#%stratified-body (begin (define-values (x) 'a)) 'b)
          [#:steps
           (splice-block (#%stratified-body (define-values (x) 'a) 'b))
           (block->letrec (#%stratified-body (letrec-values ([(x) 'a]) (#%stratified-body 'b))))
           (rename-letrec-values (#%stratified-body (letrec-values ([(x) 'a]) (#%stratified-body 'b))))
           (macro (#%stratified-body (letrec-values ([(x) 'a]) 'b)))
           (macro (letrec-values ([(x) 'a]) 'b))]
          [#:hidden-steps
           (splice-block (#%stratified-body (define-values (x) 'a) 'b))
           (block->letrec (#%stratified-body (letrec-values ([(x) 'a]) (#%stratified-body 'b))))
           (rename-letrec-values (#%stratified-body (letrec-values ([(x) 'a]) (#%stratified-body 'b))))])
   (testK "internal begin, then define-values"
          (#%stratified-body (begin) (define-values (x) 'a) 'b)
          [#:steps
           (splice-block (#%stratified-body (define-values (x) 'a) 'b))
           (block->letrec (#%stratified-body (letrec-values ([(x) 'a]) (#%stratified-body 'b))))
           (rename-letrec-values (#%stratified-body (letrec-values ([(x) 'a]) (#%stratified-body 'b))))
           (macro (#%stratified-body (letrec-values ([(x) 'a]) 'b)))
           (macro (letrec-values ([(x) 'a]) 'b))])]

  [#:suite
   "Internal definitions (mixed defs and exprs)"
   (testK "internal begin (empty)"
          (lambda () (begin) 'a)
          [#:steps (rename-lambda (lambda () (begin) 'a))
                   (splice-block (lambda () 'a))]
          #:same-hidden-steps)
   (testK "internal begin (solo)"
          (lambda () (begin 'b))
          [#:steps (rename-lambda (lambda () (begin 'b)))
                   (splice-block (lambda () 'b))]
          #:same-hidden-steps)
   (testK "internal begin"
          (lambda () (begin 'a) 'b)
          [#:steps (rename-lambda (lambda () (begin 'a) 'b))
                   (splice-block (lambda () 'a 'b))]
          #:same-hidden-steps)
   (testK "internal begin"
          (lambda () (begin 'a 'b) 'c) 
          [#:steps (rename-lambda (lambda () (begin 'a 'b) 'c))
                   (splice-block (lambda () 'a 'b 'c))]
          #:same-hidden-steps)
   (testK "internal define-values"
          (lambda () (define-values (x) 'a) 'b)
          [#:steps (rename-lambda (lambda () (define-values (x) 'a) 'b))
                   (block->letrec (lambda () (letrec-values ([(x) 'a]) 'b)))
                   (rename-letrec-values (lambda () (letrec-values ([(x) 'a]) 'b)))]
          #:same-hidden-steps)
   (testK "internal define-values in begin"
          (lambda () (begin (define-values (x) 'a)) 'b)
          [#:steps
           (rename-lambda (lambda () (begin (define-values (x) 'a)) 'b))
           (splice-block (lambda () (define-values (x) 'a) 'b))
           (block->letrec (lambda () (letrec-values ([(x) 'a]) 'b)))
           (rename-letrec-values (lambda () (letrec-values ([(x) 'a]) 'b)))]
          #:same-hidden-steps)
   (testK "internal begin, then define-values"
          (lambda () (begin) (define-values (x) 'a) 'b)
          [#:steps
           (rename-lambda (lambda () (begin) (define-values (x) 'a) 'b))
           (splice-block (lambda () (define-values (x) 'a) 'b))
           (block->letrec (lambda () (letrec-values ([(x) 'a]) 'b)))
           (rename-letrec-values (lambda () (letrec-values ([(x) 'a]) 'b)))]
          #:same-hidden-steps)
   (testK "define-values after expr"
          (lambda () 'a (define-values (x) 'b) 'c)
          [#:steps
           (rename-lambda (lambda () 'a (define-values (x) 'b) 'c))
           (block->letrec (lambda () (letrec-values ([() (begin 'a (values))] [(x) 'b]) 'c)))
           (rename-letrec-values
            (lambda () (letrec-values ([() (begin 'a (values))] [(x) 'b]) 'c)))
           (tag-app (lambda () (letrec-values ([() (begin 'a (#%app values))] [(x) 'b]) 'c)))
           ;; FIXME: should have TAG step for transform to nested let-values
           ]
          [#:hidden-steps
           (rename-lambda (lambda () 'a (define-values (x) 'b) 'c))
           (block->letrec (lambda () (letrec-values ([() (begin 'a (values))] [(x) 'b]) 'c)))
           (rename-letrec-values
            (lambda () (letrec-values ([() (begin 'a (values))] [(x) 'b]) 'c)))])]

  [#:suite
   "Top-level begin"
   (testK "begin (top-level)"
          (begin (define-values (x) 'a) 'b)
          #:no-steps)
   (testK "begin (empty)"
          (begin)
          #:no-steps)])

(define-tests proto:kernel-contexts "Kernel contexts"
  [#:suite 
   "Definitions"
   (testK "define-values"
          (define-values (x) (id 'a))
          [#:steps (macro (define-values (x) 'a))]
          #:no-hidden-steps)
   (testK "define-values"
          (define-values (x) (Tid 'a))
          [#:steps (macro (define-values (x) 'a))]
          #:same-hidden-steps)]
  [#:suite
   "Simple expressions"
   (testK "if (with else)"
          (if (Tid 'a) (Tid 'b) (Tid 'c))
          [#:steps (macro (if 'a (Tid 'b) (Tid 'c)))
                   (macro (if 'a 'b (Tid 'c)))
                   (macro (if 'a 'b 'c))]
          #:same-hidden-steps)
   (testK "wcm"
          (with-continuation-mark (id 'a) (id 'b) (id 'c))
          [#:steps (macro (with-continuation-mark 'a (id 'b) (id 'c)))
                   (macro (with-continuation-mark 'a 'b (id 'c)))
                   (macro (with-continuation-mark 'a 'b 'c))]
          #:no-hidden-steps)]
  [#:suite
   "Sequence-containing forms"
   (testK "begin"
          (begin (id 'a) (id 'b))
          [#:steps (macro (begin 'a (id 'b)))
                   (macro (begin 'a 'b))]
          #:no-hidden-steps)
   (testK "begin"
          (begin (Tid 'a) (Tid 'b))
          [#:steps (macro (begin 'a (Tid 'b)))
                   (macro (begin 'a 'b))]
          #:same-hidden-steps)
   (testK "begin0 (single)"
          (begin0 (id 'a))
          [#:steps (macro (begin0 'a))]
          #:no-hidden-steps)
   (testK "begin0 (multiple)"
          (begin0 (id 'a) (id 'b))
          [#:steps (macro (begin0 'a (id 'b)))
                   (macro (begin0 'a 'b))]
          #:no-hidden-steps)
   (testK "#%app (implicit)"
          ((id cons) (id 'a) (id 'b))
          [#:steps (tag-app (#%app (id cons) (id 'a) (id 'b)))
                   (macro (#%app cons (id 'a) (id 'b)))
                   (macro (#%app cons 'a (id 'b)))
                   (macro (#%app cons 'a 'b))]
          #:no-hidden-steps)
   (testK "#%app (implicit)"
          ((Tid cons) (Tid 'a) (Tid 'b))
          [#:steps (tag-app (#%app (Tid cons) (Tid 'a) (Tid 'b)))
                   (macro (#%app cons (Tid 'a) (Tid 'b)))
                   (macro (#%app cons 'a (Tid 'b)))
                   (macro (#%app cons 'a 'b))]
          [#:hidden-steps (macro (cons (Tid 'a) (Tid 'b)))
                          (macro (cons 'a (Tid 'b)))
                          (macro (cons 'a 'b))])
   (testK "#%app (explicit)"
          (#%app (id cons) (id 'a) (id 'b))
          [#:steps (macro (#%app cons (id 'a) (id 'b)))
                   (macro (#%app cons 'a (id 'b)))
                   (macro (#%app cons 'a 'b))]
          #:no-hidden-steps)
   (testK "#%app (explicit)"
          (#%app (Tid cons) (Tid 'a) (Tid 'b))
          [#:steps (macro (#%app cons (Tid 'a) (Tid 'b)))
                   (macro (#%app cons 'a (Tid 'b)))
                   (macro (#%app cons 'a 'b))]
          #:same-hidden-steps)]
  
  [#:suite
   "Binding forms"
   (testK "lambda (simple)"
          (lambda (x) (id x))
          [#:steps (rename-lambda (lambda (x) (id x)))
                   (macro (lambda (x) x))]
          [#:hidden-steps (rename-lambda (lambda (x) (id x)))])
   (testK "lambda (rest args)"
          (lambda (x . y) (id y))
          [#:steps (rename-lambda (lambda (x . y) (id y)))
                   (macro (lambda (x . y) y))]
          [#:hidden-steps (rename-lambda (lambda (x . y) (id y)))])
   (testK "lambda (multi)"
          (lambda (x) (id 'a) (id 'b))
          [#:steps (rename-lambda (lambda (x) (id 'a) (id 'b)))
                   (macro (lambda (x) 'a (id 'b)))
                   (macro (lambda (x) 'a 'b))]
          [#:hidden-steps (rename-lambda (lambda (x) (id 'a) (id 'b)))])
   (testK "lambda (splice)"
          (lambda (x) (begin (id 'a) (id 'b)) (id 'c))
          [#:steps (rename-lambda (lambda (x) (begin (id 'a) (id 'b)) (id 'c)))
                   (splice-block (lambda (x) (id 'a) (id 'b) (id 'c)))
                   (macro (lambda (x) 'a (id 'b) (id 'c)))
                   (macro (lambda (x) 'a 'b (id 'c)))
                   (macro (lambda (x) 'a 'b 'c))]
          [#:hidden-steps
           (rename-lambda (lambda (x) (begin (id 'a) (id 'b)) (id 'c)))
           (splice-block (lambda (x) (id 'a) (id 'b) (id 'c)))])
   (testK "lambda (splice 2)"
          (lambda (x) (id (begin 'a 'b)) (id 'c))
          [#:steps (rename-lambda (lambda (x) (id (begin 'a 'b)) (id 'c)))
                   (macro (lambda (x) (begin 'a 'b) (id 'c)))
                   (splice-block (lambda (x) 'a 'b (id 'c)))
                   (macro (lambda (x) 'a 'b 'c))])
   (testK "case-lambda"
          (case-lambda [(x) (id x)] [(x y) (id y)])
          [#:steps (rename-case-lambda (case-lambda [(x) (id x)] [(x y) (id y)]))
                   (macro (case-lambda [(x) x] [(x y) (id y)]))
                   (rename-case-lambda (case-lambda [(x) x] [(x y) (id y)]))
                   (macro (case-lambda [(x) x] [(x y) y]))]
          [#:hidden-steps
           (rename-case-lambda (case-lambda [(x) (id x)] [(x y) (id y)]))
           (rename-case-lambda (case-lambda [(x) (id x)] [(x y) (id y)]))])
   (testK "let-values"
          (let-values ([(x) (id 'a)]) (id (cons 'b x)))
          [#:steps (rename-let-values (let-values ([(x) (id 'a)]) (id (cons 'b x))))
                   (macro (let-values ([(x) 'a]) (id (cons 'b x))))
                   (macro (let-values ([(x) 'a]) (cons 'b x)))
                   (tag-app (let-values ([(x) 'a]) (#%app cons 'b x)))])
   (testK "letrec-values"
          (letrec-values ([(x) (id 'a)]) (id (cons 'b x)))
          [#:steps
           (rename-letrec-values (letrec-values ([(x) (id 'a)]) (id (cons 'b x))))
           (macro (letrec-values ([(x) 'a]) (id (cons 'b x))))
           (macro (letrec-values ([(x) 'a]) (cons 'b x)))
           (tag-app (letrec-values ([(x) 'a]) (#%app cons 'b x)))])])
