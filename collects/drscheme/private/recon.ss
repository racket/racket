#lang racket/base
(require (for-syntax racket/base))
(provide reconstitute)

(begin-for-syntax
  (define-struct sloc (inside loc) #:omit-define-syntaxes #:prefab))

(define-syntax (reconstitute orig-stx)
  (syntax-case orig-stx ()
    [(_ arg src)
     (let loop ([stx #'arg])
       (cond
         [(syntax? stx) (datum->syntax stx (loop (syntax-e stx)))]
         [(pair? stx) (cons (loop (car stx)) (loop (cdr stx)))]
         [(sloc? stx) 
          (datum->syntax #'src
                         (loop (syntax-e (sloc-inside stx)))
                         (syntax->datum (sloc-loc stx)))]
         [else stx]))]))
