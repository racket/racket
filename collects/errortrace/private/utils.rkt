#lang racket/base

(provide count-meta-levels
         generate-key-imports)

(define base (variable-reference->module-base-phase (#%variable-reference)))

(define ((count-meta-levels phase) expr)
  (syntax-case expr ()
    [(bfs . exprs)
     (free-identifier=? #'bfs #'begin-for-syntax phase base)
     (add1 (apply max 0 (map (count-meta-levels (add1 phase)) (syntax->list #'exprs))))]
    [(ds . _)
     (free-identifier=? #'ds #'define-syntaxes phase base)
     1]
    [(b . exprs)
     (free-identifier=? #'b #'begin phase base)
     (apply max 0 (map (count-meta-levels phase) (syntax->list #'exprs)))]
    [_ 0]))


(define (generate-key-imports meta-depth)
  (syntax-shift-phase-level
   (let loop ([meta-depth meta-depth])
     (let ([e ((make-syntax-introducer)
               #`(#%require (for-meta #,meta-depth 
                                      errortrace/errortrace-key)))])
       (if (zero? meta-depth)
           e
           #`(begin #,e #,(loop (sub1 meta-depth))))))
   (- (syntax-local-phase-level) base)))
