#lang racket/base

#|

tests the color setting ability during a reduction sequence.

In one window, you expect to see a red and a blue snip. as you reduce you expect to see a spectrum from blue to red

In the other window, you expect to see the currently unreducted terms in green and all others white.

|#

(require redex/reduction-semantics
         redex/gui
         racket/gui/base
         racket/class)

(reduction-steps-cutoff 1)

(let ()
  
  (define (get-range term-node)
    (let loop ([node term-node])
      (let ([parents (term-node-parents node)])
        (cond
          [(null? parents) (list node)]
          [else (cons node (loop (car parents)))]))))
  
  (define (color-range-pred sexp term-node) 
    (let* ([parents (get-range term-node)]
           [max-val (car (term-node-expr (car parents)))])
      (for-each
       (λ (node)
         (let ([val (car (term-node-expr node))])
           (term-node-set-color! node
                                 (make-object color% 
                                   (floor (- 255 (* val (/ 255 max-val))))
                                   0
                                   (floor (* val (/ 255 max-val)))))))
       parents)
      (term-node-color term-node)))
  
  (define-language empty-language)
  
  (traces 
   (reduction-relation
    empty-language
    (--> (number_1 word)
         (,(+ (term number_1) 1) word)
         inc))
   '(1 word)
   #:pred color-range-pred))

(let ()
  (define-language empty-language)
  
  (define (last-color-pred sexp term-node)
    (if (null? (term-node-children term-node))
        "green"
        "white"))
  
  (traces (reduction-relation
           empty-language
           (--> (number_1 word)
                (,(+ (term number_1) 1) word)
                inc)
           (--> (number_1 word)
                (,(* (term number_1) 2) word)
                dup))
          '(1 word)
          #:pred last-color-pred))
