#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "the function argument to call/comp has the wrong type")

(define-rewrite bug3
  (tc Γ Σ e_1 (→ (→ t_3 t_2) t_3))
  ==> 
  (tc Γ Σ e_1 (→ t_2 t_3))
  #:context (define-judgment-form)
  #:once-only)

(include/rewrite (lib "redex/benchmark/models/delim-cont/delim-cont.rkt") delim-cont bug3)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term (<> ((λ (var:tg : (Prompt Num Bool))
               (% (call/comp (λ (var:x : Bool) (if var:x #f #t))
                             var:tg)
                  var:tg
                  (λ (var:x : Num) #t)))
             (make-prompt-tag Num Bool))
            (call/comp : (→ Num Bool) (call/comp : Num ·)))))

(test small-counter-example)
