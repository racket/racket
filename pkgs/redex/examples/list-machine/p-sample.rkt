#lang racket
(require redex
         "list-machine.rkt" 
         "list-machine-typing.rkt")

(define p-sample
  (term (l0 : (begin (cons v0 v0 v1)
                     (begin (cons v0 v1 v1)
                            (begin (cons v0 v1 v1)
                                   (jump l1))))
            (l1 : (begin (branch-if-nil v1 l2)
                         (begin (fetch-field v1 1 v1)
                                (begin (branch-if-nil v0 l1)
                                       (jump l2))))
                (l2 : halt
                    end)))))

;; Eval : p -> r | 'crashed | 'multiple-results
(define (Eval p)
  (define l0 (car (judgment-holds (program-lookup ,p l0 ι) ι))) 
  (define result (apply-reduction-relation* red (term (,p (empty v0 ↦ nil) ,l0))))
  (cond
    [(= 1 (length result))
     (with-handlers ((exn:fail:redex? (λ (x) 'crashed)))
       (redex-let list-machine
                  ([(p r halt) (car result)])
                  (term r)))]
    [else 'multiple-results]))

(Eval p-sample)

(define Π-sample
  (term (l0 : (v0 : nil empty)
            (l1 : (v0 : nil (v1 : (list nil) empty))
                (l2 : empty empty)))))

(judgment-holds (check-program ,p-sample ,Π-sample))

