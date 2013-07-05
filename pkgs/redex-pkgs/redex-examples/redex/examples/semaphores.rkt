#lang racket
(require redex)

#|
  
semaphores make things much more predictable...
  
|#  

(reduction-steps-cutoff 100)

(define-language lang
  (p ((store (variable v) ...)
      (semas (variable sema-count) ...)
      (threads e ...)))
  (sema-count number
              none)
  (e (set! variable e)
     (begin e ...)
     (semaphore variable)
     (semaphore-wait e)
     (semaphore-post e)
     (lambda (variable) e)
     (e e)
     variable
     (list e ...)
     (cons e e)
     number
     (void))
  (p-ctxt ((store (variable v) ...)
           (semas (variable sema-count) ...)
           (threads e ... e-ctxt e ...)))
  (e-ctxt (e-ctxt e)
          (v e-ctxt)
          (cons e-ctxt e)
          (cons v e-ctxt)
          (list v ... e-ctxt e ...)
          (set! variable e-ctxt)
          (begin e-ctxt e ...)
          (semaphore-wait e-ctxt)
          (semaphore-post e-ctxt)
          hole)
  (v (semaphore variable)
     (lambda (variable) e)
     (list v ...)
     number
     (void)))

(define reductions
  (reduction-relation
   lang
   (--> (in-hole (name c p-ctxt) (begin v e_1 e_2 e_rest ...))
        (in-hole c (begin e_1 e_2 e_rest ...)))
   (--> (in-hole (name c p-ctxt) (cons v_1 (list v_2s ...)))
        (in-hole c (list v_1 v_2s ...)))
   (--> (in-hole (name c p-ctxt) (begin v e_1))
        (in-hole c e_1))
   (--> (in-hole (name c p-ctxt) (begin v_1))
        (in-hole c v_1))
   (--> ((store
             (variable_before v_before) ...
           ((name x variable) (name v v))
           (variable_after v_after) ...)
         (name semas any)
         (threads
          e_before ...
          (in-hole (name c e-ctxt) (name x variable))
          e_after ...))
        ((store
             (variable_before v_before) ...
           (x v)
           (variable_after v_after) ...)
         semas
         (threads
          e_before ...
          (in-hole c v)
          e_after ...)))
   (--> ((store (variable_before v_before) ...
           (variable_i v)
           (variable_after v_after) ...)
         (name semas any)
         (threads
          e_before ...
          (in-hole (name c e-ctxt) (set! variable_i v_new))
          e_after ...))
        ((store (variable_before v_before) ...
           (variable_i v_new)
           (variable_after v_after) ...)
         semas
         (threads
          e_before ...
          (in-hole c (void))
          e_after ...)))
   (--> ((name store any)
         (semas
          (variable_before v_before) ...
          (variable_sema number_n)
          (variable_after v_after) ...)
         (threads
          e_before ...
          (in-hole (name c e-ctxt) (semaphore-wait (semaphore variable_sema)))
          e_after ...))
        (store
            (semas
             (variable_before v_before) ...
             (variable_sema ,(if (= (term number_n) 1)
                                 (term none)
                                 (- (term number_n) 1)))
             (variable_after v_after) ...)
          (threads
           e_before ...
           (in-hole c (void))
           e_after ...)))
   (--> ((name store any)
         (semas
          (variable_before v_before) ...
          (variable_sema number_n)
          (variable_after v_after) ...)
         (threads
          e_before ...
          (in-hole (name c e-ctxt) (semaphore-post (semaphore variable_sema)))
          e_after ...))
        (store
            (semas
             (variable_before v_before) ...
             (variable_sema ,(+ (term number_n) 1))
             (variable_after v_after) ...)
          (threads
           e_before ...
           (in-hole c (void))
           e_after ...)))
   
   (--> ((name store any)
         (semas
          (variable_before v_before) ...
          (variable_sema none)
          (variable_after v_after) ...)
         (threads
          e_before ...
          (in-hole (name c e-ctxt) (semaphore-post (semaphore variable_sema)))
          e_after ...))
        (store
            (semas
             (variable_before v_before) ...
             (variable_sema 1)
             (variable_after v_after) ...)
          (threads
           e_before ...
           (in-hole c (void))
           e_after ...)))))

(stepper reductions
         `((store (y (list)))
           (semas)
           (threads (set! y (cons 1 y))
                    (set! y (cons 2 y)))))

(stepper reductions
         `((store (y (list)))
           (semas (x 1))
           (threads (begin (semaphore-wait (semaphore x)) 
                           (set! y (cons 1 y)) 
                           (semaphore-post (semaphore x)))
                    (begin (semaphore-wait (semaphore x))
                           (set! y (cons 2 y))
                           (semaphore-post (semaphore x))))))
