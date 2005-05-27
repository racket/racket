#|
  
semaphores make things much more predictable...
  
|#  

(module semaphores mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss")
  
  (reduction-steps-cutoff 100)
  
  (define lang
    (language
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
        (void))))
  
  (define reductions
    (list
     (reduction lang
                (in-hole (name c p-ctxt) (begin v e_1 e_2 e_rest ...))
                (plug 
                 (term c)
                 (term (begin e_1 e_2 e_rest ...))))
     (reduction lang
                (in-hole (name c p-ctxt) 
                         (cons v_1 (list v_2s ...)))
                (plug 
                 (term c)
                 (term (list v_1 v_2s ...))))
     (reduction lang
                (in-hole (name c p-ctxt) (begin v e_1))
                (plug (term c) (term e_1)))
     (reduction lang
                (in-hole (name c p-ctxt) (begin v_1))
                (plug (term c) (term v_1)))
     (reduction lang
                ((store
                  (name befores (variable v)) ...
                  ((name x variable) (name v v))
                  (name afters (variable v)) ...)
                 (name semas any)
                 (threads
                  (name e-before e) ...
                  (in-hole (name c e-ctxt) (name x variable))
                  (name e-after e) ...))
                (term 
                 ((store
                   befores ...
                   (x v)
                   afters ...)
                  semas
                  (threads
                   e-before ...
                   (in-hole c v)
                   e-after ...))))
     (reduction lang
                ((store
                  (name befores (variable v)) ...
                  (variable_i v)
                  (name afters (variable v)) ...)
                 (name semas any)
                 (threads
                  (name e-before e) ...
                  (in-hole (name c e-ctxt) (set! variable_i v_new))
                  (name e-after e) ...))
                (term 
                 ((store
                   befores ...
                   (variable_i v_new)
                   afters ...)
                  semas
                  (threads
                   e-before ...
                   (in-hole c (void))
                   e-after ...))))
     (reduction lang
                ((name store any)
                 (semas
                  (name befores (variable v)) ...
                  (variable_sema number_n)
                  (name afters (variable v)) ...)
                 (threads
                  (name e-before e) ...
                  (in-hole (name c e-ctxt) (semaphore-wait (semaphore variable_sema)))
                  (name e-after e) ...))
                (term 
                 (store
                  (semas
                   befores ...
                   (variable_sema ,(if (= (term number_n) 1)
                                       (term none)
                                       (- (term number_n) 1)))
                   afters ...)
                   (threads
                    e-before ...
                    (in-hole c (void))
                    e-after ...))))
     (reduction lang
                ((name store any)
                 (semas
                  (name befores (variable v)) ...
                  (variable_sema number_n)
                  (name afters (variable v)) ...)
                 (threads
                  (name e-before e) ...
                  (in-hole (name c e-ctxt) (semaphore-post (semaphore variable_sema)))
                  (name e-after e) ...))
                (term 
                 (store
                  (semas
                   befores ...
                   (variable_sema ,(+ (term number_n) 1))
                   afters ...)
                  (threads
                   e-before ...
                   (in-hole c (void))
                   e-after ...))))
     
     (reduction lang
                ((name store any)
                 (semas
                  (name befores (variable v)) ...
                  (variable_sema none)
                  (name afters (variable v)) ...)
                 (threads
                  (name e-before e) ...
                  (in-hole (name c e-ctxt) (semaphore-post (semaphore variable_sema)))
                  (name e-after e) ...))
                (term 
                 (store
                  (semas
                   befores ...
                   (variable_sema 1)
                   afters ...)
                  (threads
                   e-before ...
                   (in-hole c (void))
                   e-after ...))))))
  
  (traces lang
       reductions
       `((store (y (list)))
         (semas)
         (threads (set! y (cons 1 y))
                  (set! y (cons 2 y)))))
  
  (traces lang
       reductions
       `((store (y (list)))
         (semas (x 1))
         (threads (begin (semaphore-wait (semaphore x)) 
                         (set! y (cons 1 y)) 
                         (semaphore-post (semaphore x)))
                  (begin (semaphore-wait (semaphore x))
                         (set! y (cons 2 y))
                         (semaphore-post (semaphore x)))))))
