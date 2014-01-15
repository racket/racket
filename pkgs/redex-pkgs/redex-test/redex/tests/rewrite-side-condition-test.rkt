#lang racket/base
(require (for-syntax redex/private/rewrite-side-conditions
                     racket/base)
         redex/private/term ;; to get bindings for 'in-hole' etc
         rackunit)

(define-syntax (rsc stx)
  (syntax-case stx ()
    [(_ pat (nts ...) bind-names?)
     (with-handlers ((exn:fail:syntax?
                      (λ (x)
                        #`'#,(exn-message x))))
       (with-syntax ([(ignore pat (vars ...) (vars/ellipses ...))
                      (rewrite-side-conditions/check-errs 
                       (syntax->datum #'(nts ...))
                       'rsc
                       (syntax-e #'bind-names?)
                       #'pat)])
         #'(list `pat
                 `(vars ...)
                 `(vars/ellipses ...))))]))

(check-equal? (rsc 1 () #t) `(1 () ()))
(check-equal? (rsc (1) () #t) `((list 1) () ()))
(check-equal? (rsc (_ _ (name x _)) () #t) `((list any any (name x any)) (x) (x)))
(check-equal? (rsc (1 ...) () #t) `((list (repeat 1 #f #f)) () ()))
(check-equal? (rsc (1 ..._2) () #t) `((list (repeat 1 ..._2 #f)) (..._2) (..._2)))
(check-equal? (rsc (1 ..._2 1 ..._2) () #t) 
              `((list (repeat 1 ..._2 #f) (repeat 1 ..._2 #f)) (..._2 ..._2) (..._2 ..._2)))
(check-equal? (rsc (1 ..._!_3) () #t) `((list (repeat 1 #f #f)) () ()))
(check-equal? (rsc (1 ..._!_3 1 ..._!_3) () #t)
              `((list (repeat 1 #f ..._!_3) (repeat 1 #f ..._!_3)) () ()))

(check-equal? (rsc x (x) #t) `((name x (nt x)) (x) (x)))
(check-equal? (rsc x (x) #f) `((nt x) () ()))
(check-equal? (rsc x_1 (x) #t) `((name x_1 (nt x)) (x_1) (x_1)))
(check-equal? (rsc x_1 (x) #f) `((name x_1 (nt x)) (x_1) (x_1)))
(check-equal? (rsc any (x) #t) `((name any any) (any) (any)))
(check-equal? (rsc any (x) #f) `(any () ()))
(check-equal? (rsc any_1 (x) #t) `((name any_1 any) (any_1) (any_1)))
(check-equal? (rsc any_1 (x) #f) `((name any_1 any) (any_1) (any_1)))
(check-equal? (rsc ((x ...) ...) (x) #t) 
              `((list (repeat (list (repeat (name x (nt x)) #f #f)) #f #f))
                (x)
                (((x ...) ...))))
(check-equal? (rsc (any_1 any_1) (x) #f) `((list (name any_1 any) (name any_1 any)) 
                                           (any_1 any_1) 
                                           (any_1 any_1)))
(check-equal? (rsc (in-hole (hole a #f (hide-hole hole)) (cross x)) '(x) #f)
              `((in-hole (list hole a #f (hide-hole hole)) (cross x-x))
                ()
                ()))

(check-regexp-match
 #rx"any_1 is overly"
 (rsc (any_1 ..._!_1 any_1 ..._!_1) () #f))
(check-regexp-match
 #rx"any_1 is overly constrained"
 (rsc (any_1 ..._!_1 any_2 ..._!_1 (any_1 any_2) ...) () #f))
(check-regexp-match
 #rx"any_1 is overly constrained"
(rsc (any_1 ..._!_1 any_2 ..._!_1 any_1 ..._1 any_2 ..._1) () #f))
(check-regexp-match
 #rx"any_1 is overly constrained"
 (rsc (any_1 ..._!_1 any_2 ..._!_1 (any_1 any_3) ... (any_3 any_2) ...) () #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test the normalization of the ellipses underscores
;;
(check-equal? (car (rsc (x_1 ..._1 x_2 ..._2 x_2 ..._1) (x) #t))
              '(list (repeat (name x_1 (nt x)) ..._1 #f)
                     (repeat (name x_2 (nt x)) ..._1 #f)
                     (repeat (name x_2 (nt x)) ..._1 #f)))
(check-equal? (car (rsc ((x_1 ..._1 x_1 ..._2) (x_2 ..._1 x_2 ..._2) x_3 ..._2) (x) #t))
              '(list (list (repeat (name x_1 (nt x)) ..._2 #f) 
                           (repeat (name x_1 (nt x)) ..._2 #f))
                     (list (repeat (name x_2 (nt x)) ..._2 #f)
                           (repeat (name x_2 (nt x)) ..._2 #f)) 
                     (repeat (name x_3 (nt x)) ..._2 #f)))
(check-equal? (car (rsc (x_1 ..._1 x ..._2 x_1 ..._2) (x) #t))
              '(list (repeat (name x_1 (nt x)) ..._2 #f)
                     (repeat (name x (nt x)) ..._2 #f)
                     (repeat (name x_1 (nt x)) ..._2 #f)))
(check-equal? (car (rsc (x_1 ..._1 x_2 ..._2 (x_1 x_2) ..._3) (x) #t))
              '(list (repeat (name x_1 (nt x)) ..._3 #f)
                     (repeat (name x_2 (nt x)) ..._3 #f)
                     (repeat (list (name x_1 (nt x)) (name x_2 (nt x))) ..._3 #f)))
(check-equal? (car (rsc ((x_1 ..._1) ..._2 x_2 ..._3 (x_1 ..._4 x_2) ..._5) (x) #t))
              '(list (repeat (list (repeat (name x_1 (nt x)) ..._4 #f)) ..._5 #f)
                     (repeat (name x_2 (nt x)) ..._5 #f)
                     (repeat (list (repeat (name x_1 (nt x)) ..._4 #f)
                                   (name x_2 (nt x)))
                             ..._5
                             #f)))
(check-equal? (car (rsc ((x_1 ..._1) ..._2 (x_1 ..._3) ..._4 (x_1 ..._5) ..._6) (x) #t))
              '(list (repeat (list (repeat (name x_1 (nt x)) ..._5 #f)) ..._6 #f)
                     (repeat (list (repeat (name x_1 (nt x)) ..._5 #f)) ..._6 #f)
                     (repeat (list (repeat (name x_1 (nt x)) ..._5 #f)) ..._6 #f)))

(check-equal? (car (rsc (x_1 ..._1 x_1 ..._2 x_2 ..._1 x_2 ..._4 x_2 ..._3) (x) #t))
              '(list (repeat (name x_1 (nt x)) ..._3 #f)
                     (repeat (name x_1 (nt x)) ..._3 #f)
                     (repeat (name x_2 (nt x)) ..._3 #f)
                     (repeat (name x_2 (nt x)) ..._3 #f)
                     (repeat (name x_2 (nt x)) ..._3 #f)))

(check-equal? (car (rsc (x_1 ... x_1 ..._!_1 x_1 ..._1) (x) #t))
              '(list (repeat (name x_1 (nt x)) ..._1 #f)
                     (repeat (name x_1 (nt x)) ..._1 #f)
                     (repeat (name x_1 (nt x)) ..._1 #f)))

(check-equal? (car (rsc (x_1 ... x_1 ..._!_1 x_1 ..._1 x_2 ..._!_1) (x) #t))
              '(list (repeat (name x_1 (nt x)) ..._1 #f)
                     (repeat (name x_1 (nt x)) ..._1 ..._!_1)
                     (repeat (name x_1 (nt x)) ..._1 #f)
                     (repeat (name x_2 (nt x)) #f ..._!_1)))

(check-equal? (car (rsc ((3 ..._1) ..._2 (4 ..._1) ..._3) (x) #t))
              '(list (repeat (list (repeat 3 ..._1 #f)) ..._3 #f)
                     (repeat (list (repeat 4 ..._1 #f)) ..._3 #f)))

(check-equal? (car (rsc (x ..._1 x ..._2 
                           variable ..._2 variable ..._3 variable_1 ..._3 variable_1 ..._4)
                        (x) #t))
              '(list (repeat (name x (nt x)) ..._4 #f)
                     (repeat (name x (nt x)) ..._4 #f)
                     (repeat (name variable variable) ..._4 #f)
                     (repeat (name variable variable) ..._4 #f)
                     (repeat (name variable_1 variable) ..._4 #f)
                     (repeat (name variable_1 variable) ..._4 #f)))

(check-equal? (car (rsc (z_1 ... z_2 ..._!_1 (z_1 z_2) ...) (z) #t))
              '(list (repeat (name z_1 (nt z)) ..._r3 #f)
                     (repeat (name z_2 (nt z)) ..._r3 #f)
                     (repeat (list (name z_1 (nt z))
                                   (name z_2 (nt z)))
                             ..._r3
                             #f)))

(check-equal? (car (rsc (z_1 ... z_2 ..._!_1 z_3 ..._!_1 (z_1 z_2) ...) (z) #t))
              '(list (repeat (name z_1 (nt z)) ..._r4 #f)
                     (repeat (name z_2 (nt z)) ..._r4 ..._!_1)
                     (repeat (name z_3 (nt z)) #f ..._!_1)
                     (repeat (list (name z_1 (nt z))
                                   (name z_2 (nt z)))
                             ..._r4
                             #f)))

;;
;; test the normalization of the ellipses underscores
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
