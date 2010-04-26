#lang racket
;Dorai Sitaram
;1989, revised Feb. 1993, Mar. 1997

;logic variables and their manipulation
(define-struct iref (val) #:mutable)

(define *unbound* '_)

;;makes a fresh unbound ref;
;;unbound refs point to themselves
(define (make-ref [val *unbound*])
  (make-iref val))

(define _ make-ref)

(define ref? iref?)
(define deref iref-val)
(define set-ref! set-iref-val!)

(define (unbound-ref? r)
  (and (ref? r) (eq? (deref r) *unbound*)))

(define (unbind-ref! r)
  (set-ref! r *unbound*))

;frozen logic vars
(define-struct frozen (val))
(define (freeze-ref r)
  (make-ref (make-frozen r)))
(define (thaw-frozen-ref r)
  (frozen-val (deref r)))
(define (frozen-ref? r)
  (frozen? (deref r)))

;deref a structure completely (except the frozen ones, i.e.)

(define (deref* s)
  (cond ((ref? s)
         (if (frozen-ref? s) s
             (deref* (deref s))))
        ((pair? s) (cons (deref* (car s))
                         (deref* (cdr s))))
        ((vector? s)
         (vector-map deref* s))
        (else s)))

;%let introduces new logic variables

(define-syntax %let
  (syntax-rules ()
    ((%let (x ...) . e)
     (let ((x (_)) ...)
       . e))))

;the unify predicate

(define schelog-use-occurs-check? (make-parameter #f))

(define (occurs-in? var term)
  (and (schelog-use-occurs-check?)
       (let loop ((term term))
         (cond ((eqv? var term) #t)
               ((ref? term)
                (cond ((unbound-ref? term) #f)
                      ((frozen-ref? term) #f)
                      (else (loop (deref term)))))
               ((pair? term)
                (or (loop (car term)) (loop (cdr term))))
               ((vector? term)
                (loop (vector->list term)))
               (else #f)))))

(define (unify t1 t2)
  (lambda (fk)
    (letrec
        ((cleanup-n-fail
          (lambda (s)
            (for-each unbind-ref! s)
            (fk 'fail)))
         (unify1
          (lambda (t1 t2 s)
            ;(printf "unify1 ~s ~s~%" t1 t2)
            (cond ((eqv? t1 t2) s)
                  ((ref? t1)
                   (cond ((unbound-ref? t1)
                          (cond ((occurs-in? t1 t2)
                                 (cleanup-n-fail s))
                                (else 
                                 (set-ref! t1 t2)
                                 (cons t1 s))))
                         ((frozen-ref? t1)
                          (cond ((ref? t2)
                                 (cond ((unbound-ref? t2)
                                        ;(printf "t2 is unbound~%")
                                        (unify1 t2 t1 s))
                                       ((frozen-ref? t2)
                                        (cleanup-n-fail s))
                                       (else
                                        (unify1 t1 (deref t2) s))))
                                (else (cleanup-n-fail s))))
                         (else 
                          ;(printf "derefing t1~%") 
                          (unify1 (deref t1) t2 s))))
                  ((ref? t2) (unify1 t2 t1 s))
                  ((and (pair? t1) (pair? t2))
                   (unify1 (cdr t1) (cdr t2)
                           (unify1 (car t1) (car t2) s)))
                  ((and (string? t1) (string? t2))
                   (if (string=? t1 t2) s
                       (cleanup-n-fail s)))
                  ((and (vector? t1) (vector? t2))
                   (unify1 (vector->list t1)
                           (vector->list t2) s))
                  (else
                   (for-each unbind-ref! s)
                   (fk 'fail))))))
      (let ((s (unify1 t1 t2 '())))
        (lambda (d)
          (cleanup-n-fail s))))))

(define %= unify)

;disjunction

(define-syntax %or
  (syntax-rules ()
    ((%or g ...)
     (lambda (__fk)
       (let/cc __sk
         (let/cc __fk
             (__sk ((deref* g) __fk)))
          ...
          (__fk 'fail))))))

;conjunction

(define-syntax %and
  (syntax-rules ()
    ((%and g ...)
     (lambda (__fk)
       (let* ((__fk ((deref* g) __fk))
              ...)
         __fk)))))

; XXX JM This should work better
(define (! fk) (error '! "May only be used inside goal expression."))

;cut

(define-syntax (%cut-delimiter stx)
  (syntax-case stx ()
    ((%cut-delimiter g)
     (with-syntax ([! (datum->syntax stx '!)])
       (syntax/loc stx
         (lambda (__fk)
           (let ((! (lambda (__fk2) __fk)))
             ((deref* g) __fk))))))))

;Prolog-like sugar

(define-syntax (%rel stx)
  (syntax-case stx ()
    ((%rel (v ...) ((a ...) subgoal ...) ...)
     (with-syntax ([! (datum->syntax stx '!)])
       (syntax/loc stx
         (lambda __fmls
           (lambda (__fk)
             (let/cc __sk
               (let ((! (lambda (fk1) __fk)))
                 (%let (v ...)
                       (let/cc __fk
                         (let* ((__fk ((%= __fmls (list a ...)) __fk))
                                (__fk ((deref* subgoal) __fk))
                                ...)
                           (__sk __fk)))
                       ...
                       (__fk 'fail)))))))))))

;the fail and true preds

(define %fail
  (lambda (fk) (fk 'fail)))

(define %true
  (lambda (fk) fk))

;for structures ("functors"), use Scheme's list and vector
;functions and anything that's built using them.

;arithmetic

(define-syntax %is
  (syntax-rules (quote)
    ((%is v e)
     (lambda (__fk)
       ((%= v (%is (1) e __fk)) __fk)))
    
    ((%is (1) (quote x) fk) (quote x))
    ((%is (1) (x ...) fk)
     ((%is (1) x fk) ...))
    ((%is (1) x fk)
     (if (and (ref? x) (unbound-ref? x))
         (fk 'fail) (deref* x)))))

;defining arithmetic comparison operators

(define ((make-binary-arithmetic-relation f) x y)
  (%is #t (f x y)))

(define %=:= (make-binary-arithmetic-relation =))
(define %> (make-binary-arithmetic-relation >))
(define %>= (make-binary-arithmetic-relation >=))
(define %< (make-binary-arithmetic-relation <))
(define %<= (make-binary-arithmetic-relation <=))
(define %=/= (make-binary-arithmetic-relation (compose not =)))

;type predicates

(define (constant? x)
  (cond ((ref? x)
         (cond ((unbound-ref? x) #f)
               ((frozen-ref? x) #t)
               (else (constant? (deref x)))))
        ((pair? x) #f)
        ((vector? x) #f)
        (else #t)))

(define (compound? x)
  (cond ((ref? x)
         (cond ((unbound-ref? x) #f)
               ((frozen-ref? x) #f)
               (else (compound? (deref x)))))
        ((pair? x) #t)
        ((vector? x) #t)
        (else #f)))

(define (%constant x)
  (lambda (fk)
    (if (constant? x) fk (fk 'fail))))

(define (%compound x)
  (lambda (fk)
    (if (compound? x) fk (fk 'fail))))

;metalogical type predicates

(define (var? x)
  (cond ((ref? x)
         (cond ((unbound-ref? x) #t)
               ((frozen-ref? x) #f)
               (else (var? (deref x)))))
        ((pair? x) (or (var? (car x)) (var? (cdr x))))
        ((vector? x) (var? (vector->list x)))
        (else #f)))

(define (%var x)
  (lambda (fk) (if (var? x) fk (fk 'fail))))

(define (%nonvar x)
  (lambda (fk) (if (var? x) (fk 'fail) fk)))

; negation of unify

(define ((make-negation p) . args) 
  ;basically inlined cut-fail
  (lambda (fk)
    (if (let/cc k
          ((apply p args) (lambda (d) (k #f))))
        (fk 'fail)
        fk)))

(define %/=
  (make-negation %=))

;identical

(define (ident? x y)
  (cond ((ref? x)
         (cond ((unbound-ref? x)
                (cond ((ref? y)
                       (cond ((unbound-ref? y) (eq? x y))
                             ((frozen-ref? y) #f)
                             (else (ident? x (deref y)))))
                      (else #f)))
               ((frozen-ref? x)
                (cond ((ref? y)
                       (cond ((unbound-ref? y) #f)
                             ((frozen-ref? y) (eq? x y))
                             (else (ident? x (deref y)))))
                      (else #f)))
               (else (ident? (deref x) y))))
        ((pair? x)
         (cond ((ref? y)
                (cond ((unbound-ref? y) #f)
                      ((frozen-ref? y) #f)
                      (else (ident? x (deref y)))))
               ((pair? y)
                (and (ident? (car x) (car y))
                     (ident? (cdr x) (cdr y))))
               (else #f)))
        ((vector? x)
         (cond ((ref? y)
                (cond ((unbound-ref? y) #f)
                      ((frozen-ref? y) #f)
                      (else (ident? x (deref y)))))
               ((vector? y)
                (ident? (vector->list x)
                                (vector->list y)))
               (else #f)))
        (else
         (cond ((ref? y)
                (cond ((unbound-ref? y) #f)
                      ((frozen-ref? y) #f)
                      (else (ident? x (deref y)))))
               ((pair? y) #f)
               ((vector? y) #f)
               (else (eqv? x y))))))

(define (%== x y)
  (lambda (fk) (if (ident? x y) fk (fk 'fail))))

(define (%/== x y)
  (lambda (fk) (if (ident? x y) (fk 'fail) fk)))

;variables as objects

(define (freeze s)
  (let ((dict '()))
    (let loop ((s s))
      (cond ((ref? s)
             (cond ((or (unbound-ref? s) (frozen-ref? s))
                    (let ((x (assq s dict)))
                      (if x (cdr x)
                          (let ((y (freeze-ref s)))
                            (set! dict (cons (cons s y) dict))
                            y))))
                   ;((frozen-ref? s) s) ;?
                   (else (loop (deref s)))))
            ((pair? s) (cons (loop (car s)) (loop (cdr s))))
            ((vector? s)
             (list->vector (map loop (vector->list s))))
            (else s)))))

(define (melt f)
  (cond ((ref? f)
         (cond ((unbound-ref? f) f)
               ((frozen-ref? f) (thaw-frozen-ref f))
               (else (melt (deref f)))))
        ((pair? f)
         (cons (melt (car f)) (melt (cdr f))))
        ((vector? f)
         (list->vector (map melt (vector->list f))))
        (else f)))

(define (melt-new f)
  (let ((dict '()))
    (let loop ((f f))
      (cond ((ref? f)
             (cond ((unbound-ref? f) f)
                   ((frozen-ref? f)
                    (let ((x (assq f dict)))
                      (if x (cdr x)
                          (let ((y (_)))
                            (set! dict (cons (cons f y) dict))
                            y))))
                   (else (loop (deref f)))))
            ((pair? f) (cons (loop (car f)) (loop (cdr f))))
            ((vector? f)
             (list->vector (map loop (vector->list f))))
            (else f)))))

(define (copy s)
  (melt-new (freeze s)))

(define (%freeze s f)
  (lambda (fk)
    ((%= (freeze s) f) fk)))

(define (%melt f s)
  (lambda (fk)
    ((%= (melt f) s) fk)))

(define (%melt-new f s)
  (lambda (fk)
    ((%= (melt-new f) s) fk)))

(define (%copy s c)
  (lambda (fk)
    ((%= (copy s) c) fk)))

;negation as failure

(define (%not g)
  (lambda (fk)
    (if (let/cc k
          ((deref* g) (lambda (d) (k #f))))
        (fk 'fail) fk)))

;assert, asserta

(define (%empty-rel . args)
  %fail)

(define-syntax %assert
  (syntax-rules ()
    ((%assert rel-name (v ...) ((a ...) subgoal ...) ...)
     (set! rel-name
           (let ((__old-rel rel-name)
                 (__new-addition (%rel (v ...) ((a ...) subgoal ...) ...)))
             (lambda __fmls
               (%or (apply __old-rel __fmls)
                    (apply __new-addition __fmls))))))))

(define-syntax %assert-a
  (syntax-rules ()
    ((%assert-a rel-name (v ...) ((a ...) subgoal ...) ...)
     (set! rel-name
           (let ((__old-rel rel-name)
                 (__new-addition (%rel (v ...) ((a ...) subgoal ...) ...)))
             (lambda __fmls
               (%or (apply __new-addition __fmls)
                    (apply __old-rel __fmls))))))))

;set predicates

(define (set-cons e s)
  (if (member e s) s (cons e s)))

(define-syntax %free-vars
  (syntax-rules ()
    ((%free-vars (v ...) g)
     (cons 'goal-with-free-vars
           (cons (list v ...) g)))))

(define (goal-with-free-vars? x)
  (and (pair? x) (eq? (car x) 'goal-with-free-vars)))

(define ((make-bag-of kons) lv goal bag)
  (let ((fvv '()))
    (when (goal-with-free-vars? goal)
      (set! fvv (cadr goal))
      (set! goal (cddr goal)))
    (make-bag-of-aux kons fvv lv goal bag)))

(define (make-bag-of-aux kons fvv lv goal bag)
  (lambda (fk)
    (let/cc sk
      (let ((lv2 (cons fvv lv)))
        (let* ((acc '())
               (fk-final
                (lambda (d)
                  ;;(set! acc (reverse! acc))
                  (sk ((separate-bags fvv bag acc) fk))))
               (fk-retry (goal fk-final)))
          (set! acc (kons (deref* lv2) acc))
          (fk-retry 'retry))))))

(define (separate-bags fvv bag acc)
  ;;(format #t "Accum: ~s~%" acc)
  (let ((bags (let loop ((acc acc)
                         (current-fvv #f) (current-bag '())
                         (bags '()))
                (if (null? acc)
                    (cons (cons current-fvv current-bag) bags)
                    (let ((x (car acc)))
                      (let ((x-fvv (car x)) (x-lv (cdr x)))
                        (if (or (not current-fvv) (equal? x-fvv current-fvv))
                            (loop (cdr acc) x-fvv (cons x-lv current-bag) bags)
                            (loop (cdr acc) x-fvv (list x-lv)
                                  (cons (cons current-fvv current-bag) bags)))))))))
    ;;(format #t "Bags: ~a~%" bags)
    (if (null? bags) (%= bag '())
        (let ((fvv-bag (cons fvv bag)))
          (let loop ((bags bags))
            (if (null? bags) %fail
                (%or (%= fvv-bag (car bags))
                     (loop (cdr bags)))))))))

(define %bag-of (make-bag-of cons))
(define %set-of (make-bag-of set-cons))

;%bag-of-1, %set-of-1 hold if there's at least one solution

(define (%bag-of-1 x g b)
  (%and (%bag-of x g b)
        (%= b (cons (_) (_)))))

(define (%set-of-1 x g s)
  (%and (%set-of x g s)
        (%= s (cons (_) (_)))))

;user interface

;(%which (v ...) query) returns #f if query fails and instantiations
;of v ... if query succeeds.  In the latter case, type (%more) to
;retry query for more instantiations.

(define *more-k* (box 'forward))
(define *more-fk* (box 'forward))

(define-syntax %which
  (syntax-rules ()
    ((%which (v ...) g)
     (%let (v ...)
           (let/cc __qk
             (set-box! *more-k* __qk)
             (set-box! *more-fk*
                       ((deref* g)
                        (lambda (d)
                          (set-box! *more-fk* #f)
                          ((unbox *more-k*) #f))))
             ((unbox *more-k*)
              (list (list 'v (deref* v))
                    ...)))))))

(define (%more)
  (let/cc k
    (set-box! *more-k* k)
    (if (unbox *more-fk*)
        ((unbox *more-fk*) 'more)
        #f)))

;end of embedding code.  The following are
;some utilities, written in Schelog

(define (%member x y)
  (%let (xs z zs)
        (%or
         (%= y (cons x xs))
         (%and (%= y (cons z zs))
               (%member x zs)))))

(define (%if-then-else p q r)
  (%cut-delimiter
   (%or
    (%and p ! q)
    r)))

;the above could also have been written in a more
;Prolog-like fashion, viz.

(define %append
  (%rel (x xs ys zs)
        (('() ys ys))
        (((cons x xs) ys (cons x zs))
         (%append xs ys zs))))

(define %repeat
  ;;failure-driven loop
  (%rel ()
        (())
        (() (%repeat))))

(provide %/= %/== %< %<= %= %=/= %== %> %>= %and %append
         %assert %assert-a %bag-of %bag-of-1 %compound
         %constant %copy %empty-rel %fail %free-vars
         %freeze %if-then-else %is %let %melt %melt-new
         %member %nonvar %not %more %or %rel %repeat
         schelog-use-occurs-check?
         %set-of %set-of-1 %true %var %which _ !)