#lang racket
(require scheme/stxparam
         "unify.rkt")

;Dorai Sitaram
;1989, revised Feb. 1993, Mar. 1997

(define-syntax %let
  (syntax-rules ()
    ((%let (x ...) . e)
     (let ((x (_)) ...)
       . e))))

(define %= unify)

(define-syntax %or
  (syntax-rules ()
    ((%or g ...)
     (lambda (__fk)
       (let/cc __sk
         (let/cc __fk
           (__sk ((logic-var-val* g) __fk)))
         ...
         (__fk 'fail))))))

(define-syntax %and
  (syntax-rules ()
    ((%and g ...)
     (lambda (__fk)
       (let* ((__fk ((logic-var-val* g) __fk))
              ...)
         __fk)))))

(define-syntax-parameter !
  (λ (stx) (raise-syntax-error '! "May only be used syntactically inside %rel or %cut-delimiter expression." stx)))

(define-syntax %cut-delimiter
  (syntax-rules ()
    ((%cut-delimiter g)
     (lambda (__fk)
       (let ((this-! (lambda (__fk2) __fk)))
         (syntax-parameterize 
          ([! (make-rename-transformer #'this-!)])
          ((logic-var-val* g) __fk)))))))

(define-syntax %rel
  (syntax-rules ()
    ((%rel (v ...) ((a ...) subgoal ...) ...)
     (lambda __fmls
       (lambda (__fk)
         (let/cc __sk
           (let ((this-! (lambda (fk1) __fk)))
             (syntax-parameterize 
              ([! (make-rename-transformer #'this-!)])
              (%let (v ...)
                    (let/cc __fk
                      (let* ((__fk ((%= __fmls (list a ...)) __fk))
                             (__fk ((logic-var-val* subgoal) __fk))
                             ...)
                        (__sk __fk)))
                    ...
                    (__fk 'fail))))))))))

(define %fail
  (lambda (fk) (fk 'fail)))

(define %true
  (lambda (fk) fk))

(define-syntax %is
  (syntax-rules ()
    ((%is v e)
     (lambda (__fk)
       ((%= v (%is/fk e __fk)) __fk)))))
(define-syntax %is/fk
  (syntax-rules (quote)
    ((%is/fk (quote x) fk) (quote x))
    ((%is/fk (x ...) fk)
     ((%is/fk x fk) ...))
    ((%is/fk x fk)
     (if (and (logic-var? x) (unbound-logic-var? x))
         (fk 'fail) (logic-var-val* x)))))

(define ((make-binary-arithmetic-relation f) x y)
  (%and (%is #t (number? x))
        (%is #t (number? y))
        (%is #t (f x y))))

(define %=:= (make-binary-arithmetic-relation =))
(define %> (make-binary-arithmetic-relation >))
(define %>= (make-binary-arithmetic-relation >=))
(define %< (make-binary-arithmetic-relation <))
(define %<= (make-binary-arithmetic-relation <=))
(define %=/= (make-binary-arithmetic-relation (compose not =)))

(define (%constant x)
  (lambda (fk)
    (if (constant? x) fk (fk 'fail))))

(define (%compound x)
  (lambda (fk)
    (if (is-compound? x) fk (fk 'fail))))

(define (%var x)
  (lambda (fk) (if (var? x) fk (fk 'fail))))

(define (%nonvar x)
  (lambda (fk) (if (var? x) (fk 'fail) fk)))

(define ((make-negation p) . args) 
  ;basically inlined cut-fail
  (lambda (fk)
    (if (let/cc k
          ((apply p args) (lambda (d) (k #f))))
        (fk 'fail)
        fk)))

(define %/=
  (make-negation %=))

(define (%== x y)
  (lambda (fk) (if (ident? x y) fk (fk 'fail))))

(define (%/== x y)
  (lambda (fk) (if (ident? x y) (fk 'fail) fk)))

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

(define (%not g)
  (lambda (fk)
    (if (let/cc k
          ((logic-var-val* g) (lambda (d) (k #f))))
        (fk 'fail) fk)))

(define (%empty-rel . args)
  %fail)

(define-syntax %assert!
  (syntax-rules ()
    ((_ rel-name (v ...) ((a ...) subgoal ...) ...)
     (set! rel-name
           (let ((__old-rel rel-name)
                 (__new-addition (%rel (v ...) ((a ...) subgoal ...) ...)))
             (lambda __fmls
               (%or (apply __old-rel __fmls)
                    (apply __new-addition __fmls))))))))

(define-syntax %assert-after!
  (syntax-rules ()
    ((_ rel-name (v ...) ((a ...) subgoal ...) ...)
     (set! rel-name
           (let ((__old-rel rel-name)
                 (__new-addition (%rel (v ...) ((a ...) subgoal ...) ...)))
             (lambda __fmls
               (%or (apply __new-addition __fmls)
                    (apply __old-rel __fmls))))))))

(define (set-cons e s)
  (if (member e s) s (cons e s)))

(define-struct goal-with-free-vars (vars subgoal))

(define-syntax %free-vars
  (syntax-rules ()
    ((%free-vars (v ...) g)
     (make-goal-with-free-vars
           (list v ...) 
           g))))

(define ((make-bag-of kons) lv goal bag)
  (let ((fvv '()))
    (when (goal-with-free-vars? goal)
      (set! fvv (goal-with-free-vars-vars goal))
      (set! goal (goal-with-free-vars-subgoal goal)))
    (make-bag-of-aux kons fvv lv goal bag)))

(define (make-bag-of-aux kons fvv lv goal bag)
  (lambda (fk)
    (let/cc sk
      (let ((lv2 (cons fvv lv)))
        (let* ((acc '())
               (fk-final
                (lambda (d)
                  (sk ((separate-bags fvv bag acc) fk))))
               (fk-retry (goal fk-final)))
          (set! acc (kons (logic-var-val* lv2) acc))
          (fk-retry 'retry))))))

(define (separate-bags fvv bag acc)
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
    (if (null? bags) (%= bag '())
        (let ((fvv-bag (cons fvv bag)))
          (let loop ((bags bags))
            (if (null? bags) %fail
                (%or (%= fvv-bag (car bags))
                     (loop (cdr bags)))))))))

(define %bag-of (make-bag-of cons))
(define %set-of (make-bag-of set-cons))

(define (%bag-of-1 x g b)
  (%and (%bag-of x g b)
        (%= b (cons (_) (_)))))

(define (%set-of-1 x g s)
  (%and (%set-of x g s)
        (%= s (cons (_) (_)))))

(define *more-k* (box 'forward))
(define *more-fk* (box (λ (d) (error '%more "No active %which"))))

(define-syntax %which
  (syntax-rules ()
    ((%which (v ...) g)
     (%let (v ...)
           (let/cc __qk
             (set-box! *more-k* __qk)
             (set-box! *more-fk*
                       ((logic-var-val* g)
                        (lambda (d)
                          (set-box! *more-fk* #f)
                          ((unbox *more-k*) #f))))
             ((unbox *more-k*)
              (list (cons 'v (logic-var-val* v))
                    ...)))))))

(define (%more)
  (let/cc k
    (set-box! *more-k* k)
    (if (unbox *more-fk*)
        ((unbox *more-fk*) 'more)
        #f)))

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

(define %append
  (%rel (x xs ys zs)
        (('() ys ys))
        (((cons x xs) ys (cons x zs))
         (%append xs ys zs))))

(define %repeat
  (%rel ()
        (())
        (() (%repeat))))

(define fk? (symbol? . -> . any))
(define goal/c 
  (or/c goal-with-free-vars?
        (fk? . -> . fk?)))
(define relation/c
  (->* () () #:rest (listof unifiable?) goal/c))

; XXX Add contracts in theses macro expansions
(provide %and %assert! %assert-after! %cut-delimiter %free-vars %is %let
         %or %rel %which !)
(provide/contract
 [goal/c contract?]
 [logic-var? (any/c . -> . boolean?)]
 [atom? (any/c . -> . boolean?)]
 [atomic-struct? (any/c . -> . boolean?)]
 [compound-struct? (any/c . -> . boolean?)]
 [compound? (any/c . -> . boolean?)]
 [unifiable? (any/c . -> . boolean?)]
 [answer-value? (any/c . -> . boolean?)]
 [answer? (any/c . -> . boolean?)]
 [%/= (unifiable? unifiable? . -> . goal/c)]
 [%/== (unifiable? unifiable? . -> . goal/c)]
 [%< (unifiable? unifiable? . -> . goal/c)]
 [%<= (unifiable? unifiable? . -> . goal/c)]
 [%= (unifiable? unifiable? . -> . goal/c)]
 [%=/= (unifiable? unifiable? . -> . goal/c)]
 [%=:= (unifiable? unifiable? . -> . goal/c)]
 [%== (unifiable? unifiable? . -> . goal/c)]
 [%> (unifiable? unifiable? . -> . goal/c)]
 [%>= (unifiable? unifiable? . -> . goal/c)]
 [%append (unifiable? unifiable? unifiable? . -> . goal/c)]
 [%bag-of (unifiable? goal/c unifiable? . -> . goal/c)]
 [%bag-of-1 (unifiable? goal/c unifiable? . -> . goal/c)]
 [%compound (unifiable? . -> . goal/c)]
 [%constant (unifiable? . -> . goal/c)]
 [%copy (unifiable? unifiable? . -> . goal/c)]
 [%empty-rel relation/c]
 [%fail goal/c]
 [%freeze (unifiable? unifiable? . -> . goal/c)]
 [%if-then-else (goal/c goal/c goal/c . -> . goal/c)]
 [%melt (unifiable? unifiable? . -> . goal/c)]
 [%melt-new (unifiable? unifiable? . -> . goal/c)]
 [%member (unifiable? unifiable? . -> . goal/c)]
 [%nonvar (unifiable? . -> . goal/c)]
 [%not (goal/c . -> . goal/c)]
 [%more (-> answer?)]
 [%repeat (-> goal/c)]
 [use-occurs-check? (parameter/c boolean?)]
 [%set-of (unifiable? goal/c unifiable? . -> . goal/c)]
 [%set-of-1 (unifiable? goal/c unifiable? . -> . goal/c)]
 [%true goal/c]
 [%var (unifiable? . -> . goal/c)]
 [_ (-> logic-var?)]) 
