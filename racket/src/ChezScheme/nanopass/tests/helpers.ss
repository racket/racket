;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (tests helpers)
  (export compose disjoin any every choose reverse-filter fold reduce
          constant? keyword? list-of-user-primitives list-of-system-primitives
          user-primitive? system-primitive? primitive? predicate-primitive?
          value-primitive? effect-primitive? effect-free-primitive? gen-label
          reset-seed gen-symbol set? iota with-values
          empty-set singleton-set
          add-element member? empty? union intersection difference
          variable? datum? list-index primapp sys-primapp app const-datum const
          var quoted-const time printf system interpret pretty-print format set-cons
          define-who)
  (import (rnrs) 
          (tests implementation-helpers) 
          (nanopass helpers)) 
  
  (define-syntax primapp
    (syntax-rules ()
      [(_ expr expr* ...) (expr expr* ...)])) 
  
  (define-syntax sys-primapp
    (syntax-rules ()
      [(_ expr expr* ...) (expr expr* ...)])) 
  
  (define-syntax app
    (syntax-rules ()
      [(_ expr expr* ...) (expr expr* ...)])) 
  
  (define-syntax const-datum
    (syntax-rules ()
      [(_ expr) (quote expr)])) 
  
  (define-syntax const
    (syntax-rules ()
      [(_ expr) expr])) 
  
  (define-syntax var
    (syntax-rules ()
      [(_ expr) expr])) 
  
  (define-syntax quoted-const
    (syntax-rules ()
      [(_ expr) (quote expr)])) 

  (define compose
    (case-lambda
      [() (lambda (x) x)]
      [(f) f]
      [(f . g*) (lambda (x) (f ((apply compose g*) x)))])) 
  
  (define disjoin
    (case-lambda
      [() (lambda (x) #f)]
      [(p?) p?]
      [(p? . q?*) (lambda (x) 
                    (or (p? x) ((apply disjoin q?*) x)))])) 
  
  (define any
    (lambda (pred? ls)
      (let loop ([ls ls])
        (cond
          [(null? ls) #f]
          [(pred? (car ls)) #t]
          [else (loop (cdr ls))])))) 
  
  (define every
    (lambda (pred? ls)
      (let loop ([ls ls])
        (cond
          [(null? ls) #t]
          [(pred? (car ls)) (loop (cdr ls))]
          [else #f])))) 
  
  (define choose
    (lambda (pred? ls)
      (fold (lambda (elt tail)
              (if (pred? elt)
                  (cons elt tail)
                  tail))
            '()
            ls))) 
  
  (define reverse-filter
    (lambda (pred? ls)
      (fold (lambda (elt tail)
              (if (pred? elt)
                  tail
                  (cons elt tail)))
            '()
            ls))) 
  
  ;; fold op base (cons a (cons b (cons c '()))) =
  ;; (op a (op b (op c base)))
  (define fold
    (lambda (op base ls)
      (let recur ([ls ls])
        (if (null? ls)
            base
            (op (car ls) (recur (cdr ls))))))) 
  
  ;; reduce op base (cons a (cons b (cons c '())))
  ;; (op c (op b (op a base)))
  (define reduce 
    (lambda (op base ls)
      (let loop ([ls ls] [ans base])
        (if (null? ls)
            ans
            (loop (cdr ls) (op (car ls) ans)))))) 
  
  ;;; General Scheme helpers for the compiler 
  (define constant?
    (disjoin null? number? char? boolean? string?)) 

  (define keyword?
    (lambda (x)
      (and (memq x '(quote set! if begin let letrec lambda)) #t))) 
  
  (define datum?
    (lambda (x)
      (or (constant? x)
          (null? x)
          (if (pair? x)
            (and (datum? (car x)) (datum? (cdr x)))
            (and (vector? x) (for-all datum? (vector->list x))))))) 
  
  (define variable? symbol?)
  
  (define list-of-user-primitives
    '(; not is a special case
      (not 1 not) 
      
      ; predicates
      (< 2 test)
      (<= 2 test)
      (= 2 test)
      (boolean? 1 test)
      (char? 1 test)
      (eq? 2 test)
      (integer? 1 test)
      (null? 1 test)
      (pair? 1 test)
      (procedure? 1 test)

      (vector? 1 test)
      (zero? 1 test) 
      
      ; value-producing
      (* 2 value)
      (+ 2 value)
      (- 2 value)
      (add1 1 value)
      (car 1 value)
      (cdr 1 value)
      (char->integer 1 value)
      (cons 2 value) 

      (make-vector 1 value)
      (quotient 2 value)
      (remainder 2 value) 
      
      (sub1 1 value) 
      
      (vector -1 value)
      (vector-length 1 value)
      (vector-ref 2 value)
      (void 0 value) 
      
      ; side-effecting
      (set-car! 2 effect)
      (set-cdr! 2 effect) 
      
      (vector-set! 3 effect))) 
  
  (define list-of-system-primitives ; these are introduced later by the compiler
    '(; value-producing
      (closure-ref 2 value)
      (make-closure 2 value)
      (procedure-code 1 value) 
      
      ; side-effecting
      (closure-set! 3 effect) 
      
      (fref 1 value)
      (fset! 2 effect)
      (fincr! 1 effect)
      (fdecr! 1 effect)
      (href 2 value)
      (hset! 3 effect)
      (logand 2 value)
      (sll 2 value)
      (sra 2 value))) 
  
  (define user-primitive?
    (lambda (x)
      (and (assq x list-of-user-primitives) #t))) 
  
  (define system-primitive?
    (lambda (x)
      (and (assq x list-of-system-primitives) #t))) 
  
  (define primitive?
    (lambda (x)
      (or (user-primitive? x) (system-primitive? x)))) 
  
  (define predicate-primitive?
    (lambda (x)
      (cond
        [(or (assq x list-of-user-primitives)
             (assq x list-of-system-primitives)) =>
         (lambda (a) (eq? (caddr a) 'test))]
        [else #f]))) 
  
  (define value-primitive?
    (lambda (x) 
      (cond
        [(or (assq x list-of-user-primitives)
             (assq x list-of-system-primitives)) =>
         (lambda (a) (eq? (caddr a) 'value))]
        [else #f]))) 
  
  (define effect-primitive?
    (lambda (x)
      (cond
        [(or (assq x list-of-user-primitives)
             (assq x list-of-system-primitives)) =>
         (lambda (a) (eq? (caddr a) 'effect))]
        [else #f]))) 
  
  (define effect-free-primitive?
    (lambda (x)
      (not (effect-primitive? x))))

  (define gen-label
    ; at some point, gen-label should be redefined to emit
    ; assembler-friendly labels
    (lambda (sym)
      (string->symbol (format "~a%" sym))))

  (define gen-symbol-seed 0)

  (define reset-seed
    (lambda ()
      (set! gen-symbol-seed 0)))
  
  (define gen-symbol
    (lambda (sym)
      (set! gen-symbol-seed (+ gen-symbol-seed 1))
      (string->symbol (format "~a_~s" sym gen-symbol-seed)))) 
  
  (define set?
    (lambda (ls)
      (or (null? ls)
          (and (not (memq (car ls) (cdr ls))) (set? (cdr ls)))))) 
  
  ;;; ====================
  ;;; Extra syntax and helpers for multiple values 
  
  ;;; Set abstraction
  (define empty-set (lambda () '())) 
  
  (define singleton-set (lambda (elt) (list elt))) 
  
  (define add-element
    (lambda (elt set)
      (if (member? elt set)
          set
          (cons elt set)))) 
 
  (define member? memq)
  
  (define empty? null?) 

  (define set-cons
    (lambda (a set)
      (if (memq a set) set (cons a set))))
  
  (define union
    (case-lambda
      [() (empty-set)]
      [(set1 set2)
       (cond
         [(empty? set1) set2]
         [(empty? set2) set1]
         [(eq? set1 set2) set1]
         [else (reduce (lambda (elt set)
                         (if (member? elt set2) set (cons elt set)))
                       set2
                       set1)])]
      [(set1 . sets)
       (if (null? sets)
           set1
           (union set1 (reduce union (empty-set) sets)))])) 
  
  (define intersection
    (lambda (set1 . sets)
      (cond
        [(null? sets) set1]
        [(any empty? sets) (empty-set)]
        [else (choose
                (lambda (elt)
                  (every (lambda (set) (member? elt set)) sets)) set1)]))) 
  
  (define list-index
    (lambda (a ls)
      (cond
        [(null? ls) -1]
        [(eq? (car ls) a) 0]
        [else (maybe-add1 (list-index a (cdr ls)))])))  

  (define maybe-add1
    (lambda (n)
      (if (= n -1) -1 (+ n 1))))

  (define difference
    (lambda (set1 . sets)
      (let ((sets (reverse-filter empty? sets)))
        (cond
          [(null? sets) set1]
          [else (reverse-filter (lambda (elt)
                                  (any (lambda (set) 
                                         (member? elt set)) 
                                       sets))
                                set1)])))))
