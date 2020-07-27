;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (nanopass meta-syntax-dispatch)
  (export meta-syntax-dispatch)
  (import (rnrs)
          (nanopass helpers) 
          (nanopass records)) 
  
  ;; (fields->patterns '(e0 e1 e2)) => (any any any)
  ;; (fields->patterns '(e0 ...)) => ((each+ any () ()))
  ;; (fields->patterns '(e0 ... e1)) => ((each+ any (any) ()))
  ;; (fields->patterns '(e0 ... e1 e2)) => ((each+ any (any any) ()))
  ;; (fields->patterns '(([x e0] ...) e1 e2 ...)) =>
  ;;   ((each+ (any any) () ())) any (each+ (any) () ())) 
  
  ;;; syntax-dispatch expects an expression and a pattern.  If the expression
  ;;; matches the pattern a list of the matching expressions for each
  ;;; "any" is returned.  Otherwise, #f is returned.  
  
  ;;; The expression is matched with the pattern as follows: 
  
  ;;; p in pattern:                        matches:
  ;;;   ()                                 empty list
  ;;;   any                                anything
  ;;;   (p1 . p2)                          pair (list)
  ;;;   each-any                           any proper list
  ;;;   #(each p)                          (p*)
  ;;;   #(each+ p1 (p2_1 ...p2_n) p3)      (p1* (p2_n ... p2_1) . p3) 
  
  (define match-each
    (lambda (e p)
      (syntax-case e ()
        [(a dots . d)
         (and (not (ellipsis? #'a)) (not (unquote? #'a)) (ellipsis? #'dots))
         (let ([first (match #'a p '())])
           (and first
                (let ([rest (match-each #'d p)])
                  (and rest (cons (map make-nano-dots first) rest)))))]
        [(a . d)
         (and (not (ellipsis? #'a)) (not (unquote? #'a)))
         (let ([first (match #'a p '())])
           (and first
                (let ([rest (match-each #'d p)])
                  (and rest (cons first rest)))))]
        [() '()]
        [else #f]))) 
  
  (define match-each+
    (lambda (e x-pat y-pat z-pat r)
      (let f ([e e])
        (syntax-case e ()
          [(a dots . d)
           (and (not (ellipsis? #'a)) (not (unquote? #'a)) (ellipsis? #'dots))
           (let-values ([(xr* y-pat r) (f #'d)])
             (if r
                 (if (null? y-pat)
                     (let ([xr (match #'a x-pat '())])
                       (if xr
                           (values (cons (map make-nano-dots xr) xr*) y-pat r)
                           (values #f #f #f)))
                     (values '() (cdr y-pat) (match #'a (car y-pat) r)))
                 (values #f #f #f)))]
          [(a . d)
           (and (not (ellipsis? #'a)) (not (unquote? #'a)))
           (let-values ([(xr* y-pat r) (f #'d)])
             (if r
                 (if (null? y-pat)
                     (let ([xr (match #'a x-pat '())])
                       (if xr
                           (values (cons xr xr*) y-pat r)
                           (values #f #f #f)))
                     (values '() (cdr y-pat) (match #'a (car y-pat) r)))
                 (values #f #f #f)))]
          [_ (values '() y-pat (match e z-pat r))])))) 
  
  (define match-each-any
    (lambda (e)
      (syntax-case e ()
        [(a dots . d)
         (and (not (ellipsis? #'a)) (not (unquote? #'a)) (ellipsis? #'dots))
         (let ([l (match-each-any #'d)])
           (and l (cons (make-nano-dots #'a) l)))]
        [(a . d)
         (and (not (ellipsis? #'a)) (not (unquote? #'a)))
         (let ([l (match-each-any #'d)])
           (and l (cons #'a l)))]
        [() '()]
        [_ #f]))) 
  
  (define match-empty
    (lambda (p r)
      (cond
        [(null? p) r]
        [(eq? p 'any) (cons '() r)]
        [(pair? p) (match-empty (car p) (match-empty (cdr p) r))]
        [(eq? p 'each-any) (cons '() r)]
        [else
          (case (vector-ref p 0)
            [(each) (match-empty (vector-ref p 1) r)]
            [(each+) (match-empty
                       (vector-ref p 1)
                       (match-empty
                         (reverse (vector-ref p 2))
                         (match-empty (vector-ref p 3) r)))])]))) 
  
  (define match*
    (lambda (e p r)
      (cond
        [(null? p) (syntax-case e () [() r] [_ #f])]
        [(pair? p)
         (syntax-case e ()
           [(a . d) (match #'a (car p) (match #'d (cdr p) r))]
           [_ #f])]
        [(eq? p 'each-any)
         (let ([l (match-each-any e)]) (and l (cons l r)))]
        [else
          (case (vector-ref p 0)
            [(each)
             (syntax-case e ()
               [() (match-empty (vector-ref p 1) r)]
               [_ (let ([r* (match-each e (vector-ref p 1))])
                    (and r* (combine r* r)))])]
            [(each+)
             (let-values ([(xr* y-pat r)
                           (match-each+ e (vector-ref p 1) (vector-ref p 2)
                                        (vector-ref p 3) r)])
               (and r (null? y-pat)
                    (if (null? xr*)
                        (match-empty (vector-ref p 1) r)
                        (combine xr* r))))])]))) 

  (define match
    (lambda (e p r)
      (cond
        [(not r) #f]
        [(eq? p 'any)
         (and (not (ellipsis? e))
              (not (unquote? e))   ; avoid matching unquote
              (cons e r))]
        [else (match* e p r)]))) 
  
  (define meta-syntax-dispatch
    (lambda (e p)
      (match e p '()))))
