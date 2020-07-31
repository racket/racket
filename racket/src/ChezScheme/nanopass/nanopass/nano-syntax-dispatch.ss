;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (nanopass nano-syntax-dispatch)
  (export nano-syntax-dispatch)
  (import (rnrs) (nanopass helpers))
  (define match-each
    (lambda (e p)
      (cond
        [(pair? e)
         (let ((first (match (car e) p '())))
           (and first
                (let ((rest (match-each (cdr e) p)))
                  (and rest (cons first rest)))))]
        [(null? e) '()]
        [else #f]))) 
  
  (define match-each+
    (lambda (e x-pat y-pat z-pat r)
      (let f ([e e])
        (cond
          [(pair? e)
           (let-values ([(xr* y-pat r) (f (cdr e))])
             (if r
                 (if (null? y-pat) 
                     (let ([xr (match (car e) x-pat '())])
                       (if xr
                           (values (cons xr xr*) y-pat r)
                           (values #f #f #f)))
                     (values '() (cdr y-pat) (match (car e) (car y-pat) r)))
                 (values #f #f #f)))]
          [else (values '() y-pat (match e z-pat r))])))) 
  
  (define match-each-any
    (lambda (e)
      (cond
        [(pair? e)
         (let ([l (match-each-any (cdr e))])
           (and l (cons (car e) l)))]
        [(null? e) '()] 
        [else #f]))) 
  
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
        [(null? p) (and (null? e) r)]
        [(pair? p)
         (and (pair? e) (match (car e) (car p) (match (cdr e) (cdr p) r)))]
        [(eq? p 'each-any) (let ([l (match-each-any e)]) (and l (cons l r)))]
        [else
          (case (vector-ref p 0)
            [(each)
             (if (null? e)
                 (match-empty (vector-ref p 1) r)
                 (let ((r* (match-each e (vector-ref p 1))))
                   (and r* (combine r* r))))]
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
        [(eq? p 'any) (cons e r)]
        [else (match* e p r)]))) 
  
  (define nano-syntax-dispatch
    (lambda (e p)
      (cond
        [(eq? p 'any) (list e)]
        [else (match* e p '())]))))
