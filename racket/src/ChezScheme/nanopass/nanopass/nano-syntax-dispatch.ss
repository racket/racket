;;; Copyright (c) 2000-2020 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (nanopass nano-syntax-dispatch)
  (export nano-syntax-dispatch any each each+ each-any)
  (import (rnrs) (nanopass helpers))

  (define-syntax any (lambda (x) #f))
  (define-syntax each (lambda (x) #f))
  (define-syntax each+ (lambda (x) #f))
  (define-syntax each-any (lambda (x) #f))

  (define-syntax nano-syntax-dispatch
    (lambda (x)
      (define combiner
        (lambda ()
          #'(lambda (r* r)
              (let f ([r* r*])
                (if (null? (car r*))
                    r
                    (cons (map car r*) (f (map cdr r*))))))))
      (define match-each
        (lambda (p)
          (with-syntax ([matcher (match p)])
            #'(lambda (e)
                (let f ([e e])
                  (cond
                    [(pair? e)
                     (let ([first (matcher (car e) '())])
                       (and first
                            (let ([rest (f (cdr e))])
                              (and rest (cons first rest)))))]
                    [(null? e) '()]
                    [else #f]))))))
      (define match-each+
        (lambda (p1 p2 p3)
          (with-syntax ([(p2* ...) p2])
            (with-syntax ([matcher-p1 (match p1)]
                          [(matcher-p2* ...) (map match #'(p2* ...))]
                          [matcher-p3 (match p3)])
              #'(lambda (e r)
                  (let f ([e e])
                    (cond
                      [(pair? e)
                       (let-values ([(xr* y-pat r) (f (cdr e))])
                         (if r
                             (if (null? y-pat)
                                 (let ([xr (matcher-p1 (car e) '())])
                                   (if xr
                                       (values (cons xr xr*) y-pat r)
                                       (values #f #f #f)))
                                 (values '() (cdr y-pat) ((car y-pat) (car e) r)))
                             (values #f #f #f)))]
                      [else (values '() (list matcher-p2* ...) (matcher-p3 e r))])))))))
      (define match-empty
        (lambda (p)
          (syntax-case p (any each-any each each+)
            [() #'(lambda (r) r)]
            [any #`(lambda (r) (cons '() r))]
            [(a . d)
             (with-syntax ([matcher-a (match-empty #'a)]
                           [matcher-d (match-empty #'d)])
               #'(lambda (r) (matcher-a (matcher-d r))))]
            [#(each p) (match-empty #'p)]
            [#(each+ p1 (p2 ...) p3)
             (with-syntax ([matcher-p1 (match-empty #'p1)]
                           [matcher-p2 (match-empty (reverse #'(p2 ...)))]
                           [matcher-p3 (match-empty #'p3)])
               #'(lambda (r) (matcher-p1 (matcher-p2 (matcher-p3 r)))))])))
      (define match*
        (lambda (p)
          (syntax-case p (any each-any each each+)
            [() #'(lambda (e r) (and (null? e) r))]
            [(a . d)
             (with-syntax ([matcher-a (match #'a)]
                           [matcher-d (match #'d)])
               #'(lambda (e r)
                   (and (pair? e)
                        (matcher-a (car e)
                          (matcher-d (cdr e) r)))))]
            [each-any #'(lambda (e r) (and (list? e) (cons e r)))]
            [#(each p)
             (with-syntax ([matcher-empty (match-empty #'p)]
                           [matcher-each (match-each #'p)]
                           [combine (combiner)])
               #'(lambda (e r)
                   (if (null? e)
                       (matcher-empty r)
                       (let ([r* (matcher-each e)])
                         (and r* (combine r* r))))))]
            [#(each+ p1 p2 p3)
             (with-syntax ([matcher-each+ (match-each+ #'p1 #'p2 #'p3)]
                           [matcher-empty (match-empty #'p1)]
                           [combine (combiner)])
               #'(lambda (e r)
                   (let-values ([(r* y-pat r) (matcher-each+ e r)])
                     (and r (null? y-pat)
                          (if (null? r*)
                              (matcher-empty r)
                              (combine r* r))))))])))
      (define match
        (lambda (p)
          (syntax-case p (any)
            [any #'(lambda (e r) (and r (cons e r)))]
            [_ (with-syntax ([matcher (match* p)])
                 #'(lambda (e r) (and r (matcher e r))))])))
      (syntax-case x (any)
        [(_ ?e p)
         (with-syntax ([matcher (match #'p)])
           #`(matcher ?e '()))])))
)
