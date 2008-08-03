#lang scheme/base

(require (for-syntax scheme/base scheme/match))

(define-syntax (safe stx)
  (syntax-case stx ()
    [(_ expr)
     (with-handlers ([exn? (lambda (e) #`(list 'error #,(exn-message e)))])
       (let-values ([(_ x) (syntax-local-expand-expression
                            #'(with-handlers ([exn? (lambda (e)
                                                      (list 'error
                                                            (exn-message e)))])
                                (cons 'value
                                      (call-with-values (lambda () expr)
                                          list))))])
         x))]))

(provide test)
(define-syntax (test stx)
  (define (check test blame fmt . args)
    (with-syntax ([test test] [blame blame] [fmt fmt] [(arg ...) args]
                  [loc (string->symbol
                        (format "~a:~a:~a" (syntax-source blame)
                                (syntax-line blame) (syntax-column blame)))])
      #'(unless test
          (error 'loc "test failure in ~e\n  ~a" 'blame
                 (format fmt arg ...)))))
  (define (t1 x)
    #`(let ([x (safe #,x)])
        #,(check #`(and (eq? 'value (car x)) (cadr x)) x
                 "expected non-#f, got~a: ~e"
                 #'(if (eq? 'value (car x)) "" " an error") #'(cadr x))))
  (define (t2 x y)
    #`(let ([x (safe #,x)] [y #,y])
        #,(check #'(and (eq? 'value (car x)) (equal? (cadr x) y)) x
                 "expected ~e, got~a: ~e"
                 #'y #'(if (eq? 'value (car x)) "" " an error") #'(cadr x))))
  (define (te x y)
    #`(let ([x (safe #,x)] [y #,y])
        #,(check #'(eq? 'error (car x)) x
                 "expected an error, got ~e" #'(cadr x))
        #,(check #'(regexp-match? y (cadr x)) x
                 "bad error message expected ~e, got ~e" #'y #'(cadr x))))
  (let loop ([xs (map (lambda (x)
                        (if (memq (syntax-e x) '(=> <= =error> <error=))
                          (syntax-e x) x))
                      (cdr (syntax->list stx)))]
             [r '()])
    (let ([t (match xs
               [(list* x '=> y r) (cons (t2 x y) r)]
               [(list* y '<= x r) (cons (t2 x y) r)]
               [(list* x '=error> y r) (cons (te x y) r)]
               [(list* y '<error= x r) (cons (te x y) r)]
               [(list* x r) (cons (t1 x) r)]
               [(list) '()])])
      (if (pair? t)
        (loop (cdr t) (cons (car t) r))
        #`(begin #,@(reverse r))))))

;; test the `test' macro

(test (< 1 2)
      (+ 1 2) => 3
      (car '()) =error> "expects argument of type"
      (if 1) =error> "if: bad syntax"
      (test (/ 0)) =error> "expected non-#f"
      (test 1 => 2) =error> "expected 2"
      (test 1 =error> "") =error> "expected an error"
      (test (/ 0) =error> "zzz") =error> "bad error message"
      )
