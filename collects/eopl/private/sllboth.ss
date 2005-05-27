
(module sllboth mzscheme

  ;; This is stuff that lives at both table-consruction time and
  ;; table-use time. That's ok because the data is all built on
  ;; S-expression.

  (provide sllgen:action-preference-list
	   sllgen:action?
	   sllgen:make-tester-regexp
	   sllgen:make-or-regexp
	   sllgen:make-arbno-regexp
	   sllgen:make-concat-regexp
	   sllgen:tester-regexp? 
	   sllgen:or-regexp?
	   sllgen:arbno-regexp?
	   sllgen:concat-regexp?
	   sllgen:tester-symbol-list
	   sllgen:make-char-tester
	   sllgen:tester?)

  (define sllgen:action-preference-list
    '(string make-string symbol make-symbol number make-number skip))

  (define sllgen:action?
    (lambda (action)
      (and
       (pair? action)
       (member (car action) sllgen:action-preference-list)
       (symbol? (cdr action))))) 

  (define sllgen:make-tester-regexp (lambda (x) x))
  (define sllgen:make-or-regexp (lambda (res) (cons 'or res)))
  (define sllgen:make-arbno-regexp (lambda (re) (list 'arbno re)))
  (define sllgen:make-concat-regexp (lambda (rs) (cons 'concat rs)))

  (define sllgen:tester-regexp? 
    (lambda (x) 
      (and (sllgen:tester? x) (lambda (f) (f x)))))

  (define sllgen:or-regexp?
    (lambda (x)
      (and (eq? (car x) 'or)
	   (lambda (f) (f (cdr x))))))

  (define sllgen:arbno-regexp?
    (lambda (x)
      (and (eq? (car x) 'arbno)
	   (lambda (f) (f (cadr x))))))

  (define sllgen:concat-regexp?
    (lambda (x)
      (and (eq? (car x) 'concat)
	   (lambda (f) (f (cdr x))))))

  (define sllgen:tester-symbol-list '(letter digit any whitespace))

  (define sllgen:make-char-tester 
    (lambda (char) 
      (and (or (char? char)
	       (error 'scanner-generation "illegal character ~s" char))
	   char)))

  (define sllgen:tester?
    (lambda (v)
      (or (char? v)
	  (member v sllgen:tester-symbol-list)
	  (and (pair? v)
	       (eq? (car v) 'not)
	       (pair? (cdr v))
	       (char? (cadr v)))))))