#lang plai
(require (prefix-in eli: tests/eli-tester)
         "util.rkt")

(define-type WAE
  [binop (p procedure?) (lhs WAE?) (rhs WAE?)]
  [num (n number?)]
  [id (s symbol?)])

(define-syntax-rule (->string e)
  (regexp-replace "line [0-9]+" (with-both-output-to-string (λ () e)) "line ??"))
(define-syntax-rule (->err-string e)
  (regexp-replace "line [0-9]+" (with-err-output-to-string (λ () e)) "line ??"))
(define-syntax-rule (->out-string e)
  (regexp-replace "line [0-9]+" (with-output-to-string (λ () e)) "line ??"))

(define (with-err-output-to-string thunk)
  (define sp (open-output-string))
  (parameterize ([current-error-port sp])
    (thunk))
  (get-output-string sp))

(define (go catch? errors? abridged?)
  (eli:test 
   #:failure-prefix (format "~a / ~a / ~a" catch? errors? abridged?)
   (eli:test
    (plai-catch-test-exn catch?)
    (print-only-errors errors?)
    (abridged-test-output abridged?)
    
    (->string (test (list 'binop + (num 1) (num 2)) (binop + (num 1) (num 2))))
    =>
    (if abridged?
        "(bad (list 'binop #<procedure:+> (num 1) (num 2)) (binop #<procedure:+> (num 1) (num 2)))\n"
        "(bad (list (quote binop) + (num 1) (num 2)) (list 'binop #<procedure:+> (num 1) (num 2)) (binop #<procedure:+> (num 1) (num 2)) \"at line ??\")\n")
    
    (->string (test (num 5) (id 'x)))
    =>
    (if abridged?
        "(bad (num 5) (id 'x))\n"
        "(bad (num 5) (num 5) (id 'x) \"at line ??\")\n")
    
    (->string (test 1 (+ 1 0)))
    =>
    (if errors?
        ""
        (if abridged?
            "(good 1 1)\n"
            "(good 1 1 1 \"at line ??\")\n"))
    
    (->string (test 1 1))
    =>
    (if errors?
        ""
        (if abridged?
            "(good 1 1)\n"
            "(good 1 1 1 \"at line ??\")\n"))
    
    (->out-string (test 1 1))
    =>
    (if errors?
        ""
        (if abridged?
            "(good 1 1)\n"
            "(good 1 1 1 \"at line ??\")\n"))
    
    (->string (test 1 2))
    =>
    (if abridged?
        "(bad 1 2)\n"
        "(bad 1 1 2 \"at line ??\")\n")
    
    (->err-string (test 1 2))
    =>
    (if abridged?
        "(bad 1 2)\n"
        "(bad 1 1 2 \"at line ??\")\n")
    
    (->string (test (/ 1 0) 0))
    =>
    (if catch?
        (if abridged?
            "(exception \"/: division by zero\" '<no-expected-value>)\n"
            "(exception (/ 1 0) \"/: division by zero\" '<no-expected-value> \"at line ??\")\n")
        (error '/ "division by zero"))
    
    (->string (test (error "zamboni") 347))
    =>
    (if catch?
        (if abridged?
            "(exception \"zamboni\" 347)\n"
            "(exception (error \"zamboni\") \"zamboni\" 347 \"at line ??\")\n")
        (error "zamboni"))
    
    (->string (test 3.4 3.4000001))
    =>
    (if errors?
        ""
        (if abridged?
            "(good 3.4 3.4000001)\n"
            "(good 3.4 3.4 3.4000001 \"at line ??\")\n"))
    
    (->string (test +inf.0 +inf.0))
    =>
    (if errors?
        ""
        (if abridged?
            "(good +inf.0 +inf.0)\n"
            "(good +inf.0 +inf.0 +inf.0 \"at line ??\")\n"))
    
    (->string (test/pred 0 zero?))
    =>
    (if errors?
        ""
        (if abridged?
            "(good 0 'zero?)\n"
            "(good 0 0 'zero? \"at line ??\")\n"))
    
    (->string (test/pred 1 zero?))
    =>
    (if abridged?
        "(bad 1 'zero?)\n"
        "(bad 1 1 'zero? \"at line ??\")\n")
    
    (->string (test/pred 1 (error 'pred)))
    =>
    (if catch?
        (if abridged?
            "(pred-exception \"error: pred\" '<no-expected-value>)\n"
            "(pred-exception 1 \"error: pred\" '<no-expected-value> \"at line ??\")\n")
        (error 'pred))
    
    (->string (test/pred 1 (lambda (n) (/ 1 0))))
    =>
    (if catch?
        (if abridged?
            "(pred-exception \"/: division by zero\" '<no-expected-value>)\n"
            "(pred-exception 1 \"/: division by zero\" '<no-expected-value> \"at line ??\")\n")
        (error '/ "division by zero"))
    
    (->string (test/pred "a" string->number))
    =>
    (if abridged?
        "(bad \"a\" 'string->number)\n"
        "(bad \"a\" \"a\" 'string->number \"at line ??\")\n")
    
    (->string (test/exn (error "zamboni") "zamboni"))
    =>
    (if catch?
        (if errors?
            ""
            (if abridged?
                "(good \"zamboni\" \"zamboni\")\n"
                "(good (error \"zamboni\") \"zamboni\" \"zamboni\" \"at line ??\")\n"))
        (error "zamboni"))
    
    (->string (test/exn (error "samboni") "zamboni"))
    =>
    (if catch?
        (if abridged?
            "(bad \"samboni\" \"zamboni\")\n"
            "(bad (error \"samboni\") \"samboni\" \"zamboni\" \"at line ??\")\n")
        (error "samboni"))
    
    (->string (test/exn 5 "zamboni"))
    =>
    (if abridged?
        "(bad 5 \"zamboni\")\n"
        "(bad 5 5 \"zamboni\" \"at line ??\")\n")
    
    (->string (test/exn (/ 1 0) "division"))
    =>
    (if catch?
        (if abridged?
            "(exception \"/: division by zero\" '<no-expected-value>)\n"
            "(exception (/ 1 0) \"/: division by zero\" '<no-expected-value> \"at line ??\")\n")
        (error '/ "division by zero"))
    
    (->string (test/regexp (error "zamboni") "zam"))
    =>
    (if catch?
        (if errors?
            ""
            (if abridged?
                "(good \"zamboni\" \"zam\")\n"
                "(good (error \"zamboni\") \"zamboni\" \"zam\" \"at line ??\")\n"))
        (error "zamboni"))
    
    (->string (test/regexp (error "samboni") "zam"))
    =>
    (if catch?
        (if abridged?
            "(bad \"samboni\" \"zam\")\n"
            "(bad (error \"samboni\") \"samboni\" \"zam\" \"at line ??\")\n")
        (error "samboni"))
    
    (->string (test/regexp 5 "zam"))
    =>
    (if abridged?
        "(bad 5 \"zam\")\n"
        "(bad 5 5 \"zam\" \"at line ??\")\n")
    
    (->string (test/regexp (/ 1 0) "divis"))
    =>(if catch?
          (if abridged?
              "(exception \"/: division by zero\" '<no-expected-value>)\n"
              "(exception (/ 1 0) \"/: division by zero\" '<no-expected-value> \"at line ??\")\n")
          (error '/ "division by zero"))
    
    )))

(eli:test
 (for* ([catch? (in-list (list #t #f))]
        [errors? (in-list (list #t #f))]
        [abridged? (in-list (list #t #f))])
   (go catch? errors? abridged?)))
