;;; rec.ss  --  Reimplementation of SRFI 31  -- Jens Axel Soegaard
; This reimplementation provides better error messages,
; than the original.

; The inferred-name is set; this gives the proper name in the
; error message, when the returned procedure is called with
; the wrong number of arguments.

(module rec mzscheme
    (provide rec)

      (define-syntax (rec stx)
        (syntax-case stx ()
          [(rec id expr)
           (identifier? #'id)
           #`(letrec ((id expr))
               #,(syntax-property #'expr 'inferred-name (syntax-e #'id)))]
          [(rec (name id ...) body ...)
           (andmap identifier? (syntax->list #'(name id ...)))
           #`(letrec ((name (lambda (id ...) body ...)))
               #,(syntax-property #'name 'inferred-name (syntax-e #'name)))]
          [_
           (raise-syntax-error
            #f "expects either a variable followed by an expresion, or a list of variables followed by a body" stx)]))
      )

; Examples of errors caught:

; (rec)
; (rec 1 1)
; (rec (a 1 b) 1)

;; Examples of error messages, where the inferred-name is used:

; > ((rec fact
;     (lambda (n)
;       (if (= n 0)
;           1
;           (* n (fact (- n 1))))))
;   3 2)
; procedure fact: expects 1 argument, given 2: 3 2

;> ((rec (fact n)
;       (if (= n 0)
;           1
;           (* n (fact (- n 1)))))
;   3 2)
; procedure fact: expects 1 argument, given 2: 3 2
