;; structs for dromedary
(define-struct <user-type> () (make-inspector))
(define-struct (|NumT| <user-type>) (tlist) (make-inspector))
(define-struct (|FunT| <user-type>) (tlist) (make-inspector))
(define-struct (|Num| <user-type>) (tlist) (make-inspector))
(define-struct (|Lam| <user-type>) (tlist) (make-inspector))
(define-struct (|Val| <user-type>) (tlist) (make-inspector))
(define-struct (|Minus| <user-type>) (tlist) (make-inspector))
(define-struct (|Times| <user-type>) (tlist) (make-inspector))
(define-struct (|Var| <user-type>) (tlist) (make-inspector))
(define-struct (|App| <user-type>) (tlist) (make-inspector))
(define-struct (|IfZero| <user-type>) (tlist) (make-inspector))
(define-struct (|Fix| <user-type>) (tlist) (make-inspector))
(define-struct (|exn:Stuck| exn) ())
(define-struct <tuple> (list) (make-inspector))

;;structs for algol
(define-syntax (define-a60-structs stx)
  (syntax-case stx ()
    [(_ (struct-name (field ...)) ...)
     (with-syntax ([(a60:struct ...) (map (lambda (id)
                                            (datum->syntax-object
                                             id
                                             (string->symbol
                                              (format "a60:~a" (syntax-e id)))))
                                          (syntax->list (syntax (struct-name ...))))])
       (syntax (begin (define-struct a60:struct (field ...)) ...)))]))
(define-a60-structs
  ;; Expressions
  (if (test then else))
  (unary (type argtype op arg))
  (binary (type argtype op arg1 arg2))
  (subscript (array index))
  (variable (name indices))
  (app (func args))
  ;; plus numbers, strings, and booleans

  ;; Statements
  (block (decls statements))
  (compound (statements))
  (assign (variables rhs))
  (goto (target))
  (branch (test then else))
  (call (proc args))
  (for (variable values body))
  (dummy ())
  (label (name statement))

  ;; for values
  (for-number (value))
  (for-step (start step end))
  (for-while (value test))

  ;; declarations
  (type-decl (type vars))
  (array-decl (type vars))
  (switch-decl (var cases))
  (proc-decl (result-type var arg-vars by-value-vars arg-specs body)))

