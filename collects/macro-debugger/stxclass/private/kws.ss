#lang scheme/base
(require scheme/stxparam
         (for-syntax scheme/base))
(provide pattern
         ...*

         with-enclosing-fail
         enclosing-fail

         ok?
         (struct-out failed)

         current-expression
         current-macro-name)

;; (define-syntax-class name SyntaxClassDirective* SyntaxClassRHS*)
;; (define-syntax-class (name id ...) SyntaxClassDirective* SyntaxClassRHS*)

;; A SCDirective is one of
;;   #:description String
;;   #:transparent

;; A SyntaxClassRHS is
;;   (pattern Pattern PatternDirective ...)

;; A Pattern is one of
;;   name:syntaxclass
;;   (Pattern . Pattern)
;;   (Pattern ... . Pattern)
;;   (((Pattern*) HeadDirective* *) ...* . Pattern)
;;   datum, including ()

;; A PatternDirective is one of
;;   #:declare name SyntaxClassName
;;   #:declare name (SyntaxClassName expr ...)
;;   #:rename internal-id external-id
;;   #:with pattern expr
;;     #:with clauses are let*-scoped
;;   #:where expr

;; A HeadDirective is one of
;;   #:min nat/#f
;;   #:max nat/#f
;;   #:opt
;;   #:mand
;;   -- For optional heads only:
;;     #:occurs id
;;       'id' is bound to #t is the pattern occurs, #f otherwise
;;     #:default form
;;       Preceding head must have a single pvar
;;       If the head is not present, the pvar is bound to 'form' instead

(define-syntax-rule (define-keyword name)
  (define-syntax name
    (lambda (stx)
      (raise-syntax-error #f "keyword used out of context" stx))))

(define-keyword pattern)
(define-keyword basic-syntax-class)
(define-keyword ...*)
(define-keyword ...**)

(define-syntax-parameter enclosing-fail
  (lambda (stx)
    (raise-syntax-error #f
                        "used out of context: not parsing pattern"
                        stx)))

(define-syntax-parameter pattern-source
  (lambda (stx)
    (raise-syntax-error #f "used out of context: not in syntax-class parser" stx)))

(define current-expression (make-parameter #f))

(define (current-macro-name)
  (let ([expr (current-expression)])
    (and expr
         (syntax-case expr (set!)
           [(set! kw . _)
            #'kw]
           [(kw . _)
            (identifier? #'kw)
            #'kw]
           [kw
            (identifier? #'kw)
            #'kw]
           [_ #f]))))

;; A PatternParseResult is one of
;;   - (listof value)
;;   - (make-failed stx sexpr(Pattern) string frontier/#f)
(define (ok? x) (or (pair? x) (null? x)))
(define-struct failed (stx patstx reason frontier)
  #:transparent)


(define-syntax-rule (with-enclosing-fail failvar expr)
  (syntax-parameterize ((enclosing-fail
                         (make-rename-transformer (quote-syntax failvar))))
    expr))

