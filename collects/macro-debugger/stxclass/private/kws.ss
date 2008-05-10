#lang scheme/base
(require scheme/stxparam
         (for-syntax scheme/base))
(provide pattern
         union
         ...*

         try
         with-enclosing-fail
         enclosing-fail

         ok?
         (struct-out failed)

         current-expression
         current-macro-name)

;; (define-syntax-class name SyntaxClassRHS)
;; (define-syntax-class (name id ...) SyntaxClassRHS)

;; A SyntaxClassRHS is one of
;;   (pattern Pattern PatternDirective ...)
;;   (union SyntaxClassRHS ...)
;;   syntax-class-id

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
(define-keyword union)
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
         (syntax-case expr ()
           [(kw . _)
            (identifier? #'kw)
            #'kw]
           [_ #f]))))

;; A PatternParseResult is one of
;;   - (listof value)
;;   - (make-failed stx sexpr(Pattern) string)
(define (ok? x) (or (pair? x) (null? x)))
(define-struct failed (stx patstx reason)
  #:transparent)


(define-syntax-rule (with-enclosing-fail failvar expr)
  (syntax-parameterize ((enclosing-fail
                         (make-rename-transformer (quote-syntax failvar))))
    expr))

(define-syntax try
  (syntax-rules ()
    [(try failvar (expr0))
     expr0]
    [(try failvar (expr0 . exprs))
     (let ([failvar
            (lambda (x1 p1 r1 f1)
              (let ([failvar
                     (lambda (x2 p2 r2 f2)
                       (choose-error failvar x1 x2 p1 p2 r1 r2 f1 f2))])
                (try failvar exprs)))])
       expr0)]))

(define (choose-error k x1 x2 p1 p2 r1 r2 frontier1 frontier2)
  (define (go1) (k x1 p1 r1 frontier1))
  (define (go2) (k x2 p2 r2 frontier2))
  (let loop ([f1 frontier1] [f2 frontier2])
    (cond [(and (null? f1) (null? f2))
           ;; FIXME: merge
           (k x1 `(union ,p1 ,p2) #f frontier1)]
          [(and (pair? f1) (null? f2)) (go1)]
          [(and (null? f1) (pair? f2)) (go2)]
          [(and (pair? f1) (pair? f2))
           (let ([c1 (cadr f1)]
                 [c2 (cadr f2)])
             (cond [(> c1 c2) (go1)]
                   [(< c1 c2) (go2)]
                   [else (loop (cddr f1) (cddr f2))]))])))
