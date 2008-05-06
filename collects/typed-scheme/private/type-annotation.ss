#lang scheme/base

(require "type-rep.ss" "parse-type.ss" "tc-utils.ss" "subtype.ss" "utils.ss" "union.ss" "resolve-type.ss"
         "type-env.ss" "type-effect-convenience.ss")
(require (lib "plt-match.ss")
         mzlib/trace)
(provide type-annotation
         get-type
         get-type/infer
         type-label-symbol
         type-ascrip-symbol
         type-ascription
         check-type)

(define type-label-symbol 'type-label)
(define type-ascrip-symbol 'type-ascription)    

(define (print-size stx)
  (syntax-case stx ()
    [(a . b) (begin
               (printf/log "Annotation Sexp Pair ~n")
               (print-size #'a)
               (print-size #'b))]      
    [_ (printf/log "Annotation Sexp ~n" )]))

;; get the type annotation of this syntax
;; syntax -> Maybe[Type]
(define (type-annotation stx #:infer [let-binding #f])
  (define (pt prop)
    (print-size prop)
    (if (syntax? prop)
        (parse-type prop)
        (parse-type/id stx prop)))
  (cond       
    [(syntax-property stx type-label-symbol) => pt]
    [(syntax-property stx type-ascrip-symbol) => pt]
    ;; this is so that : annotation works in internal def ctxts
    [(and let-binding (identifier? stx) (lookup-type stx (lambda () #f)))
     =>
     (lambda (t)
       (maybe-finish-register-type stx)
       t)]
    [else #f]))

(define (type-ascription stx)
  (define (pt prop)
    (print-size prop)
    (if (syntax? prop)
        (parse-type prop)
        (parse-type/id stx prop)))
  (cond
    [(syntax-property stx type-ascrip-symbol) => pt]
    [else #f]))

(define (log/ann stx ty)
  (printf/log "Required Annotated Variable: ~a ~a~n" (syntax-e stx) ty))
(define (log/extra stx ty ty2)
  (printf/log "Extra Annotated Variable: ~a ~a ~a~n" (syntax-e stx) ty ty2))
(define (log/noann stx ty)
  (printf/log "Unannotated Variable: ~a ~a~n" (syntax-e stx) ty))

;; get the type annotation of this identifier, otherwise error
;; identifier -> Type
(define (get-type stx)
  (parameterize
      ([current-orig-stx stx])
    (cond
      [(type-annotation stx) => (lambda (x) 
                                  (log/ann stx x)
                                  x)]
      [(not (syntax-original? stx))
       (tc-error "untyped var: ~a" (syntax-e stx))]
      [else
       (tc-error "no type information on variable ~a" (syntax-e stx))])))

;; get the type annotations on this list of identifiers
;; if not all identifiers have annotations, return the supplied inferred type
;; list[identifier] type -> list[type]
(define (get-type/infer stxs expr tc-expr tc-expr/check)
  (match stxs
    ['() 
     (tc-expr/check expr (-values null))
     (list)]          
    [(list stx)
     (cond [(type-annotation stx #:infer #t)
            => (lambda (ann)
                 (list (tc-expr/check expr ann)))]
           [else (list (tc-expr expr))])]
    [(list stx ...)
     (let ([anns (for/list ([s stxs]) (type-annotation s #:infer #t))])
       (if (for/and ([a anns]) a)
           (begin (tc-expr/check expr (-values anns)) anns)
           (let ([ty (tc-expr expr)])
             (match ty
               [(Values: tys) 
                (if (not (= (length stxs) (length tys)))
                    (tc-error/delayed #:ret (map (lambda _ (Un)) stxs) 
                                      "Expression should produce ~a values, but produces ~a values of types ~a"
                                      (length stxs) (length tys) (stringify tys))
                    (map (lambda (stx ty)
                           (cond [(type-annotation stx #:infer #t) => (lambda (ann) (check-type stx ty ann) (log/extra stx ty ann) ann)]
                                 [else (log/noann stx ty) ty]))
                         stxs tys))]
               [ty (tc-error/delayed #:ret (map (lambda _ (Un)) stxs) 
                                     "Expression should produce ~a values, but produces one values of type "
                                     (length stxs) ty)]))))]))


;; check that e-type is compatible with ty in context of stx
;; otherwise, error
;; syntax type type -> void

(define (check-type stx e-type ty)
  (let ([stx* (current-orig-stx)])
    (parameterize ([current-orig-stx stx])
      (unless (subtype e-type ty)
        ;(printf "orig-stx: ~a" (syntax->datum stx*))
        (tc-error "Body had type:~n~a~nVariable had type:~n~a~n" e-type ty)))))
