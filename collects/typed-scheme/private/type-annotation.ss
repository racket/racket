#lang scheme/base

(require (except-in "../utils/utils.ss" extend))
(require (rep type-rep)
	 (utils tc-utils)
	 (env type-env)
	 "parse-type.ss" "subtype.ss"
         "type-effect-convenience.ss" "resolve-type.ss" "union.ss"
         scheme/match mzlib/trace)
(provide type-annotation
         get-type
         get-types
         get-type/infer
         type-label-symbol
         type-ascrip-symbol
         type-dotted-symbol
         type-ascription
         check-type
         dotted?)

(define type-label-symbol 'type-label)
(define type-ascrip-symbol 'type-ascription)
(define type-dotted-symbol 'type-dotted)

(define (print-size stx)
  (syntax-case stx ()
    [(a . b) (begin
               (printf/log "Annotation Sexp Pair ~n")
               (print-size #'a)
               (print-size #'b))]      
    [_ (printf/log "Annotation Sexp ~n" )]))

;; get the type annotation of this syntax
;; syntax -> Maybe[Type]
;; is let-binding really necessary? - remember to record the bugs!
(define (type-annotation stx #:infer [let-binding #f])
  (define (pt prop)
    #;(print-size prop)
    (if (syntax? prop)
        (parse-type prop)
        (parse-type/id stx prop)))
  ;(unless let-binding (error 'ohno))
  ;(printf "let-binding: ~a~n" let-binding)
  (cond       
    [(syntax-property stx type-label-symbol) => pt]
    [(syntax-property stx type-ascrip-symbol) => pt]
    ;; this is so that : annotation works in internal def ctxts
    [(and (identifier? stx) (lookup-type stx (lambda () #f)))
     =>
     (lambda (t)
       (maybe-finish-register-type stx)
       t)]
    [else #f]))

;(trace type-annotation)

(define (type-ascription stx)
  (define (pt prop)
    #;(print-size prop)
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
;; if #:default is provided, return that instead of error
;; identifier #:default Type -> Type
(define (get-type stx #:default [default #f])
  (parameterize
      ([current-orig-stx stx])
    (cond
      [(type-annotation stx #:infer #t)]
      [default default]
      [(not (syntax-original? stx))
       (tc-error "untyped var: ~a" (syntax-e stx))]
      [else
       (tc-error "no type information on variable ~a" (syntax-e stx))])))

;; Listof[identifer] #:default Type -> Listof[Type]
(define (get-types stxs #:default [default #f])
  (map (lambda (e) (get-type e #:default default)) stxs))

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
                    (begin
                      (tc-error/delayed 
                                      "Expression should produce ~a values, but produces ~a values of types ~a"
                                      (length stxs) (length tys) (stringify tys))
                      (map (lambda _ (Un)) stxs))
                    (map (lambda (stx ty a)
                           (cond [a => (lambda (ann) (check-type stx ty ann) #;(log/extra stx ty ann) ann)]
                                 [else #;(log/noann stx ty) ty]))
                         stxs tys anns))]
               [ty (tc-error/delayed 
                    "Expression should produce ~a values, but produces one values of type ~a"
                    (length stxs) ty)
                   (map (lambda _ (Un)) stxs)]))))]))


;; check that e-type is compatible with ty in context of stx
;; otherwise, error
;; syntax type type -> void

(define (check-type stx e-type ty)
  (let ([stx* (current-orig-stx)])
    (parameterize ([current-orig-stx stx])
      (unless (subtype e-type ty)
        ;(printf "orig-stx: ~a" (syntax->datum stx*))
        (tc-error "Body had type:~n~a~nVariable had type:~n~a~n" e-type ty)))))

(define (dotted? stx)
  (cond [(syntax-property stx type-dotted-symbol) => syntax-e]
        [else #f]))
