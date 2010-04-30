#lang scheme/base

(require "../utils/utils.rkt" 
	 (rep type-rep)
	 (utils tc-utils)
	 (env type-env)
         (except-in (types subtype union convenience resolve utils) -> ->*)
         (private parse-type)
         (only-in scheme/contract listof ->)
         scheme/match mzlib/trace)
(provide type-annotation
         get-type
         get-types
         get-type/infer
         type-label-symbol
         type-ascrip-symbol
         type-dotted-symbol
         type-ascription
         remove-ascription
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
  ;(printf "in type-annotation:~a~n" (syntax->datum stx))
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
        (parse-tc-results prop)
        (parse-tc-results/id stx prop)))
  (cond
    [(syntax-property stx type-ascrip-symbol) => pt]
    [else #f]))

(define (remove-ascription stx)
  (syntax-property stx type-ascrip-symbol #f))

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

;; list[identifier] stx (stx -> tc-results?) (stx tc-results? -> tc-results?) -> tc-results?
(d/c (get-type/infer stxs expr tc-expr tc-expr/check)
  ((listof identifier?) syntax? (syntax? . -> . tc-results?) (syntax? tc-results? . -> . tc-results?) . -> . tc-results?)
  (match stxs
    ['() 
     (tc-expr/check expr (ret null))]
    [(list stx)
     (cond [(type-annotation stx #:infer #t)
            => (lambda (ann)
                 (tc-expr/check expr (ret ann)))]
           [else (tc-expr expr)])]
    [(list stx ...)
     (let ([anns (for/list ([s stxs]) (type-annotation s #:infer #t))])
       (if (for/and ([a anns]) a)
           (begin (tc-expr/check expr (ret anns)))
           (let ([ty (tc-expr expr)])
             (match ty
               [(tc-results: tys) 
                (if (not (= (length stxs) (length tys)))
                    (begin
                      (tc-error/delayed 
                                      "Expression should produce ~a values, but produces ~a values of types ~a"
                                      (length stxs) (length tys) (stringify tys))
                      (ret (map (lambda _ (Un)) stxs)))
                    (ret 
                     (for/list ([stx stxs] [ty tys] [a anns])
                       (cond [a => (lambda (ann) (check-type stx ty ann) ann)]
                             [else ty]))))]
               [ty (tc-error/delayed 
                    "Expression should produce ~a values, but produces one values of type ~a"
                    (length stxs) ty)
                   (ret (map (lambda _ (Un)) stxs))]))))]))


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
