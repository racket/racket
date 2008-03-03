#lang scheme/base

(require "type-rep.ss" "parse-type.ss" "tc-utils.ss" "subtype.ss" "utils.ss" "union.ss" "resolve-type.ss"
         "type-env.ss")
(require (lib "plt-match.ss"))
(provide type-annotation
         get-type
         get-type/infer
         type-label-symbol
         type-ascrip-symbol
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
(define (type-annotation stx)
  (define (pt prop)
    (print-size prop)
    (if (syntax? prop)
        (parse-type prop)
        (parse-type/id stx prop)))
  (cond       
    [(syntax-property stx type-label-symbol) => pt]
    [(syntax-property stx type-ascrip-symbol) => pt]
    [(and (identifier? stx) (lookup-type stx (lambda () #f)))
     =>
     (lambda (t)
       (maybe-finish-register-type stx)
       t)]
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
(define (get-type/infer stxs e-type)
  (match (list stxs e-type)
    [(list '() (Values: '())) (list)]
    [(list (list stx ...) (Values: (list ty ...)))
     (map (lambda (stx ty)
            (cond [(type-annotation stx) => (lambda (ann) (check-type stx ty ann) (log/extra stx ty ann) ann)]
                  [else (log/noann stx ty) ty]))
          stx ty)]      
    [(list (list stx) ty)
     (cond [(type-annotation stx) => (lambda (ann)
                                       (check-type stx ty ann) 
                                       (log/extra stx ty ann)
                                       (list ann))]
           [else (log/noann stx ty) (list ty)])]
    [else (int-err "wrong type arity - get-type/infer ~a ~a" stxs e-type)]))


;; check that e-type is compatible with ty in context of stx
;; otherwise, error
;; syntax type type -> void

(define (check-type stx e-type ty)
  (let ([stx* (current-orig-stx)])
    (parameterize ([current-orig-stx stx])
      (unless (subtype e-type ty)
        ;(printf "orig-stx: ~a" (syntax->datum stx*))
        (tc-error "Body had type:~n~a~nVariable had type:~n~a~n" e-type ty)))))
