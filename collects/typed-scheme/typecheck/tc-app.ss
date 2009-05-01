#lang scheme/unit

(require (rename-in "../utils/utils.ss" [infer r:infer])
         "signatures.ss" "tc-metafunctions.ss"
         stxclass scheme/match mzlib/trace
         (for-syntax stxclass)
         (types utils abbrev)
         (utils tc-utils)
         (rep type-rep filter-rep object-rep)
         (for-template 
          (only-in '#%kernel [apply k:apply])
          "internal-forms.ss" scheme/base 
          (only-in scheme/private/class-internal make-object do-make-object)))

(import tc-expr^ tc-lambda^ tc-dots^ tc-let^)
(export tc-app^)

;; syntax tc-results? -> tc-results?
(define (tc/app/internal form expected)
  (syntax-parse form
    #:literals (#%plain-app #%plain-lambda letrec-values
                values apply k:apply not list list* call-with-values do-make-object make-object cons
                       andmap ormap)
    ;; special case for `values'
    [(#%plain-app values arg) (single-value #'arg expected)]
    [(#%plain-app values . args)
     (match expected
       [(tc-results: ets efs eos)
        (match-let ([(list (tc-result1: ts fs os) ...) 
                     (for/list ([arg (syntax->list #'args)]
                                [et ets] [ef efs] [eo eos])
                       (single-value arg (ret et ef eo)))])
          (if (= (length ts) (length ets) (length (syntax->list #'args)))
              (ret ts fs os)
              (tc-error/expr #:return expected "wrong number of values: expected ~a but got ~a"
                             (length ets) (length (syntax->list #'args)))))]
       [_ (match-let ([(list (tc-result1: ts fs os) ...) 
                       (for/list ([arg (syntax->list #'args)])
                         (single-value arg))])
            (ret ts fs os))])]    
    ;; special case for `delay'
    [(#%plain-app 
      mp1 
      (#%plain-lambda () 
        (#%plain-app mp2 (#%plain-app call-with-values (#%plain-lambda () e) list))))
     #:declare mp1 (id-from 'make-promise 'scheme/promise)
     #:declare mp2 (id-from 'make-promise 'scheme/promise)
     (ret (-Promise (tc-expr/t #'e)))]
    ;; special case for `list'
    [(#%plain-app list . args)
     (let ([tys (map tc-expr/t (syntax->list #'args))])
       (ret (apply -lst* tys)))]
    ;; special case for `list*'
    [(#%plain-app list* . args)
     (match-let* ([(list last tys-r ...) (reverse (map tc-expr/t (syntax->list #'args)))]
                  [tys (reverse tys-r)])
       (ret (foldr make-Pair last tys)))]
    ;; inference for ((lambda
    [(#%plain-app (#%plain-lambda (x ...) . body) args ...)
     #:when (= (length (syntax->list #'(x ...)))
               (length (syntax->list #'(args ...))))
     (tc/let-values #'((x) ...) #'(args ...) #'body 
                    #'(let-values ([(x) args] ...) . body)
                    expected)]
    [(#%plain-app f . args)
     (let* ([f-ty (single-value #'f)]
            [arg-tys (map single-value (syntax->list #'args))])
       (tc/funapp #'f #'args f-ty arg-tys expected))]
    [_ (int-err "tc/app NYI")]))

;(trace tc/app/internal)

;; syntax -> tc-results
(define (tc/app form) (tc/app/internal form #f))  
  
;; syntax tc-results? -> tc-results?
(define (tc/app/check form expected)
    (define t (tc/app/internal form expected))
    (check-below t expected)
    expected)

(define (object-index os i)
  (unless (number? i)
    (int-err "object-index for keywords NYI"))
  (list-ref os i))

;; in-indexes : Listof[Type] -> Sequence[index/c]
(define (in-indexes dom)
  (in-range (length dom)))

;; syntax? syntax? tc-results? (listof tc-results?) (or/c #f tc-results) -> tc-results?
(define (tc/funapp f-stx args-stx ftype0 argtys expected)
  (match* (ftype0 argtys)
    [((tc-result1: (Function: (list (arr: dom (Values: (list (Result: t-r lf-r lo-r) ...)) #f #f '()))))
      (list (tc-result1: t-a phi-a o-a) ...))
     (unless (= (length dom) (length t-a))
       (tc-error/expr #:return (ret t-r)
                      "Wrong number of arguments"))
     (for ([dom-t (in-list dom)] [arg-t (in-list t-a)])
       (check-below arg-t dom-t))
     (let* (;; Listof[Listof[LFilterSet]]
            [lfs-f (for/list ([lf lf-r])
                    (for/list ([i (in-indexes dom)])
                      (split-lfilters lf i)))]
            ;; Listof[FilterSet]
            [f-r (for/list ([lfs lfs-f])
                   (merge-filter-sets (for/list ([lf lfs] [t t-a] [o o-a])
                                    (apply-filter lf t o))))]
            ;; Listof[Object]
            [o-r (for/list ([lo lo-r])                     
                   (match lo
                     [(LPath: pi* i)
                      (match (object-index o-a i)
                        [(Path: pi x) (make-Path (append pi* pi) x)]
                        [_ (make-Empty)])]
                     [_ (make-Empty)]))])
       (ret t-r f-r o-r))]
    [(_ _)
     (int-err "funapp with keyword/rest args NYI")]))