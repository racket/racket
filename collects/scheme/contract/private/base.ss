#lang scheme/base

#|

improve method arity mismatch contract violation error messages?
  (abstract out -> and friends even more?)

|#



(provide contract
         recursive-contract
         current-contract-region)

(require (for-syntax scheme/base)
         scheme/stxparam
         unstable/srcloc
         unstable/location
         "guts.ss"
         "helpers.ss")

(define-syntax-parameter current-contract-region
  (λ (stx) #'(quote-module-path)))

(define-syntax (contract stx)
  (syntax-case stx ()
    [(_ c v pos neg name loc)
     (syntax/loc stx
       (apply-contract c v pos neg name loc))]
    [(_ a-contract to-check pos-blame-e neg-blame-e)
     #|
     (quasisyntax/loc stx
       (contract a-contract
                 to-check
                 pos-blame-e
                 neg-blame-e
                 (build-source-location (quote-syntax #,stx))
                 '#f))
     |#
     (raise-syntax-error 'contract "upgrade to new calling convention" stx)]
    [(_ a-contract-e to-check pos-blame-e neg-blame-e src-info-e)
     #|
     (syntax/loc stx
       (let* ([info src-info-e])
         (contract a-contract-e
                   to-check
                   pos-blame-e
                   neg-blame-e
                   (unpack-source info)
                   (unpack-name info))))
     |#
     (raise-syntax-error 'contract "upgrade to new calling convention" stx)]))

(define (apply-contract c v pos neg name loc)
  (let* ([c (coerce-contract 'contract c)])
    (check-sexp! 'contract "positive blame" pos)
    (check-sexp! 'contract "negative blame" neg)
    (check-sexp! 'contract "value name" name)
    (check-srcloc! 'contract "source location" loc)
    (((contract-projection c)
      (make-blame loc name (contract-name c) pos neg #f))
     v)))

(define (check-srcloc! f-name v-name v)
  (unless (srcloc? v)
    (error f-name "expected ~a to be a srcloc structure; got: ~e" v-name v))
  (check-sexp! f-name
               (format "source file of ~a" v-name)
               (source-location-source v)))

(define (check-sexp! f-name v-name v)
  (let loop ([seen #hasheq()] [x v])
    (unless (or (null? x) (boolean? x) (number? x)
                (string? x) (bytes? x) (regexp? x) (char? x)
                (symbol? x) (keyword? x)
                (path? x))
      (when (hash-has-key? seen x)
        (error f-name
               "expected ~a to be acyclic; found a cycle in ~e at ~e"
               v-name v x))
      (let ([seen (hash-set seen x #t)])
        (cond
         [(pair? x) (loop seen (car x)) (loop seen (cdr x))]
         [(mpair? x) (loop seen (mcar x)) (loop seen (mcdr x))]
         [(vector? x) (for ([y (in-vector x)]) (loop seen y))]
         [(box? x) (loop seen (unbox x))]
         [(hash? x) (for ([(y z) (in-hash x)]) (loop seen y) (loop seen z))]
         [(prefab-struct-key x) =>
          (lambda (k) (loop seen k) (loop seen (struct->vector x)))]
         [else (error f-name
                      "expected ~a to be an s-expression; ~e contained ~e"
                      v-name v x)])))))

(define (unpack-source info)
  (cond
   [(syntax? info) (build-source-location info)]
   [(list? info)
    (let ([loc (list-ref info 0)])
      (if (syntax? (srcloc-source loc))
        (struct-copy
         srcloc loc
         [source
          (resolved-module-path-name
           (module-path-index-resolve
            (syntax-source-module
             (srcloc-source loc))))])
        loc))]
   [else
    (error 'contract
           "expected a syntax object or list of two elements, got: ~e"
           info)]))

(define (unpack-name info)
  (cond
   [(syntax? info) (and (identifier? info) (syntax-e info))]
   [(list? info) (list-ref info 1)]
   [else
    (error 'contract
           "expected a syntax object or list of two elements, got: ~e"
           info)]))

(define-syntax (recursive-contract stx)
  (syntax-case stx ()
    [(_ arg)
     (syntax
      (simple-contract
       #:name '(recursive-contract arg)
       #:projection
       (λ (blame)
          (let ([ctc (coerce-contract 'recursive-contract arg)])
            (let ([f (contract-projection ctc)])
              (λ (val)
                 ((f blame) val)))))))]))
