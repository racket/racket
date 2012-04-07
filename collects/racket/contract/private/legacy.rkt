#lang racket/base

(require "blame.rkt" "prop.rkt" "misc.rkt" syntax/srcloc)

(provide make-proj-contract
         raise-contract-error
         contract-proc)

(define (raise-contract-error x src pos name fmt . args)
  (apply raise-blame-error
         (make-blame (unpack-source src)
                     (unpack-name src)
                     (λ () name)
                     (unpack-blame pos)
                     "<<unknown party>>"
                     #t)
         x
         fmt
         args))

(define (make-proj-contract name proj test)
  (make-contract
   #:name name
   #:first-order test
   #:projection
   (cond
    [(procedure-arity-includes? proj 5)
     (lambda (blame)
       (proj (blame-positive blame)
             (blame-negative blame)
             (list (blame-source blame) (blame-value blame))
             (blame-contract blame)
             (not (blame-swapped? blame))))]
    [(procedure-arity-includes? proj 4)
     (lambda (blame)
       (proj (blame-positive blame)
             (blame-negative blame)
             (list (blame-source blame) (blame-value blame))
             (blame-contract blame)))]
    [else
     (error 'make-proj-contract
            "expected a projection that accepts 4 or 5 arguments; got: ~e"
            proj)])))

(define (contract-proc c)
  (let* ([proj (contract-projection c)])
    (lambda (pos neg src name [original? #t])
      (proj (make-blame (unpack-source src)
                        (unpack-name src)
                        (λ () name)
                        (unpack-blame (if original? pos neg))
                        (unpack-blame (if original? neg pos))
                        original?)))))

;; unpack-blame : any/c -> any/c
;; Constructs an S-expression for use in the blame error messages.
;; A variable reference represents a module or top-level context.
;; Other representations of blame are returned as-is.
(define (unpack-blame blame)
  (if (variable-reference? blame)
      (let ([resolved (variable-reference->module-source blame)])
        (cond
         [(not resolved) 
          'top-level]
         [else
          (cond
           [(symbol? resolved) `(quote ,resolved)]
           [else `(file ,(path->string resolved))])]))
      blame))

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
