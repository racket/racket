#lang scheme/base

(require unstable/debug scheme/contract "../utils/utils.rkt" syntax/id-table racket/dict racket/match
         (rep type-rep object-rep) (only-in (types abbrev utils) tc-results?) (utils tc-utils)
         (env init-envs) mzlib/pconvert)

(define table (make-hasheq))

(define (reset-type-table) (set! table (make-hasheq)))

(define (add-typeof-expr e t) 
  (when (optimize?)
    (hash-set! table e t)))

(define (type-of e) (hash-ref table e (lambda () (int-err (format "no type for ~a" (syntax->datum e))))))

(define struct-fn-table (make-free-id-table))

(define (add-struct-fn! id pe mut?) (dict-set! struct-fn-table id (list pe mut?)))

(define-values (struct-accessor? struct-mutator?) 
  (let ()
    (define ((mk mut?) id)
      (cond [(dict-ref struct-fn-table id #f)
              => (match-lambda [(list pe #f) pe] [_ #f])]
            [else #f]))
    (values (mk #f) (mk #t))))

(define (make-struct-table-code)
  (parameterize ([current-print-convert-hook converter]
                 [show-sharing #f])
    #`(begin #,@(for/list ([(k v) (in-dict struct-fn-table)]
                           #:when (bound-in-this-module k))
                  (match v
                    [(list pe mut?)
                     #`(add-struct-fn! (quote-syntax #,k) 
                                       #,(print-convert pe)
                                       #,mut?)])))))

(p/c [add-typeof-expr (syntax? tc-results? . -> . any/c)]
     [type-of (syntax? . -> . tc-results?)]
     [reset-type-table (-> any/c)]
     [add-struct-fn! (identifier? StructPE? boolean? . -> . any/c)]
     [struct-accessor? (identifier? . -> . (or/c #f StructPE?))]
     [struct-mutator? (identifier? . -> . (or/c #f StructPE?))]
     [make-struct-table-code (-> syntax?)])