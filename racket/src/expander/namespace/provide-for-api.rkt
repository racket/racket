#lang racket/base
(require "provided.rkt"
         "../common/phase.rkt"
         "../common/module-path.rkt"
         "../syntax/module-binding.rkt")

(provide provides->api-provides
         variables->api-nonprovides)

(define (provides->api-provides provides self)
  (define (extract ok?)
    (define result-l
      (for*/list ([(phase at-phase) (in-hash provides)]
                  [l (in-value
                      (for/list ([(sym b/p) (in-hash at-phase)]
                                 #:when (ok? b/p))
                        (define b (provided-as-binding b/p))
                        (list sym
                              (cond
                               [(eq? self (module-binding-module b))
                                null]
                               [else
                                (for/list ([b (in-list (cons b (module-binding-extra-nominal-bindings b)))])
                                  (cond
                                   [(and (eqv? (module-binding-nominal-phase b)
                                               phase)
                                         (eq? (module-binding-nominal-sym b) sym))
                                    (module-binding-nominal-module b)]
                                   [else
                                    (list (module-binding-nominal-module b)
                                          (module-binding-phase b)
                                          (module-binding-nominal-sym b)
                                          (module-binding-nominal-phase b))]))]))))]
                  #:unless (null? l))
        (cons phase (sort l symbol<? #:key car))))
    (sort result-l phase<? #:key car))
  (values (extract (lambda (b/p) (not (provided-as-transformer? b/p))))
          (extract provided-as-transformer?)))


(define (variables->api-nonprovides provides all-vars)
  ;; Filter provideded from list of all variables
  (define result-l
    (for/list ([(phase vars) (in-hash all-vars)]
               #:when #t
               [l (in-value
                   (let ([syms (hash-ref provides phase #hasheq())])
                     (for/list ([var-sym (in-list vars)]
                                #:unless (hash-ref syms var-sym #f))
                       var-sym)))]
               #:unless (null? l))
      (cons phase (sort l symbol<?))))
  (sort result-l phase<? #:key car))
