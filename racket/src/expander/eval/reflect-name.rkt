#lang racket/base
(require "../compile/compiled-in-memory.rkt"
         "../common/contract.rkt"
         "../host/linklet.rkt"
         "../compile/linklet.rkt"
         "reflect-compiled.rkt")

(provide module-compiled-name
         module-compiled-current-name
         change-module-name
         module-compiled-immediate-name
         rebuild-linklet-directory
         compiled->linklet-directory-or-bundle)

(define/who module-compiled-name
  (case-lambda
    [(c)
     (check who compiled-module-expression? c)
     (module-compiled-current-name c)]
    [(c name)
     (check who compiled-module-expression? c)
     (unless (or (symbol? name)
                 (and (pair? name)
                      (list? name)
                      (andmap symbol? name)))
       (raise-argument-error who
                             "(or/c symbol? (cons/c symbol? (non-empty-listof symbol?)))"
                             name))
     (define-values (i-name prefix) 
       (if (symbol? name)
           (values name null)
           (let ([r (reverse name)])
             (values (car r) (reverse (cdr r))))))
     (change-module-name c i-name prefix)]))

(define (module-compiled-current-name c)
  (define ld (compiled->linklet-directory-or-bundle c))
  (define b (if (linklet-bundle? ld)
                ld
                (hash-ref (linklet-directory->hash ld) #f)))
  (hash-ref (linklet-bundle->hash b) 'name))

(define (module-compiled-immediate-name c)
  (define n (module-compiled-current-name c))
  (if (pair? n)
      (car (reverse n))
      n))

(define (change-module-name c name prefix)
  (define full-name (if (null? prefix) name (append prefix (list name))))
  (define next-prefix (if (null? prefix) (list name) full-name))
  (define (recur sub-c name)
    (if (equal? (module-compiled-current-name sub-c) (append next-prefix (list name)))
        sub-c
        (change-module-name sub-c name next-prefix)))
  (cond
   [(compiled-in-memory? c)
    (define (change-submodule-name sub-c)
      (recur sub-c (module-compiled-immediate-name sub-c)))
    (define pre-compiled-in-memorys (map change-submodule-name
                                         (compiled-in-memory-pre-compiled-in-memorys c)))
    (define post-compiled-in-memorys (map change-submodule-name
                                          (compiled-in-memory-post-compiled-in-memorys c)))
    (struct-copy compiled-in-memory c
                 [pre-compiled-in-memorys pre-compiled-in-memorys]
                 [post-compiled-in-memorys post-compiled-in-memorys]
                 [linklet-directory (rebuild-linklet-directory
                                     (update-one-name
                                      (let ([ld (compiled->linklet-directory-or-bundle c)])
                                        (if (linklet-bundle? ld)
                                            ld
                                            (hash-ref (linklet-directory->hash ld) #f)))
                                      full-name)
                                     #:bundle-ok? (symbol? full-name)
                                     (append pre-compiled-in-memorys
                                             post-compiled-in-memorys))])]
   [(linklet-directory? c)
    (hash->linklet-directory
     (for/hasheq ([(key val) (in-hash (linklet-directory->hash c))])
       (values key
               (if (not key)
                   (update-one-name val full-name)
                   (recur val key)))))]
   [else
    ;; linklet bundle
    (update-one-name c full-name)]))

(define (update-one-name lb name)
  (hash->linklet-bundle (hash-set (linklet-bundle->hash lb) 'name name)))

(define (rebuild-linklet-directory main submods #:bundle-ok? [bundle-ok? #f])
  (cond
    [(and (null? submods) bundle-ok?)
     main]
    [else
     (hash->linklet-directory
      (hash-set (for/fold ([ht #hasheq()]) ([submod (in-list submods)])
                  (define name (module-compiled-immediate-name submod))
                  (cond
                    [(hash-ref ht name #f)
                     (raise-arguments-error 'module-compiled-submodules
                                            "change would result in duplicate submodule name"
                                            "name" name)]
                    [else
                     (hash-set ht name (compiled->linklet-directory-or-bundle submod))]))
                #f
                main))]))
