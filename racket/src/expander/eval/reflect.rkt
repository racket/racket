#lang racket/base
(require "../compile/compiled-in-memory.rkt"
         "../host/linklet.rkt"
         "../common/contract.rkt"
         "module.rkt"
         "../namespace/provided.rkt"
         "../namespace/provide-for-api.rkt"
         "reflect-name.rkt")

(provide compiled-expression?

         compiled-module-expression?
         module-compiled-name
         module-compiled-submodules
         module-compiled-language-info
         module-compiled-imports
         module-compiled-exports
         module-compiled-indirect-exports
         module-compiled-cross-phase-persistent?)

;; The representation of a module with its submodules is designed to
;; make reading an individual submodule (with its submodule path
;; intact) fast and convenient --- but it makes adjusting the name
;; inconvenient, because each linklet bundle for a module encodes its
;; full submodule path. The extra layer of `compiled-in-memory`
;; support for sharing and fast compile-then-eval cycles is another
;; layer of inconvenience.

(define (compiled-expression? c)
  (or (compiled-in-memory? c)
      (linklet-directory? c)
      (linklet-bundle? c)))

(define (compiled-module-expression? c)
  (define ld (compiled->linklet-directory-or-bundle c))
  (or (and (linklet-directory? ld)
           (let ([b (hash-ref (linklet-directory->hash ld) #f #f)])
             (and b (hash-ref (linklet-bundle->hash b) 'decl #f)))
           #t)
      (and (linklet-bundle? ld)
           (hash-ref (linklet-bundle->hash ld) 'decl #f)
           #t)))

(define module-compiled-name
  (case-lambda
    [(c)
     (check 'module-compiled-name compiled-module-expression? c)
     (module-compiled-current-name c)]
    [(c name)
     (check 'module-compiled-name compiled-module-expression? c)
     (unless (or (symbol? name)
                 (and (pair? name)
                      (list? name)
                      (andmap symbol? name)))
       (raise-argument-error 'module-compiled-name
                             "(or/c symbol? (cons/c symbol? (non-empty-listof symbol?)))"
                             name))
     (define-values (i-name prefix) 
       (if (symbol? name)
           (values name null)
           (let ([r (reverse name)])
             (values (car r) (reverse (cdr r))))))
     (change-module-name c i-name prefix)]))

(define module-compiled-submodules
  (case-lambda
    [(c non-star?)
     (check 'module-compiled-submodules compiled-module-expression? c)
     (cond
      [(compiled-in-memory? c)
       ;; We have a convenient `compiled-in-memory` structure
       (if non-star?
           (compiled-in-memory-pre-compiled-in-memorys c)
           (compiled-in-memory-post-compiled-in-memorys c))]
      [else 
       ;; We have a raw linklet directory or bundle, which is designed
       ;; more for loading code than easy manipulation...
       (cond
        [(linklet-directory? c)
         (define ht (linklet-directory->hash c))
         (define bh (linklet-bundle->hash (hash-ref ht #f)))
         (define names (hash-ref bh (if non-star? 'pre 'post) null))
         (for/list ([name (in-list names)])
           (hash-ref ht name))]
        [else
         ;; a linklet bundle represents a module with no submodules
         null])])]
    [(c non-star? submods)
     (check 'module-compiled-submodules compiled-module-expression? c)
     (unless (and (list? submods)
                  (andmap compiled-module-expression? submods))
       (raise-argument-error 'module-compiled-submodules "(listof compiled-module-expression?)" submods))
     (cond
      [(and (null? submods)
            (or (linklet-bundle? (compiled->linklet-directory-or-bundle c))
                (and (compiled-in-memory? c)
                     (null? (if non-star?
                                (compiled-in-memory-pre-compiled-in-memorys c)
                                (compiled-in-memory-post-compiled-in-memorys c))))))
       ;; No change to a module without submodules
       c]
      [(and (compiled-in-memory? c)
            (andmap compiled-in-memory? submods))
       ;; All compiled-in-memory structures, so preserve them
       (define pre-compiled-in-memorys (if non-star?
                                           submods
                                           (compiled-in-memory-pre-compiled-in-memorys c)))
       (define post-compiled-in-memorys (if non-star?
                                            (compiled-in-memory-post-compiled-in-memorys c)
                                            submods))
       (define n-c (normalize-to-linklet-directory c))
       (fixup-submodule-names
        (struct-copy compiled-in-memory n-c
                     [pre-compiled-in-memorys pre-compiled-in-memorys]
                     [post-compiled-in-memorys post-compiled-in-memorys]
                     [linklet-directory (rebuild-linklet-directory
                                         (reset-submodule-names
                                          (hash-ref (linklet-directory->hash (compiled->linklet-directory-or-bundle n-c)) #f)
                                          non-star?
                                          submods)
                                         #:bundle-ok? (symbol? (module-compiled-current-name c))
                                         (append pre-compiled-in-memorys
                                                 post-compiled-in-memorys))]))]
      [else
       ;; Not all compiled-in-memory structures, so forget whatever ones we have
       (define n-c (normalize-to-linklet-directory c))
       (fixup-submodule-names
        (rebuild-linklet-directory
         (reset-submodule-names
          (hash-ref (linklet-directory->hash (compiled->linklet-directory-or-bundle n-c)) #f)
          non-star?
          submods)
         (map compiled->linklet-directory-or-bundle
              (append (if non-star? submods (module-compiled-submodules c #t))
                      (if non-star? (module-compiled-submodules c #f) submods)))))])]))

(define (module-compiled-language-info c)
  (check 'module-compiled-language-info compiled-module-expression? c)  
  (define h (compiled-module->h c))
  (hash-ref h 'language-info #f))

(define (module-compiled-imports c)
  (check 'module-compiled-imports compiled-module-expression? c)
  (define inst (compiled-module->declaration-instance c))
  (instance-variable-value inst 'requires))

(define (module-compiled-exports c)
  (check 'module-compiled-imports compiled-module-expression? c)
  (define inst (compiled-module->declaration-instance c))
  (provides->api-provides (instance-variable-value inst 'provides)
                          (instance-variable-value inst 'self-mpi)))

(define (module-compiled-indirect-exports c)
  (check 'module-compiled-indirect-imports compiled-module-expression? c)
  (define-values (h inst) (compiled-module->h+declaration-instance c))
  (define min-phase (hash-ref h 'min-phase 0))
  (define max-phase (hash-ref h 'max-phase 0))
  (variables->api-nonprovides (instance-variable-value inst 'provides)
                              (for/hash ([phase-level (in-range min-phase (add1 max-phase))])
                                (define linklet (hash-ref h phase-level #f))
                                (values phase-level
                                        (if linklet
                                            (linklet-export-variables linklet)
                                            null)))))

(define (module-compiled-cross-phase-persistent? c)
  (check 'module-compiled-cross-phase-persistent?  compiled-module-expression? c)
  (define h (compiled-module->h c))
  (hash-ref h 'cross-phase-persistent? #f))

;; ----------------------------------------

;; Normalize a compiled module that may have no submodules and is
;; represented directy by a linklet bundle to a representation that
;; uses a linklet directory
(define (normalize-to-linklet-directory c)
  (cond
   [(linklet-directory? (compiled->linklet-directory-or-bundle c))
    ;; already in linklet-directory form:
    c]
   [(linklet-bundle? c)
    (hash->linklet-directory (hasheq #f c))]
   [else
    (struct-copy compiled-in-memory c
                 [linklet-directory (normalize-to-linklet-directory
                                     (compiled-in-memory-linklet-directory c))])]))

;; ----------------------------------------

(define (fixup-submodule-names c)
  ;; Although this looks like a no-op, it forces a reset on submodule
  ;; names, except where the names already match (short-circuited in
  ;; `change-module-name`).
  (module-compiled-name c (module-compiled-name c)))

(define (reset-submodule-names b pre? submods)
  (hash->linklet-bundle
   (hash-set (linklet-bundle->hash b)
             (if pre? 'pre 'post)
             (map module-compiled-immediate-name submods))))
