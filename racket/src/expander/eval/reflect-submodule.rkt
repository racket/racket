#lang racket/base
(require "../compile/compiled-in-memory.rkt"
         "../host/linklet.rkt"
         "../common/contract.rkt"
         "../compile/linklet.rkt"
         "reflect-compiled.rkt"
         "reflect-name.rkt")

(provide module-compiled-submodules)

;; The representation of a module with its submodules is designed to
;; make reading an individual submodule (with its submodule path
;; intact) fast and convenient --- but it makes adjusting the name
;; inconvenient, because each linklet bundle for a module encodes its
;; full submodule path. The extra layer of `compiled-in-memory`
;; support for sharing and fast compile-then-eval cycles is another
;; layer of inconvenience.

(define/who module-compiled-submodules
  (case-lambda
    [(c non-star?)
     (check who compiled-module-expression? c)
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
     (check who compiled-module-expression? c)
     (unless (and (list? submods)
                  (andmap compiled-module-expression? submods))
       (raise-argument-error who "(listof compiled-module-expression?)" submods))
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
             (sort (map module-compiled-immediate-name submods) symbol<?))))
