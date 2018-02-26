#lang racket/base
(provide (struct-out import)
         (struct-out import-group)

         import-group-lookup-ready?
         
         import-group-lookup
         import-lookup

         hash-ref-either
         
         make-add-import!)

(struct import (grp id int-id ext-id))
(struct import-group (index
                      key
                      [knowns/proc #:mutable]  ; starts as a procedure to get table
                      [converter #:mutable]    ; converts table entries to `known`s (i.e., lazy conversion)
                      [import-keys #:mutable]  ; vector of imports, used for inlining
                      [imports #:mutable]))    ; starts as declared imports, but inlining can grow

(define (import-group-knowns grp)
  (define knowns/proc (import-group-knowns/proc grp))
  (cond
   [(procedure? knowns/proc)
    (define-values (knowns converter import-keys) (knowns/proc (import-group-key grp)))
    (define knowns-or-empty (or knowns (hasheq)))
    (set-import-group-knowns/proc! grp knowns-or-empty)
    (set-import-group-converter! grp converter)
    (set-import-group-import-keys! grp import-keys)
    knowns-or-empty]
   [else knowns/proc]))

(define (import-group-lookup-ready? grp)
  (define knowns/proc (import-group-knowns/proc grp))
  (not (procedure? knowns/proc)))

(define (import-group-lookup g id)
  (define v (hash-ref (import-group-knowns g) id #f))
  (if v
      (let ([converter (import-group-converter g)])
        (if converter
            (converter v)
            v))
      v))

(define (import-lookup im)
  (import-group-lookup (import-grp im) (import-ext-id im)))

(define (hash-ref-either knowns imports key)
  (or (hash-ref knowns key #f)
      (let ([im (hash-ref imports key #f)])
        (and im
             (import-lookup im)))))

(define (make-add-import! imports grps get-import-knowns add-group!)
  (define next-index (length grps))
  (lambda (im ext-id index)
    ;; The `im` argument represents an import into the current
    ;; linklet. Let L be the linklet for that import. Map `ext-id` as
    ;; either defined in L (if `index` is #f) or imported into L from
    ;; its `index`th group to a new name in the current module,
    ;; potentially adding an import or import group to the current module.
    (define grp (import-grp im))
    (cond
      [index
       (import-group-knowns grp) ; force thunk
       (define import-keys (import-group-import-keys grp))
       (define key (and import-keys (vector-ref import-keys index)))
       (and key ; no key available => can't inline
            (let ([from-grp (find-or-add-import-group! grps key
                                                       get-import-knowns
                                                       add-group!
                                                       next-index
                                                       (lambda () (set! next-index (add1 next-index))))])
              (and from-grp
                   (find-or-add-import-from-group! from-grp ext-id imports))))]
      [else
       (find-or-add-import-from-group! grp ext-id imports)])))

(define (find-or-add-import-from-group! grp ext-id imports)
  (or (for/or ([im (in-list (import-group-imports grp))])
        (and (eq? ext-id (import-ext-id im))
             (import-int-id im)))
      ;; `ext-id` from the group is not currently imported; add it as an import
      (let ([id (gensym ext-id)]
            [int-id (gensym ext-id)])
        (define im (import grp id int-id ext-id))
        (set-import-group-imports! grp (cons im (import-group-imports grp)))
        (hash-set! imports int-id im)
        int-id)))

(define (find-or-add-import-group! grps key get-import-knowns add-group! next-index inc-index!)
  (or (for/or ([grp (in-list grps)])
        (and (eq? key (import-group-key grp))
             grp))
      ;; The current linklet doesn't currently import from the linklet
      ;; that supplies an identifier to be inlined; add the linklet
      ;; as a new import group
      (let ([grp (import-group next-index key
                               get-import-knowns #f #f
                               '())])
        (inc-index!)
        (add-group! grp)
        grp)))
