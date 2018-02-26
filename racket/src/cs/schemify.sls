(library (schemify)
  (export schemify-linklet
          lift-in-schemified-linklet
          jitify-schemified-linklet
          interpretable-jitified-linklet
          interpret-linklet
          linklet-bigger-than?
          prim-knowns
          known-procedure
          a-known-constant)
  (import (chezpart)
          (rename (rumble)
                  [correlated? rumble:correlated?]
                  [correlated-e rumble:correlated-e]
                  [correlated-property rumble:correlated-property])
          (regexp)
          (io))

  ;; Bridge for flattened "schemify/wrap.rkt"
  ;; and "schemify/wrap-annotation.rkt"
  (define (primitive-table name)
    (case name
      [(|#%kernel|)
       ;; Normally, schemify is schemified so that these are accessed
       ;; directly, instead:
       (hash 'syntax? rumble:correlated?
             'syntax-e rumble:correlated-e
             'syntax-property rumble:correlated-property)]
      [else #f]))

  ;; For direct access by schemified schemify:
  (define syntax? rumble:correlated?)
  (define syntax-e rumble:correlated-e)
  (define syntax-property rumble:correlated-property)

  (include "include.ss")
  (include-generated "schemify.scm")

  (define prim-knowns
    (let-syntax ([gen
                  (lambda (stx)
                    (include-generated "known.scm")
                    ;; Constructed a quoted literal hash table that
                    ;; maps symbols to `known` prefabs
                    (let ([known-l '()])
                      (define-syntax define-primitive-table
                        (syntax-rules ()
                          [(_ id [prim known] ...)
                           (begin (set! known-l (cons (cons 'prim known) known-l))
                                  ...)]))
                      (include "primitive/kernel.ss")
                      (include "primitive/unsafe.ss")
                      (include "primitive/flfxnum.ss")
                      (include "primitive/paramz.ss")
                      (include "primitive/extfl.ss")
                      (include "primitive/network.ss")
                      (include "primitive/futures.ss")
                      (include "primitive/place.ss")
                      (include "primitive/foreign.ss")
                      (include "primitive/linklet.ss")
                      (let loop ([l known-l] [knowns (hasheq)])
                        (if (null? l)
                            #`(quote #,knowns)
                            (loop (cdr l)
                                  (hash-set knowns (caar l) (cdar l)))))))])
      (gen))))
