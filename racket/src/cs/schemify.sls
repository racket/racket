(library (schemify)
  (export schemify-linklet
          lift-in-schemified-linklet
          jitify-schemified-linklet
          xify
          extract-paths-from-schemified-linklet
          interpretable-jitified-linklet
          interpret-linklet
          linklet-bigger-than?
          make-path->compiled-path
          compiled-path->path
          force-unfasl
          prim-knowns
          known-procedure
          known-procedure/pure
          known-procedure/succeeds
          a-known-constant)
  (import (except (chezpart)
                  datum->syntax)
          (rename (rumble)
                  [correlated? rumble:correlated?]
                  [correlated-e rumble:correlated-e]
                  [correlated-property rumble:correlated-property]
                  [datum->correlated rumble:datum->correlated]
                  [correlated-source rumble:correlated-source]
                  [correlated-line rumble:correlated-line]
                  [correlated-column rumble:correlated-column]
                  [correlated-position rumble:correlated-position]
                  [correlated-span rumble:correlated-span])
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
             'syntax-property rumble:correlated-property
             'datum->syntax rumble:datum->correlated
             'syntax-source rumble:correlated-source
             'syntax-line rumble:correlated-line
             'syntax-column rumble:correlated-column
             'syntax-position rumble:correlated-position
             'syntax-span rumble:correlated-span)]
      [else #f]))

  ;; For direct access by schemified schemify:
  (define syntax? rumble:correlated?)
  (define syntax-e rumble:correlated-e)
  (define syntax-property rumble:correlated-property)
  (define datum->syntax rumble:datum->correlated)
  (define syntax-source rumble:correlated-source)
  (define syntax-line rumble:correlated-line)
  (define syntax-column rumble:correlated-column)
  (define syntax-position rumble:correlated-position)
  (define syntax-span rumble:correlated-span)

  (include "include.ss")
  (include-generated "schemify.scm")

  (define prim-knowns
    (let-syntax ([gen
                  (lambda (stx)
                    ;; Construct a hash table that maps symbols to
                    ;; `known` prefabs
                    (let ([known-l '()])
                      (define-syntax define-primitive-table
                        (syntax-rules ()
                          [(_ id [prim known] ...)
                           (begin (set! known-l (cons (cons 'prim 'known) known-l))
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
                      (let ([knowns (make-hashtable equal-hash equal?)])
                        (for-each (lambda (k)
                                    (hashtable-set! knowns (cdr k) (gensym)))
                                  known-l)
                        (with-syntax ([(id) stx])
                          (#%datum->syntax
                           #'id
                           `(let ([ht (make-eq-hashtable)]
                                  ,@(#%map (lambda (k)
                                             `[,(hashtable-ref knowns k #f) ,k])
                                           (#%vector->list (hashtable-keys knowns))))
                              ,@(#%map (lambda (k)
                                         `(hashtable-set! ht ',(car k)
                                                          ,(hashtable-ref knowns (cdr k) #f)))
                                       known-l)
                              (eq-hashtable->hash ht)))))))])
      (gen))))
