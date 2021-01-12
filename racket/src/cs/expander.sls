(library (expander)
  (export current-command-line-arguments
          executable-yield-handler
          load-on-demand-enabled
          call-in-main-thread
          version
          exit
          compile-keep-source-locations!)
  (import (rename (except (chezpart)
                          syntax->datum
                          datum->syntax)
                  [define chez:define])
          (rename (rumble)
                  [correlated? syntax?]
                  [correlated-source syntax-source]
                  [correlated-line syntax-line]
                  [correlated-column syntax-column]
                  [correlated-position syntax-position]
                  [correlated-span syntax-span]
                  [correlated-e syntax-e]
                  [correlated->datum syntax->datum]
                  [datum->correlated datum->syntax]
                  [correlated-property syntax-property]
                  [correlated-property-symbol-keys syntax-property-symbol-keys]
                  ;; Remapped to place-local register operations:
                  [unsafe-place-local-ref rumble:unsafe-place-local-ref]
                  [unsafe-place-local-set! rumble:unsafe-place-local-set!])
          (thread)
          (regexp)
          (io)
          (linklet))

  (include "place-register.ss")
  (define-place-register-define define expander-register-start expander-register-count)

  ;; The expander needs various tables to set up primitive modules, and
  ;; the `primitive-table` function is the bridge between worlds

  (define user-installed-tables (make-hasheq))

  (define primitive-table
    (case-lambda
     [(key)
      (case key
        [(|#%linklet|) linklet-table]
        [(|#%kernel|) kernel-table]
        [(|#%read|) (make-hasheq)]
        [(|#%paramz|) paramz-table]
        [(|#%unsafe|) unsafe-table]
        [(|#%foreign|) foreign-table]
        [(|#%futures|) futures-table]
        [(|#%place|) place-table]
        [(|#%flfxnum|) flfxnum-table]
        [(|#%extfl|) extfl-table]
        [(|#%network|) network-table]
        [else (hash-ref user-installed-tables key #f)])]
     [(key table)
      (hash-set! user-installed-tables key table)]))

  (define-syntax define-primitive-table
    (syntax-rules ()
      [(_ id [prim known] ...)
       (define id
         (let ([ht (make-hasheq)])
           (hash-set! ht 'prim prim)
           ...
           (unsafe-hash-seal! ht)
           ht))]))

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
  (include "primitive/internal.ss")

  ;; ----------------------------------------

  (include "include.ss")
  (include-generated "expander.scm")

  ;; ----------------------------------------

  (include "expander/env.ss")

  ;; The environment is used to evaluate linklets, so all primitives
  ;; need to be there imported there
  (parameterize ([expand-omit-library-invocations #f])
    (eval environment-imports) ; defined in "expander/env.ss"
    ;; Ensure that the library is visited, especially for a wpo build:
    (eval 'variable-set!))

  (eval `(define primitive-table ',primitive-table))

  ;; For interpretation of the outer shell of a linklet:
  (install-linklet-primitive-tables! (cons '|#%kernel| kernel-table)
                                     (cons '|#%unsafe| unsafe-table)
                                     (cons '|#%flfxnum| flfxnum-table)
                                     (cons '|#%paramz| paramz-table)
                                     (cons '|#%extfl| extfl-table)
                                     (cons '|#%network| network-table)
                                     (cons '|#%futures| futures-table)
                                     (cons '|#%place| place-table)
                                     (cons '|#%foreign| foreign-table)
                                     (cons '|#%linklet| linklet-table)
                                     (cons 'internal internal-table)
                                     (cons 'schemify schemify-table))

  ;; ----------------------------------------

  ;; `install-reader!` is from the `io` library, where the
  ;; given functions are used by the default port read handler
  (install-reader! 1/read 1/read-syntax 1/read-accept-reader 1/read-accept-lang)

  ;; `set-string->number?!` is also from the `io` library, where
  ;; the printer needs to check whether a string parses as a number
  ;; for deciding wheter to quote the string
  (set-string->number?! (lambda (str)
                          (and (1/string->number str 10 'read)
                               ;; Special case: `#%` is never read as a number or error:
                               (not (and (>= (string-length str) 2)
                                         (eqv? (string-ref str 0) #\#)
                                         (eqv? (string-ref str 1) #\%))))))

  ;; `set-maybe-raise-missing-module!` is also from the `io` library
  (set-maybe-raise-missing-module! maybe-raise-missing-module))
