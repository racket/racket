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
          (linklet)
          (only (schemify)
                force-unfasl))

  (include "place-register.ss")
  (define-place-register-define define expander-register-start expander-register-count)

  ;; Set to `#t` to make compiled code reliably compatible with
  ;; changes to primitive libraries. Changing ths setting makes
  ;; the build incompatible with previously generated ".zo" files.
  (define compile-as-independent? #f)

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

  ;; The environment is used to evaluate linklets, so all primitives
  ;; need to be there imported (prefered) or defined (less efficient,
  ;; but less tied to library implementations)
  (unless compile-as-independent?
    (parameterize ([expand-omit-library-invocations #f])
      (eval `(import (rename (rumble)
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
                             [correlated-property-symbol-keys syntax-property-symbol-keys])
                     (thread)
                     (io)
                     (regexp)
                     (linklet)
                     (only (schemify)
                           force-unfasl)))
      ;; Ensure that the library is visited, especially for a wpo build:
      (eval 'variable-set!)))

  (eval `(define primitive-table ',primitive-table))

  (let ([install-table
         (lambda (table)
           (hash-for-each table
                          (lambda (k v)
                            ;; Avoid redefining some primitives that we
                            ;; don't have to replace:
                            (unless (memq k '(vector
                                              list cons car cdr
                                              eq?
                                              values call-with-values))
                              (eval `(define ,k ',v))))))])
    (when compile-as-independent?
      (install-table kernel-table)
      (install-table unsafe-table)
      (install-table flfxnum-table)
      (install-table paramz-table)
      (install-table extfl-table)
      (install-table network-table)
      (install-table futures-table)
      (install-table place-table)
      (install-table foreign-table)
      (install-table linklet-table)
      (install-table internal-table)
      (install-table schemify-table)))

  (when compile-as-independent?
    ;; Copies of macros provided by `rumble`, plus
    ;; other bindings assumed by schemify:
    (eval '(define-syntax with-continuation-mark
             (syntax-rules ()
               [(_ key val body)
                (call-with-current-continuation-attachment
                 empty-mark-frame
                 (lambda (a)
                   (call-setting-continuation-attachment
                    (mark-frame-update a key val)
                    (lambda ()
                      body))))])))
    (eval '(define call-with-immediate-continuation-mark call-with-immediate-continuation-mark/proc))
    (eval '(define-syntax begin0
             (syntax-rules ()
               [(_ expr0 expr ...)
                (call-with-values (lambda ()
                                    (call-with-values (lambda () expr0)
                                      (case-lambda
                                       [(x) (values x #f)]
                                       [args (values args #t)])))
                  (lambda (l apply?)
                    expr ...
                    (if apply?
                        (#%apply values l)
                        l)))])))
    (eval '(define-syntax (|#%app| stx)
             (syntax-case stx ()
               [(_ rator rand ...)
                (with-syntax ([n-args (length #'(rand ...))])
                  #'((extract-procedure rator n-args) rand ...))])))
    (eval '(define-syntax (|#%name| stx)
             (syntax-case stx ()
               [(_ name val) #`(let ([name val]) name)])))
    (eval `(define raise-binding-result-arity-error ',raise-binding-result-arity-error)))

  ;; For interpretation of the outer shell of a linklet:
  (install-linklet-primitive-tables! kernel-table
                                     unsafe-table
                                     flfxnum-table
                                     paramz-table
                                     extfl-table
                                     network-table
                                     futures-table
                                     place-table
                                     foreign-table
                                     linklet-table
                                     internal-table
                                     schemify-table)

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
