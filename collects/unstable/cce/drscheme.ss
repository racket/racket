#lang scheme/gui

(require drscheme/tool
         string-constants
         "dict.ss"
         (only-in test-engine/scheme-gui make-formatter)
         (only-in test-engine/scheme-tests
                  scheme-test-data test-format test-execute)
         (lib "test-display.scm" "test-engine"))

(provide language-level^
         language-level@)

(define (read-all-syntax [port (current-input-port)]
                         [source (object-name port)]
                         [reader read-syntax])
  (let loop ()
    (let* ([next (reader source port)])
      (if (eof-object? next)
          null
          (cons next (loop))))))

(define (read-module-body [port (current-input-port)]
                          [source (object-name port)]
                          [reader read-syntax]
                          [path 'scheme]
                          [name 'program])
  (let*-values ([(line-1 col-1 pos-1) (port-next-location port)]
                [(terms) (read-all-syntax port source reader)]
                [(line-2 col-2 pos-2) (port-next-location port)]
                [(loc) (list source line-1 col-1 pos-1
                             (and pos-1 pos-2 (- pos-2 pos-1)))])
    (map (lambda (datum) (datum->syntax #'here datum loc))
         (list `(module ,name ,path
                  (,(datum->syntax #f '#%module-begin) ,@terms))
               `(require ',name)
               `(current-namespace (module->namespace '',name))))))

(define-signature language-level^
  (simple-language-level%
   make-language-level
   language-level-render-mixin
   language-level-capability-mixin
   language-level-eval-as-module-mixin
   language-level-no-executable-mixin
   language-level-macro-stepper-mixin
   language-level-check-expect-mixin
   language-level-metadata-mixin))

(define-unit language-level@
  (import drscheme:tool^)
  (export language-level^)

  (define (make-language-level
           name path
           #:number [number (equal-hash-code name)]
           #:hierarchy [hierarchy experimental-language-hierarchy]
           #:summary [summary name]
           #:url [url #f]
           #:reader [reader read-syntax]
           . mixins)
    (let* ([mx-default (drscheme:language:get-default-mixin)]
           [mx-custom (apply compose (reverse mixins))])
      (new (mx-custom (mx-default simple-language-level%))
           [module path]
           [language-position (append (map car hierarchy) (list name))]
           [language-numbers (append (map cdr hierarchy) (list number))]
           [one-line-summary summary]
           [language-url url]
           [reader (make-namespace-syntax-reader reader)])))

  (define simple-language-level%
    (drscheme:language:module-based-language->language-mixin
     (drscheme:language:simple-module-based-language->module-based-language-mixin
      drscheme:language:simple-module-based-language%)))

  (define (language-level-render-mixin to-sexp show-void?)
    (mixin (drscheme:language:language<%>) ()
      (super-new)

      (define/override (render-value/format value settings port width)
        (unless (and (void? value) (not show-void?))
          (super render-value/format (to-sexp value) settings port width)))))

  (define (language-level-capability-mixin dict)
    (mixin (drscheme:language:language<%>) ()
      (super-new)

      (define/augment (capability-value key)
        (dict-ref/failure
         dict key
         (lambda ()
           (inner (drscheme:language:get-capability-default key)
                  capability-value key))))))

  (define language-level-no-executable-mixin
    (mixin (drscheme:language:language<%>) ()
      (super-new)
      (inherit get-language-name)

      (define/override (create-executable settings parent filename)
        (message-box
         "Create Executable: Error"
         (format "Sorry, ~a does not support creating executables."
                 (get-language-name))
         #f '(ok stop)))))

  (define language-level-eval-as-module-mixin
    (mixin (drscheme:language:language<%>
            drscheme:language:module-based-language<%>) ()
      (super-new)

      (inherit get-reader get-module)

      (define/override (front-end/complete-program port settings)
        (let* ([terms #f])
          (lambda ()
            ;; On the first run through, initialize the list.
            (unless terms
              (set! terms (read-module-body port
                                            (object-name port)
                                            (get-reader)
                                            (get-module))))
            ;; Produce each list element in order.
            (if (pair? terms)
                ;; Produce and remove a list element.
                (begin0 (car terms) (set! terms (cdr terms)))
                ;; After null, eof forever.
                eof))))))

  (define language-level-macro-stepper-mixin
    (language-level-capability-mixin
     (make-immutable-hasheq
      (list (cons 'macro-stepper:enabled #t)))))

  (define language-level-check-expect-mixin
    (mixin (drscheme:language:language<%>) ()
      (super-new)
      (inherit render-value/format)

      (define/augment (capability-value key)
        (case key
          [(tests:test-menu tests:dock-menu) #t]
          [else (inner (drscheme:language:get-capability-default key)
                       capability-value
                       key)]))

      (define/override (on-execute settings run-in-user-thread)
        (let* ([drscheme-namespace (current-namespace)]
               [test-engine-path
                ((current-module-name-resolver)
                 'test-engine/scheme-tests #f #f)])
          (run-in-user-thread
           (lambda ()
             (namespace-attach-module drscheme-namespace test-engine-path)
             (namespace-require test-engine-path)
             (scheme-test-data
              (list (drscheme:rep:current-rep)
                    drscheme-eventspace
                    test-display%))
             (test-execute (get-preference 'tests:enable? (lambda () #t)))
             (test-format
              (make-formatter
               (lambda (v o) (render-value/format v settings o 40))))))
          (super on-execute settings run-in-user-thread)))))

  (define (language-level-metadata-mixin reader-module
                                         meta-lines
                                         meta->settings
                                         settings->meta)
    (mixin (drscheme:language:language<%>) ()
      (inherit default-settings)
      (super-new)

      (define/override (get-reader-module) reader-module)

      (define/override (get-metadata modname settings)
        (settings->meta modname settings))

      (define/override (metadata->settings metadata)
        (meta->settings metadata (default-settings)))

      (define/override (get-metadata-lines) meta-lines)))

  (define (generic-syntax-reader . args)
    (parameterize ([read-accept-reader #t])
      (apply read-syntax args)))

  (define (make-namespace-syntax-reader reader)
    (lambda args
      (let ([stx (apply reader args)])
        (if (syntax? stx) (namespace-syntax-introduce stx) stx))))

  (define drscheme-eventspace (current-eventspace))

  (define experimental-language-hierarchy
    (list (cons (string-constant experimental-languages)
                1000))))
