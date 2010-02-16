; Plenty of code borrowed from the HtDP languages.
#lang scheme
(require drscheme/tool
         framework/preferences
         plai/private/tool-private
         string-constants)

(provide tool@)

(define tool@
  (unit 
    (import drscheme:tool^)
    (export drscheme:tool-exports^)
    
    (define (language-extension %)
      (class %
        (inherit get-reader get-module)
        (inherit-field reader-module module)
        
        (define/override (get-reader-module) reader-module)
        
        (define/override (front-end/complete-program port settings)
          (plai-complete-program port settings (get-reader) (get-module)))
        
        ; drscheme/private/auto-language.ss insists on #reader
        ; "#lang plai\n"
        (define/override (get-metadata modname settings)
          (string-append
           ";; The first three lines of this file were inserted by DrScheme. They record metadata\n"
           ";; about the language level of this file in a form that our tools can easily process.\n"
           (format "#reader~s~n" reader-module)))
        
        ;; Change print style in default settings from 'write to 'constructor:
        (define/override (default-settings)
          (let ([s (super default-settings)])
            (drscheme:language:make-simple-settings
             (drscheme:language:simple-settings-case-sensitive s)
             'constructor
             (drscheme:language:simple-settings-fraction-style s)
             (drscheme:language:simple-settings-show-sharing s)
             (drscheme:language:simple-settings-insert-newlines s)
             'test-coverage)))
        
        (define/override (metadata->settings metadata)
          (default-settings))
        
        (define/override (get-metadata-lines) 3)
        
        (super-new)))
    
    
    ;; module-based-language-extension :    (implements drscheme:language:module-based-language<%>) 
    ;;                                   -> (implements drscheme:language:module-based-language<%>)
    ;; changes the default settings and sets a few more paramters during `on-execute'
    (define (module-based-language-extension super%)
      (class* super% ()
        
        (init-field reader-module)
        (define/override (default-settings)
          (super default-settings))
        
        (define/override (marshall-settings x)
          (super marshall-settings x))
        
        (define/override (unmarshall-settings x)
          (super unmarshall-settings x))
        
        (super-new)))
    
    ; Returns #t if PLAI is being run for the first time and #f otherwise.
    ; This is determined by the value of the plai:first-run field.  If the
    ; field is not present, assume #t.
    (define (is-first-run?)
      (preferences:set-default 'plai:first-run #t boolean?)
      (preferences:get 'plai:first-run))    
    
    ; Add type-case to lambda-like keywords, only if (is-first-run?) => #t.  PLT Scheme 4.0 adds ^def as a
    ; regexp for define-like keywords.  Hence, we no longer have a clause here for define-type.
    (define (setup-indentation!)
      (when (is-first-run?)
        (preferences:set 'plai:first-run #f)
        (let ([indentation-ht (first (preferences:get 'framework:tabify))])
          (hash-set! indentation-ht 'type-case 'lambda)
          #;(hash-set! indentation-ht 'define-type 'define))))
    
    (define (phase1) (void))
    
    ;; phase2 : -> void
    (define (phase2)
      (local ([define plai-language%
                ((drscheme:language:get-default-mixin)
                 (language-extension
                  (drscheme:language:module-based-language->language-mixin
                   (module-based-language-extension
                    (drscheme:language:simple-module-based-language->module-based-language-mixin
                     drscheme:language:simple-module-based-language%)))))]
              
              [define next-language-number (box 1)]
              
              [define (add-plai-language #:summary summary #:module module #:title title #:reader reader-module)
                (drscheme:language-configuration:add-language
                 (instantiate plai-language% ()
                   (one-line-summary summary)
                   (module module)
                   (reader-module reader-module) 
                   (language-position (list (string-constant teaching-languages)
                                            "Programming Languages: Application and Interpretation"
                                            title))
                   (language-numbers `(-500 -400 ,(unbox next-language-number)))))
                (set-box! next-language-number (add1 (unbox next-language-number)))])
        
        (add-plai-language #:summary "Scheme with datatypes" 
                           #:module `plai
                           #:reader `plai/lang/reader
                           #:title "PLAI Scheme")
        (add-plai-language #:summary "language for writing garbage collectors"
                           #:module `plai/collector
                           #:reader `plai/collector/lang/reader
                           #:title "GC Collector Scheme")
        (add-plai-language #:summary "language for testing garbage collectors"
                           #:module `plai/mutator
                           #:reader `plai/mutator/lang/reader
                           #:title "GC Mutator Scheme")
        (add-plai-language #:summary "language for writing web applications"
                           #:module `plai/web
                           #:reader `plai/web/lang/reader
                           #:title "Web Application")
        (setup-indentation!)))
    ))
