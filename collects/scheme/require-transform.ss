(module require-transform '#%kernel
  (#%require "private/stxcase-scheme.ss"
             "private/qqstx.ss"
             "private/stx.ss"
             "private/define-struct.ss"
             "private/more-scheme.ss"
             "private/small-scheme.ss"
             "private/define.ss"
             (for-template (only '#%kernel quote)))
  
  (#%provide expand-import
             make-require-transformer prop:require-transformer require-transformer?
             ;; the import struct type:
             import struct:import make-import import?
             import-local-id import-src-sym import-src-mod-path import-orig-stx import-mode import-req-mode
             ;; the import-source struct type:
             import-source struct:import-source make-import-source import-source?
             import-source-mod-path-stx import-source-mode)
  
  (define-struct* import (local-id src-sym src-mod-path mode req-mode orig-stx)
    #:guard (lambda (i s path mode req-mode stx info)
              (unless (identifier? i)
                (raise-type-error 'make-import "identifier" i))
              (unless (symbol? s)
                (raise-type-error 'make-import "symbol" s))
              (unless (module-path? path)
                (raise-type-error 'make-import "module-path" path))
              (unless (memq mode '(run syntax template label))
                (raise-type-error 'make-import "'run, 'syntax, 'template, or 'label" mode))
              (unless (memq req-mode '(run syntax template label))
                (raise-type-error 'make-import "'run, 'syntax, 'template, or 'label" req-mode))
              (unless (syntax? stx)
                (raise-type-error 'make-import "syntax" stx))
              (values i s path mode req-mode stx)))
              
  (define-struct* import-source (mod-path-stx mode)
    #:guard (lambda (path mode info)
              (unless (and (syntax? path)
                           (module-path? (syntax->datum path)))
                (raise-type-error 'make-import-source "syntax module-path" path))
              (unless (memq mode '(run syntax template label))
                (raise-type-error 'make-import-source "'run, 'syntax, 'template, or 'label" mode))
              (values path mode)))
    
  (define-values (prop:require-transformer require-transformer? require-transformer-get-proc)
    (make-struct-type-property 'require-transformer))
  
  (define-struct* rt (proc)
    #:property prop:require-transformer (lambda (t) (rt-proc t)))
  
  (define (make-require-transformer proc)
    (make-rt proc))
  
  ;; expand-import : stx bool -> (listof import)
  (define (expand-import stx)
    (syntax-case stx ()
      [simple
       (or (identifier? #'simple)
           (string? (syntax-e #'simple))
           (syntax-case stx (quote)
             [(quote s) #t]
             [_ #f]))
       (let ([mod-path
              (if (pair? (syntax-e #'simple))
                  `(quote . ,(cdr (syntax->datum #'simple)))
                  (syntax->datum #'simple))])
         (unless (module-path? mod-path)
           (raise-syntax-error
            #f
            "invalid module-path form"
            stx))
         (let-values ([(names et-names lt-names) (syntax-local-module-exports stx)])
           (values
            (apply
             append
             (map (lambda (names mode)
                    (map (lambda (name)
                           (make-import (datum->syntax
                                         stx
                                         name
                                         stx)
                                        name
                                        mod-path
                                        mode
                                        'run
                                        stx))
                         names))
                  (list names et-names lt-names)
                  (list 'run 'syntax 'label)))
            (list (make-import-source #'simple 'run)))))]
      [(id . rest)
       (identifier? #'id)
       (let ([t (syntax-local-value #'id (lambda () #f))])
         (if (require-transformer? t)
             (call-with-values
                 (lambda ()
                   (((require-transformer-get-proc t) t) stx))
               (case-lambda
                [(v mods)
                 (unless (and (list? v)
                              (andmap import? v))
                   (raise-syntax-error
                    #f
                    "first result from require transformer is not a list of imports"
                    stx))
                 (unless (and (list? mods)
                              (andmap import-source? mods))
                   (raise-syntax-error
                    #f
                    "second result from require transformer is not a list of import-sources"
                    stx))
                 (values v mods)]
                [args
                 (raise-syntax-error
                    #f
                    (format "require transformer did not produced ~a result~s instead of 2"
                            (length args)
                            (if (= 1 (length args)) "" "s"))
                    stx)]))
             (raise-syntax-error
              #f
              "not a require sub-form"
              stx)))]
      [_
       (raise-syntax-error
        #f
        "bad syntax for require sub-form"
        stx)])))
