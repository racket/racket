(module require-transform '#%kernel
  (#%require "private/stxcase-scheme.rkt"
             "private/stx.rkt"
             "private/define-struct.rkt"
             "private/small-scheme.rkt"
             "private/define.rkt"
             (for-template (only '#%kernel quote))
             (for-syntax '#%kernel))
  
  (#%provide expand-import 
             syntax-local-require-certifier 
             make-require-transformer prop:require-transformer require-transformer?
             ;; the import struct type:
             import struct:import make-import import?
             import-local-id import-src-sym import-src-mod-path import-orig-stx import-mode import-req-mode import-orig-mode
             ;; the import-source struct type:
             import-source struct:import-source make-import-source import-source?
             import-source-mod-path-stx import-source-mode)

  (define-struct* import (local-id src-sym src-mod-path mode req-mode orig-mode orig-stx)
    #:guard (lambda (i s path mode req-mode orig-mode stx info)
              (unless (identifier? i)
                (raise-type-error 'make-import "identifier" i))
              (unless (symbol? s)
                (raise-type-error 'make-import "symbol" s))
              (unless (module-path? path)
                (raise-type-error 'make-import "module-path" path))
              (unless (or (not mode)
                          (exact-integer? mode))
                (raise-type-error 'make-import "exact integer or #f" mode))
              (unless (or (not req-mode)
                          (exact-integer? req-mode))
                (raise-type-error 'make-import "'exact integer or #f" req-mode))
              (unless (or (not orig-mode)
                          (exact-integer? orig-mode))
                (raise-type-error 'make-import "'exact integer or #f" orig-mode))
              (unless (equal? mode (and req-mode orig-mode (+ req-mode orig-mode)))
                (raise-mismatch-error 'make-import 
                                      (format
                                       "orig mode: ~a and require mode: ~a not consistent with mode: " 
                                       orig-mode req-mode)
                                      mode))
              (unless (syntax? stx)
                (raise-type-error 'make-import "syntax" stx))
              (values i s path mode req-mode orig-mode stx)))
              
  (define-struct* import-source (mod-path-stx mode)
    #:guard (lambda (path mode info)
              (unless (and (syntax? path)
                           (module-path? (syntax->datum path)))
                (raise-type-error 'make-import-source "syntax module-path" path))
              (unless (or (not mode)
                          (exact-integer? mode))
                (raise-type-error 'make-import-source "exact integer or #f" mode))
              (values path mode)))
    
  (define-values (prop:require-transformer require-transformer? require-transformer-get-proc)
    (make-struct-type-property 'require-transformer))
  
  (define-struct* rt (proc)
    #:property prop:require-transformer (lambda (t) (rt-proc t)))
  
  (define (make-require-transformer proc)
    (make-rt proc))

  ;; For backward compatibility:
  (define (syntax-local-require-certifier)
    (case-lambda 
     [(v) v]
     [(v mark) v]))

  (define orig-insp (variable-reference->module-declaration-inspector
                     (#%variable-reference)))

  ;; expand-import : stx bool -> (listof import)
  (define (expand-import stx)
    (let ([disarmed-stx (syntax-disarm stx orig-insp)])
      (syntax-case disarmed-stx ()
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
           (let ([namess (syntax-local-module-exports mod-path)])
             (values
              (apply
               append
               (map (lambda (names)
                      (let ([mode (car names)])
                        (map (lambda (name)
                               (make-import (datum->syntax
                                             stx
                                             name
                                             stx)
                                            name
                                            mod-path
                                            mode
                                            0
                                            mode
                                            stx))
                             (cdr names))))
                    namess))
              (list (make-import-source (datum->syntax #'simple mod-path) 0)))))]
        [(id . rest)
         (identifier? #'id)
         (let ([t (syntax-local-value #'id (lambda () #f))])
           (if (require-transformer? t)
               (call-with-values
                   (lambda ()
                     (((require-transformer-get-proc t) t) disarmed-stx))
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
                    (format "require transformer produced ~a result~a instead of 2"
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
          stx)]))))
