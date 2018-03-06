#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/error.rkt"
         "../common/module-path.rkt")

(provide current-module-path-for-load
         maybe-raise-missing-module
         
         prop:exn:missing-module
         exn:missing-module?
         exn:missing-module-accessor
         
         (struct-out exn:fail:filesystem:missing-module)
         make-exn:fail:filesystem:missing-module
         (struct-out exn:fail:syntax:missing-module)
         make-exn:fail:syntax:missing-module)

(define-values (prop:exn:missing-module exn:missing-module? exn:missing-module-accessor)
  (make-struct-type-property 'missing-module
                             (lambda (v info)
                               (unless (and (procedure? v)
                                            (procedure-arity-includes? v 1))
                                 (raise-argument-error 'guard-for-prop:exn:missing-module
                                                       "(procedure-arity-includes/c 1)"
                                                       v))
                               v)))

(struct exn:fail:filesystem:missing-module exn:fail:filesystem (path)
  #:extra-constructor-name make-exn:fail:filesystem:missing-module
  #:transparent
  #:property prop:exn:missing-module (lambda (e) (exn:fail:filesystem:missing-module-path e)))
(struct exn:fail:syntax:missing-module exn:fail:syntax (path)
  #:extra-constructor-name make-exn:fail:syntax:missing-module
  #:transparent
  #:property prop:exn:missing-module (lambda (e) (exn:fail:syntax:missing-module-path e)))

(define current-module-path-for-load
  (make-parameter #f
                  (lambda (v)
                    (unless (or (not v)
                                (module-path? v)
                                (and (syntax? v)
                                     (module-path? (syntax->datum v))))
                      (raise-argument-error
                       'current-module-path-for-load
                       (string-append "(or/c module-path?"
                                      " (and/c syntax? (lambda (stx) (module-path? (syntax->datum stx))))"
                                      " #f)")
                       v))
                    v)))

(define (maybe-raise-missing-module name filename pre rel post errstr)
  (define path (current-module-path-for-load))
  (when path
    (when (syntax? path)
      (raise
       (exn:fail:syntax:missing-module
        (format (string-append "~a: cannot open module file\n"
                               "  module path: ~a\n"
                               "  path: ~a~a~a~a\n"
                               "  system error: ~a")
                (if (syntax-srcloc path)
                    (srcloc->string (syntax-srcloc path))
                    name)
                (syntax->datum path)
                filename pre rel post
                errstr)
        (current-continuation-marks)
        (list path)
        (syntax->datum path))))
    (raise
       (exn:fail:filesystem:missing-module
        (format (string-append "~a: cannot open module file\n"
                               "  module path: ~a\n"
                               "  path: ~a~a~a~a\n"
                               "  system error: ~a")
                name
                path
                filename pre rel post
                errstr)
        (current-continuation-marks)
        path))))
