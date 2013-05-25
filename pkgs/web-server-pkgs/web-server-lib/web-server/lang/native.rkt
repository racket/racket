#lang racket/base
(require web-server/lang/abort-resume
         (for-syntax racket/base))

(define-syntax (define-native stx)
  (syntax-case stx ()
    [(_ (id . argspec) original)
     (quasisyntax/loc stx
       (define id
         (lambda id-args
           (serial->native
            (apply original
                   (map (lambda (higher-order? arg)
                          (if higher-order?
                              (lambda arg-args
                                (native->serial (apply arg arg-args)))
                              arg))
                        (list #,@(map (lambda (arg)
                                        (syntax-case arg (ho)
                                          [ho #t]
                                          [_ #f]))
                                      (syntax->list #'argspec)))
                        id-args))))))]))

(provide define-native)
