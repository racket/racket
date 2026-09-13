#lang racket/base

(provide make-not-available)

(define (make-not-available id)
  (lambda ()
    (make-keyword-procedure
     (lambda (kws kw-args . args)
       (raise
        (exn:fail:unsupported
         (string-append
          (format "~a: " id)
          "implementation not found"
          (if (null? args)
              ";\n no arguments provided"
              (apply
               string-append
               "\n  arguments...:"
               (append
                (let loop ([kws kws] [kw-args kw-args])
                  (if (null? kws)
                      null
                      (cons (format "\n   ~a ~e"
                                    (car kws)
                                    (car kw-args))
                            (loop (cdr kws) (cdr kw-args)))))
                (let loop ([args args])
                  (if (null? args)
                      null
                      (cons (format "\n   ~e"
                                    (car args))
                            (loop (cdr args)))))))))
         (current-continuation-marks)))))))
