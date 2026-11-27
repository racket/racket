#lang racket/base

(require
 racket/port
 racket/runtime-path
 syntax/module-reader
 syntax/readerr
 (file "../../racket-tstring/private/source-transform.rkt")
) ; end require

(provide
 (rename-out (tstring-read read)
             (tstring-read-syntax read-syntax)
             (tstring-get-info get-info)
 ) ; end rename-out
) ; end provide

(define-runtime-path main-rkt "../main.rkt")

(define (wrap-reader proc)
  (lambda args
    (define-values (prefix port suffix)
      (split-reader-args args)
    ) ; end define-values
    (define transformed-port
      (open-input-string
       (string-append (format "(require (file ~s))\n" (path->string main-rkt))
                      (transform-with-read-errors port)
       ) ; end string-append
      ) ; end open-input-string
    ) ; end define transformed-port
    (port-count-lines! transformed-port)
    (apply proc
           (append prefix
                   (list transformed-port)
                   suffix
           ) ; end append
    ) ; end apply
  ) ; end lambda
) ; end define wrap-reader

(define (split-reader-args args)
  (let loop ((prefix '())
             (rest args)
        ) ; end loop bindings
    (cond
      ((null? rest)
       (error 'tstring-reader "reader arguments do not include an input port")
      ) ; end no port
      ((input-port? (car rest))
       (values (reverse prefix)
               (car rest)
               (cdr rest)
       ) ; end values
      ) ; end found port
      (else
       (loop (cons (car rest) prefix)
             (cdr rest)
       ) ; end loop
      ) ; end keep searching
    ) ; end cond
  ) ; end let loop
) ; end define split-reader-args

(define (transform-with-read-errors port)
  (define source (port->string port))
  (with-handlers ((exn:fail?
                   (lambda (exn)
                     (raise-read-error (exn-message exn)
                                       (object-name port)
                                       #f
                                       #f
                                       #f
                                       #f
                     ) ; end raise-read-error
                   ) ; end lambda
                  ) ; end exn:fail?
                 ) ; end handlers
    (transform-template-prefixes source)
  ) ; end with-handlers
) ; end define transform-with-read-errors

(define-values (tstring-read tstring-read-syntax tstring-get-info)
  (make-meta-reader 'tstring
                    "language path"
                    lang-reader-module-paths
                    wrap-reader
                    wrap-reader
                    (lambda (proc)
                      (lambda (key default)
                        (if proc
                            (proc key default)
                            default
                        ) ; end if
                      ) ; end lambda
                    ) ; end get-info wrapper
  ) ; end make-meta-reader
) ; end define-values
