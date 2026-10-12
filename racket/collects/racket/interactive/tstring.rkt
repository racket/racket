#lang racket/base

(require
 racket/port
 racket/string
 syntax/readerr
) ; end require

(namespace-require '(lib "tstring/main.rkt"))

(define (tstring-read-interaction source-name in)
  (let loop ((lines '()))
    (define line (read-line in 'any))
    (cond
      ((eof-object? line)
       (if (null? lines)
           eof
           (read-transformed-interaction source-name (string-join (reverse lines) "\n"))
       ) ; end if
      ) ; end eof
      (else
       (define source (string-join (reverse (cons line lines)) "\n"))
       (with-handlers ((exn:fail:read:eof?
                        (lambda (exn)
                          (loop (cons line lines))
                        ) ; end lambda
                       ) ; end exn:fail:read:eof?
                      ) ; end handlers
         (read-transformed-interaction source-name source)
       ) ; end with-handlers
      ) ; end line
    ) ; end cond
  ) ; end let loop
) ; end define tstring-read-interaction

(define (read-transformed-interaction source-name source)
  (define transformed-source ((get-transform-template-prefixes) source))
  (define in (open-input-string transformed-source))
  (port-count-lines! in)
  (define stx (read-syntax source-name in))
  (define extra-stx (read-syntax source-name in))
  (unless (eof-object? extra-stx)
    (raise-read-error "expected a single interaction"
                      source-name
                      #f
                      #f
                      #f
                      #f
    ) ; end raise-read-error
  ) ; end unless eof
  stx
) ; end define read-transformed-interaction

(define transform-template-prefixes-proc #f)

(define (get-transform-template-prefixes)
  (unless transform-template-prefixes-proc
    (set! transform-template-prefixes-proc
          (dynamic-require '(lib "racket-tstring/private/source-transform.rkt")
                           'transform-template-prefixes
          ) ; end dynamic-require
    ) ; end set!
  ) ; end unless cached
  transform-template-prefixes-proc
) ; end define get-transform-template-prefixes

(when (collection-file-path "main.rkt" "xrepl"
                            #:fail (lambda _ #f)
      ) ; end collection-file-path
  (dynamic-require 'xrepl #f)
  (define toplevel-prefix (dynamic-require 'xrepl/xrepl 'toplevel-prefix))
  (toplevel-prefix "")
) ; end when xrepl

(let ((init-file (cleanse-path (find-system-path 'init-file))))
  (when (file-exists? init-file)
    (load init-file)
  ) ; end when init file
) ; end let init-file

(current-read-interaction tstring-read-interaction)
