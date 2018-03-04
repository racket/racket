#lang racket/base
(require "../common/prefab.rkt"
         "readtable.rkt"
         "config.rkt"
         "special.rkt"
         "parameter.rkt"
         "error.rkt"
         "wrap.rkt"
         "closer.rkt"
         "sequence.rkt")

(provide read-struct)

(define (read-struct read-one dispatch-c in config)
  (define c (read-char/special in config))
  
  (define-syntax-rule (guard-legal e body ...)
    (cond
     [e body ...]
     [else (bad-syntax-error in config (format "~as~a" dispatch-c c))]))
  
  (define ec (effective-char c config))
  (define seq
    (case ec
      [(#\()
       (read-struct-sequence read-one c #\( #\) in config)]
      [(#\[)
       (guard-legal
        (check-parameter read-square-bracket-as-paren config)
        (read-struct-sequence read-one c #\[ #\] in config))]
      [(#\{)
       (guard-legal
        (check-parameter read-curly-brace-as-paren config)
        (read-struct-sequence read-one c #\{ #\} in config))]
      [else
       (reader-error in config
                     "expected ~a after `~as`"
                     (all-openers-str config)
                     dispatch-c)]))
  
  (when (null? seq)
    (reader-error in config
                  "missing structure description in `~as` form"
                  dispatch-c))

  (unless (prefab-key? (car seq))
    (reader-error in config
                  "invalid structure description in `~as` form"
                  dispatch-c))

  (define st (with-handlers ([exn:fail? (lambda (exn) #f)])
               (prefab-key->struct-type (car seq) (length (cdr seq)))))
  (unless st
    (reader-error in config
                  (string-append "mismatch between structure description"
                                 " and number of provided field values in `~as` form")
                  dispatch-c))
  
  (when (read-config-for-syntax? config)
    (unless (all-fields-immutable? (car seq))
      (reader-error in config
                    "cannot read mutable `~as` form as syntax"
                    dispatch-c)))
  
  (wrap (apply make-prefab-struct seq)
        in
        config
        ec))

;; ----------------------------------------

(define (read-struct-sequence read-one opener-c opener closer in config)
  (read-unwrapped-sequence read-one opener-c opener closer in config
                           #:first-read-one (lambda (init-c in config)
                                              (read-one init-c in (disable-wrapping config)))))
