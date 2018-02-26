#lang racket/base

(provide (struct-out parse-config)
         make-parse-config
         config-case-sensitive
         config-multi-line
         config-group-number
         config-group-number+1)

(struct parse-config (who
                      px?
                      case-sensitive?
                      multi-line?
                      group-number-box
                      references?-box
                      error-handler?))

(define (make-parse-config #:who [who 'regexp]
                           #:px? [px? #f]
                           #:error-handler? [error-handler? #f])
  (parse-config who
                px?
                #t ; case-sensitive?
                #f ; multi-line?
                (box 0)  ; group-number-box
                (box #f) ; references?-box
                error-handler?))

(define (config-case-sensitive config cs?)
  (struct-copy parse-config config
               [case-sensitive? cs?]))

(define (config-multi-line config mm?)
  (struct-copy parse-config config
               [multi-line? mm?]))

(define (config-group-number config)
  (unbox (parse-config-group-number-box config)))

(define (config-group-number+1 config)
  (define b (parse-config-group-number-box config))
  (set-box! b (add1 (unbox b)))
  config)
