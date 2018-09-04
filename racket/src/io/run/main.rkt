#lang racket/base
(require "../host/place-local.rkt"
         "../common/check.rkt"
         "../print/main.rkt"
         "../error/main.rkt"
         "../port/parameter.rkt"
         "../port/handler.rkt")

(provide executable-yield-handler
         current-command-line-arguments
         current-print
         current-read-interaction
         current-prompt-read
         current-get-interaction-input-port
         cache-configuration)

(define/who executable-yield-handler
  (make-parameter void (lambda (p)
                         (check who (procedure-arity-includes/c 1) p)
                         p)))

(define/who current-command-line-arguments
  (make-parameter '#() (lambda (v)
                         (define l (and (vector? v)
                                        (vector->list v)))
                         (unless (and (vector? v)
                                      (andmap string? l))
                           (raise-argument-error who "(vectorof string?)" l))
                         (list->vector (map string->immutable-string l)))))

(define/who current-print
  (make-parameter (lambda (v)
                    (unless (void? v)
                      (print v)
                      (newline)))
                  (lambda (p)
                    (check who (procedure-arity-includes/c 1) p)
                    p)))

(define/who current-read-interaction
  (make-parameter (lambda (src in)
                    (parameterize ([installed-read-accept-reader #t]
                                   [installed-read-accept-lang #f])
                      (installed-read-syntax src in)))
                  (lambda (p)
                    (check who (procedure-arity-includes/c 2) p)
                    p)))

(define/who current-prompt-read
  (make-parameter (lambda ()
                    (display "> ")
                    (let ([in ((current-get-interaction-input-port))])
                      ((current-read-interaction) (object-name in) in)))
                  (lambda (p)
                    (check who (procedure-arity-includes/c 0) p)
                    p)))

(define/who current-get-interaction-input-port
  (make-parameter (lambda () (current-input-port))
                  (lambda (p)
                    (check who (procedure-arity-includes/c 0) p)
                    p)))

;; ----------------------------------------

(define-place-local cached-values #hasheq())
(define (cache-configuration index thunk)
  (hash-ref cached-values index
            (lambda ()
              (let ([v (thunk)])
                (set! cached-values (hash-set cached-values index v))
                v))))
