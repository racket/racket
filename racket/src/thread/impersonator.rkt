#lang racket/base
(require "check.rkt"
         "internal-error.rkt"
         "evt.rkt"
         (submod "evt.rkt" for-chaperone)
         "channel.rkt"
         (submod "channel.rkt" for-impersonator))

(provide chaperone-evt
         chaperone-channel
         impersonate-channel)

;; ----------------------------------------

(define/who (chaperone-evt evt proc . args)
  (check who evt? evt)
  (check proc (procedure-arity-includes/c 1) proc)
  (do-chaperone-evt who "evt" #t evt proc args
                    (lambda (v)
                      (unless (evt? v)
                        (raise-result-error who "evt?" v)))))

(define (do-chaperone-evt who what chaperone? evt proc args check-evt)
  (check-impersonator-properties who args)
  (apply chaperone-struct
         evt
         (cond
           [(primary-evt? evt) primary-evt-ref]
           [(secondary-evt? evt) secondary-evt-ref]
           [else (internal-error "unrecognized evt to impersonate")])
         (lambda (evt v) v)
         impersonator-prop:evt
         (lambda (also-evt)
           (call-with-values (lambda () (proc evt))
             (case-lambda
               [(new-evt wrap)
                (when chaperone?
                  (check-chaperone-of what new-evt evt))
                (check-evt new-evt)
                (unless (and (procedure? wrap)
                             (procedure-arity-includes? wrap 1))
                  (raise-result-error who "(procedure-arity-includes/c 1)" wrap))
                (handle-evt new-evt
                            (lambda rs
                              (call-with-values
                               (lambda () (apply wrap rs))
                               (lambda new-rs
                                 (unless (= (length rs) (length new-rs))
                                   (raise
                                    (exn:fail:contract:arity
                                     (string-append
                                      what " " (if chaperone? "chaperone" "impersonator")
                                      ": result wrapper returned wrong number of values\n"
                                      "  expected count: " (number->string (length rs)) "\n"
                                      "  returned count: " (number->string (length new-rs)))
                                     (current-continuation-marks))))
                                 (when chaperone?
                                   (for ([r (in-list rs)]
                                         [new-r (in-list new-rs)])
                                     (check-chaperone-of what new-r r)))
                                 (apply values new-rs)))))]
               [args
                (raise
                 (exn:fail:contract:arity
                  (string-append
                   what " " (if chaperone? "chaperone" "impersonator") ": returned wrong number of values\n"
                   "  expected count: 2\n"
                   "  returned count: " (number->string (length args)))
                  (current-continuation-marks)))])))
         args))

;; ----------------------------------------

(define/who (chaperone-channel ch get-proc put-proc . args)
  (do-impersonate-channel who #t ch get-proc put-proc args))

(define/who (impersonate-channel ch get-proc put-proc . args)
  (do-impersonate-channel who #f ch get-proc put-proc args))

(define (do-impersonate-channel who chaperone? ch get-proc put-proc args)
  (check who channel? ch)
  (check who (procedure-arity-includes/c 1) get-proc)
  (check who (procedure-arity-includes/c 2) put-proc)  
  (do-chaperone-evt who "channel" chaperone? ch get-proc
                    (list* impersonator-prop:channel-put
                           (cons ch (lambda (ch v)
                                      (define new-v (put-proc ch v))
                                      (when chaperone?
                                        (check-chaperone-of "channel" new-v v))
                                      new-v))
                           args)
                    (lambda (v)
                      (unless (channel? v)
                        (raise-result-error who "channel?" v)))))

;; ----------------------------------------

(define (check-chaperone-of what new-r r)
  (unless (chaperone-of? new-r r)
    (raise
     (exn:fail:contract
      (string-append
       what " chaperone: non-chaperone result;\n"
       " received a value that is not a chaperone of the original value\n"
       "  value: " ((error-value->string-handler) r) "\n"
       "  non-chaperone value: "
       ((error-value->string-handler) new-r))
      (current-continuation-marks)))))

(define (check-impersonator-properties who args)
  (let loop ([args args])
    (unless (null? args)
      (check who impersonator-property? (car args))
      (cond
        [(null? args)
         (raise-arguments-error who
                                "missing an argument after an impersonator-property argument"
                                "impersonator property" (car args))]
        [else
         (loop (cddr args))]))))
