#lang racket/base
(require racket/contract
         racket/pretty
         racket/serialize
         racket/sandbox
         racket/file
         scribble/eval)
(provide/contract
 [make-log-based-eval
  (-> path-string? (or/c 'record 'replay) (-> any/c any))])

(define (make-log-based-eval logfile mode)
  (case mode
    ((record) (make-eval/record logfile))
    ((replay) (make-eval/replay logfile))))

(define-namespace-anchor anchor)

(define (make-eval/record logfile)
  (let* ([ev (make-base-eval)]
         [super-cust (current-custodian)]
         [out (parameterize ((current-custodian (get-user-custodian ev)))
                (open-output-file logfile #:exists 'replace))])
    (display ";; This file was created by make-log-based-eval\n" out)
    (flush-output out)
    (call-in-sandbox-context ev
      (lambda ()
        ;; Required for serialization to work.
        (namespace-attach-module (namespace-anchor->namespace anchor) 'racket/serialize)
        (let ([old-eval (current-eval)]
              [init-out-p (current-output-port)]
              [init-err-p (current-error-port)]
              [out-p (open-output-bytes)]
              [err-p (open-output-bytes)])
          (current-eval
           (lambda (x)
             (let* ([x (syntax->datum (datum->syntax #f x))]
                    [x (if (and (pair? x) (eq? (car x) '#%top-interaction)) (cdr x) x)]
                    [result
                     (with-handlers ([exn? values])
                       (call-with-values (lambda ()
                                           (parameterize ((current-eval old-eval)
                                                          (current-custodian (make-custodian))
                                                          (current-output-port out-p)
                                                          (current-error-port err-p))
                                             (begin0 (old-eval x)
                                               (wait-for-threads (current-custodian) super-cust))))
                         list))]
                    [out-s (get-output-bytes out-p #t)]
                    [err-s (get-output-bytes err-p #t)])
               (let ([result* (serialize (cond [(list? result) (cons 'values result)]
                                               [(exn? result) (list 'exn (exn-message result))]))])
                 (pretty-write (list x result* out-s err-s) out)
                 (flush-output out))
               (display out-s init-out-p)
               (display err-s init-err-p)
               (cond [(list? result) (apply values result)]
                     [(exn? result) (raise result)])))))))
    ev))

;; Wait for threads created by evaluation so that the evaluator catches output
;; they generate, etc.
;; FIXME: see what built-in scribble evaluators do
(define (wait-for-threads sub-cust super-cust)
  (let ([give-up-evt (alarm-evt (+ (current-inexact-milliseconds) 200.0))])
    ;; find a thread to wait on
    (define (find-thread cust)
      (let* ([managed (custodian-managed-list cust super-cust)]
             [thds (filter thread? managed)]
             [custs (filter custodian? managed)])
        (cond [(pair? thds) (car thds)]
              [else (ormap find-thread custs)])))
    ;; keep waiting on threads (one at a time) until time to give up
    (define (wait-loop cust)
      (let ([thd (find-thread cust)])
        (when thd
          (cond [(eq? give-up-evt (sync thd give-up-evt)) (void)]
                [else (wait-loop cust)]))))
    (wait-loop sub-cust)))

(define (make-eval/replay logfile)
  (let* ([ev (make-base-eval)]
         [evaluations (file->list logfile)])
    (call-in-sandbox-context ev
      (lambda ()
        (namespace-attach-module (namespace-anchor->namespace anchor) 'racket/serialize)
        (let ([old-eval (current-eval)]
              [init-out-p (current-output-port)]
              [init-err-p (current-error-port)])
          (current-eval
           (lambda (x)
             (let* ([x (syntax->datum (datum->syntax #f x))]
                    [x (if (and (pair? x) (eq? (car x) '#%top-interaction)) (cdr x) x)])
               (unless (and (pair? evaluations) (equal? x (car (car evaluations))))
                 ;; TODO: smarter resync
                 ;;  - can handle *additions* by removing next set!
                 ;;  - can handle *deletions* by searching forward (but may jump to far
                 ;;    if terms occur more than once, eg for stateful code)
                 ;; For now, just fail early and often.
                 (set! evaluations null)
                 (error 'eval "unable to replay evaluation of ~.s" x))
               (let* ([evaluation (car evaluations)]
                      [result (parameterize ((current-eval old-eval))
                                (deserialize (cadr evaluation)))]
                      [result (case (car result)
                                ((values) (cdr result))
                                ((exn) (make-exn (cadr result) (current-continuation-marks))))]
                      [output (caddr evaluation)]
                      [error-output (cadddr evaluation)])
                 (set! evaluations (cdr evaluations))
                 (display output init-out-p #| (current-output-port) |#)
                 (display error-output init-err-p #| (current-error-port) |#)
                 (cond [(exn? result) (raise result)]
                       [(list? result) (apply values result)]))))))))
    ev))
