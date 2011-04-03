#lang racket/base
(require (prefix-in pl- '#%place)
         '#%boot
         (only-in '#%paramz parameterization-key make-custodian-from-main)
         (only-in '#%futures processor-count)
         '#%place-struct
         racket/fixnum
         racket/flonum
         racket/vector)

(provide place
         place-sleep
         place-wait 
         place-kill
         place-channel
         place-channel-send
         place-channel-receive
         place-channel?
         place?
         place-channel-send/receive
         processor-count
         (rename-out [pl-place-enabled? place-enabled?]))

(define-struct TH-place (th ch) #:property prop:evt (lambda (x) (TH-place-channel-out (TH-place-ch x))))

(define (place-channel-send/receive ch msg)
  (place-channel-send ch msg)
  (place-channel-receive ch))

(define (make-th-async-channel)
  (define ch (make-channel))
  (values 
    (thread 
      (lambda ()
        (let loop ()
           (channel-put ch (thread-receive))
           (loop))))
    ch))
  
(define (th-place mod funcname)
  (unless (or (path-string? mod) (resolved-module-path? mod))
    (raise-type-error 'place "resolved-module-path? or path-string?" 0 mod funcname))
  (unless (symbol? funcname)
    (raise-type-error 'place "symbol?" 1 mod funcname))
  (define-values (pch cch) (th-place-channel))
  (define th (thread (lambda ()
    (with-continuation-mark
      parameterization-key
      orig-paramz
      (parameterize ([current-namespace (make-base-namespace)]
                     [current-custodian (make-custodian-from-main)])
        ((dynamic-require mod funcname) cch))))))
  (TH-place th pch))

(define (th-place-sleep n) (sleep n))
(define (th-place-wait pl) (thread-wait (TH-place-th pl)) 0)
(define (th-place-kill pl) (kill-thread (TH-place-th pl)))
(define (th-place-channel)
  (define-values (as ar) (make-th-async-channel))
  (define-values (bs br) (make-th-async-channel))
  (define pch (TH-place-channel ar bs))
  (define cch (TH-place-channel br as))
  (values pch cch))
 
(define (deep-copy x)
  (define (dcw o)
    (cond 
      [(ormap (lambda (x) (x o)) (list number? char? boolean? null? void? string? symbol? TH-place-channel?)) o]
      [(cond
        [(path? o) (path->bytes o)]
        [(bytes? o) (if (pl-place-shared? o) o (bytes-copy o))]
        [(fxvector? o) (if (pl-place-shared? o) o (fxvector-copy o))]
        [(flvector? o) (if (pl-place-shared? o) o (flvector-copy o))]
        [else #f])
        => values]
      [(TH-place? o) (dcw (TH-place-ch o))]
      [(pair? o) (cons (dcw (car o)) (dcw (cdr o)))]
      [(vector? o) (vector-map! dcw (vector-copy o))]
      [(struct? o)
        (define key (prefab-struct-key o))
        (when (not key)
          (error "Must be a prefab struct"))
        (apply make-prefab-struct 
               key 
               (map dcw (cdr (vector->list (struct->vector o)))))]
      [else (error "Error not place serializable ~a" o)]))

  (dcw x))


(define (th-place-channel-send pl msg)
  (define th  
    (cond
      [(TH-place? pl) (TH-place-channel-out (TH-place-ch pl))]
      [(TH-place-channel? pl) (TH-place-channel-out pl)]
      [else (raise-type-error 'place-channel-send "expect a place? or place-channel?" pl)]))
  (sync (thread-resume-evt th))
  (thread-send th
    (deep-copy msg)))

(define (th-place-channel-receive pl)
  (channel-get
    (cond
      [(TH-place? pl) (TH-place-channel-in (TH-place-ch pl))]
      [(TH-place-channel? pl) (TH-place-channel-in pl)]
      [else (raise-type-error 'place-channel-receive "expect a place? or place-channel?" pl)])))

(define (th-place-channel? pl)
  (or (TH-place? pl)
      (TH-place-channel? pl)))

(define-syntax-rule (define-pl x p t) (define x (if (pl-place-enabled?) p t)))

(define-pl place              pl-place              th-place)
(define-pl place-sleep        pl-place-sleep        th-place-sleep)
(define-pl place-wait         pl-place-wait         th-place-wait)
(define-pl place-kill         pl-place-kill         th-place-kill)
(define-pl place-channel      pl-place-channel      th-place-channel)
(define-pl place-channel-send pl-place-channel-send th-place-channel-send)
(define-pl place-channel-receive pl-place-channel-receive th-place-channel-receive)
(define-pl place-channel?     pl-place-channel?     th-place-channel?)
(define-pl place?             pl-place?             TH-place?)
