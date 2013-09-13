#lang racket/base
(require (prefix-in pl- '#%place)
         '#%boot
         (only-in '#%paramz parameterization-key make-custodian-from-main)
         '#%place-struct
         racket/fixnum
         racket/flonum
         racket/vector)

(provide th-dynamic-place
         ;th-dynamic-place*
         th-place-sleep
         th-place-wait
         th-place-kill
         th-place-break
         th-place-channel
         th-place-channel-put
         th-place-channel-get
         th-place-channel?
         th-place
         th-place?
         th-place-message-allowed?
         th-place-dead-evt
         )


(define-struct TH-place (th ch cust)
  #:property prop:evt (lambda (x) (TH-place-channel-in (TH-place-ch x))))
(define th-place? TH-place?)
(define th-place TH-place)

(define (make-th-async-channel)
  (define ch (make-channel))
  (values
    (thread
      (lambda ()
        (let loop ()
          (let ([v (thread-receive)])
            (channel-put ch v)
            (loop)))))
    ch))

(define (th-dynamic-place mod funcname)
  (unless (or (module-path? mod) (path? mod) (resolved-module-path? mod))
    (raise-argument-error 'dynamic-place "(or/c module-path? path? resolved-module-path?)" 0 mod funcname))
  (unless (symbol? funcname)
    (raise-argument-error 'dynamic-place "symbol?" 1 mod funcname))
  (define-values (pch cch) (th-place-channel))
  (define cust (make-custodian-from-main))
  (define th (thread
              (lambda ()
                (with-continuation-mark
                    parameterization-key
                    orig-paramz
                  (parameterize ([current-namespace (make-base-namespace)]
                                 [current-custodian cust])
                    ((dynamic-require mod funcname) cch))))))
  (TH-place th pch cust))

(define (th-place-sleep n) (sleep n))
(define (th-place-wait pl) (thread-wait (TH-place-th pl)) 0)
(define (th-place-kill pl) (custodian-shutdown-all (TH-place-cust pl)))
(define (th-place-break pl kind) (break-thread (TH-place-th pl) kind))
(define (th-place-dead-evt pl) (thread-dead-evt (TH-place-th pl)))
(define (th-place-channel)
  (define-values (as ar) (make-th-async-channel))
  (define-values (bs br) (make-th-async-channel))
  (define pch (TH-place-channel ar bs))
  (define cch (TH-place-channel br as))
  (values pch cch))

(define (deep-copy x)
  (define ht (make-hasheq))
  (define (record v new-v)
    (hash-set! ht v new-v)
    new-v)
  (define (with-placeholder o mk)
    (define ph (make-placeholder #f))
    (hash-set! ht o ph)
    (define new-o (mk))
    (placeholder-set! ph new-o)
    new-o)
  (define (dcw o)
    (cond
      [(ormap (lambda (x) (x o)) (list number? char? boolean? null? void? string? symbol? TH-place-channel?)) o]
      [(hash-ref ht o #f)
       => values]
      [(cond
        [(path-for-some-system? o) o]
        [(bytes? o) (if (pl-place-shared? o) o (record o (bytes-copy o)))]
        [(fxvector? o) (if (pl-place-shared? o) o (record o (fxvector-copy o)))]
        [(flvector? o) (if (pl-place-shared? o) o (record o (flvector-copy o)))]
        [else #f])
        => values]
      [(TH-place? o) (dcw (TH-place-ch o))]
      [(pair? o) 
       (with-placeholder
        o
        (lambda ()
          (cons (dcw (car o)) (dcw (cdr o)))))]
      [(vector? o) 
       (vector-map! dcw (record o (vector-copy o)))]
      [(hash? o) 
       (with-placeholder
        o
        (lambda ()
          (cond
           [(hash-equal? o)
            (for/fold ([nh (hash)]) ([p (in-hash-pairs o)])
              (hash-set nh (dcw (car p)) (dcw (cdr p))))]
           [(hash-eq? o)
            (for/fold ([nh (hasheq)]) ([p (in-hash-pairs o)])
              (hash-set nh (dcw (car p)) (dcw (cdr p))))]
           [else ; (hash-eqv? o)
            (for/fold ([nh (hasheqv)]) ([p (in-hash-pairs o)])
              (hash-set nh (dcw (car p)) (dcw (cdr p))))])))]
      [(and (struct? o)
            (prefab-struct-key o))
       =>
       (lambda (key)
         (with-placeholder
          o
          (lambda ()
            (apply make-prefab-struct
                   key
                   (map dcw (cdr (vector->list (struct->vector o))))))))]
      [else (raise-mismatch-error 'place-channel-put "cannot transmit a message containing value: " o)]))

  (make-reader-graph (dcw x)))


(define (th-place-channel-put pl msg)
  (define th
    (cond
      [(TH-place? pl) (TH-place-channel-out (TH-place-ch pl))]
      [(TH-place-channel? pl) (TH-place-channel-out pl)]
      [else (raise-argument-error 'place-channel-put "(or/c place? place-channel?)" pl)]))
  (void (thread-send th (deep-copy msg) #f)))

(define (th-place-channel-get pl)
  (channel-get
    (cond
      [(TH-place? pl) (TH-place-channel-in (TH-place-ch pl))]
      [(TH-place-channel? pl) (TH-place-channel-in pl)]
      [else (raise-argument-error 'place-channel-get "(or/c place? place-channel?)" pl)])))

(define (th-place-channel? pl)
  (or (TH-place? pl)
      (TH-place-channel? pl)))

(define (th-place-message-allowed? x)
  (define (dcw o)
    (cond
      [(ormap (lambda (x) (x o)) (list number? char? boolean? null? void? string? symbol? TH-place-channel?
                                       path? bytes? fxvector? flvector? TH-place?)) #t]
      [(pair? o) (and (dcw (car o)) (dcw (cdr o)))]
      [(vector? o) 
       (for/fold ([nh #t]) ([i (in-vector o)])
        (and nh (dcw i)))]
      [(hash? o)
       (for/fold ([nh #t]) ([p (in-hash-pairs o)])
         (and nh (dcw (car p)) (dcw (cdr p))))]
      [(struct? o)
        (define key (prefab-struct-key o))
        (when (not key)
          (error "Must be a prefab struct"))
        (for/fold ([nh #t]) ([p (cdr (vector->list (struct->vector o)))])
          (and nh (dcw p)))]
      [else (raise-mismatch-error 'place-channel-put "cannot transmit a message containing value: " o)]))

  (dcw x)
  #t)
