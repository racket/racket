
;; Re-implement `equal?` to support impersonators and chaperones

(define (do-equal? orig-a orig-b mode eql?)
  (let equal? ([orig-a orig-a] [orig-b orig-b] [ctx #f])
    (let loop ([a orig-a] [b orig-b])
      (or (eqv? a b)
          (cond
           [(and (hash-impersonator? a)
                 (hash-impersonator? b)
                 (not (eq? mode 'chaperone-of?)))
            ;; For immutable hashes, it's ok for the two objects to not be eq,
            ;; as long as the interpositions are the same and the underlying
            ;; values are `{impersonator,chaperone}-of?`:
            (if (eq? (hash-impersonator-procs a)
                     (hash-impersonator-procs b))
                (loop (impersonator-next a)
                      (impersonator-next b))
                (loop (impersonator-next a) b))]
           [(and (hash-chaperone? a)
                 (hash-chaperone? b))
            ;; Same as above
            (if (eq? (hash-chaperone-procs a)
                     (hash-chaperone-procs b))
                (loop (impersonator-next a)
                      (impersonator-next b))
                (loop (impersonator-next a) b))]
           [(and (props-impersonator? b)
                 (not (eq? mode 'chaperone-of?)))
            (loop a (impersonator-next b))]
           [(props-chaperone? b)
            (loop a (impersonator-next b))]
           [(and (impersonator? a)
                 (or (not (eq? mode 'chaperone-of?))
                     (chaperone? a)))
            (loop (impersonator-next a) b)]
           [(impersonator? b)
            (cond
             [(eq? mode 'impersonator-of?)
              ;; stop here, unless `prop:impersonator-of` is relevant
              (let ([a2 (extract-impersonator-of mode a)])
                (cond
                 [a2 (or (check-union-find ctx a b)
                         (let ([ctx (deeper-context ctx)])
                           (equal? a2 b ctx)))]
                 [else #f]))]
             [(eq? mode 'chaperone-of?)
              ;; `a` does not include `b`, so give up
              #f]
             [else
              (loop a (impersonator-next b))])]
           [(#%vector? a)
            (and (#%vector? b)
                 (or (not (or (eq? mode 'chaperone-of?) (eq? mode 'equal-always?)))
                     (and (immutable-vector? a)
                          (immutable-vector? b)))
                 (let ([len (#%vector-length a)])
                   (and (fx= len (#%vector-length b))
                        (or
                         (check-union-find ctx a b)
                         (let ([ctx (deeper-context ctx)])
                           (let loop ([i 0])
                             (or (fx= i len)
                                 (and (if eql?
                                          (eql? (vector-ref orig-a i)
                                                (vector-ref orig-b i))
                                          (equal? (vector-ref orig-a i)
                                                  (vector-ref orig-b i)
                                                  ctx))
                                      (loop (fx1+ i))))))))))]
           [(pair? a)
            (and (pair? b)
                 (or (check-union-find ctx a b)
                     (if eql?
                         (and (eql? (car a) (car b))
                              (eql? (cdr a) (cdr b))
                              ;; In case `eql?` doesn't return a boolean:
                              #t)
                         (let ([ctx (deeper-context ctx)])
                           (and
                            (equal? (car a) (car b) ctx)
                            (equal? (cdr a) (cdr b) ctx))))))]
           [(#%box? a)
            (and (#%box? b)
                 (or (not (or (eq? mode 'chaperone-of?) (eq? mode 'equal-always?)))
                     (and (immutable-box? a)
                          (immutable-box? b)))
                 (or (check-union-find ctx a b)
                     (if eql?
                         (and (eql? (unbox orig-a) (unbox orig-b))
                              #t)
                         (let ([ctx (deeper-context ctx)])
                           (equal? (unbox orig-a) (unbox orig-b) ctx)))))]
           [(authentic-hash? a)
            (and (authentic-hash? b)
                 (or (not (or (eq? mode 'chaperone-of?) (eq? mode 'equal-always?)))
                     (and (intmap? a)   ; authentic immutable hash
                          (intmap? b)))
                 (or (check-union-find ctx a b)
                     (hash=? orig-a orig-b
                             (or eql?
                                 (let ([ctx (deeper-context ctx)])
                                   (lambda (a b)
                                     (equal? a b ctx)))))))]
           [(stencil-vector? a)
            (and (stencil-vector? b)
                 (fx= (stencil-vector-mask a) (stencil-vector-mask b))
                 (let ([len (stencil-vector-length a)]
                       [ctx (deeper-context ctx)])
                   (let loop ([i 0])
                     (or (fx= i len)
                         (and (equal? (stencil-vector-ref a i) (stencil-vector-ref b i) ctx)
                              (loop (fx+ i 1)))))))]
           [(mpair? a)
            (and (mpair? b)
                 (not (or (eq? mode 'chaperone-of?) (eq? mode 'equal-always?)))
                 (or (check-union-find ctx a b)
                     (if eql?
                         (and (eql? (mcar a) (mcar b))
                              (eql? (mcdr a) (mcdr b))
                              #t)
                         (let ([ctx (deeper-context ctx)])
                           (and
                            (equal? (mcar a) (mcar b) ctx)
                            (equal? (mcdr a) (mcdr b) ctx))))))]
           [(record? a)
            (and (record? b)
                 ;; Check for `prop:impersonator-of`
                 (let ([a2 (and (not (eq? mode 'chaperone-of?))
                                (extract-impersonator-of mode a))]
                       [b2 (and (or (eq? mode 'equal?) (eq? mode 'equal-always?))
                                (extract-impersonator-of mode b))])
                   (cond
                    [(or a2 b2)
                     ;; `prop:impersonator-of` takes precedence over
                     ;; other forms of checking
                     (or (check-union-find ctx a b)
                         (let ([ctx (deeper-context ctx)])
                           (equal? (or a2 a) (or b2 b) ctx)))]
                    [(and (not (or (eq? mode 'equal?) (eq? mode 'equal-always?)))
                          (extract-impersonator-of mode b))
                     ;; Second argument is an impersonator, so
                     ;; `impersonator-of?` or `chaperone-of?` fails
                     #f]
                    [else
                     ;; No `prop:impersonator-of`, so check for
                     ;; `prop:equal+hash` or transparency
                     (let ([eq+hash (struct-common-equal+hash a b)])
                       (and eq+hash
                            (or (check-union-find ctx a b)
                                (let ([new-api? (equal+hash-supports-mode? eq+hash)])
                                  (cond
                                    [(and (or (eq? mode 'chaperone-of?) (eq? mode 'equal-always?))
                                          (struct-type-mutable? (record-rtd a))
                                          ;; With the old API, mutable records must be `eq?` for `chaperone-of?`
                                          ;; and `equal-always?`
                                          (not new-api?))
                                     #f]
                                    [else
                                     (let ([eql? (if eql?
                                                     (lambda (a b)
                                                       ;; Make sure record sees only booleans:
                                                       (and (eql? a b) #t))
                                                     (let ([ctx (deeper-context ctx)])
                                                       (lambda (a b)
                                                         (equal? a b ctx))))]
                                           [rec-equal? (equal+hash-equal-proc eq+hash)])
                                       (if new-api?
                                           (|#%app| rec-equal? orig-a orig-b eql? (or (eq? mode 'equal?)
                                                                                      (eq? mode 'impersonator-of?)))
                                           (|#%app| rec-equal? orig-a orig-b eql?)))])))))])))]
           [(and (or (eq? mode 'chaperone-of?) (eq? mode 'equal-always?))
                 ;; Mutable strings and bytevectors must be `eq?` for `chaperone-of?` and `equal-always?`
                 (or (mutable-string? a)
                     (mutable-string? b)
                     (mutable-bytevector? a)
                     (mutable-bytevector? b)))
            #f]
           [else
            (#%equal? a b)])))))

(define (equal? a b) (do-equal? a b 'equal? #f))
(define (impersonator-of? a b) (do-equal? a b 'impersonator-of? #f))
(define (chaperone-of? a b) (do-equal? a b 'chaperone-of? #f))
(define (equal-always? a b) (do-equal? a b 'equal-always? #f))

(define/who (equal?/recur a b eql?)
  (check who (procedure-arity-includes/c 2) eql?)
  (do-equal? a b 'equal? eql?))

(define/who (equal-always?/recur a b eql?)
  (check who (procedure-arity-includes/c 2) eql?)
  (do-equal? a b 'equal-always? eql?))

(define (struct-common-equal+hash a b)
  (let ([av (struct-property-ref prop:equal+hash (#%$record-type-descriptor a) #f)])
    (and av
         (let ([bv (struct-property-ref prop:equal+hash (#%$record-type-descriptor b) #f)])
           (and (eq? av bv)
                av)))))

;; ----------------------------------------

;; Use a hash table to detect cycles and sharing,
;; but only start using it if a comparison goes
;; deep enough.

(define (deeper-context ctx)
  (cond
   [ctx
    (let ([v (#%unbox ctx)])
      (when (fixnum? v)
        (if (fx= v 0)
            (#%set-box! ctx (make-eq-hashtable))
            (#%set-box! ctx (fx1- v)))))
    ctx]
   [else (box 32)]))

(define (check-union-find ctx a b)
  (cond
   [(and ctx
         (hashtable? (#%unbox ctx)))
    (let ([ht (#%unbox ctx)])
      (let ([av (union-find ht a)]
            [bv (union-find ht b)])
        (or (eq? av bv)
            (begin
              (hashtable-set! ht av bv)
              #f))))]
   [else #f]))

(define (union-find ht a)
  (let ([av (let loop ([a a])
              (let ([next-a (hashtable-ref ht a #f)])
                (if next-a
                    (loop next-a)
                    a)))])
    (unless (eq? av a)
      (let loop ([a a])
        (let ([next-a (hashtable-ref ht a #f)])
          (unless (eq? next-a av)
            (hashtable-set! ht a next-a)
            (loop next-a)))))
    av))

;; ----------------------------------------

;; The `key-equal-hash-code` and `key-equal?` functions allow
;; interposition on key equality through a hash table impersonator.
;; They call `equal-hash-code` or `equal?` unless the current
;; continuation maps `key-equality-wrap-key` to a key-wrapping
;; function.

(define key-equality-wrap-key '#{equality n1kcvqw4c9hh8t3fi3659ci94-0})

;; Looking in the continaution is expensive relative to `equal?`, so
;; look in a box as a quick pre-test. Multiple threads may increment
;; the counter in the box, so that's why it's only a pre-test.
(define key-equality-maybe-redirect (box 0))

(define (key-equal-hash-code k)
  (let ([get-k (and (fx> (unbox key-equality-maybe-redirect) 0)
                    (continuation-mark-set-first #f key-equality-wrap-key))])
    (if get-k
        (with-continuation-mark key-equality-wrap-key #f
          (equal-hash-code (get-k k)))
        (equal-hash-code k))))

(define (key-equal? k1 k2)
  (let ([get-k (and (fx> (unbox key-equality-maybe-redirect) 0)
                    (continuation-mark-set-first #f key-equality-wrap-key))])
    (if get-k
        (with-continuation-mark key-equality-wrap-key #f
          (equal? (get-k k1) (get-k k2)))
        (equal? k1 k2))))

(define (key-equal-always-hash-code k)
  (let ([get-k (and (fx> (unbox key-equality-maybe-redirect) 0)
                    (continuation-mark-set-first #f key-equality-wrap-key))])
    (if get-k
        (with-continuation-mark key-equality-wrap-key #f
          (equal-always-hash-code (get-k k)))
        (equal-always-hash-code k))))

(define (key-equal-always? k1 k2)
  (let ([get-k (and (fx> (unbox key-equality-maybe-redirect) 0)
                    (continuation-mark-set-first #f key-equality-wrap-key))])
    (if get-k
        (with-continuation-mark key-equality-wrap-key #f
          (equal-always? (get-k k1) (get-k k2)))
        (equal-always? k1 k2))))

(define (call-with-equality-wrap get-k key thunk)
  (unsafe-box*-cas+! key-equality-maybe-redirect 1)
  (let ([get-k
         (if (eq? key none)
             get-k
             ;; record `(get-k key)` so that we
             ;; don't have to compute it multiple
             ;; times:
             (let ([got-k (get-k key)])
               (lambda (k2)
                 (if (eq? k2 key)
                     got-k
                     (get-k k2)))))])
    (let ([r (with-continuation-mark key-equality-wrap-key get-k
               (thunk))])
      (unsafe-box*-cas+! key-equality-maybe-redirect -1)
      r)))
