;; This module implements "lazy promises" and a `force' that is iterated
;; through them.  Scheme promises are not touched: they're used as values.
;; This is similar to the *new* version of srfi-45 -- see the post-finalization
;; discussion at http://srfi.schemers.org/srfi-45/ for more details;
;; specifically, this version is the `lazy2' version from
;; http://srfi.schemers.org/srfi-45/post-mail-archive/msg00013.html and (a
;; `lazy3' variant of `force' that deals with multiple values is included and
;; commented).  Note: if you use only `force'+`delay' it behaves as in Scheme
;; (except that `force' is identity for non promise values), and `force'+`lazy'
;; are sufficient for implementing the lazy language.
(module promise "mz-without-promises.ss"

  (provide lazy delay force promise?)

  ;; (define-struct promise (p)) <-- use a more sophisticated struct below

  ;; Promise records (note: print in meaningful ways like thunks)
  (define-values (promise promise? p:ref p:set!)
    (let*-values
        ([(printer)
          (lambda (promise port write?)
            (let loop ([p (p:ref promise)])
              (cond
                [(procedure? p)
                 (cond [(object-name p)
                        => (lambda (n) (fprintf port "#<promise:~a>" n))]
                       [else (display "#<promise:?>" port)])]
                ;; no values
                [(null? p) (fprintf port "#<promise!>")]
                [(pair? p)
                 ;; single value
                 (fprintf port (if write? "#<promise!~s" "#<promise!~a")
                          (car p))
                 (when (pair? (cdr p))
                   (let ([fmt (if write? ",~s" ",~a")])
                     (for-each (lambda (x) (fprintf port fmt x)) (cdr p))))
                 (display ">" port)]
                [(promise? p) (loop (p:ref p))] ; hide sharing
                [(not p) (display "#<promise*active>" port)]
                [else (error 'promise-printer "bad promise value: ~e" p)])))]
         [(s:promise promise promise? promise-ref promise-set!)
          (make-struct-type 'promise #f 1 0 #f
            (list (cons prop:custom-write printer)))])
      (values promise
              promise?
              (make-struct-field-accessor promise-ref  0 'contents)
              (make-struct-field-mutator  promise-set! 0 'contents))))

  ;; <promise> ::=
  ;;   | (promise <thunk>)         delayed promise
  ;;   | (promise (list <object>)) forced promise (possibly multi-valued)
  ;;   | (promise <promise>)       shared promise
  ;;   | (promise #f)              currently running
  ;;   | (promise <exn>)           exception when forced (last version)

  ;; Creates a `composable' promise
  ;;   X = (force (lazy X)) = (force (lazy (lazy X))) = (force (lazy^n X))
  (define-syntax (lazy stx)
    (syntax-case stx ()
      [(lazy expr) (with-syntax ([proc (syntax-property
                                        (syntax/loc stx (lambda () expr))
                                        'inferred-name (syntax-local-name))])
                     (syntax/loc stx (promise proc)))]))

  ;; Creates a promise that does not compose
  ;;   X = (force (delay X)) = (force (lazy (delay X)))
  ;;                         = (force (lazy^n (delay X)))
  ;;   X = (force (force (delay (delay X)))) =/= (force (delay (delay X)))
  ;; so each sequence of `(lazy^n o delay)^m' requires m `force's and a
  ;; sequence of `(lazy^n o delay)^m o lazy^k' requires m+1 `force's (for k>0)
  ;; (This is not needed with a lazy language (see the above URL for details),
  ;; but provided for completeness.)
  (define-syntax (delay stx)
    (syntax-case stx ()
      [(delay expr)
       ;; see below for using multiple-values:
       (syntax/loc stx
         (lazy (promise (call-with-values (lambda () expr) list))))]))

  ;; force iterates on lazy promises (forbid dependency cycles)
  ;; * (force X) = X for non promises
  ;; * does not deal with multiple values, since they're not used by the lazy
  ;;   language (but see below)

  #; ; this version cannot handle multiple values
  (define (force promise)
    (if (promise? promise)
      (let loop ([p (p:ref promise)])
        (cond
          [(procedure? p)
           (p:set! promise #f) ; mark root for cycle detection
           (let loop ([promise* (p)])
             (if (promise? promise*)
               (let ([p* (p:ref promise*)])
                 (p:set! promise* promise) ; share with root
                 (cond [(procedure? p*) (loop (p*))]
                       [(pair? p*) (p:set! promise p*) (car p*)]
                       [(promise? p*) (loop p*)]
                       [(not p*) (error 'force "reentrant promise")]
                       [else (error 'force
                                    "invalid promise, contains ~e" p*)]))
               (begin ; error here for "library approach" (see above URL)
                 (p:set! promise (list promise*))
                 promise*)))]
          [(pair? p) (car p)]
          [(promise? p) (loop (p:ref p))]
          [(not p) (error 'force "reentrant promise")]
          [else (error 'force "invalid promise, contains ~e" p)]))
      ;; different from srfi-45: identity for non-promises
      promise))

  #; ; this version works properly with multiple values
  (define (force promise)
    (if (promise? promise)
      (let loop ([p (p:ref promise)])
        (cond
          [(procedure? p)
           (p:set! promise #f) ; mark root for cycle detection
           (let loop1 ([vals* (call-with-values p list)])
             (if (and (pair? vals*)
                      (null? (cdr vals*))
                      (promise? (car vals*)))
               (let loop2 ([promise* (car vals*)])
                 (let ([p* (p:ref promise*)])
                   (p:set! promise* promise) ; share with root
                   (cond [(procedure? p*) (loop1 (call-with-values p* list))]
                         [(or (pair? p*) (null? p*))
                          (p:set! promise p*)
                          (apply values p*)]
                         [(promise? p*) (loop2 p*)]
                         [(not p*) (error 'force "reentrant promise")]
                         [else (error 'force
                                      "invalid promise, contains ~e" p*)])))
               (begin ; error here for "library approach" (see above URL)
                 (p:set! promise vals*)
                 (apply values vals*))))]
          [(or (pair? p) (null? p)) (apply values p)]
          [(promise? p) (loop (p:ref p))]
          [(not p) (error 'force "reentrant promise")]
          [else (error 'force "invalid promise, contains ~e" p)]))
      ;; different from srfi-45: identity for non-promises
      promise))

  ;; this version deals with multiple values only in `delay' (technicality:
  ;; actually it doesn't work with `lazy' holding `lazy' of multiple values, so
  ;; `lazy' works with multiple values unless rewrapped in `lazy'.)
  #;
  (define (force promise)
    (if (promise? promise)
      (let loop ([p (p:ref promise)])
        (cond
          [(procedure? p)
           (p:set! promise #f) ; mark root for cycle detection
           (let ([vals* (call-with-values p list)])
             (if (and (pair? vals*) (null? (cdr vals*)))
               (let loop1 ([val* (car vals*)])
                 (if (promise? val*)
                   (let loop2 ([promise* val*])
                     (let ([p* (p:ref promise*)])
                       (p:set! promise* promise) ; share with root
                       (cond [(procedure? p*) (loop1 (p*))]
                             [(or (pair? p*) (null? p*))
                              (p:set! promise p*)
                              (apply values p*)]
                             [(promise? p*) (loop2 p*)]
                             [(not p*) (error 'force "reentrant promise")]
                             [else (error 'force
                                          "invalid promise, contains ~e" p*)])))
                   (begin ; error here for "library approach" (see above URL)
                     (p:set! promise (list val*))
                     val*)))
               (begin ; error here for "library approach" (see above URL)
                 (p:set! promise vals*)
                 (apply values vals*))))]
          [(or (pair? p) (null? p)) (apply values p)]
          [(promise? p) (loop (p:ref p))]
          [(not p) (error 'force "reentrant promise")]
          [else (error 'force "invalid promise, contains ~e" p)]))
      ;; different from srfi-45: identity for non-promises
      promise))

  ;; this version is like the last one, but properly registers
  ;; exceptions.
  ;; #;
  (define (force promise)
    (if (promise? promise)
      (let loop ([p (p:ref promise)])
        (cond
          [(procedure? p)
           (p:set! promise #f) ; mark root for cycle detection
           (with-handlers*
               ([void (lambda (e)
                        (let ([e (if (exn? e)
                                   e
                                   ;; make sure it's actually an exception
                                   (make-exn (format "~s" e)
                                             (current-continuation-marks)))])
                          (p:set! promise e)
                          (raise e)))])
             (let ([vals* (call-with-values p list)])
               (if (and (pair? vals*) (null? (cdr vals*)))
                 (let loop1 ([val* (car vals*)])
                   (if (promise? val*)
                     (let loop2 ([promise* val*])
                       (let ([p* (p:ref promise*)])
                         (p:set! promise* promise) ; share with root
                         (cond [(procedure? p*) (loop1 (p*))]
                               [(or (pair? p*) (null? p*))
                                (p:set! promise p*)
                                (apply values p*)]
                               [(promise? p*) (loop2 p*)]
                               [(not p*) (error 'force "reentrant promise")]
                               [else (error 'force
                                            "invalid promise, contains ~e"
                                            p*)])))
                     (begin ; error here for "library approach" (see above URL)
                       (p:set! promise (list val*))
                       val*)))
                 (begin ; error here for "library approach" (see above URL)
                   (p:set! promise vals*)
                   (apply values vals*)))))]
          [(or (pair? p) (null? p)) (apply values p)]
          [(promise? p) (loop (p:ref p))]
          [(exn? p) (raise p)]
          [(not p) (error 'force "reentrant promise")]
          [else (error 'force "invalid promise, contains ~e" p)]))
      ;; different from srfi-45: identity for non-promises
      promise))

  #|

  Timing results (#1, #2, #3, #4 are the above versions), in Lazy Scheme:

    loop: (define (foo n) (if (zero? n) n (foo (sub1 n))))
          (time (! (foo 2000000)))
    #1 cpu time: 2067 real time: 2069 gc time: 194
    #2 cpu time: 3057 real time: 3058 gc time: 231
    #3 cpu time: 2566 real time: 2567 gc time: 235
    #4 cpu time: 4744 real time: 4746 gc time: 539

    fib: (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
         (time (! (fib 29)))
    #1 cpu time: 2196 real time: 2196 gc time: 200
    #2 cpu time: 3194 real time: 3195 gc time: 227
    #3 cpu time: 2833 real time: 2833 gc time: 231
    #4 cpu time: 5837 real time: 5837 gc time: 712

  |#

  )
