(module promise '#%kernel
(#%require "private/small-scheme.ss" "private/more-scheme.ss" "private/define.ss"
           (rename "private/define-struct.ss" define-struct define-struct*)
           (for-syntax '#%kernel "private/stxcase-scheme.ss"))
(#%provide lazy delay force promise? promise-forced? promise-running?)

;; This module implements "lazy" (composable) promises and a `force'
;; that is iterated through them.

;; This is similar to the *new* version of srfi-45 -- see the
;; post-finalization discussion at http://srfi.schemers.org/srfi-45/ for
;; more details; specifically, this version is the `lazy2' version from
;; http://srfi.schemers.org/srfi-45/post-mail-archive/msg00013.html.
;; Note: if you use only `force'+`delay' it behaves as in Scheme (except
;; that `force' is identity for non promise values), and `force'+`lazy'
;; are sufficient for implementing the lazy language.

(define (promise-printer promise port write?)
  (let loop ([p (promise-val promise)])
    (cond [(reraise? p)
           (let ([v (reraise-val p)])
             (if (exn? v)
               (fprintf port (if write? "#<promise!exn!~s>" "#<promise!exn!~a>")
                        (exn-message v))
               (fprintf port (if write? "#<promise!~s>" "#<promise!~a>")
                        `(raise ,v))))]
          [(running? p)
           (let ([n (running-name p)])
             (if n
               (fprintf port "#<promise:!running!~a>" n)
               (fprintf port "#<promise:!running>")))]
          [(procedure? p)
           (cond [(object-name p)
                  => (lambda (n) (fprintf port "#<promise:~a>" n))]
                 [else (display "#<promise>" port)])]
          [(promise? p) (loop (promise-val p))] ; hide sharing
          ;; values
          [(null? p) (fprintf port "#<promise!(values)>")]
          [(null? (cdr p))
           (fprintf port (if write? "#<promise!~s>" "#<promise!~a>") (car p))]
          [else
           (display "#<promise!(values" port)
           (let ([fmt (if write? " ~s" " ~a")])
             (for-each (lambda (x) (fprintf port fmt x)) p))
           (display ")>" port)])))

;; A promise value can hold
;; - (list <value> ...): forced promise (possibly multiple-values, usually one)
;; - <promise>: a shared (redirected) promise that points at another one
;; - <thunk>: usually a delayed promise,
;;        - can also hold a `running' thunk that will throw a reentrant error
;;        - can also hold a raising-a-value thunk on exceptions and other
;;          `raise'd values (actually, applicable structs for printouts)
(define-struct promise ([val #:mutable])
  #:property prop:custom-write promise-printer)

;; Creates a `composable' promise
;;   X = (force (lazy X)) = (force (lazy (lazy X))) = (force (lazy^n X))
(define-syntax (lazy stx)
  (syntax-case stx ()
    [(_ expr)
     (with-syntax ([proc (syntax-property (syntax/loc stx (lambda () expr))
                                          'inferred-name (syntax-local-name))])
       (syntax/loc stx (make-promise proc)))]))

;; Creates a promise that does not compose
;;   X = (force (delay X)) = (force (lazy (delay X)))
;;                         = (force (lazy^n (delay X)))
;;   X = (force (force (delay (delay X)))) != (force (delay (delay X)))
;; so each sequence of `(lazy^n o delay)^m' requires m `force's and a
;; sequence of `(lazy^n o delay)^m o lazy^k' requires m+1 `force's (for k>0)
;; (This is not needed with a lazy language (see the above URL for details),
;; but provided for regular delay/force uses.)
(define-syntax (delay stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx
       (lazy (make-promise (call-with-values (lambda () expr) list))))]))

;; For simplicity and efficiency this code uses thunks in promise values for
;; exceptions: this way, we don't need to tag exception values in some special
;; way and test for them -- we just use a thunk that will raise the exception.
;; But it's still useful to refer to the exception value, so use an applicable
;; struct for them.  The same goes for a promise that is being forced: we use a
;; thunk that will throw a "reentrant promise" error -- and use an applicable
;; struct so it is identifiable.
(define-struct reraise (val)
  #:property prop:procedure (lambda (this) (raise (reraise-val this))))
(define-struct running (name)
  #:property prop:procedure (lambda (this)
                              (let ([name (running-name this)])
                                (if name
                                  (error 'force "reentrant promise ~v" name)
                                  (error 'force "reentrant promise")))))

;; force iterates on lazy promises (forbids dependency cycles)
;; * (force X) = X for non promises
;; * does not deal with multiple values, except for `delay' promises at the
;;   leaves

(define (force-proc p root)
  (let loop1 ([v (p)]) ; does not handle multiple values!
    (if (promise? v)
      (let loop2 ([promise* v])
        (let ([p* (promise-val promise*)])
          (set-promise-val! promise* root) ; share with root
          (cond [(procedure? p*) (loop1 (p*))]
                [(promise? p*) (loop2 p*)]
                [else (set-promise-val! root p*)
                      (cond [(null? p*) (values)]
                            [(null? (cdr p*)) (car p*)]
                            [else (apply values p*)])])))
      (begin ; error here for "library approach" (see above URL)
        (set-promise-val! root (list v))
        v))))

(define (force promise)
  (if (promise? promise)
    (let loop ([p (promise-val promise)])
      (cond [(procedure? p)
             ;; mark the root as running: avoids cycles, and no need to keep
             ;; banging the root promise value; it makes this non-r5rs, but
             ;; the only practical uses of these things could be ones that use
             ;; state to avoid an infinite loop.
             ;; (careful: avoid holding a reference to the thunk, to allow
             ;; safe-for-space loops)
             (set-promise-val! promise (make-running (object-name p)))
             (call-with-exception-handler
               (lambda (e) (set-promise-val! promise (make-reraise e)) e)
               (lambda () (force-proc p promise)))]
            [(promise? p) (loop (promise-val p))]
            [(null? p) (values)]
            [(null? (cdr p)) (car p)]
            [else (apply values p)]))
    ;; different from srfi-45: identity for non-promises
    promise))

(define (promise-forced? promise)
  (if (promise? promise)
    (let ([p (promise-val promise)])
      (or (not (procedure? p)) (reraise? p))) ; #f when running
    (raise-type-error 'promise-forced? "promise" promise)))

(define (promise-running? promise)
  (if (promise? promise)
    (running? (promise-val promise))
    (raise-type-error 'promise-running? "promise" promise)))

)
