(module promise '#%kernel
(#%require "private/small-scheme.ss" "private/more-scheme.ss" "private/define.ss"
           (rename "private/define-struct.ss" define-struct define-struct*)
           (for-syntax '#%kernel "private/stxcase-scheme.ss")
           '#%unsafe)
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

;; unsafe accessors
(define-syntax pref  (syntax-rules () [(_ p) (unsafe-struct-ref p 0)]))
(define-syntax pset! (syntax-rules () [(_ p x) (unsafe-struct-set! p 0 x)]))

(define (promise-printer promise port write?)
  (let loop ([p (pref promise)])
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
          [(promise? p) (loop (pref p))] ; hide sharing
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
;; - (list <value> ...): forced promise (possibly multiple-values)
;;        - composable promises deal with only one value
;; - <promise>: a shared (redirected) promise that points at another one
;;        - possible only with composable promises
;; - <thunk>: usually a delayed promise,
;;        - can also hold a `running' thunk that will throw a reentrant error
;;        - can also hold a raising-a-value thunk on exceptions and other
;;          `raise'd values (actually, applicable structs for printouts)
;; First, a generic struct, which is used for all promise-like values
(define-struct promise ([val #:mutable])
  #:property prop:custom-write promise-printer)
;; Then, a subtype for composable promises
(define-struct (composable-promise promise) ())

;; template for all delay-like constructs
(define-for-syntax (make-delayer stx maker)
  (syntax-case stx ()
    [(_ expr)
     (with-syntax ([proc (syntax-property (syntax/loc stx (lambda () expr))
                                          'inferred-name (syntax-local-name))]
                   [make maker])
       (syntax/loc stx (make proc)))]))

;; Creates a composable promise
;;   X = (force (lazy X)) = (force (lazy (lazy X))) = (force (lazy^n X))
(define-syntax (lazy stx) (make-delayer stx #'make-composable-promise))

;; Creates a (generic) promise that does not compose
;;   X = (force (delay X)) = (force (lazy (delay X)))
;;                         = (force (lazy^n (delay X)))
;;   X = (force (force (delay (delay X)))) != (force (delay (delay X)))
;; so each sequence of `(lazy^n o delay)^m' requires m `force's and a
;; sequence of `(lazy^n o delay)^m o lazy^k' requires m+1 `force's (for k>0)
;; (This is not needed with a lazy language (see the above URL for details),
;; but provided for regular delay/force uses.)
(define-syntax (delay stx) (make-delayer stx #'make-promise))

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

;; force/composable iterates on composable promises
;; * (force X) = X for non promises
;; * does not deal with multiple values in the composable case
(define (force/composable root)
  (let ([p (pref root)])
    (cond
      [(procedure? p)
       ;; mark the root as running: avoids cycles, and no need to keep banging
       ;; the root promise value; it makes this non-r5rs, but the only
       ;; practical uses of these things could be ones that use state to avoid
       ;; an infinite loop.  (See the generic forcer below.)
       ;; (careful: avoid holding a reference to the thunk, to allow
       ;; safe-for-space loops)
       (pset! root (make-running (object-name p)))
       (call-with-exception-handler
        (lambda (e) (pset! root (make-reraise e)) e)
        (lambda ()
          ;; iterate carefully through chains of composable promises
          (let loop ([v (p)]) ; does not handle multiple values!
            (cond [(composable-promise? v)
                   (let ([p* (pref v)])
                     (pset! v root) ; share with root
                     (cond [(procedure? p*) (loop (p*))]
                           ;; it must be a list of one value (because
                           ;; composable promises never hold multiple values),
                           ;; or a composable promise
                           [(pair? p*) (pset! root p*) (unsafe-car p*)]
                           ;; note: for the promise case we could jump only to
                           ;; the last `let' (for `p*'), but that makes the
                           ;; code heavier, and runs slower (probably goes over
                           ;; some inlining/unfolding threshold).
                           [else (loop p*)]))]
                  ;; reached a non-composable promise: share and force it now
                  [(promise? v) (pset! root v) (force/generic v)]
                  ;; error here for "library approach" (see above URL)
                  [else (pset! root (list v)) v]))))]
      ;; try to make the order efficient, with common cases first
      [(pair? p) (if (null? (unsafe-cdr p)) (unsafe-car p) (apply values p))]
      ;; follow all sharings (and shortcut directly to the right force)
      [(composable-promise? p) (force/composable p) (force/generic p)]
      [(null? p) (values)]
      [else (error 'force "composable promise with invalid contents: ~e" p)])))

;; generic force for "old-style" promises -- they're still useful in
;; that they allow multiple values.  In general, this is slower, but has
;; more features.  (They could allow self loops, but this means holding
;; on to the procedure and its resources while it is running, and lose
;; the ability to know that it is running; the second can be resolved
;; with a new kind of `running' value that can be used again, but the
;; first cannot be solved.  I still didn't ever see any use for them, so
;; they're still forbidden.)
(define (force/generic promise)
  (let ([p (pref promise)])
    (cond
      [(procedure? p)
       (pset! promise (make-running (object-name p)))
       (call-with-exception-handler
        (lambda (e) (pset! promise (make-reraise e)) e)
        (lambda ()
          (let ([vs (call-with-values p list)])
            (pset! promise vs)
            (cond [(null? vs) (values)]
                  [(null? (unsafe-cdr vs)) (unsafe-car vs)]
                  [else (apply values vs)]))))]
      ;; try to make the order efficient, with common cases first
      [(pair? p) (if (null? (unsafe-cdr p)) (unsafe-car p) (apply values p))]
      [(null? p) (values)]
      [else (error 'force "generic promise with invalid contents: ~e" p)])))

;; dispatcher for composable promises, generic promises, and other values
(define (force promise)
  (cond [(composable-promise? promise) (force/composable promise)]
        [(promise? promise) (force/generic promise)]
        ;; different from srfi-45: identity for non-promises
        [else promise]))

(define (promise-forced? promise)
  (if (promise? promise)
    (let ([p (pref promise)])
      (or (not (procedure? p)) (reraise? p))) ; #f when running
    (raise-type-error 'promise-forced? "promise" promise)))

(define (promise-running? promise)
  (if (promise? promise)
    (running? (pref promise))
    (raise-type-error 'promise-running? "promise" promise)))

)

#|
Simple code for timings:
  (define (c n) (lazy (if (zero? n) (delay 'hey!) (c (sub1 n)))))
  (for ([i (in-range 9)])
    (collect-garbage) (collect-garbage) (collect-garbage)
    (time (for ([i (in-range 10000)]) (force (c 2000)))))
Also, run (force (c -1)) and check constant space
|#
