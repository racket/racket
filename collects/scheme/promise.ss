(module promise '#%kernel
(#%require "private/small-scheme.ss" "private/more-scheme.ss" "private/define.ss"
           (rename "private/define-struct.ss" define-struct define-struct*)
           (for-syntax '#%kernel "private/stxcase-scheme.ss"))
(#%provide lazy delay force promise?)

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
               (fprintf port "#<promise!exn!~a>" (exn-message v))
               (fprintf port (if write? "#<promise!~a>" "#<promise!~s>")
                        `(raise ,v))))]
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

(define-struct promise (val)
  #:mutable
  #:property prop:custom-write promise-printer)
;; A promise value can hold
;; - (list <value> ...): forced promise (possibly multiple-values, usually one)
;; - <promise>: a shared (redirected) promise that points at another one
;; - <thunk>: usually a delayed promise,
;;        - can also hold a `running' thunk that will throw a reentrant error
;;        - can also hold a raising-a-value thunk on exceptions and other
;;          `raise'd values (actually, applicable structs for printouts)

;; Creates a `composable' promise
;;   X = (force (lazy X)) = (force (lazy (lazy X))) = (force (lazy^n X))
(define-syntax (lazy stx)
  (syntax-case stx ()
    [(lazy expr) (with-syntax ([proc (syntax-property
                                      (syntax/loc stx (lambda () expr))
                                      'inferred-name (syntax-local-name))])
                   (syntax/loc stx (make-promise proc)))]))

;; use this to create a value to be raised, make it a procedure so no other
;; code need to change (we could just use the exceptions -- but any value can
;; be raised); also make it have a proper printer so we can show such promises
;; properly.
(define-struct reraise (val)
  #:property prop:procedure (lambda (this) (raise (reraise-val this))))

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
    [(delay expr)
     (syntax/loc stx
       (lazy (make-promise (call-with-values (lambda () expr) list))))]))

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

;; this is used during computation to avoid reentrant loops (which makes it
;; non-r5rs, but there's no good uses for allowing that)
(define (running proc)
  ;; important: be careful not to close over the thunk!
  (let ([name (object-name proc)])
    (if name
      (lambda () (error 'force "reentrant promise ~v" name))
      (lambda () (error 'force "reentrant promise")))))

(define (force promise)
  (if (promise? promise)
    (let loop ([p (promise-val promise)])
      (cond [(procedure? p)
             ;; "mark" root as running (avoids cycles)
             (set-promise-val! promise (running p))
             (call-with-exception-handler
               (lambda (e) (set-promise-val! promise (make-reraise e)) e)
               (lambda () (force-proc p promise)))]
            [(promise? p) (loop (promise-val p))]
            [(null? p) (values)]
            [(null? (cdr p)) (car p)]
            [else (apply values p)]))
    ;; different from srfi-45: identity for non-promises
    promise))

)
