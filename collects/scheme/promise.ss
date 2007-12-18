#lang scheme/base

;;

;; This module implements "lazy promises" and a `force' that is iterated
;; through them.
;; This is similar to the *new* version of srfi-45 -- see the post-finalization
;; discussion at http://srfi.schemers.org/srfi-45/ for more details;
;; specifically, this version is the `lazy2' version from
;; http://srfi.schemers.org/srfi-45/post-mail-archive/msg00013.html and (a
;; `lazy3' variant of `force' that deals with multiple values is included and
;; commented).  Note: if you use only `force'+`delay' it behaves as in Scheme
;; (except that `force' is identity for non promise values), and `force'+`lazy'
;; are sufficient for implementing the lazy language.

(require (for-syntax scheme/base))

(provide lazy delay force promise?)

(define running
  (lambda () (error 'force "reentrant promise")))

(define (promise-printer promise port write?)
  (let loop ([p (promise-val promise)])
    (cond
     [(procedure? p)
      (cond [(object-name p)
             => (lambda (n) (fprintf port "#<promise:~a>" n))]
            [else (display "#<promise>" port)])]
     ;; no values
     [(null? p) (fprintf port "#<promise!(values)>")]
     [(pair? p)
      ;; single or multiple values
      (fprintf port 
               (if write? "#<promise!~a~s" "#<promise!~a~a")
               (if (null? (cdr p))
                   ""
                   "(values ")
               (car p))
      (when (pair? (cdr p))
        (let ([fmt (if write? " ~s" " ~a")])
          (for-each (lambda (x) (fprintf port fmt x)) (cdr p))))
      (unless (null? (cdr p))
        (display ")" port))
      (display ">" port)]
     [(promise? p) (loop (promise-val p))] ; hide sharing
     [else (loop (list p))])))

(define-struct promise (val)
  #:mutable
  #:property prop:custom-write promise-printer)

;; <promise> ::=
;;   | (promise <thunk>)         delayed promise, maybe currently running, maybe an exn promise
;;   | (promise (list <object>)) forced promise (possibly multi-valued)
;;   | (promise <promise>)       shared promise
;;   | (promise <object>)        forced promise, since values

;; Creates a `composable' promise
;;   X = (force (lazy X)) = (force (lazy (lazy X))) = (force (lazy^n X))
(define-syntax (lazy stx)
  (syntax-case stx ()
    [(lazy expr) (with-syntax ([proc (syntax-property
                                      (syntax/loc stx (lambda () expr))
                                      'inferred-name (syntax-local-name))])
                   (syntax/loc stx (make-promise proc)))]))

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
     (with-syntax ([proc (syntax-property
                          (syntax/loc stx (lambda () expr))
                          'inferred-name (syntax-local-name))])
       (syntax/loc stx
         (lazy (make-promise (call-with-values proc list)))))]))

;; force iterates on lazy promises (forbid dependency cycles)
;; * (force X) = X for non promises
;; * does not deal with multiple values, since they're not used by the lazy
;;   language (but see below)

(define handle-results
  (case-lambda
   [(single) (values #t single)]
   [multi (values #f multi)]))

(define (force promise)
  (if (promise? promise)
      (let loop ([p (promise-val promise)])
        (cond
         [(procedure? p)
          ;; mark root for cycle detection:
          (set-promise-val! promise running)
          (with-handlers* ([void (lambda (e)
                                   (set-promise-val! promise (lambda () (raise e)))
                                   (raise e))])
            (let-values ([(single? vals*)
                          (call-with-values p
                            handle-results)])
              (if single?
                  (let loop1 ([val* vals*])
                    (if (promise? val*)
                        (let loop2 ([promise* val*])
                          (let ([p* (promise-val promise*)])
                            (set-promise-val! promise* promise) ; share with root
                            (cond [(procedure? p*)
                                   (let-values ([(single? vals) 
                                                 (call-with-values p*
                                                   handle-results)])
                                     (if single?
                                         (loop1 vals)
                                         (begin
                                           (set-promise-val! promise vals)
                                           (apply values vals))))]
                                  [(or (pair? p*) (null? p*))
                                   (set-promise-val! promise p*)
                                   (apply values p*)]
                                  [(promise? p*) (loop2 p*)]
                                  [else p*])))
                        (begin ; error here for "library approach" (see above URL)
                          (if (or (null? val*) (pair? val*) (procedure? val*))
                              (set-promise-val! promise (list val*))
                              (set-promise-val! promise val*))
                          val*)))
                  (begin ; error here for "library approach" (see above URL)
                    (set-promise-val! promise vals*)
                    (apply values vals*)))))]
         [(or (pair? p) (null? p)) (apply values p)]
         [(promise? p) (loop (promise-val p))]
         [else p]))
      ;; different from srfi-45: identity for non-promises
      promise))
