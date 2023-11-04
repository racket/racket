#lang racket/base

(require (for-syntax racket/base
                     syntax/name))

(provide yield generator generator-state in-generator infinite-generator
         sequence->generator sequence->repeated-generator
         generator?)

;; (require racket/control racket/stxparam racket/splicing)

;; (define-syntax-parameter yield
;;   (lambda (stx)
;;     (raise-syntax-error
;;      #f "yield is only bound inside a sequence generator")))

;; (define (procedure->generator proc)
;;   (define tag (make-continuation-prompt-tag))
;;   (define (cont)
;;     (reset-at tag
;;       (let ([r (proc (lambda (r) (shift-at tag k (set! cont k) r)))])
;;         ;; normal return:
;;         (set! cont (lambda () r))
;;         r)))
;;   (lambda () (cont)))

;; not using parameterization (old version, doesn't deal with multiple
;; inputs/outputs as the one below)
#;
(define-syntax-rule (generator body0 body ...)
  (let ([tag (make-continuation-prompt-tag)])
    (define yielder
      (let ([yield (lambda (value) (shift-at tag k (set! cont k) value))])
        yield))
    (splicing-syntax-parameterize ([yield (make-rename-transformer #'yielder)])
      (define (cont)
        (reset-at tag
          (let ([retval (begin body0 body ...)])
            ;; normal return:
            (set! cont (lambda () retval))
            retval))))
    (define (generator) (cont))
    generator))

(define current-generator
  (make-parameter #f #f 'current-generator))

(define (yield . vs)
  (define self (current-generator))
  (unless self
    (raise-arguments-error 'yield
                           "must be called in the context of a generator"))
  (call-with-composable-continuation
   (lambda (cont)
     (suspended-generator! self cont)
     (apply abort-current-continuation yield-tag vs))
   yield-tag))

(define yield-tag (make-continuation-prompt-tag 'yield))

(define-syntax (generator stx)
  (syntax-case stx ()
    [(_ formals body0 body ...)
     (let ()
       (syntax-case #'formals ()
         [(arg ... . rest-arg)
          (and (andmap identifier? (syntax->list #'(arg ...)))
               (or (identifier? #'rest-arg)
                   (null? (syntax-e #'rest-arg))))
          (void)]
         [_
          (raise-syntax-error #f "bad syntax for formals" stx #'formals)])
       (define name
         (or (syntax-local-infer-name stx)
             'generator))
       #`(create-generator '#,name
                           #,(syntax-property
                              (syntax/loc stx
                                (lambda formals body0 body ...))
                              'inferred-name name)
                           #f))]))

(define-syntax (infinite-generator stx)
  (syntax-case stx ()
    [(_ body0 body ...)
     (let ()
       (define name
         (or (syntax-local-infer-name stx)
             'infinite-generator))
       #`(create-generator '#,name
                           (letrec ([loop #,(syntax-property
                                             (syntax/loc stx
                                               (lambda () body0 body ... (loop)))
                                             'inferred-name name)])
                             loop)
                           #t))]))

(define (create-generator name start loop?)
  (define (fresh-cont self . vs)
    (define (start-thunk)
      (parameterize ([current-generator self])
        (apply start vs)))
    (running-generator! self)
    (call-with-continuation-prompt
     (if loop?
         start-thunk
         (lambda ()
           (call-with-values start-thunk
             ;; get here only at the end of non-infinite generator
             (lambda rs
               (done-generator! self rs)
               (apply values rs)))))
     yield-tag
     values))
  (make-generator name fresh-cont 'fresh))

(define (apply-generator-cont self . vs)
  (apply (generator-cont self) self vs))

(struct generator (name [cont #:mutable] [state #:mutable])
  #:constructor-name make-generator
  #:property prop:object-name (struct-field-index name)
  #:property prop:procedure apply-generator-cont
  #:omit-define-syntaxes)

(define (running-generator! self)
  (define (running-cont self . _)
    (raise-arguments-error 'generator
                           "cannot resume a running generator"
                           "generator" self))
  (set-generator-cont! self running-cont)
  (set-generator-state! self 'running))

(define (suspended-generator! self cont)
  (define (suspended-cont self . vs)
    (running-generator! self)
    (call-with-continuation-prompt
     (lambda ()
       (apply cont vs))
     yield-tag
     values))
  (set-generator-cont! self suspended-cont)
  (set-generator-state! self 'suspended))

(define (done-generator! self rs)
  (define done-cont
    (case-lambda
      [(_) (apply values rs)]
      [(self . _) (raise-arguments-error 'generator
                                         "cannot send values to a done generator"
                                         "generator" self)]))
  (set-generator-cont! self done-cont)
  (set-generator-state! self 'done))

(define stop-value (gensym 'stop-value))

(begin-for-syntax
  (define (expand-in-generator arity arity-allowed? stx)
    (syntax-case stx ()

      [(_ #:arity n body0 body ...)
       (let ([new-arity (syntax-e #'n)])
         (unless (exact-nonnegative-integer? new-arity)
           (define message "expected a literal exact nonnegative integer")
           (raise-syntax-error #f message stx #'n))
         (unless arity-allowed?
           (define message "cannot specify arity more than once")
           (raise-syntax-error #f message stx #'n))
         (when (and arity (not (= arity new-arity)))
           (define message
             (format "arity mismatch, context expects a generator of arity ~a"
               arity))
           (raise-syntax-error #f message stx #'n))
         (define new-stx (syntax/loc stx (in-generator body0 body ...)))
         (expand-in-generator new-arity #f new-stx))]

      [(_ body0 body ...)
       (let ([real-arity (or arity 1)])
         (cond
           [(zero? real-arity)
            #'(let ([stop? #f])
                (in-producer
                  (generator () body0 body ... (set! stop? #t) (values))
                  (lambda () stop?)))]
           [else
            (define vars (generate-temporaries (build-list real-arity values)))
            (define fs (build-list (sub1 real-arity) (lambda (_) #'#f)))
            (with-syntax ([(x ...) vars]
                          [x0 (car vars)]
                          [(f ...) fs])
              #'(in-producer
                  (generator () body0 body ... (values stop-value f ...))
                  (lambda (x ...) (eq? x0 stop-value))))]))])))

(define-sequence-syntax in-generator
  (lambda (stx) (expand-in-generator #f #t stx))
  (lambda (stx)
    (syntax-case stx ()
      [((id ...) expr)
       (let ([arity (length (syntax->list #'(id ...)))])
         (with-syntax ([e (expand-in-generator arity #t #'expr)])
           #'[(id ...) e]))])))

(define (sequence->generator sequence)
  (generator () (for ([i sequence]) (yield i))))

(define (sequence->repeated-generator sequence)
  (infinite-generator (for ([i sequence]) (yield i))))

#|
;; examples
(for/list ([i (in-generator (for-each yield '(1 2 3)) (yield 'four))]) i)
(for*/list ([i (in-generator (for-each yield '(1 2 3)) (yield 'four))]
            [j (in-generator (yield 'X) (yield '-))])
  (list i j))
|#
