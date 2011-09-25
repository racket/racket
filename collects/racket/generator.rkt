#lang racket/base

(require (for-syntax racket/base)
         racket/control)

(provide yield generator generator-state in-generator infinite-generator
         sequence->generator sequence->repeated-generator
         generator?)

;; (require racket/stxparam racket/splicing)

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

(define current-yielder
  (make-parameter
   (lambda (v)
     (error 'yield "must be called in the context of a generator"))))

(define yield
  (case-lambda [()  ((current-yielder))]
               [(v) ((current-yielder) v)]
               [vs  (apply (current-yielder) vs)]))

(define yield-tag (make-continuation-prompt-tag))

(define-syntax (generator stx)
  (syntax-case stx ()
    [(_ formals body0 body ...) 
     (if (let loop ([formals #'formals])
           (cond
            [(null? formals) #t]
            [(identifier? formals) #t]
            [(syntax? formals) (loop (syntax-e formals))]
            [(pair? formals) (and (identifier? (car formals))
                                  (loop (cdr formals)))]
            [else #f]))
         #'(create-generator (let ([generator
                                    (lambda formals
                                      body0 body ...)])
                               generator))
         (raise-syntax-error
          #f
          "bad syntax for formal initial arguments"
          stx
          #'formals))]
    [_ (if (identifier? stx)
           (raise-syntax-error #f "bad syntax"  stx)
           (raise-syntax-error
            #f
            (format "use does not have the form (~a formals body ...)"
                    (syntax-e (car (syntax-e stx))))
            stx))]))

(define (create-generator start)
  (let ([state 'fresh])
    (define (cont . init-formals)
      (define (yielder . vs)
        (set! state 'suspended)
        (shift-at yield-tag k (set! cont k) (apply values vs)))
      (set! state 'running)
      (reset-at yield-tag
        (parameterize ([current-yielder yielder])
          (call-with-values
              (lambda ()
                (apply start init-formals))
              ;; get here only on at the end of the generator
              (lambda rs
                (set! cont (lambda () (set! state 'done) (apply values rs)))
                (cont))))))
    (define (err [what "send a value to"])
      (raise-mismatch-error 'generator 
                            (format "cannot ~a a ~a generator: " what state)
                            self))
    (define generator
      (case-lambda
        [()  (if (eq? state 'running)
               (err "call")
               (begin (set! state 'running) (cont)))]
        ;; yield-tag means return the state (see `generator-state' below)
        [(x) (cond [(eq? x yield-tag) state]
                   [(memq state '(suspended running fresh))
                    (set! state 'running)
                    (cont x)]
                   [else (err)])]
        [xs  (if (memq state '(suspended running fresh))
               (begin (set! state 'running) (apply cont xs))
               (err))]))
    (define self (make-generator generator))
    self))

(define-struct generator (proc)
  #:property prop:procedure 0
  #:omit-define-syntaxes)

;; Get the state -- this is a hack: uses yield-tag as a hidden value that makes
;; the generator return its state.  Protect against grabbing this tag (eg, with
;; (generator-state values)) by inspecting the result (so it can still be
;; deceived, but that will be harmless).
(define (generator-state g)
  (if (generator? g)
      (g yield-tag)
      (raise-type-error 'generator-state "generator" g)))

(define-syntax-rule (infinite-generator body0 body ...)
  (generator () (let loop () body0 body ... (loop))))

(define stop-value (gensym))

(define-sequence-syntax in-generator
  (syntax-rules ()
    [(_ body0 body ...)
     (in-producer (generator () body0 body ... stop-value) stop-value)])
  (lambda (stx)
    (syntax-case stx ()
      [(() (_ body0 body ...))
       #'[()
          (in-producer (generator () body0 body ... stop-value) stop-value)]]
      [((id ...) (_ body0 body ...))
       (with-syntax ([(stops ...) (syntax-case #'((id stop-value) ...) ()
                                    [((x v) ...) #'(v ...)])])
       #'[(id ...)
          (in-producer (generator () body0 body ... (values stops ...))
                       (lambda xs (eq? (car xs) stop-value)))])])))

(define (sequence->generator sequence)
  (generator () (for ([i sequence]) (yield i))))

(define (sequence->repeated-generator sequence)
  (sequence->generator (in-cycle sequence)))

#|
;; examples
(for/list ([i (in-generator (for-each yield '(1 2 3)) (yield 'four))]) i)
(for*/list ([i (in-generator (for-each yield '(1 2 3)) (yield 'four))]
            [j (in-generator (yield 'X) (yield '-))])
  (list i j))
|#
