#lang racket/base
(require racket/list
         "inline.rkt"
         "state.rkt"
         "id.rkt"
         "ref.rkt"
         "vehicle.rkt"
         "runstack.rkt"
         "sort.rkt"
         "out.rkt")

(provide simple?
         call-with-simple-shared
         generate-simple
         simple-quote?
         always-fixnum?)

;; A simple expression is one that can be reordered and doesn't
;; trigger a GC. The "reordered" consideration can ignore whether a
;; reference is the final reference to a variable (which may imply
;; clearing), and `call-with-simple-shared` is always used around
;; `generate-shared` to take care of that detail.

(define (simple? e in-lam state knowns)
  (define (simple? e)
    (or (and (symbol-ref? e)
             (not (mutated? (hash-ref state (unref e) #f))))
        (simple-quote? e)
        (and (pair? e)
             (symbol? (car e))
             (inline-function (car e) (length (cdr e)) (cdr e) in-lam knowns #:can-gc? #f)
             (for/and ([e (in-list (cdr e))])
               (simple? e)))))
  (simple? e))

(define (call-with-simple-shared e runstack state proc
                                 #:about-to-sync? [about-to-sync? #f])
  ;; If a runstack variable is referenced twice, lift out and share
  ;; the reference to avoid relying on an order within the simple
  ;; expression. If `about-to-sync?` is #t, then also lift out any
  ;; reference to variables.
  (define-values (saw shared)
    (let loop ([e e] [saw #hasheq()] [shared #hasheq()])
      (cond
        [(ref? e)
         (define id (ref-id e))
         (define saw-e (hash-ref saw id #f))
         (define new-saw (if saw-e saw (hash-set saw id e)))
         (values new-saw
                 (if (or saw-e about-to-sync?)
                     (hash-set shared id (genid 'c_simple))
                     shared))]
        [(pair? e)
         (for/fold ([saw saw] [shared shared]) ([e (cdr e)])
           (loop e saw shared))]
        [else (values saw shared)])))
  (unless (hash-empty? shared)
    (out-open "{")
    (for ([(id new-id) (in-sorted-hash shared symbol<?)])
      (define ref (hash-ref saw id))
      (ref-use! ref state)
      (out "Scheme_Object *~a = ~a;" (cify new-id) (runstack-ref runstack id #:ref ref))))
  (proc shared)
  (unless (hash-empty? shared)
    (out-close "}")))

;; The `e` argument can be a string as pre-generated
(define (generate-simple e shared env runstack in-lam state top-names knowns prim-names)
  (define (generate-simple e)
    (cond
      [(string? e) e]
      [(boolean? e) (if e "scheme_true" "scheme_false")]
      [(always-fixnum? e) (format "scheme_make_integer(~a)" e)]
      [(symbol-ref? e)
       (cond
         [(ref? e)
          (define id (ref-id e))
          (cond
            [(hash-ref shared id #f)
             => (lambda (new-id) (format "~a" (cify new-id)))]
            [else
             (ref-use! e state)
             (runstack-ref runstack id #:ref e)])]
         [(or (hash-ref top-names e #f)
              (hash-ref knowns e #f))
          (format "~a" (top-ref in-lam e))]
         [(hash-ref prim-names e #f)
          (cond
            [(eq? e 'null) "scheme_null"]
            [(eq? e 'eof-object) "scheme_eof"]
            [(eq? e 'unsafe-undefined) "scheme_undefined"]
            [else (format "c_prims.~a" (cify e))])]
         [else (runstack-ref runstack e)])]
      [else
       (define inliner (inline-function (car e) (length (cdr e)) (cdr e) in-lam knowns))
       (define args (apply string-append
                           (append
                            (add-between
                             (for/list ([e (in-list (cdr e))])
                               (format "~a" (generate-simple e)))
                             ", "))))
       (cond
         [(procedure? inliner) (if (procedure-arity-includes? inliner 2)
                                   (inliner args (lambda (id) (top-ref in-lam id)))
                                   (inliner args))]
         [else
          (format "~a(~a)" inliner args)])]))
  (generate-simple e))

;; ----------------------------------------

(define (simple-quote? e)
  (or (always-fixnum? e)
      (boolean? e)
      (null? e)
      (void? e)
      (and (char? e)
           (<= 0 (char->integer e) 255))))

(define (always-fixnum? e)
  (and (integer? e)
       (exact? e)
       (<= (- (expt 2 30)) e (sub1 (expt 2 30)))))
