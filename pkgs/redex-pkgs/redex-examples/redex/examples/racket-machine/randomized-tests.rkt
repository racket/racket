#lang racket

(require redex/reduction-semantics
         (only-in redex/examples/racket-machine/grammar bytecode)
         (only-in redex/examples/racket-machine/reduction -> load runtime)
         (only-in redex/examples/racket-machine/verification bytecode-ok?)
         redex/examples/racket-machine/model-impl
         compiler/zo-parse compiler/zo-marshal)

(provide (all-defined-out))

;; Test the internal properties (verifier totality, safety, and confluence)
;; on acyclic expressions. The verifier's totality is tested implicitly by
;; treating a "no clauses matched" exception as a test failure.
(define (test-internal-properties 
         #:verifier [verified? bytecode-ok?]
         #:reduction [-> ->]
         #:prepare [prepare fix]
         #:steps [steps 100]
         #:attempts [attempts 5000])
  (redex-check
   bytecode e (safe-and-confluent? (term e) (term ()) verified? -> steps)
   #:prepare prepare
   #:attempts attempts))

;; Tests the internal properties on cyclic expressions.
(define (test-internal-properties/cycles
         #:steps [steps 100]
         #:attempts [attempts 5000]
         #:print? [print? #t])
  (redex-check
   bytecode (side-condition (e (x_0 (name e_0 (proc-const (τ ...) e_0*))) ...)
                            (equal? (term (x_0 ...))
                                    (remove-duplicates (term (x_0 ...)))))
   (safe-and-confluent? (term e) (term ((x_0 e_0) ...)) bytecode-ok? -> steps)
   #:attempts attempts
   #:prepare fix-cyclic
   #:print? print?))

;; Tests the external properties (the model and implementation agree on
;; verification and evaluation questions).
(define (test-external-properties
         #:prepare [prepare (compose optimize fix)]
         #:model-verifier [model-verified? bytecode-ok?]
         #:steps [steps 100]
         #:timeout [timeout 10]
         #:attempts [attempts 5000]
         #:print? [print? #t])
  (redex-check
   bytecode e
   (model-impl-consistent? (term e) '() steps timeout model-verified?)
   #:prepare prepare
   #:attempts attempts
   #:print? print?))

; result = answer | stuck | cutoff | non-conf
(define-struct cutoff ())
(define-struct stuck (state))
(define-struct answer (value))
(define-struct non-conf (values))

; run: e ((x e) ...) nat -> result
(define (run e cycles cutoff #:reduction [-> ->])
  (let ([cache (make-hash)])
    (let loop ([s (term (load ,e ,cycles))]
               [c cutoff])
      (let ([r (hash-ref cache s #f)])
        (if (and r (>= (car r) c))
            (cdr r)
            (begin
              ; If we see this state again while it's marked pending, 
              ; it will be with a lesser cutoff, and it will indicate a 
              ; cycle in the reduction graph. Without pruning, a cycle
              ; the reduction graph produces a cutoff result; with it
              ; a cylce produces a pending, which is treated identically.
              (hash-set! cache s (cons c 'pending))
              (let ([r
                     (cond [(term (halted? ,s)) 
                            (make-answer 
                             (if (eq? s 'error)
                                 'error
                                 (car s)))]
                           [(zero? c) (make-cutoff)]
                           [else
                            (let ([nexts (map (curryr loop (sub1 c)) (apply-reduction-relation -> s))])
                              (if (null? nexts)
                                  (make-stuck s)
                                  (or (findf non-conf? nexts)
                                      (findf stuck? nexts)
                                      (let ([answers (filter answer? nexts)])
                                        (if (null? answers)
                                            (make-cutoff)
                                            (let cmp ([others (cdr answers)])
                                              (if (null? others)
                                                  (car answers)
                                                  (if (equal? (answer-value (car answers))
                                                              (answer-value (car others)))
                                                      (cmp (cdr others))
                                                      (make-non-conf 
                                                       (list (answer-value (car answers))
                                                             (answer-value (car others))))))))))))])])
                (begin
                  (hash-set! cache s (cons c r))
                  r))))))))

(define (verified/cycles? expr cycles verified?)
  (and (verified? expr)
       (andmap (match-lambda [`(,_ ,e) (verified? e)])
               cycles)))

(define (safe-and-confluent? expr cycles verified? -> steps)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (or (not (verified/cycles? expr cycles verified?))
        (match (run expr cycles steps #:reduction ->)
          [(cutoff) #t]
          [(answer _) #t]
          [(stuck _) #f]
          [(non-conf _) #f]))))

(define (model-impl-consistent? expr cycles steps timeout model-verified?)
  (define (equiv-results? m i)
    (match m
      ['uninit #f]
      [`(box ,_) #f]
      ['undefined (impl-undefined-val? i)]
      [`(clos ,_) (impl-clos-val? i)]
      ['void (void? i)]
      [else (equal? m i)]))
  (with-handlers ([exn:fail? (λ (_) #f)])
    (let ([model-verified? (verified/cycles? expr cycles model-verified?)]
          [impl-result (eval-impl-external (model->impl expr cycles) timeout)])
      (and 
       (if model-verified? 
           (not (impl-rejected? impl-result))
           (impl-rejected? impl-result))
       (or (not model-verified?)
           (match (run expr cycles steps)
             [(cutoff) #t]
             [(stuck _) #f]
             [(non-conf _) #f]
             [(answer a) (equiv-results?
                          a
                          (match impl-result
                            [(impl-answer v) v]
                            [(impl-exception _) 'error]))]))))))

;; A reimplementation of the optimizations in the implementation's
;; bytecode reader
(define (optimize expr)
  (define value?
    (let ([v? (redex-match bytecode v)])
      (λ (e)
        (or (v? e)
            (match e
              [`(proc-const ,_ ,_) #t]
              [`(lam ,_ ,_ ,_) #t]
              [`(case-lam ,_ ...) #t]
              [else #f])))))
  (define omittable?
    (match-lambda
      [`(loc ,_) #t]
      [`(loc-noclr ,_) #t]
      [`(loc-box ,_) #t]
      [`(loc-box-noclr ,_) #t]
      [`(proc-const ,_ ,_) #t]
      [`(lam ,_ ,_ ,_) #t]
      [`(case-lam ,_ ...) #t]
      [(? (redex-match bytecode v)) #t]
      [`(branch ,c ,t ,e)
       (and (omittable? c)
            (omittable? t)
            (omittable? e))]
      [`(let-one ,r ,b)
       (and (omittable? r)
            (omittable? b))]
      [`(,(or 'let-void 'let-void-box) 
         ,_ 
         (,(or 'install-value 'install-value-box) 0 ,r ,b))
       (and (omittable? r)
            (omittable? b))]
      [`(,(or 'let-void 'let-void-box) ,_ ,b)
       (omittable? b)]
      [`(let-rec ,_ ,b)
       (omittable? b)]
      [`(application void ,es ...)
       (andmap omittable? es)]
      [else #f]))
  (let recur ([e expr])
    (match e
      [`(branch ,c ,t ,e)
       (let ([c* (recur c)])
         (if (value? c*)
             (if c* (recur t) (recur e))
             `(branch ,c* ,(recur t) ,(recur e))))]
      [`(seq ,es ...)
       (letrec ([flatten (λ (es)
                           (foldr (λ (e a)
                                    (match e
                                      [`(seq ,es ...)
                                       (append es a)]
                                      [else (cons e a)]))
                                  '() es))]
                ; preserves tail position
                [omit (λ (es kept?)
                        (if (null? es)
                            '()
                            (if (and (omittable? (car es))
                                     (not (null? (cdr es))))
                                (omit (cdr es) kept?)
                                (cons (car es) (omit (cdr es) #t)))))])
         (match (omit (map recur (flatten es)) #f)
           ['() 'void]
           [`(,e) e]
           [es `(seq ,@es)]))]
      [(? list?) (map recur e)]
      [else e])))

;; Performs a write-read round trip to see the optimizations performed
;; by the implementation's bytecode reader
(define (write-read expr)
  (let-values ([(in out) (make-pipe)]
               [(tmp) (make-temporary-file)])
    (zo-marshal-to expr out)
    (let ([optimized (parameterize ([read-accept-compiled #t])
                       (read in))])
      (call-with-output-file tmp #:exists 'truncate
        (λ (p) (write optimized p))))
    (begin0
      (call-with-input-file tmp zo-parse)
      (delete-file tmp))))

(define-metafunction runtime
  halted? : p -> b
  [(halted? (V S H T ())) #t]
  [(halted? error) #t]
  [(halted? (V S H T (i_0 i_1 ...))) #f])

(define random-expr
  (let ([generator (generate-term bytecode e)])
    (λ (attempt) 
      (generator (inexact->exact (round (/ (log attempt) (log 5))))))))
(define random-value
  (let ([generator (generate-term bytecode v)])
    (λ () (generator 3))))
(define-syntax-rule (ref-or-else n e e*)
  (if (zero? n) e* e))

(define cycle-targets (make-parameter '()))

;; Makes three classes of corrections:
;; 1. replaces out-of-bounds stack offsets with random in-bounds 
;;    ones (without checking that the new target contains an
;;    appropriate value),
;; 2. replaces `ref' argument annotations with `val' annotations
;;    when the procedure appears in a context that does not allow
;;    `ref' arguments (but does not alter the procedure's body), and
;; 3. replaces `indirect' targets with ones in the `cycle-targets'
;;    parameter (and replaces the entire `indirect' expression
;;    with a random value if `cycle-targets' is empty).
(define (fix expr)
  (define (localref? i)
    (memq i '(loc loc-noclr loc-clr loc-box loc-box-noclr loc-box-clr)))
  (let recur ([depth 0] [refs? #f] [expr expr])
    (match expr
      [`(,(? localref? i) ,n) 
       (ref-or-else depth `(,i ,(modulo n depth)) (random-value))]
      
      [`(let-one ,e1 ,e2)
       `(let-one ,(recur (add1 depth) #f e1)
                 ,(recur (add1 depth) #f e2))]
      [`(,(and (or 'let-void 'let-void-box) i) ,n ,e)
       `(,i ,n ,(recur (+ depth n) #f e))]
      
      [`(boxenv ,n ,e)
       (let ([e* (recur depth #f e)])
         (ref-or-else depth `(boxenv ,(modulo n depth) ,e*) e*))]
      [`(,(and (or 'install-value 'install-value-box) i) ,n ,e1 ,e2)
       (let ([e1* (recur depth #f e1)]
             [e2* (recur depth #f e2)])
         (ref-or-else depth `(,i ,(modulo n depth) ,e1* ,e2*) `(seq ,e1* ,e2*)))]
      
      [`(application ,e ,es ...)
       (let ([m (length es)])
         `(application
           ,(recur depth #t e)
           ,@(map (curry recur (+ depth m) #f) es)))]
      [`(,(and (or 'branch 'seq) i) ,es ...)
       `(,i ,@(map (curry recur depth #f) es))]
      [`(let-rec (,ls ...) ,e)
       (let ([n (min depth (length ls))]
             [ls* (map (curry recur depth #f) ls)])
         `(let-rec ,(map (curry recur depth #f) (take ls* n))
                   ,(recur depth #f e)))]
      [`(indirect ,c) 
       (cond [(memq c (cycle-targets))
              `(indirect ,c)]
             [(not (null? (cycle-targets))) 
              `(indirect ,(list-ref (cycle-targets) (random (length (cycle-targets)))))]
             [else
              (random-value)])]
      [`(proc-const (,τs ...) ,e)
       (let ([n (length τs)])
         `(proc-const ,(if refs? τs (build-list n (λ (_) 'val)))
                      ,(recur n #f e)))]
      [`(case-lam ,ls ...)
       `(case-lam ,@(map (curry recur depth #f) ls))]
      [`(lam (,τs ...) (,ns ...) ,e)
       (let ([m (length τs)])
         `(lam ,(if refs? τs (build-list m (λ (_) 'val)))
               ,(if (zero? depth) 
                    '()
                    (map (curryr modulo depth) ns))
               ,(recur (+ m (if (zero? depth) 0 (length ns))) #f e)))]
      [(? number?) expr]
      ['void expr]
      [`(quote ,_) expr]
      [(? boolean?) expr])))

(define fix-cyclic
  (match-lambda
    [`(,e (,xs ,es) ...)
     (parameterize ([cycle-targets xs])
       (cons (fix e) 
             (map (λ (x e) (list x (fix e)))
                  xs es)))]))

;; Replaces all `indirect' expressions with random values
(define no-indirects
  (match-lambda
    [`(indirect ,_) (random-value)]
    [(? list? l) (map no-indirects l)]
    [e e]))

(define (main)
  (define-syntax-rule (test t k)
    (match t
      [#t k]
      [(counterexample p)
       (pretty-print p (current-error-port))
       (exit 1)]))
  (test (time (test-internal-properties/cycles #:attempts 4000 #:print? #f))
        (test (time (test-external-properties #:attempts 250 #:print? #f))
              (void))))

(module+ main (void (main)))
