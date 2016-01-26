#lang racket/base

(require (for-syntax racket/base
                     "arr-util.rkt")
         racket/promise
         (only-in "../../private/promise.rkt" prop:force promise-forcer)
         "prop.rkt"
         "blame.rkt"
         "guts.rkt"
         "rand.rkt"
         "generate.rkt"
         "generate-base.rkt"
         "misc.rkt"
         "list.rkt")

(provide and/c integer-in)

(define (and-name ctc)
  (apply build-compound-type-name 'and/c (base-and/c-ctcs ctc)))

(define (and-first-order ctc)
  (let ([tests (map contract-first-order (base-and/c-ctcs ctc))])
    (λ (x) (for/and ([test (in-list tests)]) (test x)))))

(define (late-neg-and-proj ctc)
  (define mk-pos-projs (map get/build-late-neg-projection (base-and/c-ctcs ctc)))
  (λ (blame)
    (define projs
      (for/list ([c (in-list mk-pos-projs)]
                 [n (in-naturals 1)])
        (c (blame-add-context blame (format "the ~a conjunct of" (n->th n))))))
    (λ (val neg-party)
      (let loop ([projs (cdr projs)]
                 [val ((car projs) val neg-party)])
        (cond
          [(null? projs) val]
          [else
           (loop (cdr projs)
                 ((car projs) val neg-party))])))))

(define (first-order-late-neg-and-proj ctc)
  (define predicates (first-order-and/c-predicates ctc))
  (define blame-accepters (map get/build-late-neg-projection (base-and/c-ctcs ctc)))
  (λ (blame)
    (define new-blame (blame-add-context blame "an and/c case of"))
    (define projs (map (λ (f) (f new-blame)) blame-accepters))
    (λ (val neg-party)
      (let loop ([predicates predicates]
                 [projs projs])
        (cond
          [(null? predicates) val]
          [else
           (cond
             [((car predicates) val)
              (loop (cdr predicates) (cdr projs))]
             [else
              ((car projs) val neg-party)])])))))

(define (and-stronger? this that)
  (and (base-and/c? that)
       (pairwise-stronger-contracts? (base-and/c-ctcs this)
                                     (base-and/c-ctcs that))))

(define (and/c-generate? ctc)
  (cond
    [(and/c-check-nonneg ctc real?) => values]
    [(and/c-check-nonneg ctc rational?) => values]
    [(null? (base-and/c-ctcs ctc)) => (λ (fuel) #f)]
    [else
     (define flat (filter flat-contract? (base-and/c-ctcs ctc)))
     (define ho (filter (λ (x) (not (flat-contract? x))) (base-and/c-ctcs ctc)))
     (cond
       [(null? ho)
        (λ (fuel)
          (define candidates
            (let loop ([sub-contracts-after (cdr (base-and/c-ctcs ctc))]
                       [sub-contract (car (base-and/c-ctcs ctc))]
                       [sub-contracts-before '()]
                       [candidates '()])
              (define sub-gen (contract-random-generate/choose sub-contract fuel))
              (define new-candidates
                (cond
                  [sub-gen
                   (cons (cons sub-gen (append (reverse sub-contracts-before) sub-contracts-after))
                         candidates)]
                  [else candidates]))
              (cond
                [(null? sub-contracts-after) new-candidates]
                [else (loop (cdr sub-contracts-after)
                            (car sub-contracts-after)
                            (cons sub-contract sub-contracts-before)
                            new-candidates)])))
          (cond
            [(null? candidates) #f]
            [else
             (λ ()
               (let loop ([attempts 10])
                 (cond
                   [(zero? attempts) contract-random-generate-fail]
                   [else
                    (define which (oneof candidates))
                    (define val ((car which)))
                    (cond
                      [(andmap (λ (p?) (p? val)) (cdr which))
                       val]
                      [else
                       (loop (- attempts 1))])])))]))]
       [(null? (cdr ho))
        (λ (fuel)
          (define ho-gen (contract-random-generate/choose (car ho) fuel))
          (cond
            [ho-gen 
             (λ ()
               (let loop ([attempts 10])
                 (cond
                   [(zero? attempts) contract-random-generate-fail]
                   [else
                    (define val (ho-gen))
                    (cond
                      [(andmap (λ (p?) (p? val)) flat)
                       val]
                      [else
                       (loop (- attempts 1))])])))]
            [else #f]))]
       [else
        (λ (fuel) #f)])]))

(define (and/c-check-nonneg ctc pred)
  (define sub-contracts (base-and/c-ctcs ctc))
  (cond
    [(pairwise-stronger-contracts?
      (list (coerce-contract 'and/c-check-nonneg pred) (not/c negative?))
      sub-contracts)
     (define go (hash-ref predicate-generator-table pred))
     (λ (fuel)
       (λ ()
         (abs (go fuel))))]
    [else #f]))

(define-struct base-and/c (ctcs))
(define-struct (first-order-and/c base-and/c) (predicates)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:late-neg-projection first-order-late-neg-and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?
   #:generate and/c-generate?))
(define-struct (chaperone-and/c base-and/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection late-neg-and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?
   #:generate and/c-generate?))
(define-struct (impersonator-and/c base-and/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection late-neg-and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?
   #:generate and/c-generate?))

(define-syntax (and/c stx)
  (syntax-case stx (pair? listof)
    [(_ pair? (listof e))
     #'(non-empty-listof e)]
    [(_ (listof e) pair?)
     #'(non-empty-listof e)]
    [(_ . args)
     #'(real-and/c . args)]
    [x
     (identifier? #'x)
     #'real-and/c]))

(define/subexpression-pos-prop/name real-and/c-name (real-and/c . raw-fs)
  (let ([contracts (coerce-contracts 'and/c raw-fs)])
    (cond
      [(null? contracts) any/c]
      [(andmap flat-contract? contracts)
       (let ([preds (map flat-contract-predicate contracts)])
         (make-first-order-and/c contracts preds))]
      [(andmap chaperone-contract? contracts)
       (make-chaperone-and/c contracts)]
      [else (make-impersonator-and/c contracts)])))



(struct integer-in-ctc (start end)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (λ (ctc) 
            `(integer-in ,(integer-in-ctc-start ctc)
                         ,(integer-in-ctc-end ctc)))
   #:first-order (λ (ctc)
                   (define start (integer-in-ctc-start ctc))
                   (define end (integer-in-ctc-end ctc))
                   (λ (x) (and (exact-integer? x)
                               (<= start x end))))
   #:stronger (λ (this that)
                (define this-start (integer-in-ctc-start this))
                (define this-end (integer-in-ctc-end that))
                (cond
                  [(integer-in-ctc? that)
                   (define that-start (integer-in-ctc-start that))
                   (define that-end (integer-in-ctc-end that))
                   (<= that-start this-start this-end that-end)]
                  [else #f]))
   #:generate (λ (ctc)
                (define start (integer-in-ctc-start ctc))
                (define end (integer-in-ctc-end ctc))
                (λ (fuel)
                  (λ ()
                    (+ start (random (min 4294967087 (+ (- end start) 1)))))))))

(define/final-prop (integer-in start end)
  (check-two-args 'integer-in start end exact-integer? exact-integer?)
  (if (= start end)
      (and/c start exact?)
      (integer-in-ctc start end)))