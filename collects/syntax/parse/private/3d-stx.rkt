#lang racket/base
(require (only-in '#%flfxnum flvector? fxvector?)
         (only-in '#%extfl extflonum? extflvector?))
(provide 2d-stx?
         check-datum)

;; Checks for 3D syntax (syntax that contains unwritable values, etc)

(define INIT-FUEL #e1e6)

;; TO DO:
;; - extension via proc (any -> list/#f),
;;   value considered good if result is list, all values in list are good

;; --

#|
Some other predicates one might like to have:
 - would (read (write x)) succeed and be equal/similar to x?
 - would (datum->syntax #f x) succeed?
 - would (syntax->datum (datum->syntax #f x)) succeed and be equal/similar to x?
 - would (eval (read (write (compile `(quote ,x))))) succeed and be equal/similar to x?

where equal/similar could mean one of the following:
 - equal?, which equates eg (vector 1 2 3) and (vector-immutable 1 2 3)
 - equal? relaxed to equate eg mutable and immutable hashes (but not prefabs)
 - equal? but also requiring same mutability at every point

Some aux definitions:

(define (rt x)
  (define-values (in out) (make-pipe))
  (write x out)
  (close-output-port out)
  (read in))

(define (wrsd x)
  (define-values (in out) (make-pipe))
  (write x out)
  (close-output-port out)
  (syntax->datum (read-syntax #f in)))

(define (dsd x)
  (syntax->datum (datum->syntax #f x)))

(define (evalc x) ;; mimics compiled zo-file constraints
  (eval (rt (compile `(quote ,x)))))

How mutability behaves:
 - for vectors, boxes:
   - read always mutable
   - read-syntax always immutable
   - (dsd x) always immutable
   - (evalc x) always immutable
 - for hashes:
   - read always immutable
   - (dsd x) same as x
   - (evalc x) always immutable (!!!)
 - for prefab structs:
   - read same as x
   - read-syntax same as x
   - (dsd x) same as x
   - (evalc x) same as x

Symbols
 - (dsd x) same as x
 - (evalc x) preserves interned, unreadable, makes new uninterned (loses eq-ness)

Chaperones allow the lazy generation of infinite trees of data
undetectable by eq?-based cycle detection.  Might be helpful to have
chaperone-eq? (not recursive, just chaperones of same object) and
chaperone-eq?-hash-code, to use with make-custom-hash.)

Impersonators allow the lazy generation of infinite trees of data,
period.

|#

;; ----

;; 2d-stx? : any ... -> boolean
;; Would (write (compile `(quote-syntax ,x))) succeed?
;; If traverse-syntax? is #t, recurs into existing syntax
;; If traverse-syntax? is #f, assumes existing stxobjs are 2d, and only
;; checks if *new* 3d syntax would be created.
(define (2d-stx? x
                 #:traverse-syntax? [traverse-syntax? #t]
                 #:irritant [irritant-box #f])
  (check-datum x
               #:syntax-mode (if traverse-syntax? 'compound 'atomic)
               #:allow-impersonators? #f
               #:allow-mutable? 'no-hash/prefab
               #:allow-unreadable-symbols? #t
               #:allow-cycles? #t
               #:irritant irritant-box))

;; ----

;; check-datum : any ... -> boolean
;; where StxMode = (U 'atomic 'compound #f)
;; Returns nat if x is "good", #f if "bad"
;; If irritant-b is a box, the first bad subvalue found is put in the box.
;; If visited-t is a hash, it is used to detect cycles.
(define (check-datum x
                     #:syntax-mode [stx-mode #f]
                     #:allow-impersonators? [allow-impersonators? #f]
                     #:allow-mutable? [allow-mutable? #f]
                     #:allow-unreadable-symbols? [allow-unreadable? #f]
                     #:allow-cycles? [allow-cycles? #f]
                     #:irritant [irritant-b #f])
  ;; Try once with some fuel. If runs out of fuel, try again with cycle checking.
  (define (run fuel visited-t)
    (check* x fuel visited-t
            stx-mode allow-impersonators? allow-mutable? allow-unreadable? allow-cycles?
            irritant-b))
  (let ([result (run INIT-FUEL #f)])
    (cond [(not (equal? result 0)) ;; nat>0 or #f
           (and result #t)]
          [else
           ;; (eprintf "out of fuel, restarting\n")
           (and (run +inf.0 (make-hasheq)) #t)])))

;; check* : any nat/+inf.0 StxMode boolean boolean boolean box -> nat/#f
;; Returns #f if bad, positive nat if good, 0 if ran out of fuel
;; If bad, places bad subvalue in irritant-b, if box
(define (check* x0 fuel0 visited-t
                stx-mode allow-impersonators? allow-mutable? allow-unreadable? allow-cycles?
                irritant-b)
  (define no-mutable? (not allow-mutable?))
  (define no-mutable-hash/prefab? (or no-mutable? (eq? allow-mutable? 'no-hash/prefab)))
  (define no-cycle? (not allow-cycles?))
  (define no-impersonator? (not allow-impersonators?))
  (define (loop x fuel)
    (if (and fuel (not (zero? fuel)))
        (loop* x fuel)
        fuel))
  (define (loop* x fuel)
    (define (bad) (when irritant-b (set-box! irritant-b x)) #f)
    (define-syntax-rule (with-mutable-check mutable? body ...) ;; don't use for hash or prefab
      (cond [(and no-mutable? mutable?)
             (bad)]
            [else
             body ...]))
    (define-syntax-rule (with-cycle-check body ...)
      (cond [(and visited-t (hash-ref visited-t x #f))
             => (lambda (status)
                  (cond [(and no-cycle? (eq? status 'traversing))
                         (bad)]
                        [else
                         fuel]))]
            [else
             (when visited-t
               (hash-set! visited-t x 'traversing))
             (begin0 (begin body ...)
               (when visited-t
                 (hash-remove! visited-t x)))]))
    ;; (eprintf "-- checking ~s, fuel ~s\n" x fuel)
    (cond
     ;; Immutable compound
     [(and visited-t (list? x))
      ;; space optimization: if list (finite), no need to store all cdr pairs in cycle table
      ;; don't do unless visited-t present, else expands fuel by arbitrary factors
      (with-cycle-check
       (for/fold ([fuel (sub1 fuel)]) ([e (in-list x)] #:break (not fuel))
         (loop e fuel)))]
     [(pair? x)
      (with-cycle-check
       (let ([fuel (loop (car x) (sub1 fuel))])
         (loop (cdr x) fuel)))]
     ;; Atomic
     [(or (null? x)
          (boolean? x)
          (number? x)
          (char? x)
          (keyword? x)
          (regexp? x)
          (extflonum? x))
      fuel]
     [(symbol? x)
      (cond [(symbol-interned? x)
             fuel]
            [(symbol-unreadable? x)
             (if allow-unreadable? fuel (bad))]
            [else ;; uninterned
             (if (eq? allow-unreadable? #t) fuel (bad))])]
     ;; Mutable flat
     [(or (string? x)
          (bytes? x))
      (with-mutable-check (not (immutable? x))
        fuel)]
     [(or (fxvector? x)
          (flvector? x)
          (extflvector? x))
      (with-mutable-check (not (immutable? x))
        fuel)]
     ;; Syntax
     [(syntax? x)
      (case stx-mode
        ((atomic) fuel)
        ((compound) (loop (syntax-e x) fuel))
        (else (bad)))]
     ;; Impersonators and chaperones
     [(and no-impersonator? (impersonator? x))  ;; else continue to chaperoned type
      (bad)]
     [(and no-impersonator? (chaperone? x))  ;; else continue to impersonated type
      (bad)]
     [else
      (with-cycle-check
       (cond
        ;; Mutable (maybe) compound
        [(vector? x)
         (with-mutable-check (not (immutable? x))
           (for/fold ([fuel fuel]) ([e (in-vector x)] #:break (not fuel))
             (loop e fuel)))]
        [(box? x)
         (with-mutable-check (not (immutable? x))
           (loop (unbox x) (sub1 fuel)))]
        [(prefab-struct-key x)
         => (lambda (key)
              (cond [(and no-mutable-hash/prefab? (mutable-prefab-key? key))
                     (bad)]
                    [else
                     ;; traverse key, since contains arbitrary auto-value
                     (let ([fuel (loop key fuel)])
                       (loop (struct->vector x) fuel))]))]
        [(hash? x)
         (cond [(and no-mutable-hash/prefab? (not (immutable? x)))
                (bad)]
               [else
                (for/fold ([fuel fuel]) ([(k v) (in-hash x)] #:break (not fuel))
                  (let ([fuel (loop k fuel)])
                    (loop v fuel)))])]
        ;; Bad
        [else
         (bad)]))]))
  (loop x0 fuel0))

;; mutable-prefab-key? : prefab-key -> boolean
(define (mutable-prefab-key? key)
  ;; A prefab-key is either
  ;;  - symbol
  ;;  - (list* symbol maybe-nat maybe-list maybe-vector prefab-key)
  ;; where mutable fields indicated by vector
  ;; This code is probably overly general; racket seems to normalize keys.
  (let loop ([k key])
    (and (pair? k)
         (or (and (vector? (car k))
                  (positive? (vector-length (car k))))
             (loop (cdr k))))))
