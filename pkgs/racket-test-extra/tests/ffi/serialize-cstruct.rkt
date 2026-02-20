#lang racket/base
(require rackunit
         racket/serialize
         ffi/unsafe
         ffi/serialize-cstruct)

(define-serializable-cstruct _point ([x _double]
                                     [y _int]))

(check-equal? (point-x (deserialize (serialize (make-point 1.0 2))))
              1.0)
(check-equal? (point-y (deserialize (serialize (make-point 1.1 3))))
              3)


(define-serializable-cstruct _fish ([color _int])
  #:version 1)

(check-equal? (fish-color (deserialize (serialize (make-fish 4))))
              4)

(define-serializable-cstruct _aq ([a _fish-pointer]
                                  [d (_gcable _aq-pointer/null)])
  #:malloc-mode 'nonatomic)

(define aq1 (make-aq/mode (make-fish 6) #f))
(set-aq-d! aq1 aq1)
(collect-garbage)
(check-equal? (let ([a (deserialize (serialize aq1))])
                (list (ptr-equal? a (aq-d a))
                      (fish-color (aq-a (aq-d a)))))
              (list #t 6))

(define old-pond
  (parameterize ([current-namespace (make-base-namespace)])
    (eval
     '(module pond racket/base
       (require ffi/unsafe
                ffi/serialize-cstruct)
       (provide make-pond)
       (define-serializable-cstruct _pond ([depth _float]))))
    (eval '(require 'pond racket/serialize))
    (eval '(serialize (make-pond 8.0)))))

(check-equal?
 (parameterize ([current-namespace (make-base-namespace)])
   (eval
    '(module pond racket/base
      (require ffi/unsafe
               ffi/serialize-cstruct)
      (provide pond-depth)
      (define-serializable-cstruct _old-pond ([depth _float]))
      (define-serializable-cstruct _pond ([num-fish _int]
                                          [depth _float])
        #:version 1
        #:other-versions ([0 deserialize-chain:cstruct:old-pond
                             (lambda (op)
                               (make-pond 0 (old-pond-depth op)))
                             (lambda (p)
                               (make-old-pond (pond-depth p)))
                             (lambda ()
                               (define p (make-pond 0 0))
                               (values p
                                       (lambda (op)
                                         (set-pond-depth! p (old-pond-depth op)))))]))))
   (eval '(require 'pond racket/serialize))
   (eval `(pond-depth (deserialize ',old-pond))))
 8.0)


(collect-garbage)
(define old-aq
  (parameterize ([current-namespace (make-base-namespace)])
    (eval
     '(module aq racket/base
       (require ffi/unsafe
                ffi/serialize-cstruct)
       (provide make-aq/mode make-fish set-aq-d!)
       (define-serializable-cstruct _fish ([color _int]))
       (define-serializable-cstruct _aq ([a _fish-pointer]
                                         [d (_gcable _aq-pointer/null)])
         #:malloc-mode 'nonatomic)))
    (eval '(require 'aq racket/serialize))
    (eval '(serialize
            (let ([aq1 (make-aq/mode (make-fish 7) #f)])
              (set-aq-d! aq1 aq1)
              aq1)))))

(check-equal?
 (parameterize ([current-namespace (make-base-namespace)])
   (eval
    '(module aq racket/base
      (require ffi/unsafe
               ffi/serialize-cstruct)
      (provide aq-a aq-b aq-d
               fish-color)
      (define-serializable-cstruct _fish ([color _int]))
      (define-serializable-cstruct _old-aq ([a _fish-pointer]
                                            [d _pointer])
        #:malloc-mode 'nonatomic)
      (define-serializable-cstruct _aq ([a _fish-pointer]
                                        [b _fish-pointer]
                                        [d (_gcable _aq-pointer/null)])
        #:malloc-mode 'nonatomic
        #:version 1
        #:other-versions ([0 deserialize-chain:cstruct:old-aq
                             (lambda (oa)
                               (make-aq/mode (old-aq-a oa)
                                             (old-aq-a oa)
                                             (cast (old-aq-d oa) _pointer _aq-pointer)))
                             (lambda (a)
                               (make-old-aq/mode (aq-a a)
                                                 (aq-d a)))
                             (lambda ()
                               (define tmp-fish (make-fish 0))
                               (define a (make-aq/mode tmp-fish tmp-fish #f))
                               (values a
                                       (lambda (oa)
                                         (set-aq-a! a (old-aq-a oa))
                                         (set-aq-b! a (old-aq-a oa))
                                         (set-aq-d! a (cast (old-aq-d oa) _pointer _aq-pointer)))))]))))
   (eval '(require 'aq racket/serialize))
   (eval `(fish-color (aq-b (aq-d (deserialize ',old-aq))))))
 7)

; Regression test: without _gcable on a pointer field in a nonatomic cstruct,
; GC can move the struct while a non-gcable wrapper still points to the old
; address, producing garbage data or crashing with "invalid memory reference".
; The _gcable annotation ensures the wrapper is GC-traced and updated when the
; struct moves.
(define-serializable-cstruct _rfish ([color _int]))
(define-serializable-cstruct _raq ([a _rfish-pointer]
                                   [d (_gcable _raq-pointer/null)])
  #:malloc-mode 'nonatomic)

(for ([_ (in-range 20)])
  ;; Allocate garbage to pressure GC into moving the struct
  (for ([__ (in-range 100)])
    (make-raq/mode (make-rfish 0) #f))
  (define raq1 (make-raq/mode (make-rfish 42) #f))
  (set-raq-d! raq1 raq1)
  ;; Save a wrapper, then force GC which may move the struct
  (define wrapper (raq-d raq1))
  (collect-garbage)
  ;; Wrapper must still read correct data (not garbage from stale address)
  (check-equal? (rfish-color (raq-a wrapper)) 42)
  ;; Serialize/deserialize must preserve the cycle
  (check-equal?
   (let ([a (deserialize (serialize raq1))])
     (list (ptr-equal? a (raq-d a))
           (rfish-color (raq-a a))))
   (list #t 42)))
