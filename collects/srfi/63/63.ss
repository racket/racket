;; Implementation of SRFI 63 "Homogeneous and Heterogeneous Arrays" for PLT 
;; Scheme.

;; Copyright (C) 2007 Chongkai Zhu

;; Released under the same terms as the SRFI reference implementation.

;; Parts of this file are based on SLIB "array.scm" Arrays for Scheme.
;; Copyright (C) 2001, 2003, 2005, 2006 Aubrey Jaffer

(module |63| mzscheme
  
  (require (lib "contract.ss"))
  
  (define-struct array:rtd
                 (dimensions
                  scales            ;list of dimension scales
                  offset            ;exact integer
                  store             ;data
                  )
                 #f)
  
  (define (array:dimensions array)
    (cond ((vector? array) (list (vector-length array)))
          ((string? array) (list (string-length array)))
          ((bytes? array) (list (bytes-length array)))
          (else (array:rtd-dimensions array))))
  
  (define (array:scales obj)
    (if (or (string? obj)
            (bytes? obj)
            (vector? obj))
        '(1)
        (array:rtd-scales obj)))
  
  (define (array:store obj)
    (if (or (string? obj)
            (bytes? obj)
            (vector? obj))
        obj
        (array:rtd-store obj)))
  
  (define (array:offset obj)
    (if (or (string? obj)
            (bytes? obj)
            (vector? obj))
        0
        (array:rtd-offset obj)))
  
  (define (array? obj)
    (or (string? obj)
        (bytes? obj)
        (vector? obj)
        (array:rtd? obj)))
  
  (define (array-rank obj)
    (if (array? obj) (length (array:dimensions obj)) 0))
  
  (define array-dimensions array:dimensions)
  
  (define (make-array prototype . dimensions)
    (define tcnt (apply * dimensions))
    (let ((store
           (cond ((string? prototype)
                  (case (string-length prototype)
                    ((0) (make-string tcnt))
                    (else (make-string tcnt
                                       (string-ref prototype 0)))))
                 ((bytes? prototype)
                  (case (bytes-length prototype)
                    ((0) (make-bytes tcnt))
                    (else (make-bytes tcnt
                                      (bytes-ref prototype 0)))))
                 (else
                  (let ((pdims (array:dimensions prototype)))
                    (case (apply * pdims)
                      ((0) (make-vector tcnt))
                      (else (make-vector tcnt
                                         (apply array-ref prototype
                                                (map (lambda (x) 0) pdims))))))))))
      (define (loop dims scales)
        (if (null? dims)
            (make-array:rtd dimensions (cdr scales) 0 store)
            (loop (cdr dims) (cons (* (car dims) (car scales)) scales))))
      (loop (reverse dimensions) '(1))))
  
  (define (make-shared-array array mapper . dimensions)
    (define odl (array:scales array))
    (define rank (length dimensions))
    (define shape
      (map (lambda (dim) (if (list? dim) dim (list 0 (sub1 dim)))) dimensions))
    (do ((idx (sub1 rank) (sub1 idx))
         (uvt (append (cdr (vector->list (make-vector rank 0))) '(1))
              (append (cdr uvt) '(0)))
         (uvts '() (cons uvt uvts)))
      ((negative? idx)
       (let ((ker0 (apply + (map * odl (apply mapper uvt)))))
         (make-array:rtd
          (map (lambda (dim) (add1 (- (cadr dim) (car dim)))) shape)
          (map (lambda (uvt) (- (apply + (map * odl (apply mapper uvt))) ker0))
               uvts)
          (apply +
                 (array:offset array)
                 (map * odl (apply mapper (map car shape))))
          (array:store array))))))
  
  (define (list->array rank proto lst)
    (define dimensions
      (do ((shp '() (cons (length row) shp))
           (row lst (car lst))
           (rnk (sub1 rank) (sub1 rnk)))
        ((negative? rnk) (reverse shp))))
    (let ((nra (apply make-array proto dimensions)))
      (define (l2ra dims idxs row)
        (cond ((null? dims)
               (apply array-set! nra row (reverse idxs)))
              ((unless (eqv? (car dims) (length row))
                 (error 'list->array
                        "non-rectangular array ~a ~a"
                        dims dimensions))
               (do ((idx 0 (add1 idx))
                    (row row (cdr row)))
                 ((>= idx (car dims)))
                 (l2ra (cdr dims) (cons idx idxs) (car row))))))
      (l2ra dimensions '() lst)
      nra))
  
  (define (array->list ra)
    (define (ra2l dims idxs)
      (if (null? dims)
          (apply array-ref ra (reverse idxs))
          (do ((lst '() (cons (ra2l (cdr dims) (cons idx idxs)) lst))
               (idx (sub1 (car dims)) (sub1 idx)))
            ((negative? idx) lst))))
    (ra2l (array-dimensions ra) '()))
  
  (define (vector->array vect prototype . dimensions)
    (define vdx (vector-length vect))
    (unless (eqv? vdx (apply * dimensions))
      (error 'vector->array
             "~a not equal to ~a" vdx (cons '* dimensions)))
    (let ((ra (apply make-array prototype dimensions)))
      (define (v2ra dims idxs)
        (cond ((null? dims)
               (set! vdx (sub1 vdx))
               (apply array-set! ra (vector-ref vect vdx) (reverse idxs)))
              (else
               (do ((idx (sub1 (car dims)) (sub1 idx)))
                 ((negative? idx) vect)
                 (v2ra (cdr dims) (cons idx idxs))))))
      (v2ra dimensions '())
      ra))
  
  (define (array->vector ra)
    (define dims (array-dimensions ra))
    (let* ((vdx (apply * dims))
           (vect (make-vector vdx)))
      (define (ra2v dims idxs)
        (if (null? dims)
            (let ((val (apply array-ref ra (reverse idxs))))
              (set! vdx (sub1 vdx))
              (vector-set! vect vdx val)
              vect)
            (do ((idx (sub1 (car dims)) (sub1 idx)))
              ((negative? idx) vect)
              (ra2v (cdr dims) (cons idx idxs)))))
      (ra2v dims '())))
  
  (define (array:in-bounds? array indices)
    (do ((bnds (array:dimensions array) (cdr bnds))
         (idxs indices (cdr idxs)))
      ((or (null? bnds)
           (null? idxs)
           (not (integer? (car idxs)))
           (not (< -1 (car idxs) (car bnds))))
       (and (null? bnds) (null? idxs)))))
  
  (define (array-in-bounds? array . indices)
    (array:in-bounds? array indices))
  
  (define (array-ref array . indices)
    (define store (array:store array))
    (or (array:in-bounds? array indices)
        (error 'array-ref "bad-indices ~a" indices))
    ((cond ((string? store)
            string-ref)
           ((bytes? store)
            bytes-ref)
           (else
            vector-ref))
     store (apply + (array:offset array) (map * (array:scales array) indices))))
  
  (define (array-set! array obj . indices)
    (define store (array:store array))
    (or (array:in-bounds? array indices)
        (error 'array-set! "bad-indices ~a" indices))
    ((cond ((string? store)
            string-set!)
           ((bytes? store)
            bytes-set!)
           (else
            vector-set!))
     store (apply + (array:offset array) (map * (array:scales array) indices))
     obj))
  
  (define A: vector)
  
  (define (make-floc/c _)
    (case-> (-> array?)
            (-> (and/c inexact? complex?) array?)))
  
  (define (make-flor/c _)
    (case-> (-> array?)
            (-> (and/c inexact? real?) array?)))
  
  (define (make-fix/c n signed?)
    (case-> 
     (-> array?)
     (-> (and/c exact? 
                (if signed?
                    (let ((x (arithmetic-shift 1 (sub1 n))))
                      (integer-in (- x) (sub1 x)))
                    (integer-in 0 (sub1 (arithmetic-shift 1 n)))))
         array?)))
  
  (provide/contract
   
   (array? (-> any/c boolean?))
   (array-rank (-> any/c natural-number/c))
   (array-dimensions (-> array? (listof natural-number/c)))
   
   (make-array 
    (->r ((proto array?)) dimensions (listof natural-number/c) array?))
   
   (make-shared-array
    (->r ((array array?) 
          (mapper procedure?))
         indices (listof natural-number/c)
         array?))
   
   (list->array
    (->r ((rank natural-number/c)
          (proto array?)
          (list (if (zero? rank)
                    any/c
                    list?)))
         array?))
   
   (array->list
    (->r ((array array?))
         (lambda (result)
           (or (zero? (array-rank array))
               (list? result)))))
   
   (array->vector
    (->r ((array array?)) vector?))
   
   (array-in-bounds?
    (->r ((array array?)) indices (listof any/c) boolean?))
   
   (array-set!
    (->r ((array array?) (_ any/c)) indices 
         (lambda _ (apply array-in-bounds? array indices)) 
         any))
   
   (array-ref
    (->r ((array array?)) indices 
         (lambda _ (apply array-in-bounds? array indices)) 
         any))
   
   (vector->array 
    (->r ((vector vector?) (proto array?)) dimensions 
         (lambda _ (eqv? (vector-length vector) (apply * dimensions)))
         any))
   
   ;; Binary flonum complex
   (rename A: A:floC128b (make-floc/c 128))
   (rename A: A:floC64b  (make-floc/c  64))
   (rename A: A:floC32b  (make-floc/c  32))
   (rename A: A:floC16b  (make-floc/c  16))
   
   ;; Binary flonum real
   (rename A: A:floR128b (make-flor/c 128))
   (rename A: A:floR64b  (make-flor/c  64))
   (rename A: A:floR32b  (make-flor/c  32))
   (rename A: A:floR16b  (make-flor/c  16))
   
   ;; Binary fixnum
   (rename A: A:fixZ64b (make-fix/c 64 #t))
   (rename A: A:fixZ32b (make-fix/c 32 #t))
   (rename A: A:fixZ16b (make-fix/c 16 #t))
   (rename A: A:fixZ8b  (make-fix/c  8 #t))
   (rename A: A:fixN64b (make-fix/c 64 #f))
   (rename A: A:fixN32b (make-fix/c 32 #f))
   (rename A: A:fixN16b (make-fix/c 16 #f))
   (rename A: A:fixN8b  (make-fix/c  8 #f))
   
   ;; Boolean
   (rename A: A:bool
    (case-> (-> array?)
            (-> boolean? array?))))
  )