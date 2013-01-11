;; Implementation of SRFI 63 "Homogeneous and Heterogeneous Arrays" for PLT 
;; Scheme.
;; Copyright (C) 2007-2013 Chongkai Zhu

;; Released under the same terms as the SRFI reference implementation.

;; Parts of this file are based on SLIB "array.scm" Arrays for Scheme.
;; Copyright (C) 2001, 2003, 2005, 2006 Aubrey Jaffer

(module |63| mzscheme
  
  (require srfi/4
           mzlib/contract)
  
  (define-syntax make-cvector
    (syntax-rules ()
      ((_ make-fvector)
       (case-lambda
         ((n)
          (cons (make-fvector n)
                (make-fvector n)))
         ((n fill)
          (cons (make-fvector n (real-part fill))
                (make-fvector n (imag-part fill))))))))
  (define-syntax cvector-ref
    (syntax-rules ()
      ((_ fvector-ref)
       (lambda (v n)
         (make-rectangular (fvector-ref (car v) n)
                           (fvector-ref (cdr v) n))))))
  (define-syntax cvector-set!
    (syntax-rules ()
      ((_ fvector-set!)
       (lambda (v n z)
         (fvector-set! (car v) n (real-part z))
         (fvector-set! (cdr v) n (imag-part z))))))
  
  (define make-c64vector
    (make-cvector make-f64vector))
  (define c64vector-ref
    (cvector-ref f64vector-ref))
  (define c64vector-set!
    (cvector-set! f64vector-set!))
  (define make-c32vector
    (make-cvector make-f32vector))
  (define c32vector-ref
    (cvector-ref f32vector-ref))
  (define c32vector-set!
    (cvector-set! f32vector-set!))
  
  (define (make-floc/c _) complex?)
  (define (make-flor/c _) real?)
  (define (make-fix/c n signed?)
    (and/c exact?
           (if signed?
               (let ((x (arithmetic-shift 1 (sub1 n))))
                 (integer-in (- x) (sub1 x)))
               (integer-in 0 (sub1 (arithmetic-shift 1 n))))))
  
  (define implementations
    (let ((table
           (make-immutable-hash-table
            (list (list 'char     make-string    string-ref    string-set!    char?)
                  (list 'byte     make-bytes     bytes-ref     bytes-set!     byte?)
                  (list 'vector   make-vector    vector-ref    vector-set!    any/c)
                  (list 'bool     make-vector    vector-ref    vector-set!    boolean?)
                  (list 'floC64b  make-c64vector c64vector-ref c64vector-set! (make-floc/c 64))
                  (list 'floC32b  make-c32vector c32vector-ref c32vector-set! (make-floc/c 32))
                  (list 'floR64b  make-f64vector f64vector-ref f64vector-set! (make-flor/c 64))
                  (list 'floR32b  make-f32vector f32vector-ref f32vector-set! (make-flor/c 32))
                  (list 'fixZ64b  make-s64vector s64vector-ref s64vector-set! (make-fix/c 64 #t))
                  (list 'fixZ32b  make-s32vector s32vector-ref s32vector-set! (make-fix/c 32 #t))
                  (list 'fixZ16b  make-s16vector s16vector-ref s16vector-set! (make-fix/c 16 #t))
                  (list 'fixZ8b   make-s8vector  s8vector-ref  s8vector-set!  (make-fix/c  8 #t))
                  (list 'fixN64b  make-u64vector u64vector-ref u64vector-set! (make-fix/c 64 #f))
                  (list 'fixN32b  make-u32vector u32vector-ref u32vector-set! (make-fix/c 32 #f))
                  (list 'fixN16b  make-u16vector u16vector-ref u16vector-set! (make-fix/c 16 #f))
                  (list 'fixN8b   make-u8vector  u8vector-ref  u8vector-set!  (make-fix/c  8 #f))))))
      (lambda (type pos)
        (list-ref (hash-table-get table type) pos))))
  
  (define (array-print array port write?)
    (display "#" port)
    (display (length (my-array-ref array 0)) port)
    (display "A" port)
    (let ((type (my-array-ref array 4)))
      (unless (eq? type 'vector)
        (display ":" port)
        (display type port)))
    (display (array->list array) port))
  
  (define-values (struct:array my-make-array my-array? my-array-ref my-array-set!)
    (make-struct-type 'array #f 5 0 #f
                      (list (cons prop:custom-write array-print))
                      #f))
  
  (define (array-dimensions array)
    (cond ((vector? array) (list (vector-length array)))
          ((string? array) (list (string-length array)))
          ((bytes? array) (list (bytes-length array)))
          (else (my-array-ref array 0))))
  (define (array-scales obj)
    (if (or (string? obj)
            (bytes? obj)
            (vector? obj))
        '(1)
        (my-array-ref obj 1)))
  (define (array-offset obj)
    (if (or (string? obj)
            (bytes? obj)
            (vector? obj))
        0
        (my-array-ref obj 2)))
  (define (array-store obj)
    (if (or (string? obj)
            (bytes? obj)
            (vector? obj))
        obj
        (my-array-ref obj 3)))
  (define (array-store-type obj)
    (cond ((string? obj) 'char)
          ((bytes? obj)  'byte)
          ((vector? obj) 'vector)
          (else (my-array-ref obj 4))))
  
  (define (array-store-maker array-type)
    (implementations array-type 0))
  (define (array-store-ref array)
    (implementations (array-store-type array) 1))
  (define (array-store-set array)
    (implementations (array-store-type array) 2))
  
  (define (array? obj)
    (or (string? obj)
        (bytes? obj)
        (vector? obj)
        (my-array? obj)))
  
 (define (s:equal? obj1 obj2)
   (or (equal? obj1 obj2)
       (cond ((and (box? obj1)
                   (box? obj2))
              (s:equal? (unbox obj1)
                        (unbox obj2)))
             ((and (pair? obj1)
                   (pair? obj2))
              (and (s:equal? (car obj1) (car obj2))
                   (s:equal? (cdr obj1) (cdr obj2))))
             ((and (vector? obj1)
                   (vector? obj2))
              (and (equal? (vector-length obj1) (vector-length obj2))
                   (let lp ((idx (sub1 (vector-length obj1))))
                     (or (negative? idx)
                         (and (s:equal? (vector-ref obj1 idx)
                                        (vector-ref obj2 idx))
                              (lp (sub1 idx)))))))
             ((and (string? obj1)
                   (string? obj2))
              (string=? obj1 obj2))
             ((and (array? obj1)
                   (array? obj2))
              (and (equal? (array-dimensions obj1) (array-dimensions obj2))
                   (s:equal? (array->vector obj1) (array->vector obj2))))
             ((and (struct? obj1)
                   (struct? obj2))
              (let-values (((obj1-type obj1-skipped?)
                            (struct-info obj1))
                           ((obj2-type obj2-skipped?)
                            (struct-info obj2)))
                (and (eq? obj1-type obj2-type)
                     (not obj1-skipped?)
                     (not obj2-skipped?)
                     (s:equal? (struct->vector obj1)
                               (struct->vector obj2)))))
             (else #f))))
  
  (define (array-rank obj)
    (if (array? obj) (length (array-dimensions obj)) 0))
  
  (define (make-array prototype . dimensions)
    (let ((prot    (array-store prototype))
          (pdims   (array-dimensions prototype))
          (onedim? (eqv? 1 (length dimensions)))
          (tcnt    (apply * dimensions)))    
      (let ((initializer
             (if (zero? (apply * pdims)) '()
                 (list      ;; a list with single element at origin
                  (apply array-ref prototype
                         (map (lambda (x) 0) pdims))))))    
        
        (cond ((and onedim? (string? prot))
               (apply make-string (car dimensions) initializer))
              ((and onedim? (vector? prot))
               (apply make-vector (car dimensions) initializer))            
              (else
               (let* ((store-type (array-store-type prototype))
                      (store (apply (array-store-maker store-type)
                                    tcnt initializer)))
                 (let loop ((dims (reverse dimensions)) (scales '(1)))
                   (if (null? dims)
                       (my-make-array dimensions (cdr scales) 0 
                                      store
                                      store-type)
                       (loop (cdr dims)
                             (cons (* (car dims) (car scales)) scales))))))))))
  
  (define (make-shared-array array mapper . dimensions)
    (define odl (array-scales array))
    (define rank (length dimensions))
    (define shape
      (map (lambda (dim) (if (list? dim) dim (list 0 (sub1 dim)))) dimensions))
    
    (do ((idx (sub1 rank) (sub1 idx))
         (uvt (if (zero? rank)
                  '()
                  (append (cdr (vector->list (make-vector rank 0))) '(1)))
              (append (cdr uvt) '(0)))
         (uvts '() (cons uvt uvts)))
      ((negative? idx)
       (let ((ker0 (apply + (map * odl (apply mapper uvt)))))
         (my-make-array
          (map (lambda (dim) (add1 (- (cadr dim) (car dim)))) shape)
          (map (lambda (uvt) (- (apply + (map * odl (apply mapper uvt))) ker0))
               uvts)
          (apply +
                 (array-offset array)
                 (map * odl (apply mapper (map car shape))))
          (array-store array)
          (array-store-type array))))))
  
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
  
  (define (array-in-bounds? array . indices)
    (do ((bnds (array-dimensions array) (cdr bnds))
         (idxs indices (cdr idxs)))
      ((or (null? bnds)
           (null? idxs)
           (not (integer? (car idxs)))
           (not (< -1 (car idxs) (car bnds))))
       (and (null? bnds) (null? idxs)))))
  
  (define (array-ref array . indices)
    ((array-store-ref array)
     (array-store array)
     (apply + (array-offset array) (map * (array-scales array) indices))))
  
  (define (array-set! array obj . indices)    
    ((array-store-set array)
     (array-store array)
     (apply + (array-offset array) (map * (array-scales array) indices))
     obj))
  
  (define (tag-maker array-type)
    (case-lambda
      (()  (my-make-array 
            '(0) '(1) 0
            ((array-store-maker array-type) 0)
            array-type))
      
      ((x) (my-make-array
            '(1) '(1) 0
            ((array-store-maker array-type) 1 x)
            array-type))))
  
  (define (make-A:/c c)
    (case-> (-> any)
            (-> c any)))    
  
  (provide array?
           array-rank
           s:equal?)
  
  (provide/contract
   
   (array-dimensions (-> array? any))
   
   (make-array
    (->* (array?) (listof natural-number/c) any))
   
   (make-shared-array
    (->* (array?
          (unconstrained-domain-> (listof natural-number/c)))
         (listof natural-number/c)
         any))
   
   (list->array
    (->r ((rank natural-number/c)
          (proto array?)
          (list (if (zero? rank)
                    any/c
                    list?)))
         any))
   
   (array->list
    (-> array? any))
   (array->vector
    (-> array? any))
   
   (array-in-bounds?
    (->* (array?) (listof natural-number/c) any))
   
   (array-set!
    (->r ((array array?)
          (val (implementations (array-store-type array) 3)))
         indices (lambda _ (apply array-in-bounds? array indices))
         any))
   
   (array-ref
    (->r ((array array?)) indices 
         (lambda _ (apply array-in-bounds? array indices)) 
         any))
   
   (vector->array
    (->r ((vector vector?) (proto array?))
         dimensions (lambda _ (eqv? (vector-length vector) (apply * dimensions)))
         any)))
  
  (define-syntax A:
    (syntax-rules ()
      ((_ name label)
       (begin (define name (tag-maker label))
              (provide/contract
               (name (make-A:/c (implementations label 3))))))))
  
  (A: A:floC32b 'floC32b)
  (A: A:floR64b 'floR64b)
  (A: A:floR32b 'floR32b)
  (A: A:fixZ64b 'fixZ64b)
  (A: A:fixZ16b 'fixZ16b)
  (A: A:fixZ32b 'fixZ32b)
  (A: A:fixZ8b  'fixZ8b)
  (A: A:fixN64b 'fixN64b)
  (A: A:fixN32b 'fixN32b)
  (A: A:fixN16b 'fixN16b)
  (A: A:fixN8b  'fixN8b)
  (A: A:bool    'bool)
  
  )
