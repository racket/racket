;; SRFI 63: Homogeneous and Heterogeneous Arrays
(module |63| mzscheme
  (require  (lib "4.ss" "srfi")
            (lib "9.ss" "srfi")
            (lib "16.ss" "srfi")
            (lib "contract.ss"))

  (define-record-type :strict-array ;:strict-array
    (make-strict-array
     dimensions scales offset store store-ref store-set store-make)
    strict-array?
    (dimensions strict-array-dimensions)
    (scales strict-array-scales)
    (offset strict-array-offset)
    (store strict-array-store)
    (store-ref strict-array-store-ref)    ; each array contains
    (store-set strict-array-store-set)    ; its ref, acc, and maker
    (store-make strict-array-store-make)) ; procedures for its storage type.
                                          ; maybe theres a better approach?
  
  (define (array-dimensions array)
    (cond ((vector? array) (list (vector-length array)))
          ((string? array) (list (string-length array)))
          (else (strict-array-dimensions array))))
  
  (define (array-scales array)
    (cond ((string? array) '(1))
          ((vector? array) '(1))
          (else (strict-array-scales array))))
  
  (define (array-store array)
    (cond ((string? array) array)
          ((vector? array) array)
          (else (strict-array-store array))))
  
  (define (array-store-ref array)
    (cond ((string? array) string-ref)
          ((vector? array) vector-ref)
          (else (strict-array-store-ref array))))
  
  (define (array-store-set array)
    (cond ((string? array) string-set!)
          ((vector? array) vector-set!)
          (else (strict-array-store-set array))))
  
  (define (array-store-make array)
    (cond ((string? array) make-string)
          ((vector? array) make-vector)
          (else (strict-array-store-make array))))
  
  (define (array-offset array)
    (cond ((string? array) 0)
          ((vector? array) 0)
          (else (strict-array-offset array))))
  
  (define (array? obj)
    (or (string? obj)
        (vector? obj)
        (strict-array? obj)))
  
  (define (s:equal? obj1 obj2) 
    (or (equal? obj1 obj2)
        (and (array? obj1) (array? obj2)
             (equal? (array-dimensions obj1)
                     (array-dimensions obj2))
             (s:equal? (array->vector obj1) (array->vector obj2)))))
 
  (define (array-rank x)
    (if (array? x)
        (length (array-dimensions x))
        0))
  
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
               (let ((store (apply (array-store-make prototype)
                                   tcnt initializer)))
                 (let loop ((dims (reverse dimensions)) (scales '(1)))
                   (if (null? dims)
                       (make-strict-array dimensions (cdr scales) 0
                                          store
                                          (array-store-ref prototype)
                                          (array-store-set prototype)
                                          (array-store-make prototype))
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
	 (make-strict-array
	  (map (lambda (dim) (add1 (- (cadr dim) (car dim)))) shape)
	  (map (lambda (uvt) (- (apply + (map * odl (apply mapper uvt))) ker0))
	       uvts)
	  (apply +
		 (array-offset array)
		 (map * odl (apply mapper (map car shape))))
	  (array-store array)
          (array-store-ref array)
          (array-store-set array)
          (array-store-make array))))))
  
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
              (;; ERROR CHECKING (should be a contract)
               (if (not (eqv? (car dims) (length row)))
                   (error "non-rectangular array" dims dimensions))
               
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
    (let ((vdx (vector-length vect))
          (ra (apply make-array prototype dimensions)))
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
              (vector-set! vect vdx val))
            (do ((idx (sub1 (car dims)) (sub1 idx)))
	      ((negative? idx) vect)
              (ra2v (cdr dims) (cons idx idxs)))))
      (ra2v dims '())
      vect))
  
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
  
  (define (tag-maker make-tagvector tagvector-ref tagvector-set)
    (case-lambda
      (()  (make-strict-array
            '(1) '(1) 0 (make-tagvector 1 0)
            tagvector-ref tagvector-set make-tagvector))
      ((x) (make-strict-array
            '(1) '(1) 0 (make-tagvector 1 x)
            tagvector-ref tagvector-set make-tagvector))))
  
  (define a:fixz8b  (tag-maker make-s8vector s8vector-ref s8vector-set!))
  (define a:fixz16b (tag-maker make-s16vector s16vector-ref s16vector-set!))
  (define a:fixz32b (tag-maker make-s32vector s32vector-ref s32vector-set!))
  (define a:fixz64b (tag-maker make-s64vector s64vector-ref s64vector-set!))
  (define a:fixn8b  (tag-maker make-u8vector u8vector-ref u8vector-set!))
  (define a:fixn16b (tag-maker make-u16vector u16vector-ref u16vector-set!))
  (define a:fixn32b (tag-maker make-u32vector u32vector-ref u32vector-set!))
  (define a:fixn64b (tag-maker make-u64vector u64vector-ref u64vector-set!))
  
  (define a:floc32b (tag-maker make-f32vector f32vector-ref f32vector-set!))
  (define a:floc64b (tag-maker make-f64vector f64vector-ref f64vector-set!))
  
  ;; Don't have anything better to do in these cases.
  
  (define (vector-maker)
    (case-lambda
      (()  (vector))
      ((x) (vector x))))  
  
  (define a:floc16b  (vector-maker))
  (define a:floc128b (vector-maker))
  
  (define a:flor16b  (vector-maker))
  (define a:flor32b  (vector-maker))
  (define a:flor64b  (vector-maker))
  (define a:flor128b (vector-maker))
  
  (define a:floq128d (vector-maker))
  (define a:floq64d  (vector-maker))
  (define a:floq32d  (vector-maker))
  
  (define a:bool (vector-maker))
  
  (provide array? s:equal? array-rank array-dimensions 
           make-array make-shared-array 
           list->array array->list array->vector
           array-in-bounds?
           
           a:fixz8b a:fixz16b a:fixz32b a:fixz64b 
           a:fixn8b a:fixn16b a:fixn32b a:fixn64b
           
           a:floc16b a:floc32b a:floc64b  a:floc128b
           a:flor16b a:flor32b a:flor64b  a:flor128b
           
           a:floq32d a:floq64d  a:floq128d
           
           a:bool)
  
  (provide/contract
   (array-set!
    (->r ((a array?) (_ any/c)) indices 
         (lambda _ (apply array-in-bounds? a indices)) any))
   
   (array-ref
    (->r ((a array?)) indices 
         (lambda _ (apply array-in-bounds? a indices)) any))
   
   (vector->array 
    (->r ((v vector?) (p array?)) dimensions
         (lambda _ (eqv? (vector-length v) (apply * dimensions))) any)))
    
  ) ; end of module |63|