;;; Copyright (C) 2005-2012 by Chongkai Zhu.
;;; Distributed under the same terms as Racket, by permission.

(module vector-lib mzscheme
  
  (require srfi/8/receive
           mzlib/etc
           mzlib/contract)
  
  (define mutable-vector/c
    (and/c vector? (not/c immutable?)))
  (define index/c
    (and/c natural-number/c
           exact?))
  
  (define (vec-start-end-contract vector?)
    (case->
     (-> vector? any)
     (->r ((vec vector?)
           (start (and/c index/c
                         (<=/c (vector-length vec)))))
          any)
     (->pp ((vec vector?)
            (start index/c)
            (end index/c))
           (<= start end (vector-length vec))
           any)))
  
  ;;; (%SMALLEST-LENGTH <vector-list> <default-length>)
  ;;;       -> exact, nonnegative integer
  ;;;   Compute the smallest length of VECTOR-LIST.  DEFAULT-LENGTH is
  ;;;   the length that is returned if VECTOR-LIST is empty.  Common use
  ;;;   of this is in n-ary vector routines:
  ;;;     (define (f vec . vectors)
  (define (%smallest-length vector-list length)
    (if (null? vector-list)
        length
        (%smallest-length (cdr vector-list)
                          (min length
                               (vector-length (car vector-list))))))
  
  (define (vectors-ref vectors i)
    (map (lambda (v) (vector-ref v i)) vectors))
  
  ;;; from vector-unfold-right
  (define (unfold1! f vec i seed)
    (if (>= i 0)
        (receive (elt new-seed)
                 (f i seed)
                 (vector-set! vec i elt)
                 (unfold1! f vec (sub1 i) new-seed))))
  
  (define unfold-contract
    (->r ((f (lambda (f)
               (and (procedure? f)
                    (procedure-arity-includes? f (add1 (length seeds))))))
          (len index/c))
         seeds list?
         any))
  
  (define copy-contract
    (case->
     (-> vector? any)
     (->r ((vec vector?)
           (start (and/c index/c
                         (<=/c (vector-length vec)))))
          any)
     (->r ((vec vector?)
           (start (and/c index/c
                         (<=/c (vector-length vec))))
           (end (and/c index/c
                       (>=/c start))))
          any)
     (->r ((vec vector?)
           (start (and/c index/c
                         (<=/c (vector-length vec))))
           (end (and/c index/c
                       (>=/c start)))
           (fill any/c))
          any)))
  
  (provide/contract (vector-unfold unfold-contract)
                    (vector-unfold-right unfold-contract)
                    (vector-copy copy-contract)
                    (vector-reverse-copy (vec-start-end-contract vector?))
                    (vector-append (->* () (listof vector?) any))
                    (vector-concatenate (-> (listof vector?) any)))
  
  ;;; (VECTOR-UNFOLD <f> <length> <initial-seed> ...) -> vector
  ;;;     (F <index> <seed> ...) -> [elt seed' ...]
  ;;;   The fundamental vector constructor.  Creates a vector whose
  ;;;   length is LENGTH and iterates across each index K between 0 and
  ;;;   LENGTH, applying F at each iteration to the current index and the
  ;;;   current seeds to receive N+1 values: first, the element to put in
  ;;;   the Kth slot and then N new seeds for the next iteration.
  (define vector-unfold
    (letrec ((tabulate!                   ; Special zero-seed case.
              (lambda (f vec i len)
                (cond ((< i len)
                       (vector-set! vec i (f i))
                       (tabulate! f vec (add1 i) len)))))
             (unfold1!                    ; Fast path for one seed.
              (lambda (f vec i len seed)
                (if (< i len)
                    (receive (elt new-seed)
                             (f i seed)
                             (vector-set! vec i elt)
                             (unfold1! f vec (add1 i) len new-seed)))))
             (unfold2+!                   ; Slower variant for N seeds.
              (lambda (f vec i len seeds)
                (if (< i len)
                    (receive (elt . new-seeds)
                             (apply f i seeds)
                             (vector-set! vec i elt)
                             (unfold2+! f vec (add1 i) len new-seeds))))))
      (lambda (f len . initial-seeds)
        (let ((vec (make-vector len)))
          (cond ((null? initial-seeds)
                 (tabulate! f vec 0 len))
                ((null? (cdr initial-seeds))
                 (unfold1! f vec 0 len (car initial-seeds)))
                (else
                 (unfold2+! f vec 0 len initial-seeds)))
          vec))))
  
  ;;; (VECTOR-UNFOLD-RIGHT <f> <length> <initial-seed> ...) -> vector
  ;;;     (F <seed> ...) -> [seed' ...]
  ;;;   Like VECTOR-UNFOLD, but it generates elements from LENGTH to 0
  ;;;   (still exclusive with  LENGTH and inclusive with 0), not 0 to
  ;;;   LENGTH as with VECTOR-UNFOLD.
  (define vector-unfold-right
    (letrec ((tabulate!
              (lambda (f vec i)
                (cond ((>= i 0)
                       (vector-set! vec i (f i))
                       (tabulate! f vec (sub1 i))))))
             (unfold2+!
              (lambda (f vec i seeds)
                (if (>= i 0)
                    (receive (elt . new-seeds)
                             (apply f i seeds)
                             (vector-set! vec i elt)
                             (unfold2+! f vec (sub1 i) new-seeds))))))
      (lambda (f len . initial-seeds)
        (let ((vec (make-vector len))
              (i (sub1 len)))
          (cond ((null? initial-seeds)
                 (tabulate! f vec i))
                ((null? (cdr initial-seeds))
                 (unfold1!  f vec i (car initial-seeds)))
                (else
                 (unfold2+! f vec i initial-seeds)))
          vec))))
  
  ;;; (VECTOR-COPY <vector> [<start> <end> <fill>]) -> vector
  ;;;   Create a newly allocated vector containing the elements from the
  ;;;   range [START,END) in VECTOR.  START defaults to 0; END defaults
  ;;;   to the length of VECTOR.  END may be greater than the length of
  ;;;   VECTOR, in which case the vector is enlarged; if FILL is passed,
  ;;;   the new locations from which there is no respective element in
  ;;;   VECTOR are filled with FILL.
  (define vector-copy
    (opt-lambda (vec (start 0) (end (vector-length vec)) (fill 0))
      (let ((new-vector
             (make-vector (- end start) fill)))
        (vector-copy! new-vector 0
                      vec        start
                      (min end (vector-length vec)))
        new-vector)))
  
  ;;; (VECTOR-REVERSE-COPY <vector> [<start> <end>]) -> vector
  ;;;   Create a newly allocated vector whose elements are the reversed
  ;;;   sequence of elements between START and END in VECTOR.  START's
  ;;;   default is 0; END's default is the length of VECTOR.
  (define vector-reverse-copy
    (opt-lambda (vec (start 0) (end (vector-length vec)))
      (let ((new (make-vector (- end start))))
        (vector-reverse-copy! new 0 vec start end)
        new)))
  
  ;;; (VECTOR-APPEND <vector> ...) -> vector
  ;;;   Append VECTOR ... into a newly allocated vector and return that
  ;;;   new vector.
  (define (vector-append . vectors)
    (vector-concatenate vectors))
  
  ;;; (VECTOR-CONCATENATE <vector-list>) -> vector
  ;;;   Concatenate the vectors in VECTOR-LIST.  This is equivalent to
  ;;;     (apply vector-append VECTOR-LIST)
  ;;; Actually, they're both implemented in terms of an internal routine.
  (define vector-concatenate
    (letrec ((compute-length
              (lambda (vectors len)
                (if (null? vectors)
                    len
                    (let ((vec (car vectors)))
                      (compute-length (cdr vectors)
                                      (+ (vector-length vec) len))))))
             (concatenate!
              (lambda (vectors target to)
                (if (null? vectors)
                    target
                    (let* ((vec1 (car vectors))
                           (len (vector-length vec1)))
                      (vector-copy! target to vec1 0 len)
                      (concatenate! (cdr vectors) target
                                    (+ to len)))))))
      (lambda (vectors)
        (let ((new-vector
               (make-vector (compute-length vectors 0))))
          (concatenate! vectors new-vector 0)
          new-vector))))
  
  (provide/contract (vector-empty?
                     (-> vector? any))
                    (vector=
                     (->* ((-> any/c any/c any))
                          (listof vector?)
                          any)))
  
  ;;; (VECTOR-EMPTY? <vector>) -> boolean
  ;;;   Return #T if VECTOR has zero elements in it, i.e. VECTOR's length
  ;;;   is 0, and #F if not.
  (define (vector-empty? vec)
    (zero? (vector-length vec)))
  
  ;;; (VECTOR= <elt=?> <vector> ...) -> boolean
  ;;;     (ELT=? <value> <value>) -> boolean
  ;;;   Determine vector equality generalized across element comparators.
  ;;;   Vectors A and B are equal iff their lengths are the same and for
  ;;;   each respective elements E_a and E_b (element=? E_a E_b) returns
  ;;;   a true value.  ELT=? is always applied to two arguments.  Element
  ;;;   comparison must be consistent with EQ?; that is, if (eq? E_a E_b)
  ;;;   results in a true value, then (ELEMENT=? E_a E_b) must result in a
  ;;;   true value.  This may be exploited to avoid multiple unnecessary
  ;;;   element comparisons.  (This implementation does, but does not deal
  ;;;   with the situation that ELEMENT=? is EQ? to avoid more unnecessary
  ;;;   comparisons, but I believe this optimization is probably fairly
  ;;;   insignificant.)
  ;;;   
  ;;;   If the number of vector arguments is zero or one, then #T is
  ;;;   automatically returned.  If there are N vector arguments,
  ;;;   VECTOR_1 VECTOR_2 ... VECTOR_N, then VECTOR_1 & VECTOR_2 are
  ;;;   compared; if they are equal, the vectors VECTOR_2 ... VECTOR_N
  ;;;   are compared.  The precise order in which ELT=? is applied is not
  ;;;   specified.
  (define (vector= elt=? . vectors)
    (or (null? vectors)
        (null? (cdr vectors))
        (let loop ((vecs vectors))
          (let ((vec1 (car vecs))
                (vec2+ (cdr vecs)))
            (or (null? vec2+)
                (and (binary-vector= elt=? vec1 (car vec2+))
                     (loop vec2+)))))))
  (define (binary-vector= elt=? vector-a vector-b)
    (or (eq? vector-a vector-b)           ;+++
        (let ((length-a (vector-length vector-a)))
          (and (= length-a (vector-length vector-b))
               (let loop ((i 0))
                 (or (= i length-a)
                     (and (elt=? (vector-ref vector-a i)
                                 (vector-ref vector-b i))
                          (loop (add1 i)))))))))
  
  (define fold-contract
    (->r ((kons (lambda (f)
                  (and (procedure? f)
                       (procedure-arity-includes? f (+ 3 (length vec))))))
          (knil any/c)
          (vec1 vector?))
         vec (listof vector?)
         any))
  
  (define (map-contract m)
    (->r ((f (lambda (f)
               (and (procedure? f)
                    (procedure-arity-includes? f (+ 2 (length vec))))))
          (vec1 m))
         vec (listof vector?)
         any))
  
  (provide/contract (vector-fold fold-contract)
                    (vector-fold-right fold-contract)
                    (vector-map (map-contract vector?))
                    (vector-map! (map-contract mutable-vector/c))
                    (vector-for-each (map-contract vector?))
                    (vector-count (map-contract vector?)))
  
  ;;; (VECTOR-FOLD <kons> <initial-knil> <vector> ...) -> knil
  ;;;     (KONS <knil> <elt> ...) -> knil' ; N vectors -> N+1 args
  ;;;   The fundamental vector iterator.  KONS is iterated over each
  ;;;   index in all of the vectors in parallel, stopping at the end of
  ;;;   the shortest; KONS is applied to an argument list of (list I
  ;;;   STATE (vector-ref VEC I) ...), where STATE is the current state
  ;;;   value -- the state value begins with KNIL and becomes whatever
  ;;;   KONS returned at the respective iteration --, and I is the
  ;;;   current index in the iteration.  The iteration is strictly left-
  ;;;   to-right.
  ;;;     (vector-fold KONS KNIL (vector E_1 E_2 ... E_N))
  ;;;       <=>
  ;;;     (KONS (... (KONS (KONS KNIL E_1) E_2) ... E_N-1) E_N)
  (define (vector-fold kons knil vec . vectors)
    (if (null? vectors)
        (%vector-fold1 kons knil (vector-length vec) vec)
        (%vector-fold2+ kons knil
                        (%smallest-length vectors
                                          (vector-length vec))
                        (cons vec vectors))))
  
  (define %vector-fold1
    (letrec ((loop (lambda (kons knil len vec i)
                     (if (= i len)
                         knil
                         (loop kons
                               (kons i knil (vector-ref vec i))
                               len vec (add1 i))))))
      (lambda (kons knil len vec)
        (loop kons knil len vec 0))))
  (define %vector-fold2+
    (letrec ((loop (lambda (kons knil len vectors i)
                     (if (= i len)
                         knil
                         (loop kons
                               (apply kons i knil
                                      (vectors-ref vectors i))
                               len vectors (add1 i))))))
      (lambda (kons knil len vectors)
        (loop kons knil len vectors 0))))
  
  ;;; (VECTOR-COUNT <predicate?> <vector> ...)
  ;;;       -> exact, nonnegative integer
  ;;;     (PREDICATE? <index> <value> ...) ; N vectors -> N+1 args
  ;;;   PREDICATE? is applied element-wise to the elements of VECTOR ...,
  ;;;   and a count is tallied of the number of elements for which a
  ;;;   true value is produced by PREDICATE?.  This count is returned.
  (define (vector-count pred? vec . vectors)
    (if (null? vectors)
        (%vector-fold1 (lambda (index count elt)
                         (if (pred? index elt)
                             (add1 count)
                             count))
                       0
                       (vector-length vec)
                       vec)
        (%vector-fold2+ (lambda (index count . elts)
                          (if (apply pred? index elts)
                              (add1 count)
                              count))
                        0
                        (%smallest-length vectors
                                          (vector-length vec))
                        (cons vec vectors))))
  
  ;;; (VECTOR-FOLD-RIGHT <kons> <initial-knil> <vector> ...) -> knil
  ;;;     (KONS <knil> <elt> ...) -> knil' ; N vectors => N+1 args
  ;;;   The fundamental vector recursor.  Iterates in parallel across
  ;;;   VECTOR ... right to left, applying KONS to the elements and the
  ;;;   current state value; the state value becomes what KONS returns
  ;;;   at each next iteration.  KNIL is the initial state value.
  ;;;     (vector-fold-right KONS KNIL (vector E_1 E_2 ... E_N))
  ;;;       <=>
  ;;;     (KONS (... (KONS (KONS KNIL E_N) E_N-1) ... E_2) E_1)
  ;;;
  ;;; Not implemented in terms of a more primitive operations that might
  ;;; called %VECTOR-FOLD-RIGHT due to the fact that it wouldn't be very
  ;;; useful elsewhere.
  (define vector-fold-right
    (letrec ((loop1 (lambda (kons knil vec i)
                      (if (zero? i)
                          knil
                          (let ((j (sub1 i)))
                            (loop1 kons
                                   (kons j knil (vector-ref vec j))
                                   vec
                                   j)))))
             (loop2+ (lambda (kons knil vectors i)
                       (if (zero? i)
                           knil
                           (let ((j (sub1 i)))
                             (loop2+ kons
                                     (apply kons j knil
                                            (vectors-ref vectors j))
                                     vectors
                                     j))))))
      (lambda (kons knil vec . vectors)
        (if (null? vectors)
            (loop1 kons knil vec (vector-length vec))
            (loop2+ kons knil (cons vec vectors)
                    (%smallest-length vectors
                                      (vector-length vec)))))))
  
  ;;; (VECTOR-MAP <f> <vector> ...) -> vector
  ;;;     (F <elt> ...) -> value ; N vectors -> N args
  ;;;   Constructs a new vector of the shortest length of the vector
  ;;;   arguments.  Each element at index I of the new vector is mapped
  ;;;   from the old vectors by (F I (vector-ref VECTOR I) ...).  The
  ;;;   dynamic order of application of F is unspecified.
  (define (vector-map f vec . vectors)
    (if (null? vectors)
        (let ((len (vector-length vec)))
          (%vector-map1! f (make-vector len) vec len))
        (let ((len (%smallest-length vectors
                                     (vector-length vec))))
          (%vector-map2+! f (make-vector len)
                          (cons vec vectors) len))))
  
  ;;; (%VECTOR-MAP1! <f> <target> <length> <vector>)
  ;;;     (F <index> <elt>) -> elt'
  (define (%vector-map1! f target vec i)
    (if (zero? i)
        target
        (let ((j (sub1 i)))
          (vector-set! target j
                       (f j (vector-ref vec j)))
          (%vector-map1! f target vec j))))
  (define (%vector-map2+! f target vectors i)
    (if (zero? i)
        target
        (let ((j (sub1 i)))
          (vector-set! target j
                       (apply f j (vectors-ref vectors j)))
          (%vector-map2+! f target vectors j))))
  
  ;;; (VECTOR-MAP! <f> <vector> ...) -> vector
  ;;;     (F <elt> ...) -> element' ; N vectors -> N args
  ;;;   Similar to VECTOR-MAP, but rather than mapping the new elements
  ;;;   into a new vector, the new mapped elements are destructively
  ;;;   inserted into the first vector.  Again, the dynamic order of
  ;;;   application of F is unspecified, so it is dangerous for F to
  ;;;   manipulate the first VECTOR.
  (define (vector-map! f vec . vectors)
    (if (null? vectors)
        (%vector-map1! f vec vec (vector-length vec))
        (%vector-map2+! f vec (cons vec vectors)
                        (%smallest-length vectors
                                          (vector-length vec)))))
  
  ;;; (VECTOR-FOR-EACH <f> <vector> ...) -> void
  ;;;     (F <elt> ...) ; N vectors -> N args
  ;;;   Simple vector iterator: applies F to each index in the range [0,
  ;;;   LENGTH), where LENGTH is the length of the smallest vector
  ;;;   argument passed, and the respective element at that index.  In
  ;;;   contrast with VECTOR-MAP, F is reliably applied to each
  ;;;   subsequent elements, starting at index 0 from left to right, in
  ;;;   the vectors.
  (define vector-for-each
    (letrec ((for-each1
              (lambda (f vec i len)
                (when (< i len)
                  (f i (vector-ref vec i))
                  (for-each1 f vec (add1 i) len))))
             (for-each2+
              (lambda (f vecs i len)
                (when (< i len)
                  (apply f i (vectors-ref vecs i))
                  (for-each2+ f vecs (add1 i) len)))))
      (lambda (f vec . vectors)
        (if (null? vectors)
            (for-each1 f vec 0 (vector-length vec))
            (for-each2+ f (cons vec vectors) 0
                        (%smallest-length vectors
                                          (vector-length vec)))))))
  
  (define index-contract
    (->r ((f (lambda (f)
               (and (procedure? f)
                    (procedure-arity-includes? f (add1 (length vec))))))
          (vec1 vector?))
         vec (listof vector?)
         any))
  
  (provide/contract (vector-index index-contract)
                    (vector-index-right index-contract)
                    (vector-skip index-contract)
                    (vector-skip-right index-contract)
                    (vector-binary-search
                     (-> vector? any/c
                         (-> any/c any/c real?)
                         any))
                    (vector-any index-contract)
                    (vector-every index-contract))
  
  ;; All the functions (except vector-binary-search) here can be 
  ;; abstracted, but for performance I didn't do so.
  
  ;;; (VECTOR-INDEX <predicate?> <vector> ...)
  ;;;       -> exact, nonnegative integer or #F
  ;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
  ;;;   Search left-to-right across VECTOR ... in parallel, returning the
  ;;;   index of the first set of values VALUE ... such that (PREDICATE?
  ;;;   VALUE ...) returns a true value; if no such set of elements is
  ;;;   reached, return #F.
  (define vector-index
    (letrec ((loop1  (lambda (pred? vec len i)
                       (cond ((= i len) #f)
                             ((pred? (vector-ref vec i)) i)
                             (else (loop1 pred? vec len (add1 i))))))
             (loop2+ (lambda (pred? vectors len i)
                       (cond ((= i len) #f)
                             ((apply pred? (vectors-ref vectors i)) i)
                             (else (loop2+ pred? vectors len (add1 i)))))))
      (lambda (pred? vec . vectors)
        (if (null? vectors)
            (loop1 pred? vec (vector-length vec) 0)
            (loop2+ pred? (cons vec vectors)
                    (%smallest-length vectors
                                      (vector-length vec))
                    0)))))
  
  ;;; (VECTOR-SKIP <predicate?> <vector> ...)
  ;;;       -> exact, nonnegative integer or #F
  ;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
  ;;;   (vector-index (lambda elts (not (apply PREDICATE? elts)))
  ;;;                 VECTOR ...)
  ;;;   Like VECTOR-INDEX, but find the index of the first set of values
  ;;;   that do _not_ satisfy PREDICATE?.
  (define vector-skip
    (letrec ((loop1  (lambda (pred? vec len i)
                       (cond ((= i len) #f)
                             ((pred? (vector-ref vec i))
                              (loop1 pred? vec len (add1 i)))
                             (else i))))
             (loop2+ (lambda (pred? vectors len i)
                       (cond ((= i len) #f)
                             ((apply pred? (vectors-ref vectors i))
                              (loop2+ pred? vectors len (add1 i)))
                             (else i)))))
      (lambda (pred? vec . vectors)
        (if (null? vectors)
            (loop1 pred? vec (vector-length vec) 0)
            (loop2+ pred? (cons vec vectors)
                    (%smallest-length vectors
                                      (vector-length vec))
                    0)))))
  
  ;;; (VECTOR-INDEX-RIGHT <predicate?> <vector> ...)
  ;;;       -> exact, nonnegative integer or #F
  ;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
  ;;;   Right-to-left variant of VECTOR-INDEX.
  (define vector-index-right
    (letrec ((loop1  (lambda (pred? vec i)
                       (if (zero? i)
                           #f
                           (let ((i (sub1 i)))
                             (if (pred? (vector-ref vec i))
                                 i
                                 (loop1 pred? vec i))))))
             (loop2+ (lambda (pred? vectors i)
                       (if (zero? i)
                           #f
                           (let ((i (sub1 i)))
                             (if (apply pred? (vectors-ref vectors i))
                                 i
                                 (loop2+ pred? vectors i)))))))
      (lambda (pred? vec . vectors)
        (if (null? vectors)
            (loop1 pred? vec (vector-length vec))
            (loop2+ pred? (cons vec vectors)
                    (%smallest-length vectors
                                      (vector-length vec)))))))
  
  ;;; (VECTOR-SKIP-RIGHT <predicate?> <vector> ...)
  ;;;       -> exact, nonnegative integer or #F
  ;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
  ;;;   Right-to-left variant of VECTOR-SKIP.
  (define vector-skip-right
    (letrec ((loop1  (lambda (pred? vec i)
                       (if (zero? i)
                           #f
                           (let ((i (sub1 i)))
                             (if (pred? (vector-ref vec i))
                                 (loop1 pred? vec i)
                                 i)))))
             (loop2+ (lambda (pred? vectors i)
                       (if (zero? i)
                           #f
                           (let ((i (sub1 i)))
                             (if (apply pred? (vectors-ref vectors i))
                                 (loop2+ pred? vectors i)
                                 i))))))
      (lambda (pred? vec . vectors)
        (if (null? vectors)
            (loop1 pred? vec (vector-length vec))
            (loop2+ pred? (cons vec vectors)
                    (%smallest-length vectors
                                      (vector-length vec)))))))
  
  ;;; (VECTOR-BINARY-SEARCH <vector> <value> <cmp>)
  ;;;       -> exact, nonnegative integer or #F
  ;;;     (CMP <value1> <value2>) -> integer
  ;;;       positive -> VALUE1 > VALUE2
  ;;;       zero     -> VALUE1 = VALUE2
  ;;;       negative -> VALUE1 < VALUE2
  ;;;   Perform a binary search through VECTOR for VALUE, comparing each
  ;;;   element to VALUE with CMP.
  (define (vector-binary-search vec value cmp)
    (let loop ((start 0)
               (end (vector-length vec)))
      (if (= start end)
          #f
          (let* ((i (quotient (+ start end) 2))
                 (comparison (cmp (vector-ref vec i) value)))
            (cond ((zero?     comparison) i)
                  ((positive? comparison) (loop start i))
                  (else                   (loop (add1 i) end)))))))
  
  ;;; (VECTOR-ANY <pred?> <vector> ...) -> value
  ;;;   Apply PRED? to each parallel element in each VECTOR ...; if PRED?
  ;;;   should ever return a true value, immediately stop and return that
  ;;;   value; otherwise, when the shortest vector runs out, return #F.
  ;;;   The iteration and order of application of PRED? across elements
  ;;;   is of the vectors is strictly left-to-right.
  (define vector-any
    (letrec ((loop1 (lambda (pred? vec i len)
                      (and (not (= i len))
                           (or (pred? (vector-ref vec i))
                               (loop1 pred? vec (add1 i) len)))))
             (loop2+ (lambda (pred? vectors i len)
                       (and (not (= i len))
                            (or (apply pred? (vectors-ref vectors i))
                                (loop2+ pred? vectors (add1 i) len))))))
      (lambda (pred? vec . vectors)
        (if (null? vectors)
            (loop1 pred? vec 0 (vector-length vec))
            (loop2+ pred? (cons vec vectors)
                    0 (%smallest-length vectors
                                        (vector-length vec)))))))
  
  ;;; (VECTOR-EVERY <pred?> <vector> ...) -> value
  ;;;   Apply PRED? to each parallel value in each VECTOR ...; if PRED?
  ;;;   should ever return #F, immediately stop and return #F; otherwise,
  ;;;   if PRED? should return a true value for each element, stopping at
  ;;;   the end of the shortest vector, return the last value that PRED?
  ;;;   returned.  In the case that there is an empty vector, return #T.
  ;;;   The iteration and order of application of PRED? across elements
  ;;;   is of the vectors is strictly left-to-right.
  (define vector-every
    (letrec ((loop1 (lambda (pred? vec i len)
                      (or (> i len)
                          (if (= i len)
                              (pred? (vector-ref vec i))
                              (and (pred? (vector-ref vec i))
                                   (loop1 pred? vec (add1 i) len))))))
             (loop2+ (lambda (pred? vectors i len)
                       (or (> i len)
                           (if (= i len)
                               (apply pred? (vectors-ref vectors i))
                               (and (apply pred? (vectors-ref vectors i))
                                    (loop2+ pred? vectors (add1 i) len)))))))
      (lambda (pred? vec . vectors)
        (if (null? vectors)
            (loop1 pred? vec 0 (sub1 (vector-length vec)))
            (loop2+ pred?
                    (cons vec vectors)
                    0
                    (sub1
                     (%smallest-length vectors
                                       (vector-length vec))))))))
  
  (define copy!-contract
    (case->
     (->r ((target mutable-vector/c)
           (tstart (and/c index/c
                          (<=/c (- (vector-length target)
                                   (vector-length source)))))
           (source vector?))
          any)
     (->r ((target mutable-vector/c)
           (tstart (and/c index/c
                          (<=/c (- (vector-length target)
                                   (- (vector-length source)
                                      sstart)))))
           (source vector?)
           (sstart (and/c index/c
                          (<=/c (vector-length source)))))
          any)
     (->pp ((target mutable-vector/c)
            (tstart (and/c index/c
                           (<=/c (- (vector-length target)
                                    (- send sstart)))))
            (source vector?)
            (sstart index/c)
            (send index/c))
           (<= sstart send (vector-length source))
           any)))
  
  (provide/contract (vector-swap!
                     (->r ((vec mutable-vector/c)
                           (i (and/c index/c
                                     (</c (vector-length vec))))
                           (j (and/c index/c
                                     (</c (vector-length vec)))))
                          any))
                    (rename my-vector-fill! s:vector-fill!
                            (case->
                             (-> vector? any/c any)
                             (->r ((vec vector?)
                                   (fill any/c)
                                   (start (and/c index/c
                                                 (<=/c (vector-length vec)))))
                                  any)
                             (->pp ((vec vector?)
                                    (fill any/c)
                                    (start index/c)
                                    (end index/c))
                                   (<= start end (vector-length vec))
                                   any)))
                    (vector-reverse! (vec-start-end-contract mutable-vector/c))
                    (vector-copy! copy!-contract)
                    (vector-reverse-copy! copy!-contract))
  
  ;;; (VECTOR-SWAP! <vector> <index1> <index2>) -> void
  ;;;   Swap the values in the locations at INDEX1 and INDEX2.
  (define (vector-swap! vec i j)
    (let ((x (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j x)))
  
  ;;; (VECTOR-FILL! <vector> <value> [<start> <end>]) -> <vector>
  ;;;   [R5RS+] Fill the locations in VECTOR between START, whose default
  ;;;   is 0, and END, whose default is the length of VECTOR, with VALUE.
  ;;;
  ;;; This one can probably be made really fast natively.
  (define my-vector-fill!
    (case-lambda
      ((vec value)
       (vector-fill! vec value))
      ((vec value start)
       (my-vector-fill! vec value start (vector-length vec)))
      ((vec value start end)
       (do ((i start (add1 i)))
         ((= i end))
         (vector-set! vec i value))
       vec)))
  
  ;;; (VECTOR-REVERSE! <vector> [<start> <end>]) -> void
  ;;;   Destructively reverse the contents of the sequence of locations
  ;;;   in VECTOR between START, whose default is 0, and END, whose
  ;;;   default is the length of VECTOR.
  (define vector-reverse!
    (letrec ((loop (lambda (vec i j)
                     (when (< i j)
                       (vector-swap! vec i j)
                       (loop vec (add1 i) (sub1 j))))))
      (opt-lambda (vec (start 0) (end (vector-length vec)))
        (loop vec start (sub1 end)))))
  
  ;;; (VECTOR-COPY! <target> <tstart> <source> [<sstart> <send>])
  ;;;       -> unspecified
  ;;;   Copy the values in the locations in [SSTART,SEND) from SOURCE to
  ;;;   to TARGET, starting at TSTART in TARGET.
  (define vector-copy!
    (letrec ((loop/l->r (lambda (target source send i j)
                          (cond ((< i send)
                                 (vector-set! target j
                                              (vector-ref source i))
                                 (loop/l->r target source send
                                            (add1 i) (add1 j))))))
             (loop/r->l (lambda (target source sstart i j)
                          (cond ((>= i sstart)
                                 (vector-set! target j
                                              (vector-ref source i))
                                 (loop/r->l target source sstart
                                            (sub1 i) (sub1 j)))))))
      (opt-lambda (target tstart source (sstart 0) (send (vector-length source)))
        (if (> sstart tstart)             ; Make sure we don't copy over
            ;   ourselves.
            (loop/l->r target source send sstart tstart)
            (loop/r->l target source sstart (sub1 send)
                       (+ -1 tstart send (- sstart)))))))
  
  ;;; (VECTOR-REVERSE-COPY! <target> <tstart> <source> [<sstart> <send>])
  (define vector-reverse-copy!
    (letrec ((loop (lambda (target source sstart i j)
                     (cond ((>= i sstart)
                            (vector-set! target j (vector-ref source i))
                            (loop target source sstart
                                  (sub1 i)
                                  (add1 j)))))))
      (opt-lambda (target tstart source (sstart 0) (send (vector-length source)))
        (cond ((and (eq? target source)
                    (= sstart tstart))
               (vector-reverse! target tstart send))
              ((and (eq? target source)
                    (or (between? sstart tstart send)
                        (between? tstart sstart
                                  (+ tstart (- send sstart)))))
               ;an error in the reference implement here
               (error 'vector-reverse-copy!
                      "Vector range for self-copying overlaps"))
              (else
               (loop target source sstart
                     (sub1 send)
                     tstart))))))
  
  (define (between? x y z)
    (and (<  x y)
         (<= y z)))
  
  (provide/contract (rename my-vector->list s:vector->list
                            (vec-start-end-contract vector?))
                    (reverse-vector->list vec-start-end-contract)
                    (reverse-list->vector (-> list? any)))
  
  ;;; (VECTOR->LIST <vector> [<start> <end>]) -> list
  ;;;   [R5RS+] Produce a list containing the elements in the locations
  ;;;   between START, whose default is 0, and END, whose default is the
  ;;;   length of VECTOR, from VECTOR.
  (define my-vector->list
    (case-lambda
      ((vec)
       (vector->list vec)) ;+++
      ((vec start)
       (my-vector->list vec start (vector-length vec)))
      ((vec start end)
       (do ((i (sub1 end) (sub1 i))
            (result '() (cons (vector-ref vec i) result)))
         ((< i start) result)))))
  
  ;;; (REVERSE-VECTOR->LIST <vector> [<start> <end>]) -> list
  ;;;   Produce a list containing the elements in the locations between
  ;;;   START, whose default is 0, and END, whose default is the length
  ;;;   of VECTOR, from VECTOR, in reverse order.
  (define reverse-vector->list
    (opt-lambda (vec (start 0) (end (vector-length vec)))
      ;(unfold (lambda (i) (= i end))     ; No SRFI 1.
      ;        (lambda (i) (vector-ref vec i))
      ;        (lambda (i) (add1 i))
      ;        start)
      (do ((i start (add1 i))
           (result '() (cons (vector-ref vec i) result)))
        ((= i end) result))))
  
  ;;; (REVERSE-LIST->VECTOR <list> -> vector
  ;;;   Produce a vector containing the elements in LIST in reverse order.
  (define (reverse-list->vector lst)
    (let* ((len (length lst))
           (vec (make-vector len)))
      (unfold1! (lambda (index l) (values (car l) (cdr l)))
                vec
                (sub1 len)
                lst)
      vec)))
