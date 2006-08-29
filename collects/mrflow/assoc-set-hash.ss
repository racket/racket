; associative sets implementation, using hash tables.
; - key equality based on eq? by default, uses equal? if given the 'equal flag
; - raises exn:assoc-set:key-not-found if key not in associative set when trying
;   to remove a key or when trying to get a value and no default thunk is given.
; - raise exn:assoc-set:duplicate-key by default when trying to add a key to a
;   set where it already exists
; - strange things might happen if you use assoc-set-union, assoc-set-intersection,
;   or assoc-set-difference with two sets that don't use the same comparaison
;   function: you might end up with duplicate keys in some sets.

(module assoc-set-hash (lib "mrflow.ss" "mrflow")
  (require
   (lib "etc.ss") ; for opt-lambda
   "assoc-set-exn.ss" ; no prefix so we can re-provide
   (prefix cst: "constants.ss")
   )
  
  ; table = (hashtableof value value)
  (define-struct assoc-set (cardinality table))
  
  (provide/contract
   (exn:assoc-set? (any/c . -> . boolean?))
   (struct (exn:assoc-set:key-not-found exn:assoc-set) ((message (and/c string? immutable?))
							(continuation-mark-set continuation-mark-set?)
							(assoc-set assoc-set?)
							(key any/c)))
   (struct (exn:assoc-set:duplicate-key exn:assoc-set) ((message (and/c string? immutable?))
							(continuation-mark-set continuation-mark-set?)
							(assoc-set assoc-set?)
                                                        (key any/c)))
   (assoc-set-make (() ((symbols 'equal)) . opt-> . assoc-set?))
   (assoc-set-reset (assoc-set? . -> . assoc-set?))
   (assoc-set? (any/c . -> . boolean?))
   (assoc-set-set ((assoc-set? any/c any/c) (boolean?) . opt-> . assoc-set?))
   (assoc-set-get ((assoc-set? any/c) ((-> any)) . opt-> . any))
   (assoc-set-in? (assoc-set? any/c . -> . boolean?))
   (assoc-set-remove ((assoc-set? any/c) (boolean?) . opt-> . assoc-set?))
   (assoc-set-cardinality (assoc-set? . -> . non-negative-exact-integer?))
   (assoc-set-empty? (assoc-set? . -> . boolean?))
   (assoc-set-copy (assoc-set? . -> . assoc-set?))
   (assoc-set-map (assoc-set? (any/c any/c . -> . any) . -> . (listof any/c)))
   (assoc-set-fold (assoc-set? (any/c any/c any/c . -> . any) any/c . -> . any))
   (assoc-set-for-each (assoc-set? (any/c any/c . -> . any) . -> . assoc-set?))
   (assoc-set-for-each! (assoc-set? (any/c any/c . -> . any) . -> . assoc-set?))
   (assoc-set-filter ((assoc-set? (any/c any/c . -> . boolean?)) ((symbols 'new 'same)) . opt-> . assoc-set?))
   (assoc-set-union ((assoc-set? assoc-set? (any/c any/c . -> . any)) ((symbols 'new 'first 'second)) . opt-> . assoc-set?))
   (assoc-set-intersection ((assoc-set? assoc-set? (any/c any/c . -> . any)) ((symbols 'new 'first 'second)) . opt-> . assoc-set?))
   (assoc-set-difference ((assoc-set? assoc-set?) ((symbols 'new 'first 'second)) . opt-> . assoc-set?))
   (assoc-set-subset? (assoc-set? assoc-set? . -> . boolean?))
   )
  
  ; (opt 'equal) -> assoc-set
  ; we test the optional argument ourselves to preserve data abstraction even in the
  ; presence of an exception
  (define assoc-set-make
    (case-lambda
      [() (make-assoc-set 0 (make-hash-table))]
      [(flag) (make-assoc-set 0 (make-hash-table 'equal))]))
  
  ; assoc-set -> assoc-set
  (define (assoc-set-reset assoc-set)
    (set-assoc-set-table! assoc-set (make-hash-table))
    (set-assoc-set-cardinality! assoc-set 0)
    assoc-set)
  
  ; value -> boolean
  ; assoc-set? comes from the structure definition
  
  ; assoc-set value value (opt boolean) -> assoc-set
  (define assoc-set-set
    (opt-lambda (assoc-set key value (exn? #t))
      (if (assoc-set-in? assoc-set key)
          (if exn?
              (raise-duplicate-key-exn "assoc-set-set" assoc-set key)
              ; silently replace
              (hash-table-put! (assoc-set-table assoc-set) key value))
          (begin
            (set-assoc-set-cardinality! assoc-set (add1 (assoc-set-cardinality assoc-set)))
            (hash-table-put! (assoc-set-table assoc-set) key value)))
      assoc-set))
  
  ; assoc-set value (-> value) -> value
  (define assoc-set-get
    (opt-lambda (assoc-set key (not-found-thunk (lambda () (raise-key-not-found-exn "assoc-set-get" assoc-set key))))
      (hash-table-get (assoc-set-table assoc-set) key not-found-thunk)))
  
  ; assoc-set value -> boolean
  (define assoc-set-in? 
    (let ([sym (gensym)])
      (lambda (assoc-set key)
        (not (eq? sym (hash-table-get (assoc-set-table assoc-set) key (lambda () sym)))))))
  
  ; assoc-set value (opt boolean) -> assoc-set
  (define assoc-set-remove
    (opt-lambda (assoc-set key (exn? #t))
      (if (assoc-set-in? assoc-set key)
          (begin
            (set-assoc-set-cardinality! assoc-set (sub1 (assoc-set-cardinality assoc-set)))
            (hash-table-remove! (assoc-set-table assoc-set) key))
          (when exn?
            (raise-key-not-found-exn "assoc-set-remove" assoc-set key)))
      assoc-set))
  
  ; assoc-set -> exact-non-negative-integer
  ; assoc-set-cardinality comes from the structure definition
  
  ; assoc-set -> boolean
  (define (assoc-set-empty? assoc-set)
    (= 0 (assoc-set-cardinality assoc-set)))
  
  ; assoc-set -> assoc-set
  (define (assoc-set-copy assoc-set)
    (let ([new-table (make-hash-table)])
      (hash-table-for-each (assoc-set-table assoc-set)
                           (lambda (key value)
                             (hash-table-put! new-table key value)))
      (make-assoc-set (assoc-set-cardinality assoc-set)
                      new-table)))
  
  ; assoc-set (value value -> value) -> (listof value)
  (define (assoc-set-map assoc-set f)
    (hash-table-map (assoc-set-table assoc-set) f))
  
  ; assoc-set (value value value -> value) value -> value
  (define (assoc-set-fold assoc-set f acc)
    (let ([acc acc])
      (hash-table-for-each (assoc-set-table assoc-set)
                           (lambda (key value)
                             (set! acc (f key value acc))))
      acc))
  
  ; assoc-set (value value -> value) -> assoc-set
  (define (assoc-set-for-each assoc-set f)
    (hash-table-for-each (assoc-set-table assoc-set) f)
    assoc-set)
  
  ; assoc-set (value value -> value) -> assoc-set
  ; we need a new table because of the "Caveat concerning concurrent access" for hash tables
  ; in the help desk.
  (define (assoc-set-for-each! assoc-set f)
    (let ([new-table (make-hash-table)])
      (hash-table-for-each (assoc-set-table assoc-set)
                           (lambda (key value)
                             (hash-table-put! new-table key (f key value))))
      (set-assoc-set-table! assoc-set new-table))
    assoc-set)
  
  ; assoc-set (value value -> boolean) (opt (union 'new 'same)) -> assoc-set
  (define assoc-set-filter
    (let (; assoc-set (value value -> boolean) -> assoc-set
          [filter-set-into-new-assoc-set
           (lambda (assoc-set tester)
             (let ([table (make-hash-table)]
                   [count 0])
               (hash-table-for-each (assoc-set-table assoc-set)
                                    (lambda (key value)
                                      (when (tester key value)
                                        (hash-table-put! table key value)
                                        (set! count (add1 count)))))
               (make-assoc-set count table)))])
      (opt-lambda (assoc-set tester (which-set 'new))
        (let ([new-assoc-set (filter-set-into-new-assoc-set assoc-set tester)])
          (case which-set
            [(new) new-assoc-set]
            [(same)
             (set-assoc-set-table! assoc-set (assoc-set-table new-assoc-set))
             (set-assoc-set-cardinality! assoc-set (assoc-set-cardinality new-assoc-set))
             assoc-set]
            ;[else (argexn:raise-arg-mismatch-exn "assoc-set-filter" '(union new same) which-set)]
            )))))
  
  ; assoc-set assoc-set (value value -> value) (opt (union 'new 'first 'second)) -> assoc-set
  (define assoc-set-union
    (let (; assoc-set assoc-set (value value -> value) -> assoc-set
          [union-second-set-into-first
           (lambda (assoc-set1 assoc-set2 merge-values)
             (let ([table (assoc-set-table assoc-set1)]
                   [count (assoc-set-cardinality assoc-set1)])
               (hash-table-for-each (assoc-set-table assoc-set2)
                                    (lambda (key value)
                                      (if (assoc-set-in? assoc-set1 key)
                                          (hash-table-put! table key
                                                           (merge-values (hash-table-get table key cst:dummy)
                                                                         value))
                                          (begin
                                            (set! count (add1 count))
                                            (hash-table-put! table key value)))))
               (set-assoc-set-cardinality! assoc-set1 count))
             assoc-set1)])
      (opt-lambda (assoc-set1 assoc-set2 merge-values (which-set 'new))
        (case which-set
          [(new)
           ; copying is presumably faster than testing
           (if (< (assoc-set-cardinality assoc-set1) (assoc-set-cardinality assoc-set2))
               (union-second-set-into-first (assoc-set-copy assoc-set2) assoc-set1)
               (union-second-set-into-first (assoc-set-copy assoc-set1) assoc-set2))]
          [(first) (union-second-set-into-first assoc-set1 assoc-set2)]
          [(second) (union-second-set-into-first assoc-set2 assoc-set1)]
          ;[else (argexn:raise-arg-mismatch-exn "assoc-set-union" '(union new first second) which-set)]
          ))))
  
  ; assoc-set assoc-set (value value -> value) (opt (union 'new 'first 'second)) -> assoc-set
  (define assoc-set-intersection
    (let (; assoc-set assoc-set (value value -> value) -> assoc-set
          [intersect-into-new-assoc-set
           (lambda (assoc-set1 assoc-set2 merge-values)
             (let ([assoc-set2-table (assoc-set-table assoc-set2)]
                   [table (make-hash-table)]
                   [count 0])
               (hash-table-for-each (assoc-set-table assoc-set1)
                                    (lambda (key value)
                                      (when (assoc-set-in? assoc-set2 key)
                                        (hash-table-put! table key
                                                         (merge-values value
                                                                       (hash-table-get assoc-set2-table key cst:dummy)))
                                        (set! count (add1 count)))))
               (make-assoc-set count table)))])
      (opt-lambda (assoc-set1 assoc-set2 merge-values (which-set 'new))
        (let ([new-assoc-set
               (if (< (assoc-set-cardinality assoc-set1) (assoc-set-cardinality assoc-set2))
                   (intersect-into-new-assoc-set assoc-set1 assoc-set2 merge-values)
                   (intersect-into-new-assoc-set assoc-set2 assoc-set1 merge-values))])
          (case which-set
            [(new) new-assoc-set]
            [(first)
             (set-assoc-set-table! assoc-set1 (assoc-set-table new-assoc-set))
             (set-assoc-set-cardinality! assoc-set1 (assoc-set-cardinality new-assoc-set))
             assoc-set1]
            [(second)
             (set-assoc-set-table! assoc-set2 (assoc-set-table new-assoc-set))
             (set-assoc-set-cardinality! assoc-set2 (assoc-set-cardinality new-assoc-set))
             assoc-set2]
            ;[else (argexn:raise-arg-mismatch-exn "assoc-set-intersection" '(union new first second) which-set)]
            )))))
  
  ; assoc-set assoc-set (opt (union 'new 'first 'second)) -> assoc-set
  (define assoc-set-difference
    (let (; assoc-set assoc-set -> assoc-set
          [difference-into-new-assoc-set
           (lambda (assoc-set1 assoc-set2)
             (let ([table (make-hash-table)]
                   [count 0])
               (hash-table-for-each (assoc-set-table assoc-set1)
                                    (lambda (key value)
                                      (unless (assoc-set-in? assoc-set2 key)
                                        (hash-table-put! table key value)
                                        (set! count (add1 count)))))
               (make-assoc-set count table)))])
      (opt-lambda (assoc-set1 assoc-set2 (which-set 'new))
        (let ([new-assoc-set (difference-into-new-assoc-set assoc-set1 assoc-set2)])
          (case which-set
            [(new) new-assoc-set]
            [(first) 
             (set-assoc-set-table! assoc-set1 (assoc-set-table new-assoc-set))
             (set-assoc-set-cardinality! assoc-set1 (assoc-set-cardinality new-assoc-set))
             assoc-set1]
            [(second)
             (set-assoc-set-table! assoc-set2 (assoc-set-table new-assoc-set))
             (set-assoc-set-cardinality! assoc-set2 (assoc-set-cardinality new-assoc-set))
             assoc-set2]
            ;[else (argexn:raise-arg-mismatch-exn "assoc-set-difference" '(union new first second) which-set)]
            )))))
  
  ; assoc-set assoc-set -> boolean
  ; compares keys only
  (define (assoc-set-subset? assoc-set1 assoc-set2)
    (let/ec k
      (hash-table-for-each (assoc-set-table assoc-set1)
                           (lambda (key value)
                             (unless (assoc-set-in? assoc-set2 key)
                               (k #f))))
      #t))
  
 )
