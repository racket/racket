; associative sets implementation, using lists.
; - key equality based on eq? by default, uses equal? if given the 'equal flag
; - raises exn:assoc-set:key-not-found if key not in associative set when trying
;   to remove a key or when trying to get a value and no default thunk is given.
; - raise exn:assoc-set:duplicate-key by default when trying to add a key to a
;   set where it already exists
; - strange things might happen if you use assoc-set-union, assoc-set-intersection,
;   or assoc-set-difference with two sets that don't use the same comparaison
;   function: you might end up with duplicate keys in some sets.
;
; Note: lots of set! and tail-recursive loops in this code, for speed

(module assoc-set-list (lib "mrflow.ss" "mrflow")
  (require
   (lib "list.ss") ; for foldr
   (lib "etc.ss") ; for opt-lambda
   "assoc-set-exn.ss" ; no prefix so we can re-provide
   )
  
  ; table = (listof (cons value value))
  (define-struct assoc-set (=? cardinality table))
  
  (provide/contract
   (exn:assoc-set? (any/c . -> . boolean?))
   (struct (exn:assoc-set:key-not-found exn:assoc-set) ((message (and/c string? immutable?))
							(continuation-marks continuation-mark-set?)
                                                        (assoc-set assoc-set?)
                                                        (key any/c)))
   (struct (exn:assoc-set:duplicate-key exn:assoc-set) ((message (and/c string? immutable?))
							(continuation-marks continuation-mark-set?)
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
      [() (make-assoc-set eq? 0 '())]
      [(flag) (make-assoc-set equal? 0 '())]))
  
  ; assoc-set -> assoc-set
  ; doesn't change =?
  (define (assoc-set-reset assoc-set)
    (set-assoc-set-table! assoc-set '())
    (set-assoc-set-cardinality! assoc-set 0)
    assoc-set)
  
  ; value -> boolean
  ; assoc-set? comes from the structure definition
  
  ; assoc-set value value (opt boolean) -> assoc-set
  (define assoc-set-set
    (opt-lambda (assoc-set key value (exn? #t))
      (let ([=? (assoc-set-=? assoc-set)]
            [original-table (assoc-set-table assoc-set)])
        (set-assoc-set-table! assoc-set (let loop ([table original-table])
                                          (if (null? table)
                                              (begin
                                                (set-assoc-set-cardinality! assoc-set (add1 (assoc-set-cardinality assoc-set)))
                                                (cons (cons key value) original-table))
                                              (let ([key-value-pair (car table)])
                                                (if (=? (car key-value-pair) key)
                                                    (if exn?
                                                        (raise-duplicate-key-exn "assoc-set-set" assoc-set key)
                                                        (begin
                                                          ; silently replace
                                                          (set-cdr! key-value-pair value)
                                                          original-table))
                                                    (loop (cdr table)))))))
        assoc-set)))
  
  ; assoc-set value (-> value) -> value
  (define assoc-set-get
    (opt-lambda (assoc-set key (not-found-thunk (lambda () (raise-key-not-found-exn "assoc-set-get" assoc-set key))))
      (let ([=? (assoc-set-=? assoc-set)])
        (let loop ([table (assoc-set-table assoc-set)])
          (if (null? table)
              (not-found-thunk)
              (let ([key-value-pair (car table)])
                (if (=? (car key-value-pair) key)
                    (cdr key-value-pair)
                    (loop (cdr table)))))))))
  
  ; assoc-set value -> boolean
  (define (assoc-set-in? assoc-set key)
    (let ([=? (assoc-set-=? assoc-set)])
      (ormap (lambda (key-value-pair)
               (=? (car key-value-pair) key))
             (assoc-set-table assoc-set))))
  
  ; assoc-set value (opt boolean) -> assoc-set
  (define assoc-set-remove
    (opt-lambda (assoc-set key (exn? #t))
      (let ([=? (assoc-set-=? assoc-set)]
            [original-table (assoc-set-table assoc-set)])
        (set-assoc-set-table! assoc-set
                              (let loop ([table original-table]
                                         [previous #f])
                                (if (null? table)
                                    (if exn?
                                        (raise-key-not-found-exn "assoc-set-remove" assoc-set key)
                                        ; silently ignore
                                        original-table)
                                    (let ([key-value-pair (car table)])
                                      (if (=? (car key-value-pair) key)
                                          (begin
                                            (set-assoc-set-cardinality! assoc-set (sub1 (assoc-set-cardinality assoc-set)))
                                            (if previous
                                                (begin
                                                  ; return shortened table
                                                  (set-cdr! previous (cdr table))
                                                  original-table)
                                                (cdr original-table)))
                                          (loop (cdr table) table)))))))
      assoc-set))
  
  ; assoc-set -> exact-non-negative-integer
  ; assoc-set-cardinality comes from the structure definition
  
  ; assoc-set -> boolean
  (define (assoc-set-empty? assoc-set)
    (= 0 (assoc-set-cardinality assoc-set)))
  
  ; (listof (cons value value)) (listof (cons value value)) -> (listof (cons value value))
  ; creates a (reversed) copy of l1 (to prevent list sharing between sets) and prefixes l2 with it
  (define (copy-reverse-and-prefix-assoc-lists l1 l2)
    (let loop ([l1 l1]
               [l2 l2])
      (if (null? l1)
          l2
          (loop (cdr l1) (cons (cons (caar l1) (cdar l1)) l2)))))
  
  ; (listof (cons value value)) -> (listof (cons value value))
  (define (copy-assoc-list l)
    (copy-reverse-and-prefix-assoc-lists l '()))
  
  ; assoc-set -> assoc-set
  (define (assoc-set-copy assoc-set)
    (make-assoc-set (assoc-set-=? assoc-set)
                         (assoc-set-cardinality assoc-set)
                         (copy-assoc-list (assoc-set-table assoc-set))))
  
  ; assoc-set (value value -> value) -> (listof value)
  (define (assoc-set-map assoc-set f)
    (let ([unary-f (lambda (key-value-pair)
                     (f (car key-value-pair) (cdr key-value-pair)))])
      (map unary-f (assoc-set-table assoc-set))))
  
  ; assoc-set (value value value -> value) value -> value
  (define (assoc-set-fold assoc-set f acc)
    (foldr (lambda (key-value-pair acc)
             (f (car key-value-pair) (cdr key-value-pair) acc))
           acc
           (assoc-set-table assoc-set)))
  
  ; assoc-set (value value -> value) -> assoc-set
  (define (assoc-set-for-each assoc-set f)
    (let ([unary-f (lambda (key-value-pair)
                     (f (car key-value-pair) (cdr key-value-pair)))])
      (for-each unary-f (assoc-set-table assoc-set)))
    assoc-set)
  
  ; assoc-set (value value -> value) -> assoc-set
  ; we know lists are never shared between sets, so we can set-cdr!
  (define (assoc-set-for-each! assoc-set f)
    (for-each (lambda (key-value-pair)
                (set-cdr! key-value-pair (f (car key-value-pair) (cdr key-value-pair))))
              (assoc-set-table assoc-set))
    assoc-set)
  
  ; assoc-set (value value -> boolean) (opt (union 'new 'same)) -> assoc-set
  (define assoc-set-filter
    (let (; assoc-set (value value -> boolean) -> assoc-set
          [filter-into-new-assoc-set
           (lambda (assoc-set tester)
             (let ([table '()]
                   [count 0])
               (for-each (lambda (key value)
                           (when (tester key value)
                             (set! table (cons (cons key value) table))
                             (set! count (add1 count))))
                         (assoc-set-table assoc-set))
               (make-assoc-set (assoc-set-=? assoc-set) count table)))])
      (opt-lambda (assoc-set tester (which-assoc-set 'new))
        (let ([new-assoc-set (filter-into-new-assoc-set assoc-set tester)])
          (case which-assoc-set
            [(new) new-assoc-set]
            [(same)
             (set-assoc-set-table! assoc-set (assoc-set-table new-assoc-set))
             (set-assoc-set-cardinality! assoc-set (assoc-set-cardinality new-assoc-set))
             assoc-set])))))
  
  ; assoc-set assoc-set (value value -> value) (opt (union 'new 'first 'second)) -> assoc-set
  (define assoc-set-union
    (opt-lambda (assoc-set1 assoc-set2 merge-values (which-assoc-set 'new))
      (let* ([=? (assoc-set-=? assoc-set1)]
             [new-assoc-set
              (let loop ([table1 (assoc-set-table assoc-set1)]
                         ; we shouldn't modify the original list
                         [table2 (copy-assoc-list (assoc-set-table assoc-set2))]
                         [count1 (assoc-set-cardinality assoc-set1)]
                         [count2 (assoc-set-cardinality assoc-set2)]
                         [acc '()]
                         [count 0])
                (if (null? table1)
                    ; we have already copied table2, so we can destructively modify it
                    (make-assoc-set =? (+ count count2)
                                         (append! table2 acc))
                    (if (null? table2)
                        (make-assoc-set =? (+ count count1)
                                             (copy-reverse-and-prefix-assoc-lists table1 acc))
                        (let ([key1 (caar table1)])
                          ; search table2 for same key
                          (let loop-assoc-set2 ([t2 table2]
                                                [previous #f])
                            (if (null? t2)
                                (begin
                                  (set! acc (cons (cons key1 (cdar table1)) acc))
                                  (set! count (add1 count))
                                  (set! table1 (cdr table1))
                                  (set! count1 (sub1 count1)))
                                (if (=? key1 (caar t2))
                                    (begin
                                      (set! acc (cons (cons key1 (merge-values (cdar table1) (cdar t2))) acc))
                                      (set! count (add1 count))
                                      (set! table1 (cdr table1))
                                      (set! count1 (sub1 count1))
                                      (if previous
                                          (set-cdr! previous (cdr t2))
                                          (set! table2 (cdr table2)))
                                      (set! count2 (sub1 count2)))
                                    (loop-assoc-set2 (cdr t2) t2))))
                          (loop table1 table2 count1 count2 acc count)))))])
        (case which-assoc-set
          [(new) new-assoc-set]
          [(first)
           (set-assoc-set-cardinality! assoc-set1 (assoc-set-cardinality new-assoc-set))
           (set-assoc-set-table! assoc-set1 (assoc-set-table new-assoc-set))
           assoc-set1]
          [(second)
           (set-assoc-set-cardinality! assoc-set2 (assoc-set-cardinality new-assoc-set))
           (set-assoc-set-table! assoc-set2 (assoc-set-table new-assoc-set))
           assoc-set2]))))
  
  ; assoc-set assoc-set (value value -> value) (opt (union 'new 'first 'second)) -> assoc-set
  (define assoc-set-intersection
    (opt-lambda (assoc-set1 assoc-set2 merge-values (which-assoc-set 'new))
      (let* ([=? (assoc-set-=? assoc-set1)]
             [new-assoc-set
              (let loop ([table1 (assoc-set-table assoc-set1)]
                         ; we shouldn't modify the original list
                         [table2 (copy-assoc-list (assoc-set-table assoc-set2))]
                         [count1 (assoc-set-cardinality assoc-set1)]
                         [count2 (assoc-set-cardinality assoc-set2)]
                         [acc '()]
                         [count 0])
                (if (null? table1)
                    (make-assoc-set =? count acc)
                    (if (null? table2)
                        (make-assoc-set =? count acc)
                        (let ([key1 (caar table1)])
                          ; search table2 for same key
                          (let loop-assoc-set2 ([t2 table2]
                                                [previous #f])
                            (if (null? t2)
                                (begin
                                  (set! table1 (cdr table1))
                                  (set! count1 (sub1 count1)))
                                (if (=? key1 (caar t2))
                                    (begin
                                      (set! acc (cons (cons key1 (merge-values (cdar table1) (cdar t2))) acc))
                                      (set! count (add1 count))
                                      (set! table1 (cdr table1))
                                      (set! count1 (sub1 count1))
                                      (if previous
                                          (set-cdr! previous (cdr t2))
                                          (set! table2 (cdr table2)))
                                      (set! count2 (sub1 count2)))
                                    (loop-assoc-set2 (cdr t2) t2))))
                          (loop table1 table2 count1 count2 acc count)))))])
        (case which-assoc-set
          [(new) new-assoc-set]
          [(first)
           (set-assoc-set-cardinality! assoc-set1 (assoc-set-cardinality new-assoc-set))
           (set-assoc-set-table! assoc-set1 (assoc-set-table new-assoc-set))
           assoc-set1]
          [(second)
           (set-assoc-set-cardinality! assoc-set2 (assoc-set-cardinality new-assoc-set))
           (set-assoc-set-table! assoc-set2 (assoc-set-table new-assoc-set))
           assoc-set2]))))
  
  ; assoc-set assoc-set (opt (union 'new 'first 'second)) -> assoc-set
  (define assoc-set-difference
    (opt-lambda (assoc-set1 assoc-set2 (which-assoc-set 'new))
      (let* ([=? (assoc-set-=? assoc-set1)]
             [new-assoc-set
              (let loop ([table1 (assoc-set-table assoc-set1)]
                         ; we shouldn't modify the original list
                         [table2 (copy-assoc-list (assoc-set-table assoc-set2))]
                         [count1 (assoc-set-cardinality assoc-set1)]
                         [count2 (assoc-set-cardinality assoc-set2)]
                         [acc '()]
                         [count 0])
                (if (null? table1)
                    (make-assoc-set =? count acc)
                    (if (null? table2)
                        (make-assoc-set =? (+ count count1)
                                             (copy-reverse-and-prefix-assoc-lists table1 acc))
                        (let ([key1 (caar table1)])
                          ; search table2 for same key
                          (let loop-assoc-set2 ([t2 table2]
                                                [previous #f])
                            (if (null? t2)
                                (begin
                                  (set! acc (cons (cons key1 (cdar table1)) acc))
                                  (set! count (add1 count))
                                  (set! table1 (cdr table1))
                                  (set! count1 (sub1 count1)))
                                (if (=? key1 (caar t2))
                                    (begin
                                      (set! table1 (cdr table1))
                                      (set! count1 (sub1 count1))
                                      (if previous
                                          (set-cdr! previous (cdr t2))
                                          (set! table2 (cdr table2)))
                                      (set! count2 (sub1 count2)))
                                    (loop-assoc-set2 (cdr t2) t2))))
                          (loop table1 table2 count1 count2 acc count)))))])
        (case which-assoc-set
          [(new) new-assoc-set]
          [(first)
           (set-assoc-set-cardinality! assoc-set1 (assoc-set-cardinality new-assoc-set))
           (set-assoc-set-table! assoc-set1 (assoc-set-table new-assoc-set))
           assoc-set1]
          [(second)
           (set-assoc-set-cardinality! assoc-set2 (assoc-set-cardinality new-assoc-set))
           (set-assoc-set-table! assoc-set2 (assoc-set-table new-assoc-set))
           assoc-set2]))))
  
  ; assoc-set assoc-set -> boolean
  ; compares keys only
  (define (assoc-set-subset? assoc-set1 assoc-set2)
    (andmap (lambda (key value)
              (assoc-set-in? assoc-set2 key))
            (assoc-set-table assoc-set1)))
  
  )
