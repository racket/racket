; sets implementation, using lists.
; - value equality based on eq? by default, uses equal? if given the 'equal flag
; - raises exn:set:value-not-found if value not in set when trying
;   to remove a value.
; - raise exn:set:duplicate-value by default when trying to add a value to a
;   set where it already exists
; - strange things might happen if you use set-union, set-intersection,
;   or set-difference with two sets that don't use the same comparaison
;   function: you might end up with duplicate values in some sets.
;
; Note: lots of set! and tail-recursive loops in this code, for speed

(module set-list (lib "mrflow.ss" "mrflow")
  (require
   (lib "list.ss") ; for foldr
   (lib "etc.ss") ; for opt-lambda
   "set-exn.ss" ; no prefix so we can re-provide
   )
  
  ; table = (listof (cons value value))
  (define-struct set (=? cardinality table))
  
  (provide/contract
   (exn:set? (any/c . -> . boolean?))
   (struct (exn:set:value-not-found exn:set) ((message (and/c string? immutable?))
                                              (continuation-mark-set continuation-mark-set?)
                                              (set set?)
                                              (value any/c)))
   (struct (exn:set:duplicate-value exn:set) ((message (and/c string? immutable?))
                                              (continuation-mark-set continuation-mark-set?)
                                              (set set?)
                                              (value any/c)))
   (set-make (() ((symbols 'equal)) . opt-> . set?))
   (set-reset (set? . -> . set?))
   (set? (any/c . -> . boolean?))
   (set-set ((set? any/c) (boolean?) . opt-> . set?))
   (set-in? (set? any/c . -> . boolean?))
   (set-remove ((set? any/c) (boolean?) . opt-> . set?))
   (set-cardinality (set? . -> . non-negative-exact-integer?))
   (set-empty? (set? . -> . boolean?))
   (set-copy (set? . -> . set?))
   (set-map (set? (any/c . -> . any) . -> . (listof any/c)))
   (set-fold (set? (any/c any/c . -> . any) any/c . -> . any))
   (set-for-each (set? (any/c . -> . any) . -> . set?))
   (set-for-each! (set? (any/c . -> . any) . -> . set?))
   (set-filter ((set? (any/c . -> . boolean?)) ((symbols 'new 'same)) . opt-> . set?))
   (set-union ((set? set?) ((symbols 'new 'first 'second)) . opt-> . set?))
   (set-intersection ((set? set?) ((symbols 'new 'first 'second)) . opt-> . set?))
   (set-difference ((set? set?) ((symbols 'new 'first 'second)) . opt-> . set?))
   (set-subset? (set? set? . -> . boolean?))
   )
  
  ; (opt 'equal) -> set
  ; we test the optional argument ourselves to preserve data abstraction even in the
  ; presence of an exception
  (define set-make
    (case-lambda
      [() (make-set eq? 0 '())]
      [(flag) (make-set equal? 0 '())]))
  
  ; set -> set
  ; doesn't change =?
  (define (set-reset set)
    (set-set-table! set '())
    (set-set-cardinality! set 0)
    set)
  
  ; value -> boolean
  ; set? comes from the structure definition
  
  ; set value (opt boolean) -> set
  (define set-set
    (opt-lambda (set value (exn? #t))
      (let ([=? (set-=? set)]
            [original-table (set-table set)])
        (set-set-table! set (let loop ([table original-table])
                              (if (null? table)
                                  (begin
                                    (set-set-cardinality! set (add1 (set-cardinality set)))
                                    (cons value original-table))
                                  (if (=? (car table) value)
                                      (if exn?
                                          (raise-duplicate-value-exn "set-set" set value)
                                          ; silently ignore
                                          original-table)
                                      (loop (cdr table)))))))
      set))
  
  ; set value -> boolean
  (define (set-in? set value)
    (let ([=? (set-=? set)])
      (ormap (lambda (current-value)
               (=? current-value value))
             (set-table set))))
  
  ; set value (opt boolean) -> set
  (define set-remove
    (opt-lambda (set value (exn? #t))
      (let ([=? (set-=? set)]
            [original-table (set-table set)])
        (set-set-table! set
                        (let loop ([table original-table]
                                   [previous #f])
                          (if (null? table)
                              (if exn?
                                  (raise-value-not-found-exn "set-remove" set value)
                                  ; silently ignore
                                  original-table)
                              (if (=? (car table) value)
                                  (begin
                                    (set-set-cardinality! set (sub1 (set-cardinality set)))
                                    (if previous
                                        (begin
                                          ; return shortened table
                                          (set-cdr! previous (cdr table))
                                          original-table)
                                        (cdr original-table)))
                                  (loop (cdr table) table))))))
      set))
  
  ; set -> exact-non-negative-integer
  ; set-cardinality comes from the structure definition
  
  ; set -> boolean
  (define (set-empty? set)
    (= 0 (set-cardinality set)))
  
  ; (listof value) (listof value) -> (listof value)
  ; creates a (reversed) copy of l1 (to prevent list sharing between sets) and prefixes l2 with it
  (define (copy-reverse-and-prefix-lists l1 l2)
    (let loop ([l1 l1]
               [l2 l2])
      (if (null? l1)
          l2
          (loop (cdr l1) (cons (car l1) l2)))))
  
  ; (listof value) -> (listof value)
  (define (copy-list l)
    (copy-reverse-and-prefix-lists l '()))
  
  ; set -> set
  (define (set-copy set)
    (make-set (set-=? set)
              (set-cardinality set)
              (copy-list (set-table set))))
  
  ; set (value -> value) -> (listof value)
  (define (set-map set f)
    (map f (set-table set)))
  
  ; set (value value -> value) value -> value
  (define (set-fold set f acc)
    (foldr f acc (set-table set)))
  
  ; set (value -> value) -> set
  (define (set-for-each set f)
    (for-each f (set-table set))
    set)
  
  ; set (value -> value) -> set
  ; it's up to the user to make sure f is injective. Otherwise we might end up with
  ; duplicates in the set.
  ; we know lists are never shared between sets, so we can set-cdr!
  (define (set-for-each! set f)
    (let loop ([table (set-table set)])
      (unless (null? table)
        (set-car! table (f (car table)))
        (loop (cdr table))))
    set)
  
  ; set (value -> boolean) (opt (union 'new 'same)) -> set
  (define set-filter
    (let (; set (value -> boolean) -> set
          [filter-into-new-set
           (lambda (set tester)
             (let loop ([table (set-table set)]
                        [new-table '()]
                        [count 0])
               (if (null? table)
                   (make-set (set-=? set) count new-table)
                   (let ([value (car table)])
                     (if (tester value)
                         (loop (cdr table) (cons value new-table) (add1 count))
                         (loop (cdr table) new-table count))))))])
      (opt-lambda (set tester (which-set 'new))
        (let ([new-set (filter-into-new-set set tester)])
          (case which-set
            [(new) new-set]
            [(same)
             (set-set-table! set (set-table new-set))
             (set-set-cardinality! set (set-cardinality new-set))
             set])))))
  
  ; set set (opt (union 'new 'first 'second)) -> set
  (define set-union
    (opt-lambda (set1 set2 (which-set 'new))
      (let* ([=? (set-=? set1)]
             [new-set
              (let loop ([table1 (set-table set1)]
                         ; we shouldn't modify the original list
                         [table2 (copy-list (set-table set2))]
                         [count1 (set-cardinality set1)]
                         [count2 (set-cardinality set2)]
                         [acc '()]
                         [count 0])
                (if (null? table1)
                    ; we have already copied table2, so we can destructively modify it
                    (make-set =? (+ count count2)
                              (append! table2 acc))
                    (if (null? table2)
                        (make-set =? (+ count count1)
                                  (copy-reverse-and-prefix-lists table1 acc))
                        (let ([value1 (car table1)])
                          ; search table2 for same value
                          (let loop-set2 ([t2 table2]
                                          [previous #f])
                            (if (null? t2)
                                (begin
                                  (set! acc (cons value1 acc))
                                  (set! count (add1 count))
                                  (set! table1 (cdr table1))
                                  (set! count1 (sub1 count1)))
                                (if (=? value1 (car t2))
                                    (begin
                                      (set! acc (cons value1 acc))
                                      (set! count (add1 count))
                                      (set! table1 (cdr table1))
                                      (set! count1 (sub1 count1))
                                      (if previous
                                          (set-cdr! previous (cdr t2))
                                          (set! table2 (cdr table2)))
                                      (set! count2 (sub1 count2)))
                                    (loop-set2 (cdr t2) t2))))
                          (loop table1 table2 count1 count2 acc count)))))])
        (case which-set
          [(new) new-set]
          [(first)
           (set-set-cardinality! set1 (set-cardinality new-set))
           (set-set-table! set1 (set-table new-set))
           set1]
          [(second)
           (set-set-cardinality! set2 (set-cardinality new-set))
           (set-set-table! set2 (set-table new-set))
           set2]))))
  
  ; set set (opt (union 'new 'first 'second)) -> set
  (define set-intersection
    (opt-lambda (set1 set2 (which-set 'new))
      (let* ([=? (set-=? set1)]
             [new-set
              (let loop ([table1 (set-table set1)]
                         ; we shouldn't modify the original list
                         [table2 (copy-list (set-table set2))]
                         [count1 (set-cardinality set1)]
                         [count2 (set-cardinality set2)]
                         [acc '()]
                         [count 0])
                (if (null? table1)
                    (make-set =? count acc)
                    (if (null? table2)
                        (make-set =? count acc)
                        (let ([value1 (car table1)])
                          ; search table2 for same value
                          (let loop-set2 ([t2 table2]
                                          [previous #f])
                            (if (null? t2)
                                (begin
                                  (set! table1 (cdr table1))
                                  (set! count1 (sub1 count1)))
                                (if (=? value1 (car t2))
                                    (begin
                                      (set! acc (cons value1 acc))
                                      (set! count (add1 count))
                                      (set! table1 (cdr table1))
                                      (set! count1 (sub1 count1))
                                      (if previous
                                          (set-cdr! previous (cdr t2))
                                          (set! table2 (cdr table2)))
                                      (set! count2 (sub1 count2)))
                                    (loop-set2 (cdr t2) t2))))
                          (loop table1 table2 count1 count2 acc count)))))])
        (case which-set
          [(new) new-set]
          [(first)
           (set-set-cardinality! set1 (set-cardinality new-set))
           (set-set-table! set1 (set-table new-set))
           set1]
          [(second)
           (set-set-cardinality! set2 (set-cardinality new-set))
           (set-set-table! set2 (set-table new-set))
           set2]))))
  
  ; set set (opt (union 'new 'first 'second)) -> set
  (define set-difference
    (opt-lambda (set1 set2 (which-set 'new))
      (let* ([=? (set-=? set1)]
             [new-set
              (let loop ([table1 (set-table set1)]
                         ; we shouldn't modify the original list
                         [table2 (copy-list (set-table set2))]
                         [count1 (set-cardinality set1)]
                         [count2 (set-cardinality set2)]
                         [acc '()]
                         [count 0])
                (if (null? table1)
                    (make-set =? count acc)
                    (if (null? table2)
                        (make-set =? (+ count count1)
                                  (copy-reverse-and-prefix-lists table1 acc))
                        (let ([value1 (car table1)])
                          ; search table2 for same value
                          (let loop-set2 ([t2 table2]
                                          [previous #f])
                            (if (null? t2)
                                (begin
                                  (set! acc (cons value1 acc))
                                  (set! count (add1 count))
                                  (set! table1 (cdr table1))
                                  (set! count1 (sub1 count1)))
                                (if (=? value1 (car t2))
                                    (begin
                                      (set! table1 (cdr table1))
                                      (set! count1 (sub1 count1))
                                      (if previous
                                          (set-cdr! previous (cdr t2))
                                          (set! table2 (cdr table2)))
                                      (set! count2 (sub1 count2)))
                                    (loop-set2 (cdr t2) t2))))
                          (loop table1 table2 count1 count2 acc count)))))])
        (case which-set
          [(new) new-set]
          [(first)
           (set-set-cardinality! set1 (set-cardinality new-set))
           (set-set-table! set1 (set-table new-set))
           set1]
          [(second)
           (set-set-cardinality! set2 (set-cardinality new-set))
           (set-set-table! set2 (set-table new-set))
           set2]))))
  
  ; set set -> boolean
  (define (set-subset? set1 set2)
    (andmap (lambda (value)
              (set-in? set2 value))
            (set-table set1)))
  
  )
