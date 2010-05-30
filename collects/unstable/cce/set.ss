#lang scheme

(require unstable/dict)

;; A Set is either a Dict or a struct with the prop:set property.
;; A SetProperty is:
;; (Vector (-> Set Any Any)
;;         (Or (-> Set Any Void) #f)
;;         (Or (-> Set Any Set) #f)
;;         (Or (-> Set Any Void) #f)
;;         (Or (-> Set Any Set) #f)
;;         (-> Set ExactInteger)
;;         (-> Set Sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Set Property
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set-property-guard : Any (List ...) -> Any
;; Protects prop:set from bad inputs.
(define (set-property-guard prop info)
  (check-vector 'prop:set "property" prop 7)
  (check-vector-element 'prop:set "property" prop 0
                        check-procedure "contains?" 2)
  (check-vector-element 'prop:set "property" prop 1
                        check-optional "insert!" check-procedure 2)
  (check-vector-element 'prop:set "property" prop 2
                        check-optional "insert" check-procedure 2)
  (check-vector-element 'prop:set "property" prop 3
                        check-optional "remove!" check-procedure 2)
  (check-vector-element 'prop:set "property" prop 4
                        check-optional "remove" check-procedure 2)
  (check-vector-element 'prop:set "property" prop 5
                        check-procedure "count" 1)
  (check-vector-element 'prop:set "property" prop 6
                        check-procedure "to-sequence" 1)
  prop)

(define-values [ prop:set set-struct? get ]
  (make-struct-type-property 'set set-property-guard))

(define (prop-contains? prop) (vector-ref prop 0))
(define (prop-insert! prop) (vector-ref prop 1))
(define (prop-insert prop) (vector-ref prop 2))
(define (prop-remove! prop) (vector-ref prop 3))
(define (prop-remove prop) (vector-ref prop 4))
(define (prop-count prop) (vector-ref prop 5))
(define (prop-to-sequence prop) (vector-ref prop 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Core Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set? set)
  (or (set-struct? set)
      (dict? set)))

(define (set-can-insert? set)
  (if (set-struct? set)
    (procedure? (prop-insert (get set)))
    (dict-can-functional-set? set)))

(define (set-can-remove? set)
  (if (set-struct? set)
    (procedure? (prop-remove (get set)))
    (and (dict-can-functional-set? set)
         (dict-can-remove-keys? set))))

(define (set-can-insert!? set)
  (if (set-struct? set)
    (procedure? (prop-insert! (get set)))
    (dict-mutable? set)))

(define (set-can-remove!? set)
  (if (set-struct? set)
    (procedure? (prop-remove! (get set)))
    (and (dict-mutable? set)
         (dict-can-remove-keys? set))))

(define (set-contains? set x)
  (if (set-struct? set)
    ((prop-contains? (get set)) set x)
    (dict-has-key? set x)))

(define (set-insert! set x)
  (if (set-struct? set)
    ((prop-insert! (get set)) set x)
    (dict-set! set x null)))

(define (set-insert set x)
  (if (set-struct? set)
    ((prop-insert (get set)) set x)
    (dict-set set x null)))

(define (set-remove! set x)
  (if (set-struct? set)
    ((prop-remove! (get set)) set x)
    (dict-remove! set x)))

(define (set-remove set x)
  (if (set-struct? set)
    ((prop-remove (get set)) set x)
    (dict-remove set x)))

(define (set-count set)
  (if (set-struct? set)
    ((prop-count (get set)) set)
    (dict-count set)))

(define (in-set set)
  (if (set-struct? set)
    ((prop-to-sequence (get set)) set)
    (in-dict-keys set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Derived Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set->list set)
  (for/list ([elem (in-set set)]) elem))

(define (set-empty? set)
  (= (set-count set) 0))

(define (set #:weak? [weak? #f]
             #:mutable? [mutable? weak?]
             #:compare [compare 'equal]
             . elements)
  (list->set elements #:mutable? mutable? #:weak? weak? #:compare compare))

(define (list->set elems
                   #:weak? [weak? #f]
                   #:mutable? [mutable? weak?]
                   #:compare [compare 'equal])
  (make-dict (for/list ([e (in-list elems)]) (cons e null))
             #:mutable? mutable? #:weak? weak? #:compare compare))

(define (empty-set #:weak? [weak? #f]
                   #:mutable? [mutable? weak?]
                   #:compare [compare 'equal])
  (empty-dict #:mutable? mutable? #:weak? weak? #:compare compare))

(define (custom-set #:compare compare
                    #:hash [hash (lambda (x) 0)]
                    #:hash2 [hash2 (lambda (x) 0)]
                    #:weak? [weak? #f]
                    #:mutable? [mutable? weak?]
                    . elems)
  (let* ([s (custom-dict compare hash hash2 #:mutable? mutable? #:weak? weak?)])
    (if mutable?
      (begin0 s
        (for ([elem (in-list elems)]) (set-insert! s elem)))
      (for/fold ([s s]) ([elem (in-list elems)])
        (set-insert s elem)))))

(define (set=? one two)
  (and (subset? one two)
       (subset? two one)))

(define (proper-subset? one two)
  (and (subset? one two)
       (not (subset? two one))))

(define (subset? one two)
  (for/and ([elem (in-set one)])
    (set-contains? two elem)))

(define (set-union set . rest)
  (for*/fold ([one set]) ([two (in-list rest)] [elem (in-set two)])
    (set-insert one elem)))

(define (set-intersection set . rest)
  (for*/fold ([one set]) ([two (in-list rest)] [elem (in-set one)]
                          #:when (not (set-contains? two elem)))
    (set-remove one elem)))

(define (set-difference set . rest)
  (for*/fold ([one set]) ([two (in-list rest)] [elem (in-set one)]
                          #:when (set-contains? two elem))
    (set-remove one elem)))

(define (set-exclusive-or set . rest)
  (for*/fold ([one set]) ([two (in-list rest)] [elem (in-set two)])
    (if (set-contains? one elem)
      (set-remove one elem)
      (set-insert one elem))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Generic Checks
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-vector caller desc value size)
  (unless (vector? value)
    (error caller "expected ~a to be a vector; got: ~e" desc value))
  (unless (= (vector-length value) size)
    (error caller
           "expected ~a to have length ~a; got length ~a in: ~e"
           desc size (vector-length value) value)))

(define (check-vector-element caller desc value index check part . args)
  (apply check
         caller
         (format "~a element ~a (~a)" desc index part)
         (vector-ref value index)
         args))

(define (check-procedure caller desc value arity)
  (unless (procedure? value)
    (error caller "expected ~a to be a procedure; got: ~e" desc value))
  (unless (procedure-arity-includes? value arity)
    (error caller
           "expected ~a to accept ~a arguments; got: ~e"
           desc
           arity
           value)))

(define (check-optional caller desc value check . args)
  (when value (apply check caller desc value args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Exports
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [set? (-> any/c boolean?)]
 [set-empty? (-> any/c boolean?)]
 [set
  (->* []
       [#:mutable? boolean? #:weak? boolean? #:compare (or/c 'eq 'eqv 'equal)]
       #:rest list?
       set?)]
 [list->set
  (->* [list?]
       [#:mutable? boolean? #:weak? boolean? #:compare (or/c 'eq 'eqv 'equal)]
       set?)]
 [empty-set
  (->* []
       [#:mutable? boolean? #:weak? boolean? #:compare (or/c 'eq 'eqv 'equal)]
       set?)]
 [custom-set
  (->* [#:compare (-> any/c any/c any/c)]
       [#:hash
        (-> any/c exact-integer?)
        #:hash2
        (-> any/c exact-integer?)
        #:mutable? boolean?
        #:weak? boolean?]
       #:rest list?
       set?)]
 [set->list (-> set? list?)]
 [set-contains? (-> set? any/c boolean?)]
 [set-insert (-> set? any/c any/c)]
 [set-remove (-> set? any/c set?)]
 [set-insert! (-> set? any/c void?)]
 [set-remove! (-> set? any/c void?)]
 [set-can-insert? (-> set? boolean?)]
 [set-can-remove? (-> set? boolean?)]
 [set-can-insert!? (-> set? boolean?)]
 [set-can-remove!? (-> set? boolean?)]
 [set-count (-> set? exact-nonnegative-integer?)]
 [in-set (-> set? sequence?)]
 [set=? (-> set? set? boolean?)]
 [subset? (-> set? set? boolean?)]
 [proper-subset? (-> set? set? boolean?)]
 [set-union
  (->* [(and/c set? set-can-insert?)] []
       #:rest (listof set?)
       set?)]
 [set-intersection
  (->* [(and/c set? set-can-remove?)] []
       #:rest (listof set?)
       set?)]
 [set-difference
  (->* [(and/c set? set-can-remove?)] []
       #:rest (listof set?)
       set?)]
 [set-exclusive-or
  (->* [(and/c set? set-can-insert? set-can-remove?)] []
       #:rest (listof set?)
       set?)]
 [prop:set struct-type-property?]
 )
