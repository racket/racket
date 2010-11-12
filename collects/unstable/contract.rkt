#lang racket/base
(require racket/contract racket/dict racket/match)

(define path-element?
  (or/c path-string? (symbols 'up 'same)))
;; Eli: We already have a notion of "path element" which is different
;;   from this (see `string->path-element') .

(define port-number? (between/c 1 65535))
(define tcp-listen-port? (between/c 0 65535))

(define (non-empty-string? x)
  (and (string? x) (not (zero? (string-length x)))))
(define (non-empty-bytes? x)
  (and (bytes? x) (not (zero? (bytes-length x)))))
(define (non-empty-vector? x)
  (and (vector? x) (not (zero? (vector-length x)))))
(define (non-empty-list? x)
  (and (list? x) (pair? x)))
(define (singleton-list? x)
  (and (pair? x) (null? (cdr x))))

;; ryanc added:

;; (if/c predicate then/c else/c) applies then/c to satisfying
;;   predicate, else/c to those that don't.
(define (if/c predicate then/c else/c)
  #|
  Naive version:
    (or/c (and/c predicate then/c)
          (and/c (not/c predicate) else/c))
  But that applies predicate twice.
  |#
  (let ([then-ctc (coerce-contract 'if/c then/c)]
        [else-ctc (coerce-contract 'if/c else/c)])
    (define name (build-compound-type-name 'if/c predicate then-ctc else-ctc))
    ;; Special case: if both flat contracts, make a flat contract.
    (if (and (flat-contract? then-ctc)
             (flat-contract? else-ctc))
        ;; flat contract
        (let ([then-pred (flat-contract-predicate then-ctc)]
              [else-pred (flat-contract-predicate else-ctc)])
          (define (pred x)
            (if (predicate x) (then-pred x) (else-pred x)))
          (flat-named-contract name pred))
        ;; ho contract
        (let ([then-proj (contract-projection then-ctc)]
              [then-fo (contract-first-order then-ctc)]
              [else-proj (contract-projection else-ctc)]
              [else-fo (contract-first-order else-ctc)])
          (define ((proj blame) x)
            (if (predicate x)
                ((then-proj blame) x)
                ((else-proj blame) x)))
          (make-contract
           #:name name
           #:projection proj
           #:first-order
           (lambda (x) (if (predicate x) (then-fo x) (else-fo x))))))))

;; failure-result/c : contract
;; Describes the optional failure argument passed to hash-ref, for example.
;; If the argument is a procedure, it must be a thunk, and it is applied. Otherwise
;; the argument is simply the value to return.
(define failure-result/c
  (if/c procedure? (-> any) any/c))

;; rename-contract : contract any/c -> contract
;; If the argument is a flat contract, so is the result.
(define (rename-contract ctc name)
  (let ([ctc (coerce-contract 'rename-contract ctc)])
    (if (flat-contract? ctc)
        (flat-named-contract name (flat-contract-predicate ctc))
        (let* ([ctc-fo (contract-first-order ctc)]
               [proj (contract-projection ctc)])
          (make-contract #:name name
                           #:projection proj
                           #:first-order ctc-fo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Flat Contracts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nat/c
  (flat-named-contract '|natural number| exact-nonnegative-integer?))

(define pos/c
  (flat-named-contract '|positive integer| exact-positive-integer?))

(define truth/c
  (flat-named-contract '|truth value| (lambda (x) #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Syntax Contracts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (syntax-datum/c datum)
  (let* ([datum/c (coerce-contract datum datum)])
    (flat-named-contract (build-compound-type-name 'syntax-datum/c datum/c)
      (lambda (v)
        (and (syntax? v)
             ((flat-contract-predicate datum/c)
              (syntax->datum v)))))))

(define (syntax-listof/c elem)
  (let* ([elem/c (coerce-contract elem elem)])
    (flat-named-contract (build-compound-type-name 'syntax-listof/c elem/c)
      (lambda (v)
        (and (syntax? v)
             ((flat-contract-predicate (listof elem/c))
              (syntax->list v)))))))

(define (syntax-list/c . elems)
  (let* ([elem/cs (map (lambda (elem) (coerce-contract elem elem)) elems)])
    (flat-named-contract (apply build-compound-type-name 'syntax-list/c elem/cs)
      (lambda (v)
        (and (syntax? v)
             ((flat-contract-predicate (apply list/c elem/cs))
              (syntax->list v)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Function Contracts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define thunk/c (-> any/c))
(define unary/c (-> any/c any/c))
(define binary/c (-> any/c any/c any/c))
(define predicate/c (-> any/c boolean?))
(define comparison/c (-> any/c any/c boolean?))
(define predicate-like/c (-> any/c truth/c))
(define comparison-like/c (-> any/c any/c truth/c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Contracted Sequences
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sequence/c . elem/cs)
  (let* ([elem/cs (for/list ([elem/c (in-list elem/cs)])
                    (coerce-contract 'sequence/c elem/c))]
         [n-cs (length elem/cs)])
    (make-proj-contract
     (apply build-compound-type-name 'sequence/c elem/cs)
     (lambda (pos neg src name blame)
       (lambda (seq)
         (unless (sequence? seq)
           (raise-contract-error
            seq src pos name
            "expected a sequence, got: ~e"
            seq))
           (make-do-sequence
            (lambda ()
              (let*-values ([(more? next) (sequence-generate seq)])
                (values
                 (lambda (idx)
                   (call-with-values next
                     (lambda elems
                       (define n-elems (length elems))
                       (unless (= n-elems n-cs)
                         (raise-contract-error
                          seq src pos name
                          "expected a sequence of ~a values, got ~a values: ~s"
                          n-cs n-elems elems))
                       (apply
                        values
                        (for/list ([elem (in-list elems)]
                                   [elem/c (in-list elem/cs)])
                          ((((proj-get elem/c) elem/c) pos neg src name blame) elem))))))
                 (lambda (idx) idx)
                 #f
                 (lambda (idx) (more?))
                 (lambda (elem) #t)
                 (lambda (idx elem) #t)))))))
     sequence?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Contracted Dictionaries
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A CDict is (make-contracted-dictionary (Listof (Cons Proj Proj)) Dict)
;; A Proj is (make-projection Contract Symbol Symbol Any Any)
(define-struct contracted-dictionary [projections bindings])
(define-struct projection [contract out in source name blame])

(define (dict/c key/c value/c)
  (let* ([key/c (coerce-contract 'dict/c key/c)]
         [value/c (coerce-contract 'dict/c value/c)])
    (make-proj-contract
     (build-compound-type-name 'dict/c key/c value/c)
     (lambda (pos neg src name blame)
       (lambda (dict)
         (unless (dict? dict)
           (raise-contract-error dict src pos name
                                 "expected a dictionary, got: ~e"
                                 dict))
         (wrap
          (cons (cons (make-projection key/c pos neg src name blame)
                      (make-projection value/c pos neg src name blame))
                (dict->projections dict))
          (dict->bindings dict))))
     dict?)))

(define-match-expander cdict
  (syntax-rules () [(_ p b) (struct contracted-dictionary [p b])]))

(define-match-expander proj
  (syntax-rules () [(_ c o i s n b) (struct projection [c o i s n b])]))

(define -ref
  (case-lambda
    [(dict key)
     (match dict
       [(cdict projs binds)
        (let* ([key (key-in projs key)])
          (value-out projs (dict-ref binds key)))])]
    [(dict key failure)
     (match dict
       [(cdict projs binds)
        (let* ([key (key-in projs key)])
          (let/ec return
            (define (fail)
              (return (if (procedure? failure) (failure) failure)))
            (value-out projs (dict-ref binds key fail))))])]))

(define (-set! dict key value)
  (match dict
    [(cdict projs binds)
     (dict-set! binds (key-in projs key) (value-in projs value))]))

(define (-set dict key value)
  (match dict
    [(cdict projs binds)
     (wrap projs (dict-set binds (key-in projs key) (value-in projs value)))]))

(define (-rem! dict key)
  (match dict
    [(cdict projs binds)
     (dict-remove! binds (key-in projs key))]))

(define (-rem dict key)
  (match dict
    [(cdict projs binds)
     (wrap projs (dict-remove binds (key-in projs key)))]))

(define (-size dict)
  (match dict
    [(cdict projs binds)
     (dict-count binds)]))

(define (-fst dict)
  (match dict
    [(cdict projs binds)
     (dict-iterate-first binds)]))

(define (-nxt dict iter)
  (match dict
    [(cdict projs binds)
     (dict-iterate-next binds iter)]))

(define (-key dict iter)
  (match dict
    [(cdict projs binds)
     (key-out projs (dict-iterate-key binds iter))]))

(define (-val dict iter)
  (match dict
    [(cdict projs binds)
     (value-out projs (dict-iterate-value binds iter))]))

(define (key-in projs key)
  (if (null? projs)
      key
      (key-in (cdr projs) (project-in (caar projs) key))))

(define (value-in projs value)
  (if (null? projs)
      value
      (value-in (cdr projs) (project-in (cdar projs) value))))

(define (key-out projs key)
  (if (null? projs)
      key
      (project-out (caar projs) (key-out (cdr projs) key))))

(define (value-out projs value)
  (if (null? projs)
      value
      (project-out (cdar projs) (value-out (cdr projs) value))))

(define (project-in p x)
  (match p
    [(proj c o i s n b)
     ((((proj-get c) c) i o s n (not b)) x)]))

(define (project-out p x)
  (match p
    [(proj c o i s n b)
     ((((proj-get c) c) o i s n b) x)]))

(define (dict->bindings dict)
  (match dict
    [(cdict projs binds) binds]
    [_ dict]))

(define (dict->projections dict)
  (match dict
    [(cdict projs binds) projs]
    [_ null]))

(define (wrap projs binds)
  ((dict->wrapper binds) projs binds))

(define (dict->wrapper dict)
  (if (dict-mutable? dict)
      (if (dict-can-functional-set? dict)
          (if (dict-can-remove-keys? dict) make-:!+- make-:!+_)
          (if (dict-can-remove-keys? dict) make-:!_- make-:!__))
      (if (dict-can-functional-set? dict)
          (if (dict-can-remove-keys? dict) make-:_+- make-:_+_)
          (if (dict-can-remove-keys? dict) make-:__- make-:___))))

;; The __- case (removal without functional or mutable update) is nonsensical.
(define prop:!+- (vector -ref -set! -set -rem! -rem -size -fst -nxt -key -val))
(define prop:!+_ (vector -ref -set! -set  #f    #f  -size -fst -nxt -key -val))
(define prop:!_- (vector -ref -set!  #f  -rem!  #f  -size -fst -nxt -key -val))
(define prop:!__ (vector -ref -set!  #f   #f    #f  -size -fst -nxt -key -val))
(define prop:_+- (vector -ref  #f   -set  #f   -rem -size -fst -nxt -key -val))
(define prop:_+_ (vector -ref  #f   -set  #f   -rem -size -fst -nxt -key -val))
(define prop:__- (vector -ref  #f    #f   #f    #f  -size -fst -nxt -key -val))
(define prop:___ (vector -ref  #f    #f   #f    #f  -size -fst -nxt -key -val))

;; The __- case (removal without functional or mutable update) is nonsensical.
(define-struct (:!+- contracted-dictionary) [] #:property prop:dict prop:!+-)
(define-struct (:!+_ contracted-dictionary) [] #:property prop:dict prop:!+_)
(define-struct (:!_- contracted-dictionary) [] #:property prop:dict prop:!_-)
(define-struct (:!__ contracted-dictionary) [] #:property prop:dict prop:!__)
(define-struct (:_+- contracted-dictionary) [] #:property prop:dict prop:_+-)
(define-struct (:_+_ contracted-dictionary) [] #:property prop:dict prop:_+_)
(define-struct (:__- contracted-dictionary) [] #:property prop:dict prop:__-)
(define-struct (:___ contracted-dictionary) [] #:property prop:dict prop:___)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Exports
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [path-element? contract?]
 [port-number? contract?]
 [tcp-listen-port? contract?]

 [non-empty-string? predicate/c]
 [non-empty-bytes? predicate/c]
 [non-empty-vector? predicate/c]
 [non-empty-list? predicate/c]
 [singleton-list? predicate/c]

 [if/c (-> procedure? contract? contract? contract?)]
 [failure-result/c contract?]
 [rename-contract (-> contract? any/c contract?)]

 [nat/c flat-contract?]
 [pos/c flat-contract?]
 [truth/c flat-contract?]

 [thunk/c contract?]
 [unary/c contract?]
 [binary/c contract?]
 [predicate/c contract?]
 [comparison/c contract?]
 [predicate-like/c contract?]
 [comparison-like/c contract?]

 [syntax-datum/c (-> flat-contract? flat-contract?)]
 [syntax-listof/c (-> flat-contract? flat-contract?)]
 [syntax-list/c
  (->* [] [] #:rest (listof flat-contract?) flat-contract?)]

 [sequence/c (->* [] [] #:rest (listof contract?) contract?)]
 [dict/c (-> contract? contract? contract?)])
