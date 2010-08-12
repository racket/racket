#lang racket/base

(require racket/dict racket/match racket/contract unstable/contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  "Missing" Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dict-has-key?
  (let ()
    (with-contract
     dict-has-key?
     ([dict-has-key? (-> dict? any/c boolean?)])
     (define (dict-has-key? dict key)
       (let/ec return
         (dict-ref dict key (lambda () (return #f)))
         #t)))
    dict-has-key?))
;; Ryan: Why the with-contract region? Why not provide/contract?

(define (dict-empty? dict)
  (= (dict-count dict) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Constructors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (empty-dict #:weak? [weak? #f]
                    #:mutable? [mutable? weak?]
                    #:compare [compare 'equal])
  (match* [mutable? weak? compare]
    ;; Immutable
    ([#f #f 'equal] (make-immutable-hash null))
    ([#f #f 'eqv] (make-immutable-hasheqv null))
    ([#f #f 'eq] (make-immutable-hasheq null))
    ;; Mutable
    ([#t #f 'equal] (make-hash))
    ([#t #f 'eqv] (make-hasheqv))
    ([#t #f 'eq] (make-hasheq))
    ;; Weak
    ([#t #t 'equal] (make-weak-hash))
    ([#t #t 'eqv] (make-weak-hash))
    ([#t #t 'eq] (make-weak-hash))
    ;; Impossible
    ([#f #t _] (error 'empty-set "cannot create an immutable weak hash"))))

(define (make-dict dict
                   #:weak? [weak? #f]
                   #:mutable? [mutable? weak?]
                   #:compare [compare 'equal])
  (let* ([MT (empty-dict #:mutable? mutable? #:weak? weak? #:compare compare)])
    (if mutable?
      (begin (dict-union! MT dict) MT)
      (dict-union MT dict))))

(define (custom-dict equiv?
                     [hash1 (lambda (x) 0)]
                     [hash2 (lambda (x) 0)]
                     #:weak? [weak? #f]
                     #:mutable? [mutable? weak?])
  (match* [mutable? weak?]
    ([#f #f] (make-immutable-custom-hash equiv? hash1 hash2))
    ([#t #f] (make-custom-hash equiv? hash1 hash2))
    ([#t #t] (make-weak-custom-hash equiv? hash1 hash2))
    ([#f #t] (error 'custom-set "cannot create an immutable weak hash"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Ref Wrappers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dict-ref/check dict key)
  (dict-ref dict key))

(define (dict-ref/identity dict key)
  (dict-ref dict key (lambda () key)))

(define (dict-ref/default dict key default)
  (dict-ref dict key (lambda () default)))

(define (dict-ref/failure dict key failure)
  (dict-ref dict key (lambda () (failure))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Union
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((dict-duplicate-error name) key value1 value2)
  (error name "duplicate values for key ~e: ~e and ~e" key value1 value2))

(define (dict-union
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                          (lambda (k x y) (combine x y))
                          (dict-duplicate-error 'dict-union))]
         one . rest)
  (for*/fold ([one one]) ([two (in-list rest)] [(k v) (in-dict two)])
    (dict-set one k (if (dict-has-key? one k)
                        (combine/key k (dict-ref one k) v)
                        v))))

(define (dict-union!
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                          (lambda (k x y) (combine x y))
                          (dict-duplicate-error 'dict-union))]
         one . rest)
  (for* ([two (in-list rest)] [(k v) (in-dict two)])
    (dict-set! one k (if (dict-has-key? one k)
                         (combine/key k (dict-ref one k) v)
                         v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Property delegation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (wrapped-dict-property
         #:unwrap unwrap
         #:wrap [wrap #f]
         #:predicate [pred (lambda (x) #t)]
         #:mutable? [mutable? #t]
         #:functional? [functional? (if wrap #t #f)]
         #:remove? [remove? #t])
  (let* ([unwrap (protect-unwrap pred unwrap)]
         [wrap (and wrap (protect-wrap pred wrap))])
    (vector (wrapped-ref unwrap)
            (and mutable? (wrapped-set! unwrap))
            (and functional? wrap (wrapped-set unwrap wrap))
            (and mutable? remove? (wrapped-remove! unwrap))
            (and functional? remove? wrap (wrapped-remove unwrap wrap))
            (wrapped-count unwrap)
            (wrapped-iterate-first unwrap)
            (wrapped-iterate-next unwrap)
            (wrapped-iterate-key unwrap)
            (wrapped-iterate-value unwrap))))

(define ((protect-unwrap pred unwrap) op x)
  (unless (pred x)
    (raise
     (make-exn:fail:contract
      (format "~a: expected a <~a>, but got: ~e"
              op (object-name pred) x)
      (current-continuation-marks))))
  (unwrap x))

(define ((protect-wrap pred wrap) op x)
  (let* ([y (wrap x)])
    (unless (pred y)
      (raise
       (make-exn:fail:contract
        (format "~a: tried to construct a <~a>, but got: ~e"
                op (object-name pred) x)
        (current-continuation-marks))))
    y))

(define (wrapped-ref unwrap)
  (case-lambda
    [(dict key) (dict-ref (unwrap 'dict-ref dict) key)]
    [(dict key fail) (dict-ref (unwrap 'dict-ref dict) key fail)]))

(define ((wrapped-set! unwrap) dict key value)
  (dict-set! (unwrap 'dict-set! dict) key value))

(define ((wrapped-set unwrap wrap) dict key value)
  (wrap 'dict-set (dict-set (unwrap 'dict-set dict) key value)))

(define ((wrapped-remove! unwrap) dict key)
  (dict-remove! (unwrap 'dict-remove! dict) key))

(define ((wrapped-remove unwrap wrap) dict key)
  (wrap 'dict-remove (dict-remove (unwrap 'dict-remove dict) key)))

(define ((wrapped-count unwrap) dict)
  (dict-count (unwrap 'dict-count dict)))

(define ((wrapped-iterate-first unwrap) dict)
  (dict-iterate-first (unwrap 'dict-iterate-first dict)))

(define ((wrapped-iterate-next unwrap) dict pos)
  (dict-iterate-next (unwrap 'dict-iterate-next dict) pos))

(define ((wrapped-iterate-key unwrap) dict pos)
  (dict-iterate-key (unwrap 'dict-iterate-key dict) pos))

(define ((wrapped-iterate-value unwrap) dict pos)
  (dict-iterate-value (unwrap 'dict-iterate-value dict) pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Exports
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide dict/c dict-has-key? dict-ref!)
(provide/contract
 [dict-empty? (-> dict? boolean?)]
 [empty-dict
  (->* []
       [#:mutable? boolean? #:weak? boolean? #:compare (or/c 'eq 'eqv 'equal)]
       hash?)]
 [make-dict
  (->* [dict?]
       [#:mutable? boolean? #:weak? boolean? #:compare (or/c 'eq 'eqv 'equal)]
       hash?)]
 [custom-dict
  (->* [(-> any/c any/c any/c)]
       [(-> any/c exact-integer?) (-> any/c exact-integer?)
        #:mutable? boolean? #:weak? boolean?]
       dict?)]
 [wrapped-dict-property
  (->* [#:unwrap (-> dict? dict?)]
       [#:wrap (-> dict? dict?)
        #:predicate (-> any/c boolean?)
        #:mutable? boolean?
        #:remove? boolean?
        #:functional? boolean?]
       vector?)]
 [dict-ref/identity (-> dict? any/c any/c)]
 [dict-ref/default (-> dict? any/c any/c any/c)]
 [dict-ref/failure (-> dict? any/c (-> any/c) any/c)]
 [dict-ref/check
  (->d ([table dict?] [key any/c]) ()
       #:pre-cond (dict-has-key? table key)
       [_ any/c])]
 [dict-domain (-> dict? list?)]
 [dict-range (-> dict? list?)]
 [dict-union (->* [(and/c dict? dict-can-functional-set?)]
                  [#:combine
                   (-> any/c any/c any/c)
                   #:combine/key
                   (-> any/c any/c any/c any/c)]
                  #:rest (listof dict?)
                  (and/c dict? dict-can-functional-set?))]
 [dict-union! (->* [(and/c dict? dict-mutable?)]
                   [#:combine
                   (-> any/c any/c any/c)
                   #:combine/key
                   (-> any/c any/c any/c any/c)]
                   #:rest (listof dict?)
                   void?)])
