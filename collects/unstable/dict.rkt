#lang racket/base

(require racket/dict racket/match racket/contract unstable/contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  "Missing" Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dict-empty? dict)
  (not (dict-iterate-first dict)))
;; Eli: This encourages ignoring the actual representation, and the fact
;; that `dict-count' can in some cases be an O(N) operation.  (And to
;; make things worse, it's not even mentioned in the docs.)
;; Ryan: Fixed complexity.

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
;; Eli: What's the point in this?  The whole dict thing is very similar
;; to an abstract class, and this code is essentially making a
;; constructor for the abstract class that decides to instantiate some
;; arbitrary subclass.  Furthermore, since this arbitrary decision is
;; always going for a hash table, this whole function is nothing more
;; than a keyworded version of `make-hash-*'.  (As a very obvious
;; example, if I have a mental model of alists, using this function
;; makes things much less efficient than just returning `null'.)  This
;; is possibly something useful, but calling it `make-dict' is bogus.
;; Another evidence for this bogosity: the documentation for this
;; function says: "Constructs an empty >>hash table<<".

(define (make-dict dict
                   #:weak? [weak? #f]
                   #:mutable? [mutable? weak?]
                   #:compare [compare 'equal])
  (let* ([MT (empty-dict #:mutable? mutable? #:weak? weak? #:compare compare)])
    (if mutable?
      (begin (dict-union! MT dict) MT)
      (dict-union MT dict))))
;; Eli: Similar bogosity to the above.  When I see `make-dict', I don't
;; think about a function that "Converts a given dictionary to a hash
;; table".  If it's useful, then it should have a more straightforward
;; name, like `dict->hash'.  Also, reusing `dict-union' is cute, but
;; makes it slower than it could be.

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
;; Eli: Again, same bogosity comment applies here.  Another point here:
;; using 0 for the default hashing functions sounds like a very bad idea
;; -- something that people will run into in the form of extremely bad
;; performance.  In this case the docs do mention this -- but why not
;; use the default hash functions that racket already provides?  Also,
;; the docs indicate that the degenerate hash function makes it
;; equivalent to a list-based dictionary, which is wrong: relying on
;; this seems bad (in case custom hashes are (or will be) more
;; sophisticated), and also it's equivalent to a list-based dictionary,
;; except with a costly constant factor for the hash machinery, and
;; without the advantages of an alist (order).  In short, the docs
;; should really say "don't use this without hash functions" -- or
;; better, use the better hash functions as a default *or* don't make
;; them optional (at least the first one).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Ref Wrappers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Eli: Ugh.  So the above constructors are keyworded versions of hash
;; constructors in various forms, and here we take a *single* function
;; from the dict interface and split it into multiple functions?  Is
;; there any point for this?  If I were told just this high-level
;; description, I'd assume that an obvious motivation for doing this
;; would be performance, but in this case performance is lost.  I also
;; lose the abolity to have a lazily computed default on the way, since
;; the default in `dict-ref/default' is a plain argument.  The only new
;; thing here is the questionable `dict-ref/identity' (at least I have
;; never seen any code where something like that would be useful).

(define (dict-ref/check dict key)
  (dict-ref dict key))
;; Eli: why the eta-expanded definition?

(define (dict-ref/identity dict key)
  (dict-ref dict key (lambda () key)))

(define (dict-ref/default dict key default)
  (dict-ref dict key (lambda () default)))

(define (dict-ref/failure dict key failure)
  (dict-ref dict key (lambda () (failure))))
;; Eli: Um, why (lambda () (failure)) and not just `failure'??

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Union
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((dict-duplicate-error name) key value1 value2)
  (error name "duplicate values for key ~e: ~e and ~e" key value1 value2))

;; Eli: If this is useful, then at least make it worth using instead of
;; writing your own code.  For example, inspect the arguments and choose
;; an efficient order for the loops, or use a temporary hash table for a
;; union of two alists, etc.  Alternatively, just write a function for
;; merging two hash tables (and call it `hash-union', of course).

;; Ryan: I prefer the names dict-add-all and dict-add-all!---no connotations
;; of symmetry, and it makes it clear that the first argument determines the
;; representation (and key constraints, etc).

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

;; Eli: Uh, what is this for?  The documentation for this is unclear: it
;; has the technical details of what this is doing, but no explanation
;; about when this is useful.  Specifically, it's unclear why I would
;; ever want a wrapped dictionary.  (My best guess would be "something
;; that chaperons are a better solution for".)

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

(provide dict/c)
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
  (->i ([table dict?] [key any/c]) ()
       #:pre (table key) (dict-has-key? table key)
       [res any/c])]
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
