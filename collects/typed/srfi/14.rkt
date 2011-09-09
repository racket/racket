#lang typed/racket/base
(require/opaque-type Char-Set char-set? srfi/14)

(define-type-alias Cursor (Pair 0 (Listof (Pair Integer Integer))))

(provide Char-Set Cursor)

(require/typed
 srfi/14
 ;; Predicates & comparison
 [char-set= (Char-Set * -> Boolean)]
 [char-set<= (Char-Set * -> Boolean)]
 [char-set-hash
  (case-lambda (Char-Set -> Integer)
               (Char-Set Integer -> Integer))]

 ;; Iterating over character sets
 [char-set-cursor (Char-Set -> Cursor)]
 [char-set-ref (Char-Set Cursor -> Char)]
 [char-set-cursor-next (Char-Set Cursor -> Cursor)]
 [end-of-char-set? (Cursor -> Boolean)]
 [char-set-map ((Char -> Char) Char-Set -> Char-Set)]

 ;; Creating character sets
 [char-set-copy (Char-Set -> Char-Set)]
 [char-set (Char * -> Char-Set)]
 [list->char-set
  (case-lambda
    ((Listof Char) -> Char-Set)
    ((Listof Char) Char-Set -> Char-Set))]
 [list->char-set! ((Listof Char) Char-Set -> Char-Set)]
 [string->char-set
  (case-lambda
    (String -> Char-Set)
    (String Char-Set -> Char-Set))]
 [string->char-set! (String Char-Set -> Char-Set)]
 [char-set-filter
  (case-lambda
    ((Char -> Any) Char-Set -> Char-Set)
    ((Char -> Any) Char-Set Char-Set -> Char-Set))]
 [char-set-filter!
  ((Char -> Any) Char-Set Char-Set -> Char-Set)]
 [ucs-range->char-set
  (case-lambda (Integer Integer -> Char-Set)
               (Integer Integer Any -> Char-Set)
               (Integer Integer Any Char-Set -> Char-Set))]
 [ucs-range->char-set!
  (Integer Integer Any Char-Set -> Char-Set)]
 [->char-set ((U String Char Char-Set) -> Char-Set)]

 ;; Querying character sets
 [char-set-size (Char-Set -> Integer)]
 [char-set-count ((Char -> Any) Char-Set -> Integer)]
 [char-set->list (Char-Set -> (Listof Char))]
 [char-set->string (Char-Set -> String)]
 [char-set-contains? (Char-Set Char -> Boolean)]

 ;; Character-set algebra
 [char-set-adjoin  (Char-Set Char * -> Char-Set)]
 [char-set-delete  (Char-Set Char * -> Char-Set)]
 [char-set-adjoin! (Char-Set Char * -> Char-Set)]
 [char-set-delete! (Char-Set Char * -> Char-Set)]
 [char-set-complement   (Char-Set -> Char-Set)]
 [char-set-union        (Char-Set * -> Char-Set)]
 [char-set-intersection (Char-Set * -> Char-Set)]
 [char-set-difference   (Char-Set Char-Set * -> Char-Set)]
 [char-set-xor          (Char-Set * -> Char-Set)]
 [char-set-diff+intersection
  (Char-Set Char-Set * -> (values Char-Set Char-Set))]
 [char-set-complement!   (Char-Set -> Char-Set)]
 [char-set-union!        (Char-Set Char-Set * -> Char-Set)]
 [char-set-intersection! (Char-Set Char-Set * -> Char-Set)]
 [char-set-difference!   (Char-Set Char-Set * -> Char-Set)]
 [char-set-xor!          (Char-Set Char-Set * -> Char-Set)]
 [char-set-diff+intersection!
  (Char-Set Char-Set Char-Set * -> (values Char-Set Char-Set))]

 ;; Standard character sets
 [char-set:lower-case Char-Set]
 [char-set:upper-case Char-Set]
 [char-set:title-case Char-Set]
 [char-set:letter Char-Set]
 [char-set:digit Char-Set]
 [char-set:letter+digit Char-Set]
 [char-set:graphic Char-Set]
 [char-set:printing Char-Set]
 [char-set:whitespace Char-Set]
 [char-set:iso-control Char-Set]
 [char-set:punctuation Char-Set]
 [char-set:symbol Char-Set]
 [char-set:hex-digit Char-Set]
 [char-set:blank Char-Set]
 [char-set:ascii Char-Set]
 [char-set:empty Char-Set]
 [char-set:full Char-Set]
 [char-set-fold (All (A) ((Char A -> A) A Char-Set -> A))]
 [char-set-unfold
  (All (A)
       (case-lambda
        ((A -> Any) (A -> Char) (A -> A) A -> Char-Set)
        ((A -> Any) (A -> Char) (A -> A) A Char-Set -> Char-Set)))]
 [char-set-unfold!
  (All (A) ((A -> Any) (A -> Char) (A -> A) A Char-Set -> Char-Set))]
 [char-set-for-each (All (A) ((Char -> A) Char-Set -> (U A Void)))]
 [char-set-any (All (A) ((Char -> A) Char-Set -> (U A #f)))]
 [char-set-every (All (A) ((Char -> A) Char-Set -> (U A Boolean)))]
 ) ; end of require/typed

;; Definitions provided here for polymorphism
#;
(define (char-set-fold comb base cs)
  (let loop ((c (char-set-cursor cs)) (b base))
    (cond [(end-of-char-set? c) b]
          [else
           (loop (char-set-cursor-next cs c)
                 (comb (char-set-ref cs c) b))])))
#;
(define char-set-unfold
  (pcase-lambda: (A)
     [([p : (A -> Any)] [f : (A -> Char)] [g : (A -> A)] [seed : A])
      (char-set-unfold p f g seed char-set:empty)]
     [([p : (A -> Any)] [f : (A -> Char)] [g : (A -> A)] [seed : A]
                        [base-cs : Char-Set])
      (char-set-unfold! p f g seed (char-set-copy base-cs))]))
#;
(define (char-set-unfold! p f g seed base-cs)
  (let lp ((seed seed) (cs base-cs))
        (if (p seed) cs                                 ; P says we are done.
            (lp (g seed)                                ; Loop on (G SEED).
                (char-set-adjoin! cs (f seed))))))

#;
(define (char-set-for-each f cs)
  (char-set-fold (lambda: ([c : Char] [b : (U A Void)]) (f c))
                 (void)
                 cs))
#;
(define (char-set-any pred cs)
  (let loop ((c (char-set-cursor cs)))
    (and (not (end-of-char-set? c))
         (or (pred (char-set-ref cs c))
             (loop (char-set-cursor-next cs c))))))
#;
(define (char-set-every pred cs)
  (let loop ((c (char-set-cursor cs)) (b (ann #t (U #t A))))
    (cond [(end-of-char-set? c) b]
          [else (and b
                     (loop (char-set-cursor-next cs c)
                           (pred (char-set-ref cs c))))])))

(provide
 ;; Predicates & comparison
 char-set?
 char-set=
 char-set<=
 char-set-hash

 ;; Iterating over character sets
 char-set-cursor
 char-set-ref
 char-set-cursor-next
 end-of-char-set?
 char-set-fold
 char-set-unfold
 char-set-unfold!
 char-set-for-each
 char-set-map

 ;; Creating character sets
 char-set-copy
 char-set
 list->char-set
 list->char-set!
 string->char-set
 string->char-set!
 char-set-filter
 char-set-filter!
 ucs-range->char-set
 ucs-range->char-set!
 ->char-set

 ;; Querying character sets
 char-set-size
 char-set-count
 char-set->list
 char-set->string
 char-set-contains?
 char-set-every
 char-set-any

 ;; Character-set algebra
 char-set-adjoin
 char-set-delete
 char-set-adjoin!
 char-set-delete!
 char-set-complement
 char-set-union
 char-set-intersection
 char-set-difference
 char-set-xor
 char-set-diff+intersection
 char-set-complement!
 char-set-union!
 char-set-intersection!
 char-set-difference!
 char-set-xor!
 char-set-diff+intersection!

 ;; Standard character sets
 char-set:lower-case
 char-set:upper-case
 char-set:title-case
 char-set:letter
 char-set:digit
 char-set:letter+digit
 char-set:graphic
 char-set:printing
 char-set:whitespace
 char-set:iso-control
 char-set:punctuation
 char-set:symbol
 char-set:hex-digit
 char-set:blank
 char-set:ascii
 char-set:empty
 char-set:full
 ) ; end of provide
