#lang at-exp scheme/base
  (require "teachprims.rkt"
           mzlib/etc
           mzlib/list
           mzlib/pretty
           syntax/docprovide
           scheme/promise
           scheme/port
           "../posn.rkt"
           (for-syntax scheme/base))

;; Documents the procedures:
(require "provide-and-scribble.rkt")

(define pp (let ([pretty-print (lambda (v) (pretty-write v))]) pretty-print))

(provide-and-scribble
   procedures

   (begin
     (require scribble/manual scribble/eval "sl-eval.rkt")
     (define (asl)
       (define *bsl
         (asl-eval
          (require 2htdp/image)
          (define c1 (circle 10 "solid" "green"))

          (define zero 0)

          (define one (list 1))

          ;; b 69, c 42, e 61
          (define list-for-hash '((b . 69) (c . 42) (e . 61) (r . 999)))

          (define hsh (make-hash list-for-hash))
          (define heq (make-hasheq list-for-hash))
          (define heqv (make-hasheqv list-for-hash))
          (define ish (make-immutable-hash list-for-hash))
          (define ieq (make-immutable-hasheq list-for-hash))
          (define ieqv (make-immutable-hasheqv list-for-hash))

          (define q (make-posn "bye" 2))
          (define p (make-posn 2 -3))
          
          (define v (vector "a" "b" "c" "d" "e"))
          
          (define b (box 33))

          (define s "hello world")
          (define x (list 2 "hello" true))))
       (set! asl (lambda () *bsl))
       *bsl))
   
   ("Numbers: Integers, Rationals, Reals, Complex, Exacts, Inexacts"
    @defproc[(random [x integer]) integer]{
    Generates a random natural number less than some given integer
    In ASL: @racket[random] generate a random inexact number between 0.0
    and 1.0 exclusive when applied to no argument.
    @interaction[#:eval (asl) (random)]
    } 
    )

   ("Reading and Printing"
    @defproc[(with-input-from-file [f string] [p (-> any)]) any]{
    Opens the named input file @racket[f] and allows @racket[p] to read from it.
    } 
    @defproc[(with-output-to-file [f string] [p (-> any)]) any]{
    Opens the named input file @racket[f] and allows @racket[p] to write to it.
    } 
    @defproc[(with-input-from-string [s string] [p (-> any)]) any]{
    Turns @racket[s] into input for @racket[read] operations in @racket[p].
    @interaction[#:eval (asl)
      (with-input-from-string "hello" read)
      (string-length (symbol->string (with-input-from-string "hello" read)))]
    } 
    @defproc[(with-output-to-string [p (-> any)]) any]{
    Produces a string from all write/display/print operations in @racket[p].
    @interaction[#:eval (asl)
      (with-output-to-string (lambda () (display 10)))]
    } 
    
    @defproc[(print [x any]) void]{
    Prints the argument as a value.
    @interaction[#:eval (asl)
      (print 10)
      (print "hello")
      (print 'hello)]
    } 
    @defproc[(display [x any]) void]{
    Prints the argument to stdout (without quotes on symbols and strings, etc.).
    @interaction[#:eval (asl)
      (display 10)
      (display "hello")
      (display 'hello)]
    } 
    @defproc[(write [x any]) void]{
    Prints the argument to stdout (in a traditional style that is somewhere between @racket[print] and @racket[display]).
    @interaction[#:eval (asl)
      (write 10)
      (write "hello")
      (write 'hello)]
    } 
    @defproc[((pp pretty-print) [x any]) void]{
    Pretty prints S-expressions (like @racket[write]). 
    @interaction[#:eval (asl)
                  (pretty-print '((1 2 3) ((a) ("hello world" true) (((false "good bye"))))))
                  (pretty-print (build-list 10 (lambda (i) (build-list 10 (lambda (j) (= i j))))))
                  ]
    }

    @defproc[(printf [f string] [x any] ...) void]{
    Formats the rest of the arguments according to the first argument and print it. } 

    @defproc[(newline) void]{
    Prints a newline.} 

    @defproc[(read) sexp]{
    Reads input from the user.}) 
   
   ("Lists"
    @defproc[(list? [x any]) boolean]{
    Determines whether some value is a list. 
    In ASL, @racket[list?] also deals with cyclic lists. 
    } 
    @defproc[((advanced-list* list*) [x any] ... [l (listof any)]) (listof any)]{
    Constructs a list by adding multiple items to a list.
    In ASL, @racket[list*] also deals with cyclic lists. 
    } 
    @defproc[((advanced-cons cons) [x X] [l (listof X)]) (listof X)]{
    Constructs a list.
    In ASL, @racket[cons] creates a mutable list. 
    } 
    @defproc[((advanced-append append) [l (listof any)] ...) (listof any)]{
    Creates a single list from several.
    In ASL, @racket[list*] also deals with cyclic lists. 
    } 
    @defproc[(assoc [x any] [l (listof any)]) (union (listof any) false)]{
    Produces the first pair on @racket[l] whose @racket[first] is @racket[equal?] to @racket[x];
     otherwise it produces @racket[false].
    @interaction[#:eval (asl) (assoc "hello" '(("world" 2) ("hello" 3) ("good" 0)))]
    }) 
   
   ("Misc"
    @defproc[(gensym) symbol?]{
    Generates a new symbol, different from all symbols in the program.
    @interaction[#:eval (asl) (gensym)]
    } 
    @defproc[(sleep [sec positive-num]) void]{
    Causes the program to sleep for the given number of seconds.
    } 
    @defproc[(current-milliseconds) exact-integer]{
    Returns the current “time” in fixnum milliseconds (possibly negative).
    @interaction[#:eval (asl) (current-milliseconds)]
    } 
    @defproc[(force [v any]) any]{
    Finds the delayed value; see also delay.
    } 
    @defproc[(promise? [x any]) boolean?]{
    Determines if a value is delayed.
    } 
    @defproc[(void) void?]{
    Produces a void value.
    @interaction[#:eval (asl) (void)]
    } 
    @defproc[(void? [x any]) boolean?]{
    Determines if a value is void.
    @interaction[#:eval (asl) (void? (void)) (void? 42)]
    }) 
   
   ("Posns"
    @defproc[(set-posn-x! [p posn] [x any]) void?]{
    Updates the x component of a posn.
    @interaction[#:eval (asl) p (set-posn-x! p 678) p]
    } 
    @defproc[(set-posn-y! [p posn] [x any]) void]{
    Updates the y component of a posn.
    @interaction[#:eval (asl) q (set-posn-y! q 678) q]
    }) 
   
   ("Vectors"
    @defproc[(vector [x X] ...) (vector X ...)]{
    Constructs a vector from the given values.
    @interaction[#:eval (asl) (vector 1 2 3 -1 -2 -3)]
    } 
    @defproc[(make-vector [n number] [x X]) (vectorof X)]{
    Constructs a vector of @racket[n] copies of @racket[x].
    @interaction[#:eval (asl) (make-vector 5 0)]
    } 
    @defproc[(build-vector [n nat] [f (nat -> X)]) (vectorof X)]{
    Constructs a vector by applying @racket[f] to the numbers @racket[0] through @racket[(- n 1)]. 
    @interaction[#:eval (asl) (build-vector 5 add1)]
    }
    @defproc[(vector-ref [v (vector X)] [n nat]) X]{
    Extracts the @racket[n]th element from @racket[v].
    @interaction[#:eval (asl) v (vector-ref v 3)]
    } 
    @defproc[(vector-length [v (vector X)]) nat]{
    Determines the length of @racket[v].
    @interaction[#:eval (asl) v (vector-length v)]
    } 
    @defproc[(vector-set! [v (vectorof X)][n nat][x X]) void]{
    Updates @racket[v] at position @racket[n] to be @racket[x].
    @interaction[#:eval (asl) v (vector-set! v 3 77) v]
    } 
    @defproc[(vector->list [v (vectorof X)]) (listof X)]{
    Transforms @racket[v] into a list. 
    @interaction[#:eval (asl) (vector->list (vector 'a 'b 'c))]
    } 
    @defproc[(list->vector [l (listof X)]) (vectorof X)]{
    Transforms @racket[l] into a vector. 
    @interaction[#:eval (asl) (list->vector (list "hello" "world" "good" "bye"))]
    } 
    @defproc[(vector? [x any]) boolean]{
    Determines if a value is a vector.
    @interaction[#:eval (asl) v (vector? v) (vector? 42)]
    }) 
   
   ("Boxes"
    @defproc[(box [x any/c]) box?]{
    Constructs a box.
    @interaction[#:eval (asl) (box 42)]
    } 
    @defproc[(unbox [b box?]) any]{
    Extracts the boxed value.
    @interaction[#:eval (asl) b (unbox b)]
    } 
    @defproc[(set-box! [b box?][x any/c]) void]{
    Updates a box.
    @interaction[#:eval (asl) b (set-box! b 31) b]
    } 
    @defproc[(box? [x any/c]) boolean?]{
    Determines if a value is a box.
    @interaction[#:eval (asl) b (box? b) (box? 42)]
    }) 
   
   ("Hash Tables"
    @defproc[((advanced-make-hash make-hash)) (hash X Y)]{
    Constructs a mutable hash table from an optional list of mappings that
    uses equal? for comparisions.
    @interaction[#:eval (asl)
      (make-hash)
      (make-hash '((b 69) (e 61) (i 999)))
      ]
    } 
    @defproc[((advanced-make-hasheq make-hasheq)) (hash X Y)]{
    Constructs a mutable hash table from an optional list of mappings that
    uses eq? for comparisions.
    @interaction[#:eval (asl)
      (make-hasheq)
      (make-hasheq '((b 69) (e 61) (i 999)))
      ] 
    } 
    @defproc[((advanced-make-hasheqv make-hasheqv)) (hash X Y)]{
    Constructs a mutable hash table from an optional list of mappings that
    uses eqv? for comparisions.
    @interaction[#:eval (asl)
      (make-hasheqv)
      (make-hasheqv '((b 69) (e 61) (i 999)))
      ]
    } 
    @defproc[((advanced-make-immutable-hash make-immutable-hash)) (hash X Y)]{
    Constructs an immutable hash table from an optional list of mappings
    that uses equal? for comparisions.
    @interaction[#:eval (asl)
      (make-immutable-hash)
      (make-immutable-hash '((b 69) (e 61) (i 999)))
      ]
    } 
    @defproc[((advanced-make-immutable-hasheq make-immutable-hasheq)) (hash X Y)]{
    Constructs an immutable hash table from an optional list of mappings
    that uses eq? for comparisions.
    @interaction[#:eval (asl)
      (make-immutable-hasheq)
      (make-immutable-hasheq '((b 69) (e 61) (i 999)))
      ]
    } 
    @defproc[((advanced-make-immutable-hasheqv make-immutable-hasheqv)) (hash X Y)]{
    Constructs an immutable hash table from an optional list of mappings
    that uses eqv? for comparisions.
    @interaction[#:eval (asl)
      (make-immutable-hasheqv)
      (make-immutable-hasheqv '((b 69) (e 61) (i 999)))
      ]
    } 
    @defproc[(hash-set! [h (hash X Y)] [k X] [v Y]) void?]{
    Updates a mutable hash table with a new mapping.
    @interaction[#:eval (asl) hsh (hash-set! hsh 'a 23) hsh]
    } 
    @defproc[(hash-set  [h (hash X Y)] [k X] [v Y]) (hash X Y)]{
    Constructs an immutable hash table with one new mapping from an
    existing immutable hash table.
    @interaction[#:eval (asl) (hash-set ish 'a 23)]
    } 
    @defproc[(hash-ref  [h (hash X Y)] [k X]) Y]{
    Extracts the value associated with a key from a hash table; the three
    ; argument case allows a default value or default value computation.
    @interaction[#:eval (asl) hsh (hash-ref hsh 'b)]
    } 
    @defproc[(hash-ref! [h (hash X Y)] [k X] [v Y]) Y]{
    Extracts the value associated with a key from a mutable hash table; if
    ; the key does not have an mapping, the third argument is used as the
    ; value (or used to compute the value) and is added to the hash table
    ; associated with the key.
    @interaction[#:eval (asl) hsh (hash-ref! hsh 'd 99) hsh]
    } 
    @defproc[(hash-update! [h (hash X Y)] [k X] [f (Y -> Y)]) void?]{
    Composes hash-ref and hash-set! to update an existing mapping; the
    ; third argument is used to compute the new mapping value; the fourth
    ; argument is used as the third argument to hash-ref.
    @interaction[#:eval (asl) hsh (hash-update! hsh 'b (lambda (old-b) (+ old-b 1))) hsh]    
    } 
    @defproc[(hash-update  [h (hash X Y)] [k X] [f (Y -> Y)]) (hash X Y)]{
    Composes hash-ref and hash-set to update an existing mapping; the third
    ; argument is used to compute the new mapping value; the fourth
    ; argument is used as the third argument to hash-ref.
    @interaction[#:eval (asl) (hash-update ish 'b (lambda (old-b) (+ old-b 1)))]        
    } 
    @defproc[(hash-has-key? [h (hash X Y)] [x X]) boolean]{
    Determines if a key is associated with a value in a hash table.
    @interaction[#:eval (asl)
      ish
      (hash-has-key? ish 'b)
      hsh
      (hash-has-key? hsh 'd)]
    } 
    @defproc[(hash-remove! [h (hash X Y)] [x X]) void]{
    Removes an mapping from a mutable hash table.
    @interaction[#:eval (asl)
      hsh
      (hash-remove! hsh 'r)
      hsh]
    } 
    @defproc[(hash-remove [h (hash X Y)] [k X]) (hash X Y)]{
    Constructs an immutable hash table with one less mapping than an
    existing immutable hash table.
    @interaction[#:eval (asl)
      ish
      (hash-remove ish 'b)]
    } 
    @defproc[(hash-map [h (hash X Y)] [f (X Y -> Z)]) (listof Z)]{
    Constructs a new list by applying a function to each mapping of a hash
    table.
    @interaction[#:eval (asl)
      ish
      (hash-map ish list)]
    } 
    @defproc[(hash-for-each [h (hash X Y)] [f (X Y -> any)]) void?]{
    Applies a function to each mapping of a hash table for effect only.
    @interaction[#:eval (asl)
      hsh
      (hash-for-each hsh (lambda (ky vl) (hash-set! hsh ky (+ vl 1))))
      hsh]
    } 
    @defproc[(hash-count [h hash]) integer]{
    Determines the number of keys mapped by a hash table.
    @interaction[#:eval (asl)
      ish
      (hash-count ish)]
    } 
    @defproc[(hash-copy [h hash]) hash]{
    Copies a hash table.
    } 
    @defproc[(hash? [x any]) boolean]{
    Determines if a value is a hash table.
    @interaction[#:eval (asl)
      ish
      (hash? ish)
      (hash? 42)]
    } 
    @defproc[(hash-equal? [h hash?]) boolean]{
    Determines if a hash table uses equal? for comparisons.
    @interaction[#:eval (asl)
      ish
      (hash-equal? ish)
      ieq
      (hash-equal? ieq)
      ]
    } 
    @defproc[(hash-eq? [h hash]) boolean]{
    Determines if a hash table uses eq? for comparisons.
    @interaction[#:eval (asl)
      hsh
      (hash-eq? hsh)
      heq
      (hash-eq? heq)
      ]
    } 
    @defproc[(hash-eqv? [h hash]) boolean]{
    Determines if a hash table uses eqv? for comparisons.
    @interaction[#:eval (asl)
      heq
      (hash-eqv? heq)
      heqv
      (hash-eqv? heqv)
      ]
    })) 

#|
@defproc[(random (case-> (integer -> integer) (-> (and/c real inexact? (>/c 0) (</c 1)))]{
 Generates a random natural number less than some given integer, or to
 generate a random inexact number between 0.0 and 1.0 exclusive.}

@defproc[((advanced-make-hash make-hash) (case-> (-> (hash X Y)) ((listof (list X Y)) -> (hash X Y))]{
 Constructs a mutable hash table from an optional list of mappings that
 uses equal? for comparisions.} 

@defproc[((advanced-make-hasheq make-hasheq) (case-> (-> (hash X Y)) ((listof (list X Y)) -> (hash X Y))]{
 Constructs a mutable hash table from an optional list of mappings that
 uses eq? for comparisions.} 

@defproc[((advanced-make-hasheqv make-hasheqv) (case-> (-> (hash X Y)) ((listof (list X Y)) -> (hash X Y))]{
 Constructs a mutable hash table from an optional list of mappings that
 uses eqv? for comparisions.} 

@defproc[((advanced-make-immutable-hash make-immutable-hash) (case-> (-> (hash X Y)) ((listof (list X Y)) -> (hash X Y))]{
 Constructs an immutable hash table from an optional list of mappings that
 uses equal? for comparisions.} 

@defproc[((advanced-make-immutable-hasheq make-immutable-hasheq) (case-> (-> (hash X Y)) ((listof (list X Y)) -> (hash X Y))]{
 Constructs an immutable hash table from an optional list of mappings that
 uses eq? for comparisions.} 

@defproc[((advanced-make-immutable-hasheqv make-immutable-hasheqv) (case-> (-> (hash X Y)) ((listof (list X Y)) -> (hash X Y))]{
 Constructs an immutable hash table from an optional list of mappings that
 uses eqv? for comparisions.} 

@defproc[(hash-ref (case-> ((hash X Y) X -> Y) ((hash X Y) X Y -> Y) ((hash X Y) X (-> Y) -> Y))]{
 Extracts the value associated with a key from a hash table; the three
 argument case allows a default value or default value computation.} 

@defproc[(hash-ref! (case-> ((hash X Y) X Y -> Y) ((hash X Y) X (-> Y) -> Y))]{
 Extracts the value associated with a key from a mutable hash table; if the
 key does not have an mapping, the third argument is used as the value (or
 used to compute the value) and is added to the hash table associated with
 the key.} 

@defproc[(hash-update! (case-> ((hash X Y) X (Y -> Y) -> void) ((hash X Y) X (Y -> Y) Y -> void) ((hash X Y) X (Y -> Y) (-> Y) -> void))]{
 Composes hash-ref and hash-set! to update an existing mapping; the third
 argument is used to compute the new mapping value; the fourth argument is
 used as the third argument to hash-ref.} 

@defproc[(hash-update (case-> ((hash X Y) X (Y -> Y) -> (hash X Y)) ((hash X Y) X (Y -> Y) Y -> (hash X Y)) ((hash X Y) X (Y -> Y) (-> Y) -> (hash X Y)))]{
 Composes hash-ref and hash-set to update an existing mapping; the third
 argument is used to compute the new mapping value; the fourth argument is
 used as the third argument to hash-ref.} 
|#
