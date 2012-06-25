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

(require "provide-and-scribble.rkt" scribble/manual)

(define pp (let ([pretty-print (lambda (v) (pretty-write v))]) pretty-print))

(provide-and-scribble
   procedures
   
   ("Numbers: Integers, Rationals, Reals, Complex, Exacts, Inexacts"
    @defproc[(random [x integer]) integer]{Generates a random natural number less than some given integer, or to generate a random inexact number between 0.0 and 1.0 exclusive.}
    )

   ("Reading and Printing"
    @defproc[(with-input-from-file [f string] [p (-> any)]) any]{Opens the named input file and to extract all input from there.}
    @defproc[(with-output-to-file [f string] [p (-> any)]) any]{Opens the named output file and to put all output there.}
    @defproc[(with-input-from-string [s string] [p (-> any)]) any]{Turns the given string into input for read* operations.}
    @defproc[(with-output-to-string [s string] [p (-> any)]) any]{Produces a string from all write/display/print operations.}
    
    @defproc[(print [x any]) void]{Prints the argument as a value to stdout.}
    @defproc[(display [x any]) void]{Prints the argument to stdout (without quotes on symbols and strings, etc.).}
    @defproc[(write [x any]) void]{Prints the argument to stdout (in a traditional style that is somewhere between print and display).}
    @defproc[((pp pretty-print) [x any]) void]{Like write, but with standard newlines and indentation.}
    @defproc[(printf [f string] [x any] ...) void]{Formats the rest of the arguments according to the first argument and print it to stdout.}
    @defproc[(newline) void]{Prints a newline to stdout.}
    @defproc[(read) sexp]{Reads input from the user.})
   
   ("Lists"
    @defproc[(list? [x any]) boolean]{Determines whether some value is a list.}
    @defproc[((advanced-list* list*) [x any] ... [l (listof any)]) (listof any)]{Constructs a list by adding multiple items to a list.}
    @defproc[((advanced-cons cons) [x X] [l (listof X)]) (listof X)]{Constructs a list.}
    @defproc[((advanced-append append) [l (listof any)] ...) (listof any)]{Creates a single list from several.}
    @defproc[(assoc [x any] [l (listof any)]) (union (listof any) false)]{Produces the first element on the list whose first is equal? to v; otherwise it produces false.})
   
   ("Misc"
    @defproc[(gensym) symbol?]{Generates a new symbol, different from all symbols in the program.}
    @defproc[(sleep [sec positive-num]) void]{Causes the program to sleep for the given number of seconds.}
    @defproc[(current-milliseconds) exact-integer]{Returns the current “time” in fixnum milliseconds (possibly negative).}
    @defproc[(force [v any]) any]{Finds the delayed value; see also delay.}
    @defproc[(promise? [x any]) boolean?]{Determines if a value is delayed.}
    @defproc[(void) void?]{Produces a void value.}
    @defproc[(void? [x any]) boolean?]{Determines if a value is void.})
   
   ("Posns"
    @defproc[(set-posn-x! [p posn] [x any]) void?]{Updates the x component of a posn.}
    @defproc[(set-posn-y! [p posn] [x any]) void]{Updates the y component of a posn.})
   
   ("Vectors"
    @defproc[(vector [x X] ...) (vector X ...)]{Constructs a vector.}
    @defproc[(make-vector [n number] [x X]) (vectorof X)]{Constructs a vector.}
    @defproc[(build-vector [n nat] [f (nat -> X)]) (vectorof X)]{Constructs a vector.}	
    @defproc[(vector-ref [v (vector X)] [n nat]) X]{Extracts an element from a vector.}
    @defproc[(vector-length [v (vector X)]) nat]{Determines the length of a vector.}	
    @defproc[(vector-set! [v (vectorof X)][n nat][x X]) void]{Updates a vector.}
    @defproc[(vector->list [v (vectorof X)]) (listof X)]{creates a list of values from the vector of values.}
    @defproc[(vector? [x any]) boolean]{Determines if a value is a vector.})
   
   ("Boxes"
    @defproc[(box [x any/c]) box?]{Constructs a box.}
    @defproc[(unbox [b box?]) any]{Extracts the boxed value.}
    @defproc[(set-box! [b box?][x any/c]) void]{Updates a box.}
    @defproc[(box? [x any/c]) boolean?]{Determines if a value is a box.})
   
   ("Hash Tables"
    @defproc[((advanced-make-hash make-hash)) (hash X Y)]{Constructs a mutable hash table from an optional list of mappings that uses equal? for comparisions.}
    @defproc[((advanced-make-hasheq make-hasheq)) (hash X Y)]{Constructs a mutable hash table from an optional list of mappings that uses eq? for comparisions.}
    @defproc[((advanced-make-hasheqv make-hasheqv)) (hash X Y)]{Constructs a mutable hash table from an optional list of mappings that uses eqv? for comparisions.}
    @defproc[((advanced-make-immutable-hash make-immutable-hash)) (hash X Y)]{Constructs an immutable hash table from an optional list of mappings that uses equal? for comparisions.}
    @defproc[((advanced-make-immutable-hasheq make-immutable-hasheq)) (hash X Y)]{Constructs an immutable hash table from an optional list of mappings that uses eq? for comparisions.}
    @defproc[((advanced-make-immutable-hasheqv make-immutable-hasheqv)) (hash X Y)]{Constructs an immutable hash table from an optional list of mappings that uses eqv? for comparisions.}
    @defproc[(hash-set! [h (hash X Y)] [k X] [v Y]) void?]{Updates a mutable hash table with a new mapping.}
    @defproc[(hash-set  [h (hash X Y)] [k X] [v Y]) (hash X Y)]{Constructs an immutable hash table with one new mapping from an existing immutable hash table.}
    @defproc[(hash-ref  [h (hash X Y)] [k X]) Y]{Extracts the value associated with a key from a hash table; the three argument case allows a default value or default value computation.}
    @defproc[(hash-ref! [h (hash X Y)] [k X] [v Y]) Y]{Extracts the value associated with a key from a mutable hash table; if the key does not have an mapping, the third argument is used as the value (or used to compute the value) and is added to the hash table associated with the key.}
    @defproc[(hash-update! [h (hash X Y)] [k X] [f (Y -> Y)]) void?]{Composes hash-ref and hash-set! to update an existing mapping; the third argument is used to compute the new mapping value; the fourth argument is used as the third argument to hash-ref.}
    @defproc[(hash-update  [h (hash X Y)] [k X] [f (Y -> Y)]) (hash X Y)]{Composes hash-ref and hash-set to update an existing mapping; the third argument is used to compute the new mapping value; the fourth argument is used as the third argument to hash-ref.}
    @defproc[(hash-has-key? [h (hash X Y)] [x X]) boolean]{Determines if a key is associated with a value in a hash table.}
    @defproc[(hash-remove! [h (hash X Y)] [x X]) void]{Removes an mapping from a mutable hash table.}
    @defproc[(hash-remove [h (hash X Y)] [k X]) (hash X Y)]{Constructs an immutable hash table with one less mapping than an existing immutable hash table.}
    @defproc[(hash-map [h (hash X Y)] [f (X Y -> Z)]) (listof Z)]{Constructs a new list by applying a function to each mapping of a hash table.}
    @defproc[(hash-for-each [h (hash X Y)] [f (X Y -> any)]) void?]{Applies a function to each mapping of a hash table for effect only.}
    @defproc[(hash-count [h hash]) integer]{Determines the number of keys mapped by a hash table.}
    @defproc[(hash-copy [h hash]) hash]{Copies a hash table.}
    @defproc[(hash? [x any]) boolean]{Determines if a value is a hash table.}
    @defproc[(hash-equal? [h hash?]) boolean]{Determines if a hash table uses equal? for comparisons.}
    @defproc[(hash-eq? [h hash]) boolean]{Determines if a hash table uses eq? for comparisons.}
    @defproc[(hash-eqv? [h hash]) boolean]{Determines if a hash table uses eqv? for comparisons.}))




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
