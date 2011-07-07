(module advanced-funs scheme/base
  (require "teachprims.rkt"
           mzlib/etc
           mzlib/list
           mzlib/pretty
           syntax/docprovide
           scheme/promise
           scheme/port
           "../posn.rkt"
           (for-syntax scheme/base))

  (define pp
    (let ([pretty-print (lambda (v)
                          (pretty-write v))])
      pretty-print))
  
  (provide-and-document
   procedures
   
   ("Numbers: Integers, Rationals, Reals, Complex, Exacts, Inexacts"
    (random (case->
             (integer -> integer)
             (-> (and/c real inexact? (>/c 0) (</c 1))))
            "Generates a random natural number less than some given integer, or to generate a random inexact number between 0.0 and 1.0 exclusive."))
   
   ("Reading and Printing"
    (with-input-from-file (string (-> any) -> any)
      "Opens the named input file and to extract all input from there.")
    (with-output-to-file (string (-> any) -> any)
      "Opens the named output file and to put all output there.")
    (with-input-from-string (string (-> any) -> any)
      "Turns the given string into input for read* operations.")
    (with-output-to-string (string (-> any) -> any)
      "Produces a string from all write/display/print operations.")
    
    
    (print (any -> void)
           "Prints the argument as a value to stdout.")
    (display (any -> void)
             "Prints the argument to stdout (without quotes on symbols and strings, etc.).")
    (write (any -> void)
           "Prints the argument to stdout (in a traditional style that is somewhere between print and display).")
    ((pp pretty-print) (any -> void)
                       "Like write, but with standard newlines and indentation.")
    (printf (string any ... -> void)
            "Formats the rest of the arguments according to the first argument and print it to stdout.")
    (newline (-> void)
             "Prints a newline to stdout.")
    (read (-> sexp) "Reads input from the user."))
   
   ("Lists"
    (list? (any -> boolean)
           "Determines whether some value is a list.")
    
    ((advanced-list* list*) (any ... (listof any) -> (listof any)) 
                            "Constructs a list by adding multiple items to a list.")
    
    ((advanced-cons cons) (X (listof X) -> (listof X))
                          "Constructs a list.")
    
    ((advanced-append append) ((listof any) ... -> (listof any))
                              "Creates a single list from several.")
    
    (assoc 
     (any (listof any) -> (listof any) or false)
     "Produces the first element on the list whose first is equal? to v; otherwise it produces false."))
   
   ("Misc"
    (gensym (-> symbol?)
            "Generates a new symbol, different from all symbols in the program.")
    (sleep (-> positive-number void)
           "Causes the program to sleep for the given number of seconds.")
    (current-milliseconds (-> exact-integer)
                          "Returns the current “time” in fixnum milliseconds (possibly negative).")
    
    (force (delay -> any) "Finds the delayed value; see also delay.")
    (promise? (any -> boolean) "Determines if a value is delayed.")
    (void (-> void) "Produces a void value.")
    (void? (any -> boolean) "Determines if a value is void."))
   
   ("Posns"
    (set-posn-x! (posn number -> void) "Updates the x component of a posn.")
    (set-posn-y! (posn number -> void) "Updates the x component of a posn."))
   
   ("Vectors"
    (vector (X ... -> (vector X ...))
            "Constructs a vector.")
    (make-vector (number X -> (vectorof X))
                 "Constructs a vector.")
    (build-vector (nat (nat -> X)  -> (vectorof X))
                  "Constructs a vector.")	
    (vector-ref ((vector X) nat -> X)
                "Extracts an element from a vector.")
    (vector-length ((vector X) -> nat)
                   "Determines the length of a vector.")	
    (vector-set! ((vectorof X) nat X -> void)
                 "Updates a vector.")
    (vector? (any -> boolean)
             "Determines if a value is a vector."))
   
   ("Boxes"
    (box (any -> box)
         "Constructs a box.")
    (unbox (box -> any)
           "Extracts the boxed value.")
    (set-box! (box any -> void)
              "Updates a box.")
    (box? (any -> boolean)
          "Determines if a value is a box."))
   
   ("Hash Tables"
    ((advanced-make-hash make-hash) 
     (case->
      (-> (hash X Y))
      ((listof (list X Y)) -> (hash X Y)))
     "Constructs a mutable hash table from an optional list of mappings that uses equal? for comparisions.")
    ((advanced-make-hasheq make-hasheq) 
     (case->
      (-> (hash X Y))
      ((listof (list X Y)) -> (hash X Y)))
     "Constructs a mutable hash table from an optional list of mappings that uses eq? for comparisions.")
    ((advanced-make-hasheqv make-hasheqv)
     (case->
      (-> (hash X Y))
      ((listof (list X Y)) -> (hash X Y)))
     "Constructs a mutable hash table from an optional list of mappings that uses eqv? for comparisions.")
    ((advanced-make-immutable-hash make-immutable-hash) 
     (case->
      (-> (hash X Y))
      ((listof (list X Y)) -> (hash X Y)))
     "Constructs an immutable hash table from an optional list of mappings that uses equal? for comparisions.")
    ((advanced-make-immutable-hasheq make-immutable-hasheq) 
     (case->
      (-> (hash X Y))
      ((listof (list X Y)) -> (hash X Y)))
     "Constructs an immutable hash table from an optional list of mappings that uses eq? for comparisions.")
    ((advanced-make-immutable-hasheqv make-immutable-hasheqv)
     (case->
      (-> (hash X Y))
      ((listof (list X Y)) -> (hash X Y)))
     "Constructs an immutable hash table from an optional list of mappings that uses eqv? for comparisions.")
    (hash-set! ((hash X Y) X Y -> void)
               "Updates a mutable hash table with a new mapping.")
    (hash-set ((hash X Y) X Y -> (hash X Y))
               "Constructs an immutable hash table with one new mapping from an existing immutable hash table.")
    (hash-ref (case->
               ((hash X Y) X -> Y)
               ((hash X Y) X Y -> Y)
               ((hash X Y) X (-> Y) -> Y))
              "Extracts the value associated with a key from a hash table; the three argument case allows a default value or default value computation.")
    (hash-ref! (case->
                ((hash X Y) X Y -> Y)
                ((hash X Y) X (-> Y) -> Y))
               "Extracts the value associated with a key from a mutable hash table; if the key does not have an mapping, the third argument is used as the value (or used to compute the value) and is added to the hash table associated with the key.")
    (hash-update! (case->
                   ((hash X Y) X (Y -> Y) -> void)
                   ((hash X Y) X (Y -> Y) Y -> void)
                   ((hash X Y) X (Y -> Y) (-> Y) -> void))
                  "Composes hash-ref and hash-set! to update an existing mapping; the third argument is used to compute the new mapping value; the fourth argument is used as the third argument to hash-ref.")
    (hash-update (case->
                   ((hash X Y) X (Y -> Y) -> (hash X Y))
                   ((hash X Y) X (Y -> Y) Y -> (hash X Y))
                   ((hash X Y) X (Y -> Y) (-> Y) -> (hash X Y)))
                  "Composes hash-ref and hash-set to update an existing mapping; the third argument is used to compute the new mapping value; the fourth argument is used as the third argument to hash-ref.")
    (hash-has-key? ((hash X Y) X -> boolean)
                   "Determines if a key is associated with a value in a hash table.")
    (hash-remove! ((hash X Y) X -> void)
                  "Removes an mapping from a mutable hash table.")
    (hash-remove ((hash X Y) X -> (hash X Y))
                 "Constructs an immutable hash table with one less mapping than an existing immutable hash table.")
    (hash-map ((hash X Y) (X Y -> A) -> (listof A))
              "Constructs a new list by applying a function to each mapping of a hash table.")
    (hash-for-each ((hash X Y) (X Y -> any) -> void)
                   "Applies a function to each mapping of a hash table for effect only.")
    (hash-count (hash -> integer)
                "Determines the number of keys mapped by a hash table.")
    (hash-copy (hash -> hash)
               "Copies a hash table.")
    (hash? (any -> boolean)
           "Determines if a value is a hash table.")
    (hash-equal? (hash -> boolean)
                 "Determines if a hash table uses equal? for comparisions.")
    (hash-eq? (hash -> boolean)
              "Determines if a hash table uses eq? for comparisions.")
    (hash-eqv? (hash -> boolean)
               "Determines if a hash table uses eqv? for comparisions."))))
