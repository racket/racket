(module advanced-funs scheme/base
  (require "teachprims.ss"
           mzlib/etc
           mzlib/list
           mzlib/pretty
           syntax/docprovide
           scheme/promise
	   scheme/port
           "../posn.ss"
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
            "to generate a random natural number less than some given integer, or to generate a random inexact number between 0.0 and 1.0 exclusive"))
   
   ("Reading and Printing"
    (with-input-from-file (string (-> any) -> any)
      "to open the named input file and to extract all input from there")
    (with-output-to-file (string (-> any) -> any)
      "to open the named output file and to put all output there")
    (with-input-from-string (string (-> any) -> any)
      "to turn the given string into input for read* operations")
    (with-output-to-string (string (-> any) -> any)
      "to produce a string from all write/display/print operations")
    

    (print (any -> void)
           "to print the argument as a value to stdout")
    (display (any -> void)
             "to print the argument to stdout (without quotes on symbols and strings, etc.)")
    (write (any -> void)
           "to print the argument to stdout (in a traditional style that is somewhere between print and display)")
    ((pp pretty-print) (any -> void)
                  "like write, but with standard newlines and indentation")
    (printf (string any ... -> void)
            "to format the rest of the arguments according to the first argument and print it to stdout")
    (newline (-> void)
             "to print a newline to stdout")
    (read (-> sexp) "to read input from the user"))
   
   ("Lists"
    (list? (any -> boolean)
           "to determine whether some value is a list")
    
    ((advanced-list* list*) (any ... (listof any) -> (listof any)) 
                            "to construct a list by adding multiple items to a list")
    
    ((advanced-cons cons) (X (listof X) -> (listof X))
                          "to construct a list")
    
    ((advanced-append append) ((listof any) ... -> (listof any))
                              "to create a single list from several")
    
    (assoc 
     (any (listof any) -> (listof any) or false)
     "to produce the first element on the list whose first is equal? to v; otherwise it produces false"))
   
   ("Misc"
    (gensym (-> symbol?)
     "to generate a new symbol, different from all symbols in the program")
    (sleep (-> positive-number void)
     "to cause the program to sleep for the given number of seconds")
    (current-milliseconds (-> exact-integer)
     "to return the current “time” in fixnum milliseconds (possibly negative)")
    
    (force (delay -> any) "to find the delayed value; see also delay")
    (promise? (any -> boolean) "to determine if a value is delayed")
    (void (-> void) "produces a void value")
    (void? (any -> boolean) "to determine if a value is void"))
   
   ("Posns"
    (set-posn-x! (posn number -> void) "to update the x component of a posn")
    (set-posn-y! (posn number -> void) "to update the x component of a posn"))
   
   ("Vectors"
    (vector (X ... -> (vector X ...))
            "to construct a vector")
    (make-vector (number X -> (vectorof X))
                 "to construct a vector")
    (build-vector (nat (nat -> X)  -> (vectorof X))
                  "to construct a vector")	
    (vector-ref ((vector X) nat -> X)
                "to extract an element from a vector")
    (vector-length ((vector X) -> nat)
                   "to determine the length of a vector")	
    (vector-set! ((vectorof X) nat X -> void)
                 "to update a vector")
    (vector? (any -> boolean)
             "to determine if a value is a vector"))
   
   ("Boxes"
    (box (any -> box)
         "to construct a box")
    (unbox (box -> any)
           "to extract the boxed value")
    (set-box! (box any -> void)
              "to update a box")
    (box? (any -> boolean)
          "to determine if a value is a box"))
   
   ("Hash Tables"
    ((advanced-make-hash make-hash) ((listof (list X Y)) -> (hash X Y))
               "to construct a hash table from a list of associations")
    (hash-set! ((hash X Y) X Y -> void)
               "to update a hash table with a new association")
    ((advanced-hash-ref hash-ref) ((hash X Y) X -> Y)
              "to extract the value associated with a key from a hash table")
    (hash-has-key? ((hash X Y) X -> boolean)
                  "to determine if a key is associated with a value in a hash table")
    (hash-remove! ((hash X Y) X -> void)
                  "to remove an association from a hash table")
    (hash-map ((hash X Y) (X Y -> A) -> (listof A))
              "to construct a new list by applying a function to each association of a hash table")
    (hash-for-each ((hash X Y) (X Y -> any) -> void)
                   "to apply a function to each association of a hash table for effect only")
    (hash? (any -> boolean)
           "to determine if value is a hash table"))))
