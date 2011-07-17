(module intermediate-funs scheme/base
  (require "teachprims.rkt" "and-or-map.rkt"
           mzlib/etc
           scheme/list
           syntax/docprovide
           (for-syntax scheme/base))

  (provide-and-document
   procedures
   (all-from-except beginner: lang/private/beginner-funs procedures + * - / append)

   ("Numbers (relaxed conditions)"
    
    (+ (number ... -> number) "Adds all given numbers.")
    (* (number ... -> number) "Multiplys all given numbers.")
    (- (number ... -> number) "Subtracts from the first all remaining numbers.")
    (/ (number ... -> number) "Divides the first by all remaining numbers.")
    )

   ("Lists"
     ((intermediate-append append) ((listof any) ... -> (listof any))
       "Creates a single list from several, by juxtaposition of the items."))

   ("Higher-Order Functions"
    (map ((X ... -> Z) (listof X) ... -> (listof Z))
	 "Constructs a new list by applying a function to each item on one or more existing lists.") 
    (for-each ((any ... -> any) (listof any) ... -> void)
	      "Applies a function to each item on one or more lists for effect only.")
    ((intermediate-filter filter) ((X -> boolean) (listof X) -> (listof X))
	    "Constructs a list from all those items on a list for which the predicate holds.")
    ((intermediate-foldr foldr) ((X Y -> Y) Y (listof X) -> Y)
	   "(foldr f base (list x-1 ... x-n)) = (f x-1 ... (f x-n base))")
    ((intermediate-foldl foldl) ((X Y -> Y) Y (listof X) -> Y)
	   "(foldl f base (list x-1 ... x-n)) = (f x-n ... (f x-1 base))")
    (build-list (nat (nat -> X) -> (listof X))
		"(build-list n f) = (list (f 0) ... (f (- n 1)))")
    ((intermediate-build-string build-string) (nat (nat -> char) -> string)
        "(build-string n f) = (string (f 0) ... (f (- n 1)))")
    ((intermediate-quicksort quicksort) ((listof X) (X X -> boolean) -> (listof X))
	       "Constructs a list from all items on a list in an order according to a predicate.")
    ((intermediate-sort sort) ((listof X) (X X -> boolean) -> (listof X))
	       "Constructs a list from all items on a list in an order according to a predicate.")
    ((intermediate-andmap andmap) ((X -> boolean) (listof X) -> boolean)
	    "(andmap p (list x-1 ... x-n)) = (and (p x-1) ... (p x-n))")
    ((intermediate-ormap ormap) ((X -> boolean) (listof X) -> boolean)
	   "(ormap p (list x-1 ... x-n)) = (or (p x-1) ... (p x-n))")

    (argmin ((X -> real) (listof X) -> X)
            "Finds the (first) element of the list that minimizes the output of the function.")
    
    (argmax ((X -> real) (listof X) -> X)
            "Finds the (first) element of the list that maximizes the output of the function.")
    
    (memf ((X -> any) (listof X) -> (union false (listof X)))
	  "Produces true if the function given as the first argument produces a non-false value for any item in the second argument.")
    (apply ((X-1 ... X-N -> Y) X-1 ... X-i (list X-i+1 ... X-N) -> Y)
	      "Applies a function using items from a list as the arguments.")
    (compose ((Y-1 -> Z) ... (Y-N -> Y-N-1) (X-1 ... X-N -> Y-N) -> (X-1 ... X-N -> Z))
	     "Composes a sequence of procedures into a single procedure.")
    (procedure? (any -> boolean)
	     "Produces true if the value is a procedure."))))
