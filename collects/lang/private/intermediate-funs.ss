(module intermediate-funs scheme/base
  (require "teachprims.ss"
	   mzlib/etc
	   mzlib/list
	   syntax/docprovide
           (for-syntax scheme/base))

  (provide-and-document
   procedures
   (all-from beginner: lang/private/beginner-funs procedures)

   ("Higher-Order Functions"
    (map ((X ... -> Z) (listof X) ... -> (listof Z))
	 "to construct a new list by applying a function to each item on one or more existing lists") 
    (for-each ((any ... -> any) (listof any) ... -> void)
	      "to apply a function to each item on one or more lists for effect only")
    (filter ((X -> boolean) (listof X) -> (listof X))
	    "to construct a list from all those items on a list for which the predicate holds")
    ((intermediate-foldr foldr) ((X Y -> Y) Y (listof X) -> Y)
	   "(foldr f base (list x-1 ... x-n)) = (f x-1 ... (f x-n base))")
    ((intermediate-foldl foldl) ((X Y -> Y) Y (listof X) -> Y)
	   "(foldl f base (list x-1 ... x-n)) = (f x-n ... (f x-1 base))")
    (build-list (nat (nat -> X) -> (listof X))
		"(build-list n f) = (list (f 0) ... (f (- n 1)))")
    ((intermediate-build-string build-string) (nat (nat -> char) -> string)
        "(build-string n f) = (string (f 0) ... (f (- n 1)))")
    ((intermediate-quicksort quicksort) ((listof X) (X X -> boolean) -> (listof X))
	       "to construct a list from all items on a list in an order according to a predicate")
    (andmap ((X -> boolean) (listof X) -> boolean)
	    "(andmap p (list x-1 ... x-n)) = (and (p x-1) (and ... (p x-n)))")
    (ormap ((X -> boolean) (listof X) -> boolean)
	   "(ormap p (list x-1 ... x-n)) = (or (p x-1) (or ... (p x-n)))")

    (memf ((X -> boolean) (listof X) -> (union false (listof X)))
	  "to determine whether the first argument produces true for some value in the second argument")
    (apply ((X-1 ... X-N -> Y) X-1 ... X-i (list X-i+1 ... X-N) -> Y)
	      "to apply a function using items from a list as the arguments")
    (compose ((Y-1 -> Z) ... (Y-N -> Y-N-1) (X-1 ... X-N -> Y-N) -> (X-1 ... X-N -> Z))
	     "to compose a sequence of procedures into a single procedure")
    (procedure? (any -> boolean)
	     "to determine if a value is a procedure"))))
