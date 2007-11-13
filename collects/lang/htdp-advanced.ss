
(module htdp-advanced scheme/base
  (require "private/teach.ss"
	   "private/teachprims.ss"
	   "private/contract-forms.ss"
	   mzlib/etc
	   mzlib/list
	   mzlib/pretty
	   syntax/docprovide
	   "posn.ss")

  ;; syntax:
  (provide (rename-out
            [advanced-define define]
            [advanced-define-struct define-struct]
            [advanced-lambda lambda]
            [advanced-app #%app]
            [beginner-top #%top]
            [intermediate-local local]
            [advanced-let let]
            [intermediate-let* let*]
            [intermediate-letrec letrec]
            [advanced-recur recur]
            [beginner-cond cond]
            [beginner-else else]
            [beginner-if if]
            [beginner-and and]
            [beginner-or or]
            [beginner-require require]
            [beginner-dots ..]
            [beginner-dots ...]
            [beginner-dots ....]
            [beginner-dots .....]
            [beginner-dots ......]
            [intermediate-quote quote]
            [intermediate-quasiquote quasiquote]
            [intermediate-unquote unquote]
            [intermediate-unquote-splicing unquote-splicing]
            [intermediate-time time]
            [advanced-begin begin]
            [advanced-begin0 begin0]
            [advanced-shared shared]
            [advanced-set! set!]
            [advanced-when when]
            [advanced-unless unless]
            [advanced-case case]
            [advanced-delay delay]
            [advanced-module-begin #%module-begin]
            ;; [advanced-contract contract]
            ;; [advanced-define-data define-data]
            )
	   #%datum
           #%top-interaction
	   empty true false)

  ;; procedures:
  (provide-and-document
   procedures

   (all-from-except intermediate: lang/htdp-intermediate-lambda procedures
		    cons list* append)

   ("Reading and Printing"
    (print (any -> void)
	   "to print the argument as a value to stdout")
    (display (any -> void)
	     "to print the argument to stdout (without quotes on symbols and strings, etc.)")
    (write (any -> void)
	   "to print the argument to stdout (in a traditional style that is somewhere between print and display)")
    (pretty-print (any -> void)
	   "like write, but with standard newlines and indentation")
    (printf (string any ... -> void)
	    "to format the rest of the arguments according to the first argument and print it to stdout")
    (newline (-> void)
	     "to print a newline to stdout")
    (read (-> sexp) "to read input from the user"))
   
   ("Lists"
     (list? (any -> boolean)
       "to determine whether some value is a list")

    ((advanced-cons cons) (X (listof X) -> (listof X))
			  "to construct a list")

    ((advanced-append append) ((listof any) ... -> (listof any))
			      "to create a single list from several"))
   
   ("Misc"
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
	  "to determine if a value is a box"))))
