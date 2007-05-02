
(module htdp-advanced mzscheme
  (require "private/teach.ss"
	   "private/teachprims.ss"
	   "private/contract-forms.ss"
	   (lib "etc.ss")
	   (lib "list.ss")
	   (lib "pretty.ss")
	   (lib "docprovide.ss" "syntax")
	   "posn.ss")

  ;; syntax:
  (provide (rename advanced-define define)
	   (rename advanced-define-struct define-struct)
	   (rename advanced-lambda lambda)
	   (rename advanced-app #%app)
	   (rename beginner-top #%top)
	   (rename intermediate-local local)
	   (rename advanced-let let)
	   (rename intermediate-let* let*)
	   (rename intermediate-letrec letrec)
	   (rename advanced-recur recur)
	   (rename beginner-cond cond)
	   (rename beginner-else else)
	   (rename beginner-if if)
	   (rename beginner-and and)
	   (rename beginner-or or)
           (rename beginner-require require)
	   (rename intermediate-quote quote)
	   (rename intermediate-quasiquote quasiquote)
	   (rename intermediate-unquote unquote)
	   (rename intermediate-unquote-splicing unquote-splicing)
	   (rename intermediate-time time)
	   (rename advanced-begin begin)
	   (rename advanced-begin0 begin0)
	   (rename advanced-shared shared)
	   (rename advanced-set! set!)
	   (rename advanced-when when)
	   (rename advanced-unless unless)
	   (rename advanced-case case)
	   (rename advanced-delay delay)
	   (rename advanced-module-begin #%module-begin)
	   ;; (rename advanced-contract contract)
	   ;; (rename advanced-define-data define-data)
	   #%datum
           #%top-interaction
	   empty true false)

  ;; procedures:
  (provide-and-document
   procedures

   (all-from-except intermediate: (lib "htdp-intermediate-lambda.ss" "lang") procedures
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

    (set-first! ((cons Y (listof X)) Y -> void)
		"to update the first item of a non-empty list")
    ((advanced-set-rest! set-rest!) ((cons Y (listof X)) (listof X) -> void)
	       "to update the rest of a non-empty list")
    (set-car! ((cons Y (listof X)) Y -> void)
	      "to update the first item of a non-empty list")
    ((advanced-set-cdr! set-cdr!) ((cons Y (listof X)) (listof X) -> void)
				  "to update the rest of a non-empty list")
    ((advanced-append append) ((listof any) ... -> (listof any))
			      "to create a single list from several, by updating the lists")
    ((advanced-append! append!) ((listof any) ... -> (listof any))
				"to create a single list from several, by updating the lists"))
   
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
