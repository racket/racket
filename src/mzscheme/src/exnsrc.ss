
#|

Initial symbols are struct types. A non-initial symbol is a struct
type without fields or subtypes. Square brackets are struct fields and
propeties (the latter in curly braces), strings are contracts/comments.

|#

#cs
(exn [exn_field_check
      (message "immutable string" "error message")
      (continuation-marks "mark set"
			  "value returned by \\scmfirst{current-continuation-marks} immediately before the exception is raised")] 
     -
     (fail [] "exceptions that represent errors"
	   (contract [] "inappropriate run-time use of a function or syntactic form"
		     (arity []
			    "application with the wrong number of arguments")
		     (divide-by-zero [] "divide by zero")
		     (continuation [] "attempt to cross a continuation barrier")
		     (variable [variable_field_check
				(id "symbol" "the variable's identifier")]
			       "unbound/not-yet-defined global or module variable"))
	   (syntax [syntax_field_check
		    (exprs "immutable list of syntax objects" "illegal expression(s)")
		    {exn:source scheme_source_property |scheme_make_prim(extract_syntax_locations)|}]
		   "syntax error, but not a \\scmfirst{read} error")
	   (read [read_field_check
		  (srclocs "immutable list of \\scmk{srcloc}s (see \\SecRef{linecol})" "source location(s) of error")
		  {exn:source scheme_source_property  |scheme_make_prim(extract_read_locations)|}]
		 "\\rawscm{read} parsing error"
		 (eof [] "unexpected end-of-file")
		 (non-char [] "unexpected non-character"))
	   (filesystem [] "error manipulating a filesystem object"
		       (exists [] "attempt to create a file that exists already")
		       (version [] "version mismatch loading an extension"))
	   (network [] "TCP and UDP errors")
	   (out-of-memory [] "out of memory")
	   (unsupported [] "unsupported feature")
	   (user [] "for end users"))
     
     (break [break_field_check
	     (continuation "escape continuation" "resumes from the break")]
	    "asynchronous break signal"))


#|
Not an exception in the above sense:
     (special-comment [width "non-negative exact integer" "width of the special comment in port positions"]
	"raised by a custom input port's special-reading procedure")
|#

