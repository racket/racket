#lang scribble/doc

@(require scribble/manual (for-label htdp/error 2htdp/image racket))

@title[#:tag "error-reporting"]{Error Reporting Functions} 

@defmodule[htdp/error]

To provide uniform error messages from teachpacks, this module provides several functions: 

@defproc[(check-arg [name (or/c symbol? string?)]
		    [chk  boolean?]
                    [expected any/c]
		    [position (or/c (and/c positive? integer?) string?)]
		    [given any/c])
          void?]{
 Checks an flat-valued argument to function @scheme[name].
 Reports an error for function @scheme[name] 
  telling students what kind of data is @scheme[expected] at the @scheme[position]-th argument 
  and displaying what value was actually @scheme[given],
  unless @scheme[chk] is @scheme[true].}

@defproc[(check-arity [name (or/c symbol? string?)]
		      [arg# (or/c (and/c positive? integer?) string?)?]
		      [args list?])
         void?]{
 Checks the arity of a procedure-valued argument to function @scheme[name].
 Reports an error for function @scheme[name] 
  telling students that @scheme[(length args)] arguments were provided but
  @scheme[arg#] were expected, unless @scheme[(= (length args) arg#)]
  produces @scheme[true].} 

@defproc[(check-proc [name (or/c symbol? string?)]
		     [proc any/c]
		     [expected natural?]
		     [arg# (or/c (and/c positive? integer?) string?)]
		     [arg-err string?])
         void?]{
 Checks [the properties of] a procedure-valued argument to function @scheme[name].
 Reports an error for function @scheme[name] 
  telling students that a procedure was expected at position @scheme[arg#]
  and that this procedure should be of arity @scheme[expected],
  unless the @scheme[proc] is a function and has the @scheme[expected] arity.
  The string @scheme[arg-err] is used to describe the higher-order argument.}

@defproc[(check-result [name (or/c symbol? string?)]
		       [pred? (-> any/c boolean?)]
		       [kind (or/c symbol? string?)]
		       [returned any/c] ...+)
          void?]{
 Checks the expected result of a procedure-valued argument.
 If the result satisfies @scheme[pred?], it is returned. 
 Otherwise, the function reports an error for function @scheme[name] 
  telling students what @scheme[kind] of value is expected and what the
  @scheme[returned] value is. NOTE: if there is more than one
  @scheme[returned] value, the function uses the second value. (MF: I forgot
  why.)}


@defproc[(check-list-list [name (or/c symbol? string?)] 
			  [chk (or/c string? false/c)]
			  [pred? any/c]
			  [given any/c]) 
          void?]{
 Checks a list-of-lists-valued argument to function @scheme[name].
 Reports an error for function @scheme[name] if a list-of-lists contains 
 a value of the wrong kind---signaled via a string-valued @scheme[chk]. 
 The @scheme[given] value is the element that went wrong. Rarely used.}

@defproc[(check-color [name (or/c symbol? string?)] 
		      [arg# natural?]
		      [given any/c]) 
         void?]{
 Checks a color-valued argument to function @scheme[name]. 
 Deprecated. Use @scheme[image-color?] instead. 
 }

@defproc[(check-fun-res [f procedure?]
			[pred? (-> any/c boolean?)]
			[type (or/c symbol? string?)]) 
          void?]{
 Creates a callback from @scheme[f] and uses @scheme[check-result] to make
 sure the result is a piece of data that satisfies @scheme[pred?],
 described as @scheme[type].
 }

@defproc[(natural? [o any/c]) boolean?]{
 Determines whether the given value is a natural number.}

@defproc[(find-non [pred? (-> any/c boolean?)] [l list?]) (or/c any/c false/c)]{
 Find an element of @scheme[l] for which @scheme[(pred? l)] produces
 @scheme[true]; otherwise return @scheme[false].}
 
@defproc[(check-dependencies [name (or/c symbol? string?)]
			     [chk  boolean?]
			     [fmt format-string?]
			     [arg any/c] ...) 
          void?]{
 Unless @scheme[chk] is @scheme[true], it raises an error called
 @scheme[name] whose message is composed from @scheme[fmt] and the
 @scheme[arg]s. 
 }

@defproc[(tp-error [name (or/c symbol? string?)]
		   [fmt format-string?]
		   [arg any/c] ...)
         void?]{
 Signals an @racket[exn:fail:contract] from @scheme[fmt] and @scheme[arg]
 for a function called @scheme[name].}

@defproc[(tp-exn? [o any/c]) boolean?]{
 Determine whether the given object is a teachpack exception 
 MF: Guillaume seems to have deprecated these structures. 
 }

@defproc[(number->ord [n natural?]) string?]{
 Convert a position number into a string, e.g., 1 into ``first'' and so
 on.}

MF: These library and its uses needs to be cleaned up. 

