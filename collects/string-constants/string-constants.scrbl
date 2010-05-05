#lang scribble/doc
@(require scribble/manual
          (for-label string-constants
                     racket))

@title{@bold{String Constants}: GUI Internationalization}

This library provides the facility for multiple languages in
DrRacket's GUI. 

@; ----------------------------------------------------------------------

@section{Using String Constants}
@defmodule[string-constants]

@defform[(string-constant name)]{
 
This form returns the string constant named @racket[name].}
   
@defform[(string-constants name)]{

This form returns a list of string constants, one for each language
that DrRacket's GUI supports.}
  
@defform[(this-language)]{

This form returns the name of the current language as a symbol.}
  
@defform[(all-languages)]{ 

This form returns a list of symbols (in the same order as those
returned from @racket[string-constants]) naming each language.}

@defproc[(set-language-pref [lang string?]) void?]{

Sets the language for the next run of DrRacket to @racket[lang], which
must be a symbol returned from @racket[all-languages].  Does not affect the
running DrRacket.}

@; ----------------------------------------------------------------------

@section{Adding String Constants}
@defmodule[string-constants/string-constant-lang]

To add string constants to DrRacket, see the files:

@itemize[
  @item{@filepath{english-string-constants.ss}}
  @item{@filepath{french-string-constants.ss}}
  @item{@filepath{spanish-string-constants.ss}}
  @item{@filepath{german-string-constants.ss}}
  @item{@filepath{danish-string-constants.ss}}
  @item{@filepath{italian-string-constants.ss}}]
  
Each file has the same format. They are each modules in the
@racketmodname[string-constants/string-constant-lang] language. The
body of each module is a finite mapping table that gives the mapping
from the symbolic name of a string constant to its translation in the
appropriate language.

The @filepath{english-string-constants} is considered the master file;
string constants will be set there and translated into each of the
other language files.  In addition, the
@filepath{english-string-constants.ss} file should contain hints about
the context of the strings whose symbol name might not be clear.

@; ----------------------------------------------------------------------

@section{Language Environment Variables}

@itemize[
  @item{@indexed-envvar{PLTSTRINGCONSTANTS}}
  @item{@indexed-envvar{STRINGCONSTANTS}}]

If either of these environment variables are set, DrRacket
shows you, during startup, which string constants are not
yet defined for each language. 

You can also specify which languages you are interested
in. If either environment variable is bound to a symbol (as
interpreted by @racket[read]) you see only the corresponding
language's messages. If either one is bound to a list of
symbols (again, as interpreted by @racket[read]) you see the
messages for all the languages in the list. If either is
bound to anything else, you see all of the languages.

The @envvar{PLTSTRINGCONSTANTS} environment variable takes precedence
of the @envvar{STRINGCONSTANTS} environment variable.
