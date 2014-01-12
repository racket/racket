#lang scribble/manual
@(require (for-label racket/base
                     "manual-ex.rkt"))

@defmodule["manual-ex.rkt" #:packages ("manual-test")]

@defproc[(f) integer?]{A function.}

@defproc[(g [x void?] [y void?]) integer?]{A function with two arguments.}

@defproc[#:kind "function" (h [x void?] [#:y y void?]) integer?]{A ``function'' with a keyword argument.}

@defproc[(i [x void?] [#:y y void? (void)]) integer?]{A function with an optional keyword argument.}

@defproc[#:link-target? #f (f) integer?]{A function, again, not a link target.}

@defproc[#:link-target? #f (f) integer? #:value 10]{A function, again, not a link target, documented to return @racket[10].}

@defproc[#:link-target? #f (f) integer? #:value (let ()
                                                  (define x 10)
                                                  x)]{
A function, again, not a link target, documented to return @racket[10] using a definition.
}

@defproc[#:kind "function" #:link-target? #f (g [x void?]) integer?]{A ``function,'' again, not a link target.}

@defproc[#:id [i #'j] (i) void?]{Source is @racket[i], documents @racket[j].}

@defproc*[#:link-target? #f ([(f) integer?] [(g [x void?] [y void?]) void?])]{Functions, yet again.}


@defform[(m datum)]{A syntactic form.}

@defform[#:link-target? #f (m datum)]{A syntactic form, again.}

@defform[#:kind "macro" #:link-target? #f (m datum)]{A ``macro,'' again.}

@defform*[#:kind "macro" #:link-target? #f [(m datum) (m same-datum)]]{A ``macro,'' yet again.}

@defform/none[(m datum)]{Yet again.}

@defidform[n]{An identifier form.}

@defidform[#:link-target? #f n]{An identifier form, again.}

@specform[(m datum)]{Specification of @racket[m].}


@defparam[p k integer?]{A parameter}

@defparam[#:link-target? #f p k integer?]{A parameter, again.}

@defparam[#:link-target? #f p k integer? #:value 10]{A parameter, again, with a documented default value.}

@defparam*[#:link-target? #f p k real? integer?]{A parameter, yet again.}

@defparam*[#:link-target? #f p k real? integer? #:value 10]{A parameter, yet again, with a documented default value.}

@defboolparam[q on?]{A boolean parameter.}

@defboolparam[#:link-target? #f q still-on?]{A boolean parameter, again.}

@defboolparam[#:link-target? #f q still-on? #:value #f]{A boolean parameter, again, with a documented default value.}


@defthing[v integer?]{A thing.}

@defthing[#:link-target? #f v integer?]{A thing, again.}

@defthing[#:link-target? #f v integer? #:value 10]{A thing, again, with a documented value.}

@defthing[#:link-target? #f v integer? #:value 12345678901234567890123456789012345678901234567890]{A thing, again, with a documented value that's too wide to fit on one line.}


@defstruct[pt ([x real?] [y real?])]{A structure type with extra name.}

@defstruct*[pn ([x real?] [y real?])]{A structure type.}

@defstruct*[#:link-target? #f pn ([x real?] [y real?])]{A structure type, again.}

@defstruct*[#:link-target? #f pn ([x real?] [y real?]) #:transparent]{A transparent structure type, again.}

@defstruct*[#:link-target? #f pn ([x real?] [y real?]) #:inspector #f]{A transparent structure type, again.}

@defstruct*[#:link-target? #f pn ([x real?] [y real?]) #:prefab]{A prefab structure type, again.}

@defstruct*[#:link-target? #f pn ([x real?] [y real?]) #:constructor-name pt]{A structure type with name, again.}

@defstruct*[#:link-target? #f pn ([x real?] [y real?]) #:extra-constructor-name pt]{A structure type with extra name, again.}

@defstruct[#:link-target? #f pt ([x real?] [y real?]) #:mutable]{A mutable structure type with extra name, again.}


@defmodule["manual-ex0.rkt" #:no-declare #:link-target? #f #:packages ()]
@defmodule["manual-ex0.rkt" #:lang #:no-declare #:link-target? #f #:packages ()]
@defmodule["manual-ex0.rkt" #:reader #:no-declare #:link-target? #f #:packages ()]

@section{Sub2}
@defmodule["manual-ex2.rkt" #:no-declare #:packages ()]

@section{Sub2a}
@defmodule*/no-declare[("manual-ex2a.rkt") #:packages ()]

@section{Sub3}
@defmodule["manual-ex3.rkt" #:lang #:no-declare #:packages ()]

@section{Sub3a}
@defmodulelang*/no-declare[("manual-ex3a.rkt") #:packages ()]

@section{Sub4-5}
@defmodule[#:multi ("manual-ex4.rkt" "manual-ex5.rkt") #:packages ()]

@section{Sub4a-5a}
@defmodule*[("manual-ex4a.rkt" "manual-ex5a.rkt") #:packages ()]

@section{Sub6}
@defmodule[#:require-form (racket load) "manual-ex6.rkt" #:packages ()]

@section{Sub6a}
@defmodule*[#:require-form (racket load) ("manual-ex6a.rkt") #:packages ()]

@section{Sub7}
@defmodule["manual-ex7.rkt" #:use-sources (racket/base) #:packages ()]

@section{Sub7a}
@defmodule*[("manual-ex7a.rkt") #:use-sources (racket/base) #:packages ()]

@section{Sub8}
@defmodule["manual-ex8.rkt" #:reader #:packages ()]

@section{Sub8a}
@defmodulereader["manual-ex8a.rkt" #:packages ()]

@section{Sub8b}
@defmodulereader*[("manual-ex8b.rkt") #:packages ()]
