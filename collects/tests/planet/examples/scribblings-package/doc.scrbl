#lang scribble/manual

@(require
   planet/version
   planet/scribble
   (for-label
     racket/base
     (this-package-in
       require-mod require-one require-two require-three require-four
       lang-mod lang-one lang-two lang-three lang-four
       reader-mod reader-one reader-two reader-three reader-four
       lib main)))

@title{Sample Planet Package: @racket[#,(this-package-version-symbol)]}

@declare-exporting/this-package[lib main]
@defmodule*/no-declare/this-package[(lib main)]

@racketmod/this-package[
main
(code:comment "Uh oh, no bindings!")
]

Here we document @racketmodname/this-package[lib], which is also the
@racketmodlink/this-package[main]{main library} of this package.

@defthing[x any/c]{Solve for @racket[x].}

@section{Require}

@subsection[#:tag-prefix "require"]{One}
@defmodule/this-package[require-mod]
@;(@defthing[x any/c]{Solve for @racket[x].})

@subsection[#:tag-prefix "require"]{Many}
@defmodule*/this-package[(require-one require-two)]
@;(@defthing[x any/c]{Solve for @racket[x].})

@subsection[#:tag-prefix "require"]{None}
@defmodule*/no-declare/this-package[(require-three require-four)]
@;(@defthing[x any/c]{Solve for @racket[x].})

@section{Lang}

@subsection[#:tag-prefix "lang"]{One}
@defmodulelang/this-package[lang-mod]
@;(@defthing[x any/c]{Solve for @racket[x].})

@subsection[#:tag-prefix "lang"]{Many}
@defmodulelang*/this-package[(lang-one lang-two)]
@;(@defthing[x any/c]{Solve for @racket[x].})

@subsection[#:tag-prefix "lang"]{None}
@defmodulelang*/no-declare/this-package[(lang-three lang-four)]
@;(@defthing[x any/c]{Solve for @racket[x].})

@section{Reader}

@subsection[#:tag-prefix "reader"]{One}
@defmodulereader/this-package[reader-mod]
@;(@defthing[x any/c]{Solve for @racket[x].})

@subsection[#:tag-prefix "reader"]{Many}
@defmodulereader*/this-package[(reader-one reader-two)]
@;(@defthing[x any/c]{Solve for @racket[x].})

@subsection[#:tag-prefix "reader"]{None}
@defmodulereader*/no-declare/this-package[(reader-three reader-four)]
@;(@defthing[x any/c]{Solve for @racket[x].})
