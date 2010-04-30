#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss"
          (for-label setup/dirs))

@title[#:tag "module-basics"]{Module Basics}

The space of module names is distinct from the space of normal Racket
definitions. Indeed, since modules typically reside in files, the
space of module names is explicitly tied to the filesystem at run
time. For example, if the file @filepath{/home/molly/cake.rkt} contains

@racketmod[
racket

(provide print-cake)

(code:comment @#,t{draws a cake with @racket[n] candles})
(define (print-cake n)
  (printf "   ~a  \n" (make-string n #\.))
  (printf " .-~a-.\n" (make-string n #\|))
  (printf " | ~a |\n" (make-string n #\space))
  (printf "---~a---\n" (make-string n #\-)))
]

then it can be used as the source of a module whose full name is based
on the path @filepath{/home/molly/cake.rkt}. The @racket[provide] line
exports the definition @racket[print-cake] so that it can be used
outside the module.

Instead of using its full path, a module is more likely to be
referenced by a relative path. For example, a file
@filepath{/home/molly/random-cake.rkt} could use the @filepath{cake.rkt} module
like this:

@racketmod[
racket

(require "cake.rkt")

(print-cake (random 30))
]

The relative reference @racket["cake.rkt"] in the import
@racket[(require "cake.rkt")] works because the @filepath{cake.rkt} module
source is in the same directory as the @filepath{random-cake.rkt}
file. (Unix-style relative paths are used for relative module
references on all platforms, much like relative URLs.)

Library modules that are distributed with Racket are usually
referenced through an unquoted, suffixless path. The path is relative
to the library installation directory, which contains directories for
individual library @deftech{collections}. The module below refers to
the @filepath{date.rkt} library that is part of the @filepath{racket}
@tech{collection}.

@racketmod[
racket

(require racket/date)

(printf "Today is ~s\n"
        (date->string (seconds->date (current-seconds))))
]

In addition to the main @tech{collection} directory, which contains
all collections that are part of the installation, collections can
also be installed in a user-specific location. Finally, additional
collection directories can be specified in configuration files or
through the @envvar{PLTCOLLECTS} search path. Try running the
following program to find out where your collections are:

@racketmod[
racket

(require setup/dirs)

(find-collects-dir) (code:comment @#,t{main collection directory})
(find-user-collects-dir) (code:comment @#,t{user-specific collection directory})
(get-collects-search-dirs) (code:comment @#,t{complete search path})
]

We discuss more forms of module reference later in
@secref["module-paths"].
