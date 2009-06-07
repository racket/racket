#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss"
          (for-label setup/dirs))

@title[#:tag "module-basics"]{Module Basics}

The space of module names is distinct from the space of normal Scheme
definitions. Indeed, since modules typically reside in files, the
space of module names is explicitly tied to the filesystem at run
time. For example, if the file @filepath{/home/molly/cake.ss} contains

@schememod[
scheme

(provide print-cake)

(code:comment @#,t{draws a cake with @scheme[n] candles})
(define (print-cake n)
  (printf "   ~a  \n" (make-string n #\.))
  (printf " .-~a-.\n" (make-string n #\|))
  (printf " | ~a |\n" (make-string n #\space))
  (printf "---~a---\n" (make-string n #\-)))
]

then it can be used as the source of a module whose full name is based
on the path @filepath{/home/molly/cake.ss}. The @scheme[provide] line
exports the definition @scheme[print-cake] so that it can be used
outside the module.

Instead of using its full path, a module is more likely to be
referenced by a relative path. For example, a file
@filepath{/home/molly/random-cake.ss} could use the @filepath{cake.ss} module
like this:

@schememod[
scheme

(require "cake.ss")

(print-cake (random 30))
]

The relative reference @scheme["cake.ss"] in the import
@scheme[(require "cake.ss")] works because the @filepath{cake.ss} module
source is in the same directory as the @filepath{random-cake.ss}
file. (Unix-style relative paths are used for relative module
references on all platforms, much like relative URLs.)

Library modules that are distributed with PLT Scheme are usually
referenced through an unquoted, suffixless path. The path is relative
to the library installation directory, which contains directories for
individual library @deftech{collections}. The module below refers to
the @filepath{date.ss} library that is part of the @filepath{scheme}
@tech{collection}.

@schememod[
scheme

(require scheme/date)

(printf "Today is ~s\n"
        (date->string (seconds->date (current-seconds))))
]

In addition to the main @tech{collection} directory, which contains
all collections that are part of the installation, collections can
also be installed in a user-specific location. Finally, additional
collection directories can be specified in configuration files or
through the @envvar{PLTCOLLECTS} search path. Try running the
following program to find out where your collections are:

@schememod[
scheme

(require setup/dirs)

(find-collects-dir) (code:comment @#,t{main collection directory})
(find-user-collects-dir) (code:comment @#,t{user-specific collection directory})
(get-collects-search-dirs) (code:comment @#,t{complete search path})
]

We discuss more forms of module reference later in
@secref["module-paths"].
