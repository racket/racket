#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "guide:module-basics"]{Module Basics}

The space of module names is distinct from the space of normal Scheme
definitions. Indeed, since modules typically reside in files, the
space of module names is explicitly tied to the filesystem at run
time. For example, if the file @file{/home/molly/cake.ss} contains

@schememod[
big

(provide print-cake)

(code:comment #, @t{draws a cake with @scheme[n] candles})
(define (print-cake n)
  (printf "   ~a  \n" (make-string n #\.))
  (printf " .-~a-.\n" (make-string n #\|))
  (printf " | ~a |\n" (make-string n #\space))
  (printf "---~a---\n" (make-string n #\-)))
]

then it can be used as the source of a module whose full name is based
on the path @file{/home/molly/cake.ss}. The @scheme[provide] line
exports the definition @scheme[print-cake] so that it can be used
outside the module.

Instead of using its full path, a module is more likely to be
referenced by a relative path. For example, a file
@file{/home/molly/random-cake.ss} could use the @file{cake.ss} module
like this:

@schememod[
big

(require "cake.ss")

(print-cake (random 30))
]

The relative reference @scheme["cake.ss"] in the import
@scheme[(require "cake.ss")] works because the @file{cake.ss} module
source is in the same directory as the @file{random-cake.ss}
file. (Unix-style relative paths are used for relative module
references on all platforms, much like relative URLs.)

Library modules that are distributed with PLT Scheme are referenced
through a @scheme[lib] path. A @scheme[lib] path is like a relative
path, but it is relative (roughly) to the library installation
directory.

@schememod[
big

(require (lib "mzlib/date.ss"))

(printf "Today is ~s\n"
        (date->string (seconds->date (current-seconds))))
]

We discuss more forms of module reference later in
@secref["guide:module-paths"].
