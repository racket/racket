#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title{Modules}

Scheme definitions and expressions are normally written inside of a
module. Although a REPL evaluates definitions and expressions outide
of a module, and although @scheme[load] can evaluate definitions and
expressions from a file as if they appeared in a REPL interaction,
code that is meant to last for more than a few seconds belongs in a
module.

The space of modules is distinct from the space of normal Scheme
definitions. Since modules typically reside in files, the space of
module names is explicitly tied to the filesystem at run time. For
example, if the file @file{/home/molly/cake.ss} contains

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
on the path @file{/home/molly/cake.ss}. Instead of using the full
path, however, the module is likely to be referenced by a releative
path. For example, a file @file{/home/molly/random-cake.ss} could use
the module like this:

@schememod[
big

(require "cake.ss")

(print-cake (random 30))
]

The relative reference @scheme[(require "cake.ss")] works because the
@file{cake.ss} module source is in the same directory as the
@file{random-cake.ss} file.

As you see in the above examples, @scheme[provide] and
@scheme[require] are module-level declarations that export and import
bindings between modules.
