#lang scribble/doc
@(require "common.rkt" scribble/eval
          (for-label syntax/modcollapse))

@(define (new-evaluator)
   (let* ([e (make-base-eval)])
     (e '(require (for-syntax racket/base) syntax/modcollapse))
     e))

@(define evaluator (new-evaluator))

@title[#:tag "modcollapse"]{Simplifying Module Paths}

@defmodule[syntax/modcollapse]

@defproc[(collapse-module-path [module-path-v module-path?]
                               [rel-to-module-path-v (or/c module-path?
                                                           (-> module-path?))])
         (or/c path? module-path?)]{

Returns a ``simplified'' module path by combining
@racket[module-path-v] with @racket[rel-to-module-path-v], where the
latter must have one of the following forms: a @racket['(lib ....)] or
symbol module path; a @racket['(file ....)] module path; a
@racket['(planet ....)] module path; a @techlink[#:doc refman]{path};
@racket['(@#,racket[quote] @#,racket[_symbol])];
a @racket['(submod @#,racket[_base] @#,racket[_symbol] ...)] module path 
where @racket[_base] would be allowed; or a thunk to generate one of those.

The result can be a path if @racket[module-path-v] contains a path
element that is needed for the result, or if
@racket[rel-to-module-path-v] is a non-string path that is needed for
the result. Similarly, the result can be @racket['submod] wrapping a
path.  Otherwise, the result is a module path in the sense of
@racket[module-path?].

When the result is a @racket['lib] or @racket['planet] module path, it
is normalized so that equivalent module paths are represented by
@racket[equal?] results. When the result is a @racket['submod] module
path, it contains only symbols after the base module path, and the
base is normalized in the case of a @racket['lib] or @racket['planet]
base.

@examples[#:eval evaluator
(collapse-module-path "m.rkt"  '(lib "n/main.rkt"))
(collapse-module-path '(submod "." x)  '(lib "n/main.rkt"))
(collapse-module-path '(submod "." x)  '(submod (lib "n/main.rkt") y))
]

}

@defproc[(collapse-module-path-index [module-path-index module-path-index?]
                                     [rel-to-module-path-v (or/c module-path?
                                                                 (-> module-path?))])
         (or/c path? module-path?)]{

Like @racket[collapse-module-path], but the input is a @techlink[#:doc
refman]{module path index}; in this case, the
@racket[rel-to-module-path-v] base is used where the module path index
contains the ``self'' index.}


@close-eval[evaluator]
