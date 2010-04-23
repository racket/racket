#lang scribble/doc
@(require (except-in "mz.ss" set)
          (for-label racket/control))

@title{Classical Control Operators}

@note-lib-only[racket/control]

@(define control-eval
         (let ([the-eval (make-base-eval)])
          (the-eval '(require racket/control))
          the-eval))

The @scheme[racket/control] library provides various control operators
from the research literature on higher-order control operators, plus a
few extra convenience forms. These control operators are implemented
in terms of @scheme[call-with-continuation-prompt],
@scheme[call-with-composable-continuations], etc., and they generally
work sensibly together. Many are redundant; for example,
@scheme[reset] and @scheme[prompt] are aliases.
 
@; ----------------------------------------------------------------------

@defproc[(abort [v any/c] ...) any]{

Returns the @scheme[v]s to a prompt using the default continuation
prompt tag and the default abort handler.

That is, @scheme[(abort v ...)] is equivalent to

@schemeblock[
(abort-current-continuation
 (default-continuation-prompt-tag)
 (lambda () (values v ...)))
]

@examples[#:eval control-eval
(prompt
  (printf "start here\n")
  (printf "answer is ~a\n" (+ 2 (abort 3))))
]}

@; ----------------------------------------------------------------------

@deftogether[(
@defform*[[(% expr)
           (% expr handler-expr)]]
@defproc[(fcontrol [v any/c]) any]
)]{


Sitaram's operators @cite["Sitaram93"].

The essential reduction rules are:

@schemeblock[
(% _val proc) => _val
(% _E[(fcontrol _val)] _proc) => (_proc _val (lambda (_x) _E[_x]))
  (code:comment @#,t{where @scheme[_E] has no @scheme[%]})
]

When @scheme[handler-expr] is omitted, @scheme[%] is the same as 
@scheme[prompt].

@examples[#:eval control-eval
(% (+ 2 (fcontrol 5))
   (lambda (v k)
     (k v)))
(% (+ 2 (fcontrol 5))
   (lambda (v k)
     v))
]}

@; ----------------------------------------------------------------------

@deftogether[(
@defform[(prompt expr ...+)]
@defform[(control id expr ...+)]
)]{

Among the earliest operators for higher-order control
@cite["Felleisen88" "Sitaram90"].

The essential reduction rules are:
@schemeblock[
(prompt _val) => _val
(prompt _E[(control _k _expr)]) => (prompt ((lambda (_k) _expr)
                                            (lambda (_v) _E[_v])))
  (code:comment @#,t{where @scheme[_E] has no @scheme[prompt]})
]

@examples[#:eval control-eval
(prompt
  (+ 2 (control k (k 5))))
(prompt
  (+ 2 (control k 5)))
(prompt
  (+ 2 (control k (+ 1 (control k1 (k1 6))))))
(prompt
  (+ 2 (control k (+ 1 (control k1 (k 6))))))
(prompt
  (+ 2 (control k (control k1 (control k2 (k2 6))))))
]}

@; ----------------------------------------------------------------------

@deftogether[(
@defform[(prompt-at prompt-tag-expr expr ...+)]
@defform[(control-at prompt-tag-expr id expr ...+)]
)]{

Like @scheme[prompt] and @scheme[control], but using specific prompt
tags:

@schemeblock[
(prompt-at _tag _val) => _val
(prompt-at _tag _E[(control-at _tag _k _expr)]) => (prompt-at _tag 
                                                    ((lambda (_k) _expr)
                                                     (lambda (_v) _E[_v])))
  (code:comment @#,t{where @scheme[_E] has no @scheme[prompt-at] for @scheme[_tag]})
]}

@; ----------------------------------------------------------------------

@deftogether[(
@defform[(reset expr ...+)]
@defform[(shift id expr ...+)]
)]{

Danvy and Filinski's operators @cite["Danvy90"].

The essential reduction rules are:

@schemeblock[
(reset _val) => _val
(reset _E[(shift _k _expr)]) => (reset ((lambda (_k) _expr) 
                                        (lambda (_v) (reset _E[_v]))))
  (code:comment @#,t{where @scheme[_E] has no @scheme[reset]})
]

The @scheme[reset] and @scheme[prompt] forms are interchangeable.}


@; ----------------------------------------------------------------------

@deftogether[(
@defform[(reset-at prompt-tag-expr expr ...+)]
@defform[(shift-at prompt-tag-expr identifer expr ...+)]
)]{

Like @scheme[reset] and @scheme[shift], but using the specified prompt
tags.}

@; ----------------------------------------------------------------------

@deftogether[(
@defform[(prompt0 expr ...+)]
@defform[(reset0 expr ...+)]
@defform[(control0 id expr ...+)]
@defform[(shift0 id expr ...+)]
)]{

Generalizations of @scheme[prompt], etc. @cite["Shan04"].

The essential reduction rules are:

@schemeblock[
(prompt0 _val) => _val
(prompt0 _E[(control0 _k _expr)]) => ((lambda (_k) _expr)
                                      (lambda (_v) _E[_v]))
(reset0 _val) => _val
(reset0 _E[(shift0 _k _expr)]) => ((lambda (_k) _expr)
                                   (lambda (_v) (reset0 _E[_v])))
]

The @scheme[reset0] and @scheme[prompt0] forms are interchangable.
Furthermore, the following reductions apply:

@schemeblock[
(prompt _E[(control0 _k _expr)]) => (prompt ((lambda (_k) _expr)
                                             (lambda (_v) _E[_v])))
(reset _E[(shift0 _k _expr)]) => (reset ((lambda (_k) _expr)
                                         (lambda (_v) (reset0 _E[_v]))))
(prompt0 _E[(control _k _expr)]) => (prompt0 ((lambda (_k) expr)
                                              (lambda (_v) _E[_v])))
(reset0 _E[(shift _k _expr)]) => (reset0 ((lambda (_k) expr)
                                          (lambda (_v) (reset _E[_v]))))
]

That is, both the @scheme[prompt]/@scheme[reset] and
@scheme[control]/@scheme[shift] sites must agree for @scheme[0]-like
behavior, otherwise the non-@scheme[0] behavior applies.}

@; ----------------------------------------------------------------------

@deftogether[(
@defform[(prompt0-at prompt-tag-expr expr ...+)]
@defform[(reset0-at prompt-tag-expr expr ...+)]
@defform[(control0-at prompt-tag-expr id expr ...+)]
@defform[(shift0-at prompt-tag-expr id expr ...+)]
)]{

Variants of @scheme[prompt0], @|etc| that accept a prompt tag.}

@; ----------------------------------------------------------------------

@defproc[(spawn [proc ((any/c . -> . any) . -> . any)]) any]{

The operators of Hieb and Dybvig @cite["Hieb90"].

The essential reduction rules are:

@schemeblock[
(prompt-at _tag _obj) => _obj
(spawn _proc) => (prompt _tag (_proc (lambda (_x) (abort _tag _x))))
(prompt-at _tag _E[(abort _tag _proc)])
  => (_proc (lambda (_x) (prompt-at _tag _E[_x])))
  (code:comment @#,t{where @scheme[_E] has no @scheme[prompt-at] for @scheme[_tag]})
]}

@; ----------------------------------------------------------------------

@defproc[(splitter [proc (((-> any) . -> . any) 
                          ((continuation? . -> . any) . -> . any) 
                          . -> . any)])
         any]{

The operator of Queinnec and Serpette @cite["Queinnec91"].

The essential reduction rules are:
@schemeblock[
(splitter _proc) => (prompt-at _tag
                     (_proc (lambda (_thunk) 
                              (abort _tag _thunk))
                            (lambda (_proc)
                              (control0-at _tag _k (_proc _k)))))
(prompt-at _tag _E[(abort _tag _thunk)]) => (_thunk)
  (code:comment @#,t{where @scheme[_E] has no @scheme[prompt-at] for @scheme[_tag]})
(prompt-at _tag _E[(control0-at _tag _k _expr)]) => ((lambda (_k) _expr)
                                                     (lambda (_x) _E[_x]))
  (code:comment @#,t{where @scheme[_E] has no @scheme[prompt-at] for @scheme[_tag]})
]}

@; ----------------------------------------------------------------------

@deftogether[(
@defproc[(new-prompt) any]
@defform[(set prompt-expr expr ...+)]
@defform[(cupto prompt-expr id expr ...+)]
)]{

The operators of Gunter et al. @cite["Gunter95"].

In this library, @scheme[new-prompt] is an alias for
@scheme[make-continuation-prompt-tag], @scheme[set] is an alias for
@scheme[prompt0-at], and @scheme[cupto] is an alias for @scheme[control0-at].

}

@close-eval[control-eval]
