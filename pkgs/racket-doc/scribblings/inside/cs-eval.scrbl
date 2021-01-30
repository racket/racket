#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/vm))

@cs-title[#:tag "cs-eval"]{Evaluation and Running Modules}

The @cppi{racket_apply} function provides basic evaluation support,
but @cppi{racket_eval}, @cppi{racket_dynamic_require}, and
@cppi{racket_namespace_require} provide higher-level support for the
most common evaluation tasks to initialize a Racket instance.

@function[(ptr racket_eval [ptr s_expr])]{

Evaluates @var{s_expr} in the initial Racket thread using its current
@tech[#:doc reference-doc]{namespace}, the same as calling
@racket[eval]. The @var{s_expr} can be an S-expression constructed
with pairs, symbols, etc., or it can be a @tech[#:doc
reference-doc]{syntax object}.

Use @cppi{racket_namespace_require} to initialize a namespace, or use
@cppi{racket_dynamic_require} to access functionality without going
through a top-level namespace. Although those functions are the same
as using @racket[namespace-require] and @racket[dynamic-require], they
work without having those identifiers bound in a namespace already.

This function and others in this section are not meant to be called
in C code that was called from Racket. See also @secref["cs-procs"]
for a discussion of @emph{entry} points versus @emph{re-entry} points.}

@function[(ptr racket_dynamic_require [ptr module_path] [ptr sym_or_false])]{

The same as calling @racket[dynamic-require] in the initial Racket
thread using its current namespace. See also @cppi{racket_eval}.}


@function[(ptr racket_namespace_require [ptr module_path])]{

The same as calling @racket[namespace-require] in the initial Racket
thread using its current namespace. See also @cppi{racket_eval}.}

@function[(ptr racket_primitive [const-char* name])]{

Accesses a primitive function in the same sense as
@racket[vm-primitive] from @racketmodname[ffi/unsafe/vm].}
