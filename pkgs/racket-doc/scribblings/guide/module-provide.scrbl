#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "module-provide"]{Exports: @racket[provide]}

By default, all of a module's definitions are private to the
module. The @racket[provide] form specifies definitions to be made
available where the module is @racket[require]d.

@specform[(provide provide-spec ...)]{}

A @racket[provide] form can only appear at module level (i.e., in the
immediate body of a @racket[module]).  Specifying multiple
@racket[_provide-spec]s in a single @racket[provide] is exactly the
same as using multiple @racket[provide]s each with a single
@racket[_provide-spec].

Each identifier can be exported at most once from a module across all
@racket[provide]s within the module. More precisely, the external name
for each export must be distinct; the same internal binding can be
exported multiple times with different external names.

The allowed shape of a @racket[_provide-spec] is defined recursively:

@;------------------------------------------------------------------------
@specspecsubform[identifier]{

In its simplest form, a @racket[_provide-spec] indicates a binding
within its module to be exported. The binding can be from either a
local definition, or from an import.

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals(rename-out)
                 (rename-out [orig-id export-id] ...)]{

A @racket[rename-out] form is similar to just specifying an identifier,
but the exported binding @racket[orig-id] is given a different name,
@racket[export-id], to importing modules.

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals(struct-out)
                 (struct-out struct-id)]{

A @racket[struct-out] form exports the bindings created by
@racket[(struct struct-id ....)].

@guideother{See @secref["define-struct"] for information on
@racket[define-struct].}

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals (all-defined-out)
                 (all-defined-out)]{

The @racket[all-defined-out] shorthand exports all bindings that are
defined within the exporting module (as opposed to imported).

Use of the @racket[all-defined-out] shorthand is generally
discouraged, because it makes less clear the actual exports for a
module, and because Racket programmers get into the habit of
thinking that definitions can be added freely to a module without
affecting its public interface (which is not the case when
@racket[all-defined-out] is used).

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (all-from-out)
                 (all-from-out module-path)]{

The @racket[all-from-out] shorthand exports all bindings in the module
that were imported using a @racket[_require-spec] that is based on
@racket[module-path].

Although different @racket[module-path]s could refer to the same
file-based module, re-exporting with @racket[all-from-out] is based
specifically on the @racket[module-path] reference, and not the module
that is actually referenced.

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (except-out)
                 (except-out provide-spec id ...)]{

Like @racket[provide-spec], but omitting the export of each
@racket[id], where @racket[id] is the external name of the binding to
omit.

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals (prefix-out)
                 (prefix-out prefix-id provide-spec)]{

Like @racket[provide-spec], but adding @racket[prefix-id] to the
beginning of the external name for each exported binding.

}

