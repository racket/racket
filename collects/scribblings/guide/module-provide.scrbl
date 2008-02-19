#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "module-provide"]{Exports: @scheme[provide]}

By default, all of a module's definitions are private to the
module. The @scheme[provide] form specifies definitions to be made
available where the module is @scheme[require]d.

@specform[(provide provide-spec ...)]{}

A @scheme[provide] form can only appear at module level (i.e., in the
immediate body of a @scheme[module]).  Specifying multiple
@scheme[_provide-spec]s in a single @scheme[provide] is exactly the
same as using multiple @scheme[provide]s each with a single
@scheme[_provide-spec].

Each identifier can be exported at most once from a module across all
@scheme[provide]s within the module. More precisely, the external name
for each export must be distinct; the same internal binding can be
exported multiple times with different external names.

The allowed shape of a @scheme[_provide-spec] is defined recursively:

@;------------------------------------------------------------------------
@specspecsubform[identifier]{

In its simplest form, a @scheme[_provide-spec] indicates a binding
within its module to be exported. The binding can be from either a
local definition, or from an import.

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals(rename-out)
                 (rename-out [orig-id export-id] ...)]{

A @scheme[rename-out] form is similar to just specifying an identifier,
but the exported binding @scheme[orig-id] is given a different name,
@scheme[export-id], to importing modules.

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals(struct-out)
                 (struct-out struct-id)]{

A @scheme[struct-out] form exports the bindings created by
@scheme[(define-struct struct-id ....)].

@guideother{See @secref["define-struct"] for information on
@scheme[define-struct].}

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals (all-defined-out)
                 (all-defined-out)]{

The @scheme[all-defined-out] shorthand exports all bindings that are
defined within the exporting module (as opposed to imported).

Use of the @scheme[all-defined-out] shorthand is generally
discouraged, because it makes less clear the actual exports for a
module, and because PLT Scheme programmers get into the habit of
thinking that definitions can be added freely to a module without
affecting its public interface (which is not the case when
@scheme[all-defined-out] is used).

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (all-from-out)
                 (all-from-out module-path)]{

The @scheme[all-from-out] shorthand exports all bindings in the module
that were imported using a @scheme[_require-spec] that is based on
@scheme[module-path].

Although different @scheme[module-path]s could refer to the same
file-based module, re-exporting with @scheme[all-from-out] is based
specifically on the @scheme[module-path] reference, and not the module
that is actually referenced.

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (except-out)
                 (except-out provide-spec id ...)]{

Like @scheme[provide-spec], but omitting the export of each
@scheme[id], where @scheme[id] is the external name of the binding to
omit.

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals (prefix-out)
                 (prefix-out prefix-id provide-spec)]{

Like @scheme[provide-spec], but adding @scheme[prefix-id] to the
beginning of the external name for each exported binding.

}

