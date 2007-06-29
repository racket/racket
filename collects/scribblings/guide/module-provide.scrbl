#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "guide:module-provide"]{Exports: @scheme[provide]}

By default, all of a module's definitions are private to the
module. The @scheme[provide] form specifies definitions to be made
available where the module is @scheme[require]d.

@specform[(provide provide-spec ...)]{}

A @scheme[provide] form can only appear at @tech{module level}.
Specifying multiple @scheme[_provide-spec]s in a single
@scheme[require] is exactly the same as using multiple
@scheme[provide]s each with a single @scheme[_provide-spec].

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
@specspecsubform[#:literals(rename)
                 (rename [orig-id export-id] ...)]{

A @scheme[rename] form is similar to just specifying an identifier,
but the exported binding @scheme[orig-id] is given a different name,
@scheme[export-id], to importing modules.

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals(all-defined)
                 (all-defined)]{

The @scheme[all-defined] shorthand exports all bindings that are
defined within the exporting module (as opposed to imported).

Use of the @scheme[all-defined] shorthand is generally discouraged,
because it makes less clear the actual exports for a module, and
because PLT Scheme programmers get into the habit of thinking that
definitions can be added freely to a module without affecting its
public interface.

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals(all-from)
                 (all-from module-path)]{

The @scheme[all-from] shorthand exports all bindings in the module
that were imported using a @scheme[_require-spec] that is based on
@scheme[module-path].

Although different @scheme[module-path]s could refer to the same
file-based module, re-exporting with @scheme[all-from] is based
specifically on the @scheme[module-path] reference, and not the module
that is actually referenced.

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals(except)
                 (except provide-spec id ...)]{

Like @scheme[provide-spec], but omitting the export of each
@scheme[id], where @scheme[id] is the external name of the binding to
omit.

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals(prefix)
                 (prefix provide-spec prefix-id)]{

Like @scheme[provide-spec], but adding @scheme[prefix-id] to the
beginning of the external name for each exported binding.

}

