#lang scribble/doc
@require[scribble/manual]
@require["utils.ss"]

@title[#:tag "doclang"]{Document Module Language}

The @filepath{doclang.ss} module is suitable for use as a module
language. It provides everything from @scheme[mzscheme], except that
it replaces the @scheme[#%module-begin] form.

The @filepath{doclang.ss} @scheme[#%module-begin] essentially packages the
body of the module into a call to @scheme[decode], binds the result to
@scheme[doc], and exports @scheme[doc].

Any module-level form other than an expression (e.g., a
@scheme[require] or @scheme[define]) is remains at the top level, and
the @scheme[doc] binding is put at the end of the module. As usual, a
module-top-level @scheme[begin] slices into the module top level.
