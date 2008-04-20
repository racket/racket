#lang scribble/doc
@(require scribble/manual
          "common.ss")

@(define rare @emph{This mode is rarely useful.})

@title{@exec{mzc}: PLT Compilation and Packaging}

The @exec{mzc} tool supports various PLT Scheme compilation and
packaging tasks.

@table-of-contents[]

@section{Running @|mzc|}

The main action of @|mzc| is determined through one of the following
command-line flags:

@itemize{

 @item{@as-index{@DFlag{make}} (the default), @as-index{@Flag{k}}
       or @as-index{@DFlag{make-collection}} :
       Compiles Scheme modules and all transitive imports to
       bytecode. See @secref["make"].}

 @item{@as-index{@DFlag{exe}}, @as-index{@DFlag{gui-exe}}, or
       @as-index{@DFlag{exe-dir}} : Creates an executable to run a
       Scheme module, or assembles all support libraries to move an
       executable to a new filesystem. See @secref["exe"].}

 @item{@as-index{@DFlag{collection-plt}} or @as-index{@DFlag{plt}} :
       packages Scheme code for installation into a different PLT
       Scheme installation. See @secref["plt"]. @|PLaneT| is usually a
       better alternative.}

 @item{@as-index{@DFlag{cc}}, @as-index{@DFlag{ld}},
       @as-index{@DFlag{xform}} or @as-index{@Flag{x}} : Compiles,
       links or transforms (for GC cooperation) C code to extend the
       PLT Scheme runtime system. See @secref["cc"]. Using the
       @scheme[scheme/foreign] FFI is often better; see
       @other-manual['(lib "scribblings/foreign/foreign.scrbl")].}

 @item{@as-index{@DFlag{c-mods}} : Creates C source to embed Scheme
       modules into an executable that also embeds PLT Scheme. See
       @secref["c-mods"].}

 @item{@as-index{@DFlag{expand}} : Pretty-prints the macro-expanded
       form of a Scheme program.}

 @item{@as-index{@DFlag{zo}}, @as-index{@Flag{z}}, or
       @as-index{@DFlag{collection-zo}} : Compiles Scheme code to
       bytecode, without following transitive imports. See
       @secref["zo"]. @|rare|}

 @item{@as-index{@DFlag{extension}}, @as-index{@Flag{e}},
       @as-index{@DFlag{c-source}}, or @as-index{@Flag{c}} : Compiles
       Scheme code to a native-code extension via C. See
       @secref["ext"]. @|rare|}

}

@include-section["make.scrbl"]
@include-section["sa.scrbl"]
@include-section["plt.scrbl"]
@include-section["cc.scrbl"]
@include-section["c-mods.scrbl"]
@include-section["zo.scrbl"]
@include-section["ext.scrbl"]
@include-section["api.scrbl"]

@index-section[]
