#lang scribble/doc
@(require scribble/manual
          "common.ss")

@(define rare @emph{This mode is rarely useful.})

@title{@exec{mzc}: Compilation and Packaging}

The @exec{mzc} tool is a kind of Swiss-army knife for PLT Scheme
compilation and packaging tasks. Its main action is determined through
one of the following command-line flags:

@itemize{

 @item{@as-index{@DFlag{make}} or @as-index{@Flag{k}} (the default) :
       Compiles Scheme modules and all transitive imports to
       bytecode. See @secref["make"].}

 @item{@as-index{@DFlag{cc}}, @as-index{@DFlag{ld}},
       @as-index{@DFlag{xform}} or @as-index{@Flag{x}} : Compiles,
       links or transforms (for GC cooperation) C code to extend the
       PLT Scheme runtime system. See @secref["cc"]. Using the
       @scheme[scheme/foreign] FFI is often better; see
       @other-manual['(lib "scribblings/foreign/foreign.scrbl")].}

 @item{@as-index{@DFlag{exe}}, @as-index{@DFlag{gui-exe}}, or
       @as-index{@DFlag{exe-dir}} : Creates an executable to run a
       Scheme module, or assembles all support libraries to move an
       executable to a new filesystem. See @secref["exe"].}

 @item{@as-index{@DFlag{collection-plt}} or @as-index{@DFlag{plt}} :
       packages Scheme code for installation into a different PLT
       Scheme installation. See @secref["plt"]. @|PLaneT| is usually a
       better alternative.}

 @item{@as-index{@DFlag{extension}}, @as-index{@Flag{e}},
       @as-index{@DFlag{c-source}}, or @as-index{@Flag{c}} : Compiles
       Scheme code to a native-code extension via C. See
       @secref["ext"]. @|rare|}

 @item{@as-index{@DFlag{zo}}, @as-index{@Flag{z}}, or
       @as-index{@DFlag{collection-zo}} : Compiles Scheme code to
       bytecode, without following transitive imports. See
       @secref["zo"]. @|rare|}

}

@include-section["make.scrbl"]
@include-section["cc.scrbl"]
@include-section["exe.scrbl"]
@include-section["plt.scrbl"]
@include-section["ext.scrbl"]
@include-section["zo.scrbl"]
