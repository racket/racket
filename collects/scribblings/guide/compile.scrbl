#lang scribble/doc
@(require scribble/manual
          "guide-utils.ss")

@title[#:tag "compile"]{Compilation and Configuration}

So far, we have talked about three main PLT Scheme executables:

@itemize[

 @item{DrScheme, which is the development environment.}

 @item{@exec{mzscheme}, which is the console-based virtual machine for
       running PLT Scheme programs (and that can be used as a
       development environment in interactive mode);}

 @item{@exec{mred}, which is like @scheme{mzscheme}, but for GUI
       applications.}

]

Three more executables help in compiling PLT Scheme programs and in
maintaining a PLT Scheme installation:

@itemize[

 @item{@exec{mzc} is a command-line tool for miscellaneous tasks, such
 as compiling Scheme source to bytecode, generating executables, and
 building distribution packages, and compiling C-implemented
 extensions to work with the run-time system. The @exec{mzc} is
 described in @other-manual['(lib
 "scribblings/mzc/mzc.scrbl")].

 For example, if you have a program @filepath{take-over-world.ss} and
 you'd like to compile it to bytecode, along with all of its
 dependencies, so that it loads more quickly, then run

   @commandline{mzc take-over-the-world.ss}}

 @item{@exec{setup-plt} is a command-line tool for managing a PLT
 Scheme installation, including manually installed packages. The
 @exec{setup-plt} tool is described in @other-manual['(lib
 "scribblings/setup-plt/setup-plt.scrbl")].

 For example, if you create your own library @techlink{collection}
 called @filepath{take-over}, and you'd like to build all bytecode and
 documentation for the collection, then run

   @commandline{setup-plt -l take-over}}

 @item{@exec{planet} is a command-line tool for managing packages that
 are normally downloaded automatically, on demand. The @exec{planet}
 tool is described in @other-manual['(lib "planet/planet.scrbl")].

 For example, if you'd like to see a list of @|PLaneT| packages that
 are currently installed, then run

    @commandline{planet show}}

]
