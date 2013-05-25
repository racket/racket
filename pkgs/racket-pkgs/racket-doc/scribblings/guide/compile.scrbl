#lang scribble/doc
@(require scribble/manual "guide-utils.rkt")

@title[#:tag "compile"]{Compilation and Configuration: @exec{raco}}

The @exec{raco} (short for ``@bold{Ra}cket @bold{co}mmand'') program
provides a command-line interface to many additional tools for
compiling Racket programs and maintaining a Racket installation.

@itemize[

 @item{@exec{raco make} compiles Racket source to bytecode.

 For example, if you have a program @filepath{take-over-world.rkt} and
 you'd like to compile it to bytecode, along with all of its
 dependencies, so that it loads more quickly, then run

   @commandline{raco make take-over-the-world.rkt}

 The bytecode file is written as @filepath{take-over-the-world_rkt.zo}
 in a @filepath{compiled} subdirectory; @index[".zo"]{@filepath{.zo}}
 is the file suffix for a bytecode file.}


 @item{@exec{raco setup} manages a Racket installation, including
 manually installed packages.

 For example, if you create your own library @techlink{collection}
 called @filepath{take-over}, and you'd like to build all bytecode and
 documentation for the collection, then run

   @commandline{raco setup take-over}}


 @item{@exec{raco planet} manages packages that are normally
 downloaded automatically, on demand.

 For example, if you'd like to see a list of @|PLaneT| packages that
 are currently installed, then run

    @commandline{raco planet show}}

]

For more information on @exec{raco}, see @other-manual['(lib
"scribblings/raco/raco.scrbl")].
