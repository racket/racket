#lang scribble/doc

@(require scribble/manual)

@title[#:tag "colors"]{Colors: colors.*}

Add 
@verbatim[#:indent 3]{
  import colors.*
}
at the top of your Definitions Window to import this library. 

This package provides classes for representing colors: 
@verbatim[#:indent 3]{
                              +--------+
                              | IColor |
                              +--------+
                                  |
                                 / \
                                 ---
                                  |
             --------------------------------------------------
             |          |         |         |         |       |
         +-------+  +-------+ +-------+ +-------+ +-------+  +-------+ 
         | Blue  |  | Green | | Red   | | White | | Yellow|  | Black | 
         +-------+  +-------+ +-------+ +-------+ +-------+  +-------+ 
}

@deftech{IColor} is an interface. Its variants are created with no arguments. 
