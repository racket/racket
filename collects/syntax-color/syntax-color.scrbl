#lang scribble/doc
@(require scribble/manual
          (for-label syntax-color/token-tree
                     syntax-color/paren-tree
                     syntax-color/scheme-lexer
                     syntax-color/default-lexer
                     framework/framework
                     framework/private/color
                     scheme))

@title{@bold{Syntax Color}: Utilities}

The @filepath{syntax-color} collection provides the underlying data
structures and some helpful utilities for the @scheme[color:text%]
class of the @other-manual['(lib
"scribblings/framework/framework.scrbl")].

@; ----------------------------------------------------------------------

@section{Parenthesis Matching}

@defmodule[syntax-color/paren-tree]

@defclass[paren-tree% object% ()]

Parenthesis matching code built on top of @scheme[token-tree%].

@; ----------------------------------------------------------------------

@section{Scheme Lexer}

@defmodule[syntax-color/scheme-lexer]

@defproc[(scheme-lexer [in input-port?]) 
         (values (or/c string? eof-object?) 
                 symbol?
                 (or/c symbol? false/c) 
                 (or/c number? false/c) 
                 (or/c number? false/c))]

A lexer for Scheme, including reader extensions (@secref[#:doc'(lib
"scribblings/reference/reference.scrbl")]{Reader_Extension}), built
specifically for @scheme[color:text%].

The @scheme[scheme-lexer] function returns 5 values:

@itemize{
  @item{Either a string containing the matching text or the eof object.  
   Block comments and specials currently return an empty string.  
   This may change in the future to other string or non-string data.}

  @item{A symbol in @scheme['(error comment sexp-comment 
   white-space constant string no-color parenthesis other symbol eof)].}

  @item{A symbol in @scheme['(|(| |)| |[| |]| |{| |}|)] or @scheme[#f].}

  @item{A number representing the starting position of the match (or @scheme[#f] if eof).}

  @item{A number representing the ending position of the match (or @scheme[#f] if eof).}}

@section{Default lexer}
@defmodule[syntax-color/default-lexer]

@defproc[(default-lexer [in input-port?]) 
         (values (or/c string? eof-object?)
                 symbol? 
                 (or/c symbol? false/c) 
                 (or/c number? false/c)
                 (or/c number? false/c))]

A lexer that only identifies @litchar{(}, @litchar{)}, @litchar{[},
@litchar{]}, @litchar["{"], and @litchar["}"] built specifically for
@scheme[color:text%].

@scheme[default-lexer] returns 5 values:

@itemize{
  @item{Either a string containing the matching text or the eof object.  
   Block specials currently return an empty string.  
   This may change in the future to other string or non-string data.}

  @item{A symbol in @scheme['(comment white-space no-color eof)].}

  @item{A symbol in @scheme['(|(| |)| |[| |]| |{| |}|)] or @scheme[#f].}

  @item{A number representing the starting position of the match (or @scheme[#f] if eof).}

  @item{A number representing the ending position of the match (or @scheme[#f] if eof).}}

                     
@; ----------------------------------------------------------------------

@section{Splay Tree for Tokenization}
@defmodule[syntax-color/token-tree]

@defclass[token-tree% object% ()]{

A splay-tree class specifically geared for the task of on-the-fly
tokenization. Instead of keying nodes on values, each node has a
length, and they are found by finding a node that follows a certain
total length of preceding nodes.

FIXME: many methods are not yet documented.

 @defconstructor[([len (or/c exact-nonnegative-integer? fasle/c)]
                  [data any/c])]{
  Creates a token tree with a single element.
 }

 @defmethod[(get-root) (or/c node? false/c)]{
  Returns the root node in the tree.
 }

 @defmethod[(search! [key-position natural-number/c]) void?]{
  Splays, setting the root node to be the closest node to
  offset @scheme[key-position] (i.e., making the total length of
  the left tree at least @scheme[key-position], if possible).
 }

}

@deftogether[(
@defproc[(node? [v any/c]) boolean?]
@defproc[(node-token-length [n node?]) natural-number/c]
@defproc[(node-token-data [n node?]) any/c]
@defproc[(node-left-subtree-length [n node?]) natural-number/c]
@defproc[(node-left [n node?]) (or/c node? false/c)]
@defproc[(node-right [n node?]) (or/c node? false/c)]
)]{

Functions for working with nodes in a @scheme[token-tree%].}


@defproc[(insert-first! [tree1 (is-a?/c token-tree%)] 
                        [tree2 (is-a?/c token-tree%)]) 
          void?]{

Inserts @scheme[tree1] into @scheme[tree2] as the first thing, setting
@scheme[tree2]'s root to @scheme[#f].}


@defproc[(insert-last! [tree1 (is-a?/c token-tree%)] 
                       [tree2 (is-a?/c token-tree%)]) 
          void?]{

Inserts @scheme[tree1] into @scheme[tree2] as the last thing, setting
@scheme[tree2]'s root to @scheme[#f].}


@defproc[(insert-last-spec! [tree (is-a?/c token-tree%)] [n natural-number/c] [v any/c]) void?]{

Same as @scheme[(insert-last! tree (new token-tree% [length n] [data
v]))]. This optimization is important for the colorer.}
