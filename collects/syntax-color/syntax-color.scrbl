#lang scribble/doc
@(require scribble/manual
          (for-label syntax-color/token-tree
                     syntax-color/paren-tree
                     syntax-color/scheme-lexer
                     syntax-color/module-lexer
                     syntax-color/scribble-lexer
                     syntax-color/default-lexer
                     framework/framework
                     framework/private/color
                     scheme))

@title{Syntax Color: Utilities}

@author["Scott Owens"]

The @filepath{syntax-color} collection provides the underlying data
structures and some helpful utilities for the @racket[color:text%]
class of the @other-manual['(lib
"scribblings/framework/framework.scrbl")].

@; ----------------------------------------------------------------------

@section{Parenthesis Matching}

@defmodule[syntax-color/paren-tree]

@defclass[paren-tree% object% ()]

Parenthesis matching code built on top of @racket[token-tree%].

@; ----------------------------------------------------------------------

@section{Scheme Lexer}

@defmodule[syntax-color/scheme-lexer]

@defproc[(scheme-lexer [in input-port?]) 
         (values (or/c string? eof-object?) 
                 symbol?
                 (or/c symbol? false/c) 
                 (or/c number? false/c) 
                 (or/c number? false/c))]{

A lexer for Scheme, including reader extensions (@secref[#:doc'(lib
"scribblings/reference/reference.scrbl")]{Reader_Extension}), built
specifically for @racket[color:text%].

The @racket[scheme-lexer] function returns 5 values:

@itemize[
  @item{Either a string containing the matching text or the eof object.  
   Block comments and specials currently return an empty string.  
   This may change in the future to other string or non-string data.}

  @item{A symbol in @racket['(error comment sexp-comment 
   white-space constant string no-color parenthesis other symbol eof)].}

  @item{A symbol in @racket['(|(| |)| |[| |]| |{| |}|)] or @racket[#f].}

  @item{A number representing the starting position of the match (or @racket[#f] if eof).}

  @item{A number representing the ending position of the match (or @racket[#f] if eof).}]

}

@defproc[(scheme-lexer/status [in input-port?]) 
         (values (or/c string? eof-object?) 
                 symbol?
                 (or/c symbol? false/c) 
                 (or/c number? false/c) 
                 (or/c number? false/c)
                 (or/c 'datum 'open 'close 'continue))]{

Like @racket[scheme-lexer], but returns an extra value. The last
return value indicates whether the consumed token should count as a
datum, an opening parenthesis (or similar starting token to group
other tokens), a closing parenthesis (or similar), or a prefix (such
as whitespace) on a datum.}

@defproc[(scheme-nobar-lexer/status [in input-port?]) 
         (values (or/c string? eof-object?) 
                 symbol?
                 (or/c symbol? false/c) 
                 (or/c number? false/c) 
                 (or/c number? false/c)
                 (or/c 'datum 'open 'close 'continue))]{

Like @racket[scheme-lexer/status], but for a dialect of Scheme where
@litchar{|} is a delimiter instead of quoting syntax for a symbol.
This function is used by @racket[scribble-lexer].}


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
@racket[color:text%].

@racket[default-lexer] returns 5 values:

@itemize[
  @item{Either a string containing the matching text or the eof object.  
   Block specials currently return an empty string.  
   This may change in the future to other string or non-string data.}

  @item{A symbol in @racket['(comment white-space no-color eof)].}

  @item{A symbol in @racket['(|(| |)| |[| |]| |{| |}|)] or @racket[#f].}

  @item{A number representing the starting position of the match (or @racket[#f] if eof).}

  @item{A number representing the ending position of the match (or @racket[#f] if eof).}]


@section{Module Lexer}

@defmodule[syntax-color/module-lexer]

@defproc[(module-lexer [in input-port?]
                       [offset exact-nonnegative-integer?]
                       [mode (or/c #f
                                   (-> input-port? any)
                                   (cons/c (-> input-port? any/c any) any/c))])
         (values (or/c string? eof-object?) 
                 symbol?
                 (or/c symbol? false/c) 
                 (or/c number? false/c) 
                 (or/c number? false/c)
                 exact-nonnegative-integer?
                 (or/c #f 
                       (-> input-port? any)
                       (cons/c (-> input-port? any/c any) any/c)))]{

Like @racket[scheme-lexer], but with several differences:

@itemize[

 @item{The @racket[module-lexer] function accepts an offset and lexer
       mode, instead of just an input port.}

 @item{In addition to the results of @racket[scheme-lexer],
       @racket[module-lexer] returns a backup distance and a new lexer
       mode.}

 @item{When @racket[mode] is @racket[#f] (indicating the start of the
       stream), the lexer checks @racket[in] for a @hash-lang[]
       specification.

       If a @hash-lang[] line is present but the specified
       language does not exist, the entire @racket[in] input is
       consumed and colored as @racket['error].

       If the language exists and the language provides a
       @racket[get-info] function, then it is called with
       @racket['color-lexer]. If the result is not @racket[#f], then
       it should be a lexer function for use with
       @racket[color:text%]. The result mode is the lexer---paired
       with @racket[#f] if the lexer is a procedure arity 3---so that
       future calls will dispatch to the language-supplied lexer.

       If the language is specified but it provides no
       @racket[get-info] or @racket['color-lexer] result, then
       @racket[scheme-lexer] is returned as the mode.}

 @item{When @racket[mode] is a lexer procedure, the lexer is applied
       to @racket[in]. The lexer's results are returned, plus the
       lexer again as the mode.}

 @item{When @racket[mode] is a pair, then the lexer procedure in the
       @racket[car] is applied to @racket[in], @racket[offset], and the mode in the
       @racket[cdr]. The lexer's results are returned, except that its
       mode result is paired back with the lexer procedure.}

]}

@section{Scribble Lexer}

@defmodule[syntax-color/scribble-lexer]

@defproc[(scribble-lexer [in input-port?]
                         [offset exact-nonnegative-integer?]
                         [mode any/c])
         (values (or/c string? eof-object?) 
                 symbol?
                 (or/c symbol? false/c) 
                 (or/c number? false/c) 
                 (or/c number? false/c)
                 exact-nonnegative-integer?
                 any/c)]{

Like @racket[scheme-lexer], but for Scheme extended with Scribbles
@"@" notation (see @secref[#:doc '(lib
"scribblings/scribble/scribble.scrbl") "reader"]).}

@defproc[(scribble-inside-lexer [in input-port?]
                                [offset exact-nonnegative-integer?]
                                [mode any/c])
         (values (or/c string? eof-object?) 
                 symbol?
                 (or/c symbol? false/c) 
                 (or/c number? false/c) 
                 (or/c number? false/c)
                 exact-nonnegative-integer?
                 any/c)]{

Like @racket[scribble-lexer], but starting in ``text'' mode instead of
Scheme mode.}

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
  offset @racket[key-position] (i.e., making the total length of
  the left tree at least @racket[key-position], if possible).
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

Functions for working with nodes in a @racket[token-tree%].}


@defproc[(insert-first! [tree1 (is-a?/c token-tree%)] 
                        [tree2 (is-a?/c token-tree%)]) 
          void?]{

Inserts @racket[tree1] into @racket[tree2] as the first thing, setting
@racket[tree2]'s root to @racket[#f].}


@defproc[(insert-last! [tree1 (is-a?/c token-tree%)] 
                       [tree2 (is-a?/c token-tree%)]) 
          void?]{

Inserts @racket[tree1] into @racket[tree2] as the last thing, setting
@racket[tree2]'s root to @racket[#f].}


@defproc[(insert-last-spec! [tree (is-a?/c token-tree%)] [n natural-number/c] [v any/c]) void?]{

Same as @racket[(insert-last! tree (new token-tree% [length n] [data
v]))]. This optimization is important for the colorer.}
