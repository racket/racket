#lang scribble/doc
@(require scribble/manual scribble/struct scribble/xref scribble/bnf
          (for-label scheme/base
                     scheme/contract
                     parser-tools/lex
                     (prefix-in : parser-tools/lex-sre)
                     parser-tools/yacc))

@title{Parser Tools: @exec{lex} and @exec{yacc}-style Parsing}

@author["Scott Owens"]

This documentation assumes familiarity with @exec{lex} and @exec{yacc}
style lexer and parser generators.

@table-of-contents[]

@; ----------------------------------------------------------------------

@section{Lexers}

@section-index["lex"]
@section-index["scanning"]
@section-index["scanner"]

@defmodule[parser-tools/lex]

@; ----------------------------------------

@subsection{Creating a Lexer}

@defform/subs[#:literals (repetition union intersection complement concatenation
                          char-range char-complement
                          eof special special-comment)
              (lexer [trigger action-expr] ...)
              ([trigger re
                        (eof)
                        (special)
                        (special-comment)]
               [re id
                   string
                   character
                   (repetition lo hi re)
                   (union re ...)
                   (intersection re ...)
                   (complement re)
                   (concatenation re ...)
                   (char-range char char)
                   (char-complement re)
                   (id datum ...)])]{

     Produces a function that takes an input-port, matches the
     @racket[re]'s against the buffer, and returns the result of
     executing the corresponding @racket[action-expr].

     @margin-note{The implementation of @racketmodname[syntax-color/scheme-lexer]
                 contains a lexer for the @racketmodname[racket] language.
                 In addition, files in the @filepath{examples} sub-directory
                 of the @filepath{parser-tools} collection contain
                 simpler example lexers.}

     An @racket[re] is matched as follows:

   @itemize[
    @item{@racket[id] --- expands to the named @deftech{lexer abbreviation};
          abbreviations are defined via @racket[define-lex-abbrev] or supplied by modules
          like @racketmodname[parser-tools/lex-sre].}
    @item{@racket[string] --- matches the sequence of characters in @racket[string].}
    @item{@racket[character] --- matches a literal @racket[character].}
    @item{@racket[(repetition lo hi re)] --- matches @racket[re] repeated between @racket[lo]
          and @racket[hi] times, inclusive; @racket[hi] can be @racket[+inf.0] for unbounded repetitions.}
    @item{@racket[(union re ...)] --- matches if any of the sub-expressions match}
    @item{@racket[(intersection re ...)] --- matches if all of the @racket[re]s match.}
    @item{@racket[(complement re)] --- matches anything that @racket[re] does not.}
    @item{@racket[(concatenation re ...)] --- matches each @racket[re] in succession.}
    @item{@racket[(char-range char char)] --- matches any character between the two (inclusive); 
         a single character string can be used as a @racket[char].}
    @item{@racket[(char-complement re)] --- matches any character not matched by @racket[re].
          The sub-expression must be a set of characters @racket[re].}
    @item{@racket[(id datum ...)] --- expands the @deftech{lexer macro} named @racket[id]; macros
          are defined via @racket[define-lex-trans].}
   ]

Note that both @racket[(concatenation)] and @racket[""] match the
empty string, @racket[(union)] matches nothing,
@racket[(intersection)] matches any string, and
@racket[(char-complement (union))] matches any single character.

The regular expression language is not designed to be used directly,
but rather as a basis for a user-friendly notation written with
regular expression macros.  For example,
@racketmodname[parser-tools/lex-sre] supplies operators from Olin
Shivers's SREs, and @racketmodname[parser-tools/lex-plt-v200] supplies
(deprecated) operators from the previous version of this library.
Since those libraries provide operators whose names match other Racket
bindings, such as @racket[*] and @racket[+], they normally must be
imported using a prefix:

@racketblock[
(require (prefix-in : parser-tools/lex-sre))
]

The suggested prefix is @racket[:], so that @racket[:*] and
@racket[:+] are imported.  Of course, a prefix other than @racket[:]
(such as @racket[re-]) will work too.

Since negation is not a common operator on regular expressions, here
are a few examples, using @racket[:] prefixed SRE syntax:

@itemize[

@item{@racketblock0[(complement "1")]

 Matches all strings except the string @racket["1"], including
 @racket["11"], @racket["111"], @racket["0"], @racket["01"],
 @racket[""], and so on.}

@item{@racketblock0[(complement (:* "1"))]

 Matches all strings that are not sequences of @racket["1"],
 including @racket["0"], @racket["00"], @racket["11110"],
 @racket["0111"], @racket["11001010"] and so on.}

@item{@racketblock0[(:& (:: any-string "111" any-string)
                        (complement (:or (:: any-string "01") (:+ "1"))))]

 Matches all strings that have 3 consecutive ones, but not those that
 end in @racket["01"] and not those that are ones only.  These
 include @racket["1110"], @racket["0001000111"] and @racket["0111"]
 but not @racket[""], @racket["11"], @racket["11101"], @racket["111"]
 and @racket["11111"].}

@item{@racketblock0[(:: "/*" (complement (:: any-string "*/" any-string)) "*/")]

 Matches Java/C block comments.  @racket["/**/"],
 @racket["/******/"], @racket["/*////*/"], @racket["/*asg4*/"] and so
 on.  It does not match @racket["/**/*/"], @racket["/* */ */"] and so
 on.  @racket[(:: any-string "*/" any-string)] matches any string
 that has a @racket["*/"] in is, so @racket[(complement (:: any-string "*/"
 any-string))] matches any string without a @racket["*/"] in it.}

@item{@racketblock0[(:: "/*" (:* (complement "*/")) "*/")]

 Matches any string that starts with @racket["/*"] and ends with
 @racket["*/"], including @racket["/* */ */ */"].
 @racket[(complement "*/")] matches any string except @racket["*/"].
 This includes @racket["*"] and @racket["/"] separately.  Thus
 @racket[(:* (complement "*/"))] matches @racket["*/"] by first
 matching @racket["*"] and then matching @racket["/"].  Any other
 string is matched directly by @racket[(complement "*/")].  In other
 words, @racket[(:* (complement "xx"))] = @racket[any-string].  It is
 usually not correct to place a @racket[:*] around a
 @racket[complement].}
]


     The following binding have special meaning inside of a lexer
     action:

     @itemize[
       @item{@racket[start-pos] --- a @racket[position] struct for the first character matched.}
       @item{@racket[end-pos] --- a @racket[position] struct for the character after the last character in the match.}
       @item{@racket[lexeme] --- the matched string.}
       @item{@racket[input-port] --- the input-port being
            processed (this is useful for matching input with multiple
            lexers).}
       @item{@racket[(return-without-pos x)] is a function (continuation) that
	immediately returns the value of @racket[x] from the lexer.  This useful
	in a src-pos lexer to prevent the lexer from adding source
	information.  For example:

	@racketblock[
	(define get-token
	  (lexer-src-pos
	  ...
	  ((comment) (get-token input-port))
	  ...))
	]

	would wrap the source location information for the comment around
	the value of the recursive call.  Using
	@racket[((comment) (return-without-pos (get-token input-port)))] 
	will cause the value of the recursive call to be returned without
	wrapping position around it.}
     ]

     The lexer raises an exception @racket[(exn:read)] if none of the
     regular expressions match the input.  Hint: If @racket[(any-char
     _custom-error-behavior)] is the last rule, then there will always
     be a match, and @racket[_custom-error-behavior] is executed to
     handle the error situation as desired, only consuming the first
     character from the input buffer.

     In addition to returning characters, input
     ports can return @racket[eof-object]s.  Custom input ports can
     also return a @racket[special-comment] value to indicate a
     non-textual comment, or return another arbitrary value (a
     special). The non-@racket[re] @racket[trigger] forms handle these
     cases:

     @itemize[

       @item{The @racket[(eof)] rule is matched when the input port
       returns an @racket[eof-object] value.  If no @racket[(eof)]
       rule is present, the lexer returns the symbol @racket['eof]
       when the port returns an @racket[eof-object] value.}

       @item{The @racket[(special-comment)] rule is matched when the
       input port returns a @racket[special-comment] structure.  If no
       @racket[special-comment] rule is present, the lexer
       automatically tries to return the next token from the input
       port.}

       @item{The @racket[(special)] rule is matched when the input
       port returns a value other than a character,
       @racket[eof-object], or @racket[special-comment] structure.  If
       no @racket[(special)] rule is present, the lexer returns
       @racket[(void)].}]

    End-of-files, specials, special-comments and special-errors cannot
    be parsed via a rule using an ordinary regular expression
    (but dropping down and manipulating the port to handle them
     is possible in some situations).

    Since the lexer gets its source information from the port, use
    @racket[port-count-lines!] to enable the tracking of line and
    column information.  Otherwise, the line and column information
    will return @racket[#f].

    When peeking from the input port raises an exception (such as by
    an embedded XML editor with malformed syntax), the exception can
    be raised before all tokens preceding the exception have been
    returned.

    Each time the racket code for a lexer is compiled (e.g. when a
    @filepath{.rkt} file containing a @racket[lexer] form is loaded),
    the lexer generator is run.  To avoid this overhead place the
    lexer into a module and compile the module to a @filepath{.zo}
    bytecode file.}

@defform[(lexer-src-pos (trigger action-expr) ...)]{

Like @racket[lexer], but for each @racket[_action-result] produced by
an @racket[action-expr], returns @racket[(make-position-token
_action-result start-pos end-pos)] instead of simply
@racket[_action-result].}

@deftogether[(
@defidform[start-pos]
@defidform[end-pos]
@defidform[lexeme]
@defidform[input-port]
@defidform[return-without-pos]
)]{

Use of these names outside of a @racket[lexer] action is a syntax
error.}

@defstruct[position ([offset exact-positive-integer?]
                     [line exact-positive-integer?]
                     [col exact-nonnegative-integer?])]{

   Instances of @racket[position] are bound to @racket[start-pos] and
   @racket[end-pos]. The @racket[offset] field contains the offset of
   the character in the input.  The @racket[line] field contains the
   line number of the character.  The @racket[col] field contains the
   offset in the current line.}

@defstruct[position-token ([token any/c]
                           [start-pos position?]
                           [end-pos position?])]{

   Lexers created with @racket[src-pos-lexers] return instances of @racket[position-token].}


@defparam[file-path source any/c]{

 A parameter that the lexer uses as the source location if it
 raises a @racket[exn:fail:rad] error.  Setting this parameter allows
 DrRacket, for example, to open the file containing the error.}


@; ----------------------------------------

@subsection{Lexer Abbreviations and Macros}

@defform[(char-set string)]{

A @tech{lexer macro} that matches any character in @racket[string].}

@defidform[any-char]{A @tech{lexer abbreviation} that matches any character.}

@defidform[any-string]{A @tech{lexer abbreviation} that matches any string.}

@defidform[nothing]{A @tech{lexer abbreviation} that matches no string.}

@deftogether[(
@defidform[alphabetic]
@defidform[lower-case]
@defidform[upper-case]
@defidform[title-case]
@defidform[numeric]
@defidform[symbolic]
@defidform[punctuation]
@defidform[graphic]
@defidform[whitespace]
@defidform[blank]
@defidform[iso-control]
)]{

@tech{Lexer abbreviations} that match @racket[char-alphabetic?]
characters, @racket[char-lower-case?] characters, etc.}

@defform[(define-lex-abbrev id re)]{

     Defines a @tech{lexer abbreviation} by associating a regular
     expression to be used in place of the @racket[id] in other
     regular expression.  The definition of name has the same scoping
     properties as a other syntactic binding (e.g., it can be exported
     from a module).}

@defform[(define-lex-abbrevs (id re) ...)]{

  Like @racket[define-lex-abbrev], but defines several @tech{lexer
  abbreviations}.}


@defform[(define-lex-trans id trans-expr)]{

     Defines a @tech{lexer macro}, where @racket[trans-expr] produces a
     transformer procedure that takes one argument.  When @racket[(id
     _datum ...)] appears as a regular expression, it is replaced with
     the result of applying the transformer to the expression.}


@; ----------------------------------------

@subsection{Lexer SRE Operators}

@defmodule[parser-tools/lex-sre]

@; Put the docs in a macro, so that we can bound the scope of
@; the import of `*', etc.:
@(define-syntax-rule (lex-sre-doc)
   (...
    (begin
      (require (for-label parser-tools/lex-sre))

@defform[(* re ...)]{

Repetition of @racket[re] sequence 0 or more times.}

@defform[(+ re ...)]{

Repetition of @racket[re] sequence 1 or more times.}

@defform[(? re ...)]{

Zero or one occurrence of @racket[re] sequence.}

@defform[(= n re ...)]{

Exactly @racket[n] occurrences of @racket[re] sequence, where
@racket[n] must be a literal exact, non-negative number.}

@defform[(>= n re ...)]{

At least @racket[n] occurrences of @racket[re] sequence, where
@racket[n] must be a literal exact, non-negative number.}

@defform[(** n m re ...)]{

Between @racket[n] and @racket[m] (inclusive) occurrences of
@racket[re] sequence, where @racket[n] must be a literal exact,
non-negative number, and @racket[m] must be literally either
@racket[#f], @racket[+inf.0], or an exact, non-negative number; a
@racket[#f] value for @racket[m] is the same as @racket[+inf.0].}

@defform[(or re ...)]{

Same as @racket[(union re ...)].}

@deftogether[(
@defform[(: re ...)]
@defform[(seq re ...)]
)]{

Both forms concatenate the @racket[re]s.}

@defform[(& re ...)]{

Intersects the @racket[re]s.}

@defform[(- re ...)]{

The set difference of the @racket[re]s.}

@defform[(~ re ...)]{

Character-set complement, which each @racket[re] must match exactly
one character.}

@defform[(/ char-or-string ...)]{

Character ranges, matching characters between successive pairs of
characters.}

)))

@(lex-sre-doc)

@; ----------------------------------------

@subsection{Lexer Legacy Operators}

@defmodule[parser-tools/lex-plt-v200]

@(define-syntax-rule (lex-v200-doc)
   (...
    (begin
      (require (for-label parser-tools/lex-plt-v200))

@t{The @racketmodname[parser-tools/lex-plt-v200] module re-exports
   @racket[*], @racket[+], @racket[?], and @racket[&] from
   @racketmodname[parser-tools/lex-sre]. It also re-exports
   @racket[:or] as @racket[:], @racket[::] as @racket[|@|], @racket[:~]
   as @racket[^], and @racket[:/] as @racket[-].}

@defform[(epsilon)]{

A @tech{lexer macro} that matches an empty sequence.}

@defform[(~ re ...)]{

The same as @racket[(complement re ...)].})))

@(lex-v200-doc)

@; ----------------------------------------

@subsection{Tokens}

Each @racket[_action-expr] in a @racket[lexer] form can produce any
kind of value, but for many purposes, producing a @deftech{token}
value is useful. Tokens are usually necessary for inter-operating with
a parser generated by @racket[parser-tools/parser], but tokens not be
the right choice when using @racket[lexer] in other situations.

@defform[(define-tokens group-id (token-id ...))]{

   Binds @racket[group-id] to the group of tokens being defined.  For
   each @racket[token-id], a function
   @racketidfont{token-}@racket[token-id] is created that takes any
   value and puts it in a token record specific to @racket[token-id].
   The token value is inspected using @racket[token-id] and
   @racket[token-value].

   A token cannot be named @racketidfont{error}, since
   @racketidfont{error} it has special use in the parser.}

@defform[(define-empty-tokens group-id (token-id ...) )]{


   Like @racket[define-tokens], except a each token constructor
   @racketidfont{token-}@racket[token-id] takes no arguments and returns
   @racket[(@#,racket[quote] token-id)].}


@defproc[(token-name [t (or/c token? symbol?)]) symbol?]{

   Returns the name of a token that is represented either by a symbol
   or a token structure.}


@defproc[(token-value [t (or/c token? symbol?)]) any/c]{

   Returns the value of a token that is represented either by a symbol
   or a token structure, returning @racket[#f] for a symbol token.}


@defproc[(token? [v any/c]) boolean?]{

  Returns @racket[#t] if @racket[val] is a
  token structure, @racket[#f] otherwise.}

@; ----------------------------------------------------------------------

@section{Parsers}

@section-index["yacc"]

@defmodule[parser-tools/yacc]

@defform/subs[#:literals (grammar tokens start end precs src-pos
                          suppress debug yacc-output prec)
              (parser clause ...)
              ([clause (grammar (non-terminal-id 
                                 ((grammar-id ...) maybe-prec expr)
                                 ...)
                                ...)
                       (tokens group-id ...)
                       (start non-terminal-id ...)
                       (end token-id ...)
                       (@#,racketidfont{error} expr)
                       (precs (assoc token-id ...) ...)
                       (src-pos)
                       (suppress)
                       (debug filename)
                       (yacc-output filename)]
               [maybe-prec code:blank
                           (prec token-id)]
               [assoc left right nonassoc])]{

    Creates a parser. The clauses may be in any order, as long as there
    are no duplicates and all non-@italic{OPTIONAL} declarations are
    present:

    @itemize[

      @item{@racketblock0[(grammar (non-terminal-id
                                    ((grammar-id ...) maybe-prec expr)
                                    ...)
                                   ...)]

      Declares the grammar to be parsed.  Each @racket[grammar-id] can
      be a @racket[token-id] from a @racket[group-id] named in a
      @racket[tokens] declaration, or it can be a
      @racket[non-terminal-id] declared in the @racket[grammar]
      declaration. The optional @racket[prec] declaration works with
      the @racket[precs] declaration. The @racket[expr] is a
      ``semantic action,'' which is evaluated when the input is found
      to match its corresponding production.

      Each action is Racket code that has the same scope as its
      parser's definition, except that the variables @racket[$1], ...,
      @racketidfont{$}@math{i} are bound, where @math{i} is the number
      of @racket[grammar-id]s in the corresponding production. Each
      @racketidfont{$}@math{k} is bound to the result of the action
      for the @math{k}@superscript{th} grammar symbol on the right of
      the production, if that grammar symbol is a non-terminal, or the
      value stored in the token if the grammar symbol is a terminal.
      If the @racket[src-pos] option is present in the parser, then
      variables @racket[$1-start-pos], ...,
      @racketidfont{$}@math{i}@racketidfont{-start-pos} and
      @racket[$1-end-pos], ...,
      @racketidfont{$}@math{i}@racketidfont{-end-pos} and are also
      available, and they refer to the position structures
      corresponding to the start and end of the corresponding
      @racket[grammar-symbol]. Grammar symbols defined as empty-tokens
      have no @racketidfont{$}@math{k} associated, but do have
      @racketidfont{$}@math{k}@racketidfont{-start-pos} and
      @racketidfont{$}@math{k}@racketidfont{-end-pos}.
      Also @racketidfont{$n-start-pos} and @racketidfont{$n-end-pos}
      are bound to the largest start and end positions, (i.e.,
      @racketidfont{$}@math{i}@racketidfont{-start-pos} and
      @racketidfont{$}@math{i}@racketidfont{-end-pos}).

      All of the productions for a given non-terminal must be grouped
      with it. That is, no @racket[non-terminal-id] may appear twice
      on the left hand side in a parser.}


      @item{@racket[(tokens group-id ...)]

      Declares that all of the tokens defined in each
      @racket[group-id]---as bound by @racket[define-tokens] or
      @racket[define-empty-tokens]---can be used by the parser in the
      @racket[grammar] declaration.}


      @item{@racket[(start non-terminal-id ...)]

      Declares a list of starting non-terminals for the grammar.}


      @item{@racket[(end token-id ...)]

      Specifies a set of tokens from which some member must follow any
      valid parse.  For example, an EOF token would be specified for a
      parser that parses entire files and a newline token for a parser
      that parses entire lines individually.}


      @item{@racket[(@#,racketidfont{error} expr)]

      The @racket[expr] should evaluate to a function which will be
      executed for its side-effect whenever the parser encounters an
      error.

      If the @racket[src-pos] declaration is present, the function
      should accept 5 arguments,:

      @racketblock[(lambda (tok-ok? tok-name tok-value _start-pos _end-pos) 
                     ....)]

      Otherwise it should accept 3:

      @racketblock[(lambda (tok-ok? tok-name tok-value) 
                     ....)]

      The first argument will be @racket[#f] if and only if the error
      is that an invalid token was received.  The second and third
      arguments will be the name and the value of the token at which
      the error was detected.  The fourth and fifth arguments, if
      present, provide the source positions of that token.}


      @item{@racket[(precs (assoc token-id ...) ...)]
      @italic{OPTIONAL}

      Precedence declarations to resolve shift/reduce and
      reduce/reduce conflicts as in @exec{yacc}/@exec{bison}. An
      @racket[assoc] must be one of @racket[left], @racket[right] or
      @racket[nonassoc].  States with multiple shift/reduce or
      reduce/reduce conflicts (or some combination thereof) are not
      resolved with precedence.}

      @item{@racket[(src-pos)] @italic{OPTIONAL}

      Causes the generated parser to expect input in the form
      @racket[(make-position-token _token _start-pos _end-pos)] instead
      of simply @racket[_token].  Include this option when using the
      parser with a lexer generated with @racket[lexer-src-pos].}


      @item{@racket[(debug filename)] @italic{OPTIONAL}

      Causes the parser generator to write the LALR table to the file
      named @racket[filename] (unless the file exists), where
      @racket[filename] is a literal string.  Additionally, if a debug
      file is specified, when a running generated parser encounters a
      parse error on some input file, after the user specified error
      expression returns, the complete parse stack is printed to
      assist in debugging the grammar of that particular parser.  The
      numbers in the stack printout correspond to the state numbers in
      the LALR table file.}


      @item{@racket[(yacc-output filename)] @italic{OPTIONAL}

      Causes the parser generator to write a grammar file in
      approximately the syntax of @exec{yacc}/@exec{bison}.  The file
      might not be a valid @exec{yacc} file, because the Racket
      grammar can use symbols that are invalid in C.}


      @item{@racket[(suppress)] @italic{OPTIONAL}

      Causes the parser generator not to report shift/reduce or
      reduce/reduce conflicts.}

    ]

    The result of a @racket[parser] expression with one @racket[start]
    non-terminal is a function, @racket[_parse], that takes one
    argument.  This argument must be a zero argument function,
    @racket[_gen], that produces successive tokens of the input each
    time it is called.  If desired, the @racket[_gen] may return
    symbols instead of tokens, and the parser will treat symbols as
    tokens of the corresponding name (with @racket[#f] as a value, so
    it is usual to return symbols only in the case of empty tokens).
    The @racket[_parse] function returns the value associated with the
    parse tree by the semantic actions.  If the parser encounters an
    error, after invoking the supplied error function, it will try to
    use error productions to continue parsing.  If it cannot, it
    raises @racket[exn:fail:read].

    If multiple non-terminals are provided in @racket[start], the
    @racket[parser] expression produces a list of parsing functions,
    one for each non-terminal in the same order. Each parsing function
    is like the result of a parser expression with only one
    @racket[start] non-terminal,

    Each time the Racket code for a @racket[parser] is compiled
    (e.g. when a @filepath{.rkt} file containing a @racket[parser] form
    is loaded), the parser generator is run.  To avoid this overhead
    place the parser into a module and compile the module to a
    @filepath{.zo} bytecode file.}

@; ----------------------------------------------------------------------

@section{Converting @exec{yacc} or @exec{bison} Grammars}

@defmodule[parser-tools/yacc-to-scheme]

@defproc[(trans [file path-string?]) any/c]{

Reads a C @exec{yacc}/@exec{bison} grammar from @racket[file] and
produces an s-expression that represents a Racket parser for use with
@racket[parser].

This function is intended to assist in the manual conversion of
grammars for use with @racket[parser], and not as a fully automatic
conversion tool.  It is not entirely robust.  For example, if the C
actions in the original grammar have nested blocks, the tool will fail.

Annotated examples are in the @filepath{examples} subdirectory of the
@filepath{parser-tools} collection.}

@; ----------------------------------------------------------------------

@index-section[]
