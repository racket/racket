#lang scribble/doc
@(require scribble/manual
	  scribble/struct
	  scribble/xref
	  scribble/bnf
          (for-label scheme/base
                     scheme/contract
                     parser-tools/lex
                     (prefix-in : parser-tools/lex-sre)
                     parser-tools/yacc))

@title{@bold{Parser Tools}: @exec{lex} and @exec{yacc}-style Parsing}

@author["Scott Owens"]

This documentation assumes familiarity with @exec{lex} and @exec{yacc}
style lexer and parser generators.

@table-of-contents[]

@; ----------------------------------------------------------------------

@section{Lexers}

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
     @scheme[re]'s against the buffer, and returns the result of
     executing the corresponding @scheme[action-expr].

     @margin-note{The implementation of @schememodname[syntax-color/scheme-lexer]
                 contains a lexer for the @schememodname[scheme] language.
                 In addition, files in the @filepath{examples} sub-directory
                 of the @filepath{parser-tools} collection contain
                 simpler example lexers.}

     An @scheme[re] is matched as follows:

   @itemize[
    @item{@scheme[id] --- expands to the named @deftech{lexer abbreviation};
          abbreviations are defined via @scheme[define-lex-abbrev] or supplied by modules
          like @schememodname[parser-tools/lex-sre].}
    @item{@scheme[string] --- matches the sequence of characters in @scheme[string].}
    @item{@scheme[character] --- matches a literal @scheme[character].}
    @item{@scheme[(repetition lo hi re)] --- matches @scheme[re] repeated between @scheme[lo] 
          and @scheme[hi] times, inclusive; @scheme[hi] can be @scheme[+inf.0] for unbounded repetitions.}
    @item{@scheme[(union re ...)] --- matches if any of the sub-expressions match}
    @item{@scheme[(intersection re ...)] --- matches if all of the @scheme[re]s match.}
    @item{@scheme[(complement re)] --- matches anything that @scheme[re] does not.}
    @item{@scheme[(concatenation re ...)] --- matches each @scheme[re] in succession.}
    @item{@scheme[(char-range char char)] --- matches any character between the two (inclusive); 
         a single character string can be used as a @scheme[char].}
    @item{@scheme[(char-complement re)] --- matches any character not matched by @scheme[re].  
          The sub-expression must be a set of characters @scheme[re].}
    @item{@scheme[(id datum ...)] --- expands the @deftech{lexer macro} named @scheme[id]; macros
          are defined via @scheme[define-lex-trans].}
   ]

Note that both @scheme[(concatenation)] and @scheme[""] match the
empty string, @scheme[(union)] matches nothing,
@scheme[(intersection)] matches any string, and
@scheme[(char-complement (union))] matches any single character.

The regular expression language is not designed to be used directly,
but rather as a basis for a user-friendly notation written with
regular expression macros.  For example,
@schememodname[parser-tools/lex-sre] supplies operators from Olin
Shivers's SREs, and @schememodname[parser-tools/lex-plt-v200] supplies
(deprecated) operators from the previous version of this library.
Since those libraries provide operators whose names match other Scheme
bindings, such as @scheme[*] and @scheme[+], they normally must be
imported using a prefix:

@schemeblock[
(require (prefix-in : parser-tools/lex-sre))
]

The suggested prefix is @scheme[:], so that @scheme[:*] and
@scheme[:+] are imported.  Of course, a prefix other than @scheme[:]
(such as @scheme[re-]) will work too.

Since negation is not a common operator on regular expressions, here
are a few examples, using @scheme[:] prefixed SRE syntax:

@itemize[

@item{@schemeblock0[(complement "1")]

 Matches all strings except the string @scheme["1"], including
 @scheme["11"], @scheme["111"], @scheme["0"], @scheme["01"],
 @scheme[""], and so on.}

@item{@schemeblock0[(complement (:* "1"))]

 Matches all strings that are not sequences of @scheme["1"],
 including @scheme["0"], @scheme["00"], @scheme["11110"],
 @scheme["0111"], @scheme["11001010"] and so on.}

@item{@schemeblock0[(:& (:: any-string "111" any-string)
                        (complement (:or (:: any-string "01") (:+ "1"))))]

 Matches all strings that have 3 consecutive ones, but not those that
 end in @scheme["01"] and not those that are ones only.  These
 include @scheme["1110"], @scheme["0001000111"] and @scheme["0111"]
 but not @scheme[""], @scheme["11"], @scheme["11101"], @scheme["111"]
 and @scheme["11111"].}

@item{@schemeblock0[(:: "/*" (complement (:: any-string "*/" any-string)) "*/")]

 Matches Java/C block comments.  @scheme["/**/"],
 @scheme["/******/"], @scheme["/*////*/"], @scheme["/*asg4*/"] and so
 on.  It does not match @scheme["/**/*/"], @scheme["/* */ */"] and so
 on.  @scheme[(:: any-string "*/" any-string)] matches any string
 that has a @scheme["*/"] in is, so @scheme[(complement (:: any-string "*/"
 any-string))] matches any string without a @scheme["*/"] in it.}

@item{@schemeblock0[(:: "/*" (:* (complement "*/")) "*/")]

 Matches any string that starts with @scheme["/*"] and and ends with
 @scheme["*/"], including @scheme["/* */ */ */"].
 @scheme[(complement "*/")] matches any string except @scheme["*/"].
 This includes @scheme["*"] and @scheme["/"] separately.  Thus
 @scheme[(:* (complement "*/"))] matches @scheme["*/"] by first
 matching @scheme["*"] and then matching @scheme["/"].  Any other
 string is matched directly by @scheme[(complement "*/")].  In other
 words, @scheme[(:* (complement "xx"))] = @scheme[any-string].  It is
 usually not correct to place a @scheme[:*] around a
 @scheme[complement].}
]


     The following binding have special meaning inside of a lexer
     action:

     @itemize[
       @item{@scheme[start-pos] --- a @scheme[position] struct for the first character matched.}
       @item{@scheme[end-pos] --- a @scheme[position] struct for the character after the last character in the match.}
       @item{@scheme[lexeme] --- the matched string.}
       @item{@scheme[input-port] --- the input-port being
            processed (this is useful for matching input with multiple
            lexers).}
       @item{@scheme[(return-without-pos x)] is a function (continuation) that
	immediately returns the value of @scheme[x] from the lexer.  This useful
	in a src-pos lexer to prevent the lexer from adding source
	information.  For example:

	@schemeblock[
	(define get-token
	  (lexer-src-pos
	  ...
	  ((comment) (get-token input-port))
	  ...))
	]

	would wrap the source location information for the comment around
	the value of the recursive call.  Using
	@scheme[((comment) (return-without-pos (get-token input-port)))] 
	will cause the value of the recursive call to be returned without
	wrapping position around it.}
     ]

     The lexer raises an exception @scheme[(exn:read)] if none of the
     regular expressions match the input.  Hint: If @scheme[(any-char
     _custom-error-behavior)] is the last rule, then there will always
     be a match, and @scheme[_custom-error-behavior] is executed to
     handle the error situation as desired, only consuming the first
     character from the input buffer.

     In addition to returning characters, input
     ports can return @scheme[eof-object]s.  Custom input ports can
     also return a @scheme[special-comment] value to indicate a
     non-textual comment, or return another arbitrary value (a
     special). The non-@scheme[re] @scheme[trigger] forms handle these
     cases:

     @itemize[

       @item{The @scheme[(eof)] rule is matched when the input port
       returns an @scheme[eof-object] value.  If no @scheme[(eof)]
       rule is present, the lexer returns the symbol @scheme['eof]
       when the port returns an @scheme[eof-object] value.}

       @item{The @scheme[(special-comment)] rule is matched when the
       input port returns a @scheme[special-comment] structure.  If no
       @scheme[special-comment] rule is present, the lexer
       automatically tries to return the next token from the input
       port.}

       @item{The @scheme[(special)] rule is matched when the input
       port returns a value other than a character,
       @scheme[eof-object], or @scheme[special-comment] structure.  If
       no @scheme[(special)] rule is present, the lexer returns
       @scheme[(void)].}]

    End-of-files, specials, special-comments and special-errors cannot
    be parsed via a rule using an ordinary regular expression
    (but dropping down and manipulating the port to handle them
     is possible in some situations).

    Since the lexer gets its source information from the port, use
    @scheme[port-count-lines!] to enable the tracking of line and
    column information.  Otherwise, the line and column information
    will return @scheme[#f].

    When peeking from the input port raises an exception (such as by
    an embedded XML editor with malformed syntax), the exception can
    be raised before all tokens preceding the exception have been
    returned.

    Each time the scheme code for a lexer is compiled (e.g. when a
    @filepath{.ss} file containing a @scheme[lexer] form is loaded),
    the lexer generator is run.  To avoid this overhead place the
    lexer into a module and compile the module to a @filepath{.zo}
    bytecode file.}

@defform[(lexer-src-pos (trigger action-expr) ...)]{

Like @scheme[lexer], but for each @scheme[_action-result] produced by
an @scheme[action-expr], returns @scheme[(make-position-token
_action-result start-pos end-pos)] instead of simply
@scheme[_action-result].}

@deftogether[(
@defidform[start-pos]
@defidform[end-pos]
@defidform[lexeme]
@defidform[input-port]
@defidform[return-without-pos]
)]{

Use of these names outside of a @scheme[lexer] action is a syntax
error.}

@defstruct[position ([offset exact-positive-integer?]
                     [line exact-positive-integer?]
                     [col exact-nonnegative-integer?])]{

   Instances of @scheme[position] are bound to @scheme[start-pos] and
   @scheme[end-pos]. The @scheme[offset] field contains the offset of
   the character in the input.  The @scheme[line] field contains the
   line number of the character.  The @scheme[col] field contains the
   offset in the current line.}

@defstruct[position-token ([token any/c]
                           [start-pos position?]
                           [end-pos position?])]{

   Lexers created with @scheme[src-pos-lexers] return instances of @scheme[position-token].}


@defparam[file-path source any/c]{

 A parameter that the lexer uses as the source location if it
 raises a @scheme[exn:fail:rad] error.  Setting this parameter allows
 DrRacket, for example, to open the file containing the error.}


@; ----------------------------------------

@subsection{Lexer Abbreviations and Macros}

@defform[(char-set string)]{

A @tech{lexer macro} that matches any character in @scheme[string].}

@defidform[any-char]{A @tech{lexer abbreviation} that matches any character.}

@defidform[any-string]{A @tech{lexer abbreviation} that matches any string.}

@defidform[nothing]{A @tech{lexer abbreviation} that matches no string.}

@deftogether[(
@defidform[alphabetic]
@defidform[lower-case]
@defidform[upper-case]
@defidform[title-case]
@defidform[symbolic]
@defidform[punctuation]
@defidform[graphic]
@defidform[whitespace]
@defidform[blank]
@defidform[iso-control]
)]{

@tech{Lexer abbreviations} that match @scheme[char-alphabetic?]
characters, @scheme[char-lower-case?] characters, etc.}

@defform[(define-lex-abbrev id re)]{

     Defines a @tech{lexer abbreviation} by associating a regular
     expression to be used in place of the @scheme[id] in other
     regular expression.  The definition of name has the same scoping
     properties as a other syntactic binding (e.g., it can be exported
     from a module).}

@defform[(define-lex-abbrevs (id re) ...)]{
  
  Like @scheme[define-lex-abbrev], but defines several @tech{lexer
  abbreviations}.}


@defform[(define-lex-trans id trans-expr)]{

     Defines a @tech{lexer macro}, where @scheme[trans-expr] produces a
     transformer procedure that takes one argument.  When @scheme[(id
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

Repetition of @scheme[re] sequence 0 or more times.}

@defform[(+ re ...)]{

Repetition of @scheme[re] sequence 1 or more times.}

@defform[(? re ...)]{

Zero or one occurrence of @scheme[re] sequence.}

@defform[(= n re ...)]{

Exactly @scheme[n] occurrences of @scheme[re] sequence, where
@scheme[n] must be a literal exact, non-negative number.}

@defform[(>= n re ...)]{

At least @scheme[n] occurrences of @scheme[re] sequence, where
@scheme[n] must be a literal exact, non-negative number.}

@defform[(** n m re ...)]{

Between @scheme[n] and @scheme[m] (inclusive) occurrences of
@scheme[re] sequence, where @scheme[n] must be a literal exact,
non-negative number, and @scheme[m] must be literally either
@scheme[#f], @scheme[+inf.0], or an exact, non-negative number; a
@scheme[#f] value for @scheme[m] is the same as @scheme[+inf.0].}

@defform[(or re ...)]{

Same as @scheme[(union re ...)].}

@deftogether[(
@defform[(: re ...)]
@defform[(seq re ...)]
)]{

Both forms concatenate the @scheme[re]s.}

@defform[(& re ...)]{

Intersects the @scheme[re]s.}

@defform[(- re ...)]{

The set difference of the @scheme[re]s.}

@defform[(~ re ...)]{

Character-set complement, which each @scheme[re] must match exactly
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

@t{The @schememodname[parser-tools/lex-plt-v200] module re-exports
   @scheme[*], @scheme[+], @scheme[?], and @scheme[&] from
   @schememodname[parser-tools/lex-sre]. It also re-exports
   @scheme[:or] as @scheme[:], @scheme[::] as @scheme[|@|], @scheme[:~]
   as @scheme[^], and @scheme[:/] as @scheme[-].}

@defform[(epsilon)]{

A @tech{lexer macro} that matches an empty sequence.}

@defform[(~ re ...)]{

The same as @scheme[(complement re ...)].})))

@(lex-v200-doc)

@; ----------------------------------------

@subsection{Tokens}

Each @scheme[_action-expr] in a @scheme[lexer] form can produce any
kind of value, but for many purposes, producing a @deftech{token}
value is useful. Tokens are usually necessary for inter-operating with
a parser generated by @scheme[parser-tools/parser], but tokens not be
the right choice when using @scheme[lexer] in other situations.

@defform[(define-tokens group-id (token-id ...))]{

   Binds @scheme[group-id] to the group of tokens being defined.  For
   each @scheme[token-id], a function
   @schemeidfont{token-}@scheme[token-id] is created that takes any
   value and puts it in a token record specific to @scheme[token-id].
   The token value is inspected using @scheme[token-id] and
   @scheme[token-value].

   A token cannot be named @schemeidfont{error}, since
   @schemeidfont{error} it has special use in the parser.}

@defform[(define-empty-tokens group-id (token-id ...) )]{


   Like @scheme[define-tokens], except a each token constructor
   @schemeidfont{token-}@scheme[token-id] takes no arguments and returns
   @scheme[(@#,scheme[quote] token-id)].}


@defproc[(token-name [t (or/c token? symbol?)]) symbol?]{

   Returns the name of a token that is represented either by a symbol
   or a token structure.}


@defproc[(token-value [t (or/c token? symbol?)]) any/c]{

   Returns the value of a token that is represented either by a symbol
   or a token structure, returning @scheme[#f] for a symbol token.}


@defproc[(token? [v any/c]) boolean?]{

  Returns @scheme[#t] if @scheme[val] is a
  token structure, @scheme[#f] otherwise.}

@; ----------------------------------------------------------------------

@section{Parsers}

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
                       (@#,schemeidfont{error} expr)
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

      @item{@schemeblock0[(grammar (non-terminal-id 
                                    ((grammar-id ...) maybe-prec expr)
                                    ...)
                                   ...)]

      Declares the grammar to be parsed.  Each @scheme[grammar-id] can
      be a @scheme[token-id] from a @scheme[group-id] named in a
      @scheme[tokens] declaration, or it can be a
      @scheme[non-terminal-id] declared in the @scheme[grammar]
      declaration. The optional @scheme[prec] declaration works with
      the @scheme[precs] declaration. The @scheme[expr] is a
      ``semantic action,'' which is evaluated when the input is found
      to match its corresponding production.

      Each action is scheme code that has the same scope as its
      parser's definition, except that the variables @scheme[$1], ...,
      @schemeidfont{$}@math{i} are bound, where @math{i} is the number
      of @scheme[grammar-id]s in the corresponding production. Each
      @schemeidfont{$}@math{k} is bound to the result of the action
      for the @math{k}@superscript{th} grammar symbol on the right of
      the production, if that grammar symbol is a non-terminal, or the
      value stored in the token if the grammar symbol is a terminal.
      If the @scheme[src-pos] option is present in the parser, then
      variables @scheme[$1-start-pos], ...,
      @schemeidfont{$}@math{i}@schemeidfont{-start-pos} and
      @scheme[$1-end-pos], ...,
      @schemeidfont{$}@math{i}@schemeidfont{-end-pos} and are also
      available, and they refer to the position structures
      corresponding to the start and end of the corresponding
      @scheme[grammar-symbol]. Grammar symbols defined as empty-tokens
      have no @schemeidfont{$}@math{k} associated, but do have
      @schemeidfont{$}@math{k}@schemeidfont{-start-pos} and
      @schemeidfont{$}@math{k}@schemeidfont{-end-pos}.
      Also @schemeidfont{$n-start-pos} and @schemeidfont{$n-end-pos}
      are bound to the largest start and end positions, (i.e.,
      @schemeidfont{$}@math{i}@schemeidfont{-start-pos} and
      @schemeidfont{$}@math{i}@schemeidfont{-end-pos}).

      All of the productions for a given non-terminal must be grouped
      with it. That is, no @scheme[non-terminal-id] may appear twice
      on the left hand side in a parser.}


      @item{@scheme[(tokens group-id ...)]

      Declares that all of the tokens defined in each
      @scheme[group-id]---as bound by @scheme[define-tokens] or
      @scheme[define-empty-tokens]---can be used by the parser in the
      @scheme[grammar] declaration.}


      @item{@scheme[(start non-terminal-id ...)]

      Declares a list of starting non-terminals for the grammar.}


      @item{@scheme[(end token-id ...)]

      Specifies a set of tokens from which some member must follow any
      valid parse.  For example, an EOF token would be specified for a
      parser that parses entire files and a newline token for a parser
      that parses entire lines individually.}


      @item{@scheme[(@#,schemeidfont{error} expr)]

      The @scheme[expr] should evaluate to a function which will be
      executed for its side-effect whenever the parser encounters an
      error.

      If the @scheme[src-pos] declaration is present, the function
      should accept 5 arguments,:

      @schemeblock[(lambda (tok-ok? tok-name tok-value _start-pos _end-pos) 
                     ....)]

      Otherwise it should accept 3:

      @schemeblock[(lambda (tok-ok? tok-name tok-value) 
                     ....)]

      The first argument will be @scheme[#f] if and only if the error
      is that an invalid token was received.  The second and third
      arguments will be the name and the value of the token at which
      the error was detected.  The fourth and fifth arguments, if
      present, provide the source positions of that token.}


      @item{@scheme[(precs (assoc token-id ...) ...)]
      @italic{OPTIONAL}

      Precedence declarations to resolve shift/reduce and
      reduce/reduce conflicts as in @exec{yacc}/@exec{bison}. An
      @scheme[assoc] must be one of @scheme[left], @scheme[right] or
      @scheme[nonassoc].  States with multiple shift/reduce or
      reduce/reduce conflicts (or some combination thereof) are not
      resolved with precedence.}

      @item{@scheme[(src-pos)] @italic{OPTIONAL}

      Causes the generated parser to expect input in the form
      @scheme[(make-position-token _token _start-pos _end-pos)] instead
      of simply @scheme[_token].  Include this option when using the
      parser with a lexer generated with @scheme[lexer-src-pos].}


      @item{@scheme[(debug filename)] @italic{OPTIONAL}

      Causes the parser generator to write the LALR table to the file
      named @scheme[filename] (unless the file exists), where
      @scheme[filename] is a literal string.  Additionally, if a debug
      file is specified, when a running generated parser encounters a
      parse error on some input file, after the user specified error
      expression returns, the complete parse stack is printed to
      assist in debugging the grammar of that particular parser.  The
      numbers in the stack printout correspond to the state numbers in
      the LALR table file.}


      @item{@scheme[(yacc-output filename)] @italic{OPTIONAL}

      Causes the parser generator to write a grammar file in
      approximately the syntax of @exec{yacc}/@exec{bison}.  The file
      might not be a valid @exec{yacc} file, because the scheme
      grammar can use symbols that are invalid in C.}


      @item{@scheme[(suppress)] @italic{OPTIONAL}

      Causes the parser generator not to report shift/reduce or
      reduce/reduce conflicts.}

    ]

    The result of a @scheme[parser] expression with one @scheme[start]
    non-terminal is a function, @scheme[_parse], that takes one
    argument.  This argument must be a zero argument function,
    @scheme[_gen], that produces successive tokens of the input each
    time it is called.  If desired, the @scheme[_gen] may return
    symbols instead of tokens, and the parser will treat symbols as
    tokens of the corresponding name (with @scheme[#f] as a value, so
    it is usual to return symbols only in the case of empty tokens).
    The @scheme[_parse] function returns the value associated with the
    parse tree by the semantic actions.  If the parser encounters an
    error, after invoking the supplied error function, it will try to
    use error productions to continue parsing.  If it cannot, it
    raises @scheme[exn:fail:read].

    If multiple non-terminals are provided in @scheme[start], the
    @scheme[parser] expression produces a list of parsing functions,
    one for each non-terminal in the same order. Each parsing function
    is like the result of a parser expression with only one
    @scheme[start] non-terminal,

    Each time the scheme code for a @scheme[parser] is compiled
    (e.g. when a @filepath{.ss} file containing a @scheme[parser] form
    is loaded), the parser generator is run.  To avoid this overhead
    place the parser into a module and compile the module to a
    @filepath{.zo} bytecode file.}

@; ----------------------------------------------------------------------

@section{Converting @exec{yacc} or @exec{bison} Grammars}

@defmodule[parser-tools/yacc-to-scheme]

@defproc[(trans [file path-string?]) any/c]{

Reads a C @exec{yacc}/@exec{bison} grammar from @scheme[file] and
produces an s-expression that represents a scheme parser for use with
@scheme[parser].

This function is intended to assist in the manual conversion of
grammars for use with @scheme[parser], and not as a fully automatic
conversion tool.  It is not entirely robust.  For example, if the C
actions in the original grammar have nested blocks, the tool will fail.

Annotated examples are in the @filepath{examples} subdirectory of the
@filepath{parser-tools} collection.}

@; ----------------------------------------------------------------------

@index-section[]
