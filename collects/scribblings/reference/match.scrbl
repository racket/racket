#lang scribble/doc
@(require "mz.ss"
          "match-grammar.ss"
          racket/match)

@(define match-eval (make-base-eval))
@(interaction-eval #:eval match-eval (require racket/match))

@title[#:tag "match"]{Pattern Matching}

@guideintro["match"]{pattern matching}

The @scheme[match] form and related forms support general pattern
matching on Scheme values. See also @secref["regexp"] for information
on regular-expression matching on strings, bytes, and streams.

@note-lib[racket/match #:use-sources (racket/match)]

@defform/subs[(match val-expr clause ...)
              ([clause [pat expr ...+]
                       [pat (=> id) expr ...+]])]{

Finds the first @scheme[pat] that matches the result of
@scheme[val-expr], and evaluates the corresponding @scheme[expr]s with
bindings introduced by @scheme[pat] (if any). The last @scheme[expr]
in the matching clause is evaluated in tail position with respect to
the @scheme[match] expression.

The @scheme[clause]s are tried in order to find a match. If no
@scheme[clause] matches, then the @exnraise[exn:misc:match?].

An optional @scheme[(=> id)] between a @scheme[pat] and the
@scheme[expr]s is bound to a @defterm{failure procedure} of zero
arguments.  If this procedure is invoked, it escapes back to the
pattern matching expression, and resumes the matching process as if
the pattern had failed to match.  The @scheme[expr]s must not mutate
the object being matched before calling the failure procedure,
otherwise the behavior of matching is unpredictable.

The grammar of @scheme[pat] is as follows, where non-italicized
identifiers are recognized symbolically (i.e., not by binding).

@|match-grammar|

In more detail, patterns match as follows:

@itemize[

 @item{@scheme[_id], excluding the reserved names @schemeidfont{_},
       @schemeidfont{...}, @schemeidfont{.._},
       @schemeidfont{..}@scheme[_k], and
       @schemeidfont{..}@scheme[_k] for non-negative integers
       @scheme[_k] --- matches anything, and binds @scheme[id] to the
       matching values. If an @scheme[_id] is used multiple times
       within a pattern, the corresponding matches must be the same
       according to @scheme[(match-equality-test)], except that
       instances of an @scheme[_id] in different @schemeidfont{or} and
       @schemeidfont{not} sub-patterns are independent.

       @examples[
       #:eval match-eval
       (match '(1 2 3)
         [(list a b a) (list a b)]
         [(list a b c) (list c b a)])
       (match '(1 '(x y z) 1)
         [(list a b a) (list a b)]
         [(list a b c) (list c b a)])
       ]}

 @item{@schemeidfont{_} --- matches anything, without binding any
       identifiers.

       @examples[
       #:eval match-eval
       (match '(1 2 3)
         [(list _ _ a) a])
       ]}

 @item{@scheme[#t], @scheme[#f], @scheme[_string], @scheme[_bytes],
       @scheme[_number], @scheme[_char], or @scheme[(#,(schemeidfont
       "quote") _datum)] --- matches an @scheme[equal?] constant.

       @examples[
       #:eval match-eval
       (match "yes"
         ["no" #f]
         ["yes" #t])
       ]}

 @item{@scheme[(#,(schemeidfont "list") _lvp ...)] --- matches a list
       of elements. In the case of @scheme[(#,(schemeidfont "list")
       _pat ...)], the pattern matches a list with as many element as
       @scheme[_pat]s, and each element must match the corresponding
       @scheme[_pat]. In the more general case, each @scheme[_lvp]
       corresponds to a ``spliced'' list of greedy matches.

       For spliced lists, @schemeidfont{...} and @schemeidfont{___}
       are synonyms for zero or more matches. The
       @schemeidfont{..}@scheme[_k] and @schemeidfont{__}@scheme[_k]
       forms are also synonyms, specifying @scheme[_k] or more
       matches. Pattern variables that precede these splicing
       operators are bound to lists of matching forms.

       @examples[
       #:eval match-eval
       (match '(1 2 3)
         [(list a b c) (list c b a)])
       (match '(1 2 3)
         [(list 1 a ...) a])
       (match '(1 2 3)
         [(list 1 a ..3) a]
         [_ 'else])
       (match '(1 2 3 4)
         [(list 1 a ..3) a]
         [_ 'else])
       (match '(1 2 3 4 5)
         [(list 1 a ..3 5) a]
         [_ 'else])
       (match '(1 (2) (2) (2) 5)
         [(list 1 (list a) ..3 5) a]
         [_ 'else])
       ]}

 @item{@scheme[(#,(schemeidfont "list-rest") _lvp ... _pat)] ---
       similar to a @schemeidfont{list} pattern, but the final
       @scheme[_pat] matches the ``rest'' of the list after the last
       @scheme[_lvp]. In fact, the matched value can be a non-list
       chain of pairs (i.e., an ``improper list'') if @scheme[_pat]
       matches non-list values.

      @examples[
       #:eval match-eval
      (match '(1 2 3 . 4)
        [(list-rest a b c d) d])
      (match '(1 2 3 . 4)
        [(list-rest a ... d) (list a d)])
      ]}

 @item{@scheme[(#,(schemeidfont "list-no-order") _pat ...)] ---
       similar to a @schemeidfont{list} pattern, but the elements to
       match each @scheme[_pat] can appear in the list in any order.

       @examples[
       #:eval match-eval
       (match '(1 2 3)
         [(list-no-order 3 2 x) x])
       ]}

 @item{@scheme[(#,(schemeidfont "list-no-order") _pat ... _lvp)] ---
       generalizes @schemeidfont{list-no-order} to allow a pattern
       that matches multiple list elements that are interspersed in
       any order with matches for the other patterns.

       @examples[
       #:eval match-eval
       (match '(1 2 3 4 5 6)
         [(list-no-order 6 2 y ...) y])
       ]}

 @item{@scheme[(#,(schemeidfont "vector") _lvp ...)] --- like a
       @schemeidfont{list} pattern, but matching a vector.

       @examples[
       #:eval match-eval
       (match #(1 (2) (2) (2) 5)
         [(vector 1 (list a) ..3 5) a])
       ]}

 @item{@scheme[(#,(schemeidfont "hash-table") (_pat _pat) ...)] ---
       similar to @schemeidfont{list-no-order}, but matching against
       hash table's key--value pairs.

       @examples[
       #:eval match-eval
       (match #hash(("a" . 1) ("b" . 2))
         [(hash-table ("b" b) ("a" a)) (list b a)])
       ]}

 @item{@scheme[(#,(schemeidfont "hash-table") (_pat _pat) ...+ _ooo)]
       --- Generalizes @schemeidfont{hash-table} to support a final
       repeating pattern.

       @examples[
       #:eval match-eval
       (match #hash(("a" . 1) ("b" . 2))
         [(hash-table (key val) ...) key])
       ]}

 @item{@scheme[(#,(schemeidfont "cons") _pat1 _pat2)] --- matches a pair value.

       @examples[
       #:eval match-eval
       (match (cons 1 2)
         [(cons a b) (+ a b)])
       ]}

 @item{@scheme[(#,(schemeidfont "mcons") _pat1 _pat2)] --- matches a mutable pair value.

       @examples[
       #:eval match-eval
       (match (mcons 1 2)
         [(cons a b) 'immutable]
	 [(mcons a b) 'mutable])
       ]}

 @item{@scheme[(#,(schemeidfont "box") _pat)] --- matches a boxed value.

       @examples[
       #:eval match-eval
       (match #&1
         [(box a) a])
       ]}

 @item{@scheme[(#,(schemeidfont "struct") _struct-id (_pat ...))] ---
       matches an instance of a structure type names
       @scheme[_struct-id], where each field in the instance matches
       the corresponding @scheme[_pat].

       Usually, @scheme[_struct-id] is defined with
       @scheme[define-struct].  More generally, @scheme[_struct-id]
       must be bound to expansion-time information for a structure
       type (see @secref["structinfo"]), where the information
       includes at least a predicate binding and field accessor
       bindings corresponding to the number of field
       @scheme[_pat]s. In particular, a module import or a
       @scheme[unit] import with a signature containing a
       @scheme[struct] declaration can provide the structure type
       information.

       @defexamples[
       #:eval match-eval
       (define-struct tree (val left right))
       (match (make-tree 0 (make-tree 1 #f #f) #f)
         [(struct tree (a (struct tree (b  _ _)) _)) (list a b)])
       ]}

 @item{@scheme[(#,(schemeidfont "struct") _struct-id _)] ---
       matches any instance of @scheme[_struct-id], without regard to
       contents of the fields of the instance.
       }

 @item{@scheme[(#,(schemeidfont "regexp") _rx-expr)] --- matches a
       string that matches the regexp pattern produced by
       @scheme[_rx-expr]; see @secref["regexp"] for more information
       about regexps.

       @examples[
       #:eval match-eval
       (match "apple"
         [(regexp #rx"p+") 'yes]
         [_ 'no])
       (match "banana"
         [(regexp #rx"p+") 'yes]
         [_ 'no])
       ]}

 @item{@scheme[(#,(schemeidfont "regexp") _rx-expr _pat)] --- extends
       the @schemeidfont{regexp} form to further constrain the match
       where the result of @scheme[regexp-match] is matched against
       @scheme[_pat].

       @examples[
       #:eval match-eval
       (match "apple"
         [(regexp #rx"p+(.)" (list _ "l")) 'yes]
         [_ 'no])
       (match "append"
         [(regexp #rx"p+(.)" (list _ "l")) 'yes]
         [_ 'no])
       ]}

 @item{@scheme[(#,(schemeidfont "pregexp") _rx-expr)] or
       @scheme[(#,(schemeidfont "regexp") _rx-expr _pat)] --- like the
       @schemeidfont{regexp} patterns, but if @scheme[_rx-expr]
       produces a string, it is converted to a pattern using
       @scheme[pregexp] instead of @scheme[regexp].}

 @item{@scheme[(#,(schemeidfont "and") _pat ...)] --- matches if all
       of the @scheme[_pat]s match.  This pattern is often used as
       @scheme[(#,(schemeidfont "and") _id _pat)] to bind @scheme[_id]
       to the entire value that matches @scheme[pat].

       @examples[
       #:eval match-eval
       (match '(1 (2 3) 4)
        [(list _ (and a (list _ ...)) _) a])
       ]}

 @item{@scheme[(#,(schemeidfont "or") _pat ...)] --- matches if any of
       the @scheme[_pat]s match. @bold{Beware}: the result expression
       can be duplicated once for each @scheme[_pat]! Identifiers in
       @scheme[_pat] are bound only in the corresponding copy of the
       result expression; in a module context, if the result
       expression refers to a binding, then that all @scheme[_pat]s
       must include the binding.

       @examples[
       #:eval match-eval
       (match '(1 2)
        [(or (list a 1) (list a 2)) a])
       ]}

 @item{@scheme[(#,(schemeidfont "not") _pat ...)] --- matches when
       none of the @scheme[_pat]s match, and binds no identifiers.

       @examples[
       #:eval match-eval
       (match '(1 2 3)
        [(list (not 4) ...) 'yes]
        [_ 'no])
       (match '(1 4 3)
        [(list (not 4) ...) 'yes]
        [_ 'no])
       ]}

 @item{@scheme[(#,(schemeidfont "app") _expr _pat)] --- applies
       @scheme[_expr] to the value to be matched; the result of the
       application is matched againt @scheme[_pat].

       @examples[
       #:eval match-eval
       (match '(1 2)
        [(app length 2) 'yes])
       ]}

 @item{@scheme[(#,(schemeidfont "?") _expr _pat ...)] --- applies
       @scheme[_expr] to the value to be matched, and checks whether
       the result is a true value; the additional @scheme[_pat]s must
       also match (i.e., @schemeidfont{?} combines a predicate
       application and an @schemeidfont{and} pattern).

       @examples[
       #:eval match-eval
       (match '(1 3 5)
        [(list (? odd?) ...) 'yes])
       ]}

  @item{@scheme[(#,(schemeidfont "quasiquote") _qp)] --- introduces a
        quasipattern, in which identifiers match symbols. Like the
        @scheme[quasiquote] expression form, @schemeidfont{unquote}
        and @schemeidfont{unquote-splicing} escape back to normal
        patterns.
        
        @examples[
       #:eval match-eval
        (match '(1 2 3)
          [`(1 ,a ,(? odd? b)) (list a b)])
        ]}

 @item{@scheme[_derived-pattern] --- matches a pattern defined by a
       macro extension via @scheme[define-match-expander].}

]}

@; ----------------------------------------------------------------------

@section{Combined Matching Forms}

@defform[(match-lambda clause ...)]{

Equivalent to @scheme[(lambda (id) (match id clause ...))].
}

@defform[(match-lambda* clause ...)]{

Equivalent to @scheme[(lambda lst (match lst clause ...))].
}

@defform[(match-let ([pat expr] ...) body ...+)]{

Generalizes @scheme[let] to support pattern bindings. Each
@scheme[expr] is matched against its corresponding @scheme[pat] (the
match must succeed), and the bindings that @scheme[pat] introduces are
visible in the @scheme[body]s.

@examples[
#:eval match-eval
(match-let ([(list a b) '(1 2)]
            [(vector x ...) #(1 2 3 4)])
  (list b a x))
]}

@defform[(match-let* ([pat expr] ...) body ...+)]{

Like @scheme[match-let], but generalizes @scheme[let*], so that the
bindings of each @scheme[pat] are available in each subsequent
@scheme[expr].

@examples[
#:eval match-eval
(match-let* ([(list a b) '(#(1 2 3 4) 2)]
             [(vector x ...) a])
  x)
]}

@defform[(match-letrec ([pat expr] ...) body ...+)]{

Like @scheme[match-let], but generalizes @scheme[letrec].}


@defform[(match-define pat expr)]{

Defines the names bound by @scheme[pat] to the values produced by
matching against the result of @scheme[expr].

@examples[
#:eval match-eval
(match-define (list a b) '(1 2))
b
]}

@; ----------------------------------------

@defproc[(exn:misc:match? [v any/c]) boolean?]{
A predicate for the exception raised by in the case of a match failure.
}


@; ----------------------------------------

@section{Extending @scheme[match]}

@defform*[((define-match-expander id proc-expr)
           (define-match-expander id proc-expr proc-expr))]{

Binds @scheme[id] to a @deftech{match expander}.

The first @scheme[proc-expr] subexpression must evaluate to a
 transformer that produces a @scheme[_pat] for @scheme[match].
 Whenever @scheme[id] appears as the beginning of a pattern, this
 transformer is given, at expansion time, a syntax object
 corresponding to the entire pattern (including @scheme[id]).  The
 pattern is the replaced with the result of the transformer.

A transformer produced by a second @scheme[proc-expr] subexpression is
 used when @scheme[id] is used in an expression context. Using the
 second @scheme[proc-expr], @scheme[id] can be given meaning both
 inside and outside patterns.}

@defparam[match-equality-test comp-proc (any/c any/c . -> . any)]{

A parameter that determines the comparison procedure used to check
whether multiple uses of an identifier match the ``same'' value. The
default is @scheme[equal?].}

@; ----------------------------------------------------------------------

@section{Library Extensions}

@defform[(struct* struct-id ([field pat] ...))]{
 Matches an instance of a structure type named @scheme[struct-id], where the field @scheme[field] in the instance matches the corresponding @scheme[pat].
                                                
 Any field of @scheme[struct-id] may be omitted and they may occur in any order.
 
 @defexamples[
  #:eval match-eval
  (define-struct tree (val left right))
  (match (make-tree 0 (make-tree 1 #f #f) #f)
    [(struct* tree ([val a]
                    [left (struct* tree ([right #f] [val b]))]))
     (list a b)])
 ]
 }

@; ----------------------------------------------------------------------

@close-eval[match-eval]
