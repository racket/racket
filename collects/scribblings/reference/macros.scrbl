#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:expansion"]{Syntax Expansion}

Expansion recursively processes a syntax-wrapped datum to parse it. In
general, the parsing of a datum depends on its outermost shape:

@itemize{

 @item{If it is a (syntax-wrapped) symbol, also known as an
       @defterm{identifier}, then a binding is determined using symbol
       along with the lexical information in the symbol's syntax
       wrapper. The binding determines the next parsing step.}

 @item{If it is a (syntax-wrapped) pair whose first element is an
      identifier, then the identifier's binding is used (as in the
      preceding case).}

 @item{If it is a (syntax-wrapped) pair, then the symbol
       @scheme['#%app] is wrapped with the lexical context of the
       pair's syntax wrapper. If the resulting @scheme[#%app]
       identifier has no binding, parsing fails with an
       @scheme[exn:fail:syntax] exception. Otherwise, the new
       identifier is @scheme[cons]ed with the pair, and then the pair
       is wrapped using the same context as the @scheme[#%app]
       identifier, and parsing starts again (i.e., it continues with
       the preceding case).}

 @item{If it is any other (syntax-wrapped) value, then the symbol
       @scheme['#%datum] is wrapped with the lexical context of the
       values syntax wrapper. If the resulting @scheme[#%datum]
       identifier has no binding, parsing fails with an
       @scheme[exn:fail:syntax] exception. Otherwise, the new
       identifier is @scheme[cons]ed with the pair, and then the pair
       is wrapped using the same context; parsing starts again (i.e.,
       it continues with the second case above).}

}

For either of the first two steps, if the identifier has no binding,
then the symbol @scheme['#%top] is wrapped with the same lexical
context as the identifier; is this @scheme[#%top] identifier has no
binding, then parsing fails with an @scheme[exn:fail:syntax]
exception.  Otherwise, parsing starts again, using the binding for
@scheme[#%top].

Thus, the possibilities that do not fail lead to an identifier with a
particular binding. This binding refers to one of three things:

@itemize{

 @item{A transformer binding, such as introduced by
       @scheme[define-syntax] or @scheme[let-syntax]. If the
       associated value is to a procedure of one argument, the
       procedure is called as a syntax transformer (see
       @secref["transformers"]), and parsing starts again with the
       syntax result. If the transformer binding is to any other kind
       of value, parsing fails with an @scheme[exn:fail:syntax]
       exception.}

 @item{A variable binding, such as introduced by a module-level
       @scheme[define] or by @scheme[let]. In this case, if the form
       being parsed is just an identifier, then it is parsed as a
       run-time reference to the location of the corersponding
       variable. If the form being parsed is a (syntax-wrapped) list,
       then an @scheme[#%app] is added to the from of the list in the
       same way as when the kfirst thing in the list is not an
       identifier (third case in the prvious enumeration), and parsing
       continues.}

 @item{Core syntax, which is parsed as described in the reminder of
       this section. Parsing core syntactic forms typically involves
       recursive parsing of sub-forms, and may introduce bindings that
       control the parsing of sub-forms.}

}

Each expansion step occurs in a particular context, and transformers
and core-syntax parsing can depend on the context. For example, a
@scheme[module] form is allowed only in a top-level context. The
possible contexts are as follows:

@itemize{

 @item{@defterm{top level} : outside of any module, definition, or
       expression, except that sub-expressions of a top-level
       @scheme[begin] form are also expanded as top-level forms.}

 @item{@defterm{module begin} : inside the body of a module, as the
       only form within the module.}

 @item{@defterm{module body} : in the body of a module (inside the
       moudule-begin layer).}

 @item{@defterm{internal definition} : in a nested context that allows
       both definitions and expressions.}

 @item{@defterm{expression} : in a context where only expressions are
       allowed.}

}

A fully expanded program---that is, a parsed program---is represented
in the same way as an unparsed program: as a syntax-wrapped
combination of symbols, pairs, and other values. However, a fully
expanded program fits a specific grammar.

@schemeblock[
#, @is-one-of[@scheme[_top-level-expr]]
  _general-top-level-expr
  (#%expression _expr)
  (module _id _name-id 
    (#%plain-module-begin _module-level-expr ...))
  (begin _top-level-expr ...)
code:blank
#, @is-one-of[@scheme[_module-level-expr]]
  _general-top-level-expr
  (provide _provide-spec ...)
code:blank
#, @is-one-of[@scheme[_general-top-level-expr]]
  _expr
  (define-values (_id ...) _expr)
  (define-syntaxes (_id ...) _expr)
  (define-values-for-syntax (_id ...) _expr)
  (require _require-spec ...)
  (require-for-syntax _require-spec ...)
  (require-for-template _require-spec ...)
code:blank
#, @is-one-of[@scheme[_expr]]
  _id
  (lambda _formals _expr ...+)
  (case-lambda (_formals _expr ...+) ...)
  (if _expr _expr)
  (if _expr _expr _expr)
  (begin _expr ...+)
  (begin0 _expr _expr ...)
  (let-values (((_id ...) _expr) ...) _expr ...+)
  (letrec-values (((_id ...) _expr) ...) _expr ...+)
  (set! _id _expr)
  (#, @scheme[quote] _datum)
  (quote-syntax _datum)
  (with-continuation-mark _expr _expr _expr)
  (#%app _expr ...+)
  (#%datum . _datum)
  (#%top . _id)
  (#%variable-reference _id)
  (#%variable-reference (#%top . _id))
code:blank
#, @is-one-of[@scheme[_formals]]
  (_id ...)
  (_id ...+ . _id)
  _id
]
