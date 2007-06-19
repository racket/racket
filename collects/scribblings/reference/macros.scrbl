#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:expansion"]{Syntax Expansion}

Expansion recursively processes a syntax object to parse it. In
general, the parsing of a datum depends on its outermost shape:

@itemize{

 @item{If it is a syntax-object symbol, also known as an
       @defterm{identifier}, then a binding is determined using symbol
       along with the lexical information in the identifier. The
       binding determines the next parsing step.}

 @item{If it is a syntax-object pair whose first element is an
      identifier, then the identifier's binding is used (as in the
      preceding case).}

 @item{If it is a syntax-object pair, then a new syntax-object symbol
       @scheme['#%app] is created using the lexical context of the
       pair. If the resulting @scheme[#%app] identifier has no
       binding, parsing fails with an @scheme[exn:fail:syntax]
       exception. Otherwise, the new identifier is combined with the
       original pair to form a new syntax-object pair (using the same
       context as the original pair), and parsing starts again (i.e.,
       it continues with the preceding case).}

 @item{If it is any other syntax object, then a new syntax-object
       symbol @scheme['#%datum] is created using the lexical context
       of the original syntax object. If the resulting
       @scheme[#%datum] identifier has no binding, parsing fails with
       an @scheme[exn:fail:syntax] exception. Otherwise, the new
       identifier is combined with the original syntax object in a new
       syntax-object pair (using the same context as the original
       pair), and parsing starts again (i.e., it continues with the
       second case above).}

}

For either of the first two steps, if the identifier has no binding,
then a new syntax-object symbol @scheme['#%top] is created using the
lexical context of the identifier; if this @scheme[#%top] identifier
has no binding, then parsing fails with an @scheme[exn:fail:syntax]
exception.  Otherwise, the new identifier is combined with the
original identifier in a new syntax-object pair (using the same
context as the original identifier), and parsing starts again.

Thus, the possibilities that do not fail lead to an identifier with a
particular binding. This binding refers to one of three things:

@itemize{

 @item{A transformer binding, such as introduced by
       @scheme[define-syntax] or @scheme[let-syntax]. If the
       associated value is to a procedure of one argument, the
       procedure is called as a syntax transformer (see
       @secref["transformers"]), and parsing starts again with the
       syntax-object result. If the transformer binding is to any
       other kind of value, parsing fails with an
       @scheme[exn:fail:syntax] exception.}

 @item{A variable binding, such as introduced by a module-level
       @scheme[define] or by @scheme[let]. In this case, if the form
       being parsed is just an identifier, then it is parsed as a
       run-time reference to the location of the corersponding
       variable. If the form being parsed is a syntax-object list,
       then an @scheme[#%app] is added to the front of the
       syntax-object list in the same way as when the first item in
       the syntax-object list is not an identifier (third case in the
       previous enumeration), and parsing continues.}

 @item{Core syntax, which is parsed as described in
       @secref["mz:syntax"]. Parsing core syntactic forms typically
       involves recursive parsing of sub-forms, and may introduce
       bindings that control the parsing of sub-forms.}

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

Different core syntax forms parse sub-forms in different contexts. For
example, a @scheme[let] form always parses the right-hand expressions
of a binding in an expression context, but it starts parsing the body
in an internal-definition context.

@section[#:tag "mz:intdef-body"]{Internal Definitions}

An internal-definition context corresponds to a partial expansion
step. A form that supports internal definitions starts by expanding
its first form in an internal-definition context, but only
partially. That is, it recursively expands only until the form becomes
one of the following:

@itemize{

 @item{A @scheme[define-values] or @scheme[define-syntaxes] form: The
       definition form is not expanded further. Instead, the next form
       is expanded partially, and so on. As soon as an expression form
       is found, the accumulated definition forms are converted to a
       @scheme[letrec-values] (if no @scheme[define-syntaxes] forms
       were found) or @scheme[letrec-syntaxes+values] form, moving the
       expression forms to the body to be expanded in expression
       context.
       
       When a @scheme[define-values] form is discovered, the lexical
       context of all syntax objects for the body sequence is
       immediately enriched with bindings for the
       @scheme[define-values] form before expansion continues. When a
       @scheme[define-syntaxes] form is discovered, the right-hand
       side is executed and a transformer binding is installed before
       expansion continues.}

 @item{A primitive expression form other than @scheme[begin]: The
       expression will be further expanded in an expression context,
       along with all remaining body forms. If any definitions were
       found, this expansion takes place after conversion to a
       @scheme[letrec-values] or @scheme[letrec-syntaxes+values]
       form. Otherwise, the expressions are expanded immediately in an
       expression context.}

 @item{A @scheme[begin] form: The sub-forms of the @scheme[begin] are
       spliced into the internal-definition sequence, and partial
       expansion continues with the first of the newly-spliced forms
       (or the next form, if the @scheme[begin] had no sub-forms).}

}

@section[#:tag "mz:fully-expanded"]{Fully Expanded Programs}

A fully expanded program---that is, a parsed program---is represented
in the same way as an unparsed program: as a syntax-object. However, a
fully expanded program fits a specific grammar.

@schemegrammar*[
#:literals (#%expression module #%plain-module-begin begin provide
            define-values define-syntaxes define-values-for-syntax
            require require-for-syntax require-for-template
            #%plain-lambda case-lambda if begin begin0 let-values letrec-values
            set! quote-syntax quote with-continuation-mark
            #%plain-app #%datum #%top #%variable-reference)
[top-level-form general-top-level-form
                (#%expression expr)
                (module id name-id 
                  (#%plain-module-begin 
                   module-level-form ...))
                (begin top-level-form ...)]
[module-level-form general-top-level-form
                   (provide provide-spec ...)]
[general-top-level-form expr
                        (define-values (id ...) expr)
                        (define-syntaxes (id ...) expr)
                        (define-values-for-syntax (id ...) expr)
                        (require require-spec ...)
                        (require-for-syntax require-spec ...)
                        (require-for-template require-spec ...)]
[expr id
      (#%plain-lambda formals expr ...+)
      (case-lambda (formals expr ...+) ...)
      (if expr expr)
      (if expr expr expr)
      (begin expr ...+)
      (begin0 expr expr ...)
      (let-values (((id ...) expr) ...) 
        expr ...+)
      (letrec-values (((id ...) expr) ...) 
        expr ...+)
      (set! id expr)
      (#, @scheme[quote] datum)
      (quote-syntax datum)
      (with-continuation-mark expr expr expr)
      (#%plain-app expr ...+)
      (#%datum . datum)
      (#%top . id)
      (#%variable-reference id)
      (#%variable-reference (#%top . id))]
[formals (id ...)
         (id ...+ . id)
         id]]
