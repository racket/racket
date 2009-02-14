#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          (for-label scheme/base
                     scheme/contract
                     stxclass 
                     stxclass/util))

@(define ellipses @scheme[...])
@(define (TODO . pre-flow)
   (make-splice
    (cons (bold "TODO: ")
          (decode-content pre-flow))))

@title{Syntax Classes}
@declare-exporting[stxclass]

Syntax classes provide an abstraction mechanism for the specification
of syntax. Basic syntax classes include @scheme[identifier] and
@scheme[keyword]. More generally, a programmer can define a ``basic''
syntax from an arbitrary predicate, although syntax classes thus
defined lose some of the benefits of declarative specification of
syntactic structure.

Programmers can also compose basic syntax classes to build
specifications of more complex syntax, such as lists of distinct
identifiers and formal arguments with keywords. Macros that manipulate
the same syntactic structures can share syntax class definitions. The
structure of syntax classes and patterns also allows
@scheme[syntax-parse] to automatically generate error messages for
syntax errors.

When a syntax class accepts (matches, includes) a syntax object, it
computes and provides attributes based on the contents of the matched
syntax. While the values of the attributes depend on the matched
syntax, the set of attributes and each attribute's ellipsis nesting
depth is fixed for each syntax class.

@defform*/subs[#:literals (pattern basic-syntax-class)
               [(define-syntax-class name-id stxclass-option ...
                  stxclass-body)
                (define-syntax-class (name-id arg-id ...) stxclass-option ... 
                  stxclass-body)]
               ([stxclass-options
                 (code:line #:attributes (attr-arity-decl ...))
                 (code:line #:description description)
                 (code:line #:transparent)
                 (code:line #:literals (literal-entry ...))]
                [attr-arity-decl
                 attr-name-id
                 (attr-name-id depth)]
                [stxclass-body
                 (code:line (pattern syntax-pattern stxclass-pattern-directive ...) ...+)
                 (code:line (basic-syntax-class parser-expr))])]{

Defines @scheme[name-id] as a syntax class. When the @scheme[arg-id]s
are present, they are bound as variables (not pattern variables) in
the body. The body of the syntax-class definition contains either one
@scheme[basic-syntax-class] clause or a non-empty sequence of
@scheme[pattern] clauses.

@specsubform[(code:line #:attributes (attr-arity-decl ...))]{

Declares the attributes of the syntax class. An attribute arity
declaration consists of the attribute name and optionally its ellipsis
depth (zero if not explicitly specified).

If the attributes are not explicitly listed, they are computed using
@techlink{attribute inference}.

}

@specsubform[(code:line #:description description)]{

The @scheme[description] argument is an expression (with the
syntax-class's parameters in scope) that should evaluate to a
string. It is used in error messages involving the syntax class. For
example, if a term is rejected by the syntax class, an error of the
form @scheme["expected <description>"] may be generated.

If absent, the name of the syntax class is used instead.

}

@specsubform[#:transparent]{

Indicates that errors may be reported with respect to the internal
structure of the syntax class.
}

@specsubform[(code:line #:literals (literal-entry))]{

Declares the literal identifiers for the syntax class's main patterns
(immediately within @scheme[pattern] variants) and @scheme[#:with]
clauses. The literals list does not affect patterns that occur within
subexpressions inside the syntax class (for example, the condition of
a @scheme[#:when] clause or the right-hand side of a @scheme[#:with]
binding).

A literal can have separate internal and external names, as described
for @scheme[syntax-parse].

}

@specsubform/subs[#:literals (pattern)
                  (pattern syntax-pattern stxclass-pattern-directive ...)
                  ([stxclass-pattern-directive
                    pattern-directive
                    (code:line #:rename internal-id external-id)])]{

Accepts syntax matching the given pattern with the accompanying
pattern directives as in @scheme[syntax-parse].

The attributes of the pattern are the pattern variables within the
@scheme[pattern] form together with all pattern variables bound by
@scheme[#:with] clauses, including nested attributes produced by
syntax classes associated with the pattern variables. 

The name of an attribute is the symbolic name of the pattern variable,
except when the name is explicitly given via a @scheme[#:rename]
clause.

@specsubform[(code:line #:rename internal-id external-id)]{

Exports the pattern variable binding named by @scheme[internal-id] as
the attribute named @scheme[external-id].

}
}

@specsubform[#:literals (basic-syntax-class)
             (basic-syntax-class parser-expr)]{

The @scheme[parser-expr] must evaluate to a procedure. This procedure
is used to parse or reject syntax objects. The arguments to the parser
procedure consist of the syntax object to parse followed by the
syntax-class parameterization arguments (the parameter names given at
the @scheme[define-syntax-class] level are not bound within the
@scheme[parser-expr]). To indicate success, the parser should return a
list of attribute values, one for each attribute listed. (For example,
a parser for a syntax class that defines no attributes returns the
empty list when it succeeds.) To indicate failure, the parser
procedure should return @scheme[#f].

The parser procedure should avoid side-effects, as they interfere with
the parsing process's backtracking and error reporting.

@TODO{Add support for better error reporting within basic syntax
class.}

}

}

@defidform[pattern]{

Keyword recognized by @scheme[define-syntax-class]. It may not be
used as an expression.
}
@defidform[basic-syntax-class]{

Keyword recognized by @scheme[define-syntax-class]. It may not be used
as an expression.

}

@section{Attributes}

A syntax class has a set of @deftech{attribute}s. Each attribute has a
name, an ellipsis depth, and a set of nested attributes. When an
instance of the syntax class is parsed and bound to a pattern
variable, additional pattern variables are bound for each of the
syntax class's attributes. The name of these additional pattern
variables is the dotted concatenation of the the primary pattern
variable with the name of the attribute.

For example, if pattern variable @scheme[p] is bound to an instance of
a syntax class with attribute @scheme[a], then the pattern variable
@scheme[p.a] is bound to the value of that attribute. The ellipsis
depth of @scheme[p.a] is the sum of the depths of @scheme[p] and
attribute @scheme[a].

If the attributes are not declared explicitly, they are computed via
@deftech{attribute inference}. For ``basic'' syntax classes, the
inferred attribute list is always empty. For compound syntax classes,
each @scheme[pattern] form is analyzed to determine its candiate
attributes. The attributes of the syntax class are the attributes
common to all of the variants (that is, the intersection of the
candidate attributes). An attribute must have the same ellipsis-depth
in each of the variants; otherwise, an error is raised.

The candidate attributes of a @scheme[pattern] variant are the pattern
variables bound by the variant's pattern (including nested attributes
contributed by their associated syntax classes) together with the
pattern variables (and nested attributes) from @scheme[#:with]
clauses.

For the purpose of attribute inference, recursive references to the
same syntax class and forward references to syntax classes not yet
defined do not contribute any nested attributes. This avoids various
problems in computing attributes, including infinitely nested
attributes.

@section{Inspection tools}

The following special forms are for debugging syntax classes.

@defform[(syntax-class-attributes syntax-class-id)]{

Returns a list of the syntax class's attributes in flattened
form. Each attribute is listed by its name and ellipsis depth.

}

@defform[(syntax-class-parse syntax-class-id stx-expr arg-expr ...)]{

Runs the parser for the syntax class (parameterized by the
@scheme[arg-expr]s) on the syntax object produced by
@scheme[stx-expr]. On success, the result is a list of vectors
representing the attribute bindings of the syntax class. Each vector
contains the attribute name, depth, and associated value. On failure,
the result is some internal representation of the failure.

}
