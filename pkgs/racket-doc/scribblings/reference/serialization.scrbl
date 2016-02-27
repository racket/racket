#lang scribble/doc
@(require "mz.rkt" racket/serialize (for-label racket/serialize racket/fasl))

@(define ser-eval (make-base-eval))
@examples[#:hidden #:eval ser-eval (require racket/serialize)]

@title[#:tag "serialization"]{Serialization}

@note-lib-only[racket/serialize #:use-sources (racket/private/serialize)]

@defproc[(serializable? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] appears to be serializable, without
checking the content of compound values, and @racket[#f] otherwise.
See @racket[serialize] for an enumeration of serializable values.}

@; ----------------------------------------------------------------------

@defproc[(serialize [v serializable?]) any]{

Returns a value that encapsulates the value @racket[v]. This value
includes only readable values, so it can be written to a stream with
@racket[write] or @racket[s-exp->fasl], later read from a stream using
@racket[read] or @racket[fasl->s-exp], and then converted to a value
like the original using @racket[deserialize]. Serialization followed
by deserialization produces a value with the same graph structure and
mutability as the original value, but the serialized value is a plain
tree (i.e., no sharing).

The following kinds of values are serializable:

@itemize[

 @item{structures created through @racket[serializable-struct] or
       @racket[serializable-struct/versions], or more generally
       structures with the @racket[prop:serializable] property (see
       @racket[prop:serializable] for more information);}

 @item{@techlink{prefab} structures;}

 @item{instances of classes defined with @racket[define-serializable-class]
       or @racket[define-serializable-class*];}

 @item{@tech{booleans}, @tech{numbers}, @tech{characters}, @tech{interned} symbols,
       @tech{unreadable symbols}, @tech{strings}, @tech{byte strings}, @tech{paths} (for a
       specific convention), @|void-const|, and the empty list;}

 @item{@tech{pairs}, @tech{mutable pairs}, @tech{vectors}, @tech{flvectors}, @tech{fxvectors},
       @tech{box}es, @tech{hash tables}, and @tech{sets};}

 @item{@racket[date], @racket[date*], @racket[arity-at-least] and @racket[srcloc]
       structures; and}
 
 @item{@tech{module path index} values.}

]

Serialization succeeds for a compound value, such as a pair, only if
all content of the value is serializable.  If a value given to
@racket[serialize] is not completely serializable, the
@exnraise[exn:fail:contract].

If @racket[v] contains a cycle (i.e., a collection of objects that
are all reachable from each other), then @racket[v] can be serialized
only if the cycle includes a mutable value, where a @tech{prefab}
structure counts as mutable only if all of its fields are mutable.

@margin-note{The @racket[serialize] and @racket[deserialize] functions
currently do not handle certain cyclic values that @racket[read] and
@racket[write] can handle, such as @racket['@#,read[(open-input-string "#0=(#0#)")]].}

See @racket[deserialize] for information on the format of serialized
data.}

@; ----------------------------------------------------------------------

@defproc[(deserialize [v any/c]) any]{

Given a value @racket[v] that was produced by @racket[serialize],
produces a value like the one given to @racket[serialize], including
the same graph structure and mutability.

A serialized representation @racket[v] is a list of six or seven
elements:

@itemize[

 @item{An optional list @racket['(1)], @racket['(2)], or @racket['(3)] that represents
       the version of the serialization format. If the first element
       of a representation is not a list, then the version is
       @racket[0]. Version 1 adds support for mutable pairs,
       version 2 adds support for @tech{unreadable symbols},
       and version 3 adds support for @racket[date*] structures.}

 @item{A non-negative exact integer @racket[_s-count] that represents the
       number of distinct structure types represented in the
       serialized data.}

 @item{A list @racket[_s-types] of length @racket[_s-count], where
       each element represents a structure type. Each structure type
       is encoded as a pair. The @racket[car] of the pair is
       @racket[#f] for a structure whose deserialization information
       is defined at the top level, otherwise it is a quoted
       @tech{module path} or a byte string (to be converted into a
       platform-specific path using @racket[bytes->path]) for a module
       that exports the structure's deserialization information.  The
       @racket[cdr] of the pair is the name of a binding (at the top
       level or exported from a module) for deserialization
       information, either a symbol or a string representing an
       @tech{unreadable symbol}. These two are used with either
       @racket[namespace-variable-binding] or @racket[dynamic-require]
       to obtain deserialization information. See
       @racket[make-deserialize-info] for more information on the
       binding's value. See also @racket[deserialize-module-guard].}

 @item{A non-negative exact integer, @racket[_g-count] that represents the
       number of graph points contained in the following list.}

 @item{A list @racket[_graph] of length @racket[_g-count], where each element
       represents a serialized value to be referenced during the
       construction of other serialized values. Each list element is
       either a box or not:

      @itemize[

       @item{A box represents a value that is part of a cycle, and for
            deserialization, it must be allocated with @racket[#f] for
            each of its fields. The content of the box indicates the
            shape of the value:

            @itemize[

            @item{a non-negative exact integer @racket[_i] for an instance
                  of a structure type that is represented by the
                  @racket[_i]th element of the @racket[_s-types] list;}

            @item{@racket['c] for a pair, which fails on
                  deserialization (since pairs are immutable; this
                  case does not appear in output generated by
                  @racket[serialize]);}
 
            @item{@racket['m] for a mutable pair;}
 
            @item{@racket['b] for a box;}

            @item{a pair whose @racket[car] is @racket['v] and whose
                  @racket[cdr] is a non-negative exact integer @racket[_s]
                  for a vector of length @racket[_s];}

            @item{a list whose first element is @racket['h] and whose
                  remaining elements are symbols that determine the
                  hash-table type:

                  @itemize[
                    @item{@racket['equal] --- @racket[(make-hash)]}
                    @item{@racket['equal 'weak] --- @racket[(make-weak-hash)]}
                    @item{@racket['weak] --- @racket[(make-weak-hasheq)]}
                    @item{no symbols --- @racket[(make-hasheq)]}
                  ]}

            @item{@racket['date*] for a @racket[date*] structure, which
                  fails on deserialization (since dates are immutable;
                  this case does not appear in output generated by
                  @racket[serialize]);}

            @item{@racket['date] for a @racket[date] structure, which
                  fails on deserialization (since dates are immutable;
                  this case does not appear in output generated by
                  @racket[serialize]);}

            @item{@racket['arity-at-least] for an
                  @racket[arity-at-least] structure, which fails on
                  deserialization (since arity-at-least are immutable; this
                  case does not appear in output generated by
                  @racket[serialize]); or}

            @item{@racket['mpi] for a @tech{module path index}, which
                  fails on deserialization (since a module path index is immutable;
                  this case does not appear in output generated by
                  @racket[serialize]).}

            @item{@racket['srcloc] for a @racket[srcloc] structure, which
                  fails on deserialization (since srclocs are immutable;
                  this case does not appear in output generated by
                  @racket[serialize]).}
            ]

            The @racket[#f]-filled value will be updated with content specified
            by the fifth element of the serialization list @racket[v].}

       @item{A non-box represents a @defterm{serial} value to be
             constructed immediately, and it is one of the following:

            @itemize[

            @item{a boolean, number, character, interned symbol, or empty list,
                  representing itself.}

            @item{a string, representing an immutable string.}

            @item{a byte string, representing an immutable byte
                  string.}

            @item{a pair whose @racket[car] is @racket['?] and whose
                  @racket[cdr] is a non-negative exact integer
                  @racket[_i]; it represents the value constructed for the
                  @racket[_i]th element of @racket[_graph], where @racket[_i] is
                  less than the position of this element within
                  @racket[_graph].}

            @item{a pair whose @racket[car] is a number @racket[_i]; it
                  represents an instance of a structure type that is
                  described by the @racket[_i]th element of the
                  @racket[_s-types] list. The @racket[cdr] of the pair is
                  a list of serials representing arguments to be
                  provided to the structure type's deserializer.}

            @item{a pair whose @racket[car] is @racket['q] and whose
                  @racket[cdr] is an immutable value; it represents
                  the quoted value.}

            @item{a pair whose @racket[car] is @racket['f]; it
                  represents an instance of a @tech{prefab} structure
                  type. The @racket[cadr] of the pair is a @tech{prefab}
                  structure type key, and the @racket[cddr] is a list of
                  serials representing the field values.}

            @item{a pair whose @racket[car] is @racket['void],
                  representing @|void-const|.}

            @item{a pair whose @racket[car] is @racket['su] and whose
                  @racket[cdr] is a character string; it represents an
                  @tech{unreadable symbol}.}

            @item{a pair whose @racket[car] is @racket['u] and whose
                  @racket[cdr] is either a byte string or character
                  string; it represents a mutable byte or character
                  string.}

            @item{a pair whose @racket[car] is @racket['p] and whose
                  @racket[cdr] is a byte string; it represents a 
                  path using the serializer's path convention 
                  (deprecated in favor of @racket['p+]).}

            @item{a pair whose @racket[car] is @racket['p+], whose
                  @racket[cadr] is a byte string, and whose @racket[cddr]
                  is one of the possible symbol results of 
                  @racket[system-path-convention-type]; it represents a 
                  path using the specified convention.}

            @item{a pair whose @racket[car] is @racket['c] and whose
                  @racket[cdr] is a pair of serials; it represents an
                  immutable pair.}

            @item{a pair whose @racket[car] is @racket['c!] and whose
                  @racket[cdr] is a pair of serials; it represents a
                  pair (but formerly represented a mutable pair), and
                  does not appear in output generated by
                  @racket[serialize].}

            @item{a pair whose @racket[car] is @racket['m] and whose
                  @racket[cdr] is a pair of serials; it represents a
                  mutable pair.}

            @item{a pair whose @racket[car] is @racket['v] and whose
                  @racket[cdr] is a list of serials; it represents an
                  immutable vector.}

            @item{a pair whose @racket[car] is @racket['v!] and whose
                  @racket[cdr] is a list of serials; it represents a
                  mutable vector.}

            @item{a pair whose @racket[car] is @racket['vl] and whose
                  @racket[cdr] is a list of serials; it represents a
                  @tech{flvector}.}

            @item{a pair whose @racket[car] is @racket['vx] and whose
                  @racket[cdr] is a list of serials; it represents a
                  @tech{fxvector}.}

            @item{a pair whose @racket[car] is @racket['b] and whose
                  @racket[cdr] is a serial; it represents an immutable
                  box.}

            @item{a pair whose @racket[car] is @racket['b!] and whose
                  @racket[cdr] is a serial; it represents a mutable
                  box.}

            @item{a pair whose @racket[car] is @racket['h], whose
                  @racket[cadr] is either @racket['!] or @racket['-]
                  (mutable or immutable, respectively), whose
                  @racket[caddr] is a list of symbols (containing
                  @racket['equal], @racket['weak], both, or neither)
                  that determines the hash table type, and whose
                  @racket[cdddr] is a list of pairs, where the
                  @racket[car] of each pair is a serial for a
                  hash-table key and the @racket[cdr] is a serial for
                  the corresponding value.}

            @item{a pair whose @racket[car] is @racket['date*] and whose
                  @racket[cdr] is a list of serials; it represents a
                  @racket[date*] structure.}

            @item{a pair whose @racket[car] is @racket['date] and whose
                  @racket[cdr] is a list of serials; it represents a
                  @racket[date] structure.}

            @item{a pair whose @racket[car] is @racket['arity-at-least]
                  and whose @racket[cdr] is a serial; it represents an
                  @racket[arity-at-least] structure.}

            @item{a pair whose @racket[car] is @racket['mpi] and whose
                  @racket[cdr] is a pair; it represents a
                  @tech{module path index} that joins the paired
                  values.}

            @item{a pair whose @racket[car] is @racket['srcloc] and whose
                  @racket[cdr] is a list of serials; it represents a
		  @racket[srcloc] structure.}
            ]}

       ]}

 @item{A list of pairs, where the @racket[car] of each pair is a
       non-negative exact integer @racket[_i] and the @racket[cdr] is a
       serial (as defined in the previous bullet). Each element
       represents an update to an @racket[_i]th element of @racket[_graph]
       that was specified as a box, and the serial describes how to
       construct a new value with the same shape as specified by the
       box. The content of this new value must be transferred into the
       value created for the box in @racket[_graph].}

 @item{A final serial (as defined in the two bullets back)
       representing the result of @racket[deserialize].}
       
]

The result of @racket[deserialize] shares no mutable values with the
argument to @racket[deserialize].

If a value provided to @racket[serialize] is a simple tree (i.e., no
sharing), then the fourth and fifth elements in the serialized
representation will be empty.}

@; ----------------------------------------------------------------------

@defproc[(serialized=? [v1 any/c] [v2 any/c]) boolean?]{

Returns @racket[#t] if @racket[v1] and @racket[v2] represent the same
serialization information.

More precisely, it returns the same value that @racket[(equal?
(deserialize v1) (deserialize v2))] would return if

@itemize[

 @item{all structure types whose deserializers are accessed with
       distinct module paths are actually distinct types;}

 @item{all structure types are transparent; and}

 @item{all structure instances contain only the constituent values
       recorded in each of @racket[v1] and @racket[v2].}

]}

@; ----------------------------------------------------------------------

@defparam[deserialize-module-guard guard (module-path? symbol? . -> . void?)]{

A parameter whose value is called by @racket[deserialize] before
dynamically loading a module via @racket[dynamic-require]. The two
arguments provided to the procedure are the same as the arguments to
be passed to @racket[dynamic-require]. The procedure can raise an
exception to disallow the @racket[dynamic-require].}

@; ----------------------------------------------------------------------

@defform[(serializable-struct id maybe-super (field ...)
                              struct-option ...)]{

Like @racket[struct], but instances of the structure type are
serializable with @racket[serialize].  This form is allowed only at
the top level or in a module's top level (so that deserialization
information can be found later).

Serialization only supports cycles involving the created structure
type when all fields are mutable (or when the cycle can be broken
through some other mutable value).

In addition to the bindings generated by @racket[struct],
@racket[serializable-struct] binds
@racketidfont{deserialize-info:}@racket[_id]@racketidfont{-v0} to
deserialization information. Furthermore, in a module context, it
automatically @racket[provide]s this binding in a @racket[deserialize-info]
submodule using @racket[module+].

The @racket[serializable-struct] form enables the construction of
structure instances from places where @racket[id] is not accessible,
since deserialization must construct instances. Furthermore,
@racket[serializable-struct] provides limited access to field
mutation, but only for instances generated through the deserialization
information bound to
@racketidfont{deserialize-info:}@racket[_id]@racketidfont{-v0}. See
@racket[make-deserialize-info] for more information.

The @racket[-v0] suffix on the deserialization enables future
versioning on the structure type through
@racket[serializable-struct/version].

When a supertype is supplied as @racket[maybe-super],
compile-time information bound to the supertype identifier must
include all of the supertype's field accessors. If any field mutator
is missing, the structure type will be treated as immutable for the
purposes of marshaling (so cycles involving only instances of the
structure type cannot be handled by the deserializer).

@examples[
#:eval ser-eval
(serializable-struct point (x y))
(point-x (deserialize (serialize (point 1 2))))
]}

@; ----------------------------------------------------------------------

@defform[(define-serializable-struct id-maybe-super (field ...)
                                      struct-option ...)]{

Like @racket[serializable-struct], but with the supertype syntax and
default constructor name of @racket[define-struct].}

@; ----------------------------------------------------------------------

@defform/subs[(serializable-struct/versions id maybe-super vers (field ...)
                                            (other-version-clause ...)
                                            struct-option ...)
              ([other-version-clause (other-vers make-proc-expr 
                                                 cycle-make-proc-expr)])]{

Like @racket[serializable-struct], but the generated deserializer
binding is
@racketidfont{deserialize-info:}@racket[_id]@racketidfont{-v}@racket[vers]. In
addition,
@racketidfont{deserialize-info:}@racket[_id]@racketidfont{-v}@racket[other-vers]
is bound for each @racket[other-vers]. The @racket[vers] and each
@racket[other-vers] must be a literal, exact, nonnegative integer.

Each @racket[make-proc-expr] should produce a procedure, and the
procedure should accept as many argument as fields in the
corresponding version of the structure type, and it produce an
instance of @racket[id]. Each @racket[cycle-make-proc-expr] should
produce a procedure of no arguments; this procedure should return two
values: an instance @racket[x] of @racket[id] (typically with
@racket[#f] for all fields) and a procedure that accepts another
instance of @racket[id] and copies its field values into @racket[x].

@examples[
#:eval ser-eval
(serializable-struct point (x y) #:mutable #:transparent)
(define ps (serialize (point 1 2)))
(deserialize ps)

(define x (point 1 10))
(set-point-x! x x)
(define xs (serialize x))
(deserialize xs)

(serializable-struct/versions point 1 (x y z)
   ([0 
     (code:comment @#,t{Constructor for simple v0 instances:})
     (lambda (x y) (point x y 0))
     (code:comment @#,t{Constructor for v0 instance in a cycle:})
     (lambda ()
       (let ([p0 (point #f #f 0)])
         (values
           p0
           (lambda (p)
             (set-point-x! p0 (point-x p))
             (set-point-y! p0 (point-y p))))))])
   #:mutable #:transparent)
(deserialize (serialize (point 4 5 6)))
(deserialize ps)
(deserialize xs)
]}

@; ----------------------------------------------------------------------

@defform[(define-serializable-struct/versions id-maybe-super vers (field ...)
                                              (other-version-clause ...)
                                              struct-option ...)]{
Like @racket[serializable-struct/versions], but with the supertype syntax and
default constructor name of @racket[define-struct].
}

@; ----------------------------------------------------------------------

@defproc[(make-deserialize-info [make procedure?]
                                [cycle-make (-> (values any/c procedure?))])
         any]{

Produces a deserialization information record to be used by
@racket[deserialize]. This information is normally tied to a
particular structure because the structure has a
@racket[prop:serializable] property value that points to a top-level
variable or module-exported variable that is bound to deserialization
information.

The @racket[make] procedure should accept as many arguments as the
structure's serializer put into a vector; normally, this is the number
of fields in the structure. It should return an instance of the
structure.

The @racket[cycle-make] procedure should accept no arguments, and it
should return two values: a structure instance @racket[x] (with dummy
field values) and an update procedure. The update procedure takes
another structure instance generated by the @racket[make], and it
transfers the field values of this instance into @racket[x].}

@; ----------------------------------------------------------------------

@defthing[prop:serializable property?]{

This property identifies structures and structure types that are
serializable. The property value should be constructed with
@racket[make-serialize-info].}

@; ----------------------------------------------------------------------

@defproc[(make-serialize-info [to-vector (any/c . -> . vector?)]
                              [deserialize-id (or identifier?
                                                  symbol?
                                                  (cons/c symbol?
                                                          module-path-index?))]
                              [can-cycle? any/c]
                              [dir path-string?])
         any]{

Produces a value to be associated with a structure type through the
@racket[prop:serializable] property. This value is used by
@racket[serialize].

The @racket[to-vector] procedure should accept a structure instance
and produce a vector for the instance's content.

The @racket[deserialize-id] value indicates a binding for deserialize
information, to either a module export or a top-level definition. It
must be one of the following:

@itemize[

 @item{If @racket[deserialize-id] is an identifier, and if
 @racket[(identifier-binding deserialize-id)] produces a list, then
 the third element is used for the exporting module, otherwise the
 top-level is assumed. Before trying an exporting module directly,
 its @racket[deserialize-info] submodule is tried; the module
 itself is tried if no @racket[deserialize-info]
 submodule is available or if the export is not found. In either case, @racket[syntax-e] is used to
 obtain the name of an exported identifier or top-level definition.}

 @item{If @racket[deserialize-id] is a symbol, it indicates a
 top-level variable that is named by the symbol.}

 @item{If @racket[deserialize-id] is a pair, the @racket[car] must be
 a symbol to name an exported identifier, and the @racket[cdr] must be
 a module path index to specify the exporting module.}

]

See @racket[make-deserialize-info] and @racket[deserialize] for more
information.

The @racket[can-cycle?] argument should be false if instances should
not be serialized in such a way that deserialization requires creating
a structure instance with dummy field values and then updating the
instance later.

The @racket[dir] argument should be a directory path that is used to
resolve a module reference for the binding of @racket[deserialize-id].
This directory path is used as a last resort when
@racket[deserialize-id] indicates a module that was loaded through a
relative path with respect to the top level. Usually, it should be
@racket[(or (current-load-relative-directory) (current-directory))].}

@; ----------------------------------------------------------------------

@close-eval[ser-eval]
