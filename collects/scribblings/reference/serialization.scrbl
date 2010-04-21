#lang scribble/doc
@(require "mz.ss"
          scheme/serialize
          (for-label scheme/serialize))

@(define ser-eval (make-base-eval))
@(interaction-eval #:eval ser-eval (require scheme/serialize))

@title[#:tag "serialization"]{Serialization}

@note-lib-only[scheme/serialize #:use-sources (racket/private/serialize)]

@defproc[(serializable? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] appears to be serializable, without
checking the content of compound values, and @scheme[#f] otherwise.
See @scheme[serialize] for an enumeration of serializable values.}

@; ----------------------------------------------------------------------

@defproc[(serialize [v serializable?]) any]{

Returns a value that encapsulates the value @scheme[v]. This value
includes only readable values, so it can be written to a stream with
@scheme[write], later read from a stream using @scheme[read], and then
converted to a value like the original using
@scheme[deserialize]. Serialization followed by deserialization
produces a value with the same graph structure and mutability as the
original value, but the serialized value is a plain tree (i.e., no
sharing).

The following kinds of values are serializable:

@itemize[

 @item{structures created through @scheme[define-serializable-struct] or
       @scheme[define-serializable-struct/version], or more generally
       structures with the @scheme[prop:serializable] property (see
       @scheme[prop:serializable] for more information);}

 @item{structures that instantiate @techlink{prefab} structure types;}

 @item{instances of classes defined with @scheme[define-serializable-class]
       or @scheme[define-serializable-class];}

 @item{booleans, numbers, characters, @tech{interned} symbols,
       @tech{unreadable symbols}, strings, byte strings, paths (for a
       specific convention), @|void-const|, and the empty list;}

 @item{pairs, mutable pairs, vectors, boxes, and hash tables;}

 @item{@scheme[date] and @scheme[arity-at-least] structures; and}
 
 @item{@tech{module path index} values.}

]

Serialization succeeds for a compound value, such as a pair, only if
all content of the value is serializable.  If a value given to
@scheme[serialize] is not completely serializable, the
@exnraise[exn:fail:contract].

See @scheme[deserialize] for information on the format of serialized
data.}

@; ----------------------------------------------------------------------

@defproc[(deserialize [v any/c]) any]{

Given a value @scheme[v] that was produced by @scheme[serialize],
produces a value like the one given to @scheme[serialize], including
the same graph structure and mutability.

A serialized representation @scheme[v] is a list of six or seven
elements:

@itemize[

 @item{An optional list @scheme['(1)] or @scheme['(2)] that represents
       the version of the serialization format. If the first element
       of a representation is not a list, then the version is
       @scheme[0]. Version 1 adds support for mutable pairs, and
       version 2 adds support for @tech{unreadable symbols}.}

 @item{A non-negative exact integer @scheme[_s-count] that represents the
       number of distinct structure types represented in the
       serialized data.}

 @item{A list @scheme[_s-types] of length @scheme[_s-count], where
       each element represents a structure type. Each structure type
       is encoded as a pair. The @scheme[car] of the pair is
       @scheme[#f] for a structure whose deserialization information
       is defined at the top level, otherwise it is a quoted
       @tech{module path} or a byte string (to be converted into a
       platform-specific path using @scheme[bytes->path]) for a module
       that exports the structure's deserialization information.  The
       @scheme[cdr] of the pair is the name of a binding (at the top
       level or exported from a module) for deserialization
       information, either a symbol or a string representing an
       @tech{unreadable symbol}. These two are used with either
       @scheme[namespace-variable-binding] or @scheme[dynamic-require]
       to obtain deserialization information. See
       @scheme[make-deserialization-info] for more information on the
       binding's value. See also @scheme[deserialize-module-guard].}

 @item{A non-negative exact integer, @scheme[_g-count] that represents the
       number of graph points contained in the following list.}

 @item{A list @scheme[_graph] of length @scheme[_g-count], where each element
       represents a serialized value to be referenced during the
       construction of other serialized values. Each list element is
       either a box or not:

      @itemize[

       @item{A box represents a value that is part of a cycle, and for
            deserialization, it must be allocated with @scheme[#f] for
            each of its fields. The content of the box indicates the
            shape of the value:

            @itemize[

            @item{a non-negative exact integer @scheme[_i] for an instance
                  of a structure type that is represented by the
                  @scheme[_i]th element of the @scheme[_s-types] list;}

            @item{@scheme['c] for a pair, which fails on
                  deserialization (since pairs are immutable; this
                  case does not appear in output generated by
                  @scheme[serialize]);}
 
            @item{@scheme['m] for a mutable pair;}
 
            @item{@scheme['b] for a box;}

            @item{a pair whose @scheme[car] is @scheme['v] and whose
                  @scheme[cdr] is a non-negative exact integer @scheme[_s]
                  for a vector of length @scheme[_s]; or}

            @item{a list whose first element is @scheme['h] and whose
                  remaining elements are symbols that determine the
                  hash-table type:

                  @itemize[
                    @item{@scheme['equal] --- @scheme[(make-hash)]}
                    @item{@scheme['equal 'weak] --- @scheme[(make-weak-hash)]}
                    @item{@scheme['weak] --- @scheme[(make-weak-hasheq)]}
                    @item{no symbols --- @scheme[(make-hasheq)]}
                  ]}

            @item{@scheme['date] for a @scheme[date] structure, which
                  fails on deserialization (since dates are immutable;
                  this case does not appear in output generated by
                  @scheme[serialize]);}

            @item{@scheme['arity-at-least] for an
                  @scheme[arity-at-least] structure, which fails on
                  deserialization (since dates are immutable; this
                  case does not appear in output generated by
                  @scheme[serialize]); or}

            @item{@scheme['mpi] for a @tech{module path index}, which
                  fails on deserialization (since dates are immutable;
                  this case does not appear in output generated by
                  @scheme[serialize]).}

            ]

            The @scheme[#f]-filled value will be updated with content specified
            by the fifth element of the serialization list @scheme[v].}

       @item{A non-box represents a @defterm{serial} value to be
             constructed immediately, and it is one of the following:

            @itemize[

            @item{a boolean, number, character, interned symbol, or empty list,
                  representing itself.}

            @item{a string, representing an immutable string.}

            @item{a byte string, representing an immutable byte
                  string.}

            @item{a pair whose @scheme[car] is @scheme['?] and whose
                  @scheme[cdr] is a non-negative exact integer
                  @scheme[_i]; it represents the value constructed for the
                  @scheme[_i]th element of @scheme[_graph], where @scheme[_i] is
                  less than the position of this element within
                  @scheme[_graph].}

            @item{a pair whose @scheme[car] is a number @scheme[_i]; it
                  represents an instance of a structure type that is
                  described by the @scheme[_i]th element of the
                  @scheme[_s-types] list. The @scheme[cdr] of the pair is
                  a list of serials representing arguments to be
                  provided to the structure type's deserializer.}

            @item{a pair whose @scheme[car] is @scheme['f]; it
                  represents an instance of a @tech{prefab} structure
                  type. The @scheme[cadr] of the pair is @tech{prefab}
                  structure type key, and the @scheme[cddr] is a list of
                  serials representing the field values.}

            @item{a pair whose @scheme[car] is @scheme['void],
                  representing @|void-const|.}

            @item{a pair whose @scheme[car] is @scheme['su] and whose
                  @scheme[cdr] is a character string; it represents a
                  @tech{unreadable symbol}.}

            @item{a pair whose @scheme[car] is @scheme['u] and whose
                  @scheme[cdr] is either a byte string or character
                  string; it represents a mutable byte or character
                  string.}

            @item{a pair whose @scheme[car] is @scheme['p] and whose
                  @scheme[cdr] is a byte string; it represents a 
                  path using the serializer's path convention 
                  (deprecated in favor of @scheme['p+]).}

            @item{a pair whose @scheme[car] is @scheme['p+], whose
                  @scheme[cadr] is a byte string, and whose @scheme[cddr]
                  is one of the possible symbol results of 
                  @scheme[system-path-convention-type]; it represents a 
                  path using the specified convention.}

            @item{a pair whose @scheme[car] is @scheme['c] and whose
                  @scheme[cdr] is a pair of serials; it represents an
                  immutable pair.}

            @item{a pair whose @scheme[car] is @scheme['c!] and whose
                  @scheme[cdr] is a pair of serials; it represents a
                  pair (but formerly presented a mutable pair), and
                  does not appear in output generated by
                  @scheme[serialize].}

            @item{a pair whose @scheme[car] is @scheme['m] and whose
                  @scheme[cdr] is a pair of serials; it represents a
                  mutable pair.}

            @item{a pair whose @scheme[car] is @scheme['v] and whose
                  @scheme[cdr] is a list of serials; it represents an
                  immutable vector.}

            @item{a pair whose @scheme[car] is @scheme['v!] and whose
                  @scheme[cdr] is a list of serials; it represents a
                  mutable vector.}

            @item{a pair whose @scheme[car] is @scheme['b] and whose
                  @scheme[cdr] is a serial; it represents an immutable
                  box.}

            @item{a pair whose @scheme[car] is @scheme['b!] and whose
                  @scheme[cdr] is a serial; it represents a mutable
                  box.}

            @item{a pair whose @scheme[car] is @scheme['h], whose
                  @scheme[cadr] is either @scheme['!] or @scheme['-]
                  (mutable or immutable, respectively), whose
                  @scheme[caddr] is a list of symbols (containing
                  @scheme['equal], @scheme['weak], both, or neither)
                  that determines the hash table type, and whose
                  @scheme[cdddr] is a list of pairs, where the
                  @scheme[car] of each pair is a serial for a
                  hash-table key and the @scheme[cdr] is a serial for
                  the corresponding value.}

            @item{a pair whose @scheme[car] is @scheme['date] and whose
                  @scheme[cdr] is a list of serials; it represents a
                  @scheme[date] structure.}

            @item{a pair whose @scheme[car] is @scheme['arity-at-least]
                  and whose @scheme[cdr] is a serial; it represents an
                  @scheme[arity-at-least] structure.}

            @item{a pair whose @scheme[car] is @scheme['mpi] and whose
                  @scheme[cdr] is a pair; it represents an
                  @tech{module path index} that joins the paired
                  values.}

            ]}

       ]}

 @item{A list of pairs, where the @scheme[car] of each pair is a
       non-negative exact integer @scheme[_i] and the @scheme[cdr] is a
       serial (as defined in the previous bullet). Each element
       represents an update to an @scheme[_i]th element of @scheme[_graph]
       that was specified as a box, and the serial describes how to
       construct a new value with the same shape as specified by the
       box. The content of this new value must be transferred into the
       value created for the box in @scheme[_graph].}

 @item{A final serial (as defined in the two bullets back)
       representing the result of @scheme[deserialize].}
       
]

The result of @scheme[deserialize] shares no mutable values with the
argument to @scheme[deserialize].

If a value provided to @scheme[serialize] is a simple tree (i.e., no
sharing), then the fourth and fifth elements in the serialized
representation will be empty.}

@; ----------------------------------------------------------------------

@defproc[(serialized=? [v1 any/c] [v2 any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v1] and @scheme[v2] represent the same
serialization information.

More precisely, it returns the same value that @scheme[(equal?
(deserialize v1) (deserialize v2))] would return if

@itemize[

 @item{all structure types whose deserializers are accessed with
       distinct module paths are actually distinct types;}

 @item{all structure types are transparent; and}

 @item{all structure instances contain only the constituent values
       recorded in each of @scheme[v1] and @scheme[v2].}

]}

@; ----------------------------------------------------------------------

@defparam[deserialize-module-guard guard (module-path? symbol? . -> . void?)]{

A parameter whose value is called by @scheme[deserialize] before
dynamically loading a module via @scheme[dynamic-require]. The two
arguments provided to the procedure are the same as the arguments to
be passed to @scheme[dynamic-require]. The procedure can raise an
exception to disallow the @scheme[dynamic-require].}

@; ----------------------------------------------------------------------

@defform[(define-serializable-struct id-maybe-super (field ...)
                                      struct-option ...)]{

Like @scheme[define-struct], but instances of the structure type are
serializable with @scheme[serialize].  This form is allowed only at
the top level or in a module's top level (so that deserialization
information can be found later).

Serialization only supports cycles involving the created structure
type when all fields are mutable (or when the cycle can be broken
through some other mutable value).

In addition to the bindings generated by @scheme[define-struct],
@scheme[define-serializable-struct] binds
@schemeidfont{deserialize-info:}@scheme[_id]@schemeidfont{-v0} to
deserialization information. Furthermore, in a module context, it
automatically @scheme[provide]s this binding.

The @scheme[define-serializable-struct] form enables the construction
of structure instances from places where
@schemeidfont{make}@scheme[id] is not accessible, since
deserialization must construct instances. Furthermore,
@scheme[define-serializable-struct] provides limited access to field
mutation, but only for instances generated through the deserialization
information bound to
@schemeidfont{deserialize-info:}@scheme[_id]@schemeidfont{-v0}. See
@scheme[make-deserialize-info] for more information.

The @scheme[-v0] suffix on the deserialization enables future
versioning on the structure type through
@scheme[define-serializable-struct/version].

When a supertype is supplied in @scheme[id-maybe-super] is supplied,
compile-time information bound to the supertype identifier must
include all of the supertype's field accessors. If any field mutator
is missing, the structure type will be treated as immutable for the
purposes of marshaling (so cycles involving only instances of the
structure type cannot be handled by the deserializer).

@examples[
#:eval ser-eval
(define-serializable-struct point (x y))
(point-x (deserialize (serialize (make-point 1 2))))
]}

@; ----------------------------------------------------------------------

@defform/subs[(define-serializable-struct/versions id-maybe-super vers (field ...)
                                                   (other-version-clause ...)
                                                   struct-option ...)
              ([other-version-clause (other-vers make-proc-expr 
                                                 cycle-make-proc-expr)])]{

Like @scheme[define-serializable-struct], but the generated
deserializer binding is
@schemeidfont{deserialize-info:}@scheme[_id]@schemeidfont{-v}@scheme[vers]. In
addition,
@schemeidfont{deserialize-info:}@scheme[_id]@schemeidfont{-v}@scheme[other-vers]
is bound for each @scheme[other-vers]. The @scheme[vers] and each
@scheme[other-vers] must be a literal, exact, nonnegative integer.

Each @scheme[make-proc-expr] should produce a procedure, and the
procedure should accept as many argument as fields in the
corresponding version of the structure type, and it produce an
instance of @scheme[id]. Each @scheme[graph-make-proc-expr] should
produce a procedure of no arguments; this procedure should return two
values: an instance @scheme[x] of @scheme[id] (typically with
@scheme[#f] for all fields) and a procedure that accepts another
instance of @scheme[id] and copies its field values into @scheme[x].

@examples[
#:eval ser-eval
(define-serializable-struct point (x y) #:mutable #:transparent)
(define ps (serialize (make-point 1 2)))
(deserialize ps)

(define x (make-point 1 10))
(set-point-x! x x)
(define xs (serialize x))
(deserialize xs)

(define-serializable-struct/versions point 1 (x y z)
   ([0 
     (code:comment @#,t{Constructor for simple v0 instances:})
     (lambda (x y) (make-point x y 0))
     (code:comment @#,t{Constructor for v0 instance in a cycle:})
     (lambda ()
       (let ([p0 (make-point #f #f 0)])
         (values
           p0
           (lambda (p)
             (set-point-x! p0 (point-x p))
             (set-point-y! p0 (point-y p))))))])
   #:mutable #:transparent)
(deserialize (serialize (make-point 4 5 6)))
(deserialize ps)
(deserialize xs)
]}

@; ----------------------------------------------------------------------

@defproc[(make-deserialize-info [make procedure?]
                                [cycle-make (-> (values any/c procedure?))])
         any]{

Produces a deserialization information record to be used by
@scheme[deserialize]. This information is normally tied to a
particular structure because the structure has a
@scheme[prop:serializable] property value that points to a top-level
variable or module-exported variable that is bound to deserialization
information.

The @scheme[make] procedure should accept as many argument as the
structure's serializer put into a vector; normally, this is the number
of fields in the structure. It should return an instance of the
structure.

The @scheme[cycle-make] procedure should accept no arguments, and it
should return two values: a structure instance @scheme[x] (with dummy
field values) and an update procedure. The update procedure takes
another structure instance generated by the @scheme[make], and it
transfers the field values of this instance into @scheme[x].}

@; ----------------------------------------------------------------------

@defthing[prop:serializable property?]{

This property identifies structures and structure types that are
serializable. The property value should be constructed with
@scheme[make-serialize-info].}

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
@scheme[prop:serializable] property. This value is used by
@scheme[serialize].

The @scheme[to-vector] procedure should accept a structure instance
and produce a vector for the instance's content.

The @scheme[deserialize-id] value indicates a binding for deserialize
information, to either a module export or a top-level definition. It
must be one of the following:

@itemize[

 @item{If @scheme[deserialize-id] is an identifier, and if
 @scheme[(identifier-binding deserialize-id)] produces a list, then
 the third element is used for the exporting module, otherwise the
 top-level is assumed. In either case, @scheme[syntax-e] is used to
 obtain the name of an exported identifier or top-level definition.}

 @item{If @scheme[deserialize-id] is a symbol, it indicates a
 top-level variable that is named by the symbol.}

 @item{If @scheme[deserialize-id] is a pair, the @scheme[car] must be
 a symbol to name an exported identifier, and the @scheme[cdr] must be
 a module path index to specify the exporting module.}

]

See @scheme[make-deserialize-info] and @scheme[deserialize] for more
information.

The @scheme[can-cycle?] argument should be false if instances should
not be serialized in such a way that deserialization requires creating
a structure instance with dummy field values and then updating the
instance later.

The @scheme[dir] argument should be a directory path that is used to
resolve a module reference for the binding of @scheme[deserialize-id].
This directory path is used as a last resort when
@scheme[deserialize-id] indicates a module that was loaded through a
relative path with respect to the top level. Usually, it should be
@scheme[(or (current-load-relative-directory) (current-directory))].}

@; ----------------------------------------------------------------------

@close-eval[ser-eval]
