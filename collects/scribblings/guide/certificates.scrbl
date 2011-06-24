#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "stx-certs" #:style 'quiet]{Syntax Certificates}

A use of a macro can expand into a use of an identifier that is not
exported from the module that binds the macro. In general, such an
identifier must not be extracted from the expanded expression and used
in a different context, because using the identifier in a different
context may break invariants of the macro's module.

For example, the following module exports a macro @racket[go] that
expands to a use of @racket[unchecked-go]:

@racketblock[
(module m mzscheme
  (provide go)
  (define (unchecked-go n x) 
    (code:comment @#,t{to avoid disaster, @racket[n] must be a number})
    (+ n 17))
  (define-syntax (go stx)
    (syntax-case stx ()
     [(_ x)
      #'(unchecked-go 8 x)])))
]

If the reference to @racket[unchecked-go] is extracted from the
expansion of @racket[(go 'a)], then it might be inserted into a new
expression, @racket[(unchecked-go #f 'a)], leading to disaster. The
@racket[datum->syntax] procedure can be used similarly to construct
references to an unexported identifier, even when no macro expansion
includes a reference to the identifier.

To prevent such abuses of unexported identifiers, the expander rejects
references to unexported identifiers unless they appear in
@defterm{certified} syntax objects. The macro expander always
certifies a syntax object that is produced by a transformer. For
example, when @racket[(go 'a)] is expanded to @racket[(unchecked-go 8
'a)], a certificate is attached to the result @racket[(unchecked-go 8
'a)]. Extracting just @racket[unchecked-go] removes the identifier
from the certified expression, so that the reference is disallowed
when it is inserted into @racket[(unchecked-go #f 'a)]. The
@racket[expand] and @racket[local-expand] (when used with an empty
stop list) functions lift all certificates to the outermost result
expression, except as indicated by @racket['certify-mode] syntax
properties (see @refsecref["stxcerts"]).

In addition to checking module references, the macro expander
disallows references to local bindings where the binding identifier is
less certified than the reference. Otherwise, the expansion of
@racket[(go 'a)] could be wrapped with a local binding that redirects
@racket[#%app] to @racket[values], thus obtaining the value of
@racket[unchecked-go]. Note that a capturing @racket[#%app] would have
to be extracted from the expansion of @racket[(go 'a)], since lexical
scope would prevent an arbitrary @racket[#%app] from capturing.  The
act of extracting @racket[#%app] removes its certification, whereas
the @racket[#%app] within the expansion is still certified; comparing
these certifications, the macro expander rejects the local-binding
reference, and @racket[unchecked-go] remains protected.

In much the same way that the macro expander copies properties from a
syntax transformer's input to its output (see @refsecref["stxprops"]),
the expander copies certificates from a transformer's input to its
output. Building on the previous example,

@racketblock[
(module n mzscheme
  (require m)
  (provide go-more)
  (define y 'hello)
  (define-syntax (go-more stx)
    #'(go y)))
]

the expansion of @racket[(go-more)] introduces a reference to the
unexported @racket[y] in @racket[(go y)], and a certificate allows the
reference to @racket[y]. As @racket[(go y)] is expanded to
@racket[(unchecked-go 8 y)], the certificate that allows @racket[y] is
copied over, in addition to the certificate that allows the reference
to @racket[unchecked-go].

When a protected identifier becomes inaccessible by direct reference
(i.e., when the current code inspector is changed so that it does not
control the module's invocation; see @refsecref["modprotect"]), the
protected identifier is treated like an unexported identifier.

@;------------------------------------------------------------------------
@section[#:tag "stxinactivecerts"]{Certificate Propagation}

When the result of a macro expansion contains a @racket[quote-syntax]
form, the macro expansion's certificate must be attached to the
resulting syntax object to support macro-generating macros. In
general, when the macro expander encounters @racket[quote-syntax], it
attaches all certificates from enclosing expressions to the quoted
syntax constant. However, the certificates are attached to the syntax
constant as @defterm{inactive} certificates, and inactive certificates
do not count directly for certifying identifier access. Inactive
certificates become active when the macro expander certifies the
result of a macro expansion; at that time, the expander removes all
inactive certificates within the expansion result and attaches active
versions of the certificates to the overall expansion result.

For example, suppose that the @racket[go] macro is implemented through
a macro:

@racketblock[
(module m mzscheme
  (provide def-go)
  (define (unchecked-go n x) 
    (+ n 17))
  (define-syntax (def-go stx)
   (syntax-case stx ()
     [(_ go)
      #'(define-syntax (go stx)
          (syntax-case stx ()
           [(_ x)
            #'(unchecked-go 8 x)]))])))
]

When @racket[def-go] is used inside another module, the generated
macro should legally generate expressions that use
@racket[unchecked-go], since @racket[def-go] in @racket[m] had
complete control over the generated macro.

@racketblock[
(module n mzscheme
  (require m)
  (def-go go)
  (go 10)) ; access to @racket[unchecked-go] is allowed
]

This example works because the expansion of @racket[(def-go go)] is
certified to access protected identifiers in @racket[m], including
@racket[unchecked-go]. Specifically, the certified expansion is a
definition of the macro @racket[go], which includes a syntax-object
constant @racket[unchecked-go]. Since the enclosing macro declaration
is certified, the @racket[unchecked-go] syntax constant gets an
inactive certificate to access protected identifiers of
@racket[m]. When @racket[(go 10)] is expanded, the inactive
certificate on @racket[unchecked-go] is activated for the macro result
@racket[(unchecked-go 8 10)], and the access of @racket[unchecked-go]
is allowed.

To see why @racket[unchecked-go] as a syntax constant must be given an
inactive certificate instead of an active one, it's helpful to write
the @racket[def-go] macro as follows:

@racketblock[
(define-syntax (def-go stx)
 (syntax-case stx ()
   [(_ go)
    #'(define-syntax (go stx)
        (syntax-case stx ()
         [(_ x)
          (with-syntax ([ug (quote-syntax unchecked-go)])
            #'(ug 8 x))]))]))
]

In this case, @racket[unchecked-go] is clearly quoted as an immediate
syntax object in the expansion of @racket[(def-go go)]. If this syntax
object were given an active certificate, then it would keep the
certificate---directly on the identifier @racket[unchecked-go]---in
the result @racket[(unchecked-go 8 10)]. Consequently, the
@racket[unchecked-go] identifier could be extracted and used with its
certificate intact. Attaching an inactive certificate to
@racket[unchecked-go] and activating it only for the complete result
@racket[(unchecked-go 8 10)] ensures that @racket[unchecked-go] is
used only in the way intended by the implementor of @racket[def-go].

The @racket[datum->syntax] procedure allows inactive certificates to
be transferred from one syntax object to another. Such transfers are
allowed because a macro transformer with access to the syntax object
could already wrap it with an arbitrary context before activating the
certificates. In practice, transferring inactive certificates is
useful mainly to macros that implement new template forms, such as
@racket[syntax/loc].

@;------------------------------------------------------------------------
@section{Internal Certificates}

In some cases, a macro implementor intends to allow limited
destructuring of a macro result without losing the result's
certificate. For example, given the following @racket[define-like-y]
macro,

@racketblock[
(module q mzscheme
  (provide define-like-y)
  (define y 'hello)
  (define-syntax (define-like-y stx)
    (syntax-case stx ()
      [(_ id) #'(define-values (id) y)])))
]

someone may use the macro in an internal definition:

@racketblock[
(let ()
  (define-like-y x)
  x)
]

The implementor of the @racket[q] module most likely intended to allow
such uses of @racket[define-like-y]. To convert an internal definition
into a @racket[letrec] binding, however, the @racket[define] form
produced by @racket[define-like-y] must be deconstructed, which would
normally lose the certificate that allows the reference to @racket[y].

The internal use of @racket[define-like-y] is allowed because the
macro expander treats specially a transformer result that is a syntax
list beginning with @racket[define-values]. In that case, instead of
attaching the certificate to the overall expression, the certificate
is instead attached to each individual element of the syntax list,
pushing the certificates into the second element of the list so that
they are attached to the defined identifiers. Thus, a certificate is
attached to @racket[define-values], @racket[x], and @racket[y] in the
expansion result @racket[(define-values (x) y)], and the definition
can be deconstructed for conversion to @racket[letrec].

Just like the new certificate that is added to a transformer result,
old certificates from the input are similarly moved to syntax-list
elements when the result starts with @racket[define-values]. Thus,
@racket[define-like-y] could have been implemented to produce
@racket[(define id y)], using @racket[define] instead of
@racket[define-values]. In that case, the certificate to allow
reference to @racket[y] would be attached initially to the expansion
result @racket[(define x y)], but as the @racket[define] is expanded
to @racket[define-values], the certificate would be moved to the
parts.

The macro expander treats syntax-list results starting with
@racket[define-syntaxes] in the same way that it treats results
starting with @racket[define-values]. Syntax-list results starting
with @racket[begin] are treated similarly, except that the second
element of the syntax list is treated like all the other elements
(i.e., the certificate is attached to the element instead of its
content). Furthermore, the macro expander applies this special
handling recursively, in case a macro produces a @racket[begin] form
that contains nested @racket[define-values] forms.

The default application of certificates can be overridden by attaching
a @racket['certify-mode] property (see @refsecref["stxprops"]) to the
result syntax object of a macro transformer. If the property value is
@racket['opaque], then the certificate is attached to the syntax
object and not its parts. If the property value is
@racket['transparent], then the certificate is attached to the syntax
object's parts. If the property value is
@racket['transparent-binding], then the certificate is attached to the
syntax object's parts and to the sub-parts of the second part (as for
@racket[define-values] and @racket[define-syntaxes]). The
@racket['transparent] and @racket['transparent-binding] modes triggers
recursive property checking at the parts, so that the certificate can
be pushed arbitrarily deep into a transformer's result.
