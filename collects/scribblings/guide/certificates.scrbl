#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "stx-certs" #:style 'quiet]{Syntax Certificates}

A use of a macro can expand into a use of an identifier that is not
exported from the module that binds the macro. In general, such an
identifier must not be extracted from the expanded expression and used
in a different context, because using the identifier in a different
context may break invariants of the macro's module.

For example, the following module exports a macro @scheme[go] that
expands to a use of @scheme[unchecked-go]:

@schemeblock[
(module m mzscheme
  (provide go)
  (define (unchecked-go n x) 
    (code:comment @#,t{to avoid disaster, @scheme[n] must be a number})
    (+ n 17))
  (define-syntax (go stx)
    (syntax-case stx ()
     [(_ x)
      #'(unchecked-go 8 x)])))
]

If the reference to @scheme[unchecked-go] is extracted from the
expansion of @scheme[(go 'a)], then it might be inserted into a new
expression, @scheme[(unchecked-go #f 'a)], leading to disaster. The
@scheme[datum->syntax] procedure can be used similarly to construct
references to an unexported identifier, even when no macro expansion
includes a reference to the identifier.

To prevent such abuses of unexported identifiers, the expander rejects
references to unexported identifiers unless they appear in
@defterm{certified} syntax objects. The macro expander always
certifies a syntax object that is produced by a transformer. For
example, when @scheme[(go 'a)] is expanded to @scheme[(unchecked-go 8
'a)], a certificate is attached to the result @scheme[(unchecked-go 8
'a)]. Extracting just @scheme[unchecked-go] removes the identifier
from the certified expression, so that the reference is disallowed
when it is inserted into @scheme[(unchecked-go #f 'a)]. The
@scheme[expand] and @scheme[local-expand] (when used with an empty
stop list) functions lift all certificates to the outermost result
expression, except as indicated by @scheme['certify-mode] syntax
properties (see @refsecref["stxcerts"]).

In addition to checking module references, the macro expander
disallows references to local bindings where the binding identifier is
less certified than the reference. Otherwise, the expansion of
@scheme[(go 'a)] could be wrapped with a local binding that redirects
@scheme[#%app] to @scheme[values], thus obtaining the value of
@scheme[unchecked-go]. Note that a capturing @scheme[#%app] would have
to be extracted from the expansion of @scheme[(go 'a)], since lexical
scope would prevent an arbitrary @scheme[#%app] from capturing.  The
act of extracting @scheme[#%app] removes its certification, whereas
the @scheme[#%app] within the expansion is still certified; comparing
these certifications, the macro expander rejects the local-binding
reference, and @scheme[unchecked-go] remains protected.

In much the same way that the macro expander copies properties from a
syntax transformer's input to its output (see @refsecref["stxprops"]),
the expander copies certificates from a transformer's input to its
output. Building on the previous example,

@schemeblock[
(module n mzscheme
  (require m)
  (provide go-more)
  (define y 'hello)
  (define-syntax (go-more stx)
    #'(go y)))
]

the expansion of @scheme[(go-more)] introduces a reference to the
unexported @scheme[y] in @scheme[(go y)], and a certificate allows the
reference to @scheme[y]. As @scheme[(go y)] is expanded to
@scheme[(unchecked-go 8 y)], the certificate that allows @scheme[y] is
copied over, in addition to the certificate that allows the reference
to @scheme[unchecked-go].

When a protected identifier becomes inaccessible by direct reference
(i.e., when the current code inspector is changed so that it does not
control the module's invocation; see @refsecref["modprotect"]), the
protected identifier is treated like an unexported identifier.

@;------------------------------------------------------------------------
@section[#:tag "stxinactivecerts"]{Certificate Propagation}

When the result of a macro expansion contains a @scheme[quote-syntax]
form, the macro expansion's certificate must be attached to the
resulting syntax object to support macro-generating macros. In
general, when the macro expander encounters @scheme[quote-syntax], it
attaches all certificates from enclosing expressions to the quoted
syntax constant. However, the certificates are attached to the syntax
constant as @defterm{inactive} certificates, and inactive certificates
do not count directly for certifying identifier access. Inactive
certificates become active when the macro expander certifies the
result of a macro expansion; at that time, the expander removes all
inactive certificates within the expansion result and attaches active
versions of the certificates to the overall expansion result.

For example, suppose that the @scheme[go] macro is implemented through
a macro:

@schemeblock[
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

When @scheme[def-go] is used inside another module, the generated
macro should legally generate expressions that use
@scheme[unchecked-go], since @scheme[def-go] in @scheme[m] had
complete control over the generated macro.

@schemeblock[
(module n mzscheme
  (require m)
  (def-go go)
  (go 10)) ; access to @scheme[unchecked-go] is allowed
]

This example works because the expansion of @scheme[(def-go go)] is
certified to access protected identifiers in @scheme[m], including
@scheme[unchecked-go]. Specifically, the certified expansion is a
definition of the macro @scheme[go], which includes a syntax-object
constant @scheme[unchecked-go]. Since the enclosing macro declaration
is certified, the @scheme[unchecked-go] syntax constant gets an
inactive certificate to access protected identifiers of
@scheme[m]. When @scheme[(go 10)] is expanded, the inactive
certificate on @scheme[unchecked-go] is activated for the macro result
@scheme[(unchecked-go 8 10)], and the access of @scheme[unchecked-go]
is allowed.

To see why @scheme[unchecked-go] as a syntax constant must be given an
inactive certificate instead of an active one, it's helpful to write
the @scheme[def-go] macro as follows:

@schemeblock[
(define-syntax (def-go stx)
 (syntax-case stx ()
   [(_ go)
    #'(define-syntax (go stx)
        (syntax-case stx ()
         [(_ x)
          (with-syntax ([ug (quote-syntax unchecked-go)])
            #'(ug 8 x))]))]))
]

In this case, @scheme[unchecked-go] is clearly quoted as an immediate
syntax object in the expansion of @scheme[(def-go go)]. If this syntax
object were given an active certificate, then it would keep the
certificate---directly on the identifier @scheme[unchecked-go]---in
the result @scheme[(unchecked-go 8 10)]. Consequently, the
@scheme[unchecked-go] identifier could be extracted and used with its
certificate intact. Attaching an inactive certificate to
@scheme[unchecked-go] and activating it only for the complete result
@scheme[(unchecked-go 8 10)] ensures that @scheme[unchecked-go] is
used only in the way intended by the implementor of @scheme[def-go].

The @scheme[datum->syntax] procedure allows inactive certificates to
be transferred from one syntax object to another. Such transfers are
allowed because a macro transformer with access to the syntax object
could already wrap it with an arbitrary context before activating the
certificates. In practice, transferring inactive certificates is
useful mainly to macros that implement new template forms, such as
@scheme[syntax/loc].

@;------------------------------------------------------------------------
@section{Internal Certificates}

In some cases, a macro implementor intends to allow limited
destructuring of a macro result without losing the result's
certificate. For example, given the following @scheme[define-like-y]
macro,

@schemeblock[
(module q mzscheme
  (provide define-like-y)
  (define y 'hello)
  (define-syntax (define-like-y stx)
    (syntax-case stx ()
      [(_ id) #'(define-values (id) y)])))
]

someone may use the macro in an internal definition:

@schemeblock[
(let ()
  (define-like-y x)
  x)
]

The implementor of the @scheme[q] module most likely intended to allow
such uses of @scheme[define-like-y]. To convert an internal definition
into a @scheme[letrec] binding, however, the @scheme[define] form
produced by @scheme[define-like-y] must be deconstructed, which would
normally lose the certificate that allows the reference to @scheme[y].

The internal use of @scheme[define-like-y] is allowed because the
macro expander treats specially a transformer result that is a syntax
list beginning with @scheme[define-values]. In that case, instead of
attaching the certificate to the overall expression, the certificate
is instead attached to each individual element of the syntax list,
pushing the certificates into the second element of the list so that
they are attached to the defined identifiers. Thus, a certificate is
attached to @scheme[define-values], @scheme[x], and @scheme[y] in the
expansion result @scheme[(define-values (x) y)], and the definition
can be deconstructed for conversion to @scheme[letrec].

Just like the new certificate that is added to a transformer result,
old certificates from the input are similarly moved to syntax-list
elements when the result starts with @scheme[define-values]. Thus,
@scheme[define-like-y] could have been implemented to produce
@scheme[(define id y)], using @scheme[define] instead of
@scheme[define-values]. In that case, the certificate to allow
reference to @scheme[y] would be attached initially to the expansion
result @scheme[(define x y)], but as the @scheme[define] is expanded
to @scheme[define-values], the certificate would be moved to the
parts.

The macro expander treats syntax-list results starting with
@scheme[define-syntaxes] in the same way that it treats results
starting with @scheme[define-values]. Syntax-list results starting
with @scheme[begin] are treated similarly, except that the second
element of the syntax list is treated like all the other elements
(i.e., the certificate is attached to the element instead of its
content). Furthermore, the macro expander applies this special
handling recursively, in case a macro produces a @scheme[begin] form
that contains nested @scheme[define-values] forms.

The default application of certificates can be overridden by attaching
a @scheme['certify-mode] property (see @refsecref["stxprops"]) to the
result syntax object of a macro transformer. If the property value is
@scheme['opaque], then the certificate is attached to the syntax
object and not its parts. If the property value is
@scheme['transparent], then the certificate is attached to the syntax
object's parts. If the property value is
@scheme['transparent-binding], then the certificate is attached to the
syntax object's parts and to the sub-parts of the second part (as for
@scheme[define-values] and @scheme[define-syntaxes]). The
@scheme['transparent] and @scheme['transparent-binding] modes triggers
recursive property checking at the parts, so that the certificate can
be pushed arbitrarily deep into a transformer's result.
