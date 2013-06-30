#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/class100
                     mzlib/class
                     scheme/contract))

@mzlib[#:mode title class100]

@deprecated[@racketmodname[racket/class]]{This library will be
removed in a future version.}

The @racket[class100] and @racket[class100*] forms provide a syntax
close to that of @racket[class] and @racket[class*] in Racket
versions 100 through 103, but with the semantics of the current
@racketmodname[scheme/class]-based class system. For a class defined
with @racket[class100], keyword-based initialization arguments can be
propagated to the superclass, but by-position arguments are not (i.e.,
the expansion of @racket[class100] to @racket[class] always includes
an @racket[init-rest] clause).

The @racket[class100] form uses keywords (e.g., @racket[public]) that
are defined by the @racketmodname[mzlib/class] library, so typically
@racketmodname[scheme/class] must be imported into any context that
imports @racketmodname[mzlib/class100].


@defform/subs[
#:literals (sequence public override augment pubment
            overment augride private private-field inherit
            rename)
(class100* superclass-expr (interface-expr ...) init-ids
  class100-clause
  ...)
([init-ids id
           (id ... id-with-default ...) 
           (id ... id-with-default ... . id) ]
 [id-with-default (id default-expr) ]
 [class100-clause (sequence expr ...) 
                  (public public-method-decl ...) 
                  (override public-method-decl ...) 
                  (augment public-method-decl ...) 
                  (pubment public-method-decl ...) 
                  (overment public-method-decl ...) 
                  (augride public-method-decl ...) 
                  (private private-method-decl ...) 
                  (private-field private-var-decl ...) 
                  (inherit inherit-method-decl ...) 
                  (rename rename-method-decl ...) ]
 [public-method-decl ((internal-id external-id) method-procedure)
                     (id method-procedure)]
 [private-method-decl (id method-procedure)]
 [private-var-decl (id initial-value-expr)
                   (id)
                   id]
 [inherit-method-decl id
                      (internal-instance-id external-inherited-id)]
 [rename-method-decl (internal-id external-id)])]


@defform[
(class100 superclass-expr init-ids
    class100-clause
    ...)
]{

Like @racket[class100*], but without @racket[interface-expr]s.}


@defform[(class100-asi superclass instance-id-clause ...)]{

Like @racket[class100], but all initialization arguments are
automatically passed on to the superclass initialization procedure by
position.}


@defform[(class100*-asi superclass interfaces instance-id-clause ...)]{

Like @racket[class100*], but all initialization arguments are
automatically passed on to the superclass initialization procedure by
position.}


@defform[(super-init init-arg-expr ...)]{

An alias for @racket[super-make-object].}

