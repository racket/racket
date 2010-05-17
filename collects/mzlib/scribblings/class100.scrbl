#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/class100
                     mzlib/class
                     scheme/contract))

@mzlib[#:mode title class100]

The @scheme[class100] and @scheme[class100*] forms provide a syntax
close to that of @scheme[class] and @scheme[class*] in PLT Scheme
versions 100 through 103, but with the semantics of the current
@schememodname[scheme/class]-based class system. For a class defined
with @scheme[class100], keyword-based initialization arguments can be
propagated to the superclass, but by-position arguments are not (i.e.,
the expansion of @scheme[class100] to @scheme[class] always includes
an @scheme[init-rest] clause).

The @scheme[class100] form uses keywords (e.g., @scheme[public]) that
are defined by the @schememodname[mzlib/class] library, so typically
@schememodname[scheme/class] must be imported into any context that
imports @schememodname[mzlib/class100].


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

Like @scheme[class100*], but without @scheme[interface-expr]s.}


@defform[(class100-asi superclass instance-id-clause ...)]{

Like @scheme[class100], but all initialization arguments are
automatically passed on to the superclass initialization procedure by
position.}


@defform[(class100*-asi superclass interfaces instance-id-clause ...)]{

Like @scheme[class100*], but all initialization arguments are
automatically passed on to the superclass initialization procedure by
position.}


@defform[(super-init init-arg-expr ...)]{

An alias for @scheme[super-make-object].}

