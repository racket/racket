#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "stxcerts"]{Syntax Taints}

@guideintro["stx-certs"]{syntax taints}

The @deftech{tamper status} of a syntax object is either
@tech{tainted}, @tech{armed}, or clean:

@itemlist[

 @item{A @deftech{tainted} identifier is rejected by the macro expander
       for use as either a binding or expression. If a syntax object
       is @tech{tainted}, then any syntax object in the result of
       @racket[(syntax-e _stx)] is @tech{tainted}, and
       @racket[datum->syntax] with @racket[_stx] as its first argument
       produces a @tech{tainted} syntax object.

       Other derived operations, such as pattern matching in
       @racket[syntax-case], also taint syntax objects when extracting
       them from a @tech{tainted} syntax object.}

 @item{An @deftech{armed} syntax object has a set of @deftech{dye packs},
       which creates taints if the armed syntax object is used without
       first @tech{disarm}ing the dye packs. In particular, if a
       syntax object is @tech{armed}, @racket[syntax-e],
       @racket[datum->syntax], @racket[quote-syntax], and derived
       operations effectively treat the syntax object as
       @tech{tainted}.  The macro expander, in contrast,
       @tech{disarms} dye packs before pulling apart syntax
       objects.

       Each @tech{dye pack}, which is added to a syntax object with
       the @racket[syntax-arm] function, is keyed by an
       @tech{inspector}. A dye pack can be @deftech{disarm}ed using
       @racket[syntax-disarm] with an inspector that is the same as or
       a superior of the dye pack's inspector.}

 @item{A @defterm{clean} syntax object has no immediate taints or dye
       packs, although it may contain syntax objects that are
       @tech{tainted} or @tech{armed}.}

]

Taints cannot be removed, and attempting to arm a syntax object that
is already tainted has no effect on the resulting syntax object.

The macro expander @tech{disarm}s any syntax object that it encounters
in an expression position or as a module body. A syntax object is
therefore disarmed when it is provided to a @tech{syntax
transformer}. The transformer's result, however, is @deftech{rearm}ed
by copying to it any @tech{dye packs} that were originally attached to
the transformer's input. The @tech{rearm}ing process
@elemtag['(explain "taint-mode")]{obeys} the following rules:

@itemize[

 @item{If the result has a @indexed-racket['taint-mode] property (see
          @secref["stxprops"]) that is @indexed-racket['opaque], then
          dye packs are attached to the immediate syntax object.}

 @item{If the result has a @indexed-racket['taint-mode] property that
          is @indexed-racket['none], then no dye pack is attached to
          the syntax object. The @racket['none] mode is rarely
          appropriate.}

 @item{If the result has a @racket['taint-mode] property that is
          @indexed-racket['transparent], then the dye packs are
          propagated recursively to syntax object that corresponds to
          elements of the syntax object's datum as a list (or, more
          precisely, to the @racket[car]s of the datum as reached by
          any number of @racket[cdr]s), and the immediate syntax
          object loses its lexical context; If the immediate syntax
          object is already @tech{armed}, then recursive propagation
          taints the elements. Recursive propagation uses syntax
          properties and shapes, as for the immediate
          @tech{rearm}ing.}

 @item{If the result has a @racket['taint-mode] property that is
          @indexed-racket['transparent-binding], then dye packs are
          attached in a way similar to @racket['transparent], but
          further treating the syntax object corresponding to the
          second list element as having a @racket['transparent] value
          for the @racket['taint-mode] property if it does not already
          have a @racket['taint-mode] property value.}

 @item{If the result has no @racket['taint-mode] property value, but
          its datum is a pair, and if the syntax object corresponding
          to the @racket[car] of the pair is an identifier bound to
          @racket[begin], @racket[module], or
          @racket[#%plain-module-begin], then dye packs are propagated
          as if the syntax object had the @racket['transparent]
          property value.}

 @item{If the result has no @racket['taint-mode] property value, but
          its datum is a pair, and if the syntax object corresponding
          to the @racket[car] of the pair is an identifier bound to
          @racket[define-values] or @racket[define-syntaxes], then dye
          packs are propagated as if the syntax object had the
          @racket['transparent-binding] property value.}

]

For backward compatibility, a @indexed-racket['certify-mode] property
is treated the same as a @racket['taint-mode] property if the former
is not attached. To avoid accidental transfer of a
@racket['taint-mode] or @racket['certify-mode] property value, the
expander always removes any @racket['taint-mode] and
@racket['certify-mode] property on a syntax object that is passed to a
@tech{syntax transformer}.

@defproc[(syntax-tainted? [stx syntax?]) boolean?]{

Returns @racket[#t] if @racket[stx] is @tech{tainted}, @racket[#f]
otherwise.}


@defproc[(syntax-arm [stx syntax?]
                     [inspector (or/c inspector? #f) #f]
                     [use-mode? any/c #f])
         syntax?]{

Produces a syntax object like @racket[stx], but @tech{armed} with a
@tech{dye pack} that is keyed by @racket[inspector].

A @racket[#f] value for @racket[inspector] is equivalent to an
inspector that depends on the current dynamic context:

@itemlist[

 @item{when applying a syntax transformer is being applied, the
       declaration-time code inspector of the module in which a syntax
       transformer was bound;}

 @item{when a module is being visited, the module's declaration-time
       code inspector;}

 @item{@racket[(current-code-inspector)], otherwise.}

]

If @racket[use-mode?] is @racket[#f], then if @racket[stx] is
@tech{tainted} or already armed with the key @racket[inspector], the
result is @racket[stx].

If @racket[use-mode?] is a true value, then a @tech{dye pack} is
not necessarily added directly to @racket[stx]. Instead, the @tech{dye pack}
is pushed to interior syntax objects in the same way that the
expander pushes armings into a syntax transformer's results when
@tech{rearm}ing (based on a @racket['taint-mode] @tech{syntax
property} or identifier bindings); see @elemref['(explain
"taint-mode")]{the expander's rearming rules} for more information. To
the degree that pushing dye packs into a syntax object must destructure
@racket[stx], existing taints or dye packs can lead to @tech{tainted}
results rather than @tech{armed} results.}


@defproc[(syntax-protect [stx syntax?]) syntax?]{

Equivalent to @racket[(syntax-arm stx #f #f #t)].}


@defproc[(syntax-disarm [stx syntax?]
                        [inspector (or/c inspector? #f)])
         syntax?]{

Produces a @tech{disarm}ed version of @racket[stx], removing any
immediate @tech{dye packs} that match @racket[inspector]. An inspector
matches when it is either the same as or a super-inspector of the dye
pack's inspector.  A @racket[#f] value for @racket[inspector] is
replaced by a specific inspector in the same way as for
@racket[syntax-arm].}


@defproc[(syntax-rearm [stx syntax?]
                       [from-stx syntax?]
                       [use-mode? any/c #f])
         syntax?]{

Produces a @tech{rearm}ed or @tech{tainted} version of @racket[stx] by
adding all immediate taints and @tech{dye packs} of @racket[from-stx].

If @racket[use-mode?] is a true value, @racket[stx] is not necessarily
@tech{tainted} or @tech{armed} directly. Instead, taints or @tech{dye
packs} are pushed to interior syntax objects in the same way as for
@racket[syntax-arm] or @elemref['(explain "taint-mode")]{rearming by
the expander}.}


@defproc[(syntax-taint [stx syntax?]) syntax?]{

Returns @tech{tainted} version of @racket[stx]---equivalent to
@racket[(datum->syntax (syntax-arm stx) (syntax-e stx) stx stx)]---or
@racket[stx] if it is already tainted.}
