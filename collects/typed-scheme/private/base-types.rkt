#lang s-exp "type-env-lang.rkt"

[Complex -Number]
[Float-Complex -InexactComplex] ; for consistency with float vs inexact-real
[Inexact-Complex -InexactComplex] ; for backward compatiblity
[Number -Number]
[Integer -Integer]
[Real -Real]
[Exact-Rational -ExactRational]
[Float -Flonum] ;; these 2 are the default, 64-bit floats, can be optimized
[Nonnegative-Float -NonnegativeFlonum] ;; associated test is: flonum? 
[Inexact-Real -InexactReal] ;; any inexact real. could be 32- or 64-bit float
                            ;; associated test is: inexact-real?
[Exact-Positive-Integer -ExactPositiveInteger]
[Exact-Nonnegative-Integer -ExactNonnegativeInteger]
[Positive-Fixnum -PositiveFixnum]
[Nonnegative-Fixnum -NonnegativeFixnum]
[Fixnum -Fixnum]
[Natural -ExactNonnegativeInteger]
[Zero (-val 0)]

[Void -Void]
[Boolean -Boolean]
[Symbol -Symbol]
[String -String]
[Any Univ]
[Port -Port]
[Path -Path]
[Path-String -Pathlike]
[Regexp -Regexp]
[PRegexp -PRegexp]
[Byte-Regexp -Byte-Regexp]
[Byte-PRegexp -Byte-PRegexp]
[Char -Char]
[Namespace -Namespace]
[Input-Port -Input-Port]
[Output-Port -Output-Port]
[Bytes -Bytes]
[EOF (-val eof)]
[Sexpof (-poly (a) (-Sexpof a))]   ;; recursive union of sexps with a
[Syntaxof (-poly (a) (-Syntax a))] ;; syntax-e yields a
[Syntax-E In-Syntax] ;; possible results of syntax-e on "2D" syntax
[Syntax Any-Syntax]  ;; (Syntaxof Syntax-E): "2D" syntax
[Datum Syntax-Sexp]  ;; (Sexpof Syntax), datum->syntax yields "2D" syntax
[Sexp -Sexp]         ;; (Sexpof (U)), syntax->datum of "2D" syntax
[Identifier Ident]
[Procedure top-func]
[Keyword -Keyword]
[Thread -Thread]
[Resolved-Module-Path -Resolved-Module-Path]
[Module-Path -Module-Path]
[Module-Path-Index -Module-Path-Index]
[Compiled-Module-Expression -Compiled-Module-Expression]
[Listof -Listof]
[Vectorof (-poly (a) (make-Vector a))]
[FlVector -FlVector]
[Option (-poly (a) (-opt a))]
[HashTable (-poly (a b) (-HT a b))]
[Promise (-poly (a) (-Promise a))]
[Pair (-poly (a b) (-pair a b))]
[Boxof (-poly (a) (make-Box a))]
[Channelof (-poly (a) (make-Channel a))]
[Continuation-Mark-Set -Cont-Mark-Set]
[False (-val #f)]
[True (-val #t)]
[Null (-val null)]
[Nothing (Un)]
[Futureof (-poly (a) (-future a))]
[Pairof (-poly (a b) (-pair a b))]
[MPairof (-poly (a b) (-mpair a b))]
[MListof (-poly (a) (-mlst a))]
[Sequenceof (-poly (a) (-seq a))]

