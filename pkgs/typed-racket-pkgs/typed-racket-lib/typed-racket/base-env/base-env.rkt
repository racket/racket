#lang s-exp "env-lang.rkt"

;; This module defines Typed Racket's main base type environment.

(require
 (for-template
  (except-in racket -> ->* one-of/c class)
  racket/unsafe/ops
  racket/unsafe/undefined
  (only-in racket/extflonum floating-point-bytes->extfl extfl->floating-point-bytes)
  ;(only-in rnrs/lists-6 fold-left)
  '#%paramz
  "extra-procs.rkt"
  (only-in '#%kernel [apply kernel:apply] [reverse kernel:reverse])
  (only-in racket/private/pre-base new-apply-proc)
  compatibility/mlist
  racket/private/stx
  (only-in mzscheme make-namespace)
  (only-in racket/match/runtime match:error matchable? match-equality-test))
 "base-structs.rkt"
 racket/file
 (only-in racket/private/pre-base new-apply-proc)
 (only-in (types abbrev) [-Boolean B] [-Symbol Sym] -Flat)
 (only-in (types numeric-tower) [-Number N])
 (only-in (rep type-rep)
          make-ClassTop
          make-Name
          make-ValuesDots
          make-MPairTop
          make-BoxTop make-ChannelTop make-VectorTop
          make-ThreadCellTop
          make-Ephemeron
          make-CustodianBox
          make-HeterogeneousVector
          make-Continuation-Mark-Keyof
          make-Continuation-Mark-KeyTop
          make-Prompt-Tagof
          make-Prompt-TagTop
          make-StructType make-StructTypeTop
          make-ListDots))

;; Racket Reference
;; Section 4.1
[boolean? (make-pred-ty B)]
[not (make-pred-ty (-val #f))]
[equal? (-> Univ Univ B)]
[eqv? (-> Univ Univ B)]
[eq? (-> Univ Univ B)]

[equal?/recur (-> Univ Univ (-> Univ Univ Univ) B)]
[immutable? (-> Univ B)]
[prop:equal+hash -Struct-Type-Property]

;; Section 4.1.1 (racket/bool)
[true (-val #t)]
[false (-val #f)]
[boolean=? (B B . -> . B)]
[symbol=? (Sym Sym . -> . B)]
[false? (make-pred-ty (-val #f))]
[xor (-> Univ Univ Univ)]

;; Section 4.2 (Numbers)
;; These are mostly defined in base-env-numeric.rkt

;; Section 4.2.2.7 (Random Numbers)
[random
  (cl->* (->opt -PosFixnum [-Pseudo-Random-Generator] -NonNegFixnum)
         (->opt -Int [-Pseudo-Random-Generator] -Nat)
         (->opt [-Pseudo-Random-Generator] -Flonum))]

[random-seed (-> -PosInt -Void)]
[make-pseudo-random-generator (-> -Pseudo-Random-Generator)]
[pseudo-random-generator? (make-pred-ty -Pseudo-Random-Generator)]
[current-pseudo-random-generator (-Param -Pseudo-Random-Generator -Pseudo-Random-Generator)]
[pseudo-random-generator->vector
 (-> -Pseudo-Random-Generator (make-HeterogeneousVector (list -PosInt -PosInt -PosInt -PosInt -PosInt -PosInt)))]
[vector->pseudo-random-generator
 (-> (make-HeterogeneousVector (list -PosInt -PosInt -PosInt -PosInt -PosInt -PosInt)) -Pseudo-Random-Generator)]
[vector->pseudo-random-generator!
 (-> -Pseudo-Random-Generator (make-HeterogeneousVector (list -PosInt -PosInt -PosInt -PosInt -PosInt -PosInt)) -Void)]

;; Section 4.2.2.8 (Number-String Conversions)
[number->string (->opt N [N] -String)]
[string->number (->opt -String [N] (Un (-val #f) N))]

[floating-point-bytes->real (->opt -Bytes [Univ -Nat -Nat] -Flonum)]
[real->floating-point-bytes (->opt -Real (one-of/c  4 8) [Univ -Bytes -Nat] -Bytes)]
[system-big-endian? (-> B)]

[order-of-magnitude (-> -PosReal -Int)]

;; Section 4.2.5.3 (ExtFlonum-Bytes Conversions)
[floating-point-bytes->extfl (->opt -Bytes [Univ -Nat -Nat] -ExtFlonum)]
[extfl->floating-point-bytes (->opt -ExtFlonum [Univ -Bytes -Nat] -Bytes)]

;; Section 4.3 (Strings)
[string? (make-pred-ty -String)]
;make-string (in Index)
[string (->* '() -Char -String)]
[string->immutable-string (-> -String -String)]
[string-length (-String . -> . -Index)]
[unsafe-string-length (-String . -> . -Index)]
;string-ref (in Index)
;string-set! (in Index)
;substring (in Index)
[string-copy (-> -String -String)]
;string-copy! (in Index)
[string-fill! (-> -String -Char -Void)]
[string-append (->* null -String -String)]

[string->list (-String . -> . (-lst -Char))]
[list->string ((-lst -Char) . -> . -String)]
;build-string (in Index)

[string=? (->* (list -String -String) -String B)]
[string<? (->* (list -String -String) -String B)]
[string<=? (->* (list -String -String) -String B)]
[string>? (->* (list -String -String) -String B)]
[string>=? (->* (list -String -String) -String B)]

[string-ci=? (->* (list -String -String) -String B)]
[string-ci<? (->* (list -String -String) -String B)]
[string-ci<=? (->* (list -String -String) -String B)]
[string-ci>? (->* (list -String -String) -String B)]
[string-ci>=? (->* (list -String -String) -String B)]

[string-upcase (-> -String -String)]
[string-downcase (-> -String -String)]
[string-titlecase (-> -String -String)]
[string-foldcase (-> -String -String)]

[string-normalize-nfd (-> -String -String)]
[string-normalize-nfkd (-> -String -String)]
[string-normalize-nfc (-> -String -String)]
[string-normalize-nfkc (-> -String -String)]

[string-locale=? (->* (list -String -String) -String B)]
[string-locale<? (->* (list -String -String) -String B)]
[string-locale>? (->* (list -String -String) -String B)]

[string-locale-ci=? (->* (list -String -String) -String B)]
[string-locale-ci<? (->* (list -String -String) -String B)]
[string-locale-ci>? (->* (list -String -String) -String B)]

[string-locale-upcase (-> -String -String)]
[string-locale-downcase (-> -String -String)]

;; Section 4.3.5 (racket/string)
[string-append*
 (cl->* (-> (-lst -String) -String)
        (-> -String (-lst -String) -String))]
[string-join
 (->optkey (-lst -String) [-String]
           #:before-last -String #f #:before-first -String #f #:after-last -String #f
           -String)]
[string-normalize-spaces
 (->optkey -String [(Un -String -Regexp) -String]
           #:trim? Univ #f
           #:repeat? Univ #f
           -String)]
[string-replace
 (->key -String (Un -String -Regexp) -String
        #:all? Univ #f
        -String)]
[string-split
 (->optkey -String
           [(Un -String -Regexp)]
           #:trim? Univ #f
           #:repeat? Univ #f
           (-lst -String))]
[string-trim
 (->optkey -String [(Un -String -Regexp)]
           #:left? Univ #f
           #:right? Univ #f
           #:repeat? Univ #f
           -String)]

;; Section 4.3.6 (racket/format)
[~a (->optkey []
              #:rest Univ
              #:separator -String #f
              #:width (Un -Nat (-val #f)) #f
              #:max-width (Un -Nat (-val +inf.0)) #f
              #:min-width -Nat #f
              #:limit-marker -String #f
              #:align (one-of/c 'left 'center 'right) #f
              #:pad-string -String #f
              #:left-pad-string -String #f
              #:right-pad-string -String #f
              -String)]
[~v (->optkey []
              #:rest Univ
              #:separator -String #f
              #:width (Un -Nat (-val #f)) #f
              #:max-width (Un -Nat (-val +inf.0)) #f
              #:min-width -Nat #f
              #:limit-marker -String #f
              #:align (one-of/c 'left 'center 'right) #f
              #:pad-string -String #f
              #:left-pad-string -String #f
              #:right-pad-string -String #f
              -String)]
[~s (->optkey []
              #:rest Univ
              #:separator -String #f
              #:width (Un -Nat (-val #f)) #f
              #:max-width (Un -Nat (-val +inf.0)) #f
              #:min-width -Nat #f
              #:limit-marker -String #f
              #:align (one-of/c 'left 'center 'right) #f
              #:pad-string -String #f
              #:left-pad-string -String #f
              #:right-pad-string -String #f
              -String)]
[~e (->optkey []
              #:rest Univ
              #:separator -String #f
              #:width (Un -Nat (-val #f)) #f
              #:max-width (Un -Nat (-val +inf.0)) #f
              #:min-width -Nat #f
              #:limit-marker -String #f
              #:align (one-of/c 'left 'center 'right) #f
              #:pad-string -String #f
              #:left-pad-string -String #f
              #:right-pad-string -String #f
              -String)]
[~r (->optkey -Real []
              #:sign (Un (-val #f) (one-of/c '+ '++ 'parens)
                         (-lst* (Un -String (-lst* -String -String))
                                (Un -String (-lst* -String -String))
                                (Un -String (-lst* -String -String))))
                     #f
              #:base (Un -Integer (-lst* (-val 'up) -Integer)) #f
              #:precision (Un -Integer (-lst* (-val '=) -Integer)) #f
              #:notation (Un (-val 'positional) (-val 'exponential)
                             (-> -Real (one-of/c 'positional 'exponential)))
                         #f
              #:format-exponent (-opt (Un -String (-> -Integer -String))) #f
              #:min-width -Integer #f
              #:pad-string -String #f
              -String)]
[~.a (->optkey []
               #:rest Univ
               #:separator -String #f
               #:width (Un -Nat (-val #f)) #f
               #:max-width (Un -Nat (-val +inf.0)) #f
               #:min-width -Nat #f
               #:limit-marker -String #f
               #:align (one-of/c 'left 'center 'right) #f
               #:pad-string -String #f
               #:left-pad-string -String #f
               #:right-pad-string -String #f
               -String)]
[~.v (->optkey []
               #:rest Univ
               #:separator -String #f
               #:width (Un -Nat (-val #f)) #f
               #:max-width (Un -Nat (-val +inf.0)) #f
               #:min-width -Nat #f
               #:limit-marker -String #f
               #:align (one-of/c 'left 'center 'right) #f
               #:pad-string -String #f
               #:left-pad-string -String #f
               #:right-pad-string -String #f
               -String)]
[~.s (->optkey []
               #:rest Univ
               #:separator -String #f
               #:width (Un -Nat (-val #f)) #f
               #:max-width (Un -Nat (-val +inf.0)) #f
               #:min-width -Nat #f
               #:limit-marker -String #f
               #:align (one-of/c 'left 'center 'right) #f
               #:pad-string -String #f
               #:left-pad-string -String #f
               #:right-pad-string -String #f
               -String)]

;; Section 4.4 (Byte Strings)
[bytes (->* (list) -Integer -Bytes)]
[bytes? (make-pred-ty -Bytes)]
[make-bytes (cl-> [(-Integer -Integer) -Bytes]
                  [(-Integer) -Bytes])]
[bytes->immutable-bytes (-> -Bytes -Bytes)]
[byte? (make-pred-ty -Byte)]
[bytes-append (->* (list) -Bytes -Bytes)]
[bytes-length (-> -Bytes -Index)]
[unsafe-bytes-length (-> -Bytes -Index)]
[bytes-copy (-> -Bytes -Bytes)]
[bytes->list (-> -Bytes (-lst -Byte))]
[list->bytes (-> (-lst -Integer) -Bytes)]
[make-shared-bytes (cl-> [(-Integer -Byte) -Bytes]
                         [(-Integer) -Bytes])]
[shared-bytes (->* (list) -Byte -Bytes)]
[bytes<? (->* (list -Bytes) -Bytes B)]
[bytes>? (->* (list -Bytes) -Bytes B)]
[bytes=? (->* (list -Bytes) -Bytes B)]

[bytes-open-converter (-> -String -String (-opt -Bytes-Converter))]
[bytes-close-converter (-> -Bytes-Converter -Void)]
[bytes-convert
 (cl->*
  (->opt -Bytes-Converter
         -Bytes
         [-Nat
          -Nat
          (-val #f)
          -Nat
          (-opt -Nat)]
    (-values (list
              -Bytes
              -Nat
              (one-of/c 'complete 'continues  'aborts  'error))))
  (->opt -Bytes-Converter
         -Bytes
         -Nat
         -Nat
         -Bytes
         [-Nat
          (-opt -Nat)]
    (-values (list
              -Nat
              -Nat
              (one-of/c 'complete 'continues  'aborts  'error)))))]

[bytes-convert-end
 (cl->*
  (->opt -Bytes-Converter
         [(-val #f)
          -Nat
          (-opt -Nat)]
    (-values (list
              -Bytes
              (one-of/c 'complete 'continues))))
  (->opt -Bytes-Converter
         -Bytes
         [-Nat
          (-opt -Nat)]
    (-values (list
              -Nat
              (one-of/c 'complete 'continues)))))]

[bytes-converter? (make-pred-ty -Bytes-Converter)]

[locale-string-encoding (-> -String)]

[bytes-append* ((-lst -Bytes) . -> . -Bytes)]
[bytes-join ((-lst -Bytes) -Bytes . -> . -Bytes)]

;; Section 4.5 (Characters)
[char? (make-pred-ty -Char)]
[char=? (->* (list -Char -Char) -Char B)]
[char<=? (->* (list -Char -Char) -Char B)]
[char>=? (->* (list -Char -Char) -Char B)]
[char<? (->* (list -Char -Char) -Char B)]
[char>? (->* (list -Char -Char) -Char B)]
[char-ci=? (->* (list -Char -Char) -Char B)]
[char-ci<=? (->* (list -Char -Char) -Char B)]
[char-ci>=? (->* (list -Char -Char) -Char B)]
[char-ci>? (->* (list -Char -Char) -Char B)]
[char-ci<? (->* (list -Char -Char) -Char B)]

[char-alphabetic? (-> -Char B)]
[char-lower-case? (-> -Char B)]
[char-upper-case? (-> -Char B)]
[char-title-case? (-> -Char B)]
[char-numeric? (-> -Char B)]
[char-symbolic? (-> -Char B)]
[char-punctuation? (-> -Char B)]
[char-graphic? (-> -Char B)]
[char-whitespace? (-> -Char B)]
[char-blank? (-> -Char B)]
[char-iso-control? (-> -Char B)]
[char-general-category
 (-> -Char (apply Un (map -val
                          '(lu ll lt lm lo mn mc me nd nl no ps pe pi pf pd
                            pc po sc sm sk so zs zp zl cc cf cs co cn))))]
[make-known-char-range-list (-> (-lst (-lst* -PosInt -PosInt B)))]

[char-upcase (-> -Char -Char)]
[char-downcase (-> -Char -Char)]
[char-titlecase (-> -Char -Char)]
[char-foldcase (-> -Char -Char)]
[char->integer (-> -Char -Index)]
[integer->char (-> -Integer -Char)]
[char-utf-8-length (-> -Char (apply Un (map -val '(1 2 3 4 5 6))))]

;; Section 4.6 (Symbols)
[symbol? (make-pred-ty Sym)]
[symbol-interned? (-> Sym B)]
[symbol-unreadable? (-> Sym B)]
[symbol->string (Sym . -> . -String)]
[string->symbol (-String . -> . Sym)]
[string->uninterned-symbol (-String . -> . Sym)]
[string->unreadable-symbol (-String . -> . Sym)]
[gensym (->opt [(Un Sym -String)] Sym)]

;; Section 4.7 (Regular Expressions)
[regexp? (make-pred-ty -Regexp)]
[pregexp? (make-pred-ty -PRegexp)]
[byte-regexp? (make-pred-ty -Byte-Regexp)]
[byte-pregexp? (make-pred-ty -Byte-PRegexp)]
[regexp (-String . -> . -Base-Regexp)]
[pregexp (-String . -> . -PRegexp)]
[byte-regexp (-Bytes . -> . -Byte-Base-Regexp)]
[byte-pregexp (-Bytes . -> . -Byte-PRegexp)]

[regexp-quote (cl->*
               (->opt -String [Univ] -String)
               (->opt -Bytes [Univ] -Bytes))]

[regexp-max-lookbehind (-> (Un -Regexp -Byte-Regexp) -Nat)]

;In Index
;regexp-match
;regexp-match*
;regexp-try-match
;regexp-match-positions
;regexp-match?

[regexp-match-exact? (-> -Pattern (Un -String -Bytes -Path) B)]

;In Index
;regexp-match-peek
;regexp-match-peek-positions
;regexp-match-peek-immediate
;regexp-match-peek-positions-immediate
;regexp-match-peek-positions*
;regexp-match/end
;regexp-match-positions/end
;regexp-match-peek-positions/end
;regexp-match-peek-positions-immediate/end

[regexp-replace
 (cl->*
  (->opt (Un -String -Regexp) -String (Un -String (->* (list -String) -String -String)) [-Bytes] -String)
  (->opt (Un -Bytes -Byte-Regexp) (Un -Bytes -String) (Un -Bytes -String (->* (list -Bytes) -Bytes -Bytes)) [-Bytes] -Bytes)
  (->opt -Pattern -Bytes (Un -Bytes -String (->* (list -Bytes) -Bytes -Bytes)) [-Bytes] -Bytes))]
[regexp-replace*
 (cl->*
  (->opt (Un -String -Regexp) -String (Un -String (->* (list -String) -String -String)) [-Bytes] -String)
  (->opt (Un -Bytes -Byte-Regexp) (Un -Bytes -String) (Un -Bytes -String (->* (list -Bytes) -Bytes -Bytes)) [-Bytes] -Bytes)
  (->opt -Pattern -Bytes (Un -Bytes -String (->* (list -Bytes) -Bytes -Bytes)) [-Bytes] -Bytes))]
[regexp-replaces
 (-> (Un -String -Bytes)
     (-lst (-lst* (Un -String -Bytes -Regexp -Byte-Regexp)
                  (Un -String -Bytes
                      (->* (list -String) -String -String)
                      (->* (list -Bytes) -Bytes -Bytes))))
     (Un -String -Bytes))]
[regexp-replace-quote
 (cl->*
  [-> -String -String]
  [-> -Bytes -Bytes])]

;; Section 4.8 (Keywords)
[keyword? (make-pred-ty -Keyword)]
[string->keyword (-String . -> . -Keyword)]
[keyword->string (-Keyword . -> . -String)]
[keyword<? (->* (list -Keyword -Keyword) -Keyword B)]

;; Section 4.9 (Pairs and Lists)
[car   (-poly (a b)
              (cl->*
               (->acc (list (-pair a b)) a (list -car))
               (->* (list (-lst a)) a)))]
[cdr   (-poly (a b)
              (cl->*
               (->acc (list (-pair a b)) b (list -cdr))
               (->* (list (-lst a)) (-lst a))))]

;; these type signatures do not cover all valid uses of these pair accessors
[caar (-poly (a b c)
             (cl->* [->acc (list (-pair (-pair a b) c)) a (list -car -car)]
                    [-> (-lst (-pair a b)) a]
                    [-> (-pair (-lst a) b) a]
                    [-> (-lst (-lst a)) a]))]
[cdar (-poly (a b c)
             (cl->* [->acc (list (-pair (-pair a b) c)) b (list -cdr -car)]
                    [-> (-lst (-pair a b)) b]
                    [-> (-pair (-lst a) b) (-lst a)]
                    [-> (-lst (-lst a)) (-lst a)]))]
[cadr (-poly (a b c)
             (cl->* [->acc (list (-pair a (-pair b c))) b (list -car -cdr)]
                    [-> (-lst a) a]))]
[cddr  (-poly (a b c)
              (cl->* [->acc (list (-pair a (-pair b c))) c (list -cdr -cdr)]
                     [-> (-lst a) (-lst a)]))]

[caaar (-poly (a b c d)
              (cl->* [->acc (list (-pair (-pair (-pair a b) c) d)) a (list -car -car -car)]
                     [-> (-lst (-lst (-lst a))) a]))]
[cdaar (-poly (a b c d)
              (cl->* [->acc (list (-pair (-pair (-pair a b) c) d)) b (list -cdr -car -car)]
                     [-> (-lst (-lst (-lst a))) (-lst a)]))]
[cadar (-poly (a b c d)
              (cl->* [->acc (list (-pair (-pair a (-pair b c)) d)) b (list -car -cdr -car)]
                     [-> (-lst (-lst a)) a]))]
[cddar (-poly (a b c d)
              (cl->* [->acc (list (-pair (-pair a (-pair b c)) d)) c (list -cdr -cdr -car)]
                     [-> (-lst (-lst a)) (-lst a)]))]
[caadr (-poly (a b c d)
              (cl->* [->acc (list (-pair a (-pair (-pair b c) d))) b (list -car -car -cdr)]
                     [-> (-lst (-lst a)) a]))]
[cdadr (-poly (a b c d)
              (cl->* [->acc (list (-pair a (-pair (-pair b c) d))) c (list -cdr -car -cdr)]
                     [-> (-lst (-lst a)) (-lst a)]))]
[caddr  (-poly (a b c d)
              (cl->* [->acc (list (-pair a (-pair b (-pair c d)))) c (list -car -cdr -cdr)]
                     [-> (-lst a) a]))]
[cdddr (-poly (a b c d)
              (cl->* [->acc (list (-pair a (-pair b (-pair c d)))) d (list -cdr -cdr -cdr)]
                     [-> (-lst a) (-lst a)]))]

[caaaar (-poly (a b c d e)
               (cl->* [->acc (list (-pair (-pair (-pair (-pair a b) c) d) e)) a (list -car -car -car -car)]
                      [-> (-lst (-lst (-lst (-lst a)))) a]))]
[cdaaar (-poly (a b c d e)
               (cl->* [->acc (list (-pair (-pair (-pair (-pair a b) c) d) e)) b (list -cdr -car -car -car)]
                      [-> (-lst (-lst (-lst (-lst a)))) (-lst a)]))]
[cadaar (-poly (a b c d e)
               (cl->* [->acc (list (-pair (-pair (-pair a (-pair b c)) d) e)) b (list -car -cdr -car -car)]
                      [-> (-lst (-lst (-lst a))) a]))]
[cddaar (-poly (a b c d e)
               (cl->* [->acc (list (-pair (-pair (-pair b (-pair b c)) d) e)) c (list -cdr -cdr -car -car)]
                      [-> (-lst (-lst (-lst a))) (-lst a)]))]
[caadar (-poly (a b c d e)
               (cl->* [->acc (list (-pair (-pair a (-pair (-pair b c) d)) e)) b (list -car -car -cdr -car)]
                      [-> (-lst (-lst (-lst a))) a]))]
[cdadar (-poly (a b c d e)
               (cl->* [->acc (list (-pair (-pair a (-pair (-pair b c) d)) e)) c (list -cdr -car -cdr -car)]
                      [-> (-lst (-lst (-lst a))) (-lst a)]))]
[caddar (-poly (a b c d e)
               (cl->* [->acc (list (-pair (-pair a (-pair b (-pair c d))) e)) c (list -car -cdr -cdr -car)]
                      [-> (-lst (-lst a)) a]))]
[cdddar (-poly (a b c d e)
               (cl->* [->acc (list (-pair (-pair a (-pair b (-pair c d))) e)) d (list -cdr -cdr -cdr -car)]
                      [-> (-lst (-lst a)) (-lst a)]))]
[caaadr (-poly (a b c d e)
               (cl->* [->acc (list (-pair a (-pair (-pair (-pair b c) d) e))) b (list -car -car -car -cdr)]
                      [-> (-lst (-lst (-lst a))) a]))]
[cdaadr (-poly (a b c d e)
               (cl->* [->acc (list (-pair a (-pair (-pair (-pair b c) d) e))) c (list -cdr -car -car -cdr)]
                      [-> (-lst (-lst (-lst a))) (-lst a)]))]
[cadadr (-poly (a b c d e)
               (cl->* [->acc (list (-pair a (-pair (-pair b (-pair c d)) e))) c (list -car -cdr -car -cdr)]
                      [-> (-lst (-lst a)) a]))]
[cddadr (-poly (a b c d e)
               (cl->* [->acc (list (-pair a (-pair (-pair b (-pair c d)) e))) d (list -cdr -cdr -car -cdr)]
                      [-> (-lst (-lst a)) (-lst a)]))]
[caaddr (-poly (a b c d e)
               (cl->* [->acc (list (-pair a (-pair b (-pair (-pair c d) e)))) c (list -car -car -cdr -cdr)]
                      [-> (-lst (-lst a)) a]))]
[cdaddr (-poly (a b c d e)
               (cl->* [->acc (list (-pair a (-pair b (-pair (-pair c d) e)))) d (list -cdr -car -cdr -cdr)]
                      [-> (-lst (-lst a)) (-lst a)]))]
[cadddr (-poly (a b c d e)
               (cl->* [->acc (list (-pair a (-pair b (-pair c (-pair d e))))) d (list -car -cdr -cdr -cdr)]
                      [-> (-lst a) a]))]
[cddddr (-poly (a b c d e)
               (cl->* [->acc (list (-pair a (-pair b (-pair c (-pair d e))))) e (list -cdr -cdr -cdr -cdr)]
                      [-> (-lst a) (-lst a)]))]

[first (-poly (a b)
              (cl->*
               (->acc (list (-pair a (-lst b))) a (list -car))
               (->* (list (-lst a)) a)))]
[second (-poly (a r t)
               (cl->* [->acc (list (-lst* a r #:tail (-lst t))) r (list -car -cdr)]
                      [->* (list (-lst a)) a]))]
[third (-poly (a b r t)
              (cl->* [->acc (list (-lst* a b r #:tail (-lst t))) r (list -car -cdr -cdr)]
                     [->* (list (-lst a)) a]))]
[fourth  (-poly (a b c r t)
              (cl->* [->acc (list (-lst* a b c r #:tail (-lst t))) r (list -car -cdr -cdr -cdr)]
                     [->* (list (-lst a)) a]))]
[fifth   (-poly (a b c d r t)
              (cl->* [->acc (list (-lst* a b c d r #:tail (-lst t))) r (list -car -cdr -cdr -cdr -cdr)]
                     [->* (list (-lst a)) a]))]
[sixth   (-poly (a b c d e r t)
              (cl->* [->acc (list (-lst* a b c d e r #:tail (-lst t))) r (list -car -cdr -cdr -cdr -cdr -cdr)]
                     [->* (list (-lst a)) a]))]
[seventh (-poly (a b c d e f r t)
              (cl->* [->acc (list (-lst* a b c d e f r #:tail (-lst t))) r (list -car -cdr -cdr -cdr -cdr -cdr -cdr)]
                     [->* (list (-lst a)) a]))]
[eighth  (-poly (a b c d e f g r t)
              (cl->* [->acc (list (-lst* a b c d e f g r #:tail (-lst t))) r (list -car -cdr -cdr -cdr -cdr -cdr -cdr -cdr)]
                     [->* (list (-lst a)) a]))]
[ninth   (-poly (a b c d e f g h r t)
              (cl->* [->acc (list (-lst* a b c d e f g h r #:tail (-lst t))) r (list -car -cdr -cdr -cdr -cdr -cdr -cdr -cdr -cdr)]
                     [->* (list (-lst a)) a]))]
[tenth   (-poly (a b c d e f g h i r t)
              (cl->* [->acc (list (-lst* a b c d e f g h i r #:tail (-lst t))) r (list -car -cdr -cdr -cdr -cdr -cdr -cdr -cdr -cdr -cdr)]
                     [->* (list (-lst a)) a]))]
[rest (-poly (a b)
             (cl->*
              (->acc (list (-pair a (-lst b))) (-lst b) (list -cdr))
              (->* (list (-lst a)) (-lst a))))]

[cons (-poly (a b)
             (cl->* [->* (list a (-lst a)) (-lst a)]
                    [->* (list a b) (-pair a b)]))]
#;[*cons (-poly (a b) (cl->
                     [(a b) (-pair a b)]
                     [(a (-lst a)) (-lst a)]))]
#;[*list? (make-pred-ty (-lst Univ))]

[null? (make-pred-ty -Null)]
[null -Null]
[cons? (make-pred-ty (-pair Univ Univ))]
[pair? (make-pred-ty (-pair Univ Univ))]
[empty? (make-pred-ty -Null)]
[empty -Null]

[ormap (-polydots (a c b) (->... (list (->... (list a) (b b) c) (-lst a)) ((-lst b) b) (Un c (-val #f))))]
[andmap (-polydots (a c d b) (cl->*
                              (make-pred-ty (list (make-pred-ty (list a) c d) (-lst a)) c (-lst d)
                                            ;; predicate on second argument
                                            (-arg-path 1))
                              (->... (list (->... (list a) (b b) c) (-lst a)) ((-lst b) b) (Un c (-val #t)))))]

[reverse (-poly (a) (-> (-lst a) (-lst a)))]
[kernel:reverse (-poly (a) (-> (-lst a) (-lst a)))]
[append (-poly (a) (->* (list) (-lst a) (-lst a)))]
[length (-poly (a) (-> (-lst a) -Index))]
[memq (-poly (a) (-> Univ (-lst a) (-opt (-lst a))))]
[memv (-poly (a) (-> Univ (-lst a) (-opt (-lst a))))]
[memf (-poly (a) ((a . -> . Univ) (-lst a) . -> . (-opt (-lst a))))]
[member (-poly (a)
          (cl->* (Univ (-lst a) . -> . (-opt (-lst a)))
                 (Univ (-lst a) (-> a a Univ)
                       . -> . (-opt (-lst a)))))]
[findf (-poly (a) ((a . -> . B) (-lst a) . -> . (-opt a)))]

[assq  (-poly (a b) (Univ (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assv  (-poly (a b) (Univ (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assoc (-poly (a b) (Univ (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assf  (-poly (a b) ((a . -> . Univ) (-lst (-pair a b))
                     . -> . (-opt (-pair a b))))]

[list? (make-pred-ty (-lst Univ))]
[list (-poly (a) (->* '() a (-lst a)))]
[map (-polydots (c a b)
                (cl->*
                 (-> (-> a c) (-pair a (-lst a)) (-pair c (-lst c)))
                 ((list
                   ((list a) (b b) . ->... . c)
                   (-lst a))
                  ((-lst b) b) . ->... .(-lst c))))]
[for-each (-polydots (a b) ((list ((list a) (b b) . ->... . Univ) (-lst a))
                            ((-lst b) b) . ->... . -Void))]
#;[fold-left (-polydots (c a b) ((list ((list c a) (b b) . ->... . c) c (-lst a))
                               ((-lst b) b) . ->... . c))]
#;[fold-right (-polydots (c a b) ((list ((list c a) (b b) . ->... . c) c (-lst a))
                                ((-lst b) b) . ->... . c))]
[foldl
 (-poly (a b c d)
        (cl-> [((a b . -> . b) b (-lst a)) b]
              [((a b c . -> . c) c (-lst a) (-lst b)) c]
              [((a b c d . -> . d) d (-lst a) (-lst b) (-lst d)) d]))]
[foldr  (-poly (a b c d)
               (cl-> [((a b . -> . b) b (-lst a)) b]
                     [((a b c . -> . c) c (-lst a) (-lst b)) c]
                     [((a b c d . -> . d) d (-lst a) (-lst b) (-lst d)) d]))]
[filter (-poly (a b) (cl->*
                      ((asym-pred a Univ (-FS (-filter b 0) -top))
                       (-lst a)
                       . -> .
                       (-lst b))
                      ((a . -> . Univ) (-lst a) . -> . (-lst a))))]
[filter-not (-poly (a) (cl->*
                        ((a . -> . Univ) (-lst a) . -> . (-lst a))))]
[shuffle (-poly (a) (-> (-lst a) (-lst a)))]

[remove  (-poly (a) (Univ (-lst a) . -> . (-lst a)))]
[remq    (-poly (a) (Univ (-lst a) . -> . (-lst a)))]
[remv    (-poly (a) (Univ (-lst a) . -> . (-lst a)))]
[remove* (-poly (a b) ((-lst a) (-lst b) [(a b . -> . B)] . ->opt . (-lst b)))]
[remq*   (-poly (a) (-> (-lst Univ) (-lst a) (-lst a)))]
[remv*   (-poly (a) (-> (-lst Univ) (-lst a) (-lst a)))]
#|
[sort (-poly (a b) (cl->* ((-lst a) (a a . -> . B)
                          #:cache-keys? B #f
                          . ->key . (-lst a))
                         ((-lst a) (b b . -> . B)
                          #:key (a . -> . b) #t
                          #:cache-keys? B #f
                          . ->key . (-lst a))))]
|#

;; Section 4.9.7 (racket/list)
[filter-map (-polydots (c a b)
                       ((list
                         ((list a) (b b) . ->... . (-opt c))
                         (-lst a))
                        ((-lst b) b) . ->... . (-lst c)))]
[count (-polydots (a b)
                  ((list
                    ((list a) (b b) . ->... . Univ)
                    (-lst a))
                   ((-lst b) b)
                   . ->... .
                   -Index))]
[partition
 (-poly (a b) (cl->* (-> (-> a Univ) (-lst a) (-values (list (-lst a) (-lst a))))
                     (-> (make-pred-ty a) (-lst b) (-values (list (-lst a) (-lst b))))))]

[last   (-poly (a) ((-lst a) . -> . a))]
[add-between (-poly (a b) ((-lst a) b
                           #:splice? -Boolean #f
                           #:before-first (-lst b) #f
                           #:before-last b #f
                           #:after-last (-lst b) #f
                           . ->key . (-lst (Un a b))))]

[last-pair (-poly (a) ((-mu x (Un a -Null (-pair a x)))
                       . -> .
                       (Un (-pair a a) (-lst* a))))]
[takef
 (-poly (a b)
   (cl->*
    (-> (-lst a)
        (asym-pred a Univ (-FS (-filter b 0) -top))
        (-lst b))
    (-> (-lst a) (-> a Univ) (-lst a))))]
[dropf (-poly (a) (-> (-lst a) (-> a Univ) (-lst a)))]
[splitf-at
 (-poly (a b)
   (cl->*
    (-> (-lst a)
        (asym-pred a Univ (-FS (-filter b 0) -top))
        (-values (list (-lst b) (-lst a))))
    (-> (-lst a) (-> a Univ) (-values (list (-lst a) (-lst a))))))]
[takef-right
 (-poly (a b)
   (cl->*
    (-> (-lst a)
        (asym-pred a Univ (-FS (-filter b 0) -top))
        (-lst b))
    (-> (-lst a) (-> a Univ) (-lst a))))]
[dropf-right (-poly (a) (-> (-lst a) (-> a Univ) (-lst a)))]
[splitf-at-right
 (-poly (a b)
   (cl->*
    (-> (-lst a)
        (asym-pred a Univ (-FS (-filter b 0) -top))
        (-values (list (-lst a) (-lst b))))
    (-> (-lst a) (-> a Univ) (-values (list (-lst a) (-lst a))))))]
[append-map
 (-polydots (c a b) ((list ((list a) (b b) . ->... . (-lst c)) (-lst a))
                     ((-lst b) b) . ->... .(-lst c)))]
[append*
 (-poly (a) ((-lst (-lst a)) . -> . (-lst a)))]
[permutations (-poly (a) (-> (-lst a) (-lst (-lst a))))]
[in-permutations (-poly (a) (-> (-lst a) (-seq (-lst a))))]
[argmin (-poly (a) ((a . -> . -Real) (-lst a) . -> . a))]
[argmax (-poly (a) ((a . -> . -Real) (-lst a) . -> . a))]

;; Section 4.9.8 (Immutable Cyclic Data)
[make-reader-graph (-> Univ Univ)]

;; Section 4.10 (Mutable Pairs)
[mcons (-poly (a b) (-> a b (-mpair a b)))]
[mcar (-poly (a b)
             (cl->* (-> (-mpair a b) a)
                    (-> (-mlst a) a)))]
[mcdr (-poly (a b)
             (cl->* (-> (-mpair a b) b)
                    (-> (-mlst a) (-mlst a))))]
[set-mcar! (-poly (a b)
                  (cl->* (-> (-mpair a b) a -Void)
                         (-> (-mlst a) a -Void)))]
[set-mcdr! (-poly (a b)
                  (cl->* (-> (-mpair a b) b -Void)
                         (-> (-mlst a) (-mlst a) -Void)))]
[unsafe-mcar (-poly (a b)
                    (cl->* (-> (-mpair a b) a)
                           (-> (-mlst a) a)))]
[unsafe-mcdr (-poly (a b)
                    (cl->* (-> (-mpair a b) b)
                           (-> (-mlst a) (-mlst a))))]
[unsafe-set-mcar! (-poly (a b)
                         (cl->* (-> (-mpair a b) a -Void)
                                (-> (-mlst a) a -Void)))]
[unsafe-set-mcdr! (-poly (a b)
                         (cl->* (-> (-mpair a b) b -Void)
                                (-> (-mlst a) (-mlst a) -Void)))]
[mpair? (make-pred-ty (make-MPairTop))]
[mlist (-poly (a) (->* (list) a (-mlst a)))]
[mlength (-poly (a) (-> (-mlst a) -Index))]
[mreverse! (-poly (a) (-> (-mlst a) (-mlst a)))]
[mappend (-poly (a) (->* (list) (-mlst a) (-mlst a)))]

;; Section 4.11 (Vectors)
[vector? (make-pred-ty (make-VectorTop))]
[vector->list (-poly (a) (-> (-vec a) (-lst a)))]
[list->vector (-poly (a) (-> (-lst a) (-vec a)))]
[vector-length ((make-VectorTop) . -> . -Index)]
[vector (-poly (a) (->* (list) a (-vec a)))]
[vector-immutable (-poly (a) (->* (list) a (-vec a)))]
[vector->immutable-vector (-poly (a) (-> (-vec a) (-vec a)))]
[vector-fill! (-poly (a) (-> (-vec a) a -Void))]
[vector-argmax (-poly (a) (-> (-> a -Real) (-vec a) a))]
[vector-argmin (-poly (a) (-> (-> a -Real) (-vec a) a))]
[vector-memq (-poly (a) (-> Univ (-vec a) (-opt -Index)))]
[vector-memv (-poly (a) (-> Univ (-vec a) (-opt -Index)))]
[vector-member (-poly (a) (Univ (-vec a) . -> . (-opt -Index)))]
;; [vector->values no good type here]

;; Section 4.11.1 (racket/vector)
[vector-count (-polydots (a b)
                         ((list
                           ((list a) (b b) . ->... . Univ)
                           (-vec a))
                          ((-vec b) b)
                          . ->... .
                          -Index))]
[vector-filter (-poly (a b) (cl->*
                              ((asym-pred a Univ (-FS (-filter b 0) -top))
                               (-vec a)
                               . -> .
                               (-vec b))
                              ((a . -> . Univ) (-vec a) . -> . (-vec a))))]

[vector-filter-not
 (-poly (a b) (cl->* ((a . -> . Univ) (-vec a) . -> . (-vec a))))]
[vector-copy
 (-poly (a)
        (cl->* ((-vec a) . -> . (-vec a))
               ((-vec a) -Integer . -> . (-vec a))
               ((-vec a) -Integer -Integer . -> . (-vec a))))]
[vector-map (-polydots (c a b) ((list ((list a) (b b) . ->... . c) (-vec a))
                                ((-vec b) b) . ->... .(-vec c)))]
[vector-map! (-polydots (a b) ((list ((list a) (b b) . ->... . a) (-vec a))
                               ((-vec b) b) . ->... .(-vec a)))]
[vector-append (-poly (a) (->* (list) (-vec a) (-vec a)))]
[vector-take   (-poly (a) ((-vec a) -Integer . -> . (-vec a)))]
[vector-drop   (-poly (a) ((-vec a) -Integer . -> . (-vec a)))]
[vector-take-right   (-poly (a) ((-vec a) -Integer . -> . (-vec a)))]
[vector-drop-right   (-poly (a) ((-vec a) -Integer . -> . (-vec a)))]
[vector-split-at
 (-poly (a) ((-vec a) -Integer . -> . (-values (list (-vec a) (-vec a)))))]
[vector-split-at-right
 (-poly (a) ((-vec a) -Integer . -> . (-values (list (-vec a) (-vec a)))))]

;; Section 4.12 (Boxes)
[box (-poly (a) (a . -> . (-box a)))]
[box-immutable (-poly (a) (a . -> . (-box a)))]
[unbox (-poly (a) (cl->*
                   ((-box a) . -> . a)
                   ((make-BoxTop) . -> . Univ)))]
[set-box! (-poly (a) ((-box a) a . -> . -Void))]
[unsafe-unbox (-poly (a) (cl->*
                          ((-box a) . -> . a)
                          ((make-BoxTop) . -> . Univ)))]
[unsafe-set-box! (-poly (a) ((-box a) a . -> . -Void))]
[unsafe-unbox* (-poly (a) (cl->*
                           ((-box a) . -> . a)
                           ((make-BoxTop) . -> . Univ)))]
[unsafe-set-box*! (-poly (a) ((-box a) a . -> . -Void))]
[box? (make-pred-ty (make-BoxTop))]

;; Section 4.13 (Hash Tables)
[hash? (make-pred-ty -HashTop)]
[hash-eq? (-> -HashTop B)]
[hash-eqv? (-> -HashTop B)]
[hash-equal? (-> -HashTop B)]
[hash-weak? (-> -HashTop B)]
;; not a very useful type, but better than nothing
[hash (-poly (a b) (-> (-HT a b)))]
[hasheqv (-poly (a b) (-> (-HT a b)))]
[hasheq (-poly (a b) (-> (-HT a b)))]
[make-hash (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-hasheq (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-hasheqv (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-weak-hash (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-weak-hasheq (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-weak-hasheqv (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-immutable-hash (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-immutable-hasheq (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-immutable-hasheqv (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]

[hash-set (-poly (a b) ((-HT a b) a b . -> . (-HT a b)))]
[hash-set! (-poly (a b) ((-HT a b) a b . -> . -Void))]
[hash-ref (-poly (a b c)
                 (cl-> [((-HT a b) a) b]
                       [((-HT a b) a (-val #f)) (-opt b)]
                       [((-HT a b) a (-> c)) (Un b c)]
                       [(-HashTop a) Univ]
                       [(-HashTop a (-val #f)) Univ]
                       [(-HashTop a (-> c)) Univ]))]
[hash-ref! (-poly (a b) (-> (-HT a b) a (-> b) b))]
[hash-has-key? (-HashTop Univ . -> . B)]
[hash-update! (-poly (a b)
                     (cl-> [((-HT a b) a (-> b b)) -Void]
                           [((-HT a b) a (-> b b) (-> b)) -Void]))]
[hash-update (-poly (a b)
                    (cl-> [((-HT a b) a (-> b b)) (-HT a b)]
                          [((-HT a b) a (-> b b) (-> b)) (-HT a b)]))]
[hash-remove (-poly (a b) (cl-> [((-HT a b) Univ) (-HT a b)]
                                [(-HashTop Univ) -HashTop]))]
[hash-remove! (-poly (a b) (cl-> [((-HT a b) a) -Void]
                                 [(-HashTop a) -Void]))]
[hash-map (-poly (a b c) (cl-> [((-HT a b) (a b . -> . c)) (-lst c)]
                               [(-HashTop (Univ Univ . -> . c)) (-lst c)]))]
[hash-for-each (-poly (a b c) (cl-> [((-HT a b) (-> a b c)) -Void]
                                    [(-HashTop (-> Univ Univ c)) -Void]))]
[hash-count (-poly (a b) (cl-> [((-HT a b)) -Index]
                               [(-HashTop) -Index]))]
[hash-keys (-poly (a b) (cl-> [((-HT a b)) (-lst a)]
                              [(-HashTop) (-lst Univ)]))]
[hash-values (-poly (a b) (cl-> [((-HT a b)) (-lst b)]
                                [(-HashTop) (-lst Univ)]))]
[hash->list (-poly (a b) (cl-> [((-HT a b)) (-lst (-pair a b))]
                               [(-HashTop) (-lst (-pair Univ Univ))]))]

[hash-copy (-poly (a b) (-> (-HT a b) (-HT a b)))]
[eq-hash-code (-> Univ -Fixnum)]
[eqv-hash-code (-> Univ -Fixnum)]
[equal-hash-code (-> Univ -Fixnum)]
[equal-secondary-hash-code (-> Univ -Fixnum)]
[hash-iterate-first (-poly (a b)
                           ((-HT a b) . -> . (Un (-val #f) -Integer)))]
[hash-iterate-next (-poly (a b)
                           ((-HT a b) -Integer . -> . (Un (-val #f) -Integer)))]
[hash-iterate-key (-poly (a b)
                           ((-HT a b) -Integer . -> . a))]
[hash-iterate-value (-poly (a b)
                           ((-HT a b) -Integer . -> . b))]

[make-custom-hash (->opt (-> Univ Univ Univ) (-> Univ -Nat) [(-> Univ -Nat)] Univ)]
[make-immutable-custom-hash (->opt (-> Univ Univ Univ) (-> Univ -Nat) [(-> Univ -Nat)] Univ)]
[make-weak-custom-hash (->opt (-> Univ Univ Univ) (-> Univ -Nat) [(-> Univ -Nat)] Univ)]

;; Section 4.14 (Sequences and Streams)
[sequence? (make-pred-ty (-seq Univ))]
[in-sequences
 (-poly (a) (->* (list) (-seq a) (-seq a)))]
[in-cycle
 (-poly (a) (->* (list) (-seq a) (-seq a)))]
[in-parallel
 (-poly (a b c)
   (cl->* (-> (-seq a) (-seq a))
          (-> (-seq a) (-seq b) (-seq a b))
          (-> (-seq a) (-seq b) (-seq c) (-seq a b c))))]
[in-values-sequence
 (-poly (a b c)
   (cl->* (-> (-seq a) (-seq (-lst* a)))
          (-> (-seq a b) (-seq (-lst* a b)))
          (-> (-seq a b c) (-seq (-lst* a b c)))))]
[in-values*-sequence
 (-poly (a b c)
   (cl->* (-> (-seq a) (-seq a))
          (-> (-seq a b) (-seq (-lst* a b)))
          (-> (-seq a b c) (-seq (-lst* a b c)))))]
[stop-before (-poly (a) ((-seq a) (a . -> . Univ) . -> . (-seq a)))]
[stop-after (-poly (a) ((-seq a) (a . -> . Univ) . -> . (-seq a)))]
[make-do-sequence (-poly (a b) ((-> (-values (list (a . -> . b)
                                                   (a . -> . a)
                                                   a
                                                   (Un (a . -> . Univ) (-val #f))
                                                   (Un (b . -> . Univ) (-val #f))
                                                   (Un (a b . -> . Univ) (-val #f)))))
                                . -> . (-seq b)))]
[sequence-generate (-poly (a) ((-seq a) . -> . (-values (list (-> -Boolean) (-> a)))))]
;; Doesn't work (mu types are single-valued only):
;[sequence-generate*  (-poly (a) ((-seq a) . -> . (-mu t (-values (list (Un (-lst a) (-val #f)) t)))))]
;; Doesn't render nicely (but seems to work fine):
[empty-sequence (-poly (a) (-seq a))]
[sequence->list (-poly (a) ((-seq a) . -> . (-lst a)))]
[sequence-length (-poly (a) ((-seq a) . -> . -Nat))]
[sequence-ref (-poly (a) ((-seq a) -Integer . -> . a))]
[sequence-tail (-poly (a) ((-seq a) -Integer . -> . (-seq a)))]
[sequence-append (-poly (a) (->* (list) (-seq a) (-seq a)))]
[sequence-map (-poly (a b) ((a . -> . b) (-seq a) . -> . (-seq b)))]
[sequence-andmap (-poly (a b) ((a . -> . b) (-seq a) . -> . (Un b (-val #t))))]
[sequence-ormap (-poly (a b) ((a . -> . b) (-seq a) . -> . (Un b (-val #f))))]
[sequence-for-each (-poly (a) ((a . -> . Univ) (-seq a) . -> . -Void))]
[sequence-fold (-poly (a b) ((b a . -> . b) b (-seq a) . -> . b))]
[sequence-count (-poly (a) ((a . -> . Univ) (-seq a) . -> . -Nat))]
[sequence-filter (-poly (a b) (cl->*
                                ((asym-pred a Univ (-FS (-filter b 0) -top))
                                 (-seq a)
                                 . -> .
                                 (-seq b))
                                ((a . -> . Univ) (-seq a) . -> . (-seq a))))]
[sequence-add-between (-poly (a) ((-seq a) a . -> . (-seq a)))]

;; Section 4.16 (Sets)
[set (-poly (e) (->* (list) e (-set e)))]
[seteqv (-poly (e) (->* (list) e (-set e)))]
[seteq (-poly (e) (->* (list) e (-set e)))]
[set-empty? (-poly (e) (-> (-set e) B))]
[set-count (-poly (e) (-> (-set e) -Index))]
[set-member? (-poly (e) (-> (-set e) e B))]
[set-first (-poly (e) (-> (-set e) e))]
[set-rest (-poly (e) (-> (-set e) (-set e)))]
[set-add (-poly (e) (-> (-set e) e (-set e)))]
[set-remove (-poly (e) (-> (-set e) Univ (-set e)))]

[set-union (-poly (e) (->* (list (-set e)) (-set e) (-set e)))]
[set-intersect (-poly (a b) (->* (list (-set a)) (-set b) (-set a)))]
[set-subtract (-poly (a b) (->* (list (-set a)) (-set b) (-set a)))]
[set-symmetric-difference (-poly (e) (->* (list (-set e)) (-set e) (-set e)))]

[set=? (-poly (a b) (-> (-set a) (-set b) B))]

[subset? (-poly (e) (-> (-set e) (-set e) B))]
[proper-subset? (-poly (e) (-> (-set e) (-set e) B))]
[set-map (-poly (e b) (-> (-set e) (-> e b) (-lst b)))]
[set-for-each (-poly (e b) (-> (-set e) (-> e b) -Void))]
[generic-set? (asym-pred Univ B (-FS -top (-not-filter (-set Univ) 0)))]
[set? (make-pred-ty (-set Univ))]
[set-equal? (-poly (e) (-> (-set e) B))]
[set-eqv? (-poly (e) (-> (-set e) B))]
[set-eq? (-poly (e) (-> (-set e) B))]

[list->set    (-poly (e) (-> (-lst e) (-set e)))]
[list->seteq  (-poly (e) (-> (-lst e) (-set e)))]
[list->seteqv (-poly (e) (-> (-lst e) (-set e)))]
[set->list (-poly (e) (-> (-set e) (-lst e)))]

;; Section 4.17 (Procedures)
[procedure? (make-pred-ty top-func)]
[compose (-poly (a b c) (-> (-> b c) (-> a b) (-> a c)))]
[compose1 (-poly (a b c) (-> (-> b c) (-> a b) (-> a c)))]
[procedure-rename (-> top-func -Symbol top-func)]
[procedure->method (-> top-func top-func)]
[procedure-closure-contents-eq? (-> top-func top-func -Boolean)]
;; keyword-apply - hard to give a type
[procedure-arity (-> top-func (Un -Nat -Arity-At-Least (-lst (Un -Nat -Arity-At-Least))))]
[procedure-arity? (make-pred-ty (Un -Nat -Arity-At-Least (-lst (Un -Nat -Arity-At-Least))))]
[procedure-arity-includes? (->opt top-func -Nat [Univ] B)]
[procedure-reduce-arity (-> top-func (Un -Nat -Arity-At-Least (-lst (Un -Nat -Arity-At-Least))) top-func)]
[procedure-keywords (-> top-func (-values (list (-lst -Keyword) (-opt (-lst -Keyword)))))]

[apply        (-poly (a b) (((list) a . ->* . b) (-lst a) . -> . b))]
[new-apply-proc (-poly (a b) (((list) a . ->* . b) (-lst a) . -> . b))]
[kernel:apply (-poly (a b) (((list) a . ->* . b) (-lst a) . -> . b))]
[time-apply
 (-polydots (b a) (cl->*
                   (-> (-> b) -Null (-values (list (-lst* b) -Nat -Nat -Nat)))
                   (-> (->... '() (a a) b)
                       (make-ListDots a 'a)
                       (-values (list (-lst* b) -Nat -Nat -Nat)))))]

;; Section 4.17.3 (racket/function)
[identity (-poly (a) (->acc (list a) a null))]
[const (-poly (a) (-> a (->* '() Univ a)))]
[negate (-polydots (a b c d)
          (cl->* (-> (-> c Univ : (-FS (-filter a 0) (-not-filter b 0)))
                     (-> c -Boolean : (-FS (-not-filter b 0) (-filter a 0))))
                 (-> (-> c Univ : (-FS (-filter a 0) (-filter b 0)))
                     (-> c -Boolean : (-FS (-filter b 0) (-filter a 0))))
                 (-> (-> c Univ : (-FS (-not-filter a 0) (-filter b 0)))
                     (-> c -Boolean : (-FS (-filter b 0) (-not-filter a 0))))
                 (-> (-> c Univ : (-FS (-not-filter a 0) (-not-filter b 0)))
                     (-> c -Boolean : (-FS (-not-filter b 0) (-not-filter a 0))))
                 (-> ((list) [d d] . ->... . Univ)
                     ((list) [d d] . ->... . -Boolean))))]
;; probably the most useful cases
;; doesn't cover cases where we pass multiple of the function's arguments to curry,
;; also doesn't express that the returned function is itself curried
[curry (-polydots (a c b)
                  (cl->* ((->... (list a) (b b) c) a . -> . (->... '() (b b) c))
                         ((->... (list a) (b b) c) . -> . (a . -> . (->... '() (b b) c)))))]
(primitive? (-> Univ B))
(primitive-closure? (-> Univ B))

;; Section 4.18 (Void and Undefined)
[void (->* '() Univ -Void)]
[void? (make-pred-ty -Void)]

;; Section 5.2 (Structure Types)
[make-struct-type
 (->opt -Symbol
        (-opt (make-StructTypeTop))
        -Nat -Nat
        [Univ
         (-lst (-pair -Struct-Type-Property Univ))
         (Un -Inspector (-val #f) (-val 'prefab))
         (Un top-func (-val #f) -Nat)
         (-lst -Nat)
         (-opt top-func)
         (-opt -Symbol)]
        (-values (list (make-StructTypeTop) top-func top-func top-func top-func)))]
[make-struct-field-accessor (->opt top-func -Nat [(-opt -Symbol)] top-func)]
[make-struct-field-mutator  (->opt top-func -Nat [(-opt -Symbol)] top-func)]

;; Section 5.3 (Structure Type Properties)
[make-struct-type-property
 (->opt Sym
       [(Un (one-of/c #f 'can-impersonate) (-> Univ (-lst Univ)))
        (-lst (-pair -Struct-Type-Property (-> Univ Univ)))
        Univ]
       (-values (list -Struct-Type-Property (-> Univ B) (-> Univ Univ))))]

[struct-type-property? (make-pred-ty -Struct-Type-Property)]
[struct-type-property-accessor-procedure? (-> Univ B)]

;; Section 5.6 (Structure Utilities)
[struct->vector (Univ . -> . (-vec Univ))]
[struct? (-> Univ -Boolean)]
[struct-type? (make-pred-ty (make-StructTypeTop))]

;; Section 6.2 (Classes)
[object% (-class)]

;; Section 6.11 (Object, Class, and Interface Utilities)
[object? (make-pred-ty (-object))]
[class? (make-pred-ty (make-ClassTop))]
;; TODO: interface?
;;       generic?
[object=? (-> (-object) (-object) -Boolean)]
[object->vector (->opt (-object) [Univ] (-vec Univ))]
;; TODO: class->interface
;;       object-interface
[is-a? (-> Univ (make-ClassTop) -Boolean)]
[subclass? (-> Univ (make-ClassTop) -Boolean)]
;; TODO: implementation?
;;       interface-extension?
;;       method-in-interface?
;;       interface->method-names
[object-method-arity-includes? (-> (-object) -Symbol -Nat -Boolean)]
[field-names (-> (-object) (-lst -Symbol))]
[object-info (-> (-object) (-values (list (Un (make-ClassTop) (-val #f)) -Boolean)))]
;; TODO: class-info (is this sound to allow?)

;; Section 9.1
[exn:misc:match? (-> Univ B)]
;; this is a hack
[match:error ((list) Univ . ->* . (Un))]
;[match:error (Univ . -> . (Un))]
[match-equality-test (-Param (Univ Univ . -> . Univ) (Univ Univ . -> . Univ))]
[matchable? (make-pred-ty (Un -String -Bytes))]

;; Section 10.1
[values (-polydots (a b) (cl->*
                           (-> (-values null))
                           (->acc (list a) a null)
                           ((list a) (b b) . ->... . (make-ValuesDots (list (-result a)) b 'b))))]
[call-with-values
  (-polydots (b a)
    (cl->*
     ((-> (make-ValuesDots null a 'a)) (null (a a) . ->... . b) . -> .  b)
     ((-> ManyUniv) ((list) Univ . ->* . b) . -> . b)))]

;; Section 10.2
[raise (cl->* ((Un -Flat -Exn) . -> . (Un))
              ((Un -Flat -Exn) Univ . -> . (Un)))]
[error
 (cl->* (-> Sym (Un))
        (->* (list -String) Univ (Un))
        (->* (list Sym -String) Univ (Un)))]
[raise-user-error
 (cl->* (-> Sym (Un))
        (->* (list -String) Univ (Un))
        (->* (list Sym -String) Univ (Un)))]

;raise-type-error (in index)
[raise-mismatch-error (-> Sym -String Univ (Un))]
;raise-arity-error
[raise-syntax-error (->opt (-opt Sym) -String [Univ Univ (-lst (-Syntax Univ))] (Un))]

[call-with-exception-handler (-poly (a) (-> (-> Univ a) (-> a) a))]
[uncaught-exception-handler (-Param (-> Univ ManyUniv) (-> Univ ManyUniv))]

[error-escape-handler (-Param (-> ManyUniv) (-> ManyUniv))]
[error-display-handler (-Param (-> -String Univ ManyUniv) (-> -String Univ ManyUniv))]
[error-value->string-handler (-Param (-> Univ -Nat -String) (-> Univ -Nat -String))]
[error-print-context-length (-Param -Nat -Nat)]
[error-print-width (-Param -Nat -Nat)]
[error-print-source-location (-Param Univ B)]

;; Section 10.2.5
[prop:exn:srclocs -Struct-Type-Property]
[exn:srclocs? (-> Univ B)]
[exn:srclocs-accessor (-> Univ (-lst Univ))] ;TODO

;; Section 10.3 (Delayed Evaluation)
[promise? (make-pred-ty (-Promise Univ))]
[force (-poly (a) (->acc (list (-Promise a)) a (list -force)))]
[promise-forced? (-poly (a) (-> (-Promise a) B))]
[promise-running? (-poly (a) (-> (-Promise a) B))]

;; Section 10.4 (Continuations)
[call-with-continuation-prompt
 (-polydots (a b d c)
   (cl->*
    (-> (-> b) (make-Prompt-Tagof b (-> (-> d) d)) (Un b d))
    (-> (-> b) (make-Prompt-Tagof b (->... '() (c c) d)) (->... '() (c c) d)
        (Un b d))
    (-> (-> b) Univ)))]
[abort-current-continuation
 (-polydots (a b d e c)
   (cl->*
    (->... (list (make-Prompt-Tagof b (->... '() (c c) d))) (c c) e)
    (->... (list (make-Prompt-Tagof b (->... '() (c c) ManyUniv))) (c c) e)))]
[make-continuation-prompt-tag
 (-poly (a b) (->opt [Sym] (make-Prompt-Tagof a b)))]
;; default-continuation-prompt-tag is defined in "base-contracted.rkt"
[call-with-current-continuation
 (-polydots (a b c)
   (cl->* (-> (-> (-> (Un)) (-values null)) (-values null))
          (-> (-> (->... (list a) (c c) (Un))
                  (make-ValuesDots (list (-result b)) c 'c))
              (make-ValuesDots (list (-result (Un a b))) c 'c))))]
[call/cc
 (-polydots (a b c)
   (cl->* (-> (-> (-> (Un)) (-values null)) (-values null))
          (-> (-> (->... (list a) (c c) (Un))
                  (make-ValuesDots (list (-result b)) c 'c))
              (make-ValuesDots (list (-result (Un a b))) c 'c))))]
[call-with-composable-continuation
 (-polydots (b c a)
   (-> ;; takes a continuation and should return the same
       ;; type as the continuation's input type
       (-> (->... '() (a a) b) (make-ValuesDots '() a 'a))
       (make-Prompt-Tagof b c)
       ;; the continuation's input is the same as the
       ;; return type here
       (make-ValuesDots '() a 'a)))]
[call-with-escape-continuation
 (-polydots (a b c)
   (cl->* (-> (-> (-> (Un)) (-values null)) (-values null))
          (-> (-> (->... (list a) (c c) (Un))
                  (make-ValuesDots (list (-result b)) c 'c))
              (make-ValuesDots (list (-result (Un a b))) c 'c))))]
[call/ec
 (-polydots (a b c)
   (cl->* (-> (-> (-> (Un)) (-values null)) (-values null))
          (-> (-> (->... (list a) (c c) (Un))
                  (make-ValuesDots (list (-result b)) c 'c))
              (make-ValuesDots (list (-result (Un a b))) c 'c))))]
[call-with-continuation-barrier (-poly (a) (-> (-> a) a))]
[continuation-prompt-available? (-> (make-Prompt-TagTop) B)]
[continuation?
 (asym-pred Univ B (-FS (-filter top-func 0) -top))]
[continuation-prompt-tag? (make-pred-ty (make-Prompt-TagTop))]
[dynamic-wind (-poly (a) (-> (-> ManyUniv) (-> a) (-> ManyUniv) a))]

;; Section 10.5 (Continuation Marks)
;; continuation-marks needs type for continuations as other
;; possible first argument
[continuation-marks
 (->opt (Un (-val #f) -Thread) [(make-Prompt-TagTop)] -Cont-Mark-Set)]
[current-continuation-marks (->opt [(make-Prompt-TagTop)] -Cont-Mark-Set)]
[continuation-mark-set->list
 (-poly (a)
        (cl->*
         (->opt -Cont-Mark-Set (make-Continuation-Mark-Keyof a)
                [(make-Prompt-TagTop)] (-lst a))
         (->opt -Cont-Mark-Set Univ [(make-Prompt-TagTop)] (-lst Univ))))]
[continuation-mark-set->list*
 (-poly (a b)
        (cl->*
         (->opt -Cont-Mark-Set
                (-lst (make-Continuation-Mark-Keyof a))
                [b (make-Prompt-TagTop)]
                (-lst (-vec (Un a b))))
         (->opt -Cont-Mark-Set
                (-lst Univ)
                [Univ (make-Prompt-TagTop)]
                (-lst (-vec Univ)))))]
[continuation-mark-set-first
 (-poly (a b)
        (cl->*
         (-> (-opt -Cont-Mark-Set) (make-Continuation-Mark-Keyof a)
             (-opt a))
         (->opt (-opt -Cont-Mark-Set) (make-Continuation-Mark-Keyof a)
                [b (make-Prompt-TagTop)]
                (Un a b))
         (->opt (-opt -Cont-Mark-Set) Univ [Univ (make-Prompt-TagTop)] Univ)))]
[call-with-immediate-continuation-mark
 (-poly (a) (->opt Univ (-> Univ a) [Univ] a))]
[continuation-mark-key? (make-pred-ty (make-Continuation-Mark-KeyTop))]
[continuation-mark-set? (make-pred-ty -Cont-Mark-Set)]
[make-continuation-mark-key
 (-poly (a) (->opt [-Symbol] (make-Continuation-Mark-Keyof a)))]
[continuation-mark-set->context
 (-> -Cont-Mark-Set (-lst (-pair (-opt Sym) Univ)))] ;TODO add srcloc

;; Section 10.6
[break-enabled (cl->* (-> B) (-> B -Void))]

;; Section 10.7 (Exiting)
[exit (->opt [Univ] (Un))]
[exit-handler (-Param (-> Univ ManyUniv) (-> Univ ManyUniv))]
[executable-yield-handler (-Param (-> -Byte ManyUniv) (-> -Byte ManyUniv))]

;; Section 11.1 (Threads)

;; Section 11.1.1
[thread (-> (-> Univ) -Thread)]
[thread? (make-pred-ty -Thread)]
[current-thread (-> -Thread)]
[thread/suspend-to-kill (-> (-> Univ) -Thread)]
[call-in-nested-thread (-poly (a) (->opt (-> a) [-Custodian] a))]

;; Section 11.1.2
[thread-suspend (-Thread . -> . -Void)]
[thread-resume (->opt -Thread [(Un (-val #f) -Thread -Custodian)] -Void)]
[kill-thread (-Thread . -> . -Void)]
[break-thread (-Thread . -> . -Void)]
[sleep ([N] . ->opt . -Void)]
[thread-running? (-Thread . -> . B)]
[thread-dead? (-Thread . -> . B)]

;; Section 11.1.3
[thread-wait (-Thread . -> . -Void)]
[thread-dead-evt (-> -Thread (-mu x (-evt x)))]
[thread-resume-evt (-> -Thread (-mu x (-evt x)))]
[thread-suspend-evt (-> -Thread (-mu x (-evt x)))]

;; Section 11.1.4
[thread-send
 (-poly (a) (cl->* (-> -Thread Univ -Void)
                   (-> -Thread Univ (-val #f) (-opt -Void))
                   (-> -Thread Univ (-> a) (Un -Void a))))]
[thread-receive (-> Univ)]
[thread-try-receive (-> Univ)]
[thread-receive-evt (-> (-mu x (-evt x)))]
[thread-rewind-receive (-> (-lst Univ) -Void)]

;; Section 11.2.1
[evt? (make-pred-ty (-evt Univ))]
[sync (-poly (a) (->* '() (-evt a) a))]
[sync/timeout
 (-poly (a b)
   (cl->*
    (->* (list (-val #f)) (-evt a) a)
    (->* (list -NonNegReal) (-evt a) (-opt a))
    (->* (list (-> b)) (-evt a) (Un a b))))]
[sync/enable-break (-poly (a) (->* '() (-evt a) a))]
[sync/timeout/enable-break
 (-poly (a b)
   (cl->*
    (->* (list (-val #f)) (-evt a) a)
    (->* (list -NonNegReal) (-evt a) (-opt a))
    (->* (list (-> b)) (-evt a) (Un a b))))]
[choice-evt (-poly (a) (->* '() (-evt a) (-evt a)))]
[wrap-evt (-poly (a b) (-> (-evt a) (-> a b) (-evt b)))]
[handle-evt (-poly (a b) (-> (-evt a) (-> a b) (-evt b)))]
[guard-evt (-poly (a) (-> (-> (-evt a)) (-evt a)))]
[nack-guard-evt
 (-poly (a)
   (-> (-> (-evt -Void) (-evt a))
       (-evt a)))]
[poll-guard-evt
 (-poly (a) (-> (-> -Boolean (-evt a)) (-evt a)))]
[always-evt (-mu x (-evt x))]
[replace-evt (-poly (a b)
               (cl->*
                 (-> (-evt a) (-> a (-evt b)) (-evt b))
                 (-> (-evt a) (-> a b) (-mu x (-evt x)))))]
[never-evt (-evt (Un))]
[system-idle-evt (-> (-evt -Void))]
[alarm-evt (-> -Real (-mu x (-evt x)))]
[handle-evt? (asym-pred Univ B (-FS (-filter (-evt Univ) 0) -top))]
[current-evt-pseudo-random-generator
 (-Param -Pseudo-Random-Generator -Pseudo-Random-Generator)]

;; Section 11.2.2
[make-channel (-poly (a) (-> (-channel a)))]
[channel? (make-pred-ty (make-ChannelTop))]
[channel-get (-poly (a) ((-channel a) . -> . a))]
[channel-try-get (-poly (a) ((-channel a) . -> . (Un a (-val #f))))]
[channel-put (-poly (a) ((-channel a) a . -> . -Void))]
[channel-put-evt (-poly (a) (-> (-channel a) a (-mu x (-evt x))))]
[channel-put-evt? (asym-pred Univ B (-FS (-filter (-mu x (-evt x)) 0) -top))]

;; Section 11.2.3 (Semaphores)
[semaphore? (make-pred-ty -Semaphore)]
[make-semaphore (->opt [-Nat] -Semaphore)]
[semaphore-post (-> -Semaphore -Void)]
[semaphore-wait (-> -Semaphore -Void)]
[semaphore-try-wait? (-> -Semaphore B)]
[semaphore-wait/enable-break (-> -Semaphore -Void)]
[semaphore-peek-evt (-> -Semaphore (-mu x (-evt x)))]
[semaphore-peek-evt? (asym-pred Univ B (-FS (-filter (-mu x (-evt x)) 0) -top))]
;[call-with-semaphore ???]
;[call-with-semaphore/enable-break ???]

;; Section 11.3.1 (Thread Cells)
[thread-cell? (make-pred-ty (make-ThreadCellTop))]
[make-thread-cell (-poly (a) (->opt a [Univ] (-thread-cell a)))]
[thread-cell-ref (-poly (a) (-> (-thread-cell a) a))]
[thread-cell-set! (-poly (a) (-> (-thread-cell a) a -Void))]
[current-preserved-thread-cell-values
 (cl->* (-> Univ) (-> Univ -Void))]

;; Section 11.3.3 (Parameters)

;hidden parameter bindings
[parameterization-key Sym]
[extend-parameterization (-poly (a b) (-> Univ (-Param a b) a Univ))]

[make-parameter (-poly (a b) (cl-> [(a) (-Param a a)]
                                   [(b (a . -> . b)) (-Param a b)]))]
[make-derived-parameter (-poly (a b c d) (-> (-Param a b) (-> c a) (-> b d) (-Param c d)))]
[parameter? (make-pred-ty (-Param -Bottom Univ))]
[parameter-procedure=? (-poly (a b c d) (-> (-Param a b) (-Param c d) B))]

[current-parameterization (-> -Parameterization)]
[call-with-parameterization (-poly (a) (-> -Parameterization (-> a) a))]
[parameterization? (make-pred-ty -Parameterization)]

;; Section 11.4 (Futures)
[future (-poly (A) ((-> A) . -> . (-future A)))]
[touch (-poly (A) ((-future A) . -> . A))]
[futures-enabled? (-> -Boolean)]
[current-future (-> (-opt (-future Univ)))]
[future? (make-pred-ty (-future Univ))]
[would-be-future (-poly (A) ((-> A) . -> . (-future A)))]
[processor-count (-> -PosInt)]

;; Section 11.5 (Places)
[place-enabled? (-> -Boolean)]
[place? (make-pred-ty -Place)]
[place-channel? (make-pred-ty -Place-Channel)]
;; FIXME: the `#:at` keyword is for remote places, not supported yet
[dynamic-place (->key (Un -Module-Path -Path) Sym
                      #:at (-val #f) #f
                      #:named (Un (-val #f) -Symbol) #f
                      -Place)]
[dynamic-place* (->key (Un -Module-Path -Path) Sym
                       #:in (-opt -Input-Port) #f
                       #:out (-opt -Output-Port) #f
                       #:err (-opt -Output-Port) #f
                      (-values (list -Place
                                     (-opt -Output-Port)
                                     (-opt -Input-Port)
                                     (-opt -Input-Port))))]
[place-wait (-> -Place -Int)]
[place-dead-evt (-> -Place (-mu x (-evt x)))]
[place-break (->opt -Place [(-opt (one-of/c 'hang-up 'terminate))] -Void)]
[place-kill (-> -Place -Void)]
[place-channel (-> (-values (list -Place-Channel -Place-Channel)))]
[place-channel-put (-> -Place-Channel Univ -Void)]
[place-channel-get (-> -Place-Channel Univ)]
[place-channel-put/get (-> -Place-Channel Univ Univ)]
[place-message-allowed? (-> Univ -Boolean)]

;; Section 12 (Macros)

;; Section 12.2
[syntax? (make-pred-ty (-Syntax Univ))]

[syntax-source (-> (-Syntax Univ) Univ)]
[syntax-line (-> (-Syntax Univ) (-opt -PosInt))]
[syntax-column (-> (-Syntax Univ) (-opt -Nat))]
[syntax-position (-> (-Syntax Univ) (-opt -PosInt))]
[syntax-span (-> (-Syntax Univ) (-opt -Nat))]

[syntax-original? (-poly (a) (-> (-Syntax a) B))]
[syntax-source-module (->opt (-Syntax Univ) [Univ] (Un (-val #f) -Path Sym -Module-Path-Index))]
[syntax-e (-poly (a) (->acc (list (-Syntax a)) a (list -syntax-e)))]
[syntax->list (-poly (a) (-> (-Syntax (-lst a)) (-lst a)))]
[syntax->datum (cl->* (-> Any-Syntax -Sexp)
                      (-> (-Syntax Univ) Univ))]

[datum->syntax
 (let* ([Pre Syntax-Sexp]
        [I (-Syntax Sym)]
        [A Any-Syntax]
        [S (-Syntax Univ)]
        [ctxt (-opt S)]
        [srclist (-lst*
                   Univ
                   (-opt -Integer)
                   (-opt -Integer)
                   (-opt -Integer)
                   (-opt -Integer))]
        [srcvec (make-HeterogeneousVector (list
                                           Univ
                                           (-opt -Integer)
                                           (-opt -Integer)
                                           (-opt -Integer)
                                           (-opt -Integer)))]
        [srcloc (Un S (-val #f) srclist srcvec)]
        [prop (-opt S)]
        [cert (-opt S)])
   (cl->*
    (->opt ctxt Sym  [srcloc prop cert] I)
    (->opt ctxt Pre  [srcloc prop cert] A)
    (->opt ctxt Univ [srcloc prop cert] S)))]

[identifier? (make-pred-ty (-Syntax Sym))]

[generate-temporaries (-> (Un (-Syntax (-lst Univ)) (-lst Univ)) (-lst (-Syntax Sym)))]
[identifier-prune-lexical-context (->opt (-Syntax Sym) [(-lst Sym)] (-Syntax Sym))]
[identifier-prune-to-source-module (-> (-Syntax Sym) (-Syntax Sym))]

;; Section 12.3
[bound-identifier=? (Ident Ident [(-opt -Integer)] . ->opt . B)]

[free-identifier=? (Ident Ident [(-opt -Integer)] . ->opt . B)]
[free-label-identifier=? (Ident Ident . -> . B)]
[free-transformer-identifier=? (Ident Ident . -> . B)]
[free-template-identifier=? (Ident Ident . -> . B)]

[check-duplicate-identifier ((-lst (-Syntax Sym)) . -> . (-opt (-Syntax Sym)))]

[identifier-binding
 (Ident [(-opt -Integer)]. ->opt .
  (Un (-val 'lexical) (-val #f)
   (-lst* -Module-Path-Index
          -Symbol
          -Module-Path-Index
          -Symbol
          -Nat
          (-opt -Integer)
          (-opt -Integer))))]
[identifier-transformer-binding
 (Ident . -> .
  (Un (-val 'lexical) (-val #f)
   (-lst* -Module-Path-Index
          -Symbol
          -Module-Path-Index
          -Symbol
          -Nat
          (-opt -Integer)
          (-opt -Integer))))]
[identifier-template-binding
 (Ident . -> .
  (Un (-val 'lexical) (-val #f)
   (-lst* -Module-Path-Index
          -Symbol
          -Module-Path-Index
          -Symbol
          -Nat
          (-opt -Integer)
          (-opt -Integer))))]
[identifier-label-binding
 (Ident . -> .
  (Un (-val 'lexical) (-val #f)
   (-lst* -Module-Path-Index
          -Symbol
          -Module-Path-Index
          -Symbol
          -Nat
          (-opt -Integer)
          (-opt -Integer))))]

;; Section 12.4
[set!-transformer? (-> Univ B)]
[make-set!-transformer (-> (-> (-Syntax Univ) (-Syntax Univ)) Univ)]
[set!-transformer-procedure (-> Univ (-> (-Syntax Univ) (-Syntax Univ)))]
[prop:set!-transformer -Struct-Type-Property]

[rename-transformer? (-> Univ B)]
[make-rename-transformer (->opt (-Syntax Sym) [(-> (-Syntax Sym) (-Syntax Sym))] Univ)]
[rename-transformer-target (-> Univ (-Syntax Sym))]
[prop:rename-transformer -Struct-Type-Property]

[local-expand
 (->opt (-Syntax Univ)
        (Un (-val 'expression)
            (-val 'top-level)
            (-val 'module)
            (-val 'module-begin)
            (-lst Univ))
        (-opt (-lst (-Syntax Sym)))
        [(Un -Internal-Definition-Context (-pair -Internal-Definition-Context (-lst -Internal-Definition-Context)) (-val #f))]
        (-Syntax Univ))]

[syntax-local-expand-expression (-> (-Syntax Univ) (-values (list (-Syntax Univ) (-Syntax Univ))))]

[local-expand/capture-lifts
 (->opt (-Syntax Univ)
        (Un (-val 'expression)
            (-val 'top-level)
            (-val 'module)
            (-val 'module-begin)
            (-lst Univ))
        (-opt (-lst (-Syntax Sym)))
        [(Un -Internal-Definition-Context (-pair -Internal-Definition-Context (-lst -Internal-Definition-Context)) (-val #f))
         Univ]
        (-Syntax Univ))]

[local-transformer-expand
 (->opt (-Syntax Univ)
        (Un (-val 'expression)
            (-val 'top-level)
            (-val 'module)
            (-val 'module-begin)
            (-lst Univ))
        (-opt (-lst (-Syntax Sym)))
        [(Un -Internal-Definition-Context (-pair -Internal-Definition-Context (-lst -Internal-Definition-Context)) (-val #f))]
        (-Syntax Univ))]

[local-transformer-expand/capture-lifts
 (->opt (-Syntax Univ)
        (Un (-val 'expression)
            (-val 'top-level)
            (-val 'module)
            (-val 'module-begin)
            (-lst Univ))
        (-opt (-lst (-Syntax Sym)))
        [(Un -Internal-Definition-Context (-pair -Internal-Definition-Context (-lst -Internal-Definition-Context)) (-val #f))
         Univ]
        (-Syntax Univ))]

[internal-definition-context? (make-pred-ty -Internal-Definition-Context)]
[syntax-local-make-definition-context (->opt [(-opt -Internal-Definition-Context)] -Internal-Definition-Context)]
[syntax-local-bind-syntaxes (-> (-lst (-Syntax Sym)) (-opt (-Syntax Univ)) -Internal-Definition-Context -Void)]
[internal-definition-context-seal (-> -Internal-Definition-Context -Void)]
[identifier-remove-from-definition-context (-> (-Syntax Sym) (Un -Internal-Definition-Context (-lst -Internal-Definition-Context)) (-Syntax Sym))]

[syntax-local-value (->opt (-Syntax Sym) [(-opt (-> Univ)) (-opt -Internal-Definition-Context)] Univ)]
[syntax-local-value/immediate (->opt (-Syntax Sym) [(-opt (-> (-values (list Univ Univ)))) (-opt -Internal-Definition-Context)]
                                     (-values (list Univ Univ)))]
[syntax-local-lift-expression (-> (-Syntax Univ) (-Syntax Sym))]
[syntax-local-lift-values-expression (-> -Nat (-Syntax Univ) (-lst (-Syntax Sym)))]
[syntax-local-lift-context (-> Univ)]
[syntax-local-lift-module-end-declaration (-> (-Syntax Univ) -Void)]
[syntax-local-lift-require (-poly (a) (-> Univ (-Syntax a) (-Syntax a)))]
[syntax-local-lift-provide (-> Univ -Void)]
[syntax-local-name (-> Univ)]
[syntax-local-context (-> (Un (-val 'expression) (-val 'top-level) (-val 'module) (-val 'module-begin) (-lst Univ)))]
[syntax-local-phase-level (-> -Int)]
[syntax-local-module-exports (-> -Module-Path (-values (list (-lst Sym) (-lst Sym) (-lst Sym))))]
[syntax-local-get-shadower (-> (-Syntax Sym) (-Syntax Sym))]
[syntax-local-certifier (->opt [B] (-poly (a) (->opt (-Syntax a) [Univ (-opt (-poly (b) (-> (-Syntax b) (-Syntax b))))] (-Syntax a))))]
[syntax-transforming? (-> B)]

[syntax-local-introduce (-poly (a) (-> (-Syntax a) (-Syntax a)))]
[make-syntax-introducer (-> (-poly (a) (-> (-Syntax a) (-Syntax a))))]
[make-syntax-delta-introducer (->opt (-Syntax Univ) [(-opt (-Syntax Univ)) (-opt -Int)] (-poly (a) (-> (-Syntax a) (-Syntax a))))]
[syntax-local-make-delta-introducer (-> (-Syntax Sym) (-> (-Syntax Sym) (-Syntax Sym)))]

[syntax-local-transforming-module-provides? (-> B)]
[syntax-local-module-defined-identifiers (-> (-HT (Un (-val #f) -Int) (-lst (-Syntax Sym))))]
[syntax-local-module-required-identifiers (-> (-opt -Module-Path) (Un B -Int) (-lst (-pair (-opt -Int) (-lst (-Syntax Sym)))))]

;; Section 12.5

;; Section 12.6

;; Section 12.7
[syntax-property (-poly (a) (cl->* (-> (-Syntax a) Univ Univ (-Syntax a))
                                   (-> (-Syntax Univ) Univ Univ)))]
[syntax-property-symbol-keys (-> (-Syntax Univ) (-lst Sym))]
[syntax-track-origin (-poly (a) (-> (-Syntax a) (-Syntax Univ) (-Syntax Univ) (-Syntax a)))]

;; Section 12.8
[syntax-recertify (-poly (a) (-> (-Syntax a) (-Syntax Univ) -Inspector Univ (-Syntax a)))]

;; Section 12.9
[expand (-> Univ (-Syntax Univ))]
[expand-syntax (-> (-Syntax Univ) (-Syntax Univ))]
[expand-once (-> Univ (-Syntax Univ))]
[expand-syntax-once (-> (-Syntax Univ) (-Syntax Univ))]
[expand-to-top-form (-> Univ (-Syntax Univ))]
[expand-syntax-to-top-form (-> (-Syntax Univ) (-Syntax Univ))]

;; Section 13 (Input and Output)

;; Section 13.1 (Ports)

;; Section 13.1.1
[current-locale (-Param -String -String)]

;; Section 13.1.2
[input-port? (make-pred-ty -Input-Port)]
[output-port? (make-pred-ty -Output-Port)]
[port? (make-pred-ty -Port)]

[close-input-port (-> -Input-Port -Void)]
[close-output-port (-> -Output-Port -Void)]

[port-closed? (-> -Port B)]
[port-closed-evt (-> -Port (-mu x (-evt x)))]

[current-input-port (-Param -Input-Port -Input-Port)]
[current-output-port (-Param -Output-Port -Output-Port)]
[current-error-port (-Param -Output-Port -Output-Port)]

[file-stream-port? (-> Univ B)]
[terminal-port? (-> Univ B)]

[eof (-val eof)]
[eof-object? (make-pred-ty (-val eof))]

;; Section 13.1.3
[flush-output (->opt [-Output-Port] -Void)]
[file-stream-buffer-mode (cl-> [(-Port) (one-of/c 'none 'line 'block #f)]
                               [(-Port (one-of/c 'none 'line 'block)) -Void])]
[file-position (cl-> [(-Port) -Nat]
                     [(-Port -Integer) -Void])]
[file-position* (-> -Port (Un -Nat (-val #f)))]

;; Section 13.1.4
[port-count-lines! (-> (Un -Input-Port -Output-Port) -Void)]
[port-next-location (-> (Un -Input-Port -Output-Port) (-values (list (-opt -PosInt) (-opt -Nat) (-opt -PosInt))))]
[port-count-lines-enabled (-Param Univ B)]

;; Section 13.1.5
#|
[open-input-file (->key -Pathlike #:mode (one-of/c 'binary 'text) #f -Input-Port)]

[open-output-file
 (->key -Pathlike
        #:mode (one-of/c 'binary 'text) #f
        #:exists (one-of/c 'error 'append 'update 'can-update
                           'replace 'truncate
                           'must-truncate 'truncate/replace)
        #f
        -Output-Port)]
[open-input-output-file
 (->key -Pathlike
        #:mode (one-of/c 'binary 'text) #f
        #:exists (one-of/c 'error 'append 'update 'can-update
                           'replace 'truncate
                           'must-truncate 'truncate/replace)
        #f
        (-values (list -Input-Port -Output-Port)))]


[call-with-input-file (-poly (a) (-Pathlike (-Input-Port . -> . a) #:mode (Un (-val 'binary) (-val 'text)) #f . ->key .  a))]
[call-with-output-file (-poly (a) (-Pathlike (-Output-Port . -> . a)
                                   #:exists (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace) #f
                                   #:mode (one-of/c 'binary 'text) #f
                                   . ->key .  a))]

[call-with-input-file* (-poly (a) (-Pathlike (-Input-Port . -> . a) #:mode (Un (-val 'binary) (-val 'text)) #f . ->key .  a))]
[call-with-output-file* (-poly (a) (-Pathlike (-Output-Port . -> . a)
                                   #:exists (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace) #f
                                   #:mode (one-of/c 'binary 'text) #f
                                   . ->key .  a))]

[with-input-from-file
 (-poly (a) (->key -Pathlike (-> a) #:mode (one-of/c 'binary 'text) #f a))]
[with-output-to-file
 (-poly (a) (->key -Pathlike (-> a)
                   #:exists (one-of/c 'error 'append 'update 'can-update
                           'replace 'truncate
                           'must-truncate 'truncate/replace)
                   #f
                   #:mode (one-of/c 'binary 'text) #f
                   a))]
|#

[port-try-file-lock? (-> (Un -Input-Port -Output-Port) (one-of/c 'shared 'exclusive) B)]
[port-file-unlock (-> (Un -Input-Port -Output-Port) -Void)]
[port-file-identity (-> (Un -Input-Port -Output-Port) -PosInt)]

;; Section 13.1.6
[open-input-string (-> -String -Input-Port)]
[open-input-bytes (-> -Bytes -Input-Port)]
[open-output-string
 ([Univ] . ->opt . -Output-Port)]
[open-output-bytes
 ([Univ] . ->opt . -Output-Port)]

;FIXME
;These should be fixed to only accept output-ports generated by open-output-{string,bytes}
[get-output-bytes (-Output-Port [Univ N N] . ->opt . -Bytes)]
[get-output-string (-> -Output-Port -String)]

;; Section 13.1.7
[make-pipe
 (cl->* [->opt [N] (-values (list -Input-Port -Output-Port))])]
[pipe-content-length (-> (Un -Input-Port -Output-Port) -Nat)]

;; Section 13.1.8
[prop:input-port -Struct-Type-Property]
[prop:output-port -Struct-Type-Property]

;; Section 13.1.9
[make-input-port
 (->opt Univ
        (Un (-> -Bytes (Un -Nat (-val eof) top-func (-evt Univ)))
            -Input-Port)
        (Un (-> -Bytes -Nat (-opt (-evt Univ))
                (Un -Nat (-val eof) top-func (-evt Univ) (-val #f)))
            -Input-Port
            (-val #f))
        (-> Univ)
        [(-opt (-> (-evt Univ)))
         (-opt (-> -PosInt (-evt Univ) (-evt Univ) Univ))
         (-opt (-> (-values (list (-opt -Integer)
                                  (-opt -Integer)
                                  (-opt -Integer)))))
         (-> Univ)
         (Un -Integer -Port (-val #f) (-> (-opt -Integer)))
         (-opt (cl->* (-> (one-of/c 'block 'none) Univ)
                      (-> (-opt (one-of/c 'block 'none)))))]
        -Input-Port)]
[make-output-port
 (->opt Univ
        (-evt Univ)
        (Un (-> -Bytes -Nat -Nat -Boolean -Boolean
                (Un -Integer (-val #f) (-evt Univ)))
            -Output-Port)
        (-> Univ)
        [(-opt (Un -Output-Port (-> Univ -Boolean -Boolean Univ)))
         (-opt (-> -Bytes -Nat -Nat (-evt Univ)))
         (-opt (-> Univ (-evt Univ)))
         (-opt (-> (-values (list (-opt -Integer)
                                  (-opt -Integer)
                                  (-opt -Integer)))))
         (-> Univ)
         (Un -Integer -Port (-val #f) (-> (-opt -Integer)))
         (-opt (cl->* (-> (one-of/c 'block 'none) Univ)
                      (-> (-opt (one-of/c 'block 'none)))))]
        -Output-Port)]

;; Section 13.1.10

;; Section 13.1.10.1
[port->list
 (-poly (a) (cl->*
             (-> (-lst Univ))
             (->opt (-> -Input-Port a) [-Input-Port] (-lst a))))]
[port->string (->opt [-Input-Port] -String)]
[port->bytes (->opt [-Input-Port] -Bytes)]
#|
[port->lines
 (cl->*
  (->key #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -String))
  (->key -Input-Port #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -String)))]
[port->bytes-lines
 (cl->*
  (->key #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -Bytes))
  (->key -Input-Port #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -Bytes)))]


[display-lines (cl->*
 ((-lst Univ) #:separator Univ #f . ->key . -Void)
 ((-lst Univ) -Output-Port #:separator Univ #f . ->key . -Void))]
|#

[call-with-output-string (-> (-> -Output-Port ManyUniv) -String)]
[call-with-output-bytes (-> (-> -Output-Port ManyUniv) -Bytes)]

[with-output-to-string
  (-> (-> ManyUniv) -String)]
[with-output-to-bytes
  (-> (-> ManyUniv) -Bytes)]

[call-with-input-string
 (-polydots (a)
   (-> -String (-> -Input-Port (make-ValuesDots '() a 'a))
       (make-ValuesDots '() a 'a)))]
[call-with-input-bytes
 (-polydots (a)
   (-> -Bytes (-> -Input-Port (make-ValuesDots '() a 'a))
       (make-ValuesDots '() a 'a)))]

[with-input-from-string (-poly (a) (-> -String (-> a) a))]
[with-input-from-bytes (-poly (a) (-> -Bytes (-> a) a))]

;; Section 13.1.10.2
[input-port-append  (->* (list Univ) -Input-Port -Input-Port)]

;TODO write the type for this
;It is fairly complicated and require events
;make-input-port/read-to-peek

[make-limited-input-port (->opt -Input-Port -Nat [Univ] -Input-Port)]
[make-pipe-with-specials (->opt [-Nat Univ Univ] (-values (list -Input-Port -Output-Port)))]

[merge-input (->opt -Input-Port -Input-Port [(-opt -Nat)] -Input-Port)]
[open-output-nowhere (-> -Output-Port)]
[peeking-input-port (->opt -Input-Port [Univ -Nat] -Input-Port)]

[reencode-input-port
 (->opt -Input-Port -String (-opt -Bytes) [Univ Univ Univ (-> -String -Input-Port ManyUniv)] -Input-Port)]
[reencode-output-port
 (->opt -Output-Port -String (-opt -Bytes) [Univ Univ (-opt -Bytes) (-> -String -Output-Port ManyUniv)] -Output-Port)]

[dup-input-port (-Input-Port (B) . ->opt . -Input-Port)]
[dup-output-port (-Output-Port (B) . ->opt . -Input-Port)]

[relocate-input-port (->opt -Input-Port (-opt -PosInt) (-opt -Nat) -PosInt [Univ] -Input-Port)]
[relocate-output-port (->opt -Output-Port (-opt -PosInt) (-opt -Nat) -PosInt [Univ] -Output-Port)]

[transplant-input-port (->opt -Input-Port (-opt (-> (-values (list (-opt -PosInt) (-opt -Nat) (-opt -PosInt))))) -PosInt [Univ (-> ManyUniv)] -Input-Port)]
[transplant-output-port (->opt -Output-Port (-opt (-> (-values (list (-opt -PosInt) (-opt -Nat) (-opt -PosInt))))) -PosInt [Univ (-> ManyUniv)] -Output-Port)]

;; Section 13.1.10.3
[eof-evt (-> -Input-Port (-evt (-val eof)))]
[read-bytes-evt (-> -Nat -Input-Port (-evt (Un -Bytes (-val eof))))]
;read-bytes!-evt (need progress event support)
[read-bytes-avail!-evt
 (-> -Bytes -Input-Port (-evt (Un -Nat (-val eof))))]
[read-string-evt (-> -Nat -Input-Port (-evt (Un -String (-val eof))))]
[read-string!-evt (-> -String -Input-Port (-evt (Un -Nat (-val eof))))]
[read-line-evt (-> -Input-Port
                   (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)
                   (-evt (Un -String (-val eof))))]
[read-bytes-line-evt
 (-> -Input-Port
     (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)
     (-evt (Un -Bytes (-val eof))))]
;peek-bytes-evt (ditto progress event)
;peek-bytes!-evt
;peek-bytes-avail!-evt
;peek-string-evt
;peek-string!-evt
;regexp-match-evt

;; Section 13.1.10.4

[convert-stream (-> -String -Input-Port -String -Output-Port -Void)]
[copy-port (->* (list -Input-Port -Output-Port) -Output-Port -Void)]

;; Section 13.2
[read-char (->opt [-Input-Port] (Un -Char (-val eof)))]
[read-byte (->opt [-Input-Port] (Un -Byte (-val eof)))]

[read-line  (->opt [-Input-Port Sym] (Un -String (-val eof)))]
[read-bytes-line (->opt [-Input-Port Sym] (Un -Bytes (-val eof)))]

;read-string (in index)
;read-bytes (in index)

;read-string! (in index)
[read-bytes! (->opt -Bytes [-Input-Port -Nat -Nat] (Un -PosInt (-val eof)))]
[read-bytes-avail! (->opt -Bytes [-Input-Port -Nat -Nat] (Un -PosInt (-val eof) (-> (-opt -PosInt) (-opt -Nat) (-opt -PosInt) (-opt -Nat) Univ)))]
[read-bytes-avail!* (->opt -Bytes [-Input-Port -Nat -Nat] (Un -Nat (-val eof) (-> (-opt -PosInt) (-opt -Nat) (-opt -PosInt) (-opt -Nat) Univ)))]
[read-bytes-avail!/enable-break (->opt -Bytes [-Input-Port -Nat -Nat] (Un -PosInt (-val eof) (-> (-opt -PosInt) (-opt -Nat) (-opt -PosInt) (-opt -Nat) Univ)))]

[peek-string (->opt -Nat -Nat [-Input-Port] (Un -String (-val eof)))]
[peek-bytes (->opt -Nat -Nat [-Input-Port] (Un -Bytes (-val eof)))]

[peek-string! (->opt -String -Nat [-Input-Port -Nat -Nat] (Un -PosInt (-val eof)))]
[peek-bytes! (->opt -Bytes -Nat [-Input-Port -Nat -Nat] (Un -PosInt (-val eof)))]
[peek-bytes-avail! (->opt -Bytes -Nat [(-val #f) -Input-Port -Nat -Nat] (Un -Nat (-val eof) (-> (-opt -PosInt) (-opt -Nat) (-opt -PosInt) (-opt -Nat) Univ)))]
[peek-bytes-avail!* (->opt -Bytes -Nat [(-val #f) -Input-Port -Nat -Nat] (Un -Nat (-val eof) (-> (-opt -PosInt) (-opt -Nat) (-opt -PosInt) (-opt -Nat) Univ)))]
[peek-bytes-avail!/enable-break (->opt -Bytes -Nat [(-val #f) -Input-Port -Nat -Nat] (Un -Nat (-val eof) (-> (-opt -PosInt) (-opt -Nat) (-opt -PosInt) (-opt -Nat) Univ)))]

[read-char-or-special (->opt [-Input-Port] Univ)]
[read-byte-or-special (->opt [-Input-Port] Univ)]

;peek-char (in index)
;peek-byte (in index)
[peek-char-or-special (->opt [-Input-Port -Nat] Univ)]
[peek-byte-or-special (->opt [-Input-Port -Nat] Univ)]

;port-progress-evt TODO event

[port-provides-progress-evts? (-> -Input-Port B)]

[port-commit-peeked (->opt -Nat Univ Univ [-Input-Port] B)]

[byte-ready? (->opt [-Input-Port] B)]
[char-ready? (->opt [-Input-Port] B)]

;; Section 13.3 (Byte and String Output)
;; some are now in base-env-indexing-abs.rkt
[write-char (cl-> [(-Char) -Void]
                  [(-Char -Output-Port) -Void])]
;write-byte (in index)

[newline (->opt [-Output-Port] -Void)]

;In index
;write-string
;write-bytes
;write-bytes-avail*
;write-bytes-avail/enable-break

[write-special (->opt Univ [-Output-Port] B)]
[write-special-avail* (->opt Univ [-Output-Port] B)]

;; Need event type before we can include these
;;write-special-avail*
;;write-bytes-avail-evt
;;write-special-evt
;;
[port-writes-atomic? (-Output-Port . -> . -Boolean)]
[port-writes-special? (-Output-Port . -> . -Boolean)]

;; Section 13.4 (Reading)
[read (->opt [-Input-Port] Univ)]
[read-syntax (->opt [Univ -Input-Port] (Un (-Syntax Univ) (-val eof)))]
[read/recursive (->opt [-Input-Port (-opt -Char) (-opt -Read-Table) Univ] Univ)]
[read-syntax/recursive (->opt [Univ -Input-Port (-opt -Char) (-opt -Read-Table) Univ] Univ)]
[read-language (->opt [-Input-Port (-> ManyUniv)] (-opt (-> Univ Univ ManyUniv)))]

[read-case-sensitive (-Param Univ B)]
[read-square-bracket-as-paren (-Param Univ B)]
[read-curly-brace-as-paren (-Param Univ B)]
[read-accept-box (-Param Univ B)]
[read-accept-compiled (-Param Univ B)]
[read-accept-bar-quote (-Param Univ B)]
[read-accept-graph (-Param Univ B)]
[read-decimal-as-inexact (-Param Univ B)]
[read-accept-dot (-Param Univ B)]
[read-accept-infix-dot (-Param Univ B)]
[read-accept-quasiquote (-Param Univ B)]
[read-accept-reader (-Param B B)]
[read-accept-lang (-Param Univ B)]
[current-reader-guard (-Param (-> Univ ManyUniv) (-> Univ ManyUniv))]
[current-readtable (-Param (-opt -Read-Table) (-opt -Read-Table))]
[read-on-demand-source (-Param -Path -Path)]

[port-read-handler
 (cl->* (-> -Input-Port (->opt -Input-Port [Univ] Univ))
        (-> -Input-Port (->opt -Input-Port [Univ] Univ) -Void))]

;; Section 13.5 (Writing)
[write   (Univ [-Output-Port] . ->opt . -Void)]
[display (Univ [-Output-Port] . ->opt . -Void)]
[print   (Univ [-Output-Port (one-of/c 0 1)] . ->opt . -Void)]
[displayln (Univ [-Output-Port] . ->opt . -Void)]
[fprintf (->* (list -Output-Port -String) Univ -Void)]
[printf (->* (list -String) Univ -Void)]
[eprintf (->* (list -String) Univ -Void)]
[format (->* (list -String) Univ -String)]

[print-pair-curly-braces (-Param Univ B)]
[print-mpair-curly-braces (-Param Univ B)]
[print-unreadable (-Param Univ B)]
[print-graph (-Param Univ B)]
[print-struct (-Param Univ B)]
[print-box (-Param Univ B)]
[print-vector-length (-Param Univ B)]
[print-hash-table (-Param Univ B)]
[print-boolean-long-form (-Param Univ B)]
[print-reader-abbreviations (-Param Univ B)]
[print-as-expression (-Param Univ B)]
[print-syntax-width (-Param (Un (-val +inf.0) -Nat) (Un (-val +inf.0) -Nat))]

[current-write-relative-directory (-Param (-opt -Path) (-opt -Path))]

[port-write-handler
 (cl->* (-> -Output-Port (-> Univ -Output-Port ManyUniv))
        (-> -Output-Port (-> Univ -Output-Port ManyUniv) -Void))]
[port-display-handler
 (cl->* (-> -Output-Port (-> Univ -Output-Port ManyUniv))
        (-> -Output-Port (-> Univ -Output-Port ManyUniv) -Void))]
[port-print-handler
 (cl->* (-> -Output-Port (-> Univ -Output-Port ManyUniv))
        (-> -Output-Port (-> Univ -Output-Port ManyUniv) -Void))]

[global-port-print-handler (-Param (Un (-> Univ -Output-Port ManyUniv) (-> Univ -Output-Port (one-of/c 0 1) ManyUniv))
                                   (-> Univ -Output-Port (one-of/c 0 1) ManyUniv))]

;; Section 13.6 (racket/pretty)
[pretty-print (Univ [-Output-Port (one-of/c 0 1)] . ->opt . -Void)]
[pretty-write (Univ [-Output-Port] . ->opt . -Void)]
[pretty-display (Univ [-Output-Port] . ->opt . -Void)]
[pretty-format (Univ [-Nat] . ->opt . -String)]
[pretty-print-handler (-> Univ -Void)]

[pretty-print-columns (-Param (Un -Nat (-val 'infinity)) (Un -Nat (-val 'infinity)))]
[pretty-print-depth (-Param (-opt -Nat) (-opt -Nat))]
[pretty-print-exact-as-decimal (-Param Univ B)]
[pretty-print-.-symbol-without-bars (-Param Univ B)]
[pretty-print-show-inexactness (-Param Univ B)]
[pretty-print-abbreviate-read-macros (-Param Univ B)]

[pretty-print-style-table? (make-pred-ty -Pretty-Print-Style-Table)]
[pretty-print-current-style-table (-Param -Pretty-Print-Style-Table -Pretty-Print-Style-Table)]
[pretty-print-extend-style-table (-> (-opt -Pretty-Print-Style-Table) (-lst Sym) (-lst Sym) -Pretty-Print-Style-Table)]
[pretty-print-remap-stylable (-Param (-> Univ (-opt Sym)) (-> Univ (-opt Sym)))]

[pretty-print-newline (-> -Output-Port -Nat -Void)]
[pretty-print-print-line (-Param (-> (-opt -Nat) -Output-Port -Nat (Un -Nat (-val 'infinity)) -Nat)
                                 (-> (-opt -Nat) -Output-Port -Nat (Un -Nat (-val 'infinity)) -Nat))]
[pretty-print-size-hook (-Param (-> Univ B -Output-Port (-opt -Nat)) (-> Univ B -Output-Port (-opt -Nat)))]
[pretty-print-print-hook (-Param (-> Univ B -Output-Port -Void) (-> Univ B -Output-Port -Void))]
[pretty-print-pre-print-hook (-Param (-> Univ -Output-Port -Void) (-> Univ -Output-Port -Void))]
[pretty-print-post-print-hook (-Param (-> Univ -Output-Port -Void) (-> Univ -Output-Port -Void))]

[pretty-printing (-Param Univ B)]

[make-tentative-pretty-print-output-port (-> -Output-Port -Nat (-> ManyUniv) -Output-Port)]
[tentative-pretty-print-port-transfer (-> -Output-Port -Output-Port -Void)]
[tentative-pretty-print-port-cancel (-> -Output-Port -Void)]

;; Section 13.7

;; Section 13.7.1
[readtable? (make-pred-ty -Read-Table)]
[make-readtable
 (cl->*
  (-> -Read-Table -Read-Table)
  (-> -Read-Table
      (-opt -Char) (Un (one-of/c 'terminating-macro 'non-terminating-macro 'dispatch-macro) -Char)
      (-> -Char -Input-Port (-opt -PosInt) (-opt -Nat) (-opt -PosInt) (-opt -Nat) Univ)
      -Read-Table)
  (-> -Read-Table
      (-opt -Char) (Un (one-of/c 'terminating-macro 'non-terminating-macro 'dispatch-macro) -Char)
      (-> -Char -Input-Port (-opt -PosInt) (-opt -Nat) (-opt -PosInt) (-opt -Nat) Univ)
      (-opt -Char) (Un (one-of/c 'terminating-macro 'non-terminating-macro 'dispatch-macro) -Char)
      (-> -Char -Input-Port (-opt -PosInt) (-opt -Nat) (-opt -PosInt) (-opt -Nat) Univ)
      -Read-Table))]

[readtable-mapping (-> -Read-Table -Char
                       (-values (list
                                 (Un -Char (one-of/c  'terminating-macro 'non-terminating-macro))
                                 (-opt (Un (-> -Char -Input-Port (-opt -PosInt) (-opt -Nat)
                                               (-opt -PosInt) (-opt -Nat) Univ)
                                           (cl->*

                                             (-> -Char -Input-Port (-opt -PosInt) (-opt -Nat)
                                                 (-opt -PosInt) (-opt -Nat) Univ)
                                             (-> -Char -Input-Port Univ))))
                                 (-opt (Un (-> -Char -Input-Port (-opt -PosInt) (-opt -Nat)
                                               (-opt -PosInt) (-opt -Nat) Univ)
                                           (cl->*

                                             (-> -Char -Input-Port (-opt -PosInt) (-opt -Nat)
                                                 (-opt -PosInt) (-opt -Nat) Univ)
                                             (-> -Char -Input-Port Univ)))))))]

;; Section 13.7.2
;; Nothing defined here

;; Section 13.7.3
[special-comment? (make-pred-ty -Special-Comment)]
[make-special-comment (-> Univ -Special-Comment)]
[special-comment-value (-> -Special-Comment Univ)]

;; Section 13.8
[prop:custom-write -Struct-Type-Property]
[custom-write? (-> Univ B)]
[custom-write-accessor (-> Univ (-> Univ -Output-Port B ManyUniv))]

[prop:custom-print-quotable -Struct-Type-Property]
[custom-print-quotable? (-> Univ B)]
[custom-print-quotable-accessor (-> Univ Univ)]

;; Section 13.9

;; Section 13.10

;; Section 14.1 (Namespaces)
[namespace? (make-pred-ty -Namespace)]
[make-namespace (->opt [(one-of/c  'empty 'initial)] -Namespace)]
[make-empty-namespace (-> -Namespace)]
[make-base-empty-namespace (-> -Namespace)]
[make-base-namespace (-> -Namespace)]

[namespace-anchor? (make-pred-ty -Namespace-Anchor)]
[namespace-anchor->empty-namespace (-> -Namespace-Anchor -Namespace)]
[namespace-anchor->namespace (-> -Namespace-Anchor -Namespace)]

[current-namespace (-Param -Namespace -Namespace)]
[namespace-symbol->identifier (-> Sym Ident)]
[namespace-base-phase (->opt [-Namespace] -Integer)]
[namespace-module-identifier (->opt [(Un -Namespace -Integer (-val #f))] Ident)]
[namespace-variable-value (->opt Sym [Univ (-opt (-> Univ)) -Namespace] Univ)]
[namespace-set-variable-value! (->opt Sym [Univ Univ -Namespace] -Void)]
[namespace-undefine-variable! (->opt Sym [-Namespace] -Void)]
[namespace-mapped-symbols (->opt [-Namespace] (-lst Sym))]
[namespace-require (-> Univ -Void)]
[namespace-require/copy (-> Univ -Void)]
[namespace-require/constant (-> Univ -Void)]
[namespace-require/expansion-time (-> Univ -Void)]
[namespace-attach-module (->opt -Namespace -Module-Path [-Namespace] Univ)]
[namespace-unprotect-module (->opt -Inspector -Module-Path [-Namespace] -Void)]
[namespace-module-registry (-> -Namespace Univ)]
[module->namespace (-> -Module-Path -Namespace)]
[namespace-syntax-introduce (-poly (a) (-> (-Syntax a) (-Syntax a)))]
[module-provide-protected? (-> -Module-Path-Index Sym B)]

[variable-reference? (make-pred-ty -Variable-Reference)]
[variable-reference->empty-namespace (-> -Variable-Reference -Namespace)]
[variable-reference->namespace (-> -Variable-Reference -Namespace)]
[variable-reference->resolved-module-path (-> -Variable-Reference (-opt -Resolved-Module-Path))]
[variable-reference->module-declaration-inspector (-> -Variable-Reference -Inspector)]
[variable-reference->module-source (-> -Variable-Reference (Un Sym (-val #f) -Path))]
[variable-reference->phase (-> -Variable-Reference -Nat)]
[variable-reference-constant? (-> -Variable-Reference -Boolean)]

;; Section 14.2 (Evaluation and Compilation)
[current-eval (-Param (-> Univ ManyUniv) (-> Univ ManyUniv))]
[eval (->opt Univ [-Namespace] ManyUniv)]
[eval-syntax (->opt (-Syntax Univ) [-Namespace] ManyUniv)]

[current-load (-Param (-> -Path (-opt Sym) ManyUniv) (-> -Path (-opt Sym) ManyUniv))]
[load (-> -Pathlike ManyUniv)]
[load-relative (-> -Pathlike ManyUniv)]
[load/cd (-> -Pathlike ManyUniv)]

[current-load-extension (-Param (-> -Path (-opt Sym) ManyUniv) (-> -Path (-opt Sym) ManyUniv))]
[load-extension (-> -Pathlike ManyUniv)]
[load-relative-extension (-> -Pathlike ManyUniv)]

[current-load/use-compiled (-Param (-> -Path (-opt Sym) ManyUniv) (-> -Path (-opt Sym) ManyUniv))]
[load/use-compiled (-> -Pathlike ManyUniv)]

[current-load-relative-directory (-Param (-opt -Pathlike) (-opt -Path))]
[use-compiled-file-paths (-Param (-lst -Path) (-lst -Path))]

[read-eval-print-loop (-> -Void)]
[current-prompt-read (-Param (-> Univ) (-> Univ))]
[current-get-interaction-input-port (-Param (-> -Input-Port) (-> -Input-Port))]
[current-read-interaction (-Param (-> Univ -Input-Port Univ) (-> Univ -Input-Port Univ))]
[current-print (-Param (-> Univ ManyUniv) (-> Univ ManyUniv))]

[current-compile (-Param (-> Univ B -Compiled-Expression) (-> Univ B -Compiled-Expression))]
[compile (-> Univ -Compiled-Expression)]
[compile-syntax (-> (-Syntax Univ) -Compiled-Expression)]
[compiled-expression? (make-pred-ty -Compiled-Expression)]

[compile-enforce-module-constants (-Param B B)]
[compile-allow-set!-undefined (-Param B B)]
[compile-context-preservation-enabled (-Param B B)]
[eval-jit-enabled (-Param B B)]
[load-on-demand-enabled (-Param B B)]

;; Section 14.4 (Module Names and Loading)
[resolved-module-path? (make-pred-ty -Resolved-Module-Path)]
[make-resolved-module-path (-> (Un -Symbol -Path) -Resolved-Module-Path)]
[resolved-module-path-name (-> -Resolved-Module-Path (Un -Path -Symbol))]
[module-path? (asym-pred Univ B (-FS (-filter -Module-Path 0) -top))]

[current-module-name-resolver (-Param (cl->* (-Resolved-Module-Path Univ . -> . Univ)
                                             ((Un -Module-Path -Path)
                                              (-opt -Resolved-Module-Path)
                                              (-opt (-Syntax Univ))
                                              -Boolean
                                              . -> . -Resolved-Module-Path))
                                      (cl->* (-Resolved-Module-Path Univ . -> . Univ)
                                             ((Un -Module-Path -Path)
                                              (-opt -Resolved-Module-Path)
                                              (-opt (-Syntax Univ))
                                              -Boolean
                                              . -> . -Resolved-Module-Path)))]
[current-module-declare-name (-Param (-opt -Resolved-Module-Path)
                                     (-opt -Resolved-Module-Path))]
[current-module-declare-source (-Param (-opt (Un -Symbol -Path))
                                       (-opt (Un -Symbol -Path)))]
[module-path-index? (make-pred-ty -Module-Path-Index)]
[module-path-index-resolve (-> -Module-Path-Index -Resolved-Module-Path)]
[module-path-index-split (-> -Module-Path-Index
                             (-values
                              (list (-opt -Module-Path)
                                    (-opt (Un -Module-Path-Index
                                              -Resolved-Module-Path)))))]
[module-path-index-join (-> (-opt -Module-Path)
                            (-opt (Un -Module-Path-Index -Resolved-Module-Path))
                            -Module-Path-Index)]
[compiled-module-expression? (make-pred-ty -Compiled-Module-Expression)]
[module-compiled-name (-> -Compiled-Module-Expression -Symbol)]
[module-compiled-imports (-> -Compiled-Module-Expression
                             (-lst (-pair (-opt -Integer)
                                          (-lst -Module-Path-Index))))]

[module-compiled-exports
 (-> -Compiled-Module-Expression
     (-values
      (list
        (-lst (-pair (-opt -Integer)
                     (-lst (-lst*
                             -Symbol
                             (-lst (Un -Module-Path-Index
                                       (-lst*
                                         -Module-Path-Index
                                         (-opt -Integer)
                                         -Symbol
                                         (-opt -Integer))))))))
        (-lst (-pair (-opt -Integer)
                     (-lst (-lst*
                             -Symbol
                             (-lst (Un -Module-Path-Index
                                       (-lst*
                                         -Module-Path-Index
                                         (-opt -Integer)
                                         -Symbol
                                         (-opt -Integer)))))))))))]
[module-compiled-language-info
 (-> -Compiled-Module-Expression
     (-opt (make-HeterogeneousVector (list -Module-Path -Symbol Univ))))]

;; Section 14.4.3
[dynamic-require
 (let ((mod (Un -Module-Path -Resolved-Module-Path -Module-Path-Index)))
  (-poly (a)
   (cl->* (-> mod (Un (one-of/c #f 0) -Void) -Void)
          (-> mod (Un (one-of/c #f 0) -Void) (-> a) (Un -Void a))
          (->opt mod Sym [(-> Univ)] ManyUniv))))]

[dynamic-require-for-syntax
 (let ((mod (Un -Module-Path -Resolved-Module-Path -Module-Path-Index)))
  (-poly (a)
   (cl->* (-> mod (-val #f) -Void)
          (-> mod (-val #f) (-> a) (Un -Void a))
          (->opt mod Sym [(-> Univ)] ManyUniv))))]

[module->language-info
 (->opt (Un -Module-Path -Path -Resolved-Module-Path)
        [Univ]
        (-opt (make-HeterogeneousVector (list -Module-Path -Symbol Univ))))]

[module->imports (-> -Compiled-Module-Expression
                             (-lst (-pair (-opt -Integer)
                                          (-lst -Module-Path-Index))))]
[module->exports
 (-> -Compiled-Module-Expression
     (-values
      (list
       (-lst (-pair (-opt -Integer)
                    (-lst (-lst* -Symbol
                                 (-lst
                                  (Un -Module-Path-Index
                                      (-lst* -Module-Path-Index
                                             (-opt -Integer)
                                             -Symbol
                                             (-opt -Integer))))))))
       (-lst (-pair (-opt -Integer)
                    (-lst (-lst* -Symbol
                                 (-lst
                                  (Un -Module-Path-Index
                                      (-lst* -Module-Path-Index
                                             (-opt -Integer)
                                             -Symbol
                                             (-opt -Integer)))))))))))]

;; Section 14.5 (Impersonators and Chaperones)
[impersonator? (Univ . -> . B)]
[chaperone? (Univ . -> . B)]
[impersonator-of? (Univ Univ . -> . B)]
[chaperone-of? (Univ Univ . -> . B)]

[make-impersonator-property (-> Sym (-values (list -Impersonator-Property (-> Univ B) (-> Univ Univ))))]
[impersonator-property? (make-pred-ty -Impersonator-Property)]
[impersonator-property-accessor-procedure? (-> Univ B)]
[impersonator-prop:application-mark -Impersonator-Property]

;; Section 14.6 (Security Guards)
[security-guard? (make-pred-ty -Security-Guard)]
[make-security-guard
 (->opt -Security-Guard
        (-> Sym (-opt -Path) (-lst Sym) ManyUniv)
        (-> Sym (-opt -String) (-opt -PosInt) (one-of/c 'server 'client)  ManyUniv)
        [(-opt (-> Sym -Path -Path ManyUniv))]
        -Security-Guard)]
[current-security-guard (-Param -Security-Guard -Security-Guard)]

;; Section 14.7 (Custodians)
[custodian? (make-pred-ty -Custodian)]
[make-custodian (->opt [-Custodian] -Custodian)]
[custodian-shutdown-all (-> -Custodian -Void)]
[current-custodian (-Param -Custodian -Custodian)]
[custodian-managed-list (-> -Custodian -Custodian (-lst Univ))]
[custodian-memory-accounting-available? (-> B)]
[custodian-require-memory (-> -Custodian -Nat -Custodian -Void)]
[custodian-limit-memory (->opt -Custodian -Nat [-Custodian] -Void)]

[make-custodian-box (-poly (a) (-> -Custodian a (make-CustodianBox a)))]
[custodian-box? (make-pred-ty (-poly (a) (make-CustodianBox a)))]
[custodian-box-value (-poly (a) (-> (make-CustodianBox a) a))]

;; Section 14.8 (Thread Groups)
[make-thread-group (->opt [-Thread-Group] -Thread-Group)]
[thread-group? (make-pred-ty -Thread-Group)]
[current-thread-group (-Param -Thread-Group -Thread-Group)]

;; Section 14.9 (Structure Inspectors)
[inspector? (make-pred-ty -Inspector)]
[make-inspector (->opt [-Inspector] -Inspector)]
[make-sibling-inspector (->opt [-Inspector] -Inspector)]
[current-inspector (-Param -Inspector -Inspector)]

[struct-info (-> Univ (-values (list (-opt (make-StructTypeTop)) B)))]
[struct-type-info
 (-poly (a)
   (cl->*
    (-> (make-StructType a)
        (-values (list Sym -Nat -Nat (-> a -Nat Univ)
                       (-> a -Nat (Un) -Void) (-lst -Nat)
                       (-opt (make-StructTypeTop)) B)))
    (-> (make-StructTypeTop)
        (-values (list Sym -Nat -Nat top-func top-func (-lst -Nat)
                       (-opt (make-StructTypeTop)) B)))))]
[struct-type-make-constructor (-> (make-StructTypeTop) top-func)]
[struct-type-make-predicate
 (-poly (a)
   (cl->*
    (-> (make-StructType a) (make-pred-ty a))
    (-> (make-StructTypeTop) (-> Univ B))))]
[object-name (-> Univ Univ)]

;; Section 14.9 (Code Inspectors)
[current-code-inspector (-Param -Inspector -Inspector)]

;; Section 15.1 (Path Manipulation)
[path? (make-pred-ty -Path)]
[path-string? (asym-pred Univ B
                         (-FS (-filter (Un -Path -String) 0)
                              (-not-filter -Path 0)))]
[path-for-some-system? (make-pred-ty -SomeSystemPath)]

[string->path (-> -String -Path)]
[bytes->path (cl->* (-> -Bytes -Path) (-> -Bytes -PathConventionType -SomeSystemPath))]
[path->string (-> -Path -String)]
[path->bytes (-> -SomeSystemPath -Bytes)]

[string->path-element (-> -String -Path)]
[bytes->path-element (cl->* (-> -Bytes -Path) (-> -Bytes -PathConventionType -SomeSystemPath))]
[path-element->string (-> -Path -String)]
[path-element->bytes (-> -SomeSystemPath -Bytes)]

[path-convention-type (-> -SomeSystemPath -PathConventionType)]
[system-path-convention-type (-> -PathConventionType)]

[build-path
 (cl->*
  ((list -Pathlike*) -Pathlike* . ->* . -Path)
  ((list -SomeSystemPathlike*) -SomeSystemPathlike* . ->* . -SomeSystemPath))]
[build-path/convention-type
  ((list -PathConventionType -SomeSystemPathlike*) -SomeSystemPathlike* . ->* . -SomeSystemPath)]

[absolute-path? (-> -SomeSystemPath B)]
[relative-path? (-> -SomeSystemPath B)]
[complete-path? (-> -SomeSystemPath B)]

[path->complete-path
 (cl->* (-> -Pathlike -Path)
        (-> -Pathlike -Pathlike -Path)
        (-> -SomeSystemPathlike -SomeSystemPathlike -SomeSystemPath))]

[path->directory-path
 (cl->* (-> -Pathlike -Path)
        (-> -SomeSystemPathlike -SomeSystemPath))]

[resolve-path (-> -Pathlike -Path)]
[cleanse-path
 (cl->* (-> -Pathlike -Path)
        (-> -SomeSystemPathlike -SomeSystemPath))]
[expand-user-path (-> -Pathlike -Path)]

[simplify-path
 (cl->*
  (-Pathlike . -> . -Path)
  (-Pathlike B . -> . -Path)
  (-SomeSystemPathlike B . -> . -SomeSystemPath))]

[normal-case-path
 (cl->* (-> -Pathlike -Path)
        (-> -SomeSystemPathlike -SomeSystemPath))]

[split-path
 (cl->*
  (-> -Pathlike
      (-values (list
                (Un -Path (-val 'relative) (-val #f))
                (Un -Path (-val 'up) (-val 'same))
                B)))
  (-> -SomeSystemPathlike
      (-values (list
                (Un -SomeSystemPath (one-of/c 'relative #f))
                (Un -SomeSystemPath (one-of/c 'up 'same))
                B))))]

[path-replace-suffix
 (cl->*
  (-> -Pathlike (Un -String -Bytes) -Path)
  (-> -SomeSystemPathlike (Un -String -Bytes) -SomeSystemPath))]

[path-add-suffix
 (cl->*
  (-> -Pathlike (Un -String -Bytes) -Path)
  (-> -SomeSystemPathlike (Un -String -Bytes) -SomeSystemPath))]

;; Section 15.1.2 (racket/path)
[explode-path (-SomeSystemPathlike . -> . (-lst (Un -SomeSystemPath (one-of/c 'up 'same))))]
[simple-form-path (-Pathlike . -> . -Path)]
[normalize-path (cl->* (-Pathlike [-Pathlike] . ->opt . -Path))]
[filename-extension (-SomeSystemPathlike . -> . (-opt -Bytes))]
[file-name-from-path (-Pathlike . -> . (-opt -Path))]
[path-only (-SomeSystemPathlike . -> . (-opt -Path))]
[some-system-path->string (-SomeSystemPath . -> . -String)]
[string->some-system-path
 (-String (one-of/c 'unix 'windows) . -> . -SomeSystemPath)]

;; Section 15.2

;; Section 15.2.1
[find-system-path (Sym . -> . -Path)]
[path-list-string->path-list ((Un -String -Bytes) (-lst -Path) . -> . (-lst -Path))]
[find-executable-path (->opt -Pathlike [(-opt -Pathlike) Univ] (-opt -Path))]

;; Section 15.2.2
[file-exists? (-> -Pathlike B)]
[link-exists? (-> -Pathlike B)]
[delete-file (-> -Pathlike -Void)]
[rename-file-or-directory (->opt -Pathlike -Pathlike [Univ] -Void)]

[file-or-directory-modify-seconds
 (-poly (a)
   (cl->* (-Pathlike . -> . -NonNegFixnum)
          (-Pathlike (-val #f) . -> . -NonNegFixnum)
          (-Pathlike -Nat . -> . -Void)
          (-Pathlike (-val #f) (-> a) . -> . (Un a -NonNegFixnum))
          (-Pathlike -Nat (-> a) . -> . (Un a -Void))))]


[file-or-directory-permissions
 (cl->* (-> -Pathlike (-lst (one-of/c 'read 'write 'execute)))
        (-> -Pathlike (-val #f) (-lst (one-of/c 'read 'write 'execute)))
        (-> -Pathlike (-val 'bits) -NonNegFixnum)
        (-> -Pathlike -NonNegFixnum -Void))]

[file-or-directory-identity (->opt -Pathlike [Univ] -PosInt)]
[file-size (-> -Pathlike -Nat)]

[copy-file (-> -Pathlike -Pathlike -Void)]
[make-file-or-directory-link (-> -Pathlike -Pathlike -Void)]

;; Section 15.2.3
[current-directory (-Param -Pathlike -Path)]
[current-drive (-> -Path)]

[directory-exists? (-> -Pathlike B)]
[make-directory (-> -Pathlike -Void)]
[delete-directory (-> -Pathlike -Void)]
[directory-list (->opt [-Pathlike] (-lst -Path))]
[filesystem-root-list (-> (-lst -Path))]

;; Section 15.2.4

;; Section 15.2.5 (racket/file)
[copy-directory/files (->key -Pathlike -Pathlike  #:keep-modify-seconds? Univ #f -Void)]
[delete-directory/files (->key -Pathlike #:must-exist? Univ #f -Void)]

[find-files (->optkey (-> -Path Univ) [(-opt -Pathlike)] #:follow-links? Univ #f (-lst -Path))]
[pathlist-closure (->key (-lst -Pathlike) #:follow-links? Univ #f (-lst -Path))]

[fold-files
 (-poly
  (a)
  (let ([funarg* (-Path (one-of/c 'file 'dir 'link) a . -> . (-values (list a Univ)))]
        [funarg (-Path (one-of/c 'file 'dir 'link) a . -> . a)])
     ((Un funarg funarg*) a [(-opt -Pathlike) Univ]. ->opt . a)))]

[make-directory* (-> -Pathlike -Void)]
#;[make-temporary-file (->opt [-String (Un -Pathlike (-val 'directory) (-val #f)) (-opt -Pathlike)] -Path)]

[put-preferences (->opt (-lst -Symbol) (-lst Univ) [(-> -Path Univ) (-opt -Pathlike)] -Void)]
[preferences-lock-file-mode (-> (one-of/c 'exists 'file-lock))]

[make-lock-file-name (->opt -Pathlike [-Pathlike] -Pathlike)]

[user-read-bit     (-val user-read-bit)]
[user-write-bit    (-val user-write-bit)]
[user-execute-bit  (-val user-execute-bit)]
[group-read-bit    (-val group-read-bit)]
[group-write-bit   (-val group-write-bit)]
[group-execute-bit (-val group-execute-bit)]
[other-read-bit    (-val other-read-bit)]
[other-write-bit   (-val other-write-bit)]
[other-execute-bit (-val other-execute-bit)]

;; Section 15.3.1 (racket/tcp)
[tcp-listen (-Integer [-Integer Univ (-opt -String)] . ->opt . -TCP-Listener)]
[tcp-connect (-String -Integer . -> . (-values (list -Input-Port -Output-Port)))]
[tcp-connect/enable-break (-String -Integer . -> . (-values (list -Input-Port -Output-Port)))]
[tcp-accept (-TCP-Listener . -> . (-values (list -Input-Port -Output-Port)) )]
[tcp-accept/enable-break (-TCP-Listener . -> . (-values (list -Input-Port -Output-Port)) )]

[tcp-accept-ready? (-TCP-Listener . -> . B)]
[tcp-close (-TCP-Listener . -> . -Void)]
[tcp-listener? (make-pred-ty -TCP-Listener)]

[tcp-accept-evt
 (-> -TCP-Listener
     (-evt (-lst* -Input-Port -Output-Port)))]

[tcp-abandon-port (-Port . -> . -Void)]
[tcp-addresses (cl->*
                (-Port [(-val #f)] . ->opt . (-values (list -String -String)))
                (-Port (-val #t) . -> . (-values (list -String -Index -String -Index))))]

[tcp-port? (asym-pred Univ B (-FS (-filter (Un -Input-Port -Output-Port) 0) -top))]

;; Section 15.3.2 (racket/udp)
[udp-open-socket (->opt [(-opt -String) (-opt -String)] -UDP-Socket)]
[udp-bind! (-> -UDP-Socket (-opt -String) -Nat -Void)]
[udp-connect! (-> -UDP-Socket (-opt -String) (-opt -Nat) -Void)]

[udp-send-to (->opt -UDP-Socket -String -Nat -Bytes [-Nat -Nat] -Void)]
[udp-send (->opt -UDP-Socket -Bytes [-Nat -Nat] -Void)]
[udp-send-to* (->opt -UDP-Socket -String -Nat -Bytes [-Nat -Nat] B)]
[udp-send* (->opt -UDP-Socket -Bytes [-Nat -Nat] B)]
[udp-send-to/enable-break (->opt -UDP-Socket -String -Nat -Bytes [-Nat -Nat] -Void)]
[udp-send/enable-break (->opt -UDP-Socket -Bytes [-Nat -Nat] -Void)]

[udp-receive! (->opt -UDP-Socket -Bytes [-Nat -Nat] (-values (list -Nat -String -Nat)))]
[udp-receive!* (->opt -UDP-Socket -Bytes [-Nat -Nat] (-values (list (-opt -Nat) (-opt -String) (-opt -Nat))))]
[udp-receive!/enable-break (->opt -UDP-Socket -Bytes [-Nat -Nat] (-values (list -Nat -String -Nat)))]

[udp-close (-> -UDP-Socket -Void)]
[udp? (make-pred-ty -UDP-Socket)]
[udp-bound? (-> -UDP-Socket B)]
[udp-connected? (-> -UDP-Socket B)]

[udp-send-ready-evt (-> -UDP-Socket (-mu x (-evt x)))]
[udp-receive-ready-evt (-> -UDP-Socket (-mu x (-evt x)))]
[udp-send-to-evt (->opt -UDP-Socket -String -Nat -Bytes [-Nat -Nat]
                        (-evt -Void))]
[udp-send-evt (->opt -UDP-Socket -Bytes [-Nat -Nat]
                     (-evt -Void))]
[udp-receive!-evt
 (->opt -UDP-Socket -Bytes [-Nat -Nat]
        (-evt (-lst* -Nat -String -Nat)))]

[udp-addresses
 (cl->*
  (->opt -UDP-Socket [(-val #f)] (-values (list -String -String)))
  (-> -UDP-Socket (-val #t) (-values (list -String -NonNegFixnum -String -NonNegFixnum))))]

;; Section 15.4 (Processes)

;; `subprocess' has 3 arguments and 3 return values which are either
;; ports or false. There is a relation that if a port argument is
;; false, then the corresponding return value is a port, and if it is
;; a port the return value is false. `process/ports` and
;; `process*/ports' have a similar behavior.  This behavior is encoded
;; in the type system as each possible combination of port arguments
;; being one case of a case-> type. There is also a final case where
;; the unions are preserved because TR currently can not deal with
;; applying a case lambda to union types, and ensure that while no one
;; branch covers every union, every union is covered by a branch.
;; There is also twice as many cases to deal with the 'exact behavior
;; for windows.

[subprocess
 (let* ((make-opt-in-port (lambda (port) (if port -Input-Port (-val #f))))
        (make-opt-out-port (lambda (port) (if port -Output-Port (-val #f))))
        (ret-type (-values (list -Subprocess (-opt -Input-Port) (-opt -Output-Port) (-opt -Input-Port))))
        ;; out, in, err, and exact are all booleans and correspond to
        ;; whether or not the argument is a port (#t) or #f (#f).
        ;; The return value is the function type that is one branch
        ;; of the case lambda.
        (make-specific-case (lambda (out in err exact)
                              (let ((arg-out (make-opt-out-port out))
                                    (arg-in (make-opt-in-port in))
                                    (arg-err (make-opt-out-port err))
                                    (result (-values (list -Subprocess
                                                           (make-opt-in-port (not out))
                                                           (make-opt-out-port (not in))
                                                           (make-opt-in-port (not err))))))
                                (if exact
                                    (-> arg-out arg-in arg-err -Pathlike (-val 'exact) -String result)
                                    (->* (list arg-out arg-in arg-err -Pathlike)
                                         (Un -Path -String -Bytes)
                                         result)))))
        (specific-cases
         (let ((bools '(#t #f)))
          (for*/list ((out bools) (in bools) (err bools) (exact bools))
            (make-specific-case out in err exact)))))
  (apply cl->*
    (append specific-cases
     (list
      (->* (list (-opt -Output-Port) (-opt -Input-Port) (-opt -Output-Port) -Pathlike)
                    (Un -Path -String -Bytes) ret-type)
      (-> (-opt -Output-Port) (-opt -Input-Port) (-opt -Output-Port) -Pathlike (-val 'exact) -String ret-type)))))]
[subprocess-wait (-> -Subprocess -Void)]
[subprocess-status (-> -Subprocess (Un (-val 'running) -Nat))]
[subprocess-kill (-> -Subprocess Univ -Void)]
[subprocess-pid (-> -Subprocess -Nat)]
[subprocess? (make-pred-ty -Subprocess)]
[current-subprocess-custodian-mode (-Param (one-of/c #f 'kill 'interrupt)
                                           (one-of/c #f 'kill 'interrupt))]
[subprocess-group-enabled (-Param Univ B)]

[shell-execute (-> (-opt -String) -String -String -Pathlike Sym (-val #f))]

;; Section 15.4.1 (racket/system)
[system ((Un -String -Bytes) [] #:set-pwd? Univ #f . ->optkey . -Boolean)]
[system* (-Pathlike [] #:rest (Un -Path -String -Bytes) #:set-pwd? Univ #f . ->optkey . -Boolean)]
[system/exit-code ((Un -String -Bytes) [] #:set-pwd? Univ #f . ->optkey . -Byte)]
[system*/exit-code (-Pathlike [] #:rest (Un -Path -String -Bytes) #:set-pwd? Univ #f . ->optkey . -Byte)]

[process (->key
	  -String
	   #:set-pwd? Univ #f
	  (-lst* -Input-Port -Output-Port -Nat -Input-Port
                 (cl->* (-> (-val 'status) (one-of/c 'running 'done-ok 'done-error))
                        (-> (-val 'exit-code) (-opt -Byte))
                        (-> (-val 'wait) ManyUniv)
                        (-> (-val 'interrupt) -Void)
                        (-> (-val 'kill) -Void))))]

[process*
 (cl->*
   (->optkey -Pathlike [] #:rest (Un -Path -String -Bytes) #:set-pwd? Univ #f
        (-lst* -Input-Port -Output-Port -Nat -Input-Port
               (cl->* (-> (-val 'status) (one-of/c 'running 'done-ok 'done-error))
                      (-> (-val 'exit-code) (-opt -Byte))
                      (-> (-val 'wait) ManyUniv)
                      (-> (-val 'interrupt) -Void)
                      (-> (-val 'kill) -Void))))
   (->key -Pathlike (-val 'exact) -String
	  #:set-pwd? Univ #f
	  (-lst* -Input-Port -Output-Port -Nat -Input-Port
                 (cl->* (-> (-val 'status) (one-of/c 'running 'done-ok 'done-error))
                        (-> (-val 'exit-code) (-opt -Byte))
                        (-> (-val 'wait) ManyUniv)
                        (-> (-val 'interrupt) -Void)
                        (-> (-val 'kill) -Void)))))]

[process/ports
 (let* ((fun-type
         (cl->* (-> (-val 'status) (one-of/c 'running 'done-ok 'done-error))
                (-> (-val 'exit-code) (-opt -Byte))
                (-> (-val 'wait) ManyUniv)
                (-> (-val 'interrupt) -Void)
                (-> (-val 'kill) -Void)))
        (make-opt-in-port (lambda (port) (if port -Input-Port (-val #f))))
        (make-opt-out-port (lambda (port) (if port -Output-Port (-val #f))))
        ;; out, in, and exact are all booleans and correspond to
        ;; whether or not the argument is a port (#t) or #f (#f).
        ;; err is either a boolean or 'stdout where 'stdout corresponds
        ;; to the input being the value 'stdout.
        ;; The return value is the function type that is one branch
        ;; of the case lambda.
        (make-specific-case (lambda (out in err)
                              (->key (make-opt-out-port out)
				     (make-opt-in-port in)
				     (case err
				       ((stdout) (-val 'stdout))
				       (else (make-opt-out-port err)))
				     -String
				     #:set-pwd? Univ #f
				     (-lst* (make-opt-in-port (not out))
					    (make-opt-out-port (not in))
					    -Nat
					    (make-opt-in-port (not err))
					    fun-type))))
        (specific-cases
         (let ((bools '(#t #f))
               (err-vals '(#t #f stdout)))
          (for*/list ((out bools) (in bools) (err err-vals))
            (make-specific-case out in err)))))
   (apply cl->*
    (append
      specific-cases
      (list
       (->key (-opt -Output-Port) (-opt -Input-Port) (Un -Output-Port (one-of/c #f 'stdout)) -String
	      #:set-pwd? Univ #f
	      (-lst* (-opt -Input-Port) (-opt -Output-Port) -Nat (-opt -Input-Port) fun-type))))))]

[process*/ports
 (let* ((fun-type
         (cl->* (-> (-val 'status) (one-of/c 'running 'done-ok 'done-error))
                (-> (-val 'exit-code) (-opt -Byte))
                (-> (-val 'wait) ManyUniv)
                (-> (-val 'interrupt) -Void)
                (-> (-val 'kill) -Void)))
        (make-opt-in-port (lambda (port) (if port -Input-Port (-val #f))))
        (make-opt-out-port (lambda (port) (if port -Output-Port (-val #f))))
        ;; out, in, and exact are all booleans and correspond to
        ;; whether or not the argument is a port (#t) or #f (#f).
        ;; err is either a boolean or 'stdout where 'stdout corresponds
        ;; to the input being the value 'stdout.
        ;; The return value is the function type that is one branch
        ;; of the case lambda.
        (make-specific-case (lambda (out in err exact)
                              (let ((arg-out (make-opt-out-port out))
                                    (arg-in (make-opt-in-port in))
                                    (arg-err
                                      (case err
                                        ((stdout) (-val 'stdout))
                                        (else (make-opt-out-port err))))
                                    (result
                                     (-lst* (make-opt-in-port (not out))
                                            (make-opt-out-port (not in))
                                            -Nat
                                            (make-opt-in-port (not err))
                                            fun-type)))
                                (if exact
                                  (->key arg-out arg-in arg-err -Pathlike (-val 'exact) -String  #:set-pwd? Univ #f result)
                                  (->optkey arg-out arg-in arg-err -Pathlike []
					    #:rest (Un -Path -String -Bytes)
					    #:set-pwd? Univ #f
					    result)))))
        (specific-cases
         (let ((bools '(#t #f))
               (err-vals '(#t #f stdout)))
          (for*/list ((out bools) (in bools) (err err-vals) (exact bools))
            (make-specific-case out in err exact)))))
   (apply cl->*
    (append specific-cases
     (list
       (->optkey (-opt -Output-Port) (-opt -Input-Port) (Un -Output-Port (one-of/c #f 'stdout)) -Pathlike
		 []
		 #:rest (Un -Path -String -Bytes)
		 #:set-pwd? Univ #f
		 (-lst* (-opt -Input-Port) (-opt -Output-Port) -Nat (-opt -Input-Port) fun-type))
       (->key (-opt -Output-Port) (-opt -Input-Port) (Un -Output-Port (one-of/c #f 'stdout)) -Pathlike (-val 'exact) -String
	      #:set-pwd? Univ #f
	      (-lst* (-opt -Input-Port) (-opt -Output-Port) -Nat (-opt -Input-Port) fun-type))))))]

;; Section 15.5 (Logging)
[logger? (make-pred-ty -Logger)]
[make-logger (->opt [(-opt Sym) (-opt -Logger)] -Logger)]
[logger-name (-> -Logger (-opt Sym))]
[current-logger (-Param -Logger -Logger)]

[log-message (cl->* (-> -Logger -Log-Level -String Univ -Void)
                    (-> -Logger -Log-Level (Un (-val #f) -Symbol) -String Univ -Void))]
[log-level? (-> -Logger -Log-Level  B)]

[log-receiver? (make-pred-ty -Log-Receiver)]
[make-log-receiver (-> -Logger -Log-Level -Log-Receiver)]

;; Section 15.6 (Time)
[seconds->date (cl->* (-Integer . -> . -Date)
                      (-Integer Univ . -> . -Date))]
[current-seconds (-> -Integer)]
[current-milliseconds (-> -Fixnum)]
[current-inexact-milliseconds (-> -Real)]
[current-gc-milliseconds (-> -Fixnum)]
[current-process-milliseconds (-> -Fixnum)]

;; Section 15.7
[getenv (-> -String (Un -String (-val #f)))]
[putenv (-> -String -String B)]

;; Section 15.8
[system-type
 (cl->*
  (-> (-val 'os) (Un (-val 'unix) (-val 'windows) (-val 'macosx)))
  (-> (-val 'gc) (Un (-val 'cgc) (-val '3m)))
  (-> (-val 'link) (Un (-val 'static) (-val 'shared) (-val 'dll) (-val 'framework)))
  (-> (-val 'so-suffix) -Bytes)
  (-> (-val 'machine) -String)
  (-> (-val 'word) -PosInt))]
[system-language+country (-> -String)]
[system-library-subpath (->opt [(Un (-val #f) (-val 'cgc) (-val '3m))] -Path)]

[version (-> -String)]
[banner (-> -String)]

[current-command-line-arguments (-Param (-vec -String) (-vec -String))]
[current-thread-initial-stack-size (-Param -PosInt -PosInt)]
;vector-set-performance-stats! TODO complicated

;; Section 15.9 (racket/cmdline)
[parse-command-line
 (let ([mode-sym (one-of/c 'once-each 'once-any 'multi 'final 'help-labels)])
   (-polydots (b a)
              (cl->* (-Pathlike
                      (Un (-lst -String) (-vec -String))
                      (-lst (-pair mode-sym (-lst (-lst Univ))))
                      ((list Univ) [a a] . ->... . b)
                      (-lst -String)
                      . -> . b))))]

;; Section 16.2 (Ephemerons)
[make-ephemeron (-poly (k v) (-> k v (make-Ephemeron v)))]
[ephemeron? (make-pred-ty (make-Ephemeron Univ))]
[ephemeron-value (-poly (v) (-> (make-Ephemeron v) (Un (-val #f) v)))]

;; Section 16.3 (Wills and Executors)
[make-will-executor (-> -Will-Executor)]
[will-executor? (make-pred-ty -Will-Executor)]
[will-register (-poly (a) (-> -Will-Executor a (-> a ManyUniv) -Void))]
[will-execute (-> -Will-Executor ManyUniv)]
[will-try-execute (-> -Will-Executor ManyUniv)]

;; Section 16.4
[collect-garbage (-> -Void)]
[current-memory-use (-> -Nat)]
[dump-memory-stats (-> Univ)]

;; Section 17.2 (Unsafe Data Extraction)
[unsafe-car (-poly (a b)
                   (cl->*
                    (->acc (list (-pair a b)) a (list -car))
                    (->* (list (-lst a)) a)))]
[unsafe-cdr (-poly (a b)
                   (cl->*
                    (->acc (list (-pair a b)) b (list -cdr))
                    (->* (list (-lst a)) (-lst a))))]
[unsafe-vector-length ((make-VectorTop) . -> . -Index)]
[unsafe-vector*-length ((make-VectorTop) . -> . -Index)]
[unsafe-struct-ref top-func]
[unsafe-struct*-ref top-func]
[unsafe-struct-set! top-func]
[unsafe-struct*-set! top-func]

;; Section 17.4 (Unsafe Undefined)
[check-not-unsafe-undefined (-poly (a) (-> a -Symbol a))]
[check-not-unsafe-undefined/assign (-poly (a) (-> a -Symbol a))]

;; Section 18.2 (Libraries and Collections)
[find-library-collection-paths (->opt [(-lst -Pathlike) (-lst -Pathlike)] (-lst -Path))]
[collection-file-path (->* (list -Pathlike) -Pathlike -Path)]
[collection-path (->* (list) -Pathlike -Path)]
[current-library-collection-paths (-Param -Path -Path)]
[use-user-specific-search-paths (-Param Univ B)]

;; Typed Racket Reference
;; Section 4
[assert (-poly (a b) (cl->*
                      (Univ (make-pred-ty (list a) Univ b) . -> . b)
                      (-> (Un a (-val #f)) a)))]
[defined? (->* (list Univ) -Boolean : (-FS (-not-filter -Undefined 0) (-filter -Undefined 0)))]

;; Syntax Manual
;; Section 2.1 (syntax/stx)
;; Needed for `with-syntax'
[stx->list (-> (-Syntax Univ) (Un (-lst (-Syntax Univ)) (-val #f)))]
[stx-list? (-> (-Syntax Univ) -Boolean)]

;; Keyword functions moved back to here:
[file->string
 (->key -Pathlike #:mode (one-of/c 'binary 'text) #f -String)]
(file->bytes (->key -Pathlike #:mode (one-of/c 'binary 'text) #f -Bytes))
(file->value (->key -Pathlike #:mode (one-of/c 'binary 'text) #f Univ))
(file->lines
 (->key
  -Pathlike
  #:mode
  (one-of/c 'binary 'text)
  #f
  #:line-mode
  (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)
  #f
  (-lst -String)))
(file->bytes-lines
 (->key
  -Pathlike
  #:line-mode
  (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)
  #f
  #:mode
  (one-of/c 'binary 'text)
  #f
  (-lst -Bytes)))
(display-to-file
 (->key
  Univ
  -Pathlike
  #:exists
  (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace)
  #f
  #:mode
  (one-of/c 'binary 'text)
  #f
  -Void))
(display-lines-to-file
 (->key
  (-lst Univ)
  -Pathlike
  #:separator
  Univ
  #f
  #:mode
  (one-of/c 'binary 'text)
  #f
  #:exists
  (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace)
  #f
  -Void))
(write-to-file
 (->key
  Univ
  -Pathlike
  #:exists
  (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace)
  #f
  #:mode
  (one-of/c 'binary 'text)
  #f
  -Void))
(file->list
 (-poly (a)
  (cl->*
   (->optkey -Pathlike [(-> -Input-Port (Un))] #:mode (one-of/c 'binary 'text) #f (-lst Univ))
   (->optkey -Pathlike [(-> -Input-Port a)] #:mode (one-of/c 'binary 'text) #f (-lst a)))))
(get-preference
 (let ((use-lock-type Univ)
       (timeout-lock-there-type (-opt (-> -Path Univ)))
       (lock-there-type (-opt (-> -Path Univ))))
   (cl->*
    (->key
     -Symbol
     #:use-lock?
     use-lock-type
     #f
     #:timeout-lock-there
     timeout-lock-there-type
     #f
     #:lock-there
     lock-there-type
     #f
     Univ)
    (->key
     -Symbol
     (-> Univ)
     #:use-lock?
     use-lock-type
     #f
     #:timeout-lock-there
     timeout-lock-there-type
     #f
     #:lock-there
     lock-there-type
     #f
     Univ)
    (->key
     -Symbol
     (-> Univ)
     Univ
     #:use-lock?
     use-lock-type
     #f
     #:timeout-lock-there
     timeout-lock-there-type
     #f
     #:lock-there
     lock-there-type
     #f
     Univ)
    (->key
     -Symbol
     (-> Univ)
     Univ
     (-opt -Pathlike)
     #:use-lock?
     use-lock-type
     #f
     #:timeout-lock-there
     timeout-lock-there-type
     #f
     #:lock-there
     lock-there-type
     #f
     Univ))))
(make-handle-get-preference-locked
 (let ((lock-there-type (-opt (-> -Path Univ)))
       (max-delay-type -Real))
   (->optkey -Real -Symbol [(-> Univ) Univ (-opt -Pathlike)]
             #:lock-there lock-there-type #f #:max-delay max-delay-type #f
             (-> -Pathlike Univ))))
(call-with-file-lock/timeout
 (-poly
  (a)
  (->key
   (-opt -Pathlike)
   (one-of/c 'shared 'exclusive)
   (-> a)
   (-> a)
   #:lock-file
   (-opt -Pathlike)
   #f
   #:delay
   -Real
   #f
   #:max-delay
   -Real
   #f
   a)))
(sort
 (-poly
  (a b)
  (cl->*
   (->key (-lst a) (-> a a -Boolean) #:key (-> a a) #f #:cache-keys? -Boolean #f (-lst a))
   (->key (-lst a) (-> b b -Boolean) #:key (-> a b) #f #:cache-keys? -Boolean #f (-lst a)))))
(remove-duplicates
 (-poly
  (a b)
  (cl->*
   (->optkey (-lst a) ((-> a a Univ)) #:key (-> a a) #f (-lst a))
   (->optkey (-lst a) ((-> b b Univ)) #:key (-> a b) #f (-lst a)))))
(open-input-file (->key -Pathlike
                        #:mode (one-of/c 'binary 'text) #f
                        #:for-module? Univ #f
                        -Input-Port))
(open-output-file
 (->key
  -Pathlike
  #:mode
  (one-of/c 'binary 'text)
  #f
  #:exists
  (one-of/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace)
  #f
  -Output-Port))
(open-input-output-file
 (->key
  -Pathlike
  #:mode
  (one-of/c 'binary 'text)
  #f
  #:exists
  (one-of/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace)
  #f
  (-values (list -Input-Port -Output-Port))))
(call-with-input-file
    (-poly (a) (->key -Pathlike (-> -Input-Port a) #:mode (Un (-val 'binary) (-val 'text)) #f a)))
(call-with-output-file
    (-poly (a)
     (->key
      -Pathlike
      (-> -Output-Port a)
      #:exists
      (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace 'can-update 'must-truncate)
      #f
      #:mode
      (one-of/c 'binary 'text)
      #f
      a)))
(call-with-input-file* (-poly (a) (->key -Pathlike (-> -Input-Port a) #:mode (Un (-val 'binary) (-val 'text)) #f a)))
(call-with-output-file*
 (-poly
  (a)
  (->key
   -Pathlike
   (-> -Output-Port a)
   #:exists
   (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace 'can-update 'must-truncate)
   #f
   #:mode
   (one-of/c 'binary 'text)
   #f
   a)))
(with-input-from-file (-poly (a) (->key -Pathlike (-> a) #:mode (Un (-val 'binary) (-val 'text)) #f a)))
(with-output-to-file
    (-poly
     (a)
     (->key
      -Pathlike
      (-> a)
      #:exists
      (one-of/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace)
      #f
      #:mode
      (one-of/c 'binary 'text)
      #f
      a)))
(port->lines
 (->optkey [-Input-Port] #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -String)))
(port->bytes-lines
  (->optkey [-Input-Port] #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -Bytes)))
(display-lines
 (->optkey (-lst Univ) [-Output-Port] #:separator Univ #f -Void))
(find-relative-path (->key -SomeSystemPathlike -SomeSystemPathlike #:more-than-root? Univ #f -SomeSystemPath))
(regexp-match*
 (let ((N -Integer)
       (?N (-opt -Integer))
       (-StrRx (Un -String -Regexp))
       (-BtsRx (Un -Bytes -Byte-Regexp))
       (-StrInput (Un -String -Path))
       (-BtsInput (Un -Input-Port -Bytes))
       (sel (λ (t) (-opt (-> (-lst t) t)))))
   (cl->*
    (->optkey -StrRx -StrInput (N ?N -Bytes)
              #:match-select (sel -String) #f #:gap-select Univ #f
              (-lst -String))
    (->optkey -BtsRx (Un -StrInput -BtsInput) (N ?N -Bytes)
              #:match-select (sel -Bytes) #f #:gap-select Univ #f
              (-lst -Bytes))
    (->optkey -Pattern -BtsInput (N ?N -Bytes)
              #:match-select (sel -Bytes) #f #:gap-select Univ #f
              (-lst -Bytes)))))
(regexp-match-positions*
 (let* ((?outp (-opt -Output-Port))
        (B -Boolean)
        (N -Integer)
        (?N (-opt -Integer))
        (ind-pair (-pair -Index -Index))
        (sel (-> (-lst (-opt ind-pair)) (-opt ind-pair)))
        (output (-opt (-pair ind-pair (-lst (-opt ind-pair)))))
        (-Input (Un -String -Input-Port -Bytes -Path)))
   (->optkey -Pattern -Input (N ?N -Bytes) #:match-select sel #f output)))
