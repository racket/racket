#lang scheme

(require
 "../utils/utils.ss"
 scheme/tcp 
 scheme/unsafe/ops
 (only-in rnrs/lists-6 fold-left)
 '#%paramz
 "extra-procs.ss"
 (utils tc-utils )
 (types  union convenience)
 (only-in '#%kernel [apply kernel:apply])
 scheme/promise scheme/system
 (only-in string-constants/private/only-once maybe-print-message)
 (only-in scheme/match/runtime match:error matchable? match-equality-test)
 (for-template scheme) 
 (rename-in (types abbrev) [-Number N] [-Boolean B] [-Symbol Sym] [-Nat -Nat*]))

(provide indexing)

(define-syntax-rule (indexing -Nat)
  (make-env
   
   [build-list (-poly (a) (-Nat (-Nat* . -> . a) . -> . (-lst a)))]
   [make-list (-poly (a) (-Nat a . -> . (-lst a)))]
   
   [string-ref (-> -String -Nat -Char)]
   [substring (->opt -String -Nat [-Nat] -String)]
   [make-string (cl-> [(-Nat) -String] [(-Nat -Char) -String])]
   [string-set! (-String -Nat -Char . -> . -Void)]
   
   [list-ref  (-poly (a) ((-lst a) -Nat . -> . a))]
   [list-tail (-poly (a) ((-lst a) -Nat . -> . (-lst a)))]
   
   [regexp-match
    (let ([?outp   (-opt -Output-Port)]
          [N       -Nat]
          [?N      (-opt -Nat)]
          [optlist (lambda (t) (-opt (-lst (-opt t))))]
          [-StrRx  (Un -String -Regexp -PRegexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
          [-InpBts (Un -Input-Port -Bytes)])
      (cl->*
       (-StrRx   -String [N ?N ?outp] . ->opt . (optlist -String))
       (-BtsRx   -String [N ?N ?outp] . ->opt . (optlist -Bytes))
       (-Pattern -InpBts [N ?N ?outp] . ->opt . (optlist -Bytes))))]
   [regexp-match?
    (let ([?outp   (-opt -Output-Port)]
          [N       -Nat]
          [?N      (-opt -Nat)]
          [optlist (lambda (t) (-opt (-lst (-opt t))))]
          [-StrRx  (Un -String -Regexp -PRegexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
          [-InpBts (Un -Input-Port -Bytes)])
      (cl->*
       (-StrRx   -String [N ?N ?outp] . ->opt . -Boolean)
       (-BtsRx   -String [N ?N ?outp] . ->opt . -Boolean)
       (-Pattern -InpBts [N ?N ?outp] . ->opt . -Boolean)))]
   [regexp-match*
    (let ([N       -Nat]
          [?N      (-opt -Nat)]
          [-StrRx  (Un -String -Regexp -PRegexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
          [-InpBts (Un -Input-Port -Bytes)])
      (cl->*
       (-StrRx   -String [N ?N] . ->opt . (-lst -String))
       (-BtsRx   -String [N ?N] . ->opt . (-lst -Bytes))
       (-Pattern -InpBts [N ?N] . ->opt . (-lst -Bytes))))]
   [regexp-try-match
    (let ([?outp   (-opt -Output-Port)]
          [?N      (-opt -Nat)]
          [optlist (lambda (t) (-opt (-lst (-opt t))))])
      (->opt -Pattern -Input-Port [-Nat ?N ?outp] (optlist -Bytes)))]
   
   [regexp-match-positions
    (let ([?outp   (-opt -Output-Port)]
          [N       -Nat]
          [?N      (-opt -Nat)]
          [optlist (lambda (t) (-opt (-lst (-opt t))))]
          [-StrRx  (Un -String -Regexp -PRegexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
          [-InpBts (Un -Input-Port -Bytes)])
      (->opt -Pattern (Un -String -InpBts) [N ?N ?outp] (optlist (-pair -Nat -Nat))))]
   [regexp-match-positions*
    (let ([?outp   (-opt -Output-Port)]
          [?N      (-opt -Nat)]
          [optlist (lambda (t) (-opt (-lst (-opt t))))]
          [-StrRx  (Un -String -Regexp -PRegexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
          [-InpBts (Un -Input-Port -Bytes)])
      (->opt -Pattern (Un -String -InpBts) [-Nat ?N ?outp] (-lst (-pair -Nat -Nat))))]
   
   
   [take   (-poly (a) ((-lst a) -Nat . -> . (-lst a)))]
   [drop   (-poly (a) ((-lst a) -Nat . -> . (-lst a)))]
   [take-right   (-poly (a) ((-lst a) -Nat . -> . (-lst a)))]
   [drop-right   (-poly (a) ((-lst a) -Nat . -> . (-lst a)))]
   [split-at
    (-poly (a) ((list (-lst a)) -Nat . ->* . (-values (list (-lst a) (-lst a)))))]
   [split-at-right
    (-poly (a) ((list (-lst a)) -Nat . ->* . (-values (list (-lst a) (-lst a)))))]
   
   [vector-ref (-poly (a) ((-vec a) -Nat . -> . a))]
   [build-vector (-poly (a) (-Nat (-Nat . -> . a) . -> . (-vec a)))]
   [vector-set! (-poly (a) (-> (-vec a) -Nat a -Void))]
   [vector-copy! (-poly (a) ((-vec a) -Nat (-vec a) [-Nat -Nat] . ->opt . -Void))]
   [make-vector (-poly (a) (cl-> [(-Nat) (-vec -Integer)]
                                 [(-Nat a) (-vec a)]))]
   
   [peek-char
    (cl->* [->opt [-Input-Port -Nat] (Un -Char (-val eof))])]
   [peek-byte
    (cl->* [->opt [-Input-Port -Nat] (Un -Byte (-val eof))])]
   
   ;; string.ss
   [real->decimal-string (N [-Nat] . ->opt .  -String)]
   
   [random (cl-> [(-Nat) -Nat*] [() -Real])]
   
   [raise-type-error
    (cl->
     [(Sym -String Univ) (Un)]
     [(Sym -String -Nat (-lst Univ)) (Un)])]
   
   ))
   