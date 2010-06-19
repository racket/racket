#lang racket

(require
 "../utils/utils.rkt"
 racket/tcp 
 (only-in rnrs/lists-6 fold-left)
 '#%paramz
 "extra-procs.rkt"
 (utils tc-utils )
 (types  union convenience)
 (only-in '#%kernel [apply kernel:apply])
 racket/promise racket/system
 (only-in string-constants/private/only-once maybe-print-message)
 (only-in racket/match/runtime match:error matchable? match-equality-test)
 (for-template racket racket/unsafe/ops)
 (rename-in (types abbrev) [-Number N] [-Boolean B] [-Symbol Sym]))

(provide indexing)

(define-syntax-rule (indexing index-type)
  (make-env
   
   [build-list (-poly (a) (index-type (-Nat . -> . a) . -> . (-lst a)))]
   [make-list (-poly (a) (index-type a . -> . (-lst a)))]
   
   [string-ref (-> -String index-type -Char)]
   [substring (->opt -String index-type [index-type] -String)]
   [make-string (cl-> [(index-type) -String] [(index-type -Char) -String])]
   [string-set! (-String index-type -Char . -> . -Void)]
   [string-copy! (-String index-type -String [index-type index-type] . ->opt . -Void)]

   [read-string (index-type [-Input-Port] . ->opt . (Un -String (-val eof)))]
   [read-string! (-String [-Input-Port index-type index-type] . ->opt . (Un -Nat (-val eof)))]
   [read-bytes (index-type [-Input-Port] . ->opt . (Un -Bytes (-val eof)))]

   [write-byte (cl-> [(index-type) -Void]
                     [(index-type -Output-Port) -Void])]
   [write-string (cl-> [(-String) -Nat]
                       [(-String -Output-Port) -Nat]
                       [(-String -Output-Port index-type) -Nat]
                       [(-String -Output-Port index-type index-type) -Nat])]
   [write-bytes  (cl-> [(-Bytes) -Nat]
                       [(-Bytes -Output-Port) -Nat]
                       [(-Bytes -Output-Port index-type) -Nat]
                       [(-Bytes -Output-Port index-type index-type) -Nat])]
   [write-bytes-avail  (cl-> [(-Bytes) -Nat]
                             [(-Bytes -Output-Port) -Nat]
                             [(-Bytes -Output-Port index-type) -Nat]
                             [(-Bytes -Output-Port index-type index-type) -Nat])]
   [write-bytes-avail*  (cl-> [(-Bytes) (-opt -Nat)]
                              [(-Bytes -Output-Port) (-opt -Nat)]
                              [(-Bytes -Output-Port index-type) (-opt -Nat)]
                              [(-Bytes -Output-Port index-type index-type) (-opt -Nat)])]
   [write-bytes-avail/enable-break (cl-> [(-Bytes) -Nat]
                                         [(-Bytes -Output-Port) -Nat]
                                         [(-Bytes -Output-Port index-type) -Nat]
                                         [(-Bytes -Output-Port index-type index-type) -Nat])]


   
   [list-ref  (-poly (a) ((-lst a) index-type . -> . a))]
   [list-tail (-poly (a) ((-lst a) index-type . -> . (-lst a)))]
   
   [regexp-match
    (let ([?outp   (-opt -Output-Port)]
          [N       index-type]
          [?N      (-opt index-type)]
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
          [N       index-type]
          [?N      (-opt index-type)]
          [optlist (lambda (t) (-opt (-lst (-opt t))))]
          [-StrRx  (Un -String -Regexp -PRegexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
          [-InpBts (Un -Input-Port -Bytes)])
      (cl->*
       (-StrRx   -String [N ?N ?outp] . ->opt . -Boolean)
       (-BtsRx   -String [N ?N ?outp] . ->opt . -Boolean)
       (-Pattern -InpBts [N ?N ?outp] . ->opt . -Boolean)))]
   [regexp-match*
    (let ([N       index-type]
          [?N      (-opt index-type)]
          [-StrRx  (Un -String -Regexp -PRegexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
          [-InpBts (Un -Input-Port -Bytes)])
      (cl->*
       (-StrRx   -String [N ?N] . ->opt . (-lst -String))
       (-BtsRx   -String [N ?N] . ->opt . (-lst -Bytes))
       (-Pattern -InpBts [N ?N] . ->opt . (-lst -Bytes))))]
   [regexp-try-match
    (let ([?outp   (-opt -Output-Port)]
          [?N      (-opt index-type)]
          [optlist (lambda (t) (-opt (-lst (-opt t))))])
      (->opt -Pattern -Input-Port [index-type ?N ?outp] (optlist -Bytes)))]
   
   [regexp-match-positions
    (let ([?outp   (-opt -Output-Port)]
          [N       index-type]
          [?N      (-opt index-type)]
          [optlist (lambda (t) (-opt (-lst (-opt t))))]
          [-StrRx  (Un -String -Regexp -PRegexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
          [-InpBts (Un -Input-Port -Bytes)])
      (->opt -Pattern (Un -String -InpBts) [N ?N ?outp] (optlist (-pair -Nat -Nat))))]
   [regexp-match-positions*
    (let ([?outp   (-opt -Output-Port)]
          [?N      (-opt index-type)]
          [optlist (lambda (t) (-opt (-lst (-opt t))))]
          [-StrRx  (Un -String -Regexp -PRegexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
          [-InpBts (Un -Input-Port -Bytes)])
      (->opt -Pattern (Un -String -InpBts) [index-type ?N ?outp] (-lst (-pair -Nat -Nat))))]
   
   
   [take   (-poly (a) ((-lst a) index-type . -> . (-lst a)))]
   [drop   (-poly (a) ((-lst a) index-type . -> . (-lst a)))]
   [take-right   (-poly (a) ((-lst a) index-type . -> . (-lst a)))]
   [drop-right   (-poly (a) ((-lst a) index-type . -> . (-lst a)))]
   [split-at
    (-poly (a) ((list (-lst a)) index-type . ->* . (-values (list (-lst a) (-lst a)))))]
   [split-at-right
    (-poly (a) ((list (-lst a)) index-type . ->* . (-values (list (-lst a) (-lst a)))))]
   
   [vector-ref (-poly (a) ((-vec a) index-type . -> . a))]
   [unsafe-vector-ref (-poly (a) ((-vec a) index-type . -> . a))]
   [unsafe-vector*-ref (-poly (a) ((-vec a) index-type . -> . a))]
   [build-vector (-poly (a) (index-type (index-type . -> . a) . -> . (-vec a)))]
   [vector-set! (-poly (a) (-> (-vec a) index-type a -Void))]
   [vector-copy! (-poly (a) ((-vec a) index-type (-vec a) [index-type index-type] . ->opt . -Void))]
   [make-vector (-poly (a) (cl-> [(index-type) (-vec -Nat)]
                                 [(index-type a) (-vec a)]))]
   
   [peek-char
    (cl->* [->opt [-Input-Port index-type] (Un -Char (-val eof))])]
   [peek-byte
    (cl->* [->opt [-Input-Port index-type] (Un -Byte (-val eof))])]
   
   ;; string.rkt
   [real->decimal-string (N [index-type] . ->opt .  -String)]
   
   [random (cl-> [(index-type) -Nat] [() -Real])]
   
   [raise-type-error
    (cl->
     [(Sym -String Univ) (Un)]
     [(Sym -String index-type (-lst Univ)) (Un)])]
   
   ))
 
