#lang racket

(require
 "../utils/utils.rkt"
 (for-template '#%paramz racket/base racket/list
               racket/tcp
               (only-in rnrs/lists-6 fold-left)
               '#%paramz
               (only-in '#%kernel [apply kernel:apply])
               racket/promise racket/system
               (only-in string-constants/private/only-once maybe-print-message)
               (only-in racket/match/runtime match:error matchable? match-equality-test)
               racket/unsafe/ops)
 (utils tc-utils)
 (types union convenience)
 (rename-in (types abbrev numeric-tower) [-Number N] [-Boolean B] [-Symbol Sym]))

(provide indexing)

(define-syntax-rule (indexing index-type)
  (make-env

   [build-list (-poly (a) (index-type (-Index . -> . a) . -> . (-lst a)))]
   [make-list (-poly (a) (index-type a . -> . (-lst a)))]

   [string-ref (-> -String index-type -Char)]
   [substring (->opt -String index-type [index-type] -String)]
   [make-string (cl-> [(index-type) -String] [(index-type -Char) -String])]
   [string-set! (-String index-type -Char . -> . -Void)]
   [string-copy! (-String index-type -String [index-type index-type] . ->opt . -Void)]

   [read-string (index-type [-Input-Port] . ->opt . (Un -String (-val eof)))]
   [read-string! (-String [-Input-Port index-type index-type] . ->opt . (Un -Index (-val eof)))]
   [read-bytes (index-type [-Input-Port] . ->opt . (Un -Bytes (-val eof)))]

   [write-byte (cl-> [(index-type) -Void]
                     [(index-type -Output-Port) -Void])]
   [write-string (cl-> [(-String) -Index]
                       [(-String -Output-Port) -Index]
                       [(-String -Output-Port index-type) -Index]
                       [(-String -Output-Port index-type index-type) -Index])]
   [write-bytes  (cl-> [(-Bytes) -Index]
                       [(-Bytes -Output-Port) -Index]
                       [(-Bytes -Output-Port index-type) -Index]
                       [(-Bytes -Output-Port index-type index-type) -Index])]
   [write-bytes-avail  (cl-> [(-Bytes) -Index]
                             [(-Bytes -Output-Port) -Index]
                             [(-Bytes -Output-Port index-type) -Index]
                             [(-Bytes -Output-Port index-type index-type) -Index])]
   [write-bytes-avail*  (cl-> [(-Bytes) (-opt -Index)]
                              [(-Bytes -Output-Port) (-opt -Index)]
                              [(-Bytes -Output-Port index-type) (-opt -Index)]
                              [(-Bytes -Output-Port index-type index-type) (-opt -Index)])]
   [write-bytes-avail/enable-break (cl-> [(-Bytes) -Index]
                                         [(-Bytes -Output-Port) -Index]
                                         [(-Bytes -Output-Port index-type) -Index]
                                         [(-Bytes -Output-Port index-type index-type) -Index])]



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
      (->opt -Pattern (Un -String -InpBts) [N ?N ?outp] (optlist (-pair -Index -Index))))]
   [regexp-match-positions*
    (let ([?outp   (-opt -Output-Port)]
          [?N      (-opt index-type)]
          [optlist (lambda (t) (-opt (-lst (-opt t))))]
          [-StrRx  (Un -String -Regexp -PRegexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
          [-InpBts (Un -Input-Port -Bytes)])
      (->opt -Pattern (Un -String -InpBts) [index-type ?N ?outp] (-lst (-pair -Index -Index))))]


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
   [build-vector (-poly (a) (index-type (-Index . -> . a) . -> . (-vec a)))]
   [vector-set! (-poly (a) (-> (-vec a) index-type a -Void))]
   [unsafe-vector-set! (-poly (a) (-> (-vec a) index-type a -Void))]
   [unsafe-vector*-set! (-poly (a) (-> (-vec a) index-type a -Void))]
   [vector-copy! (-poly (a) ((-vec a) index-type (-vec a) [index-type index-type] . ->opt . -Void))]
   [make-vector (-poly (a) (cl-> [(index-type) (-vec (Un -Integer a))]
                                 [(index-type a) (-vec a)]))]

   [bytes-ref (-> -Bytes index-type -Byte)]
   [unsafe-bytes-ref (-> -Bytes index-type -Byte)]
   [bytes-set! (-> -Bytes index-type index-type -Void)]
   [unsafe-bytes-set! (-> -Bytes index-type index-type -Void)]
   [subbytes (cl-> [(-Bytes index-type) -Bytes] [(-Bytes index-type index-type) -Bytes])]
   [bytes-copy! (-Bytes index-type -Bytes [index-type index-type] . ->opt . -Void)]
   [bytes-fill! (-> -Bytes index-type -Void)]
   [bytes->string/utf-8 (-Bytes [(Un (-val #f) -Char) index-type index-type] . ->opt . -String)]
   [bytes->string/locale (-Bytes [(Un (-val #f) -Char) index-type index-type] . ->opt . -String)]
   [bytes->string/latin-1 (-Bytes [(Un (-val #f) -Char) index-type index-type] . ->opt . -String)]
   [string->bytes/utf-8 (-String [(Un (-val #f) index-type) index-type index-type] . ->opt . -Bytes)]
   [string->bytes/locale (-String [(Un (-val #f) index-type) index-type index-type] . ->opt . -Bytes)]
   [string->bytes/latin-1 (-String [(Un (-val #f) index-type) index-type index-type] . ->opt . -Bytes)]
   [string-utf-8-length (-String [index-type index-type] . ->opt . -Index)]
   [bytes-utf-8-length (-Bytes [(Un (-val #f) -Char) index-type index-type] . ->opt . -Index)]
   [bytes-utf-8-ref (-Bytes [index-type (Un (-val #f) -Char) index-type index-type] . ->opt . -Char)]
   [bytes-utf-8-index (-Bytes [index-type (Un (-val #f) -Char) index-type index-type] . ->opt . -Index)]

   [integer->integer-bytes (-Integer index-type Univ [Univ -Bytes index-type] . ->opt . -Bytes)]
   [integer-bytes->integer (-Bytes Univ [Univ index-type index-type] . ->opt . -Integer)]

   [peek-char
    (cl->* [->opt [-Input-Port index-type] (Un -Char (-val eof))])]
   [peek-byte
    (cl->* [->opt [-Input-Port index-type] (Un -Byte (-val eof))])]

   ;; string.rkt
   [real->decimal-string (-Real [index-type] . ->opt .  -String)]

   [random (cl-> [(index-type) -Nat] [() -Real])]

   [raise-type-error
    (cl->*
     [-> Sym -String Univ (Un)]
     [->* (list Sym -String index-type) Univ (Un)])]

   ))

