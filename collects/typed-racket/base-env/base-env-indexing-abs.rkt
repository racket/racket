#lang racket/base

(require
 "../utils/utils.rkt"
 (for-template racket/base racket/list racket/unsafe/ops racket/flonum)
 (utils tc-utils) 
 (rename-in (types union convenience abbrev numeric-tower) [-Number N] [-Boolean B] [-Symbol Sym]))

(provide indexing)

(define-syntax-rule (make-env* [i t] ...) (make-env [i (λ () t)] ...))

(define-syntax-rule (indexing index-type)
  (make-env*

   [build-list (-poly (a) (index-type (-Index . -> . a) . -> . (-lst a)))]
   [make-list (-poly (a) (index-type a . -> . (-lst a)))]

   ;Section 3.3

   [make-string (cl-> [(index-type) -String] [(index-type -Char) -String])]
   [string-ref (-> -String index-type -Char)]
   [string-set! (-String index-type -Char . -> . -Void)]
   [substring (->opt -String index-type [index-type] -String)]
   [string-copy! (-String index-type -String [index-type index-type] . ->opt . -Void)]
   [build-string (index-type (index-type . -> . -Char) . -> . -String)]



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
          [optlist (lambda (t) (-opt (-pair t (-lst (-opt t)))))]
          [-StrRx  (Un -String -Regexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp)]
          [-StrInput (Un -String -Path)]
          [-BtsInput (Un -Input-Port -Bytes)])
      (cl->*
       (-StrRx -StrInput [N ?N ?outp -Bytes] . ->opt . (optlist -String))
       (-BtsRx (Un -StrInput -BtsInput) [N ?N ?outp -Bytes] . ->opt . (optlist -Bytes))
       (-Pattern -BtsInput    [N ?N ?outp -Bytes] . ->opt . (optlist -Bytes))))]

   [regexp-match*
    (let ([N       index-type]
          [?N      (-opt index-type)]
          [-StrRx  (Un -String -Regexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp)]
          [-StrInput (Un -String -Path)]
          [-BtsInput (Un -Input-Port -Bytes)])
      (cl->*
       (-StrRx -StrInput [N ?N -Bytes] . ->opt . (-lst -String))
       (-BtsRx (Un -StrInput -BtsInput) [N ?N -Bytes] . ->opt . (-lst -Bytes))
       (-Pattern -BtsInput [N ?N -Bytes] . ->opt . (-lst -Bytes))))]

   [regexp-try-match
    (let ([?outp   (-opt -Output-Port)]
          [N       index-type]
          [?N      (-opt index-type)]
          [optlist (lambda (t) (-opt (-pair t (-lst (-opt t)))))]
          [-StrRx  (Un -String -Regexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp)])
       ((Un -BtsRx -StrRx) -Input-Port [N ?N ?outp -Bytes] . ->opt . (optlist -Bytes)))]


   [regexp-match-positions
    (let* ([?outp   (-opt -Output-Port)]
           [N       index-type]
           [?N      (-opt index-type)]
           [ind-pair (-pair -Index -Index)]
           [output (-opt (-pair ind-pair (-lst (-opt ind-pair))))]
           [-Input (Un -String -Input-Port -Bytes -Path)])
      (->opt -Pattern -Input [N ?N ?outp -Bytes] output))]


   [regexp-match-positions*
    (let* ([?outp   (-opt -Output-Port)]
           [N       index-type]
           [?N      (-opt index-type)]
           [ind-pair (-pair -Index -Index)]
           [output (-lst ind-pair)]
           [-Input (Un -String -Input-Port -Bytes -Path)])
      (->opt -Pattern -Input [N ?N ?outp -Bytes] output))]


   [regexp-match?
    (let ([?outp   (-opt -Output-Port)]
          [N       index-type]
          [?N      (-opt index-type)]
          [-Input (Un -String -Input-Port -Bytes -Path)])
       (-Pattern -Input [N ?N ?outp -Bytes] . ->opt . B))]




   [regexp-match-peek
    (let ([progress (-val #f)]
          [N       index-type]
          [?N      (-opt index-type)]
          [optlist (lambda (t) (-opt (-pair t (-lst (-opt t)))))])
       (-Pattern -Input-Port [N ?N progress -Bytes] . ->opt . (optlist -Bytes)))]


   [regexp-match-peek-positions
    (let* ([progress (-val #f)]
           [N       index-type]
           [?N      (-opt index-type)]
           [ind-pair (-pair -Index -Index)]
           [output (-opt (-pair ind-pair (-lst (-opt ind-pair))))])
      (->opt -Pattern -Input-Port [N ?N progress -Bytes] output))]


   [regexp-match-peek-immediate
    (let ([progress (-val #f)]
          [N       index-type]
          [?N      (-opt index-type)]
          [optlist (lambda (t) (-opt (-pair t (-lst (-opt t)))))])
       (-Pattern -Input-Port [N ?N progress -Bytes] . ->opt . (optlist -Bytes)))]


   [regexp-match-peek-positions-immediate
    (let* ([progress (-val #f)]
           [N       index-type]
           [?N      (-opt index-type)]
           [ind-pair (-pair -Index -Index)]
           [output (-opt (-pair ind-pair (-lst (-opt ind-pair))))])
      (->opt -Pattern -Input-Port [N ?N progress -Bytes] output))]



   [regexp-match-peek-positions*
    (let* ([progress (-val #f)]
           [N       index-type]
           [?N      (-opt index-type)]
           [ind-pair (-pair -Index -Index)]
           [output (-lst ind-pair)])
      (->opt -Pattern -Input-Port [N ?N progress -Bytes] output))]


   [regexp-match/end
    (let ([?outp   (-opt -Output-Port)]
          [N       index-type]
          [?N      (-opt index-type)]
          [optlist (lambda (t) (-opt (-pair t (-lst (-opt t)))))]
          [-StrRx  (Un -String -Regexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp)]
          [-StrInput (Un -String -Path)]
          [-BtsInput (Un -Input-Port -Bytes)])
      (cl->*
       (-StrRx -StrInput                [N ?N ?outp -Bytes N] . ->opt . (-values (list (optlist -String) (-opt -Bytes))))
       (-BtsRx (Un -StrInput -BtsInput) [N ?N ?outp -Bytes N] . ->opt . (-values (list (optlist -Bytes) (-opt -Bytes))))
       (-Pattern -BtsInput              [N ?N ?outp -Bytes N] . ->opt . (-values (list (optlist -Bytes) (-opt -Bytes))))))]


   [regexp-match-positions/end
    (let* ([?outp   (-opt -Output-Port)]
           [N       index-type]
           [?N      (-opt index-type)]
           [ind-pair (-pair -Index -Index)]
           [output (-opt (-pair ind-pair (-lst (-opt ind-pair))))]
           [-Input (Un -String -Input-Port -Bytes -Path)])
      (->opt -Pattern -Input [N ?N -Bytes N] (-values (list output (-opt -Bytes)))))]




   [regexp-match-peek-positions/end
    (let* ([progress (-val #f)]
           [N       index-type]
           [?N      (-opt index-type)]
           [ind-pair (-pair -Index -Index)]
           [output (-opt (-pair ind-pair (-lst (-opt ind-pair))))])
      (->opt -Pattern -Input-Port [N ?N progress -Bytes N] (-values (list output (-opt -Bytes)))))]


   [regexp-match-peek-positions-immediate/end
    (let* ([progress (-val #f)]
           [N       index-type]
           [?N      (-opt index-type)]
           [ind-pair (-pair -Index -Index)]
           [output (-opt (-pair ind-pair (-lst (-opt ind-pair))))])
      (->opt -Pattern -Input-Port [N ?N progress -Bytes N] (-values (list output (-opt -Bytes)))))]



   [regexp-split
    (let ([N       index-type]
          [?N      (-opt index-type)]
          [output (lambda (t) (-pair t (-lst t)))]
          [-StrRx  (Un -String -Regexp)]
          [-BtsRx  (Un -Bytes  -Byte-Regexp)]
          [-BtsInput (Un -Input-Port -Bytes)])
      (cl->*
       (-StrRx -String                [N ?N -Bytes] . ->opt . (output -String))
       (-BtsRx (Un -String -BtsInput) [N ?N -Bytes] . ->opt . (output -Bytes))
       (-Pattern -BtsInput            [N ?N -Bytes] . ->opt . (output -Bytes))))]












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

   ;; flvector ops

   [flvector? (make-pred-ty -FlVector)]
   [flvector (->* (list) -Flonum -FlVector)]
   [make-flvector (cl->* (-> index-type -FlVector)
                         (-> index-type -Flonum -FlVector))]

   [flvector-length (-> -FlVector -NonNegFixnum)]
   [flvector-ref (cl->* (-> -FlVector -NonNegFixnum -Flonum)
                        (-> -FlVector -Fixnum -Flonum)
                        (-> -FlVector index-type -Flonum))]
   [flvector-set! (cl->* (-> -FlVector -NonNegFixnum -Flonum -Void)
                         (-> -FlVector -Fixnum -Flonum -Void)
                         (-> -FlVector index-type -Flonum -Void))]

   [unsafe-flvector-length (-> -FlVector -NonNegFixnum)]
   [unsafe-flvector-ref (cl->* (-> -FlVector -NonNegFixnum -Flonum)
                               (-> -FlVector -Fixnum -Flonum)
                               (-> -FlVector index-type -Flonum))]
   [unsafe-flvector-set! (cl->* (-> -FlVector -NonNegFixnum -Flonum -Void)
                                (-> -FlVector -Fixnum -Flonum -Void)
                                (-> -FlVector index-type -Flonum -Void))]


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
   [integer-bytes->integer
    (cl->*
     ;; Any truthy value (not only #t) would work here.
     ;; We can define a truthy type (without difference types (- Univ #f))
     ;; by unioning everything (including StructTop and co).
     ;; We should do this at some point.
     (-Bytes (-val #t) [Univ index-type index-type] . ->opt . -Nat)
     (-Bytes Univ [Univ index-type index-type] . ->opt . -Integer))]

   [peek-char
    (cl->* [->opt [-Input-Port index-type] (Un -Char (-val eof))])]
   [peek-byte
    (cl->* [->opt [-Input-Port index-type] (Un -Byte (-val eof))])]

   ;; string.rkt
   [real->decimal-string (-Real [index-type] . ->opt .  -String)]


   [raise-type-error
    (cl->*
     [-> Sym -String Univ (Un)]
     [->* (list Sym -String index-type) Univ (Un)])]

   ))

