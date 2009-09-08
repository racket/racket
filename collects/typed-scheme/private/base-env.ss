#lang s-exp "env-lang.ss"

(require
 scheme/list
 scheme/tcp
 scheme
 scheme/unsafe/ops
 (only-in rnrs/lists-6 fold-left)
 '#%paramz
 (only-in '#%kernel [apply kernel:apply])
 scheme/promise
 (only-in string-constants/private/only-once maybe-print-message)
 (only-in scheme/match/runtime match:error matchable? match-equality-test)
 (for-syntax (only-in (types abbrev) [-Number N] [-Boolean B] [-Symbol Sym])))

[raise (Univ . -> . (Un))]

[car   (-poly (a b) 
              (cl->*
               (->acc (list (-pair a b)) a (list -car))
               (->* (list (-lst a)) a)))]
[cdr   (-poly (a b) 
              (cl->*
               (->acc (list (-pair a b)) b (list -cdr))
               (->* (list (-lst a)) (-lst a))))]

[cadr (-poly (a b c)
             (cl-> [((-pair a (-pair b c))) b]
                   [((-lst a)) a]))]
[caddr  (-poly (a) (-> (-lst a) a))]
[cadddr (-poly (a) (-> (-lst a) a))]
[cddr  (-poly (a) (-> (-lst a) (-lst a)))]
[cdddr (-poly (a) (-> (-lst a) (-lst a)))]

[first (-poly (a b) (cl-> [((-pair a b)) a] [((-lst a)) a]))]
[second (-poly (a b c)
               (cl-> [((-pair a (-pair b c))) b]
                     [((-lst a)) a]))]
[third (-poly (a b c d)
              (cl-> [((-pair a (-pair b (-pair c d)))) c]
                    [((-lst a)) a]))]
[fourth (-poly (a) ((-lst a) . -> .  a))]
[fifth  (-poly (a) ((-lst a) . -> .  a))]
[sixth  (-poly (a) ((-lst a) . -> .  a))]
[rest (-poly (a) ((-lst a) . -> .  (-lst a)))]

[cons (-poly (a b)
             (cl-> [(a (-lst a)) (-lst a)]
                   [(a b) (-pair a b)]))]
[*cons (-poly (a b) (cl->
                     [(a b) (-pair a b)]
                     [(a (-lst a)) (-lst a)]))]
[*list? (make-pred-ty (-lst Univ))]

[null? (make-pred-ty (-val null))]
[eof-object? (make-pred-ty (-val eof))]
[null (-val null)]
[number? (make-pred-ty N)]
[char? (make-pred-ty -Char)]
[integer? (Univ . -> . B : (-LFS (list (-filter N)) (list (-not-filter -Integer))))]
[exact-integer? (make-pred-ty -Integer)]
[boolean? (make-pred-ty B)]
[add1 (cl->* (-> -Integer -Integer)
             (-> N N))]
[sub1 (cl->* (-> -Integer -Integer)
             (-> N N))]
[eq? (-> Univ Univ B)]
[eqv? (-> Univ Univ B)]
[equal? (-> Univ Univ B)]
[even? (-> N B)]
[assert (-poly (a) (-> (Un a (-val #f)) a))]
[gensym (cl-> [(Sym) Sym]
              [() Sym])]
[string-append (->* null -String -String)]
[open-input-string (-> -String -Input-Port)]
[open-output-file
 (->key -Pathlike
        #:mode (one-of/c 'binary 'text) #f
        #:exists (one-of/c 'error 'append 'update 'can-update
                           'replace 'truncate
                           'must-truncate 'truncate/replace)
        #f
        -Output-Port)]
[read (cl->
       [(-Port) -Sexp]
       [() -Sexp])]
[ormap (-polydots (a c b) (->... (list (->... (list a) (b b) c) (-lst a)) ((-lst b) b) c))]
[andmap (-polydots (a c b) (->... (list (->... (list a) (b b) c) (-lst a)) ((-lst b) b) c))]
[newline (cl-> [() -Void]
               [(-Port) -Void])]
[not (-> Univ B)]
[box (-poly (a) (a . -> . (-box a)))]
[unbox (-poly (a) ((-box a) . -> . a))]
[set-box! (-poly (a) ((-box a) a . -> . -Void))]
[box? (make-pred-ty (-box Univ))]
[cons? (make-pred-ty (-pair Univ Univ))]
[pair? (make-pred-ty (-pair Univ Univ)) #;(-poly (a b) (make-pred-ty (-pair a b)))]
[empty? (make-pred-ty (-val null))]
[empty (-val null)]
[string? (make-pred-ty -String)]
[string (->* '() -Char -String)]
[symbol? (make-pred-ty Sym)]
[keyword? (make-pred-ty -Keyword)]
[list? (make-pred-ty (-lst Univ))]
[list (-poly (a) (->* '() a (-lst a)))]
[procedure? (make-pred-ty top-func)]
[map (-polydots (c a b) ((list ((list a) (b b) . ->... . c) (-lst a))
                         ((-lst b) b) . ->... .(-lst c)))]
[for-each (-polydots (c a b) ((list ((list a) (b b) . ->... . Univ) (-lst a))
                              ((-lst b) b) . ->... . -Void))]
[fold-left (-polydots (c a b) ((list ((list c a) (b b) . ->... . c) c (-lst a))
                               ((-lst b) b) . ->... . c))]
[fold-right (-polydots (c a b) ((list ((list c a) (b b) . ->... . c) c (-lst a))
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
                      ((make-pred-ty (list a) B b)
                       (-lst a)
                       . -> .
                       (-lst b))
                      ((a . -> . B) (-lst a) . -> . (-lst a))))]
[filter-map (-polydots (c a b)
                       ((list
                         ((list a) (b b) . ->... . (-opt c))
                         (-lst a))
                        ((-lst b) b) . ->... . (-lst c)))]
[take   (-poly (a) ((-lst a) -Integer . -> . (-lst a)))]
[drop   (-poly (a) ((-lst a) -Integer . -> . (-lst a)))]
[last   (-poly (a) ((-lst a) . -> . a))]
[add-between (-poly (a b) ((-lst a) b . -> . (-lst (Un a b))))]
[remove  (-poly (a) (a (-lst a) . -> . (-lst a)))]
[remq    (-poly (a) (a (-lst a) . -> . (-lst a)))]
[remv    (-poly (a) (a (-lst a) . -> . (-lst a)))]
[remove* (-poly (a b) (cl-> [((-lst a) (-lst a)) (-lst a)]
                            [((-lst a) (-lst b) (a b . -> . B)) (-lst b)]))]
[remq*   (-poly (a b) (cl-> [((-lst a) (-lst a)) (-lst a)]
                            [((-lst a) (-lst b) (a b . -> . B)) (-lst b)]))]
[remv*   (-poly (a b) (cl-> [((-lst a) (-lst a)) (-lst a)]
                            [((-lst a) (-lst b) (a b . -> . B)) (-lst b)]))]

(error 
 (make-Function (list 
                 (make-arr (list Sym -String) (Un) #:rest Univ)
                 (make-arr (list -String) (Un) #:rest Univ)
                 (make-arr (list Sym) (Un)))))

[namespace-variable-value
 (cl-> [(Sym) Univ]
       [(Sym B -Namespace (-> Univ)) Univ])]

[match:error (Univ . -> . (Un))]
[match-equality-test (-Param (Univ Univ . -> . Univ) (Univ Univ . -> . Univ))]
[matchable? (make-pred-ty (Un -String -Bytes))]
[display (cl-> [(Univ) -Void] [(Univ -Port) -Void])]
[write   (cl-> [(Univ) -Void] [(Univ -Port) -Void])]
[print   (cl-> [(Univ) -Void] [(Univ -Port) -Void])]
[void (->* '() Univ -Void)]
[void? (make-pred-ty -Void)]
[printf (->* (list -String) Univ -Void)]
[fprintf (->* (list -Output-Port -String) Univ -Void)]
[format (->* (list -String) Univ -String)]
[fst (-poly (a b) (-> (-pair a b) a))]
[snd (-poly (a b) (-> (-pair a b) b))]

[sleep (N . -> . -Void)]

[=  (->* (list N N) N B)]
[>= (->* (list N N) N B)]
[<  (->* (list N N) N B)]
[<= (->* (list N N) N B)]
[>  (->* (list N N) N B)]
[zero? (N . -> . B)]
[* (cl->* (->* '() -Integer -Integer) (->* '() N N))]
[/ (cl->* (->* (list N) N N))]
[+ (cl->* (->* '() -Integer -Integer) (->* '() N N))]
[- (cl->* (->* (list -Integer) -Integer -Integer) (->* (list N) N N))]
[max (->* (list N) N N)]
[min (->* (list N) N N)]
[vector? (make-pred-ty (-vec Univ))]
[vector-ref (-poly (a) ((-vec a) N . -> . a))]
[build-vector (-poly (a) (-Integer (-Integer . -> . a) . -> . (-vec a)))]
[build-list (-poly (a) (-Integer (-Integer . -> . a) . -> . (-lst a)))]
[reverse (-poly (a) (-> (-lst a) (-lst a)))]
[append (-poly (a) (->* (list) (-lst a) (-lst a)))]
[length (-poly (a) (-> (-lst a) -Integer))]
[memq (-poly (a) (-> a (-lst a) (-opt (-lst a))))]
[memv (-poly (a) (-> a (-lst a) (-opt (-lst a))))]
[memf (-poly (a) ((a . -> . B) (-lst a) . -> . (-opt (-lst a))))]
[member (-poly (a) (a (-lst a) . -> . (-opt (-lst a))))]
[findf (-poly (a) ((a . -> . B) (-lst a) . -> . (-opt a)))]

[string<? (->* (list -String -String) -String B)]
[string>? (->* (list -String -String) -String B)]
[string=? (->* (list -String -String) -String B)]
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
[string<=? (->* (list -String -String) -String B)]
[string>=? (->* (list -String -String) -String B)]

[string-ci<? (->* (list -String -String) -String B)]
[string-ci>? (->* (list -String -String) -String B)]
[string-ci=? (->* (list -String -String) -String B)]
[string-ci<=? (->* (list -String -String) -String B)]
[string-ci>=? (->* (list -String -String) -String B)]

[string-upcase (-> -String -String)]
[string-downcase (-> -String -String)]
[string-titlecase (-> -String -String)]
[string-foldcase (-> -String -String)]
[char-upcase (-> -Char -Char)]
[char-downcase (-> -Char -Char)]
[char-titlecase (-> -Char -Char)]
[char-foldcase (-> -Char -Char)]

[string-normalize-nfd (-> -String -String)]
[string-normalize-nfkd (-> -String -String)]
[string-normalize-nfc (-> -String -String)]
[string-normalize-nfkc (-> -String -String)]

[string-copy (-> -String -String)]
[string->immutable-string (-> -String -String)]
[string-ref (-> -String N -Char)]
[substring (cl->*
            (-> -String N -String)
            (-> -String N N -String))]
[string->path (-> -String -Path)]
[file-exists? (-> -Pathlike B)]

[build-path ((list -Pathlike*) -Pathlike* . ->* . -Path)]
[string->number (-> -String (-opt N))]
[with-input-from-file
 (-poly (a) (cl-> [(-Pathlike  (-> a))  a]
                  [(-Pathlike (-> a) Sym) a]))]
[with-output-to-file
 (-poly (a) (cl-> [(-Pathlike  (-> a))  a]
                  [(-Pathlike (-> a) Sym) a]))]

[random (cl-> [(-Integer) -Integer] [() N])]

[assq  (-poly (a b) (a (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assv  (-poly (a b) (a (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assoc (-poly (a b) (a (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assf  (-poly (a b) ((a . -> . Univ) (-lst (-pair a b))
                     . -> . (-opt (-pair a b))))]

[list-ref  (-poly (a) ((-lst a) -Integer . -> . a))]
[list-tail (-poly (a) ((-lst a) -Integer . -> . (-lst a)))]
[positive? (-> N B)]
[negative? (-> N B)]
[odd? (-> -Integer B)]
[even? (-> -Integer B)]

[apply        (-poly (a b) (((list) a . ->* . b) (-lst a) . -> . b))]
[kernel:apply (-poly (a b) (((list) a . ->* . b) (-lst a) . -> . b))]
[time-apply (-polydots (b a) 
		       (make-Function 
			(list (make-arr
			       (list ((list) (a a) . ->... . b)
				     (-lst a))                               
			       (-values (list (-pair b (-val '())) N N N))))))]

[call/cc (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (Un a b)))]
[call/ec (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (Un a b)))]
[call-with-current-continuation (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (Un a b)))]
[call-with-escape-continuation (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (Un a b)))]

[struct->vector (Univ . -> . (-vec Univ))]

[quotient (-Integer -Integer . -> . -Integer)]
[remainder (-Integer -Integer . -> . -Integer)]
[quotient/remainder 
 (make-Function (list (make-arr (list -Integer -Integer) (-values (list -Integer -Integer)))))]

;; parameter stuff

[parameterization-key Sym]
[extend-parameterization (-poly (a b) (-> Univ (-Param a b) a Univ))]
[continuation-mark-set-first (-> (-opt -Cont-Mark-Set) Univ Univ)]
[make-parameter (-poly (a b) (cl-> [(a) (-Param a a)]
                                   [(b (a . -> . b)) (-Param a b)]))]
[current-directory (-Param -Pathlike -Path)]
[current-namespace (-Param -Namespace -Namespace)]
[print-struct (-Param B B)]
[read-decimal-as-inexact (-Param B B)]
[current-command-line-arguments (-Param (-vec -String) (-vec -String))]

;; regexp stuff
[regexp? (make-pred-ty -Regexp)]
[pregexp? (make-pred-ty -PRegexp)]
[byte-regexp? (make-pred-ty -Byte-Regexp)]
[byte-pregexp? (make-pred-ty -Byte-PRegexp)]
[regexp (-String . -> . -Regexp)]
[pregexp (-String . -> . -PRegexp)]
[byte-regexp (-Bytes . -> . -Byte-Regexp)]
[byte-pregexp (-Bytes . -> . -Byte-PRegexp)]
[regexp-quote (cl-> [(-String) -String]
                    [(-String -Boolean) -String]
                    [(-Bytes) -Bytes]
                    [(-Bytes -Boolean) -Bytes])]
 
[regexp-match
 (let ([?outp   (-opt -Output-Port)]
       [?N      (-opt N)]
       [optlist (lambda (t) (-opt (-lst (-opt t))))]
       [-StrRx  (Un -String -Regexp -PRegexp)]
       [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
       [-InpBts (Un -Input-Port -Bytes)])
   (cl-> [(-StrRx   -String           ) (optlist -String)]
         [(-StrRx   -String N         ) (optlist -String)]
         [(-StrRx   -String N ?N      ) (optlist -String)]
         [(-StrRx   -String N ?N ?outp) (optlist -String)]
         [(-BtsRx   -String           ) (optlist -Bytes)]
         [(-BtsRx   -String N         ) (optlist -Bytes)]
         [(-BtsRx   -String N ?N      ) (optlist -Bytes)]
         [(-BtsRx   -String N ?N ?outp) (optlist -Bytes)]
         [(-Pattern -InpBts           ) (optlist -Bytes)]
         [(-Pattern -InpBts N         ) (optlist -Bytes)]
         [(-Pattern -InpBts N ?N      ) (optlist -Bytes)]
         [(-Pattern -InpBts N ?N ?outp) (optlist -Bytes)]))]

[regexp-match*
 (let ([?N      (-opt N)]
       [-StrRx  (Un -String -Regexp -PRegexp)]
       [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
       [-InpBts (Un -Input-Port -Bytes)])
   (cl->*
    (-StrRx   -String [N ?N] . ->opt . (-lst -String))
    (-BtsRx   -String [N ?N] . ->opt . (-lst -Bytes))
    (-Pattern -InpBts [N ?N] . ->opt . (-lst -Bytes))))]
[regexp-try-match
 (let ([?outp   (-opt -Output-Port)]
       [?N      (-opt N)]
       [optlist (lambda (t) (-opt (-lst (-opt t))))])
   (->opt -Pattern -Input-Port [N ?N ?outp] (optlist -Bytes)))]

[regexp-match-exact?
 (-Pattern (Un -String -Bytes -Input-Port) . -> . B)]


[regexp-match-positions
 (let ([?outp   (-opt -Output-Port)]
       [?N      (-opt N)]
       [optlist (lambda (t) (-opt (-lst (-opt t))))]
       [-StrRx  (Un -String -Regexp -PRegexp)]
       [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
       [-InpBts (Un -Input-Port -Bytes)])
   (->opt -Pattern (Un -String -InpBts) [N ?N ?outp] (optlist (-pair -Nat -Nat))))]
[regexp-match-positions*
 (let ([?outp   (-opt -Output-Port)]
       [?N      (-opt N)]
       [optlist (lambda (t) (-opt (-lst (-opt t))))]
       [-StrRx  (Un -String -Regexp -PRegexp)]
       [-BtsRx  (Un -Bytes  -Byte-Regexp -Byte-PRegexp)]
       [-InpBts (Un -Input-Port -Bytes)])
   (->opt -Pattern (Un -String -InpBts) [N ?N ?outp] (-lst (-pair -Nat -Nat))))]
#;
[regexp-match-peek-positions*]
#;
[regexp-split]

[regexp-quote (cl->*
               (->opt -String [Univ] -String)
               (->opt -Bytes [Univ] -Bytes))]
[regexp-replace-quote
 (cl->*
  [-> -String -String]
  [-> -Bytes -Bytes])]




[number->string (N . -> . -String)]

[current-milliseconds (-> -Integer)]
[modulo (cl->* (-Integer -Integer . -> . -Integer))]

;; errors

[raise-type-error
 (cl->
  [(Sym -String Univ) (Un)]
  [(Sym -String N (-lst Univ)) (Un)])]

;; this is a hack

[match:error ((list) Univ . ->* . (Un))]

[vector-set! (-poly (a) (-> (-vec a) N a -Void))]

[vector->list (-poly (a) (-> (-vec a) (-lst a)))]
[list->vector (-poly (a) (-> (-lst a) (-vec a)))]
[exact? (N . -> . B)]
[inexact? (N . -> . B)]
[exact->inexact (N . -> . N)]
[inexact->exact (N . -> . N)]

[real? (Univ . -> . B : (-LFS (list (-filter N)) (list)))]
[complex? (Univ . -> . B : (-LFS (list (-filter N)) (list)))]
[rational? (Univ . -> . B : (-LFS (list (-filter N)) (list)))]
[floor    (-> N N)]
[ceiling  (-> N N)]
[truncate (-> N N)]
[make-rectangular (N N . -> . N)]
[make-polar (N N . -> . N)]
[real-part (N . -> . N)]
[imag-part (N . -> . N)]
[magnitude (N . -> . N)]
[angle     (N . -> . N)]
[numerator   (N . -> . -Integer)]
[denominator (N . -> . -Integer)]
[rationalize (N N . -> . N)]
[expt (cl->* (-Integer -Integer . -> . -Integer) (N N . -> . N))]
[sqrt (N . -> . N)]
[log  (N . -> . N)]
[exp  (N . -> . N)]
[cos  (N . -> . N)]
[sin  (N . -> . N)]
[tan  (N . -> . N)]
[acos (N . -> . N)]
[asin (N . -> . N)]
[atan (N . -> . N)]
[gcd  (null -Integer . ->* . -Integer)]
[lcm  (null -Integer . ->* . -Integer)]

[arithmetic-shift (-Integer -Integer . -> . -Integer)]
[bitwise-and (null -Integer . ->* . -Integer)]
[bitwise-ior (null -Integer . ->* . -Integer)]
[bitwise-not (null -Integer . ->* . -Integer)]
[bitwise-xor (null -Integer . ->* . -Integer)]

[vector (-poly (a) (->* (list) a (-vec a)))]
[make-string (cl-> [(-Integer) -String] [(-Integer -Char) -String])]
[abs (N . -> . N)]
[substring (cl-> [(-String -Integer) -String]
                 [(-String -Integer -Integer) -String])]
[string-length (-String . -> . -Integer)]
[string-set! (-String -Integer -Char . -> . -Void)]
[make-vector (-poly (a) (cl-> [(-Integer) (-vec -Integer)]
			      [(-Integer a) (-vec a)]))]

[file-exists? (-Pathlike . -> . B)]
[string->symbol (-String . -> . Sym)]
[symbol->string (Sym . -> . -String)]
[string->keyword (-String . -> . -Keyword)]
[keyword->string (-Keyword . -> . -String)]
[vector-length (-poly (a) ((-vec a) . -> . -Integer))]

[call-with-input-file (-poly (a) (-String (-Input-Port . -> . a) #:mode (Un (-val 'binary) (-val 'text)) #f . ->key .  a))]
[call-with-output-file (-poly (a) (-String (-Output-Port . -> . a)
                                   #:exists (one-of/c error 'append 'update 'replace 'truncate 'truncate/replace) #f
                                   #:mode (Un (-val 'binary) (-val 'text)) #f 
                                   . ->key .  a))]

[current-output-port (-Param -Output-Port -Output-Port)]
[current-error-port (-Param -Output-Port -Output-Port)]
[current-input-port (-Param -Input-Port -Input-Port)]
[round (N . -> . -Integer)]
[seconds->date (-Integer . -> . (make-Name #'date))]
[current-seconds (-> -Integer)]
[current-print (-Param (Univ . -> . Univ) (Univ . -> . Univ))]
[path->string (-> -Path -String)]

[link-exists? (-> -Pathlike B)]
[directory-exists? (-> -Pathlike B)]
[file-exists? (-> -Pathlike B)]
[directory-list (cl-> [() (-lst -Path)]
                      [(-Path) (-lst -Path)])]

[make-hash (-poly (a b) (-> (-HT a b)))]
[make-hasheq (-poly (a b) (-> (-HT a b)))]
[make-weak-hash (-poly (a b) (-> (-HT a b)))]
[make-weak-hasheq (-poly (a b) (-> (-HT a b)))]

[hash-set! (-poly (a b) ((-HT a b) a b . -> . -Void))]
[hash-map (-poly (a b c) ((-HT a b) (a b . -> . c) . -> . (-lst c)))]
[hash-ref (-poly (a b c)
                 (cl-> [((-HT a b) a) b]
                       [((-HT a b) a (-> c)) (Un b c)]
                       [((-HT a b) a c) (Un b c)]))]
[hash-ref! (-poly (a b)
                  (cl-> [((-HT a b) a (-> b)) b]
                        [((-HT a b) a b) b]))]
[hash-iterate-first (-poly (a b)
                           ((-HT a b) . -> . (Un (-val #f) -Integer)))]
[hash-iterate-next (-poly (a b)
                           ((-HT a b) -Integer . -> . (Un (-val #f) -Integer)))]
[hash-iterate-key (-poly (a b)
                           ((-HT a b) -Integer . -> . a))]
[hash-iterate-value (-poly (a b)
                           ((-HT a b) -Integer . -> . b))]
#;[hash-table-index (-poly (a b) ((-HT a b) a b . -> . -Void))]

[bytes (->* (list) N -Bytes)]
[bytes-ref (-> -Bytes N N)]
[bytes-append (->* (list -Bytes) -Bytes -Bytes)]
[subbytes (cl-> [(-Bytes N) -Bytes] [(-Bytes N N) -Bytes])]
[bytes-length (-> -Bytes N)]
[read-bytes-line (cl-> [() -Bytes]
                       [(-Input-Port) -Bytes]
                       [(-Input-Port Sym) -Bytes])]
[open-input-file (->key -Pathlike #:mode (Un (-val 'binary) (-val 'text)) #f -Input-Port)]
[close-input-port (-> -Input-Port -Void)]
[close-output-port (-> -Output-Port -Void)]
[read-line (cl-> [() -String]
                 [(-Input-Port) -String]
                 [(-Input-Port Sym) -String])]
[copy-file (-> -Pathlike -Pathlike -Void)]
[bytes->string/utf-8 (-> -Bytes -String)]

[force (-poly (a) (-> (-Promise a) a))]
[bytes<? (->* (list -Bytes) -Bytes B)]
[regexp-replace*
 (cl->* (-Pattern (Un -Bytes -String) (Un -Bytes -String) . -> . -Bytes)
        (-Pattern -String -String . -> . -String))]
[peek-char
 (cl->* [-> (Un -Char (-val eof))]
        [-Input-Port . -> . (Un -Char (-val eof))]
        [-Input-Port N . -> . (Un -Char (-val eof))])]
[peek-byte
 (cl->* [-> (Un -Byte (-val eof))]
        [-Input-Port . -> . (Un -Byte (-val eof))]
        [-Input-Port N . -> . (Un -Byte (-val eof))])]
[read-char
 (cl->* [-> (Un -Char (-val eof))]
        [-Input-Port . -> . (Un -Char (-val eof))])]
[read-byte
 (cl->* [-> (Un -Byte (-val eof))]
        [-Input-Port . -> . (Un -Byte (-val eof))])]
[make-pipe
 (cl->* [-> (-values (list -Input-Port -Output-Port))]
        [N . -> . (-values (list -Input-Port -Output-Port))])]
[open-output-bytes
 (cl->* [-> -Output-Port]
        [Univ . -> . -Output-Port])]
[get-output-bytes
 (cl->* [-Output-Port . -> . -Bytes]
        [-Output-Port Univ . -> . -Bytes]
        [-Output-Port Univ N . -> . -Bytes]
        [-Output-Port Univ N N . -> . -Bytes]
        [-Output-Port N . -> . -Bytes]
        [-Output-Port N N . -> . -Bytes])]
#;[exn:fail? (-> Univ B)]
#;[exn:fail:read? (-> Univ B)]

[open-output-string (-> -Output-Port)]
;; FIXME - wrong
[get-output-string (-> -Output-Port -String)]

[make-directory (-> -Path -Void)]

[hash-for-each (-poly (a b c)
                      (-> (-HT a b) (-> a b c) -Void))]

[delete-file (-> -Pathlike -Void)]
[make-namespace (cl->* (-> -Namespace)
                       (-> (Un (-val 'empty) (-val 'initial)) -Namespace))]
[make-base-namespace (-> -Namespace)]
[eval (-> -Sexp Univ)]

[exit (-> (Un))]

[module->namespace (-> -Sexp -Namespace)]
[current-namespace (-Param -Namespace -Namespace)]

[getenv (-> -String (Un -String (-val #f)))]

;; syntax operations

[expand (-> (-Syntax Univ) (-Syntax Univ))]
[expand-once (-> (-Syntax Univ) (-Syntax Univ))]

[syntax-source (-> (-Syntax Univ) Univ)]
[syntax-position (-> (-Syntax Univ) (-opt N))]
[datum->syntax 
 (-poly 
  (a)
  (let* ([Pre Syntax-Sexp]
         [I (-Syntax Sym)]
         [A Any-Syntax]
	 [S (-Syntax Univ)]
         [ctxt (-opt S)]
	 [srclist (-Tuple (list
                           Univ 
                           (-opt -Integer)
                           (-opt -Integer)
                           (-opt -Integer)
                           (-opt -Integer)))]
	 [srcloc (Un S (-val #f) srclist)]
	 [prop (-opt S)]
	 [cert (-opt S)])
    (cl->*
     (-> ctxt Sym I)
     (-> ctxt Pre A)
     (-> ctxt Univ S)
     (-> ctxt Sym srcloc I)
     (-> ctxt Pre srcloc A)
     (-> ctxt Univ srcloc S)
     (-> ctxt Sym srcloc prop I)
     (-> ctxt Pre srcloc prop A)
     (-> ctxt Univ srcloc prop S)
     (-> ctxt Sym srcloc prop cert I)
     (-> ctxt Pre srcloc prop cert A)
     (-> ctxt Univ srcloc prop cert S))))]

[syntax->datum (cl->* (-> Any-Syntax -Sexp)
                      (-> (-Syntax Univ) Univ))]
[syntax-e (-poly (a) (-> (-Syntax a) a))]
[syntax-original? (-poly (a) (-> (-Syntax a) B))]
[identifier? (make-pred-ty (-Syntax Sym))]
[syntax? (make-pred-ty (-Syntax Univ))]
[syntax-property (-poly (a) (cl->* (-> (-Syntax a) Univ Univ (-Syntax a))
                                   (-> (-Syntax Univ) Univ Univ)))]

[values (-polydots (a) (null (a a) . ->... . (make-ValuesDots null a 'a)))]
[call-with-values (-polydots (b a) ((-> (make-ValuesDots null a 'a)) (null (a a) . ->... . b) . -> .  b))]

[eof (-val eof)]
[read-accept-reader (-Param B B)]

[maybe-print-message (-String . -> . -Void)]

[list->string ((-lst -Char) . -> . -String)]
[string->list (-String . -> . (-lst -Char))]
[sort (-poly (a) ((-lst a) (a a . -> . B) . -> . (-lst a)))]
[find-system-path (Sym . -> . -Path)]

[object-name (Univ . -> . Univ)]
;; scheme/cmdline

[parse-command-line
 (let ([mode-sym (one-of/c 'once-each 'once-any 'multi 'final 'help-labels)])
   (-polydots (b a)
              (cl->* (-Pathlike 
                      (Un (-lst -String) (-vec -String))
                      (-lst (-pair mode-sym (-lst (-lst Univ))))
                      ((list Univ) [a a] . ->... . b)
                      (-lst -String)
                      . -> . b))))]

;; scheme/list
[last-pair (-poly (a) ((-mu x (Un a (-val '()) (-pair a x)))
                       . -> . 
                       (Un (-pair a a) (-pair a (-val '())))))]
[remove-duplicates
 (-poly (a)
        (cl->*
         ((-lst a) . -> . (-lst a))
         ((-lst a) (a a . -> . Univ) . -> . (-lst a))))]
[append-map
 (-polydots (c a b) ((list ((list a) (b b) . ->... . (-lst c)) (-lst a))
                     ((-lst b) b) . ->... .(-lst c)))]
[split-at
 (-poly (a) ((list (-lst a)) -Integer . ->* . (-values (list (-lst a) (-lst a)))))]
[append*
 (-poly (a) ((-lst (-lst a)) . -> . (-lst a)))]

;; scheme/tcp
[tcp-listener? (make-pred-ty -TCP-Listener)]
[tcp-abandon-port (-Port . -> . -Void)]
[tcp-accept (-TCP-Listener . -> . (-values (list -Input-Port -Output-Port)) )]
[tcp-accept/enable-break (-TCP-Listener . -> . (-values (list -Input-Port -Output-Port)) )]
[tcp-accept-ready? (-TCP-Listener . -> . B )]
[tcp-addresses (-Port . -> . (-values (list N N)))]
[tcp-close (-TCP-Listener . -> . -Void )]
[tcp-connect (-String -Integer . -> . (-values (list -Input-Port -Output-Port)))]
[tcp-connect/enable-break (-String -Integer . -> . (-values (list -Input-Port -Output-Port)))]
[tcp-listen (N . -> . -TCP-Listener)]

;; scheme/bool
[boolean=? (B B . -> . B)]
[symbol=? (Sym Sym . -> . B)]
[false? (make-pred-ty (-val #f))]

;; with-stx.ss
[generate-temporaries ((Un (-Syntax Univ) (-lst Univ)) . -> . (-lst (-Syntax Sym)))]
[check-duplicate-identifier ((-lst (-Syntax Sym)) . -> . (-opt (-Syntax Sym)))]

;; string.ss
[real->decimal-string (N [-Nat] . ->opt .  -String)]

[current-continuation-marks (-> -Cont-Mark-Set)]

;; scheme/path

[explode-path (-Pathlike . -> . (-lst (Un -Path (-val 'up) (-val 'same))))]
[find-relative-path (-Pathlike -Pathlike . -> . -Path)]
[simple-form-path (-Pathlike . -> . -Path)]
[normalize-path (cl->* (-Pathlike . -> . -Path)
                       (-Pathlike -Pathlike . -> . -Path))]
[filename-extension (-Pathlike . -> . (-opt -Bytes))]
[file-name-from-path (-Pathlike . -> . (-opt -Path))]
[path-only (-Pathlike . -> . -Path)]
[some-system-path->string (-Path . -> . -String)]
[string->some-system-path 
 (-String (Un (-val 'unix) (-val 'windows)) . -> . -Path)]

;; scheme/math

[sgn (-Real . -> . -Real)]
[pi N]
[sqr (N . -> . N)]
[sgn (N . -> . N)]
[conjugate (N . -> . N)]
[sinh (N . -> . N)]
[cosh (N . -> . N)]
[tanh (N . -> . N)]

;; scheme/pretty

[pretty-print
 (cl->* (Univ . -> . -Void)
        (Univ -Output-Port . -> . -Void))]
[pretty-display
 (cl->* (Univ . -> . -Void)
        (Univ -Output-Port . -> . -Void))]
[pretty-format
 (cl->* (Univ . -> . -Void)
        (Univ -Integer . -> . -Void))]

;; unsafe

[unsafe-cdr (-poly (a b) 
              (cl->*
               (->acc (list (-pair a b)) b (list -cdr))))]