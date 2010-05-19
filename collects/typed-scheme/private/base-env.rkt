#lang s-exp "env-lang.rkt"

(require
 scheme/tcp
 scheme
 scheme/unsafe/ops
 scheme/fixnum
 (only-in rnrs/lists-6 fold-left)
 '#%paramz
 "extra-procs.rkt"
 (only-in '#%kernel [apply kernel:apply])
 scheme/promise scheme/system
 (only-in string-constants/private/only-once maybe-print-message)
 (only-in mzscheme make-namespace)
 (only-in racket/match/runtime match:error matchable? match-equality-test)
 (for-syntax (only-in (types abbrev) [-Number N] [-Boolean B] [-Symbol Sym])
             (only-in (rep type-rep) make-HashtableTop make-MPairTop make-BoxTop make-VectorTop)))

[raise (Univ . -> . (Un))]
[raise-syntax-error (cl->* 
                     (-> (Un (-val #f) -Symbol)
                               -String
                               (Un))
                     (-> (Un (-val #f) -Symbol)
                               -String
                               Univ
                               (Un))
                     (-> (Un (-val #f) -Symbol)
                               -String
                               Univ
                               Univ
                               (Un)))]

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
[second (-poly (a b c)
               (cl->* [->acc (list (-pair a (-pair b (-lst c)))) b (list -car -cdr)]
                      [->* (list (-lst a)) a]))]
[third (-poly (a b c d)
              (cl->* [->acc (list (-pair a (-pair b (-pair c (-lst d))))) c (list -car -cdr -cdr)]
                     [->* (list (-lst a)) a]))]
[fourth  (-poly (a) ((-lst a) . -> .  a))]
[fifth   (-poly (a) ((-lst a) . -> .  a))]
[sixth   (-poly (a) ((-lst a) . -> .  a))]
[seventh (-poly (a) ((-lst a) . -> .  a))]
[eighth  (-poly (a) ((-lst a) . -> .  a))]
[ninth   (-poly (a) ((-lst a) . -> .  a))]
[tenth   (-poly (a) ((-lst a) . -> .  a))]
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

[null? (make-pred-ty (-val null))]
[eof-object? (make-pred-ty (-val eof))]
[null (-val null)]
[char? (make-pred-ty -Char)]

[boolean? (make-pred-ty B)]
[eq? (-> Univ Univ B)]
[eqv? (-> Univ Univ B)]
[equal? (-> Univ Univ B)]
[assert (-poly (a b) (cl->*
		      (Univ (make-pred-ty (list a) Univ b) . -> . b)
		      (-> (Un a (-val #f)) a)))]
[gensym (->opt [Sym] Sym)]
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
[read (->opt [-Input-Port] -Sexp)]
[ormap (-polydots (a c b) (->... (list (->... (list a) (b b) c) (-lst a)) ((-lst b) b) c))]
[andmap (-polydots (a c d b) (cl->*
                              ;; 1 means predicate on second argument
                              (make-pred-ty (list (make-pred-ty (list a) c d) (-lst a)) c (-lst d) 1)
                              (->... (list (->... (list a) (b b) c) (-lst a)) ((-lst b) b) c)))]
[newline (->opt [-Output-Port] -Void)]
[not (-> Univ B)]
[box (-poly (a) (a . -> . (-box a)))]
[unbox (-poly (a) (cl->*                   
                   ((-box a) . -> . a)
                   ((make-BoxTop) . -> . Univ)))]
[set-box! (-poly (a) ((-box a) a . -> . -Void))]
[box? (make-pred-ty (make-BoxTop))]
[cons? (make-pred-ty (-pair Univ Univ))]
[pair? (make-pred-ty (-pair Univ Univ))]
[empty? (make-pred-ty (-val null))]
[empty (-val null)]

[string? (make-pred-ty -String)]
[string (->* '() -Char -String)]
[string-length (-String . -> . -Nat)]
[unsafe-string-length (-String . -> . -Nat)]

[symbol? (make-pred-ty Sym)]
[keyword? (make-pred-ty -Keyword)]
[list? (make-pred-ty (-lst Univ))]
[list (-poly (a) (->* '() a (-lst a)))]
[procedure? (make-pred-ty top-func)]
[map (-polydots (c a b) 
		(cl->*
		 (-> (-> a c) (-pair a (-lst a)) (-pair c (-lst c)))
		((list 
		  ((list a) (b b) . ->... . c) 
		  (-lst a))
		 ((-lst b) b) . ->... .(-lst c))))]
[for-each (-polydots (c a b) ((list ((list a) (b b) . ->... . Univ) (-lst a))
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
                      ((make-pred-ty (list a) Univ b)
                       (-lst a)
                       . -> .
                       (-lst b))
                      ((a . -> . Univ) (-lst a) . -> . (-lst a))))]
[filter-not (-poly (a) (cl->*
                        ((a . -> . Univ) (-lst a) . -> . (-lst a))))]
[remove  (-poly (a) (a (-lst a) . -> . (-lst a)))]
[remq    (-poly (a) (a (-lst a) . -> . (-lst a)))]
[remv    (-poly (a) (a (-lst a) . -> . (-lst a)))]
[remove* (-poly (a b) ((-lst a) (-lst a) [(a b . -> . B)] . ->opt . (-lst b)))]
[remq*   (-poly (a) (cl-> [((-lst a) (-lst a)) (-lst a)]))]
[remv*   (-poly (a) (cl-> [((-lst a) (-lst a)) (-lst a)]))]

(error 
 (make-Function (list 
                 (make-arr (list Sym -String) (Un) #:rest Univ)
                 (make-arr (list -String) (Un) #:rest Univ)
                 (make-arr (list Sym) (Un)))))

[namespace-variable-value (Sym [Univ (-opt (-> Univ)) -Namespace] . ->opt . Univ)]

[match:error (Univ . -> . (Un))]
[match-equality-test (-Param (Univ Univ . -> . Univ) (Univ Univ . -> . Univ))]
[matchable? (make-pred-ty (Un -String -Bytes))]
[display (Univ [-Output-Port] . ->opt . -Void)]
[write   (Univ [-Output-Port] . ->opt . -Void)]
[print   (Univ [-Output-Port] . ->opt . -Void)]
[void (->* '() Univ -Void)]
[void? (make-pred-ty -Void)]
[printf (->* (list -String) Univ -Void)]
[fprintf (->* (list -Output-Port -String) Univ -Void)]
[format (->* (list -String) Univ -String)]

[sleep (N . -> . -Void)]

[reverse (-poly (a) (-> (-lst a) (-lst a)))]
[append (-poly (a) (->* (list) (-lst a) (-lst a)))]
[length (-poly (a) (-> (-lst a) -Nat))]
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
[char->integer (-> -Char -Nat)]
[integer->char (-> -Nat -Char)]

[string-normalize-nfd (-> -String -String)]
[string-normalize-nfkd (-> -String -String)]
[string-normalize-nfc (-> -String -String)]
[string-normalize-nfkc (-> -String -String)]

[string-copy (-> -String -String)]
[string->immutable-string (-> -String -String)]
[string->path (-> -String -Path)]
[file-exists? (-> -Pathlike B)]

[build-path ((list -Pathlike*) -Pathlike* . ->* . -Path)]
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


[assq  (-poly (a b) (a (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assv  (-poly (a b) (a (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assoc (-poly (a b) (a (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assf  (-poly (a b) ((a . -> . Univ) (-lst (-pair a b))
                     . -> . (-opt (-pair a b))))]

[apply        (-poly (a b) (((list) a . ->* . b) (-lst a) . -> . b))]
[kernel:apply (-poly (a b) (((list) a . ->* . b) (-lst a) . -> . b))]
[time-apply (-poly (a b c)
                   (cl->*
                    (-> 
                     (-> a)
                     (-Tuple (list))
                     (-values (list (-pair a (-val '())) -Nat -Nat -Nat)))
                    (-> 
                     (-> b a)
                     (-Tuple (list b))
                     (-values (list (-pair a (-val '())) -Nat -Nat -Nat)))
                    (-> 
                     (-> b c a)
                     (-Tuple (list b c))
                     (-values (list (-pair a (-val '())) -Nat -Nat -Nat)))))]

[call/cc (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (Un a b)))]
[call/ec (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (Un a b)))]
[call-with-current-continuation (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (Un a b)))]
[call-with-escape-continuation (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (Un a b)))]

[struct->vector (Univ . -> . (-vec Univ))]
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
[regexp-quote (cl->*
	       (-String [-Boolean] . ->opt . -String)
	       (-Bytes  [-Boolean] . ->opt . -Bytes))]

[regexp-match-exact?
 (-Pattern (Un -String -Bytes -Input-Port) . -> . B)]


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

[number->string (->opt N [N] -String)]
[string->number (->opt -String [N] (Un (-val #f) N))]

[current-milliseconds (-> -Integer)]

;; errors

;; this is a hack

[match:error ((list) Univ . ->* . (Un))]

[arithmetic-shift (-Integer -Integer . -> . -Integer)]
[bitwise-and (null -Integer . ->* . -Integer)]
[bitwise-ior (null -Integer . ->* . -Integer)]
[bitwise-not (null -Integer . ->* . -Integer)]
[bitwise-xor (null -Integer . ->* . -Integer)]

[abs (-Real . -> . -Real)]

[file-exists? (-Pathlike . -> . B)]
[string->symbol (-String . -> . Sym)]
[symbol->string (Sym . -> . -String)]
[string->keyword (-String . -> . -Keyword)]
[keyword->string (-Keyword . -> . -String)]

;; vectors
[vector? (make-pred-ty (make-VectorTop))]

[vector->list (-poly (a) (-> (-vec a) (-lst a)))]
[list->vector (-poly (a) (-> (-lst a) (-vec a)))]
[vector-length ((make-VectorTop) . -> . -Nat)]
[vector (-poly (a) (->* (list) a (-vec a)))]
[vector-immutable (-poly (a) (->* (list) a (-vec a)))]
[vector->immutable-vector (-poly (a) (-> (-vec a) (-vec a)))]
[vector-fill! (-poly (a) (-> (-vec a) a -Void))]
[vector-argmax (-poly (a) (-> (-> a -Real) (-vec a) a))]
[vector-argmin (-poly (a) (-> (-> a -Real) (-vec a) a))]
[vector-memq (-poly (a) (-> a (-vec a) (-opt -Nat)))]
[vector-memv (-poly (a) (-> a (-vec a) (-opt -Nat)))]
[vector-member (-poly (a) (a (-vec a) . -> . (-opt -Nat)))]
;; [vector->values no good type here]



[call-with-input-file (-poly (a) (-String (-Input-Port . -> . a) #:mode (Un (-val 'binary) (-val 'text)) #f . ->key .  a))]
[call-with-output-file (-poly (a) (-String (-Output-Port . -> . a)
                                   #:exists (one-of/c error 'append 'update 'replace 'truncate 'truncate/replace) #f
                                   #:mode (Un (-val 'binary) (-val 'text)) #f 
                                   . ->key .  a))]

[current-output-port (-Param -Output-Port -Output-Port)]
[current-error-port (-Param -Output-Port -Output-Port)]
[current-input-port (-Param -Input-Port -Input-Port)]
[seconds->date (-Integer . -> . (make-Name #'date))]
[current-seconds (-> -Integer)]
[current-print (-Param (Univ . -> . Univ) (Univ . -> . Univ))]
[path->string (-> -Path -String)]

[link-exists? (-> -Pathlike B)]
[directory-exists? (-> -Pathlike B)]
[file-exists? (-> -Pathlike B)]
[directory-list (cl-> [() (-lst -Path)]
                      [(-Path) (-lst -Path)])]

[hash? (make-pred-ty (make-HashtableTop))]
[hash-eq? (-> (make-HashtableTop) B)]
[hash-eqv? (-> (make-HashtableTop) B)]
[hash-weak? (-> (make-HashtableTop) B)]
[make-hash (-poly (a b) (-> (-HT a b)))]
[make-hasheq (-poly (a b) (-> (-HT a b)))]
[make-hasheqv (-poly (a b) (-> (-HT a b)))]
[make-weak-hash (-poly (a b) (-> (-HT a b)))]
[make-weak-hasheq (-poly (a b) (-> (-HT a b)))]
[make-weak-hasheqv (-poly (a b) (-> (-HT a b)))]
[make-immutable-hash (-poly (a b) (-> (-lst (-pair a b)) (-HT a b)))]
[make-immutable-hasheq (-poly (a b) (-> (-lst (-pair a b)) (-HT a b)))]
[make-immutable-hasheqv (-poly (a b) (-> (-lst (-pair a b)) (-HT a b)))]

[hash-set (-poly (a b) ((-HT a b) a b . -> . (-HT a b)))]
[hash-set! (-poly (a b) ((-HT a b) a b . -> . -Void))]
[hash-map (-poly (a b c) ((-HT a b) (a b . -> . c) . -> . (-lst c)))]
[hash-ref (-poly (a b c)
                 (cl-> [((-HT a b) a) b]
                       [((-HT a b) a (-> c)) (Un b c)]))]
[hash-ref! (-poly (a b)
                  (cl-> [((-HT a b) a (-> b)) b]))]
[hash-has-key? (-poly (a b) (-> (-HT a b) a B))]
[hash-update! (-poly (a b)
                     (cl-> [((-HT a b) a (-> b b)) -Void]
                           [((-HT a b) a (-> b b) (-> b)) -Void]))]
[hash-update (-poly (a b)
                    (cl-> [((-HT a b) a (-> b b)) (-HT a b)]
                          [((-HT a b) a (-> b b) (-> b)) (-HT a b)]))]
[hash-remove (-poly (a b) ((-HT a b) a . -> . (-HT a b)))]
[hash-remove! (-poly (a b) ((-HT a b) a . -> . -Void))]
[hash-map (-poly (a b c) ((-HT a b) (a b . -> . c) . -> . (-lst c)))]
[hash-for-each (-poly (a b c) (-> (-HT a b) (-> a b c) -Void))]
[hash-count (-poly (a b) (-> (-HT a b) -Nat))]
[hash-copy (-poly (a b) (-> (-HT a b) (-HT a b)))]
[eq-hash-code (-poly (a) (-> a -Integer))]
[eqv-hash-code (-poly (a) (-> a -Integer))]
[equal-hash-code (-poly (a) (-> a -Integer))]
[equal-secondary-hash-code (-poly (a) (-> a -Integer))]
[hash-iterate-first (-poly (a b)
                           ((-HT a b) . -> . (Un (-val #f) -Integer)))]
[hash-iterate-next (-poly (a b)
                           ((-HT a b) -Integer . -> . (Un (-val #f) -Integer)))]
[hash-iterate-key (-poly (a b)
                           ((-HT a b) -Integer . -> . a))]
[hash-iterate-value (-poly (a b)
                           ((-HT a b) -Integer . -> . b))]
#;[hash-table-index (-poly (a b) ((-HT a b) a b . -> . -Void))]

[bytes (->* (list) -Integer -Bytes)]
[bytes-ref (-> -Bytes -Integer -Integer)]
[bytes-append (->* (list -Bytes) -Bytes -Bytes)]
[subbytes (cl-> [(-Bytes -Integer) -Bytes] [(-Bytes -Integer -Integer) -Bytes])]
[bytes-length (-> -Bytes -Nat)]
[unsafe-bytes-length (-> -Bytes -Nat)]

[read-bytes-line (->opt [-Input-Port Sym] -Bytes)]
[open-input-file (->key -Pathlike #:mode (Un (-val 'binary) (-val 'text)) #f -Input-Port)]
[close-input-port (-> -Input-Port -Void)]
[close-output-port (-> -Output-Port -Void)]
[read-line  (->opt [-Input-Port Sym] -String)]
[copy-file (-> -Pathlike -Pathlike -Void)]
[bytes->string/utf-8 (-> -Bytes -String)]

[force (-poly (a) (-> (-Promise a) a))]
[bytes<? (->* (list -Bytes) -Bytes B)]
[regexp-replace*
 (cl->* (-Pattern -String -String . -> . -String)
	(-Pattern (Un -Bytes -String) (Un -Bytes -String) . -> . -Bytes))]
[read-char
 (cl->* [->opt [-Input-Port] (Un -Char (-val eof))])]
[read-byte
 (cl->* [-> (Un -Byte (-val eof))]
        [-Input-Port . -> . (Un -Byte (-val eof))])]
[make-pipe
 (cl->* [->opt [N] (-values (list -Input-Port -Output-Port))])]
[open-output-bytes
 (cl->* [[Univ] . ->opt . -Output-Port])]
[get-output-bytes (-Output-Port [Univ N N] . ->opt . -Bytes)]
#;[exn:fail? (-> Univ B)]
#;[exn:fail:read? (-> Univ B)]

[open-output-string (-> -Output-Port)]
;; FIXME - wrong
[get-output-string (-> -Output-Port -String)]

[make-directory (-> -Path -Void)]

[delete-file (-> -Pathlike -Void)]
[make-namespace (->opt [(Un (-val 'empty) (-val 'initial))] -Namespace)]
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
    (->opt ctxt Sym  [srcloc prop cert] I)
    (->opt ctxt Pre  [srcloc prop cert] A)
    (->opt ctxt Univ [srcloc prop cert] S)))]

[syntax->datum (cl->* (-> Any-Syntax -Sexp)
                      (-> (-Syntax Univ) Univ))]
[syntax-e (-poly (a) (->acc (list (-Syntax a)) a (list -syntax-e)))]
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

[path? (make-pred-ty -Path)]

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
[count (-polydots (a b)
                  ((list
                    ((list a) (b b) . ->... . Univ)
                    (-lst a))
                   ((-lst b) b) 
                   . ->... . 
                   -Integer))]
[filter-map (-polydots (c a b)
                       ((list
                         ((list a) (b b) . ->... . (-opt c))
                         (-lst a))
                        ((-lst b) b) . ->... . (-lst c)))]
[last   (-poly (a) ((-lst a) . -> . a))]
[add-between (-poly (a b) ((-lst a) b . -> . (-lst (Un a b))))]

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
[append*
 (-poly (a) ((-lst (-lst a)) . -> . (-lst a)))]

;; scheme/tcp
[tcp-listener? (make-pred-ty -TCP-Listener)]
[tcp-abandon-port (-Port . -> . -Void)]
[tcp-accept (-TCP-Listener . -> . (-values (list -Input-Port -Output-Port)) )]
[tcp-accept/enable-break (-TCP-Listener . -> . (-values (list -Input-Port -Output-Port)) )]
[tcp-accept-ready? (-TCP-Listener . -> . B )]
[tcp-addresses (cl->*
		(-Port [(-val #f)] . ->opt . (-values (list -String -String)))
		(-Port (-val #t) . -> . (-values (list -String -Nat -String -Nat))))]
[tcp-close (-TCP-Listener . -> . -Void )]
[tcp-connect (-String -Integer . -> . (-values (list -Input-Port -Output-Port)))]
[tcp-connect/enable-break (-String -Integer . -> . (-values (list -Input-Port -Output-Port)))]
[tcp-listen (N . -> . -TCP-Listener)]

;; scheme/bool
[boolean=? (B B . -> . B)]
[symbol=? (Sym Sym . -> . B)]
[false? (make-pred-ty (-val #f))]

;; with-stx.rkt
[generate-temporaries ((Un (-Syntax Univ) (-lst Univ)) . -> . (-lst (-Syntax Sym)))]
[check-duplicate-identifier ((-lst (-Syntax Sym)) . -> . (-opt (-Syntax Sym)))]


[current-continuation-marks (-> -Cont-Mark-Set)]

;; scheme/port
[port->lines (cl->* ([-Input-Port] . ->opt . (-lst -String)))]
[with-output-to-string
  (-> (-> Univ) -String)]
[open-output-nowhere (-> -Output-Port)]

;; scheme/path

[explode-path (-Pathlike . -> . (-lst (Un -Path (-val 'up) (-val 'same))))]
[find-relative-path (-Pathlike -Pathlike . -> . -Path)]
[simple-form-path (-Pathlike . -> . -Path)]
[normalize-path (cl->* (-Pathlike [-Pathlike] . ->opt . -Path))]
[filename-extension (-Pathlike . -> . (-opt -Bytes))]
[file-name-from-path (-Pathlike . -> . (-opt -Path))]
[path-only (-Pathlike . -> . -Path)]
[some-system-path->string (-Path . -> . -String)]
[string->some-system-path 
 (-String (Un (-val 'unix) (-val 'windows)) . -> . -Path)]

;; scheme/file
[fold-files 
 (-poly 
  (a) 
  (let ([funarg* (-Path (one-of/c 'file 'dir 'link) a . -> . (-values (list a Univ)))]
        [funarg (-Path (one-of/c 'file 'dir 'link) a . -> . a)])
    (cl->*
     (funarg a [(-opt -Pathlike) Univ]. ->opt . a)
     (funarg* a [(-opt -Pathlike) Univ]. ->opt . a))))]


;; scheme/pretty

[pretty-print (Univ [-Output-Port] . ->opt . -Void)]
[pretty-display (Univ [-Output-Port] . ->opt . -Void)]
[pretty-format (Univ [-Output-Port] . ->opt . -Void)]

;; unsafe

[unsafe-vector-ref (-poly (a) ((-vec a) -Nat . -> . a))]
[unsafe-vector*-ref (-poly (a) ((-vec a) -Nat . -> . a))]
[unsafe-vector-length (-poly (a) ((-vec a) . -> . -Nat))]
[unsafe-vector*-length (-poly (a) ((-vec a) . -> . -Nat))]
[unsafe-car (-poly (a b) 
              (cl->*
               (->acc (list (-pair a b)) a (list -car))))]
[unsafe-cdr (-poly (a b) 
              (cl->*
               (->acc (list (-pair a b)) b (list -cdr))))]

;; scheme/vector
[vector-count (-polydots (a b)
                         ((list
                           ((list a) (b b) . ->... . Univ)
                           (-vec a))
                          ((-vec b) b)
                          . ->... . 
                          -Integer))]
[vector-filter (-poly (a b) (cl->*
                             ((make-pred-ty (list a) Univ b)
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
 (-poly (a) ((list (-vec a)) -Integer . ->* . (-values (list (-vec a) (-vec a)))))]
[vector-split-at-right
 (-poly (a) ((list (-vec a)) -Integer . ->* . (-values (list (-vec a) (-vec a)))))]


;; scheme/system
[system (-String . -> . -Boolean)]
[system* ((list -Pathlike) -String . ->* . -Boolean)]
[system/exit-code (-String . -> . -Integer)]
[system*/exit-code ((list -Pathlike) -String . ->* . -Integer)]

;; Byte and String Output (Section 12.3 of the Reference)
[write-char (cl-> [(-Char) -Void]
                  [(-Char -Output-Port) -Void])]
[write-byte (cl-> [(-Nat) -Void]
                  [(-Nat -Output-Port) -Void])]
[newline (cl-> [() -Void]
               [(-Output-Port) -Void])]
[write-string (cl-> [(-String) -Nat]
                    [(-String -Output-Port) -Nat]
                    [(-String -Output-Port -Nat) -Nat]
                    [(-String -Output-Port -Nat -Nat) -Nat])]
[write-bytes  (cl-> [(-Bytes) -Nat]
                    [(-Bytes -Output-Port) -Nat]
                    [(-Bytes -Output-Port -Nat) -Nat]
                    [(-Bytes -Output-Port -Nat -Nat) -Nat])]
[write-bytes-avail  (cl-> [(-Bytes) -Nat]
                          [(-Bytes -Output-Port) -Nat]
                          [(-Bytes -Output-Port -Nat) -Nat]
                          [(-Bytes -Output-Port -Nat -Nat) -Nat])]
[write-bytes-avail*  (cl-> [(-Bytes) (-opt -Nat)]
                           [(-Bytes -Output-Port) (-opt -Nat)]
                           [(-Bytes -Output-Port -Nat) (-opt -Nat)]
                           [(-Bytes -Output-Port -Nat -Nat) (-opt -Nat)])]
[write-bytes-avail/enable-break (cl-> [(-Bytes) -Nat]
                                      [(-Bytes -Output-Port) -Nat]
                                      [(-Bytes -Output-Port -Nat) -Nat]
                                      [(-Bytes -Output-Port -Nat -Nat) -Nat])]
[write-special (cl-> [(Univ) -Boolean]
                     [(Univ -Output-Port) -Boolean])]
;; Need event type before we can include these
;;write-special-avail* 
;;write-bytes-avail-evt
;;write-special-evt
[port-writes-atomic? (-Output-Port . -> . -Boolean)]
[port-writes-special? (-Output-Port . -> . -Boolean)]
