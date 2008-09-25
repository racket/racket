#lang s-exp "env-lang.ss"

(require
 (only-in scheme/list cons? take drop add-between last filter-map)
 (only-in rnrs/lists-6 fold-left)
 '#%paramz
 scheme/promise
 (only-in scheme/match/runtime match:error))

[raise (Univ . -> . (Un))]

(car (-poly (a b) (cl-> [((-pair a b)) a]
                        [((make-Listof a)) a])))
[first (-poly (a b) (cl-> [((-pair a b)) a]
                          [((make-Listof a)) a]))]
[second (-poly (a b c)
               (cl->                
                [((-pair a (-pair b c))) b]
                [((-lst a)) a]))]
[third (-poly (a b c d)
              (cl->                
               [((-pair a (-pair b (-pair c d)))) c]
               [((-lst a)) a]))]
[fourth (-poly (a) ((-lst a) . -> .  a))]
[fifth (-poly (a) ((-lst a) . -> .  a))]
[sixth (-poly (a) ((-lst a) . -> .  a))]
[rest (-poly (a) ((-lst a) . -> .  (-lst a)))]
(cadr 
 (-poly (a b c)
        (cl->                
         [((-pair a (-pair b c))) b]
         [((-lst a)) a])))
(caddr (-poly (a) (-> (-lst a) a)))
(cadddr (-poly (a) (-> (-lst a) a)))
(cdr (-poly (a b) (cl-> [((-pair a b)) b]
                                   [((make-Listof a)) (make-Listof a)])))
(cddr (-poly (a) (-> (make-Listof a) (make-Listof a))))
(cdddr (-poly (a) (-> (make-Listof a) (make-Listof a))))
(cons (-poly (a b) 
             (cl-> [(a (-lst a)) (-lst a)]
                   [(a b) (-pair a b)])))
[*cons (-poly (a b) (cl-> 
                     [(a b) (-pair a b)]
                     [(a (-lst a)) (-lst a)]))]
[*list? (make-pred-ty (-lst Univ))]

(null? (make-pred-ty (-val null)))
(eof-object? (make-pred-ty (-val eof)))
[null (-val null)]
(number? (make-pred-ty N))
[char? (make-pred-ty -Char)]
(integer? (make-pred-ty -Integer))
(boolean? (make-pred-ty B))
(add1 (cl->*
       (-> -Integer -Integer)
       (-> N N)))
(sub1 (cl->*
       #;(-> -Integer -Integer)
       (-> N N)))
(eq? (-> Univ Univ B))
(eqv? (-> Univ Univ B))
(equal? (-> Univ Univ B))
(even? (-> N B))
[assert (-poly (a) (-> (*Un a (-val #f)) a))]
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
[floor (-> N N)]
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
[list? (make-pred-ty (-lst Univ))]
[list (-poly (a) (->* '() a (-lst a)))]
[procedure? (make-pred-ty (make-Function (list (make-top-arr))))]
[map (-polydots (c a b) ((list ((list a) (b b) . ->... . c) (-lst a))
                         ((-lst b) b) . ->... .(-lst c)))]
[for-each (-polydots (c a b) ((list ((list a) (b b) . ->... . Univ) (-lst a))
                              ((-lst b) b) . ->... . -Void))]
[fold-left (-polydots (c a b) ((list ((list c a) (b b) . ->... . c) c (-lst a))
                               ((-lst b) b) . ->... . c))]
[fold-right (-polydots (c a b) ((list ((list c a) (b b) . ->... . c) c (-lst a))
                                ((-lst b) b) . ->... . c))]
[foldl
 (-poly (a b c)
        (cl-> [((a b . -> . b) b (make-Listof a)) b]
              [((a b c . -> . c) c (make-Listof a) (make-Listof b)) c]))]
[foldr  (-poly (a b c) ((a b . -> . b) b (-lst a) . -> . b))]
[filter (-poly (a b) (cl->*
                      ((a . -> . B
                          :
                          (list (make-Latent-Restrict-Effect b))
                          (list (make-Latent-Remove-Effect b)))
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
[remove* (-poly (a b)
                (cl-> [((-lst a) (-lst a)) (-lst a)]
                      [((-lst a) (-lst b) (a b . -> . B)) (-lst b)]))]

(error 
 (make-Function (list 
                 (make-arr null (Un))
                 (make-arr (list Sym -String) (Un) Univ)
                 (make-arr (list -String) (Un) Univ)
                 (make-arr (list Sym) (Un)))))


[namespace-variable-value 
 (cl-> 
  [(Sym) Univ]
  [(Sym B -Namespace (-> Univ)) Univ])]

(match:error (Univ . -> . (Un)))
(display 
 (cl->
  [(Univ) -Void]
  [(Univ -Port) -Void]))
[void (->* '() Univ -Void)]
[void? (make-pred-ty -Void)]
[printf (->* (list -String) Univ -Void)]
[fprintf (->* (list -Output-Port -String) Univ -Void)]
[format (->* (list -String) Univ -String)]   
(fst (-poly (a b) (-> (-pair a b) a)))
(snd (-poly (a b) (-> (-pair a b) b)))

(= (->* (list N N) N B))
(>= (->* (list N N) N B))
(< (->* (list N N) N B))
(<= (->* (list N N) N B))
[> (->* (list N) N B)]
(zero? (N . -> . B))
(* (cl->* (->* '() -Integer -Integer) (->* '() N N)))
(/ (cl->* (->* (list N) N N)))
(+ (cl->* (->* '() -Integer -Integer) (->* '() N N)))
(- (cl->* (->* (list -Integer) -Integer -Integer) (->* (list N) N N)))
(max (->* (list N) N N))
(min (->* (list N) N N))
[vector-ref 
 (-poly (a) ((-vec a) N . -> . a))]
[build-vector (-poly (a) (-Integer (-Integer . -> . a) . -> . (-vec a)))]
[build-list (-poly (a) (-Integer (-Integer . -> . a) . -> . (-lst a)))]
[reverse (-poly (a) (-> (make-Listof a) (make-Listof a)))]
[append (-poly (a) (->* (list) (-lst a) (-lst a)))]
[length (-poly (a) (-> (make-Listof a) -Integer))]
[memq (-poly (a) (-> a (make-Listof a) (-opt (make-Listof a))))]
[memv (-poly (a) (-> a (make-Listof a) (-opt (make-Listof a))))]
[memf (-poly (a) ((a . -> . B) (-lst a) . -> . (-opt (-lst a))))]
[member 
 (-poly (a) (a (-lst a) . -> . (-opt (-lst a))))]
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

[string-normalize-nfd (-> -String -String)]
[string-normalize-nfkd (-> -String -String)]
[string-normalize-nfc (-> -String -String)]
[string-normalize-nfkc (-> -String -String)]

[string-ref (-> -String N -Char)]
[substring (cl->*
            (-> -String N -String)
            (-> -String N N -String))]
[string->path (-> -String -Path)]
[file-exists? (-> -Pathlike B)]

[assq    (-poly (a b) (a (-lst (-pair a b)) . -> . (Un (-pair a b) (-val #f))))]

[build-path ((list -Pathlike*) -Pathlike* . ->* . -Path)]
[string->number (-> -String (-opt N))]
[with-input-from-file 
    (-poly (a) 
           (cl->
            [(-Pathlike  (-> a))  a]
            [(-Pathlike (-> a) Sym) a]))]
[with-output-to-file
    (-poly (a) 
           (cl->
            [(-Pathlike  (-> a))  a]
            [(-Pathlike (-> a) Sym) a]))]

[random (cl->
         [(-Integer) -Integer]
         [() -Integer])]

[assoc (-poly (a b) (a (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assf  (-poly (a b)
              ((a . -> . B) (-lst (-pair a b)) . -> . (-opt (-pair a b))))]

[list-ref (-poly (a) ((-lst a) -Integer . -> . a))]
[positive? (-> N B)]
[negative? (-> N B)]
[odd? (-> N B)]
[even? (-> N B)]

[apply (-poly (a b) (((list) a . ->* . b) (-lst a) . -> . b))]

[call/cc (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (*Un a b)))]
[call/ec (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (*Un a b)))]

[quotient (-Integer -Integer . -> . -Integer)]
[remainder (-Integer -Integer . -> . -Integer)]
[quotient/remainder (-Integer -Integer . -> . (-values (list -Integer -Integer)))]

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
[regexp-match 
 (cl->
  [((*Un -String -Regexp) -String) (-opt (-lst (-opt -String)))]
  [(-Pattern -String) (-opt (-lst (-opt (*Un -Bytes -String))))]
  [(-Pattern -String N) (-opt (-lst (-opt (*Un -Bytes -String))))]
  [(-Pattern -String N (-opt N)) (-opt (-lst (-opt (*Un -Bytes -String))))]
  [(-Pattern -String N (-opt N) (-opt -Output-Port)) (-lst (-opt (*Un -Bytes -String)))]
  [(-Pattern -String (-opt N) (-opt -Output-Port)) (-lst (-opt (*Un -Bytes -String)))]
  [(-Pattern -String (-opt -Output-Port)) (-lst (-opt (*Un -Bytes -String)))]
  [(-Pattern (*Un -Input-Port -Bytes)) (-opt (-lst (-opt -Bytes)))]
  [(-Pattern (*Un -Input-Port -Bytes) N) (-opt (-lst (-opt -Bytes)))]
  [(-Pattern (*Un -Input-Port -Bytes) N (-opt N)) (-opt (-lst (-opt -Bytes)))]
  [(-Pattern (*Un -Input-Port -Bytes) (-opt N)) (-opt (-lst (-opt -Bytes)))]
  [(-Pattern (*Un -Input-Port -Bytes) N (-opt N) (-opt -Output-Port)) (-lst (-opt -Bytes))])]


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
[expt (cl->* (-Integer -Integer . -> . -Integer) (N N . -> . N))]
[vector (-poly (a) (->* (list) a (-vec a)))]
[real? (Univ . -> . B)]
[real-part (N . -> . N)]
[imag-part (N . -> . N)]
[magnitude (N . -> . N)]
[angle (N . -> . N)]
[numerator (N . -> . -Integer)]
[denominator (N . -> . -Integer)]
[exact->inexact (N . -> . N)]
[inexact->exact (N . -> . N)]
[make-string
 (cl->
  [(N) -String]
  [(N -Char) -String])]
[arithmetic-shift (-Integer -Integer . -> . -Integer)]
[abs (N . -> . N)]
[substring (cl-> [(-String N) -String]
                 [(-String N N) -String])]
[string-length (-String . -> . N)]
[string-set! (-String N -Char . -> . -Void)]
[make-vector
 (-poly (a)
        (cl->
         [(N) (-vec N)]
         [(N a) (-vec a)]))]

[file-exists? (-Pathlike . -> . B)]
[string->symbol (-String . -> . Sym)]
[symbol->string (Sym . -> . -String)]
[vector-length (-poly (a) ((-vec a) . -> . N))]

[call-with-input-file (-poly (a) 
                             (cl->
                              [(-String (-Port . -> . a))  a]
                              [(-String (-Port . -> . a) Sym)  a]))]

[call-with-output-file (-poly (a) 
                              (cl->
                               [(-String (-Port . -> . a))  a]
                               [(-String (-Port . -> . a) Sym)  a]))]
[current-output-port (-Param -Output-Port -Output-Port)]
[current-error-port (-Param -Output-Port -Output-Port)]
[current-input-port (-Param -Input-Port -Input-Port)]
[round (N . -> . N)]
[seconds->date (N . -> . (make-Name #'date))]
[current-seconds (-> N)]
[sqrt (-> N N)]
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
                 (cl->
                  (((-HT a b) a) b)
                  (((-HT a b) a (-> c)) (*Un b c))
                  (((-HT a b) a c) (*Un b c))))]
#;[hash-table-index (-poly (a b) ((-HT a b) a b . -> . -Void))]

[bytes (->* (list) N -Bytes)]
[bytes-ref (-> -Bytes N N)]
[bytes-append (->* (list -Bytes) -Bytes -Bytes)]
[subbytes (cl->
           [(-Bytes N) -Bytes]
           [(-Bytes N N) -Bytes])]
[bytes-length (-> -Bytes N)]
[open-input-file (->key -Pathlike #:mode (Un (-val 'binary) (-val 'text)) #f -Input-Port)]
[close-input-port (-> -Input-Port -Void)]
[close-output-port (-> -Output-Port -Void)]   
[read-line (cl->
            [() -String]
            [(-Input-Port) -String]
            [(-Input-Port Sym) -String])]
[copy-file (-> -Pathlike -Pathlike -Void)]  
[bytes->string/utf-8 (-> -Bytes -String)]

[force (-poly (a) (-> (-Promise a) a))]
[bytes<? (->* (list -Bytes) -Bytes B)]
[regexp-replace* 
 (cl->*
  (-Pattern (*Un -Bytes -String) (*Un -Bytes -String) . -> . -Bytes)
  (-Pattern -String -String . -> . -String))]
[peek-char
 (cl->*
  [-> -Char]
  [-Input-Port . -> . -Char]
  [-Input-Port N . -> . -Char]
  [N . -> . -Char])]
[peek-byte
 (cl->*
  [-> -Byte]
  [-Input-Port . -> . -Byte]
  [-Input-Port N . -> . -Byte]
  [N . -> . -Byte])]
[make-pipe
 (cl->*
  [-> (-values (list -Input-Port -Output-Port))]
  [N . -> . (-values (list -Input-Port -Output-Port))])]
[open-output-bytes
 (cl->*
  [-> -Output-Port]
  [Univ . -> . -Output-Port])]
[get-output-bytes
 (cl->*
  [-Output-Port . -> . -Bytes]
  [-Output-Port Univ . -> . -Bytes]
  [-Output-Port Univ N . -> . -Bytes]
  [-Output-Port Univ N N . -> . -Bytes]
  [-Output-Port N . -> . -Bytes]
  [-Output-Port N N . -> . -Bytes])]
#;[exn:fail? (-> Univ B)]
#;[exn:fail:read? (-> Univ B)]

[write (-> -Sexp -Void)]
[open-output-string (-> -Output-Port)]
;; FIXME - wrong
[get-output-string (-> -Output-Port -String)]

[make-directory (-> -Path -Void)]

[hash-for-each (-poly (a b c)
                      (-> (-HT a b) (-> a b c) -Void))]

[delete-file (-> -Pathlike -Void)]
[make-namespace (cl->* (-> -Namespace)
                       (-> (*Un (-val 'empty) (-val 'initial)) -Namespace))]
[make-base-namespace (-> -Namespace)]
[eval (-> -Sexp Univ)]

[exit (-> (Un))]

[module->namespace (-> -Sexp -Namespace)]
[current-namespace (-Param -Namespace -Namespace)]

;; syntax operations

[expand (-> (-Syntax Univ) (-Syntax Univ))]
[expand-once (-> (-Syntax Univ) (-Syntax Univ))]

[syntax-source (-poly (a) (-> (-Syntax a) Univ))]
[syntax-position (-poly (a) (-> (-Syntax a) (-opt N)))]
[datum->syntax (cl->* 
                (-> (-opt (-Syntax Univ)) Sym (-Syntax Sym))
                (-> (-opt (-Syntax Univ)) Univ (-Syntax Univ)))]
[syntax->datum (-poly (a) (-> (-Syntax a) Univ))]
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
