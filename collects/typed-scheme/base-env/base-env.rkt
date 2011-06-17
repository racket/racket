#lang s-exp "env-lang.rkt"

(require


 (for-template
  (except-in racket -> ->* one-of/c)
  racket/unsafe/ops
  racket/tcp
  racket/fixnum
  racket/future
  (only-in rnrs/lists-6 fold-left)
  '#%paramz
  "extra-procs.rkt"
  (only-in '#%kernel [apply kernel:apply])
  (only-in racket/private/pre-base new-apply-proc)
  scheme/promise scheme/system
  racket/function
  racket/mpair
  racket/base
  racket/set
  syntax/stx racket/private/stx
  (only-in string-constants/private/only-once maybe-print-message)
  (only-in mzscheme make-namespace)
  (only-in racket/match/runtime match:error matchable? match-equality-test))
 (only-in racket/private/pre-base new-apply-proc)
 (only-in (types abbrev numeric-tower) [-Number N] [-Boolean B] [-Symbol Sym])
 (only-in (rep type-rep) make-HashtableTop make-MPairTop
          make-BoxTop make-ChannelTop make-VectorTop
          make-ThreadCellTop
          make-Ephemeron
          make-HeterogenousVector))

;Section 9.2

[raise (Univ . -> . (Un))]

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
[null (-val null)]
[char? (make-pred-ty -Char)]

;Section 3.1

[boolean? (make-pred-ty B)]
[not (make-pred-ty (-val #f))]
[equal? (-> Univ Univ B)]
[eqv? (-> Univ Univ B)]
[eq? (-> Univ Univ B)]

[equal?/recur (-> Univ Univ (-> Univ Univ Univ) B)]
[immutable? (-> Univ B)]
[prop:equal+hash -Struct-Type-Property]


;; scheme/bool
[true (-val #t)]
[false (-val #f)]
[boolean=? (B B . -> . B)]
[symbol=? (Sym Sym . -> . B)]
[false? (make-pred-ty (-val #f))]




[assert (-poly (a b) (cl->*
		      (Univ (make-pred-ty (list a) Univ b) . -> . b)
		      (-> (Un a (-val #f)) a)))]
[defined? (->* (list Univ) -Boolean : (-FS (-not-filter -Undefined 0 null) (-filter -Undefined 0 null)))]


[ormap (-polydots (a c b) (->... (list (->... (list a) (b b) c) (-lst a)) ((-lst b) b) c))]
[andmap (-polydots (a c d b) (cl->*
                              ;; 1 means predicate on second argument
                              (make-pred-ty (list (make-pred-ty (list a) c d) (-lst a)) c (-lst d) 1)
                              (->... (list (->... (list a) (b b) c) (-lst a)) ((-lst b) b) c)))]

[box (-poly (a) (a . -> . (-box a)))]
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
[cons? (make-pred-ty (-pair Univ Univ))]
[pair? (make-pred-ty (-pair Univ Univ))]
[empty? (make-pred-ty (-val null))]
[empty (-val null)]

[make-channel (-poly (a) (-> (-channel a)))]
[channel? (make-pred-ty (make-ChannelTop))]
[channel-get (-poly (a) ((-channel a) . -> . a))]
[channel-try-get (-poly (a) ((-channel a) . -> . (Un a (-val #f))))]
[channel-put (-poly (a) ((-channel a) a . -> . -Void))]



;Section 3.3

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



; racket/string
[string-append*
 (cl->* (-> (-lst -String) -String)
        (-> -String (-lst -String) -String))]

[string-join (-> (-lst -String) -String -String)]


;Section 3.6
[symbol? (make-pred-ty Sym)]
[symbol-interned? (-> Sym B)]
[symbol-unreadable? (-> Sym B)]

[symbol->string (Sym . -> . -String)]


[string->symbol (-String . -> . Sym)]
[string->uninterned-symbol (-String . -> . Sym)]
[string->unreadable-symbol (-String . -> . Sym)]
[gensym (->opt [Sym] Sym)]



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


[namespace-variable-value (Sym [Univ (-opt (-> Univ)) -Namespace] . ->opt . Univ)]

;[match:error (Univ . -> . (Un))]
[match-equality-test (-Param (Univ Univ . -> . Univ) (Univ Univ . -> . Univ))]
[matchable? (make-pred-ty (Un -String -Bytes))]
[void (->* '() Univ -Void)]
[void? (make-pred-ty -Void)]

;Section 10.1

;Section 10.1.1
[thread (-> (-> Univ) -Thread)]
[thread? (make-pred-ty -Thread)]
[current-thread (-> -Thread)]
[thread/suspend-to-kill (-> (-> Univ) -Thread)]
[call-in-nested-thread (-poly (a) (->opt (-> a) [-Custodian] a))]

;Section 10.1.2
[thread-suspend (-Thread . -> . -Void)]
[thread-resume (->opt -Thread [(Un (-val #f) -Thread -Custodian)] -Void)]
[kill-thread (-Thread . -> . -Void)]
[break-thread (-Thread . -> . -Void)]
[sleep ([N] . ->opt . -Void)]
[thread-running? (-Thread . -> . B)]
[thread-dead? (-Thread . -> . B)]

;Section 10.1.3
[thread-wait (-Thread . -> . -Void)]

;TODO need event types
;thread-dead-evt
;thread-resume-evt
;thread-suspend-evt

;Section 10.1.4
[thread-send (-poly (a) 
 (cl->*
  (-> -Thread Univ -Void)
  (-> -Thread Univ (-val #f) (-opt -Void))
  (-> -Thread Univ (-> a) (Un -Void a))))]
[thread-receive (-> Univ)]
[thread-try-receive (-> Univ)]
[thread-rewind-receive (-> (-lst Univ) -Void)]

;Section 10.3.1 (Thread Cells)

[thread-cell? (make-pred-ty (make-ThreadCellTop))]
[make-thread-cell (-poly (a) (->opt a [Univ] (-thread-cell a)))]
[thread-cell-ref (-poly (a) (-> (-thread-cell a) a))]
[thread-cell-set! (-poly (a) (-> (-thread-cell a) a -Void))]
[current-preserved-thread-cell-values
 (cl->* (-> Univ) (-> Univ -Void))]


;Section 10.3.3 (Parameters)

;hidden parameter bindings
[parameterization-key Sym]
[extend-parameterization (-poly (a b) (-> Univ (-Param a b) a Univ))]

[make-parameter (-poly (a b) (cl-> [(a) (-Param a a)]
                                   [(b (a . -> . b)) (-Param a b)]))]
[make-derived-parameter (-poly (a b c d) (-> (-Param a b) (-> c a) (-> b d) (-Param c d)))]
[parameter? (make-pred-ty (-poly (a b) (-Param a b)))]
[parameter-procedure=? (-poly (a b c d) (-> (-Param a b) (-Param c d) B))]

[current-parameterization (-> -Parameterization)]
[call-with-parameterization (-poly (a) (-> -Parameterization (-> a) a))]
[parameterization? (make-pred-ty -Parameterization)]





[future (-poly (A) ((-> A) . -> . (-future A)))]
[touch (-poly (A) ((-future A) . -> . A))]

[reverse (-poly (a) (-> (-lst a) (-lst a)))]
[append (-poly (a) (->* (list) (-lst a) (-lst a)))]
[length (-poly (a) (-> (-lst a) -Index))]
[memq (-poly (a) (-> a (-lst a) (-opt (-lst a))))]
[memv (-poly (a) (-> a (-lst a) (-opt (-lst a))))]
[memf (-poly (a) ((a . -> . B) (-lst a) . -> . (-opt (-lst a))))]
[member (-poly (a) (a (-lst a) . -> . (-opt (-lst a))))]
[findf (-poly (a) ((a . -> . B) (-lst a) . -> . (-opt a)))]

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
[char-general-category (-> -Char (apply Un (map -val
  '(lu ll lt lm lo mn mc me nd nl no ps pe pi pf pd
    pc po sc sm sk so zs zp zl cc cf cs co cn))))]
[make-known-char-range-list (-> (-lst (-Tuple (list -PosInt -PosInt B))))]

[char-upcase (-> -Char -Char)]
[char-downcase (-> -Char -Char)]
[char-titlecase (-> -Char -Char)]
[char-foldcase (-> -Char -Char)]
[char->integer (-> -Char -Index)]
[integer->char (-> -Integer -Char)]
[char-utf-8-length (-> -Char (apply Un (map -val '(1 2 3 4 5 6))))]






[assq  (-poly (a b) (a (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assv  (-poly (a b) (a (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assoc (-poly (a b) (a (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
[assf  (-poly (a b) ((a . -> . Univ) (-lst (-pair a b))
                     . -> . (-opt (-pair a b))))]

[apply        (-poly (a b) (((list) a . ->* . b) (-lst a) . -> . b))]
[new-apply-proc (-poly (a b) (((list) a . ->* . b) (-lst a) . -> . b))]
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
[unsafe-struct-ref top-func]
[unsafe-struct*-ref top-func]
[unsafe-struct-set! top-func]
[unsafe-struct*-set! top-func]

[continuation-mark-set-first (-> (-opt -Cont-Mark-Set) Univ Univ)]
[current-directory (-Param -Pathlike -Path)]
[current-command-line-arguments (-Param (-vec -String) (-vec -String))]


;; Section 3.7
;; Regular Expressions 

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

[string->keyword (-String . -> . -Keyword)]
[keyword->string (-Keyword . -> . -String)]

;; vectors
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
[vector-memq (-poly (a) (-> a (-vec a) (-opt -Index)))]
[vector-memv (-poly (a) (-> a (-vec a) (-opt -Index)))]
[vector-member (-poly (a) (a (-vec a) . -> . (-opt -Index)))]
;; [vector->values no good type here]




[seconds->date (-Integer . -> . (make-Name #'date))]
[current-seconds (-> -Integer)]
[current-print (-Param (Univ . -> . Univ) (Univ . -> . Univ))]

[link-exists? (-> -Pathlike B)]
[directory-exists? (-> -Pathlike B)]
[file-exists? (-> -Pathlike B)]
[directory-list (cl-> [() (-lst -Path)]
                      [(-Pathlike) (-lst -Path)])]
[file-or-directory-modify-seconds
 (cl->* (-Pathlike . -> . -Nat)
        (-Pathlike (-val #f) . -> . -Nat)
        (-Pathlike -Nat . -> . -Void)
        (-Pathlike (-opt -Nat) (-> Univ) . -> . Univ))]

[file-or-directory-permissions (-> -Pathlike (-lst (one-of/c 'read 'write 'execute)))]
[file-or-directory-identity (->opt -Pathlike (Univ) -Nat)]
[file-size (-> -Pathlike -Nat)]


;; path manipulation

[path? (make-pred-ty -Path)]
[path-string? (-> Univ B)]
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



[build-path (cl->*
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

[resolve-path (-> -Path -Path)]
[cleanse-path
 (cl->* (-> -Pathlike -Path)
        (-> -SomeSystemPathlike -SomeSystemPath))]
[expand-user-path (-> -Path -Path)]

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
                (Un -SomeSystemPath (-val 'relative) (-val #f))
                (Un -SomeSystemPath (-val 'up) (-val 'same))
                B))))]

[path-replace-suffix
 (cl->*
  (-> -Pathlike (Un -String -Bytes) -Path)
  (-> -SomeSystemPathlike (Un -String -Bytes) -SomeSystemPath))]

[path-add-suffix
 (cl->*
  (-> -Pathlike (Un -String -Bytes) -Path)
  (-> -SomeSystemPathlike (Un -String -Bytes) -SomeSystemPath))]




[hash? (make-pred-ty (make-HashtableTop))]
[hash-eq? (-> (make-HashtableTop) B)]
[hash-eqv? (-> (make-HashtableTop) B)]
[hash-weak? (-> (make-HashtableTop) B)]
[make-hash (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-hasheq (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-hasheqv (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-weak-hash (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-weak-hasheq (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-weak-hasheqv (-poly (a b) (->opt [(-lst (-pair a b))] (-HT a b)))]
[make-immutable-hash (-poly (a b) (-> (-lst (-pair a b)) (-HT a b)))]
[make-immutable-hasheq (-poly (a b) (-> (-lst (-pair a b)) (-HT a b)))]
[make-immutable-hasheqv (-poly (a b) (-> (-lst (-pair a b)) (-HT a b)))]

[hash-set (-poly (a b) ((-HT a b) a b . -> . (-HT a b)))]
[hash-set! (-poly (a b) ((-HT a b) a b . -> . -Void))]
[hash-ref (-poly (a b c)
                 (cl-> [((-HT a b) a) b]
                       [((-HT a b) a (-> c)) (Un b c)]))]
[hash-ref! (-poly (a b)
                  (cl-> [((-HT a b) a b) b]
                        [((-HT a b) a (-> b)) b]))]
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
[hash-count (-poly (a b) (-> (-HT a b) -Index))]
[hash-keys (-poly (a b) ((-HT a b) . -> . (-lst a)))]
[hash-values (-poly (a b) ((-HT a b) . -> . (-lst b)))]
[hash->list (-poly (a b) ((-HT a b) . -> . (-lst (-pair a b))))]

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

;Set operations
[set (-poly (e) (->* (list) e (-set e)))]
[seteqv (-poly (e) (->* (list) e (-set e)))]
[seteq (-poly (e) (->* (list) e (-set e)))]
[set-empty? (-poly (e) (-> (-set e) B))]
[set-count (-poly (e) (-> (-set e) -Index))]
[set-member? (-poly (e) (-> (-set e) e B))]
[set-add (-poly (e) (-> (-set e) e (-set e)))]

[set-remove (-poly (e) (-> (-set e) e (-set e)))]

[subset? (-poly (e) (-> (-set e) (-set e) B))]
[set-map (-poly (e b) (-> (-set e) (-> e b) (-lst b)))]
[set-for-each (-poly (e b) (-> (-set e) (-> e b) -Void))]
[set? (make-pred-ty (-poly (e) (-set e)))]
[set-equal? (-poly (e) (-> (-set e) B))]
[set-eqv? (-poly (e) (-> (-set e) B))]
[set-eq? (-poly (e) (-> (-set e) B))]

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
[bytes<? (->* (list -Bytes) -Bytes B)]
[bytes>? (->* (list -Bytes) -Bytes B)]
[bytes=? (->* (list -Bytes) -Bytes B)]

[copy-file (-> -Pathlike -Pathlike -Void)]

[force (-poly (a) (-> (-Promise a) a))]


[make-directory (-> -Pathlike -Void)]

[delete-file (-> -Pathlike -Void)]
[make-namespace (->opt [(one-of/c 'empty 'initial)] -Namespace)]
[make-base-namespace (-> -Namespace)]
[eval (->opt Univ [-Namespace] Univ)]

[exit (-> (Un))]

[collect-garbage (-> -Void)]
[current-memory-use (-> -Nat)]
[dump-memory-stats (-> Univ)]

[module->namespace (-> (-mu x (-lst (Un -Symbol -String -Nat x (-val #f)))) -Namespace)]
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

[read-accept-reader (-Param B B)]

[maybe-print-message (-String . -> . -Void)]

[sort (-poly (a b) (cl->* ((-lst a) (a a . -> . B)
                          #:cache-keys? B #f
                          . ->key . (-lst a))
                         ((-lst a) (b b . -> . B)
                          #:key (a . -> . b) #t
                          #:cache-keys? B #f
                          . ->key . (-lst a))))]
[find-system-path (Sym . -> . -Path)]

[object-name (Univ . -> . Univ)]


;; scheme/function
[const (-poly (a) (-> a (->* '() Univ a)))]
(primitive? (-> Univ B))
(primitive-closure? (-> Univ B))


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
                   -Index))]
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
[argmin (-poly (a) ((a . -> . -Real) (-lst a) . -> . a))]
[argmax (-poly (a) ((a . -> . -Real) (-lst a) . -> . a))]

;; scheme/tcp
[tcp-listener? (make-pred-ty -TCP-Listener)]
[tcp-abandon-port (-Port . -> . -Void)]
[tcp-accept (-TCP-Listener . -> . (-values (list -Input-Port -Output-Port)) )]
[tcp-accept/enable-break (-TCP-Listener . -> . (-values (list -Input-Port -Output-Port)) )]
[tcp-accept-ready? (-TCP-Listener . -> . B )]
[tcp-addresses (cl->*
		(-Port [(-val #f)] . ->opt . (-values (list -String -String)))
		(-Port (-val #t) . -> . (-values (list -String -Index -String -Index))))]
[tcp-close (-TCP-Listener . -> . -Void )]
[tcp-connect (-String -Integer . -> . (-values (list -Input-Port -Output-Port)))]
[tcp-connect/enable-break (-String -Integer . -> . (-values (list -Input-Port -Output-Port)))]
[tcp-listen (-Integer [-Integer Univ (-opt -String)] . ->opt . -TCP-Listener)]


;; with-stx.rkt
[generate-temporaries ((Un (-Syntax Univ) (-lst Univ)) . -> . (-lst (-Syntax Sym)))]
[check-duplicate-identifier ((-lst (-Syntax Sym)) . -> . (-opt (-Syntax Sym)))]


[current-continuation-marks (-> -Cont-Mark-Set)]

;; scheme/path

[explode-path (-SomeSystemPathlike . -> . (-lst (Un -SomeSystemPath (one-of/c 'up 'same))))]
[find-relative-path (-SomeSystemPathlike -SomeSystemPathlike . -> . -SomeSystemPath)]
[simple-form-path (-Pathlike . -> . -Path)]
[normalize-path (cl->* (-Pathlike [-Pathlike] . ->opt . -Path))]
[filename-extension (-SomeSystemPathlike . -> . (-opt -Bytes))]
[file-name-from-path (-Pathlike . -> . (-opt -Path))]
[path-only (-SomeSystemPathlike . -> . (-opt -Path))]
[some-system-path->string (-SomeSystemPath . -> . -String)]
[string->some-system-path
 (-String (one-of/c 'unix 'windows) . -> . -SomeSystemPath)]



;; scheme/file
[fold-files
 (-poly
  (a)
  (let ([funarg* (-Path (one-of/c 'file 'dir 'link) a . -> . (-values (list a Univ)))]
        [funarg (-Path (one-of/c 'file 'dir 'link) a . -> . a)])
    (cl->*
     (funarg a [(-opt -Pathlike) Univ]. ->opt . a)
     (funarg* a [(-opt -Pathlike) Univ]. ->opt . a))))]


;; unsafe

[unsafe-vector-length ((make-VectorTop) . -> . -Index)]
[unsafe-vector*-length ((make-VectorTop) . -> . -Index)]
[unsafe-car (-poly (a b)
                   (cl->*
                    (->acc (list (-pair a b)) a (list -car))
                    (->* (list (-lst a)) a)))]
[unsafe-cdr (-poly (a b)
                   (cl->*
                    (->acc (list (-pair a b)) b (list -cdr))
                    (->* (list (-lst a)) (-lst a))))]


;; scheme/vector
[vector-count (-polydots (a b)
                         ((list
                           ((list a) (b b) . ->... . Univ)
                           (-vec a))
                          ((-vec b) b)
                          . ->... .
                          -Index))]
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
[system ((Un -String -Bytes) . -> . -Boolean)]
[system* ((list -Pathlike) (Un -Path -String -Bytes) . ->* . -Boolean)]
[system/exit-code ((Un -String -Bytes) . -> . -Integer)]
[system*/exit-code ((list -Pathlike) (Un -Path -String -Bytes) . ->* . -Integer)]


;; probably the most useful cases
[curry (-poly (a b c)
	      (cl->* ((a b . -> . c) a . -> . (b . -> . c))
		     ((a b . -> . c) . -> . (a . -> . (b . -> . c)))))]
;; mutable pairs
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

;; module names and loading
[resolved-module-path? (make-pred-ty -Resolved-Module-Path)]
[make-resolved-module-path (-> (Un -Symbol -Path) -Resolved-Module-Path)]
[resolved-module-path-name (-> -Resolved-Module-Path (Un -Path -Symbol))]
[module-path? (make-pred-ty -Module-Path)]
[current-module-name-resolver (-Param (cl->* (-Resolved-Module-Path . -> . Univ)
                                             ((Un -Module-Path -Path)
                                              (-opt -Resolved-Module-Path)
                                              (-opt (-Syntax Univ))
                                              -Boolean
                                              . -> . -Resolved-Module-Path))
                                      (cl->* (-Resolved-Module-Path . -> . Univ)
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
                    (-lst (-pair -Symbol
                                 (-pair
                                  (-lst
                                   (Un -Module-Path-Index
                                       (-pair -Module-Path-Index
                                              (-pair (-opt -Integer)
                                                     (-pair -Symbol
                                                            (-pair (-opt -Integer)
                                                                   (-val null)))))))
                                  (-val null))))))
       (-lst (-pair (-opt -Integer)
                    (-lst (-pair -Symbol
                                 (-pair
                                  (-lst
                                   (Un -Module-Path-Index
                                       (-pair -Module-Path-Index
                                              (-pair (-opt -Integer)
                                                     (-pair -Symbol
                                                            (-pair (-opt -Integer)
                                                                   (-val null)))))))
                                  (-val null)))))))))]
[module-compiled-language-info
 (-> -Compiled-Module-Expression
     (-opt (make-HeterogenousVector (list -Module-Path -Symbol Univ))))]

[compose (-poly (a b c) (-> (-> b c) (-> a b) (-> a c)))]


;ephemerons
[make-ephemeron (-poly (k v) (-> k v (make-Ephemeron v)))]
[ephemeron? (make-pred-ty (make-Ephemeron Univ))]
[ephemeron-value (-poly (v) (-> (make-Ephemeron v) (Un (-val #f) v)))]

; syntax/stx (needed for `with-syntax')
[stx->list (-> (-Syntax Univ) (-lst (-Syntax Univ)))]
[stx-list? (-> (-Syntax Univ) -Boolean)]

;Section 12

;Section 12.1 (Ports)

;Section 12.1.1
[current-locale (-Param -String -String)]

;Section 12.1.2

[input-port? (make-pred-ty -Input-Port)]
[output-port? (make-pred-ty -Output-Port)]
[port? (make-pred-ty -Port)]


[close-input-port (-> -Input-Port -Void)]
[close-output-port (-> -Output-Port -Void)]

[port-closed? (-> -Port B)]

[current-input-port (-Param -Input-Port -Input-Port)]
[current-output-port (-Param -Output-Port -Output-Port)]
[current-error-port (-Param -Output-Port -Output-Port)]


[file-stream-port? (-> Univ B)]
[terminal-port? (-> Univ B)]

[eof (-val eof)]
[eof-object? (make-pred-ty (-val eof))]

;Section 12.1.3
[flush-output (->opt [-Output-Port] -Void)]
[file-stream-buffer-mode (cl-> [(-Port) (one-of/c 'none 'line 'block #f)]
                               [(-Port (one-of/c 'none 'line 'block)) -Void])]
[file-position (cl-> [(-Port) -Nat]
                     [(-Port -Integer) -Void])]

;Section 12.1.4
[port-count-lines! (-> (Un -Input-Port -Output-Port) -Void)]
[port-next-location (-> (Un -Input-Port -Output-Port) (-values (list (-opt -PosInt) (-opt -Nat) (-opt -PosInt))))]
[port-count-lines-enabled (-Param Univ B)]

;Section 12.1.5
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


[port-try-file-lock? (-> (Un -Input-Port -Output-Port) (one-of/c 'shared 'exclusive) B)]
[port-file-unlock (-> (Un -Input-Port -Output-Port) -Void)]
[port-file-identity (-> (Un -Input-Port -Output-Port) -PosInt)]

;12.1.6

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


;12.1.7

[make-pipe
 (cl->* [->opt [N] (-values (list -Input-Port -Output-Port))])]
[pipe-content-length (-> (Un -Input-Port -Output-Port) -Nat)]

;12.1.8

[prop:input-port -Struct-Type-Property]
[prop:output-port -Struct-Type-Property]

;12.1.9


;TODO write the types for these
;They are fairly complicated and require events

;make-input-port
;make-output-port

;12.1.10


;12.1.10.1
[port->list
 (-poly (a) (cl->*
  (-> (-lst Univ))
  (->opt (-> -Input-Port a) [-Input-Port] (-lst a))))]
[port->string (->opt [-Input-Port] -String)]
[port->bytes (->opt [-Input-Port] -Bytes)]
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


[call-with-output-string (-> (-> -Output-Port ManyUniv) -String)]
[call-with-output-bytes (-> (-> -Output-Port ManyUniv) -Bytes)]

[with-output-to-string
  (-> (-> ManyUniv) -String)]
[with-output-to-bytes
  (-> (-> ManyUniv) -Bytes)]

[call-with-input-string (-poly (a) (-> -String (-> -Input-Port a) a))]
[call-with-input-bytes (-poly (a) (-> -Bytes (-> -Input-Port a) a))]

[with-input-from-string (-poly (a) (-> -String (-> a) a))]
[with-input-from-bytes (-poly (a) (-> -Bytes (-> a) a))]


;12.1.10.2

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

;12.1.10.3

;eof-evt
;read-bytes-evt
;read-bytes!-evt
;read-bytes-avail!-evt
;read-string-evt
;read-string!-evt
;read-line-evt
;read-bytes-line-evt
;peek-bytes-evt
;peek-bytes!-evt
;peek-bytes-avail!-evt
;peek-string-evt
;peek-string!-evt
;regexp-match-evt

;12.1.10.4

[convert-stream (-> -String -Input-Port -String -Output-Port -Void)]
[copy-port (->* (list -Input-Port -Output-Port) -Output-Port -Void)]

;12.2

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



;; Byte and String Output (Section 12.3 of the Reference)
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


;Section 12.4
;Reading

[read (->opt [-Input-Port] Univ)]
[read-syntax (->opt [Univ -Input-Port] (Un (-Syntax Univ) (-val eof)))]
[read/recursive (->opt [-Input-Port (-opt -Char) (-opt -Read-Table) Univ] Univ)]
[read-syntax/recursive (->opt [Univ -Input-Port (-opt -Char) (-opt -Read-Table) Univ] Univ)]
[read-language (->opt [-Input-Port (-> ManyUniv)] (-> Univ Univ ManyUniv))]

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
[read-accept-lang (-Param Univ B)]
[current-reader-guard (-Param (-> Univ ManyUniv) (-> Univ ManyUniv))]
[current-readtable (-Param (-opt -Read-Table) (-opt -Read-Table))]
[read-on-demand-source (-Param -Path -Path)]

[port-read-handler
 (cl->* (-> -Input-Port (->opt -Input-Port [Univ] Univ))
        (-> -Input-Port (->opt -Input-Port [Univ] Univ) -Void))]

[read-honu (->opt [-Input-Port] Univ)]
[read-honu-syntax (->opt [Univ -Input-Port] (Un (-Syntax Univ) (-val eof)))]
[read-honu/recursive (->opt [-Input-Port (-opt -Char) (-opt -Read-Table) Univ] Univ)]
[read-honu-syntax/recursive (->opt [Univ -Input-Port (-opt -Char) (-opt -Read-Table) Univ] Univ)]

; Section 12.5
; Writing
[write   (Univ [-Output-Port] . ->opt . -Void)]
[display (Univ [-Output-Port] . ->opt . -Void)]
[print   (Univ [-Output-Port] . ->opt . -Void)]
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
[print-honu (-Param Univ B)]
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



;Section 12.6
;The Reader

;Section 12.7
;The Printer



;Section 12.8
;; racket/pretty

[pretty-print (Univ [-Output-Port (one-of/c 0 1)] . ->opt . -Void)]
[pretty-write (Univ [-Output-Port] . ->opt . -Void)]
[pretty-display (Univ [-Output-Port] . ->opt . -Void)]
[pretty-format (Univ [-Output-Port] . ->opt . -Void)]
[pretty-print-handler (-> Univ -Void)]

[pretty-print-columns (-Param (-opt -Nat) (-opt -Nat))]
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


;Section 12.9

;12.9.1
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

;12.9.2
;Nothing defined here


;12.9.3

[special-comment? (make-pred-ty -Special-Comment)]
[make-special-comment (-> Univ -Special-Comment)]
[special-comment-value (-> -Special-Comment Univ)]

;Section 12.10


[prop:custom-write -Struct-Type-Property]
[custom-write? (-> Univ B)]
[custom-write-accessor (-> Univ (-> Univ -Output-Port B ManyUniv))]


[prop:custom-print-quotable -Struct-Type-Property]
[custom-print-quotable? (-> Univ B)]
[custom-print-quotable-accessor (-> Univ Univ)]

;Section 12.11

;Section 12.12



