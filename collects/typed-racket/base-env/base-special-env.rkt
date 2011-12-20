#lang racket/base

;; this file cheats to define types for unexported variables that are expanded into by Racket macros
(require
 "../utils/utils.rkt"
 racket/promise
 string-constants/string-constant
 racket/private/kw racket/file racket/port syntax/parse racket/path
 (for-template (only-in racket/private/kw kw-expander-proc kw-expander-impl)
               racket/base racket/file racket/port racket/path racket/list)
 (utils tc-utils)
 (env init-envs)
 (except-in (rep filter-rep object-rep type-rep) make-arr)
 (types convenience union)
 (only-in (types convenience) [make-arr* make-arr])
 (for-syntax racket/base syntax/parse (only-in racket/syntax syntax-local-eval)))

(define-syntax (define-initial-env stx)
  (syntax-parse stx
    [(_ initialize-env [id-expr ty] ... #:middle [id-expr* ty*] ...)
     #`(begin
         (define initial-env (make-env [id-expr (λ () ty)] ... ))
         (do-time "finished special types")
         (define initial-env* (make-env [id-expr* (λ () ty*)] ...))
         (define (initialize-env) (initialize-type-env initial-env) (initialize-type-env initial-env*))
         (provide initialize-env))]))

(define (make-template-identifier what where)
  (let ([name (module-path-index-resolve (module-path-index-join where #f))])
    (parameterize ([current-namespace (make-empty-namespace)])
      (namespace-attach-module (current-namespace) ''#%kernel)
      (parameterize ([current-module-declare-name name])
        (eval `(,#'module any '#%kernel
                 (#%provide ,what)
                 (define-values (,what) #f))))
      (namespace-require `(for-template ,name))
      (namespace-syntax-introduce (datum->syntax #f what)))))


(define-initial-env initialize-special
  ;; make-promise
  [(make-template-identifier 'delay 'racket/private/promise)
   (-poly (a) (-> (-> a) (-Promise a)))]

  ;; language
  [(make-template-identifier 'language 'string-constants/string-constant)
   -Symbol]
  ;; qq-append
 [(make-template-identifier 'qq-append 'racket/private/qq-and-or)
  (-poly (a b)
        (cl->*
         (-> (-lst a) (-val '()) (-lst a))
         (-> (-lst a) (-lst b) (-lst (*Un a b)))))]
  ;; make-sequence
  [(make-template-identifier 'make-sequence 'racket/private/for)
   (-poly (a b)
          (let ([seq-vals
                 (lambda (a)
                   (-values (list
                             (-> Univ (-values a))
                             (-> Univ Univ)
                             Univ
                             (Un (-> Univ Univ) (-val #f))
                             (Un (->* a Univ) (-val #f))
                             (Un (->* (cons Univ a) Univ) (-val #f)))))])
            (cl->*
             (-> Univ -Byte         (seq-vals (list -Byte)))
             (-> Univ -Index        (seq-vals (list -Index)))
             ;; Generous. Negative numbers aren't allowed.
             (-> Univ -Fixnum       (seq-vals (list -NonNegFixnum)))
             (-> Univ -Int          (seq-vals (list -Nat)))
             (-> Univ (-seq a) (seq-vals (list a)))
             (-> Univ (-seq a b) (seq-vals (list a b))))))]
  ;; in-range
  [(make-template-identifier 'in-range 'racket/private/for)
   (cl->* (-Byte [-Byte -Byte] . ->opt . (-seq -Byte))
          (-PosFixnum -Fixnum [-Nat] . ->opt . (-seq -PosFixnum))
          (-NonNegFixnum [-Fixnum -Nat] . ->opt . (-seq -NonNegFixnum))
          (-Fixnum [-Fixnum -Int] . ->opt . (-seq -Fixnum))
          (-PosInt -Int [-Nat] . ->opt . (-seq -PosInt))
          (-Nat [-Int -Nat] . ->opt . (-seq -Nat))
          (-Int [-Int -Int] . ->opt . (-seq -Int)))]
  ;; in-naturals
  [(make-template-identifier 'in-naturals 'racket/private/for)
   (cl->* (-> -PosInt (-seq -PosInt))
          (-> -Int (-seq -Nat)))]
  ;; in-list
  [(make-template-identifier 'in-list 'racket/private/for)
   (-poly (a) (-> (-lst a) (-seq a)))]
  ;; in-vector
  [(make-template-identifier 'in-vector 'racket/private/for)
   (-poly (a) (->opt (-vec a) [-Int (-opt -Int) -Int] (-seq a)))]
  ;; in-string
  [(make-template-identifier 'in-string 'racket/private/for)
   (->opt -String [-Int (-opt -Int) -Int] (-seq -Char))]
  ;; in-bytes
  [(make-template-identifier 'in-bytes 'racket/private/for)
   (->opt -Bytes [-Int (-opt -Int) -Int] (-seq -Byte))]
  ;; in-hash and friends
  [(make-template-identifier 'in-hash 'racket/private/for)
   (-poly (a b) (-> (-HT a b) (-seq a b)))]
  [(make-template-identifier 'in-hash-keys 'racket/private/for)
   (-poly (a b) (-> (-HT a b) (-seq a)))]
  [(make-template-identifier 'in-hash-values 'racket/private/for)
   (-poly (a b) (-> (-HT a b) (-seq b)))]
  ;; in-port
  [(make-template-identifier 'in-port 'racket/private/for)
   (->opt [(-> -Input-Port Univ) -Input-Port] (-seq Univ))]
  ;; in-input-port-bytes
  [(make-template-identifier 'in-input-port-bytes 'racket/private/for)
   (-> -Input-Port (-seq -Byte))]
  ;; in-input-port-chars
  [(make-template-identifier 'in-input-port-chars 'racket/private/for)
   (-> -Input-Port (-seq -Char))]
  ;; in-lines
  [(make-template-identifier 'in-lines 'racket/private/for)
   (->opt [-Input-Port -Symbol] (-seq -String))]
  ;; in-bytes-lines
  [(make-template-identifier 'in-bytes-lines 'racket/private/for)
   (->opt [-Input-Port -Symbol] (-seq -Bytes))]
  ;; check-in-bytes-lines
  [(make-template-identifier 'check-in-bytes-lines 'racket/private/for)
   (-> Univ Univ Univ)]
  ;; check-in-lines
  [(make-template-identifier 'check-in-lines 'racket/private/for)
   (-> Univ Univ Univ)]
  ;; check-in-port
  [(make-template-identifier 'check-in-port 'racket/private/for)
   (-> Univ Univ Univ)]
  ;; from the expansion of `with-syntax'
  [(make-template-identifier 'apply-pattern-substitute 'racket/private/stxcase)
   (->* (list (-Syntax Univ) Univ) Univ Any-Syntax)]
  ;; same
  [(make-template-identifier 'with-syntax-fail 'racket/private/with-stx)
   (-> (-Syntax Univ) (Un))]


  [(make-template-identifier 'make-temporary-file/proc 'racket/file)
   (->opt [-String (Un -Pathlike (-val 'directory) (-val #f)) (-opt -Pathlike)] -Path)]

  ;; below here: keyword-argument functions from the base environment
  ;; FIXME: abstraction to remove duplication here
  #:middle

  [((kw-expander-proc (syntax-local-value #'file->string)))
   (->key -Pathlike #:mode (one-of/c 'binary 'text) #f -String)]
  [((kw-expander-impl (syntax-local-value #'file->string)))
   (-> (Un (-val #f) (one-of/c 'binary 'text)) -Boolean -Pathlike -String)]

  [((kw-expander-proc (syntax-local-value #'file->bytes)))
   (->key -Pathlike #:mode (one-of/c 'binary 'text) #f -Bytes)]
  [((kw-expander-impl (syntax-local-value #'file->bytes)))
   (-> (Un (-val #f) (one-of/c 'binary 'text)) -Boolean -Pathlike -Bytes)]

  [((kw-expander-proc (syntax-local-value #'file->value)))
   (->key -Pathlike #:mode (one-of/c 'binary 'text) #f Univ)]
  [((kw-expander-impl (syntax-local-value #'file->value)))
   (-> (Un (-val #f) (one-of/c 'binary 'text)) -Boolean -Pathlike Univ)]

  [((kw-expander-proc (syntax-local-value #'file->lines)))
   (->key -Pathlike #:mode (one-of/c 'binary 'text) #f
        #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f
        (-lst -String))]
  [((kw-expander-impl (syntax-local-value #'file->lines)))
   (-> (Un (-val #f) (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)) -Boolean
       (Un (-val #f) (one-of/c 'binary 'text)) -Boolean
       -Pathlike (-lst -String))]

  [((kw-expander-proc (syntax-local-value #'file->bytes-lines)))
   (->key -Pathlike
          #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f
          #:mode (one-of/c 'binary 'text) #f
          (-lst -Bytes))]
  [((kw-expander-impl (syntax-local-value #'file->bytes-lines)))
   (-> (Un (-val #f) (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)) -Boolean
       (Un (-val #f) (one-of/c 'binary 'text)) -Boolean
       -Pathlike (-lst -Bytes))]

  [((kw-expander-proc (syntax-local-value #'display-to-file)))
   (->key Univ -Pathlike
        #:exists (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace) #f
        #:mode (one-of/c 'binary 'text) #f
        -Void)]
  [((kw-expander-impl (syntax-local-value #'display-to-file)))
   (-> (Un (-val #f) (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace)) -Boolean
       (Un (-val #f) (one-of/c 'binary 'text)) -Boolean
       Univ -Pathlike -Void)]

  [((kw-expander-proc (syntax-local-value #'display-lines-to-file)))
   (->key (-lst Univ) -Pathlike
        #:separator Univ #f
        #:mode (one-of/c 'binary 'text) #f
        #:exists (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace) #f
        -Void)]
  [((kw-expander-impl (syntax-local-value #'display-lines-to-file)))
   (-> (Un (-val #f) (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace)) -Boolean
       (Un (-val #f) (one-of/c 'binary 'text)) -Boolean
       (Un (-val #f) Univ) -Boolean
       (-lst Univ) -Pathlike -Void)]

  [((kw-expander-proc (syntax-local-value #'write-to-file)))
   (->key Univ -Pathlike
        #:exists (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace) #f
        #:mode (one-of/c 'binary 'text) #f
        -Void)]
  [((kw-expander-impl (syntax-local-value #'write-to-file)))
   (-> (Un (-val #f) (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace)) -Boolean
       (Un (-val #f) (one-of/c 'binary 'text)) -Boolean
       Univ -Pathlike -Void)]

  [((kw-expander-proc (syntax-local-value #'file->list)))
   (-poly (a)
          (cl->* (->key -Pathlike #:mode (one-of/c 'binary 'text) #f (-lst Univ))
                 (->key -Pathlike (-> -Input-Port a) #:mode (one-of/c 'binary 'text) #f (-lst a))))]
  [((kw-expander-impl (syntax-local-value #'file->list)))
   (-poly (a)
          (cl->* (-> (Un (-val #f) (one-of/c 'binary 'text)) -Boolean -Pathlike (-> -Input-Port a) (-val #t) (-lst a))
                 (-> (Un (-val #f) (one-of/c 'binary 'text)) -Boolean -Pathlike Univ               -Boolean  (-lst Univ))))]

  [((kw-expander-proc (syntax-local-value #'get-preference)))
   (let ((use-lock-type Univ)
         (timeout-lock-there-type (-opt (-> -Path Univ)))
         (lock-there-type (-opt (-> -Path Univ))))
     (cl->*
      (->key -Symbol
             #:use-lock? use-lock-type #f #:timeout-lock-there timeout-lock-there-type #f #:lock-there lock-there-type #f
             Univ)
      (->key -Symbol (-> Univ)
             #:use-lock? use-lock-type #f #:timeout-lock-there timeout-lock-there-type #f #:lock-there lock-there-type #f
             Univ)
      (->key -Symbol (-> Univ) Univ
             #:use-lock? use-lock-type #f #:timeout-lock-there timeout-lock-there-type #f #:lock-there lock-there-type #f
             Univ)
      (->key -Symbol (-> Univ) Univ (-opt -Pathlike)
             #:use-lock? use-lock-type #f #:timeout-lock-there timeout-lock-there-type #f #:lock-there lock-there-type #f
             Univ)))]
  [((kw-expander-impl (syntax-local-value #'get-preference)))
   (let ((use-lock-type Univ)
         (timeout-lock-there-type (-opt (-> -Path Univ)))
         (lock-there-type (-opt (-> -Path Univ))))
     (-> (-opt lock-there-type) -Boolean
         (-opt timeout-lock-there-type) -Boolean
         (-opt use-lock-type) -Boolean
         -Symbol
         (-opt (-> Univ)) (-opt Univ) (-opt (-opt -Pathlike))
         -Boolean -Boolean -Boolean
         Univ))]
  [((kw-expander-proc (syntax-local-value #'make-handle-get-preference-locked)))
   (let ((lock-there-type (-opt (-> -Path Univ))) (max-delay-type -Real))
     (cl->*
      (->key -Real -Symbol
             #:lock-there lock-there-type #f #:max-delay max-delay-type #f
             (-> -Pathlike Univ))
      (->key -Real -Symbol (-> Univ)
             #:lock-there lock-there-type #f #:max-delay max-delay-type #f
             (-> -Pathlike Univ))
      (->key -Real -Symbol (-> Univ) Univ
             #:lock-there lock-there-type #f #:max-delay max-delay-type #f
             (-> -Pathlike Univ))
      (->key -Real -Symbol (-> Univ) Univ (-opt -Pathlike)
             #:lock-there lock-there-type #f #:max-delay max-delay-type #f
             (-> -Pathlike Univ))))]
  [((kw-expander-impl (syntax-local-value #'make-handle-get-preference-locked)))
   (let ((lock-there-type (-opt (-> -Path Univ))) (max-delay-type -Real))
     (-> (-opt lock-there-type) -Boolean
         (-opt max-delay-type) -Boolean
         -Real -Symbol
         (-opt (-> Univ)) (-opt Univ) (-opt (-opt -Pathlike))
         -Boolean -Boolean -Boolean
         (-> -Pathlike Univ)))]

  [((kw-expander-proc (syntax-local-value #'call-with-file-lock/timeout)))
   (-poly (a)
          (->key (-opt -Pathlike)
                 (one-of/c 'shared 'exclusive)
                 (-> a)
                 (-> a)
                 #:lock-file (-opt -Pathlike) #f
                 #:delay -Real #f
                 #:max-delay -Real #f
                 a))]
  [((kw-expander-impl (syntax-local-value #'call-with-file-lock/timeout)))
   (-poly (a)
          (-> (-opt -Real) -Boolean
              (-opt (-opt -Pathlike)) -Boolean
              (-opt -Real) -Boolean
              (-opt -Pathlike)
              (one-of/c 'shared 'exclusive)
              (-> a)
              (-> a)
              a))]

   [((kw-expander-proc (syntax-local-value #'sort)))
    (-poly (a b) (cl->* ((-lst a) (a a . -> . -Boolean)
                                  #:cache-keys? -Boolean #f
                                  . ->key . (-lst a))
                        ((-lst a) (b b . -> . -Boolean)
                                  #:key (a . -> . b) #t
                                  #:cache-keys? -Boolean #f
                                  . ->key . (-lst a))))]
  [((kw-expander-impl (syntax-local-value #'sort)))
   (-poly (a b)
          (cl->*
           ;; #:key not provided
           (->
            -Boolean -Boolean Univ (-val #f)
            (-lst a) (a a . -> . -Boolean)
            (-lst a))
           ;; #:key provided
           (->
            -Boolean -Boolean (a . -> . b) (-val #t)
            (-lst a) (b b . -> . -Boolean)
            (-lst a))))]
  
  [((kw-expander-proc (syntax-local-value #'remove-duplicates)))
   (-poly (a b) (cl->* 
                 ((-lst a) . -> . (-lst a))
                 ((-lst a) (a a . -> . Univ)
                           . -> . (-lst a))
                 ((-lst a) #:key (a . -> . b) #f
                           . ->key . (-lst a))                     
                 ((-lst a) (b b . -> . Univ)
                           #:key (a . -> . b) #t
                           . ->key . (-lst a))))]
  [((kw-expander-impl (syntax-local-value #'remove-duplicates)))
   (-poly (a b)
          (cl->*
           (Univ (-val #f) ;; no key
            (-lst a) (-val #f) -Boolean
            . -> . (-lst a))
           (Univ (-val #f) ;; no key
            (-lst a) (-> a a Univ) -Boolean
            . -> . (-lst a))
           ((a . -> . b) (-val #t) ;; no key
            (-lst a) (-opt (-> b b Univ)) -Boolean
            . -> . (-lst a))))]

  [((kw-expander-proc (syntax-local-value #'open-input-file)))
   (->key -Pathlike #:mode (one-of/c 'binary 'text) #f -Input-Port)]
  [((kw-expander-impl (syntax-local-value #'open-input-file)))
   (-> (-opt (one-of/c 'binary 'text)) -Boolean -Pathlike -Input-Port)]

  [((kw-expander-proc (syntax-local-value #'open-output-file)))
   (->key -Pathlike
        #:mode (one-of/c 'binary 'text) #f
        #:exists (one-of/c 'error 'append 'update 'can-update
                           'replace 'truncate
                           'must-truncate 'truncate/replace)
        #f
        -Output-Port)]
  [((kw-expander-impl (syntax-local-value #'open-output-file)))
   (-> (-opt (one-of/c 'error 'append 'update 'can-update
                           'replace 'truncate
                           'must-truncate 'truncate/replace))
       -Boolean
       (-opt (one-of/c 'binary 'text)) -Boolean
       -Pathlike
       -Output-Port)]

  [((kw-expander-proc (syntax-local-value #'open-input-output-file)))
   (->key -Pathlike
        #:mode (one-of/c 'binary 'text) #f
        #:exists (one-of/c 'error 'append 'update 'can-update
                           'replace 'truncate
                           'must-truncate 'truncate/replace)
        #f
        (-values (list -Input-Port -Output-Port)))]
  [((kw-expander-impl (syntax-local-value #'open-input-output-file)))
   (-> (-opt (one-of/c 'error 'append 'update 'can-update
                           'replace 'truncate
                           'must-truncate 'truncate/replace))
       -Boolean
       (-opt (one-of/c 'binary 'text)) -Boolean
       -Pathlike
       (-values (list -Input-Port -Output-Port)))]

  [((kw-expander-proc (syntax-local-value #'call-with-input-file)))
   (-poly (a) (-Pathlike (-Input-Port . -> . a) #:mode (Un (-val 'binary) (-val 'text)) #f . ->key .  a))]
  [((kw-expander-impl (syntax-local-value #'call-with-input-file)))
   (-poly (a) (-> (-opt (one-of/c 'binary 'text)) -Boolean -Pathlike (-Input-Port . -> . a) a))]

  [((kw-expander-proc (syntax-local-value #'call-with-output-file)))
   (-poly (a) (-Pathlike (-Output-Port . -> . a)
                                   #:exists (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace) #f
                                   #:mode (one-of/c 'binary 'text) #f
                                   . ->key .  a))]
  [((kw-expander-impl (syntax-local-value #'call-with-output-file)))
   (-poly (a) (-> (-opt (one-of/c 'error 'append 'update 'can-update
                           'replace 'truncate
                           'must-truncate 'truncate/replace))
                  -Boolean
                  (-opt (one-of/c 'binary 'text)) -Boolean
                  -Pathlike (-Output-Port . -> . a)
                  a))]

  ;;

  [((kw-expander-proc (syntax-local-value #'call-with-input-file*)))
   (-poly (a) (-Pathlike (-Input-Port . -> . a) #:mode (Un (-val 'binary) (-val 'text)) #f . ->key .  a))]
  [((kw-expander-impl (syntax-local-value #'call-with-input-file*)))
   (-poly (a) (-> (-opt (one-of/c 'binary 'text)) -Boolean -Pathlike (-Input-Port . -> . a) a))]

  [((kw-expander-proc (syntax-local-value #'call-with-output-file*)))
   (-poly (a) (-Pathlike (-Output-Port . -> . a)
                         #:exists (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace) #f
                         #:mode (one-of/c 'binary 'text) #f
                         . ->key .  a))]
  [((kw-expander-impl (syntax-local-value #'call-with-output-file*)))
   (-poly (a) (-> (-opt (one-of/c 'error 'append 'update 'can-update
                                  'replace 'truncate
                                  'must-truncate 'truncate/replace))
                  -Boolean
                  (-opt (one-of/c 'binary 'text)) -Boolean
                  -Pathlike (-Output-Port . -> . a)
                  a))]
  ;;
  [((kw-expander-proc (syntax-local-value #'with-input-from-file)))
   (-poly (a) (-Pathlike (-> a) #:mode (Un (-val 'binary) (-val 'text)) #f . ->key .  a))]
  [((kw-expander-impl (syntax-local-value #'with-input-from-file)))
   (-poly (a) (-> (-opt (one-of/c 'binary 'text)) -Boolean -Pathlike (-> a) a))]

  [((kw-expander-proc (syntax-local-value #'with-output-to-file)))
   (-poly (a) (->key -Pathlike (-> a)
                   #:exists (one-of/c 'error 'append 'update 'can-update
                           'replace 'truncate
                           'must-truncate 'truncate/replace)
                   #f
                   #:mode (one-of/c 'binary 'text) #f
                   a))]
  [((kw-expander-impl (syntax-local-value #'with-output-to-file)))
   (-poly (a) (-> (-opt (one-of/c 'error 'append 'update 'can-update
                           'replace 'truncate
                           'must-truncate 'truncate/replace))
                  -Boolean
                  (-opt (one-of/c 'binary 'text)) -Boolean
                  -Pathlike (-> a)
                  a))]


  [((kw-expander-proc (syntax-local-value #'port->lines)))
   (cl->*
    (->key #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -String))
    (->key -Input-Port #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -String)))]
  [((kw-expander-impl (syntax-local-value #'port->lines)))
   ((-opt (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)) -Boolean
    (-opt -Input-Port) -Boolean
    . -> .
    (-lst -String))]


  [((kw-expander-proc (syntax-local-value #'port->bytes-lines)))
   (cl->*
    (->key #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -Bytes))
    (->key -Input-Port #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -Bytes)))]
  [((kw-expander-impl (syntax-local-value #'port->bytes-lines)))
   ((-opt (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)) -Boolean
    (-opt -Input-Port) -Boolean
    . -> .
    (-lst -Bytes))]


  [((kw-expander-proc (syntax-local-value #'display-lines)))
   (cl->*
    ((-lst Univ) #:separator Univ #f . ->key . -Void)
    ((-lst Univ) -Output-Port #:separator Univ #f . ->key . -Void))]
  [((kw-expander-impl (syntax-local-value #'display-lines)))
   ((-opt Univ) -Boolean
    (-lst Univ)
    (-opt -Output-Port) -Boolean
    . -> . -Void)]
  ; [find-relative-path (-SomeSystemPathlike -SomeSystemPathlike . -> . -SomeSystemPath)]
  [((kw-expander-proc (syntax-local-value #'find-relative-path)))
   (-SomeSystemPathlike -SomeSystemPathlike  #:more-than-root? Univ #f . ->key . -SomeSystemPath)]
  [((kw-expander-impl (syntax-local-value #'find-relative-path)))
   (Univ -Boolean -SomeSystemPathlike -SomeSystemPathlike  . -> . -SomeSystemPath)]
  )
