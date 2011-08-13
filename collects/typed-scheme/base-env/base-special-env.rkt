#lang racket/base

;; this file cheats to define types for unexported variables that are expanded into by Racket macros
(require
 "../utils/utils.rkt"
 racket/promise
 string-constants/string-constant
 racket/private/kw racket/file racket/port
 (for-syntax racket/base syntax/parse (only-in racket/syntax syntax-local-eval)
             (utils tc-utils)
             (env init-envs)
             (except-in (rep filter-rep object-rep type-rep) make-arr)
             (types convenience union)
             (only-in (types convenience) [make-arr* make-arr])))

(define-syntax (define-initial-env stx)
  (syntax-parse stx
    [(_ initialize-env [id-expr ty] ...)
     (with-syntax ([(id ...)
                    (for/list ([expr (syntax->list #'(id-expr ...))])
                      (syntax-local-eval expr))])
       #`(begin
           (define-for-syntax initial-env
             (make-env
              [id ty] ...))
           (define-for-syntax (initialize-env)
             (initialize-type-env initial-env))
           (provide (for-syntax initialize-env))))]))

(define-initial-env initialize-special
  ;; make-promise
  [(syntax-parse (local-expand #'(delay 3) 'expression null)
     #:context #'make-promise
     [(_ mp . _) #'mp])
   (-poly (a) (-> (-> a) (-Promise a)))]
  ;; language
  [(syntax-parse (local-expand #'(this-language) 'expression null)
     #:context #'language
     [lang #'lang])
   -Symbol]
  ;; qq-append
  [(syntax-parse (local-expand #'`(,@'() 1) 'expression null)
     #:context #'qq-append
     [(_ qqa . _) #'qqa])
   (-poly (a b)
         (cl->*
          (-> (-lst a) (-val '()) (-lst a))
          (-> (-lst a) (-lst b) (-lst (*Un a b)))))]
  ;; make-sequence
  [(syntax-parse (local-expand #'(for ([x '()]) x) 'expression #f)
     #:context #'make-sequence
     #:literals (let-values quote)
     [(let-values ([_ (m-s '(_) '())]) . _) #'m-s])
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
             (-> Univ (-seq a) (seq-vals (list a)))
             (-> Univ (-seq a b) (seq-vals (list a b))))))]
  ;; in-range
  [(syntax-parse (local-expand #'(in-range 1) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (cl->* (-PosFixnum -Fixnum [-Nat] . ->opt . (-seq -PosFixnum))
          (-NonNegFixnum [-Fixnum -Nat] . ->opt . (-seq -NonNegFixnum))
          (-Fixnum [-Fixnum -Int] . ->opt . (-seq -Fixnum))
          (-PosInt -Int [-Nat] . ->opt . (-seq -PosInt))
          (-Nat [-Int -Nat] . ->opt . (-seq -Nat))
          (-Int [-Int -Int] . ->opt . (-seq -Int)))]
  ;; in-naturals
  [(syntax-parse (local-expand #'(in-naturals) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (cl->* (-> -PosInt (-seq -PosInt))
          (-> -Int (-seq -Nat)))]
  ;; in-list
  [(syntax-parse (local-expand #'(in-list '(1 2 3)) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a) (-> (-lst a) (-seq a)))]
  ;; in-vector
  [(syntax-parse (local-expand #'(in-vector (vector 1 2 3)) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a) (->opt (-vec a) [-Int (-opt -Int) -Int] (-seq a)))]
  ;; in-string
  [(syntax-parse (local-expand #'(in-string "abc") 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt -String [-Int (-opt -Int) -Int] (-seq -Char))]
  ;; in-bytes
  [(syntax-parse (local-expand #'(in-bytes #"abc") 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt -Bytes [-Int (-opt -Int) -Int] (-seq -Byte))]
  ;; in-hash and friends
  [(syntax-parse (local-expand #'(in-hash #hash((1 . 2))) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a b) (-> (-HT a b) (-seq a b)))]
  [(syntax-parse (local-expand #'(in-hash-keys #hash((1 . 2))) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a b) (-> (-HT a b) (-seq a)))]
  [(syntax-parse (local-expand #'(in-hash-values #hash((1 . 2))) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a b) (-> (-HT a b) (-seq b)))]
  ;; in-port
  [(syntax-parse (local-expand #'(in-port) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt [(-> -Input-Port Univ) -Input-Port] (-seq Univ))]
  ;; in-input-port-bytes
  [(syntax-parse (local-expand #'(in-input-port-bytes (open-input-bytes #"abc")) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-> -Input-Port (-seq -Byte))]
  ;; in-input-port-chars
  [(syntax-parse (local-expand #'(in-input-port-chars (open-input-string "abc")) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-> -Input-Port (-seq -Char))]
  ;; in-lines
  [(syntax-parse (local-expand #'(in-lines) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt [-Input-Port -Symbol] (-seq -String))]
  ;; in-bytes-lines
  [(syntax-parse (local-expand #'(in-bytes-lines) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt [-Input-Port -Symbol] (-seq -Bytes))]
  ;; check-in-bytes-lines
  [(syntax-parse (local-expand #'(for ([i (in-bytes-lines 0)]) i)
                               'expression #f)
                 #:literals (let-values let)
     [(let-values ((_ (let _ (c . _) . _))
                   . _)
        . _)
      #'c])
   (-> Univ Univ Univ)]
  ;; check-in-lines
  [(syntax-parse (local-expand #'(for ([i (in-lines 0)]) i)
                               'expression #f)
                 #:literals (let-values #%app let)
     [(let-values ((_ (let _ (c . _) . _))
                   . _)
        . _)
      #'c])
   (-> Univ Univ Univ)]
  ;; check-in-port
  [(syntax-parse (local-expand #'(for ([i (in-port 0)]) i)
                               'expression #f)
                 #:literals (let-values #%app let)
     [(let-values ((_ (let _ (c . _) . _))
                   . _)
        . _)
      #'c])
   (-> Univ Univ Univ)]
  ;; from the expansion of `with-syntax'
  [(syntax-parse (local-expand #'(with-syntax ([x 1]) #'(x)) 'expression null)
     #:literals (let-values #%plain-app #%plain-lambda if letrec-syntaxes+values)
     [(let-values _
        (let-values _
          (let-values _
            (if _
                (let-values _ (letrec-syntaxes+values _ _ (#%plain-app (#%plain-lambda _ (#%plain-app apply-pattern-substitute _ _ _)) _)))
                _))))
      #'apply-pattern-substitute])
   (->* (list (-Syntax Univ) Univ) Univ Any-Syntax)]

  [(syntax-parse (local-expand #'(with-syntax ([x 1]) #'(x)) 'expression null)
     #:literals (let-values #%plain-app #%plain-lambda if letrec-syntaxes+values)
    [(let-values _ (let-values _
                     (let-values _ (if _ _ (let-values _
                                             (if _ (let-values _ (letrec-syntaxes+values _ _ (#%plain-app with-syntax-fail _))) _))))))
      #'with-syntax-fail])
   (-> (-Syntax Univ) (Un))]

  ;; below here: keyword-argument functions from the base environment
  ;; FIXME: abstraction to remove duplication here

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
          (-lst -String))]
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
#;  [((kw-expander-impl (syntax-local-value #'port->bytes-lines)))
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
  )
