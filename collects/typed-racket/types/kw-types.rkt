#lang racket/base

(require "convenience.rkt" "../rep/type-rep.rkt"
         "union.rkt" "abbrev.rkt" "../utils/tc-utils.rkt"
         racket/list racket/dict racket/set racket/match)

;; convert : [Listof Keyword] [Listof Type] [Listof Type] [Option Type] [Option Type] -> (values Type Type)
(define (convert kw-t plain-t opt-t rng rest drest)
  (define-values (mand-kw-t opt-kw-t) (partition (match-lambda [(Keyword: _ _ m) m]) kw-t))
  (define arities
    (for/list ([i (length opt-t)])
      (make-arr* (append plain-t (take opt-t i))
                 rng
                 #:kws kw-t
                 #:rest rest
                 #:drest drest)))
  (define ts 
    (flatten
     (list
      mand-kw-t
      (for/list ([k (in-list opt-kw-t)])
        (match k
          [(Keyword: _ t _) (list (-opt t) -Boolean)]))
      plain-t
      (for/list ([t (in-list opt-t)]) (-opt t))
      (for/list ([t (in-list opt-t)]) -Boolean))))  
  (make-Function (list (make-arr* ts rng #:rest rest #:drest drest))))

(define (prefix-of a b)
  (define (drest-equal? a b)
    (match* (a b)
      [((list t b) (list t* b*)) (and (type-equal? t t*) (equal? b b*))]
      [(_ _) #f]))
  (define (kw-equal? a b)
    (and (equal? (length a) (length b))
         (for/and ([k1 a] [k2 b])
           (type-equal? k1 k2))))
  (match* (a b)
    [((arr: args result rest drest kws)
      (arr: args* result* rest* drest* kws*))
     (and (< (length args) (length args*))
          (or (equal? rest rest*) (type-equal? rest rest*))
          (or (equal? drest drest*) (drest-equal? drest drest*))
          (type-equal? result result*)
          (or (equal? kws kws*) (kw-equal? kws kws*))
          (for/and ([p args] [p* args*])
            (type-equal? p p*)))]))

(define (arity-length a)
  (match a
    [(arr: args result rest drest kws) (length args)]))


(define (arg-diff a1 a2)
  (match a2
    [(arr: args _ _ _ _) (drop args (arity-length a1))]))

(define (find-prefixes l)
  (define l* (sort l < #:key arity-length))
  (for/fold ([d (list)]) ([e (in-list l*)])
    (define prefix (for/or ([p (in-dict-keys d)])
                     (and (prefix-of p e) p)))
    (if prefix
        (dict-set d prefix (arg-diff prefix e))
        (dict-set d e empty))))

(define (kw-convert ft)
    (match ft
      [(Function: arrs)
       (define table (find-prefixes arrs))
       (define fns 
         (for/list ([(k v) (in-dict table)])
           (match k
             [(arr: mand rng rest drest kws)
              (convert kws mand v rng rest drest)])))
       (apply cl->* fns)]
      [(Poly-names: names (Function: arrs))
       (define table (find-prefixes arrs))
       (define fns 
         (for/list ([(k v) (in-dict table)])
           (match k
             [(arr: mand rng rest drest kws)
              (convert kws mand v rng rest drest)])))
       (make-Poly names (apply cl->* fns))]
      [_ (int-err 'kw-convert "non-function type" ft)]))

(provide kw-convert)

#|
(define pre
  (list
   (->key -Pathlike #:mode (one-of/c 'binary 'text) #f -String)
   (->key -Pathlike #:mode (one-of/c 'binary 'text) #f -Bytes)
   (->key -Pathlike #:mode (one-of/c 'binary 'text) #f Univ)
   (->key
    -Pathlike
    #:mode
    (one-of/c 'binary 'text)
    #f
    #:line-mode
    (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)
    #f
    (-lst -String))
   (->key
    -Pathlike
    #:line-mode
    (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)
    #f
    #:mode
    (one-of/c 'binary 'text)
    #f
    (-lst -Bytes))
   (->key
    Univ
    -Pathlike
    #:exists
    (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace)
    #f
    #:mode
    (one-of/c 'binary 'text)
    #f
    -Void)
   (->key
    (-lst Univ)
    -Pathlike
    #:separator
    Univ
    #f
    #:mode
    (one-of/c 'binary 'text)
    #f
    #:exists
    (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace)
    #f
    -Void)
   (->key
    Univ
    -Pathlike
    #:exists
    (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace)
    #f
    #:mode
    (one-of/c 'binary 'text)
    #f
    -Void)
   (-poly
    (a)
    (cl->*
     (->optkey -Pathlike [(-> -Input-Port a)]    #:mode (one-of/c 'binary 'text) #f (-lst a))
     (->optkey -Pathlike [(-> -Input-Port Univ)] #:mode (one-of/c 'binary 'text) #f (-lst Univ))))
   (let ((use-lock-type Univ) (timeout-lock-there-type (-opt (-> -Path Univ))) (lock-there-type (-opt (-> -Path Univ))))
     (cl->*
      (->key
       -Symbol
       #:use-lock?
       use-lock-type
       #f
       #:timeout-lock-there
       timeout-lock-there-type
       #f
       #:lock-there
       lock-there-type
       #f
       Univ)
      (->key
       -Symbol
       (-> Univ)
       #:use-lock?
       use-lock-type
       #f
       #:timeout-lock-there
       timeout-lock-there-type
       #f
       #:lock-there
       lock-there-type
       #f
       Univ)
      (->key
       -Symbol
       (-> Univ)
       Univ
       #:use-lock?
       use-lock-type
       #f
       #:timeout-lock-there
       timeout-lock-there-type
       #f
       #:lock-there
       lock-there-type
       #f
       Univ)
      (->key
       -Symbol
       (-> Univ)
       Univ
       (-opt -Pathlike)
       #:use-lock?
       use-lock-type
       #f
       #:timeout-lock-there
       timeout-lock-there-type
       #f
       #:lock-there
       lock-there-type
       #f
       Univ)))
   (let ((lock-there-type (-opt (-> -Path Univ))) (max-delay-type -Real))
     (cl->*
      (->key -Real -Symbol #:lock-there lock-there-type #f #:max-delay max-delay-type #f (-> -Pathlike Univ))
      (->key -Real -Symbol (-> Univ) #:lock-there lock-there-type #f #:max-delay max-delay-type #f (-> -Pathlike Univ))
      (->key -Real -Symbol (-> Univ) Univ #:lock-there lock-there-type #f #:max-delay max-delay-type #f (-> -Pathlike Univ))
      (->key
       -Real
       -Symbol
       (-> Univ)
       Univ
       (-opt -Pathlike)
       #:lock-there
       lock-there-type
       #f
       #:max-delay
       max-delay-type
       #f
       (-> -Pathlike Univ))))
   (-poly
    (a)
    (->key
     (-opt -Pathlike)
     (one-of/c 'shared 'exclusive)
     (-> a)
     (-> a)
     #:lock-file
     (-opt -Pathlike)
     #f
     #:delay
     -Real
     #f
     #:max-delay
     -Real
     #f
     a))
   (-poly
    (a b)
    (cl->*
     (->key (-lst a) (-> a a -Boolean) #:key (-> a a) #f #:cache-keys? -Boolean #f (-lst a))
     (->key (-lst a) (-> b b -Boolean) #:key (-> a b) #f #:cache-keys? -Boolean #f (-lst a))))
   (-poly
    (a b)
    (cl->*
     (->optkey (-lst a) [(-> a a Univ)] #:key (-> a a) #f (-lst a))
     (->optkey (-lst a) [(-> b b Univ)] #:key (-> a b) #f (-lst a))))
   (->key -Pathlike #:mode (one-of/c 'binary 'text) #f -Input-Port)
   (->key
    -Pathlike
    #:mode
    (one-of/c 'binary 'text)
    #f
    #:exists
    (one-of/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace)
    #f
    -Output-Port)
   (->key
    -Pathlike
    #:mode
    (one-of/c 'binary 'text)
    #f
    #:exists
    (one-of/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace)
    #f
    (-values (list -Input-Port -Output-Port)))
   (-poly (a) (->key -Pathlike (-> -Input-Port a) #:mode (Un (-val 'binary) (-val 'text)) #f a))
   (-poly
    (a)
    (->key
     -Pathlike
     (-> -Output-Port a)
     #:exists
     (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace  'can-update 'must-truncate)
     #f
     #:mode
     (one-of/c 'binary 'text)
     #f
     a))
   (-poly (a) (->key -Pathlike (-> -Input-Port a) #:mode (Un (-val 'binary) (-val 'text)) #f a))
   (-poly
    (a)
    (->key
     -Pathlike
     (-> -Output-Port a)
     #:exists
     (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace 'can-update 'must-truncate)
     #f
     #:mode
     (one-of/c 'binary 'text)
     #f
     a))
   (-poly (a) (->key -Pathlike (-> a) #:mode (Un (-val 'binary) (-val 'text)) #f a))
   (-poly
    (a)
    (->key
     -Pathlike
     (-> a)
     #:exists
     (one-of/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace)
     #f
     #:mode
     (one-of/c 'binary 'text)
     #f
     a))
   (cl->*
    (->key #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -String))
    (->key -Input-Port #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -String)))
   (cl->*
    (->key #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -Bytes))
    (->key -Input-Port #:line-mode (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one) #f (-lst -Bytes)))
   (cl->* (->key (-lst Univ) #:separator Univ #f -Void) (->key (-lst Univ) -Output-Port #:separator Univ #f -Void))
   (->key -SomeSystemPathlike -SomeSystemPathlike #:more-than-root? Univ #f -SomeSystemPath)
   (let ((N -Integer)
         (?N (-opt -Integer))
         (-StrRx (Un -String -Regexp))
         (-BtsRx (Un -Bytes -Byte-Regexp))
         (-StrInput (Un -String -Path))
         (-BtsInput (Un -Input-Port -Bytes))
         (sel (λ (t) (-opt (-> (-lst t) t)))))
     (cl->*
      (->optkey -StrRx -StrInput (N ?N -Bytes)               
                #:match-select (sel -String) #f #:gap-select Univ #f
                (-lst -String))
      (->optkey -BtsRx (Un -StrInput -BtsInput) (N ?N -Bytes) 
                #:match-select (sel -Bytes) #f #:gap-select Univ #f
                (-lst -Bytes))
      (->optkey -Pattern -BtsInput (N ?N -Bytes)
                #:match-select (sel -Bytes) #f #:gap-select Univ #f
                (-lst -Bytes))))
   (let* ((?outp (-opt -Output-Port))
          (B -Boolean)
          (N -Integer)
          (?N (-opt -Integer))
          (ind-pair (-pair -Index -Index))
          (sel (-> (-lst (-opt ind-pair)) (-opt ind-pair)))
          (output (-opt (-pair ind-pair (-lst (-opt ind-pair)))))
          (-Input (Un -String -Input-Port -Bytes -Path)))
     (->optkey -Pattern -Input [N ?N -Bytes] #:match-select sel #f output))))

(define post 
  (list (-> (Un (-val #f) (one-of/c 'binary 'text)) -Boolean -Pathlike -String)
        (-> (Un (-val #f) (one-of/c 'binary 'text)) -Boolean -Pathlike -Bytes)
        (-> (Un (-val #f) (one-of/c 'binary 'text)) -Boolean -Pathlike Univ)
        (->
         (Un (-val #f) (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one))
         -Boolean
         (Un (-val #f) (one-of/c 'binary 'text))
         -Boolean
         -Pathlike
         (-lst -String))
        (->
         (Un (-val #f) (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one))
         -Boolean
         (Un (-val #f) (one-of/c 'binary 'text))
         -Boolean
         -Pathlike
         (-lst -Bytes))
        (->
         (Un (-val #f) (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace))
         -Boolean
         (Un (-val #f) (one-of/c 'binary 'text))
         -Boolean
         Univ
         -Pathlike
         -Void)
        (->
         (Un (-val #f) (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace))
         -Boolean
         (Un (-val #f) (one-of/c 'binary 'text))
         -Boolean
         (Un (-val #f) Univ)
         -Boolean
         (-lst Univ)
         -Pathlike
         -Void)
        (->
         (Un (-val #f) (one-of/c 'error 'append 'update 'replace 'truncate 'truncate/replace))
         -Boolean
         (Un (-val #f) (one-of/c 'binary 'text))
         -Boolean
         Univ
         -Pathlike
         -Void)
        (-poly
         (a)
         (cl->*
          (-> (Un (-val #f) (one-of/c 'binary 'text)) -Boolean -Pathlike (-> -Input-Port a) (-val #t) (-lst a))
          (-> (Un (-val #f) (one-of/c 'binary 'text)) -Boolean -Pathlike Univ -Boolean (-lst Univ))))
        (let ((use-lock-type Univ) (timeout-lock-there-type (-opt (-> -Path Univ))) (lock-there-type (-opt (-> -Path Univ))))
          (->
           (-opt lock-there-type)
           -Boolean
           (-opt timeout-lock-there-type)
           -Boolean
           (-opt use-lock-type)
           -Boolean
           -Symbol
           (-opt (-> Univ))
           (-opt Univ)
           (-opt (-opt -Pathlike))
           -Boolean
           -Boolean
           -Boolean
           Univ))
        (let ((lock-there-type (-opt (-> -Path Univ))) (max-delay-type -Real))
          (->
           (-opt lock-there-type)
           -Boolean
           (-opt max-delay-type)
           -Boolean
           -Real
           -Symbol
           (-opt (-> Univ))
           (-opt Univ)
           (-opt (-opt -Pathlike))
           -Boolean
           -Boolean
           -Boolean
           (-> -Pathlike Univ)))
        (-poly
         (a)
         (->
          (-opt -Real)
          -Boolean
          (-opt (-opt -Pathlike))
          -Boolean
          (-opt -Real)
          -Boolean
          (-opt -Pathlike)
          (one-of/c 'shared 'exclusive)
          (-> a)
          (-> a)
          a))
        (-poly
         (a b)
         (cl->*
          (-> -Boolean -Boolean Univ (-val #f) (-lst a) (-> a a -Boolean) (-lst a))
          (-> -Boolean -Boolean (-> a b) (-val #t) (-lst a) (-> b b -Boolean) (-lst a))))
        (-poly
         (a b)
         (cl->*
          (-> Univ (-val #f) (-lst a) (-val #f) -Boolean (-lst a))
          (-> Univ (-val #f) (-lst a) (-> a a Univ) -Boolean (-lst a))
          (-> (-> a b) (-val #t) (-lst a) (-opt (-> b b Univ)) -Boolean (-lst a))))
        (-> (-opt (one-of/c 'binary 'text)) -Boolean -Pathlike -Input-Port)
        (->
         (-opt (one-of/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace))
         -Boolean
         (-opt (one-of/c 'binary 'text))
         -Boolean
         -Pathlike
         -Output-Port)
        (->
         (-opt (one-of/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace))
         -Boolean
         (-opt (one-of/c 'binary 'text))
         -Boolean
         -Pathlike
         (-values (list -Input-Port -Output-Port)))
        (-poly (a) (-> (-opt (one-of/c 'binary 'text)) -Boolean -Pathlike (-> -Input-Port a) a))
        (-poly
         (a)
         (->
          (-opt (one-of/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace))
          -Boolean
          (-opt (one-of/c 'binary 'text))
          -Boolean
          -Pathlike
          (-> -Output-Port a)
          a))
        (-poly (a) (-> (-opt (one-of/c 'binary 'text)) -Boolean -Pathlike (-> -Input-Port a) a))
        (-poly
         (a)
         (->
          (-opt (one-of/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace))
          -Boolean
          (-opt (one-of/c 'binary 'text))
          -Boolean
          -Pathlike
          (-> -Output-Port a)
          a))
        (-poly (a) (-> (-opt (one-of/c 'binary 'text)) -Boolean -Pathlike (-> a) a))
        (-poly
         (a)
         (->
          (-opt (one-of/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace))
          -Boolean
          (-opt (one-of/c 'binary 'text))
          -Boolean
          -Pathlike
          (-> a)
          a))
        (-> (-opt (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)) -Boolean (-opt -Input-Port) -Boolean (-lst -String))
        (-> (-opt (one-of/c 'linefeed 'return 'return-linefeed 'any 'any-one)) -Boolean (-opt -Input-Port) -Boolean (-lst -Bytes))
        (-> (-opt Univ) -Boolean (-lst Univ) (-opt -Output-Port) -Boolean -Void)
        (-> Univ -Boolean -SomeSystemPathlike -SomeSystemPathlike -SomeSystemPath)
        (let ((N -Integer)
              (B -Boolean)
              (?N (-opt -Integer))
              (-StrRx (Un -String -Regexp))
              (-BtsRx (Un -Bytes -Byte-Regexp))
              (-StrInput (Un -String -Path))
              (sel (λ (t) (-opt (-> (-lst t) t))))
              (-BtsInput (Un -Input-Port -Bytes)))
          (cl->*
           (-> Univ B (sel -String) B -StrRx   -StrInput
               (-opt N) (-opt ?N) (-opt -Bytes) B B B (-lst -String))
           (-> Univ B (sel -Bytes)  B -BtsRx   (Un -StrInput -BtsInput)
               (-opt N) (-opt ?N) (-opt -Bytes) B B B (-lst -Bytes))
           (-> Univ B (sel -Bytes)  B -Pattern -BtsInput
               (-opt N) (-opt ?N) (-opt -Bytes) B B B (-lst -Bytes))))
        
        (let* ([?outp   (-opt -Output-Port)]
               [B       -Boolean]
               [N       -Integer]
               [?N      (-opt -Integer)]
               [ind-pair (-pair -Index -Index)]
               [output (-opt (-pair ind-pair (-lst (-opt ind-pair))))]
               (sel (-> (-lst (-opt ind-pair)) (-opt ind-pair)))
               [-Input (Un -String -Input-Port -Bytes -Path)])
          (-> (-opt sel) B -Pattern -Input (-opt N) (-opt ?N) (-opt -Bytes) B B B output))))
|#