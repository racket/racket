#lang scheme

(require "matcher.ss"
         "reduction-semantics.ss"
         "underscore-allowed.ss"
         "term.ss"
         "error.ss"
         "struct.ss"
         (for-syntax "rewrite-side-conditions.ss")
         (for-syntax "term-fn.ss")
         (for-syntax "reduction-semantics.ss")
         (for-syntax "keyword-macros.ss")
         mrlib/tex-table)

(define (exotic-choice? [random random]) (= 0 (random 5)))
(define (use-lang-literal? [random random]) (= 0 (random 20)))

(define default-check-attempts 1000)

(define ascii-chars-threshold 1000)
(define tex-chars-threshold 1500)
(define chinese-chars-threshold 2500)

(define (pick-var lang-lits attempt [random random])
  (let ([length (add1 (random-natural 4/5 random))])
    (string->symbol (random-string lang-lits length attempt random))))

(define (pick-char attempt [random random])
  (cond [(or (< attempt ascii-chars-threshold) (not (exotic-choice? random)))
         (let ([i (random (add1 (- (char->integer #\z) (char->integer #\a))))]
               [cap? (zero? (random 2))])
           (integer->char (+ i (char->integer (if cap? #\A #\a)))))]
        [(or (< attempt tex-chars-threshold) (not (exotic-choice? random)))
         (let ([i (random (- #x7E #x20 1))]
               [_ (- (char->integer #\_) #x20)])
           (integer->char (+ #x20 (if (= i _) (add1 i) i))))]
        [(or (< attempt chinese-chars-threshold) (not (exotic-choice? random)))
         (car (string->list (pick-from-list (map cadr tex-shortcut-table) random)))]
        [else
         (integer->char (+ #x4E00 (random (- #x9FCF #x4E00))))]))

(define (random-string lang-lits length attempt [random random])
  (if (and (not (null? lang-lits)) (use-lang-literal? random))
      (pick-from-list lang-lits random)
      (list->string (build-list length (λ (_) (pick-char attempt random))))))

(define (pick-any lang sexp [random random]) 
  (let ([c-lang (rg-lang-clang lang)]
        [c-sexp (rg-lang-clang sexp)])
    (if (and (not (null? (compiled-lang-lang c-lang))) (zero? (random 5)))
        (values lang (pick-from-list (map nt-name (compiled-lang-lang c-lang)) random))
        (values sexp (nt-name (car (compiled-lang-lang c-sexp)))))))

(define (pick-string lang-lits attempt [random random])
  (random-string lang-lits (random-natural 1/5 random) attempt random))

(define (pick-nts name cross? lang attempt)
  (nt-rhs (nt-by-name lang name cross?)))

(define (pick-from-list l [random random]) (list-ref l (random (length l))))

;; Chooses a random (exact) natural number from the "shifted" geometric distribution:
;;   P(random-natural = k) = p(1-p)^k
;;
;; P(random-natural >= k) = (1-p)^(k+1)
;; E(random-natural) = (1-p)/p
;; Var(random-natural) = (1-p)/p^2
(define (random-natural p [random random])
  (sub1 (inexact->exact (ceiling (real-part (/ (log (random)) (log (- 1 p))))))))

(define (negative? random)
  (zero? (random 2)))

(define (random-integer p [random random])
  (* (if (negative? random) -1 1) (random-natural p random)))

(define (random-rational p [random random])
  (/ (random-integer p random) (add1 (random-natural p random))))

(define (random-real p [random random])
  (* (random) 2 (random-integer p random)))

(define (random-complex p [random random])
  (let ([randoms (list random-integer random-rational random-real)])
    (make-rectangular ((pick-from-list randoms random) p random) 
                      ((pick-from-list randoms random) p random))))

(define integer-threshold 100)
(define rational-threshold 500)
(define real-threshold 1000)
(define complex-threshold 2000)

(define default-retries 100)
(define retry-threshold (max chinese-chars-threshold complex-threshold))
(define proportion-before-threshold 9/10)
(define proportion-at-size 1/10)
(define post-threshold-incr 50)

;; Determines the parameter p for which random-natural's expected value is E
(define (expected-value->p E)
  ;; E = 0 => p = 1, which breaks random-natural
  (/ 1 (+ (max 1 E) 1)))

; Determines a size measure for numbers, sequences, etc., using the
; attempt count.
(define (attempt->size n)
  (inexact->exact (floor (/ (log (add1 n)) (log 5)))))

(define (pick-number attempt #:top-threshold [top-threshold complex-threshold] [random random])
  (let loop ([threshold 0] 
             [generator random-natural]
             [levels `((,integer-threshold . ,random-integer)
                       (,rational-threshold . ,random-rational)
                       (,real-threshold . ,random-real)
                       (,complex-threshold . ,random-complex))])
    (if (or (null? levels)
            (< attempt (caar levels))
            (< top-threshold (caar levels))
            (not (exotic-choice? random)))
        (generator (expected-value->p (attempt->size (- attempt threshold))) random)
        (loop (caar levels) (cdar levels) (cdr levels)))))

(define (pick-natural attempt [random random])
  (pick-number attempt #:top-threshold 0 random))

(define (pick-integer attempt [random random])
  (pick-number attempt #:top-threshold integer-threshold random))

(define (pick-real attempt [random random])
  (pick-number attempt #:top-threshold real-threshold random))

(define (pick-sequence-length attempt)
  (random-natural (expected-value->p (attempt->size attempt))))

(define (zip . lists)
  (apply (curry map list) lists))

(define (min-prods nt base-table)
  (let* ([sizes (hash-ref base-table (nt-name nt))]
         [min-size (apply min/f sizes)])
    (map cadr (filter (λ (x) (equal? min-size (car x))) (zip sizes (nt-rhs nt))))))

(define-struct rg-lang (clang lits base-cases))
(define (prepare-lang lang)
  (let ([lits (map symbol->string (compiled-lang-literals lang))]
        [parsed (parse-language lang)])
    (make-rg-lang parsed lits (find-base-cases parsed))))

(define-struct (exn:fail:redex:generation-failure exn:fail:redex) ())
(define (raise-gen-fail who what attempts)
  (let ([str (format "~a: unable to generate ~a in ~a attempt~a" 
                     who what attempts (if (= attempts 1) "" "s"))])
    (raise (make-exn:fail:redex:generation-failure str (current-continuation-marks)))))

(define (generate lang decisions@ what)
  (define-values/invoke-unit decisions@
    (import) (export decisions^))
  
  (define ((generate-nt lang base-cases generate retries)
           name cross? size attempt in-hole env)
    (let*-values
        ([(term _)
          (generate/pred 
           name
           (λ (size attempt) 
             (let ([rhs (pick-from-list
                         (if (zero? size)
                             (min-prods (nt-by-name lang name cross?) 
                                        ((if cross? base-cases-cross base-cases-non-cross)
                                         base-cases))
                             ((next-non-terminal-decision) name cross? lang attempt)))])
               (generate (max 0 (sub1 size)) attempt empty-env in-hole (rhs-pattern rhs))))
           (λ (_ env) (mismatches-satisfied? env))
           size attempt retries)])
      term))
  
  (define (generate-sequence ellipsis generate env length)
    (define (split-environment env)
      (foldl (λ (var seq-envs)
               (let ([vals (hash-ref env var #f)])
                 (if vals
                     (map (λ (seq-env val) (hash-set seq-env var val)) seq-envs vals)
                     seq-envs)))
             (build-list length (λ (_) #hash())) (ellipsis-vars ellipsis)))
    (define (merge-environments seq-envs)
      (foldl (λ (var env)
               (hash-set env var (map (λ (seq-env) (hash-ref seq-env var)) seq-envs)))
             env (ellipsis-vars ellipsis)))
    (let-values
        ([(seq envs)
          (let recur ([envs (split-environment env)])
            (if (null? envs)
                (values null null)
                (let*-values 
                    ([(term env) (generate (car envs) the-hole (ellipsis-pattern ellipsis))]
                     [(terms envs) (recur (cdr envs))])
                  (values (cons term terms) (cons env envs)))))])
      (values seq (merge-environments envs))))
  
  (define (generate/pred name gen pred init-sz init-att retries)
    (let ([pre-threshold-incr 
           (ceiling
            (/ (- retry-threshold init-att)
               (* proportion-before-threshold (add1 retries))))]
          [incr-size? 
           (λ (remain)
             (zero? 
              (modulo (sub1 remain) 
                      (ceiling (* proportion-at-size 
                                  retries)))))])
      (let retry ([remaining (add1 retries)]
                  [size init-sz]
                  [attempt init-att])
        (if (zero? remaining)
            (raise-gen-fail what (format "pattern ~a" name) retries)
            (let-values ([(term env) (gen size attempt)])
              (if (pred term env)
                  (values term env)
                  (retry (sub1 remaining)
                         (if (incr-size? remaining) (add1 size) size)
                         (+ attempt
                            (if (>= attempt retry-threshold)
                                post-threshold-incr
                                pre-threshold-incr)))))))))
  
  (define (generate/prior name env generate)
    (let* ([none (gensym)]
           [prior (hash-ref env name none)])
      (if (eq? prior none)
          (let-values ([(term env) (generate)])
            (values term (hash-set env name term)))
          (values prior env))))
  
  (define (mismatches-satisfied? env)
    (let ([groups (make-hasheq)])
      (define (get-group group)
        (hash-ref groups group
                  (λ ()
                    (let ([vals (make-hash)])
                      (hash-set! groups group vals)
                      vals))))
      (for/and ([(name val) env])
        (or (not (mismatch? name))
            (let ([prior (get-group (mismatch-group name))])
              (and (not (hash-ref prior val #f))
                   (hash-set! prior val #t)))))))
  
  (define empty-env #hash())
  
  (define (bindings env)
    (make-bindings
     (for/fold ([bindings null]) ([(key val) env])
               (if (binder? key) 
                   (cons (make-bind (binder-name key) val) bindings)
                   bindings))))
  
  (define (generate-pat lang sexp retries size attempt env in-hole pat)
    (define recur (curry generate-pat lang sexp retries size attempt))
    (define recur/pat (recur env in-hole))
    (define ((recur/pat/size-attempt pat) size attempt)
      (generate-pat lang sexp retries size attempt env in-hole pat))
    
    (define clang (rg-lang-clang lang))
    (define gen-nt 
      (generate-nt 
       clang 
       (rg-lang-base-cases lang)
       (curry generate-pat lang sexp retries)
       retries))
    
    (match pat
      [`number (values ((next-number-decision) attempt) env)]
      [`natural (values ((next-natural-decision) attempt) env)]
      [`integer (values ((next-integer-decision) attempt) env)]
      [`real (values ((next-real-decision) attempt) env)]
      [`(variable-except ,vars ...)
       (generate/pred 'variable
                      (recur/pat/size-attempt 'variable)
                      (λ (var _) (not (memq var vars)))
                      size attempt retries)]
      [`variable 
       (values ((next-variable-decision) (rg-lang-lits lang) attempt)
               env)]
      [`variable-not-otherwise-mentioned
       (generate/pred 'variable
                      (recur/pat/size-attempt 'variable)
                      (λ (var _) (not (memq var (compiled-lang-literals clang))))
                      size attempt retries)]
      [`(variable-prefix ,prefix) 
       (define (symbol-append prefix suffix)
         (string->symbol (string-append (symbol->string prefix) (symbol->string suffix))))
       (let-values ([(term env) (recur/pat 'variable)])
         (values (symbol-append prefix term) env))]
      [`string 
       (values ((next-string-decision) (rg-lang-lits lang) attempt)
               env)]
      [`(side-condition ,pat ,(? procedure? condition) ,guard-src-loc)
       (generate/pred `(side-condition ,(unparse-pattern pat) ,guard-src-loc) 
                      (recur/pat/size-attempt pat)
                      (λ (_ env) (condition (bindings env)))
                      size attempt retries)]
      [`(name ,(? symbol? id) ,p)
       (let-values ([(term env) (recur/pat p)])
         (values term (hash-set env (make-binder id) term)))]
      [`hole (values in-hole env)]
      [`(in-hole ,context ,contractum)
       (let-values ([(term env) (recur/pat contractum)])
         (recur env term context))]
      [`(hide-hole ,pattern) (recur env the-hole pattern)]
      [`any
       (let*-values ([(new-lang nt) ((next-any-decision) lang sexp)]
                     [(term _) (generate-pat new-lang
                                             sexp
                                             retries
                                             size
                                             attempt
                                             empty-env
                                             the-hole
                                             nt)])
         (values term env))]
      [(? (is-nt? clang))
       (values (gen-nt pat #f size attempt in-hole env) env)]
      [(struct binder ((or (? (is-nt? clang) nt) 
                           (app (symbol-match named-nt-rx) (? (is-nt? clang) nt)))))
       (generate/prior pat env (λ () (recur/pat nt)))]
      [(struct binder ((or (? built-in? b) 
                           (app (symbol-match named-nt-rx) (? built-in? b)))))
       (generate/prior pat env (λ () (recur/pat b)))]
      [(struct mismatch (name (app (symbol-match mismatch-nt-rx)
                                   (? symbol? (? (is-nt? clang) nt)))))
       (let-values ([(term _) (recur/pat nt)])
         (values term (hash-set env pat term)))]
      [(struct mismatch (name (app (symbol-match mismatch-nt-rx)
                                   (? symbol? (? built-in? b)))))
       (let-values ([(term _) (recur/pat b)])
         (values term (hash-set env pat term)))]
      [`(cross ,(? symbol? cross-nt))
       (values (gen-nt cross-nt #t size attempt in-hole env) env)]
      [(or (? symbol?) (? number?) (? string?) (? boolean?) (? null?)) (values pat env)]
      [(list-rest (and (struct ellipsis (name sub-pat class vars)) ellipsis) rest)
       (let*-values ([(length) (let ([prior (hash-ref env class #f)])
                                 (if prior prior ((next-sequence-decision) attempt)))]
                     [(seq env) (generate-sequence ellipsis recur env length)]
                     [(rest env) (recur (hash-set (hash-set env class length) name length)
                                   in-hole rest)])
         (values (append seq rest) env))]
      [(list-rest pat rest)
       (let*-values 
           ([(pat-term env) (recur/pat pat)]
            [(rest-term env) (recur env in-hole rest)])
         (values (cons pat-term rest-term) env))]
      [else
       (error what "unknown pattern ~s\n" pat)]))
  
  (let ([rg-lang (prepare-lang lang)]
        [rg-sexp (prepare-lang sexp)])
    (λ (pat)
      (let ([parsed (reassign-classes (parse-pattern pat lang 'top-level))])
        (λ (size attempt retries)
          (let-values ([(term env)
                        (generate/pred 
                         pat
                         (λ (size attempt)
                           (generate-pat 
                            rg-lang 
                            rg-sexp
                            retries
                            size
                            attempt
                            empty-env
                            the-hole
                            parsed))
                         (λ (_ env) (mismatches-satisfied? env))
                         size attempt retries)])
            (values term (bindings env))))))))

(define-struct base-cases (cross non-cross))

;; find-base-cases : (list/c nt) -> base-cases
(define (find-base-cases lang)
  (define nt-table (make-hash))
  (define changed? #f)
  (define (nt-get nt) (hash-ref nt-table nt 'inf))
  (define (nt-set nt new) 
    (let ([old (nt-get nt)])
      (unless (equal? new old)
        (set! changed? #t)
        (hash-set! nt-table nt new))))
  
  (define ((process-nt cross?) nt)
    (nt-set (cons cross? (nt-name nt)) (apply min/f (map process-rhs (nt-rhs nt)))))
  
  (define (process-rhs rhs)
    (let ([nts (rhs->nts (rhs-pattern rhs))])
      (if (null? nts) 
          0
          (add1/f (apply max/f (map nt-get nts))))))
  
  ;; rhs->path : pattern -> (listof (cons/c boolean symbol))
  ;; determines all of the non-terminals in a pattern
  (define (rhs->nts pat)
    (let ([nts '()])
      (let loop ([pat pat])
        (match pat
          [(? symbol? pat)
           (when ((is-nt? lang) (symbol->nt pat))
             (set! nts (cons (cons #f (symbol->nt pat)) nts)))]
          [`(cross ,(? symbol? x-nt))
           (set! nts (cons (cons #t x-nt) nts))]
          [`(variable-except ,s ...) (void)]
          [`(variable-prefix ,p) (void)]
          [`() (void)]
          [(struct ellipsis (_ p _ _))
           (loop p)]
          [`(,a . ,b)
           (loop a)
           (loop b)]
          [_ (void)]))
      nts))

  ;; build-table : (listof nt) -> hash
  (define (build-table nts)
    (let ([tbl (make-hasheq)])
      (for-each
       (λ (nt) (hash-set! tbl (nt-name nt) (map process-rhs (nt-rhs nt))))
       nts)
      tbl))
  
  (let loop ()
    (set! changed? #f)
    (for-each (process-nt #f) (compiled-lang-lang lang))
    (for-each (process-nt #t) (compiled-lang-cclang lang))
    (when changed?
      (loop)))
  
  (make-base-cases
   (build-table (compiled-lang-cclang lang))
   (build-table (compiled-lang-lang lang))))

(define min/f
  (case-lambda
    [(a) a]
    [(a b) 
     (cond
       [(eq? a 'inf) b]
       [(eq? b 'inf) a]
       [else (min a b)])]
    [(a b . c) (min/f a (apply min/f b c))]))
(define max/f
  (case-lambda
    [(a) a]
    [(a b) 
     (cond
       [(eq? a 'inf) a]
       [(eq? b 'inf) b]
       [else (max a b)])]
    [(a b . c) (max/f a (apply max/f b c))]))
(define (add1/f a) (if (eq? a 'inf) 'inf (+ a 1)))

;; is-nt? : compiled-lang any -> boolean
(define ((is-nt? lang) x)
  (and (hash-ref (compiled-lang-ht lang) x #f) #t))

;; built-in? : any -> boolean
(define (built-in? x)
  (and (memq x underscore-allowed) #t))

;; nt-by-name : lang symbol boolean -> nt
(define (nt-by-name lang name cross?)
  (findf (λ (nt) (eq? name (nt-name nt))) 
         (if cross? 
             (compiled-lang-cclang lang)
             (compiled-lang-lang lang))))

(define named-nt-rx #rx"^([^_]+)_[^_]*$")
(define mismatch-nt-rx #rx"([^_]+)_!_[^_]*$")
(define named-ellipsis-rx #rx"^\\.\\.\\._[^_]*$")
(define mismatch-ellipsis-rx #rx"^\\.\\.\\._!_[^_]*$")

;; symbol-match : regexp -> any -> (or/c false symbol)
;; Returns the sub-symbol matching the sub-pattern inside
;; the first capturing parens.
(define ((symbol-match rx) x)
  (and (symbol? x) 
       (let ([match (regexp-match rx (symbol->string x))])
         (and match (cadr match) (string->symbol (cadr match))))))

(define-struct class (id) #:inspector (make-inspector))
(define-struct mismatch (id group) #:inspector (make-inspector))
(define-struct binder (name) #:inspector (make-inspector))

;; name: (or/c symbol? mismatch?)
;;   The generator records `name' in the environment when generating an ellipsis,
;;   to enforce sequence length constraints.
;; class: class?
;;   When one binding appears under two (non-nested) ellipses, the sequences generated
;;   must have the same length; `class' groups ellipses to reflect this constraint.
;; var: (list/c (or/c symbol? class? mismatch? binder?))
;;   the bindings within an ellipses, used to split and merge the environment before
;;   and after generating an ellipsis
(define-struct ellipsis (name pattern class vars) #:inspector (make-inspector))

;; parse-pattern : pattern compiled-lang (or/c 'cross 'top-level 'grammar) -> parsed-pattern
;; Turns "pat ...", "pat ..._id", and "pat ..._!_id" into ellipsis structs,
;; "nt_!_id" into mismatch structs, "nt_id" into binder structs, and
;; "nt/underscore-allowed" in top-level patterns into binder structs.
(define (parse-pattern pattern lang mode)
  (define (recur pat vars)
    (match pat
      [(or (app (symbol-match named-nt-rx) (or (? (is-nt? lang)) (? built-in?)))
           (and (? (λ (_) (eq? mode 'top-level))) (or (? (is-nt? lang)) (? built-in?))))
       (let ([b (make-binder pat)])
         (values b (cons b vars)))]
      [(app (symbol-match mismatch-nt-rx) (or (? (is-nt? lang)) (? built-in?)))
       (let ([mismatch (make-mismatch (gensym) pat)])
         (values mismatch (cons mismatch vars)))]
      [`(name ,name ,sub-pat)
       (let-values ([(parsed vars) (recur sub-pat vars)])
         (values `(name ,name ,parsed) (cons (make-binder name) vars)))]
      [(list-rest sub-pat (and (? symbol?) (app symbol->string (regexp named-ellipsis-rx)) name) rest)
       (let*-values ([(sub-pat-parsed sub-pat-vars) (recur sub-pat null)]
                     [(seq) (make-ellipsis name sub-pat-parsed (make-class name) sub-pat-vars)]
                     [(vars) (append (list* name (make-class name) sub-pat-vars) vars)]
                     [(rest-parsed vars) (recur rest vars)])
         (values (cons seq rest-parsed) vars))]
      [(list-rest sub-pat '... rest)
       (let*-values ([(sub-pat-parsed sub-pat-vars) (recur sub-pat null)]
                     [(class) (make-class (gensym))]
                     [(seq) (make-ellipsis '... sub-pat-parsed class sub-pat-vars)]
                     [(rest-parsed vars) (recur rest (cons class (append sub-pat-vars vars)))])
         (values (cons seq rest-parsed) vars))]
      [(list-rest sub-pat (and (? symbol?) (app symbol->string (regexp mismatch-ellipsis-rx)) name) rest)
       (let*-values ([(sub-pat-parsed sub-pat-vars) (recur sub-pat null)]
                     [(mismatch) (make-mismatch (gensym) name)]
                     [(class) (make-class (gensym))]
                     [(seq) (make-ellipsis mismatch sub-pat-parsed class sub-pat-vars)]
                     [(vars) (append (list* class mismatch sub-pat-vars) vars)]
                     [(rest-parsed vars) (recur rest vars)])
         (values (cons seq rest-parsed) vars))]
      [(and (? (λ (_) (not (eq? mode 'cross)))) `(cross ,(and (? (is-nt? lang)) nt)))
       (let ([nt-str (symbol->string nt)])
         (values `(cross ,(string->symbol (string-append nt-str "-" nt-str))) vars))]
      [`(side-condition ,pat ,guard ,guard-src-loc)
       (let-values ([(parsed vars) (recur pat vars)])
         (values `(side-condition ,parsed ,guard ,guard-src-loc) vars))]
      [(cons first rest)
       (let-values ([(first-parsed vars) (recur first vars)])
         (let-values ([(rest-parsed vars) (recur rest vars)])
           (values (cons first-parsed rest-parsed) vars)))]
      [_ (values pat vars)]))
  (let-values ([(parsed _) (recur pattern null)])
    parsed))

;; parse-language: compiled-lang -> compiled-lang
(define (parse-language lang)
  (define ((parse-nt mode) nt)
    (make-nt (nt-name nt) (map (parse-rhs mode) (nt-rhs nt))))
  (define ((parse-rhs mode) rhs)
    (make-rhs (reassign-classes (parse-pattern (rhs-pattern rhs) lang mode))))
  
  (struct-copy 
   compiled-lang lang
   [lang (map (parse-nt 'grammar) (compiled-lang-lang lang))]
   [cclang (map (parse-nt 'cross) (compiled-lang-cclang lang))]))

;; unparse-pattern: parsed-pattern -> pattern
(define unparse-pattern
  (match-lambda
    [(struct binder (name)) name]
    [(struct mismatch (_ group)) group]
    [(list-rest (struct ellipsis (name sub-pat _ _)) rest)
     (let ([ellipsis (if (mismatch? name) (mismatch-group name) name)])
       (list* (unparse-pattern sub-pat) ellipsis (unparse-pattern rest)))]
    [(cons first rest) 
     (cons (unparse-pattern first) (unparse-pattern rest))]
    [else else]))

;; class-reassignments : parsed-pattern -> hash[sym -o> sym]
(define (class-reassignments pattern)
  ; union-find w/o balancing or path compression (at least for now)
  (define (union e f sets)
    (hash-set sets (find f sets) (find e sets)))
  (define (find e sets)
    (let recur ([chd e] [par (hash-ref sets e #f)])
      (if (and par (not (eq? chd par))) (recur par (hash-ref sets par #f)) chd)))
  
  (let* ([last-contexts (make-hasheq)]
         [record-binder
          (λ (pat under assignments)
            (if (null? under) 
                assignments
                (let ([last (hash-ref last-contexts pat #f)])
                  (if last
                      (foldl (λ (cur last asgns) (union cur last asgns)) assignments under last)
                      (begin
                        (hash-set! last-contexts pat under)
                        assignments)))))]
         [assignments
          (let recur ([pat pattern] [under null] [assignments #hasheq()])
            (match pat
              ;; `(name ,id ,sub-pat) not considered, since bindings introduced
              ;; by name must be unique.
              [(struct binder (name))
               (record-binder name under assignments)]
              [(struct ellipsis (name sub-pat (struct class (cls)) _))
               (recur sub-pat (cons cls under)
                 (if (and (symbol? name) (regexp-match named-ellipsis-rx (symbol->string name)))
                     (record-binder name under assignments)
                     assignments))]
              [(? list?)
               (foldl (λ (pat asgns) (recur pat under asgns)) assignments pat)]
              [_ assignments]))])
    (make-immutable-hasheq (hash-map assignments (λ (cls _) (cons cls (find cls assignments)))))))

(define (reassign-classes pattern)
  (let* ([reassignments (class-reassignments pattern)]
         [rewrite (λ (c) (make-class (hash-ref reassignments (class-id c) (class-id c))))])
    (let recur ([pat pattern])
      (match pat
        [(struct ellipsis (name sub-pat class vars))
         (make-ellipsis name (recur sub-pat) (rewrite class)
                        (map (λ (v) (if (class? v) (rewrite v) v)) vars))]
        [(? list?) (map recur pat)]
        [_ pat]))))

;; used in generating the `any' pattern
(define-language sexp (sexp variable string number hole (sexp ...)))

(define-for-syntax (metafunc name)
  (let ([tf (syntax-local-value name (λ () #f))])
    (and (term-fn? tf) (term-fn-get-id tf))))

(define-for-syntax (metafunc/err name stx)
  (let ([m (metafunc name)])
    (if m m (raise-syntax-error #f "not a metafunction" stx name))))

(define (assert-nat name x)
  (if (and (integer? x) (>= x 0))
      x
      (raise-type-error name "natural number" x)))
(define (assert-rel name x)
  (if (reduction-relation? x)
      x
      (raise-type-error 'redex-check "reduction-relation" x)))

(define-for-syntax (term-generator lang pat decisions@ what)
  (with-syntax ([pattern 
                 (rewrite-side-conditions/check-errs 
                  (language-id-nts lang what)
                  what #t pat)])
    #`((generate #,lang #,decisions@ '#,what) `pattern)))

(define-syntax (generate-term stx)
  (syntax-case stx ()
    [(name lang pat size . kw-args)
     (with-syntax ([(attempt retries)
                    (parse-kw-args `((#:attempt-num . 1)
                                     (#:retries . ,#'default-retries))
                                   (syntax kw-args)
                                   stx)])
       (syntax/loc stx
         ((generate-term lang pat) size #:attempt-num attempt #:retries retries)))]
    [(name lang pat)
     (with-syntax ([make-gen (term-generator #'lang
                                             #'pat
                                             #'(generation-decisions)
                                             (syntax-e #'name))])
       (syntax/loc stx
         (let ([generate make-gen])
           (λ (size #:attempt-num [attempt-num 1] #:retries [retries default-retries])
             (let ([att (assert-nat 'name attempt-num)]
                   [ret (assert-nat 'name retries)])
               (let-values ([(term _) (generate size att ret)])
                 term))))))]))

(define-for-syntax (show-message stx)
  (syntax-case stx ()
    [(what . _)
     (identifier? #'what)
     (with-syntax ([loc (if (and (path? (syntax-source stx))
                                 (syntax-line stx))
                            (format "~a:~a"
                                    (path->string (syntax-source stx)) 
                                    (syntax-line stx))
                            #f)])
       #`(λ (msg)
           (fprintf 
            (current-output-port)
            "~a: ~a~a"
            'what (if loc (string-append loc "\n") "") msg)))]))

(define-syntax (redex-check stx)
  (syntax-case stx ()
    [(_ lang pat property . kw-args)
     (let-values ([(names names/ellipses) 
                   (extract-names (language-id-nts #'lang 'redex-check)
                                  'redex-check #t #'pat)]
                  [(attempts-stx source-stx retries-stx)
                   (apply values
                          (parse-kw-args `((#:attempts . ,#'default-check-attempts)
                                           (#:source . #f)
                                           (#:retries . ,#'default-retries))
                                         (syntax kw-args)
                                         stx))])
       (with-syntax ([(name ...) names]
                     [(name/ellipses ...) names/ellipses]
                     [show (show-message stx)])
         (with-syntax ([property (syntax
                                  (λ (_ bindings)
                                    (term-let ([name/ellipses (lookup-binding bindings 'name)] ...)
                                              property)))])
           (quasisyntax/loc stx
             (let ([att (assert-nat 'redex-check #,attempts-stx)]
                   [ret (assert-nat 'redex-check #,retries-stx)])
               (unsyntax
                (if source-stx
                    #`(let-values ([(metafunc/red-rel num-cases) 
                                    #,(cond [(and (identifier? source-stx) (metafunc source-stx))
                                             => (λ (x) #`(values #,x (length (metafunc-proc-cases #,x))))]
                                            [else
                                             #`(let ([r (assert-rel 'redex-check #,source-stx)])
                                                 (values r (length (reduction-relation-make-procs r))))])])
                        (check-lhs-pats
                         lang
                         metafunc/red-rel
                         property
                         random-decisions@
                         (max 1 (floor (/ att num-cases)))
                         ret
                         'redex-check
                         show
                         (test-match lang pat)
                         (λ (generated) (redex-error 'redex-check "~s does not match ~s" generated 'pat))))
                    #`(check-prop
                       #,(term-generator #'lang #'pat #'random-decisions@ 'redex-check)
                       property att ret show)))
               (void))))))]))

(define (format-attempts a)
  (format "~a attempt~a" a (if (= 1 a) "" "s")))

(define (check-prop generator property attempts retries show)
  (when (check generator property attempts retries show)
    (show (format "no counterexamples in ~a\n"
                  (format-attempts attempts)))))

(define (check generator property attempts retries show
               #:source [source #f]
               #:match [match #f]
               #:match-fail [match-fail #f])
  (let loop ([remaining attempts])
    (if (zero? remaining)
        #t
        (let ([attempt (add1 (- attempts remaining))])
          (let-values ([(term bindings) (generator (attempt->size attempt) attempt retries)])
            (if (andmap (λ (bindings) 
                          (with-handlers 
                              ([exn:fail?
                                (λ (exn) 
                                  (show 
                                   (format "checking ~s raises an exception\n" term))
                                  (raise exn))])
                            (property term bindings)))
                        (cond [(and match match-fail (match term)) 
                               => (curry map (compose make-bindings match-bindings))]
                              [match (match-fail term)]
                              [else (list bindings)]))
                (loop (sub1 remaining))
                (begin
                  (show
                   (format "counterexample found after ~a~a:\n"
                           (format-attempts attempt)
                           (if source (format " with ~a" source) "")))
                  (pretty-print term (current-output-port))
                  #f)))))))

(define-syntax (check-metafunction-contract stx)
  (syntax-case stx ()
    [(_ name . kw-args)
     (identifier? #'name)
     (with-syntax ([m (metafunc/err #'name stx)]
                   [(attempts retries)
                    (parse-kw-args `((#:attempts . ,#'default-check-attempts)
                                     (#:retries . ,#'default-retries))
                                   (syntax kw-args)
                                   stx)]
                   [show (show-message stx)])
       (syntax/loc stx 
        (let ([lang (metafunc-proc-lang m)]
              [dom (metafunc-proc-dom-pat m)]
              [decisions@ (generation-decisions)]
              [att (assert-nat 'check-metafunction-contract attempts)])
          (check-prop 
           ((generate lang decisions@ 'check-metafunction-contract)
            (if dom dom '(any (... ...))))
           (λ (t _) 
             (with-handlers ([exn:fail:redex? (λ (_) #f)])
               (begin (term (name ,@t)) #t)))
           att
           retries
           show))))]))

(define (check-lhs-pats lang mf/rr prop decisions@ attempts retries what show
                         [match #f]
                         [match-fail #f])
  (let ([lang-gen (generate lang decisions@ what)])
    (let-values ([(pats srcs)
                  (cond [(metafunc-proc? mf/rr)
                         (values (map metafunc-case-lhs-pat (metafunc-proc-cases mf/rr))
                                 (metafunction-srcs mf/rr))]
                        [(reduction-relation? mf/rr)
                         (values (map (λ (rwp) ((rewrite-proc-lhs rwp) lang)) (reduction-relation-make-procs mf/rr))
                                 (reduction-relation-srcs mf/rr))])])
      (when (for/and ([pat pats] [src srcs])
              (with-handlers ([exn:fail:redex:generation-failure?
                               ; Produce an error message that blames the LHS as a whole.
                               (λ (_)
                                 (raise-gen-fail what (format "LHS of ~a" src) retries))])
                (check
                 (lang-gen pat)
                 prop
                 attempts
                 retries
                 show
                 #:source src
                 #:match match
                 #:match-fail match-fail)))
        (show
         (format "no counterexamples in ~a (with each clause)\n"
                 (format-attempts attempts)))))))

(define-syntax (check-metafunction stx)
  (syntax-case stx ()
    [(_ name property . kw-args)
     (with-syntax ([m (metafunc/err #'name stx)]
                   [(attempts retries)
                    (parse-kw-args `((#:attempts . , #'default-check-attempts)
                                     (#:retries . ,#'default-retries))
                                   (syntax kw-args)
                                   stx)]
                   [show (show-message stx)])
       (syntax/loc stx
         (let ([att (assert-nat 'check-metafunction attempts)]
               [ret (assert-nat 'check-metafunction retries)])
           (check-lhs-pats 
            (metafunc-proc-lang m)
            m
            (λ (term _) (property term))
            (generation-decisions)
            att
            ret
            'check-metafunction
            show))))]))

(define (reduction-relation-srcs r)
  (map (λ (proc) (or (rewrite-proc-name proc)
                     (format "clause at ~a" (rewrite-proc-lhs-src proc))))
       (reduction-relation-make-procs r)))

(define (metafunction-srcs m)
  (map (compose (curry format "clause at ~a") metafunc-case-src-loc)
       (metafunc-proc-cases m)))

(define-syntax (check-reduction-relation stx)
  (syntax-case stx ()
    [(_ relation property . kw-args)
     (with-syntax ([(attempts retries decisions@)
                    (parse-kw-args `((#:attempts . , #'default-check-attempts)
                                     (#:retries . ,#'default-retries)
                                     (#:decisions . ,#'random-decisions@))
                                   (syntax kw-args)
                                   stx)]
                   [show (show-message stx)])
       (syntax/loc stx
         (let ([att attempts]
               [ret (assert-nat 'check-reduction-relation retries)]
               [rel (assert-rel 'check-reduction-relation relation)])
           (check-lhs-pats
            (reduction-relation-lang rel)
            rel
            (λ (term _) (property term))
            decisions@
            attempts
            retries
            'check-reduction-relation
            show))))]))

(define-signature decisions^
  (next-variable-decision
   next-number-decision
   next-natural-decision
   next-integer-decision
   next-real-decision
   next-non-terminal-decision
   next-sequence-decision
   next-any-decision
   next-string-decision))

(define random-decisions@
  (unit (import) (export decisions^)
        (define (next-variable-decision) pick-var)
        (define (next-number-decision) pick-number)
        (define (next-natural-decision) pick-natural)
        (define (next-integer-decision) pick-integer)
        (define (next-real-decision) pick-real)
        (define (next-non-terminal-decision) pick-nts)
        (define (next-sequence-decision) pick-sequence-length)
        (define (next-any-decision) pick-any)
        (define (next-string-decision) pick-string)))

(define generation-decisions (make-parameter random-decisions@))

(provide redex-check
         generate-term
         check-metafunction-contract
         check-reduction-relation
         check-metafunction
         exn:fail:redex:generation-failure?)

(provide (struct-out ellipsis) 
         (struct-out mismatch)
         (struct-out class)
         (struct-out binder)
         (struct-out base-cases))

(provide pick-from-list pick-sequence-length pick-nts
         pick-char pick-var pick-string pick-any
         pick-number pick-natural pick-integer pick-real
         parse-pattern unparse-pattern
         parse-language prepare-lang
         class-reassignments reassign-classes
         default-retries proportion-at-size
         retry-threshold proportion-before-threshold post-threshold-incr
         is-nt? nt-by-name min-prods
         generation-decisions decisions^ 
         random-string
         sexp find-base-cases)
