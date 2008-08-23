#|

iteratively grow the set of numbers & variables during generation.

redex: disallow non-terminals on rhs of rules unless they are actually bound(?)

need support for: 
 - collecting statistics
 - simplifying test cases

To do a better job of not generating programs with free variables, 
  keep track of which forms introduce binders 
  and prefer to generate that before generating any variables
  (also get rid of kludge, as below)

|#

#lang scheme

(require "matcher.ss"
         "reduction-semantics.ss"
         "underscore-allowed.ss"
         "term.ss"
         (for-syntax "rewrite-side-conditions.ss")
         mrlib/tex-table)

(define random-numbers '(0 1 -1 17 8))
(define (allow-free-var? [random random]) (= 0 (random 30)))
(define (exotic-char? [random random]) (= 0 (random 10)))
(define (use-lang-literal? [random random]) (= 0 (random 20)))
(define (try-to-introduce-binder?) (= 0 (random 2)) #f)

(define (hash->keys hash) (hash-map hash (λ (k v) k)))

(define (lang-literals lang)
  (define (process-pattern pat lits)
    (cond [(symbol? pat) (process-pattern (symbol->string pat) lits)]
          [(string? pat) (hash-set lits pat (void))]
          [(number? pat) (process-pattern (number->string pat) lits)]
          [(or (procedure? pat) (boolean? pat) (null? pat)) lits]
          [(pair? pat) (foldl process-pattern lits pat)]
          [else (error 'lang-literals "unexpected pattern ~s" pat)]))
  (define (process-non-terminal nt chars)
    (foldl (λ (rhs chars) (process-pattern (rhs-pattern rhs) chars)) 
           chars (nt-rhs nt)))
  (hash->keys 
   (foldl process-non-terminal 
          (make-immutable-hash null) (compiled-lang-lang lang))))

(define (unique-chars strings)
  (define (record-chars char chars)
    (if (char=? char #\_) chars (hash-set chars char (void))))
  (hash->keys
   (foldl (λ (s c) (foldl record-chars c (string->list s)))
          (make-immutable-hash null) strings)))

(define generation-retries 100)
(define ascii-chars-threshold 50)
(define tex-chars-threshold 500)
(define chinese-chars-threshold 2000)

;; E(pick-length) = 4/5(1 + E(pick-length)) = 4
;; P(pick-length >= 50) = 4/5^50 ≈ 0.00143%
(define (pick-length [random random]) 
  (cond
    [(zero? (random 5)) 0]
    [else (+ 1 (pick-length random))]))

;; pick-length averages about 4, has a max of about 50 and likes the small numbers:
#;
(let ([l (build-list 100000 (λ (x) (pick-length)))])
  (values (/ (apply + l) (length l))
          (apply max l)
          (let ([ht (make-hash)])
            (for-each
             (λ (n) (hash-set! ht n (+ 1  (hash-ref ht n 0))))
             l)
            (sort (hash-map ht (λ (x y) (list x (/ y (length l) 1.0))))
                  (λ (x y) (> (cadr x) (cadr y)))))))

(define (pick-var lang-chars lang-lits bound-vars attempt [random random])
  ;; E(length) = 4/5 + 1/5(1 + E(length)) = 5/4
  ;; P(length=c) = 4/(5^c)
  (define (length) (if (not (zero? (random 5))) 1 (add1 (length))))
  (if (or (null? bound-vars) (allow-free-var? random))
      (string->symbol (random-string lang-chars lang-lits (length) attempt random))
      (pick-from-list bound-vars random)))

(define (pick-char attempt lang-chars [random random])
  (if (and (not (null? lang-chars)) 
           (or (< attempt ascii-chars-threshold)
               (not (exotic-char? random))))
      (pick-from-list lang-chars random)
      (if (or (< attempt tex-chars-threshold) (not (exotic-char? random)))
          (let ([i (random (- #x7E #x20 1))]
                [_ (- (char->integer #\_) #x20)])
            (integer->char (+ #x20 (if (= i _) (add1 i) i))))
          (if (or (< attempt chinese-chars-threshold) (not (exotic-char? random)))
              (car (string->list (pick-from-list (map cadr tex-shortcut-table) random)))
              (integer->char (+ #x4E00 (random (- #x9FCF #x4E00))))))))

(define (random-string lang-chars lang-lits length attempt [random random])
  (if (and (not (null? lang-lits)) (use-lang-literal? random))
      (pick-from-list lang-lits random)
      (list->string (build-list length (λ (_) (pick-char attempt lang-chars random))))))

(define (pick-any lang [random random]) 
  (if (zero? (random 5))
      (values lang (pick-from-list (map nt-name (compiled-lang-lang lang)) random))
      (values sexp (nt-name (car (compiled-lang-lang sexp))))))

(define (pick-string lang-chars lang-lits attempt [random random])
  (random-string lang-chars lang-lits (pick-length random) attempt random))

(define (pick-nt prods bound-vars size)
  (let* ([binders (filter (λ (x) (not (null? (rhs-var-info x)))) prods)]
         [do-intro-binder? (and (not (zero? size)) (null? bound-vars)
                                (not (null? binders)) (try-to-introduce-binder?))])
    (pick-from-list (if do-intro-binder? binders prods))))

(define (pick-from-list l [random random]) (list-ref l (random (length l))))

(define (min-prods nt base-table)
  (let* ([sizes (hash-ref base-table (nt-name nt))]
         [min-size (apply min/f sizes)]
         [zip (λ (l m) (map cons l m))])
    (map cdr (filter (λ (x) (equal? min-size (car x))) (zip sizes (nt-rhs nt))))))

(define (generation-failure pat)
  (error 'generate "unable to generate pattern ~s in ~s attempts" 
         pat generation-retries))

(define ((disjunction . preds) x)
  (if (null? preds)
      #f
      (or ((car preds) x) ((apply disjunction (cdr preds)) x))))

;; used in generating the `any' pattern
(define-language sexp (sexp variable string number hole (sexp ...)))

(define (generate* lang pat size attempt [decisions@ random-decisions@])
  (define-values/invoke-unit decisions@
    (import) (export decisions^))
  
  (define lang-lits (lang-literals lang))
  (define lang-chars (unique-chars lang-lits))
  (define base-table (find-base-cases lang))
  
  (define (generate-nt nt fvt-id bound-vars size in-hole initial-state)
    (let loop ([nts (compiled-lang-lang lang)])
      (cond
        [(null? nts) (error 'generate-nt "didn't find non-terminal ~s" nt)]
        [(eq? (nt-name (car nts)) nt)
         (let*-values 
             ([(rhs) 
               ((next-non-terminal-decision) 
                (if (zero? size) (min-prods (car nts) base-table) (nt-rhs (car nts)))
                bound-vars size)]
              [(term post-nt-state) 
               (((generate-pat 
                  (append (extract-bound-vars fvt-id initial-state) bound-vars)
                  (max 0 (sub1 size)))
                 (rhs-pattern rhs) in-hole) 
                (make-gen-state
                 (make-state (map fvt-entry (rhs-var-info rhs)) #hasheq() #hasheq())
                 (if in-hole initial-state #f)))]
              [(new-state) (if in-hole (gen-state-hole post-nt-state) initial-state)])
           (values term (extend-found-vars fvt-id term new-state)))]
        [else (loop (cdr nts))])))
  
  (define-struct gen-state (current hole))
  (define-struct state (fvt matches mismatches))
  (define (set-current-matches state id term)
    (make-gen-state
     (make-state
      (state-fvt (gen-state-current state))
      (hash-set (state-matches (gen-state-current state)) id term)
      (state-mismatches (gen-state-current state)))
     (gen-state-hole state)))
  (define (set-current-mismatches state id term)
    (make-gen-state
     (make-state
      (state-fvt (gen-state-current state))
      (state-matches (gen-state-current state))
      (hash-set (state-mismatches (gen-state-current state)) id term))
     (gen-state-hole state)))
  
  (define-struct found-vars (nt source bound-vars found-nt?))
  (define (fvt-entry binds)
    (make-found-vars (binds-binds binds) (binds-source binds) '() #f))
  
  (define (((generate-pat bound-vars size) pat in-hole [fvt-id pat]) state)
    (define recur (generate-pat bound-vars size))
    (define (recur/pat pat) ((recur pat in-hole) state))
    (define (generate/pred pred pat [gen (λ () (recur/pat pat))])
      (let retry ([remaining generation-retries])
        (if (zero? remaining)
            (generation-failure pat)
            (let-values ([(term state) (gen)])
              (if (pred term (state-matches (gen-state-current state)))
                  (values term state)
                  (retry (sub1 remaining)))))))
    (match pat
      [`number (values ((next-number-decision) random-numbers) state)]
      [`(variable-except ,vars ...)
       (generate/pred (λ (var _) (not (memq var vars))) 'variable)]
      [`variable (values ((next-variable-decision) lang-chars lang-lits bound-vars attempt) state)]
      [`variable-not-otherwise-mentioned
       (generate/pred (λ (var _) (not (memq var (compiled-lang-literals lang)))) 'variable)]
      [`(variable-prefix ,prefix) 
       (define (symbol-append prefix suffix)
         (string->symbol (string-append (symbol->string prefix) (symbol->string suffix))))
       (let-values ([(term state) (recur/pat 'variable)])
         (values (symbol-append prefix term) state))]
      [`string (values ((next-string-decision) lang-chars lang-lits attempt) state)]
      [`(side-condition ,pat ,(? procedure? condition))
       ;; `matches' includes bindings beyond those bound in `pat',
       ;; but compiled side-conditions ignore these.
       (generate/pred (λ (_ matches) (condition (make-bindings (hash-map matches make-bind)))) pat)]
      [`(name ,(? symbol? id) ,p)
       (let-values ([(term state) (recur/pat p)])
         (values term (set-current-matches state id term)))]
      [`hole
       (cond [(not in-hole) (values the-hole state)]
             [(gen-state-hole state)
              (let-values ([(term hole-state) (in-hole (gen-state-hole state))])
                (values term (make-gen-state (gen-state-current state) hole-state)))]
             [else (in-hole state)])]
      [`(in-hole ,context ,contractum)
       ((recur context (recur contractum in-hole)) state)]
      [`(hide-hole ,pattern) ((recur pattern #f) state)]
      [`any
       (let-values ([(lang nt) ((next-any-decision) lang)])
         (values (generate* lang nt size attempt decisions@) state))]
      [(? (λ (p) (is-nt? lang p)))
       (generate-nt pat fvt-id bound-vars size in-hole state)]
      [(and (? symbol?) (app symbol->string (regexp named-nt-rx (list _ nt))))
       (let* ([undecorated (string->symbol nt)]
              [none (gensym)]
              [prior (hash-ref (state-matches (gen-state-current state)) pat none)])
         (if (eq? prior none)
             (let-values 
                 ([(term state) ((recur undecorated in-hole pat) state)])
               (values term (set-current-matches state pat term)))
             (values prior state)))]
      [(and (? symbol?) (app symbol->string (regexp mismatch-nt-rx (list _ nt))))
       (let*-values 
           ([(undecorated) (string->symbol nt)]
            [(prior) (hash-ref (state-mismatches (gen-state-current state)) pat null)]
            [(generate-mismatch)
             (λ () ((recur undecorated in-hole pat) state))]
            [(term state) 
             (generate/pred (λ (t _) (not (member t prior))) undecorated generate-mismatch)])
         (values term (set-current-mismatches state pat (cons term prior))))]
      [(? (disjunction symbol? number? string? boolean? null?)) (values pat state)]
      [(list-rest pat '... rest)
       (recur/pat (append (build-list ((next-sequence-decision)) (λ (_) pat)) rest))]
      [(list-rest pat rest)
       (let*-values 
           ([(pat-term state) (recur/pat pat)]
            [(rest-term state)
             ((recur rest in-hole) state)])
         (values (cons pat-term rest-term) state))]
      [else
       (error 'generate "unknown pattern ~s\n" pat)]))
  
  (define (extract-bound-vars pat state)
    (let loop ([found-vars-table (state-fvt (gen-state-current state))])
      (cond
        [(null? found-vars-table) '()]
        [else (let ([found-vars (car found-vars-table)])
                (if (eq? pat (found-vars-nt found-vars))
                    (found-vars-bound-vars found-vars)
                    (loop (cdr found-vars-table))))])))
    
  (define (extend-found-vars pat res state)
    (make-gen-state
     (make-state
      (map
       (λ (found-vars)
         (cond
           [(eq? (found-vars-source found-vars) pat)
            (let ([new-found-vars
                   (make-found-vars (found-vars-nt found-vars)
                                    (found-vars-source found-vars)
                                    (cons res (found-vars-bound-vars found-vars))
                                    #f)])
              (when (found-vars-found-nt? found-vars)
                (error 'generate "kludge in #:binds was exposed! #:binds ~s ~s" 
                       (found-vars-nt found-vars)
                       (found-vars-source found-vars)))
              new-found-vars)]
           [(eq? (found-vars-nt found-vars) pat)
            (make-found-vars (found-vars-nt found-vars)
                             (found-vars-source found-vars)
                             (found-vars-bound-vars found-vars)
                             #t)]
           [else found-vars]))
       (state-fvt (gen-state-current state)))
      (state-matches (gen-state-current state))
      (state-mismatches (gen-state-current state)))
     (gen-state-hole state)))

  (let ([initial-state (make-gen-state (make-state null #hasheq() #hasheq()) #f)])
    (let-values ([(term _) (((generate-pat null size) pat #f) initial-state)])
      term)))

;; find-base-cases : compiled-language -> hash-table
(define (find-base-cases lang)
  (define nt-table (make-hasheq))
  (define changed? #f)
  (define (nt-get nt) (hash-ref nt-table nt 'inf))
  (define (nt-set nt new) 
    (let ([old (nt-get nt)])
      (unless (equal? new old)
        (set! changed? #t)
        (hash-set! nt-table nt new))))
  
  (define (process-nt nt)
    (nt-set (nt-name nt) (apply min/f (map process-rhs (nt-rhs nt)))))
  
  (define (process-rhs rhs)
    (let ([nts (rhs->nts (rhs-pattern rhs))])
      (if (null? nts) 
          0
          (add1/f (apply max/f (map nt-get nts))))))
  
  ;; rhs->path : pattern -> (listof symbol)
  ;; determines all of the non-terminals in a pattern
  (define (rhs->nts pat)
    (let ([nts '()])
      (let loop ([pat pat])
        (match pat
          [(? symbol? pat)
           (when (is-nt? lang (symbol->nt pat))
             (set! nts (cons (symbol->nt pat) nts)))]
          [(or (? number?) (? string?) (? procedure?) (? boolean?)) (void)]
          [`() (void)]
          [`(,a ,'... . ,b) 
           (loop a)
           (loop b)]
          [`(,a . ,b)
           (loop a)
           (loop b)]))
      nts))
  
  (let loop ()
    (set! changed? #f)
    (for-each process-nt (compiled-lang-lang lang))
    (when changed?
      (loop)))
  
  (let ([ht (make-hash)])
    (for-each
     (λ (nt) (hash-set! ht (nt-name nt) (map process-rhs (nt-rhs nt))))
     (compiled-lang-lang lang))
    ht))

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
(define (is-nt? lang x)
  (and (hash-ref (compiled-lang-ht lang) x #f) #t))

(define named-nt-rx #rx"^([^_]+)_[^_]*$")
(define mismatch-nt-rx #rx"([^_]+)_!_[^_]*$")
(define named-ellipsis-rx #rx"^..._[^_]*$")
(define mismatch-ellipsis-rx #rx"^..._!_[^_]*$")

(define-struct ellipsis (name pattern constraints))
(define-struct (named-ellipsis ellipsis) (src-name))
(define-struct (mismatch-ellipsis ellipsis) ())

;; parse-pattern : pattern -> parsed-pattern
;; Turns "pat ..." and "pat ..._id" into named-ellipsis structs
;; and "pat ..._!_id" into mismatch-ellipsis structs.
(define parse-pattern
  (match-lambda
    [(list-rest pat (and (? symbol?) (app symbol->string (regexp named-ellipsis-rx)) name) rest)
     (cons (make-named-ellipsis name (parse-pattern pat) null name) (parse-pattern rest))]
    [(list-rest pat '... rest)
     (cons (make-named-ellipsis (gensym '..._) (parse-pattern pat) null '...) (parse-pattern rest))]
    [(list-rest pat (and (? symbol?) (app symbol->string (regexp mismatch-ellipsis-rx)) name) rest)
     (cons (make-mismatch-ellipsis name (parse-pattern pat) null) (parse-pattern rest))]
    [(cons first rest) (cons (parse-pattern first) (parse-pattern rest))]
    [else else]))

(define (hash-cons hash key val)
  (hash-set hash key (cons val (hash-ref hash key null))))

;; An ellipsis-context (listof ellipsis?) records the ellipses above a
;; a position in a pattern.
;;
;; ellipsis-context-sets : parsed-pattern -> (listof (listof contexts))
;; Extracts ellipsis-context-sets for each named non-teminal (e.g., x_1).
(define (ellipsis-context-sets pattern)
  (hash-map
   (let recur ([pattern pattern] [under null] [contexts #hasheq()])
     (match pattern
       [(and (? symbol?) (app symbol->string (regexp named-nt-rx)))
        (if (null? under) contexts (hash-cons contexts pattern under))]
       [(struct ellipsis (_ sub-pattern _))
        (recur sub-pattern (cons pattern under) contexts)]
       [(? list?)
        (foldl (λ (p n) (recur p under n)) contexts pattern)]
       [_ contexts]))
   (λ (named-nt contexts) contexts)))

;; representative-context 
;;   (cons/c ellipsis-context (listof ellipses-context)) -> ellipses-context
;; Merges a (non-empty) list of ellipsis-contexts of common depth into a 
;; single "representative" context in which ...
(define (representative-context contexts)
  (foldl (λ (current representative)
           (map (λ (c r) (if (named-ellipsis? c) c r))
                current representative)) 
         (car contexts) (cdr contexts)))

;; ellipsis-names-rewrites : parsed-pattern -> hash[sym -o> sym]
;; Produces a hash mapping ellipsis names to new names, for use in
;; `rewrite-named-ellipsis'
(define (ellipsis-names-rewrites pat)
  ; union-find w/o balancing or path compression (for now)
  (define (union e f sets)
    (hash-set sets (find f sets) (find e sets)))
  (define (find e sets)
    (let recur ([chd e] [par (hash-ref sets e #f)])
      (if (and par (not (eq? chd par))) (recur par (hash-ref sets par #f)) chd)))
  
  (define (context-set-equivalence contexts classes)
    (let ([representative (representative-context contexts)])
      (foldl 
       (λ (context classes)
         (foldl 
          (λ (cur rep classes)
            (if (or (mismatch-ellipsis? cur) (mismatch-ellipsis? rep))
                classes
                (union (ellipsis-name rep) (ellipsis-name cur) classes)))
          classes context representative))
       classes contexts)))
  
  (let ([classes (foldl context-set-equivalence #hasheq() (ellipsis-context-sets pat))])
    (make-immutable-hasheq 
     (hash-map classes (λ (named-ellip _) (cons named-ellip (find named-ellip classes)))))))

(define (rewrite-named-ellipses pat rewrites)
  (match pat
    [(struct named-ellipsis (name sub-pat constraints src-name))
     (let ([rewritten (rewrite-named-ellipses sub-pat rewrites)])
       (make-named-ellipsis (hash-ref rewrites name name) rewritten constraints src-name))]
    [(struct mismatch-ellipsis (name sub-pat constraints))
     (make-mismatch-ellipsis name (rewrite-named-ellipses sub-pat rewrites) constraints)]
    [(? list?) (map (λ (p) (rewrite-named-ellipses p rewrites)) pat)]
    [_ pat]))

;; sequence-length-constraints : parsed-pattern -> hash[symbol -> (listof symbol)]
;; Produces a hash mapping ellipsis names to the names of the ellipses that cannot
;; have the same sequence length.
(define (sequence-length-constraints pat)
  (define empty-set #hasheq())
  (define (extend member set) (hash-set set member #t))
  (define (member? element set) (hash-ref set element #f))
  (define (set->list set) (hash-map set (λ (elem _) elem)))
  
  (define (exclude ellip from constraints)
    (hash-set constraints (ellipsis-name from) 
              (extend (ellipsis-name ellip)
                      (hash-ref constraints (ellipsis-name from) empty-set))))
  
  (define (context-set-constraints contexts constraints)
    (let ([representative (representative-context contexts)])
      (foldl
       (λ (context constraints)
         (foldl 
          (λ (cur rep constraints)
            (if (or (mismatch-ellipsis? rep) (named-ellipsis? cur))
                constraints
                (exclude cur rep (exclude rep cur constraints))))
          constraints context representative))
       constraints contexts)))
  
  (make-immutable-hash
   (hash-map
    (foldl context-set-constraints #hasheq() (ellipsis-context-sets pat))
    (λ (ellip-name exclusion-set) (cons ellip-name (set->list exclusion-set))))))

(define-syntax check
  (syntax-rules ()
    [(_ lang ([id pat] ...) attempts size property)
     (let loop ([remaining attempts])
       (if (zero? remaining)
           #t
           (let ([attempt (add1 (- attempts remaining))])
             (term-let 
              ([id (generate lang pat size attempt)] ...)
              (let ([generated (term ((,'id id) ...))])
                (if (with-handlers 
                        ([exn:fail? (λ (exn) (error 'check "term ~s raises ~s" generated exn))])
                      property)
                    (loop (sub1 remaining))
                    (format "failed after ~s attempts: ~s" 
                            attempt generated)))))))]))

(define-syntax (generate stx)
  (syntax-case stx ()
    [(_ lang pat size attempt)
     (syntax (generate lang pat size attempt random-decisions@))]
    [(_ lang pat size attempt decisions@)
     (with-syntax ([rewritten 
                    (rewrite-side-conditions/check-errs 
                     (language-id-nts #'lang 'generate)
                     'generate
                     #f
                     #'pat)])
       (syntax (generate* lang `rewritten size attempt decisions@)))]))

(define-signature decisions^
  (next-variable-decision
   next-number-decision
   next-non-terminal-decision
   next-sequence-decision
   next-any-decision
   next-string-decision))

(define random-decisions@
  (unit (import) (export decisions^)
        (define (next-variable-decision) pick-var)
        (define (next-number-decision) pick-from-list)
        (define (next-non-terminal-decision) pick-nt)
        (define (next-sequence-decision) pick-length)
        (define (next-any-decision) pick-any)
        (define (next-string-decision) pick-string)))

(provide pick-from-list pick-var pick-length min-prods decisions^ 
         is-nt? lang-literals pick-char random-string pick-string
         check pick-nt unique-chars pick-any sexp generate parse-pattern
         ellipsis-context-sets ellipsis-names-rewrites sequence-length-constraints
         (struct-out ellipsis) (struct-out named-ellipsis) (struct-out mismatch-ellipsis))

(provide/contract
 [find-base-cases (-> compiled-lang? hash?)])