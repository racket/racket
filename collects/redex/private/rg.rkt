#lang racket/base

(require "matcher.rkt"
         "reduction-semantics.rkt"
         "underscore-allowed.rkt"
         "error.rkt"
         "struct.rkt"
         "match-a-pattern.rkt"
         (for-syntax "reduction-semantics.rkt")
         racket/dict
         racket/contract
         racket/promise
         racket/unit
         racket/match
         mrlib/tex-table)

(define redex-pseudo-random-generator
  (make-parameter (current-pseudo-random-generator)))
(define (generator-random . arg)
  (parameterize ([current-pseudo-random-generator (redex-pseudo-random-generator)])
    (apply random arg)))

(define (exotic-choice? [random generator-random]) (= 0 (random 5)))
(define (use-lang-literal? [random generator-random]) (= 0 (random 20)))

(define default-check-attempts (make-parameter 1000))

(define ascii-chars-threshold 1000)
(define tex-chars-threshold 1500)
(define chinese-chars-threshold 2500)

(define (pick-var lang-lits attempt [random generator-random])
  (let ([length (add1 (random-natural 4/5 random))])
    (string->symbol (random-string lang-lits length attempt random))))

(define (pick-char attempt [random generator-random])
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

(define (random-string lang-lits length attempt [random generator-random])
  (if (and (not (null? lang-lits)) (use-lang-literal? random))
      (pick-from-list lang-lits random)
      (list->string (build-list length (λ (_) (pick-char attempt random))))))

(define (pick-any lang sexp [random generator-random]) 
  (if (and (> (dict-count (rg-lang-non-cross lang)) 0) (zero? (random 5)))
      (let ([nts (rg-lang-non-cross lang)])
        (values lang (pick-from-list (dict-map nts (λ (nt _) nt)) random)))
      (values sexp 'sexp)))

(define (pick-string lang-lits attempt [random generator-random])
  (random-string lang-lits (random-natural 1/5 random) attempt random))

;; next-non-terminal-decision selects a subset of a non-terminal's productions.
;; This implementation, the default, chooses them all, but many of the
;; generator's test cases restrict the productions.
(define pick-nts values)

(define (pick-from-list l [random generator-random])
  (list-ref l (random (length l))))

;; Chooses a random (exact) natural number from the "shifted" geometric distribution:
;;   P(random-natural = k) = p(1-p)^k
;;
;; P(random-natural >= k) = (1-p)^(k+1)
;; E(random-natural) = (1-p)/p
;; Var(random-natural) = (1-p)/p^2
(define (random-natural p [random generator-random])
  (sub1 (inexact->exact (ceiling (real-part (/ (log (random)) (log (- 1 p))))))))

(define (negative? random)
  (zero? (random 2)))

(define (random-integer p [random generator-random])
  (* (if (negative? random) -1 1) (random-natural p random)))

(define (random-rational p [random generator-random])
  (/ (random-integer p random) (add1 (random-natural p random))))

(define (random-real p [random generator-random])
  (* (random) 2 (random-integer p random)))

(define (random-complex p [random generator-random])
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
(define default-attempt-size
  (λ (n) (inexact->exact (floor (/ (log (add1 n)) (log 5))))))
(define attempt-size/c
  (-> natural-number/c natural-number/c))
(define attempt->size 
  (make-parameter default-attempt-size))

(define (pick-number attempt #:top-threshold [top-threshold complex-threshold] [random generator-random])
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
        (generator (expected-value->p ((attempt->size) (- attempt threshold))) random)
        (loop (caar levels) (cdar levels) (cdr levels)))))

(define (pick-natural attempt [random generator-random])
  (pick-number attempt #:top-threshold 0 random))

(define (pick-integer attempt [random generator-random])
  (pick-number attempt #:top-threshold integer-threshold random))

(define (pick-real attempt [random generator-random])
  (pick-number attempt #:top-threshold real-threshold random))

(define (pick-boolean attempt [random generator-random])
  (zero? (random 2)))

(define (pick-sequence-length size)
  (random-natural (expected-value->p size)))

(define (min-prods nt prods base-table)
  (let* ([sizes (hash-ref base-table nt)]
         [min-size (apply min/f sizes)])
    (map cadr (filter (λ (x) (equal? min-size (car x))) (map list sizes prods)))))

(define-struct rg-lang (non-cross delayed-cross base-cases))
(define (rg-lang-cross x) (force (rg-lang-delayed-cross x)))
(define (prepare-lang lang)
  (values lang 
          (map symbol->string (compiled-lang-literals lang))
          (find-base-cases lang)))

(define-struct (exn:fail:redex:generation-failure exn:fail:redex) ())
(define (raise-gen-fail who what attempts)
  (let ([str (format "~a: unable to generate ~a in ~a attempt~a" 
                     who what attempts (if (= attempts 1) "" "s"))])
    (raise (make-exn:fail:redex:generation-failure str (current-continuation-marks)))))

(define (compile lang what)
  (define-values/invoke-unit (generation-decisions)
    (import) (export decisions^))
  
  (define (gen-nt lang name cross? retries size attempt filler)
    (define productions
      (hash-ref ((if cross? rg-lang-cross rg-lang-non-cross) lang) name))
    (define-values (term _)
      (let ([gen (pick-from-list
                  (if (zero? size)
                      (min-prods name productions 
                                 ((if cross? base-cases-cross base-cases-non-cross)
                                  (rg-lang-base-cases lang)))
                      ((next-non-terminal-decision) productions)))])
        (gen retries (max 0 (sub1 size)) attempt empty-env filler)))
    term)
  
  (define (generate/pred name gen pred init-sz init-att retries)
    (define pre-threshold-incr 
      (ceiling
       (/ (- retry-threshold init-att)
          (* proportion-before-threshold (add1 retries)))))
    (define (incr-size? remain)
      (zero? 
       (modulo (sub1 remain) 
               (ceiling (* proportion-at-size retries)))))
    (let retry ([remaining (add1 retries)]
                [size init-sz]
                [attempt init-att])
      (if (zero? remaining)
          (raise-gen-fail what (format "pattern ~s" name) retries)
          (let-values ([(term env) (gen size attempt)])
            (if (pred term env)
                (values term env)
                (retry (sub1 remaining)
                       (if (incr-size? remaining) (add1 size) size)
                       (+ attempt
                          (if (>= attempt retry-threshold)
                              post-threshold-incr
                              pre-threshold-incr))))))))
  
  (define (generate/prior name env gen)
    (let* ([none (gensym)]
           [prior (hash-ref env name none)])
      (if (eq? prior none)
          (let-values ([(term env) (gen)])
            (values term (hash-set env name term)))
          (values prior env))))
  
  
  (define (generate-sequence gen env vars length)
    (define (split-environment env)
      (foldl (λ (var seq-envs)
               (let ([vals (hash-ref env var #f)])
                 (if vals
                     (map (λ (seq-env val) (hash-set seq-env var val)) seq-envs vals)
                     seq-envs)))
             (build-list length (λ (_) #hash())) vars))
    (define (merge-environments seq-envs)
      (foldl (λ (var env)
               (hash-set env var (map (λ (seq-env) (hash-ref seq-env var)) seq-envs)))
             env vars))
    (define-values (seq envs)
      (let recur ([envs (split-environment env)])
        (if (null? envs)
            (values null null)
            (let*-values 
                ([(hd env) (gen (car envs))]
                 [(tl envs) (recur (cdr envs))])
              (values (cons hd tl) (cons env envs))))))
    (values seq (merge-environments envs)))
  
  (define ((generator/attempts g) r s a e f)
    (values (g a) e))
  
  (define (mismatches-satisfied? env)
    (define groups (make-hasheq))
    (define (get-group group)
      (hash-ref groups group
                (λ ()
                  (let ([vals (make-hash)])
                    (hash-set! groups group vals)
                    vals))))
    (for/and ([(name val) env])
      (or (not (mismatch? name))
          (let ([prior (get-group (mismatch-var name))])
            (and (not (hash-ref prior val #f))
                 (hash-set! prior val #t))))))
  
  (define empty-env #hash())
  
  (define (bindings env)
    (make-bindings
     (for/fold ([bindings null]) ([(key val) (in-hash env)])
       (if (symbol? key) 
           (cons (make-bind key val) bindings)
           bindings))))
  
  (define-values (langp lits lang-bases) (prepare-lang lang))
  (define-values (sexpp _ sexp-bases) (prepare-lang sexp))
  (define lit-syms (compiled-lang-literals lang))
  
  (define (compile pat any?)
    
    (define vars-table (make-hash))
    (define (find-vars pat) (hash-ref vars-table pat '()))
    (define mismatch-id 0)
    (define-values (rewritten-pat vars)
      (let loop ([pat pat])
        (define (add/ret pat vars) 
          (hash-set! vars-table pat vars)
          (values pat vars))
        (define (build-mismatch var) 
          (set! mismatch-id (+ mismatch-id 1))
          (make-mismatch mismatch-id var))
        (match-a-pattern pat
          [`any (values pat '())]
          [`number (values pat '())]
          [`string (values pat '())]
          [`natural (values pat '())]
          [`integer (values pat '())]
          [`real (values pat '())]
          [`boolean (values pat '())]
          [`variable (values pat '())]
          [`(variable-except ,vars ...) (values pat '())]
          [`(variable-prefix ,var) (values pat '())]
          [`variable-not-otherwise-mentioned (values pat '())]
          [`hole (values pat '())]
          [`(nt ,x) (values pat '())]
          [`(name ,name ,p) 
           (define-values (p-rewritten p-names) (loop p))
           (add/ret `(name ,name ,p-rewritten) (cons name p-names))]
          [`(mismatch-name ,name ,p)
           (define mm (build-mismatch name)) 
           (define-values (p-rewritten p-names) (loop p))
           (add/ret `(mismatch-name ,mm ,p-rewritten)
                    (cons mm p-names))]
          [`(in-hole ,p1 ,p2) 
           (define-values (p1-rewritten p1-names) (loop p1))
           (define-values (p2-rewritten p2-names) (loop p2))
           (add/ret `(in-hole ,p1-rewritten ,p2-rewritten)
                    (append p1-names p2-names))]
          [`(hide-hole ,p) 
           (define-values (p-rewritten p-names) (loop p))
           (add/ret `(hide-hole ,p-rewritten) p-names)]
          [`(side-condition ,p ,e ,e2) 
           (define-values (p-rewritten p-names) (loop p))
           (add/ret `(side-condition ,p-rewritten ,e ,e2) p-names)]
          [`(cross ,var) (values pat '())]
          [`(list ,lpats ...)
           (define-values (lpats-rewritten vars)
             (for/fold ([ps-rewritten '()]
                        [vars '()])
                       ([lpat (in-list lpats)])
               (match lpat 
                 [`(repeat ,p ,name ,mismatch-name)
                  (define l1 (if name (list name) '()))
                  (define mm (and mismatch-name
                                  (build-mismatch mismatch-name)))
                  (define l2 (if mm (cons mm l1) l1))
                  (define-values (p-rewritten p-vars) (loop p))
                  (values (cons `(repeat ,p-rewritten ,name ,mm) ps-rewritten)
                          (append l2 p-vars vars))]
                 [_ 
                  (define-values (p-rewritten p-vars) (loop lpat))
                  (values (cons p-rewritten ps-rewritten)
                          (append p-vars vars))])))
           (add/ret `(list ,@(reverse lpats-rewritten))
                    vars)]
          [(? (compose not pair?)) (values pat '())])))
    
    (let* ([nt? (is-nt? (if any? sexpp langp))]
           [mismatches? #f]
           [generator 
            ; retries size attempt env filler -> (values terms env)
            ;
            ; Patterns like (in-hole C_1 p) require constructing both an unfilled context
            ; (exposed via the C_1 binding) and a filled context (exposed as the result).
            ; A generator constructs both by constructing the context, using either 
            ; `the-hole' or `the-not-hole' as appropriate, then filling it using `plug'.
            ; Before returning its result, a generator replaces occurrences of `the-not-hole'
            ; with `the-hole' to avoid exposing the distinction to the user, but 
            ; `the-not-hole' remains in bindings supplied to side-condition predicates, to
            ; match the behavior of the matcher.
            ;
            ; Repeated traversals via `plug' are not asymptotically worse than simultaneously
            ; constructing the filled and unfilled pattern, due to languages like this one,
            ; which names contexts in a way that prevents any sharing.
            ; (define-language L
            ;  (W hole
            ;  ; extra parens to avoid matcher loop
            ;  (in-hole (W_1) (+ natural hole))))
            (let recur ([pat rewritten-pat])
              (match-a-pattern pat
                [`any
                 (λ (r s a e f)
                   (let*-values ([(lang nt) ((next-any-decision) langc sexpc)]
                                 [(term) (gen-nt lang nt #f r s a the-not-hole)])
                     (values term e)))]
                [`number (generator/attempts (λ (a) ((next-number-decision) a)))]
                [`string (generator/attempts (λ (a) ((next-string-decision) lits a)))]
                [`natural (generator/attempts (λ (a) ((next-natural-decision) a)))]
                [`integer (generator/attempts (λ (a) ((next-integer-decision) a)))]
                [`real (generator/attempts (λ (a) ((next-real-decision) a)))]
                [`boolean (generator/attempts (λ (a) ((next-boolean-decision) a)))]
                [`variable (generator/attempts (λ (a) ((next-variable-decision) lits a)))]
                [`(variable-except ,vars ...)
                 (let ([g (recur 'variable)])
                   (λ (r s a e f)
                     (generate/pred pat
                                    (λ (s a) (g r s a e f))
                                    (λ (var _) (not (memq var vars)))
                                    s a r)))]
                [`(variable-prefix ,prefix)
                 (define (symbol-append prefix suffix)
                   (string->symbol (string-append (symbol->string prefix) (symbol->string suffix))))
                 (let ([g (recur 'variable)])
                   (λ (r s a e f)
                     (let-values ([(t e) (g r s a e f)])
                       (values (symbol-append prefix t) e))))]
                [`variable-not-otherwise-mentioned
                 (let ([g (recur 'variable)])
                   (λ (r s a e f)
                     (generate/pred pat
                                    (λ (s a) (g r s a e f))
                                    (λ (var _) (not (memq var lit-syms)))
                                    s a r)))]
                [`hole (λ (r s a e f) (values f e))]
                [`(nt ,nt-id)
                   (λ (r s a e f)
                     (values (gen-nt (if any? sexpc langc) nt-id #f r s a f) e))]
                [`(name ,id ,p)
                 (let ([g (recur p)])
                   (λ (r s a e f)
                     (generate/prior id e (λ () (g r s a e f)))))]
                [`(mismatch-name ,id ,pat)
                 (let ([g (recur pat)])
                   (set! mismatches? #t)
                   (λ (r s a e f)
                     (let-values ([(t e) (g r s a e f)])
                       (values t (hash-set e id t)))))]
                [`(in-hole ,context ,filler)
                 (let ([c-context (recur context)]
                       [c-filler (recur filler)])
                   (λ (r s a e f)
                     (let*-values ([(filler env) (c-filler r s a e f)]
                                   [(context env) (c-context r s a env the-hole)])
                       (values (plug context filler) env))))]
                [`(hide-hole ,pattern)
                 (let ([g (recur pattern)])
                   (λ (r s a e f)
                     (g r s a e the-not-hole)))]
                [`(side-condition ,pat ,(? procedure? condition) ,guard-src-loc)
                 (let ([g (recur pat)])
                   (λ (r s a e f)
                     (generate/pred `(side-condition ,(unparse-pattern pat) ,guard-src-loc) 
                                    (λ (s a) (g r s a e f))
                                    (λ (_ env) (condition (bindings env)))
                                    s a r)))]
                [`(cross ,(? symbol? p))
                 (λ (r s a e f)
                   (values (gen-nt (if any? sexpc langc) p #t r s a f) e))]
                
                [`(list ,in-lpats ...)
                 (let loop ([lpats in-lpats])
                   (match lpats
                     [`() (λ (r s a e f) (values '() e))]
                     [(cons `(repeat ,sub-pat ,name ,mismatch-name) rst)
                      (let ([elemg (recur sub-pat)]
                            [tailg (loop rst)]
                            [vars (find-vars sub-pat)])
                        (when mismatch-name
                          (set! mismatches? #t))
                        (λ (r s a env0 f)
                          (define len 
                            (let ([prior (and name (hash-ref env0 name #f))])
                              (if prior
                                  prior
                                  (if (zero? s) 0 ((next-sequence-decision) s)))))
                          (let*-values ([(seq env) (generate-sequence (λ (e) (elemg r s a e f)) env0 vars len)]
                                        [(env) (if name (hash-set env name len) env)]
                                        [(env) (if mismatch-name 
                                                   (hash-set env mismatch-name len)
                                                   env)]
                                        [(tail env) (tailg r s a env f)])
                            (values (append seq tail) env))))]
                     [(cons hdp tlp)
                      (let ([hdg (recur hdp)]
                            [tlg (loop tlp)])
                        (λ (r s a env f)
                          (let*-values 
                              ([(hd env) (hdg r s a env f)]
                               [(tl env) (tlg r s a env f)])
                            (values (cons hd tl) env))))]))]
                [(? (compose not pair?))
                 (λ (r s a e f) (values pat e))]))])
      (if mismatches?
          (λ (r s a e f)
            (let ([g (λ (s a) (generator r s a e f))]
                  [p? (λ (_ e) (mismatches-satisfied? e))])
              (generate/pred (unparse-pattern pat) g p? s a r)))
          generator)))
  (define (compile-non-terminals nts any?)
    (make-immutable-hash
     (map (λ (nt) (cons (nt-name nt)
                        (map (λ (p) (compile (rhs-pattern p) any?))
                             (nt-rhs nt))))
          nts)))
  (define (compile-language lang bases any?)
    (make-rg-lang
     (compile-non-terminals (compiled-lang-lang lang) any?)
     (delay (compile-non-terminals (compiled-lang-cclang lang) any?))
     bases))
  (define langc (compile-language langp lang-bases #f))
  (define sexpc (compile-language sexpp sexp-bases #t))
  (define (compile-pattern pat) (compile pat #f))
  (λ (pat)
    (define g (compile-pattern pat))
    (λ (size attempt retries)
      (define-values (t e) (g retries size attempt empty-env the-hole))
      (values (let replace-the-not-hole ([t t])
                (cond [(eq? t the-not-hole) the-hole]
                      [(list? t) (map replace-the-not-hole t)]
                      [else t])) 
              (bindings e)))))

(define-struct base-cases (delayed-cross non-cross))
(define (base-cases-cross x) (force (base-cases-delayed-cross x)))

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
        (match-a-pattern pat
          [`any (void)]
          [`number (void)]
          [`string (void)]
          [`natural (void)]
          [`integer (void)]
          [`real (void)]
          [`boolean (void)]
          [`variable (void)]
          [`(variable-except ,vars ...) (void)]
          [`(variable-prefix ,var) (void)]
          [`variable-not-otherwise-mentioned (void)]
          [`hole (void)]
          [`(nt ,var) (set! nts (cons (cons #f var) nts))]
          [`(name ,n ,p) (loop p)]
          [`(mismatch-name ,n ,p) (loop p)]
          [`(in-hole ,p1 ,p2) (loop p1) (loop p2)]
          [`(hide-hole ,p) (loop p)]
          [`(side-condition ,p ,exp ,info) (loop p)]
          [`(cross ,x-nt)
           (set! nts (cons (cons #t x-nt) nts))]
          [`(list ,lpats ...)
           (for ([lpat (in-list lpats)])
             (match lpat
               [`(repeat ,p ,name ,mismatch?)
                (loop p)]
               [_ (loop lpat)]))]
          [(? (compose not pair?)) (void)]))
      nts))

  ;; build-table : (listof nt) -> hash
  (define (build-table nts)
    (let ([tbl (make-hasheq)])
      (for-each
       (λ (nt) (hash-set! tbl (nt-name nt) (map process-rhs (nt-rhs nt))))
       nts)
      tbl))
  
  ;; we can delay the work of computing the base cases for
  ;; the cross part of the language since none of the productions 
  ;; refer to it (as that's not allowed in general and would be
  ;; quite confusing if it were...)
  (let loop ()
    (set! changed? #f)
    (for-each (process-nt #f) (compiled-lang-lang lang))
    (when changed?
      (loop)))
  (make-base-cases
   (delay (begin
            (let loop ()
              (set! changed? #f)
              (for-each (process-nt #t) (compiled-lang-cclang lang))
              (when changed?
                (loop)))
            (build-table (compiled-lang-cclang lang))))
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

(define-struct class (id) #:transparent)

(define-struct mismatch (id var) #:transparent)

(define-struct binder (name) #:transparent)
(define binder-pattern
  (match-lambda
    [(struct binder (name))
     (match ((symbol-match named-nt-rx) name)
       [#f name]
       [p p])]))

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

;; unparse-pattern: parsed-pattern -> pattern
(define unparse-pattern
  (match-lambda
    [(struct binder (name)) name]
    [(struct mismatch (id var)) var]
    [(list-rest (struct ellipsis (name sub-pat _ _)) rest)
     (let ([ellipsis (if (mismatch? name) (mismatch-var name) name)])
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
         [assignments #hasheq()] 
         [record-binder
          (λ (pat under)
            (set! assignments
                  (if (null? under) 
                      assignments
                      (let ([last (hash-ref last-contexts pat #f)])
                        (if last
                            (foldl (λ (cur last asgns) (union cur last asgns)) assignments under last)
                            (begin
                              (hash-set! last-contexts pat under)
                              assignments))))))])
    (let recur ([pat pattern] [under null])
      (match-a-pattern pat
                       [`any assignments]
                       [`number assignments]
                       [`string assignments]
                       [`natural assignments]
                       [`integer assignments]
                       [`real assignments]
                       [`boolean assignments]
                       [`variable assignments]
                       [`(variable-except ,vars ...) assignments]
                       [`(variable-prefix ,var) assignments]
                       [`variable-not-otherwise-mentioned assignments]
                       [`hole assignments]
                       [`(nt ,var) assignments]
                       [`(name ,var ,pat)
                        (record-binder var under)
                        (recur pat under)]
                       [`(mismatch-name ,var ,pat)
                        (recur pat under)]
                       [`(in-hole ,p1 ,p2)
                        (recur p2 under)
                        (recur p1 under)]
                       [`(hide-hole ,p)
                        (recur p under)]
                       [`(side-condition ,p ,exp ,srcloc)
                        (recur p under)]
                       [`(cross ,nt) assignments]
                       [`(list ,lpats ...)
                        (for ([lpat (in-list lpats)])
                          (match lpat
                            [`(repeat ,p ,name ,mismatch?)
                             (record-binder name under)
                             (recur p (cons (or name (gensym)) under))]
                            [else (recur lpat under)]))
                        assignments]
                       [(? (compose not pair?)) assignments]))
    (make-immutable-hasheq (hash-map assignments (λ (cls _) (cons cls (find cls assignments)))))))

(define (reassign-classes pattern)
  (let* ([reassignments (class-reassignments pattern)]
         [rewrite (λ (c) (make-class (hash-ref reassignments (class-id c) (class-id c))))])
    (let recur ([pat pattern])
      (match pat
        #;
        [`(repeat ,sub-pat ,name ,mismatch?)
         `(repeat ,(recur sub-pat)
                  ,(rewrite name)
                  ,mismatch?)]
        [(struct ellipsis (name sub-pat class vars))
         (make-ellipsis name (recur sub-pat) (rewrite class)
                        (map (λ (v) (if (class? v) (rewrite v) v)) vars))]
        [(? list?) (map recur pat)]
        [_ pat]))))

;; used in generating the `any' pattern
(define-language sexp (sexp variable string number hole (sexp ...)))

(define-signature decisions^
  (next-variable-decision
   next-number-decision
   next-natural-decision
   next-integer-decision
   next-real-decision
   next-boolean-decision
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
        (define (next-boolean-decision) pick-boolean)
        (define (next-non-terminal-decision) pick-nts)
        (define (next-sequence-decision) pick-sequence-length)
        (define (next-any-decision) pick-any)
        (define (next-string-decision) pick-string)))

(define generation-decisions (make-parameter random-decisions@))

(provide (struct-out ellipsis) 
         (struct-out mismatch)
         (struct-out class)
         (struct-out binder)
         (struct-out rg-lang)
         (struct-out base-cases) 
         base-cases-cross
         (struct-out exn:fail:redex:generation-failure)
         raise-gen-fail)

(provide pick-from-list pick-sequence-length pick-nts
         pick-char pick-var pick-string pick-any pick-boolean
         pick-number pick-natural pick-integer pick-real
         unparse-pattern
         prepare-lang
         class-reassignments reassign-classes
         default-retries
         default-attempt-size
         default-check-attempts
         attempt-size/c
         proportion-at-size
         retry-threshold proportion-before-threshold post-threshold-incr
         is-nt? nt-by-name min-prods
         generation-decisions decisions^ 
         random-string
         sexp 
         find-base-cases
         attempt->size
         redex-pseudo-random-generator)

(provide compile)
