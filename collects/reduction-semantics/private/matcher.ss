#|

Note: the patterns described in the doc.txt file are
slightly different than the patterns processed here.
The difference is in the form of the side-condition
expressions. Here they are procedures that accept
binding structures, instead of expressions. The
reduction (And other) macros do this transformation
before the pattern compiler is invoked.

|#
(module matcher mzscheme
  (require (lib "list.ss")
           (lib "match.ss")
           (lib "etc.ss")
           (lib "contract.ss"))
  
  ;; lang = (listof nt)
  ;; nt = (make-nt sym (listof rhs))
  ;; rhs = (make-rhs single-pattern)
  ;; single-pattern = sexp
  (define-struct nt (name rhs) (make-inspector))
  (define-struct rhs (pattern) (make-inspector))
  
  ;; var = (make-var sym sexp)
  ;; patterns are sexps with `var's embedded
  ;; in them. It means to match the
  ;; embedded sexp and return that binding
  
  ;; bindings = (make-bindings (listof rib))
  ;; rib = (make-rib sym sexp)
  ;; if a rib has a pair, the first element of the pair should be treated as a prefix on the identifer
  (define-values (make-bindings bindings-table bindings?)
    (let () 
      (define-struct bindings (table) (make-inspector)) ;; for testing, add inspector
      (values (lambda (table)
                (unless (and (list? table)
                             (andmap rib? table))
                  (error 'make-bindings "expected <(listof rib)>, got ~e" table))
                (make-bindings table))
              bindings-table
              bindings?)))
  
  (define-struct rib (name exp) (make-inspector)) ;; for testing, add inspector
  
  ;; repeat = (make-repeat compiled-pattern (listof rib))
  (define-struct repeat (pat empty-bindings) (make-inspector)) ;; inspector for tests below
  
  ;; compiled-pattern : exp (union #f none sym) -> (union #f (listof mtch))
  ;; mtch = (make-match bindings sexp[context w/none-inside for the hole] (union none sexp[hole]))
  ;; mtch is short for "match"
  (define-values (mtch-bindings mtch-context mtch-hole make-mtch mtch?)
    (let ()
      (define-struct mtch (bindings context hole) (make-inspector))
      (values mtch-bindings
              mtch-context
              mtch-hole
              (lambda (a b c)
                (unless (bindings? a)
                  (error 'make-mtch "expected bindings for first agument, got ~e" a))
                (make-mtch a b c))
              mtch?)))
  ;; used to mean no context is available; also used as the "name" for an unnamed (ie, normal) hole
  (define none
    (let ()
      (define-struct none ())
      (make-none)))
  (define (none? x) (eq? x none))
  
  ;; compiled-lang : (make-compiled-lang (listof nt) 
  ;;                                     hash-table[sym -o> compiled-pattern]
  ;;                                     hash-table[sym -o> compiled-pattern]
  ;;                                     hash-table[sym -o> compiled-pattern]
  ;;                                     hash-table[sym -o> boolean])
  ;;                                     hash-table[sexp[pattern] -o> (cons compiled-pattern boolean)])
  ;; hole-info = (union #f none symbol)
  ;;               #f means we're not in a `in-hole' context
  ;;               none means we're looking for a normal hole
  ;;               symbol means we're looking for a named hole named by the symbol
  (define compiled-pattern (any/c (union false/c none? symbol?) . -> . (union false/c (listof mtch?))))
  
  (define-struct compiled-lang (lang ht list-ht across-ht has-hole-ht cache))
  
  ;; lookup-binding : bindings (union sym (cons sym sym)) [(-> any)] -> any
  (define lookup-binding 
    (opt-lambda (bindings
		 sym
		 [fail (lambda () (error 'lookup-binding "didn't find ~e in ~e" sym bindings))])
      (let loop ([ribs (bindings-table bindings)])
        (cond
          [(null? ribs) (fail)]
          [else
           (let ([rib (car ribs)])
             (if (equal? (rib-name rib) sym)
                 (rib-exp rib)
                 (loop (cdr ribs))))]))))
  
  ;; compile-language : lang -> compiled-lang
  (define (compile-language lang)
    (let* ([clang-ht (make-hash-table)]
           [clang-list-ht (make-hash-table)]
           [across-ht (make-hash-table)]
           [has-hole-ht (build-has-hole-ht lang)]
           [cache (make-hash-table 'equal)]
           [clang (make-compiled-lang lang clang-ht clang-list-ht across-ht has-hole-ht cache)]
           [non-list-nt-table (build-non-list-nt-label lang)]
           [list-nt-table (build-list-nt-label lang)]
           [do-compilation
            (lambda (ht list-ht lang prefix-cross?)
              (for-each
               (lambda (nt)
                 (for-each
                  (lambda (rhs)
                    (let-values ([(compiled-pattern has-hole?) 
                                  (compile-pattern/cross? clang (rhs-pattern rhs) prefix-cross?)])
                      (let ([add-to-ht
                             (lambda (ht)
                               (hash-table-put!
                                ht
                                (nt-name nt)
                                (cons compiled-pattern
                                      (hash-table-get ht (nt-name nt)))))])
                        (when (may-be-non-list-pattern? (rhs-pattern rhs)
                                                        non-list-nt-table)
                          (add-to-ht ht))
                        (when (may-be-list-pattern? (rhs-pattern rhs) 
                                                     list-nt-table)
                          (add-to-ht list-ht)))))
                  (nt-rhs nt)))
               lang))]
           [init-ht
            (lambda (ht)
              (for-each (lambda (nt) (hash-table-put! ht (nt-name nt) null))
                        lang))])
      
      (init-ht clang-ht)
      (init-ht clang-list-ht)
      
      (hash-table-for-each
       clang-ht
       (lambda (nt rhs)
         (when (has-underscore? nt)
           (error 'compile-language "cannot use underscore in nonterminal name, ~s" nt))))
      
      (let ([compatible-context-language
             (build-compatible-context-language clang-ht lang)])
        (for-each (lambda (nt)
                    (hash-table-put! across-ht (nt-name nt) null))
                  compatible-context-language)
        (do-compilation clang-ht clang-list-ht lang #t)
        (do-compilation across-ht across-ht compatible-context-language #f)
        clang)))
  
  ; build-has-hole-ht : (listof nt) -> hash-table[symbol -o> boolean]
  ; produces a map of nonterminal -> whether that nonterminal could produce a hole
  (define (build-has-hole-ht lang)
    (build-nt-property 
     lang
     (lambda (pattern recur)
       (match pattern
         [`any #f]
         [`number #f]
         [`string #f]
         [`variable #f] 
         [`(variable-except ,@(vars ...)) #f]
         [`hole #t]
         [`(hole ,(? symbol? hole-name)) #t]
         [(? string?) #f]
         [(? symbol?)
          ;; cannot be a non-terminal, otherwise this function isn't called
          #f]
         [`(name ,name ,pat)
           (recur pat)]
         [`(in-hole ,context ,contractum)
           (recur contractum)]
         [`(in-named-hole ,hole-name ,context ,contractum)
           (recur contractum)]
         [`(side-condition ,pat ,condition)
           (recur pat)]
         [(? list?)
          (ormap recur pattern)]
         [else #f]))
     #t
     (lambda (lst) (ormap values lst))))
  
  ;; build-nt-property : lang (pattern[not-non-terminal] (pattern -> boolean) -> boolean) boolean
  ;;                  -> hash-table[symbol[nt] -> boolean]
  (define (build-nt-property lang test-rhs conservative-answer combine-rhss)
    (let ([ht (make-hash-table)]
          [rhs-ht (make-hash-table)])
      (for-each
       (lambda (nt)
         (hash-table-put! rhs-ht (nt-name nt) (nt-rhs nt))
         (hash-table-put! ht (nt-name nt) 'unknown))
       lang)
      (let ()
        (define (check-nt nt-sym)
          (let ([current (hash-table-get ht nt-sym)])
            (case current
              [(unknown)
               (hash-table-put! ht nt-sym 'computing)
               (let ([answer (combine-rhss 
                              (map (lambda (x) (check-rhs (rhs-pattern x)))
                                   (hash-table-get rhs-ht nt-sym)))])
                 (hash-table-put! ht nt-sym answer)
                 answer)]
              [(computing) conservative-answer]
              [else current])))
        (define (check-rhs rhs)
          (cond
            [(hash-table-maps? ht rhs)
             (check-nt rhs)]
            [else (test-rhs rhs check-rhs)]))
        (for-each (lambda (nt) (check-nt (nt-name nt)))
                  lang)
        ht)))
          
  ;; build-compatible-context-language : lang -> lang
  (define (build-compatible-context-language clang-ht lang)
    (apply
     append
     (map 
      (lambda (nt1)
        (map
         (lambda (nt2)
           (let ([compat-nt (build-compatible-contexts/nt clang-ht (nt-name nt1) nt2)])
             (if (eq? (nt-name nt1) (nt-name nt2))
                 (make-nt (nt-name compat-nt)
                          (cons
                           (make-rhs 'hole)
                           (nt-rhs compat-nt)))
                 compat-nt)))
         lang))
      lang)))
    
  ;; build-compatible-contexts : clang-ht prefix nt -> nt
  ;; constructs the compatible closure evaluation context from nt.
  (define (build-compatible-contexts/nt clang-ht prefix nt)
    (make-nt
     (symbol-append prefix '- (nt-name nt))
     (apply append
            (map
             (lambda (rhs)
               (let-values ([(maker count) (build-compatible-context-maker clang-ht
                                                                           (rhs-pattern rhs)
                                                                           prefix)])
                 (let loop ([i count])
                   (cond
                     [(zero? i) null]
                     [else (let ([nts (build-across-nts (nt-name nt) count (- i 1))])
                             (cons (make-rhs (maker (box nts)))
                                   (loop (- i 1))))]))))
             (nt-rhs nt)))))
    
  (define (symbol-append . args)
    (string->symbol (apply string-append (map symbol->string args))))
  
  ;; build-across-nts : symbol number number -> (listof pattern)
  (define (build-across-nts nt count i)
    (let loop ([j count])
      (cond
        [(zero? j) null]
        [else
         (cons (= i (- j 1)) 
               (loop (- j 1)))])))
    
  ;; build-compatible-context-maker : symbol pattern -> (values ((box (listof pattern)) -> pattern) number)
  ;; when the result function is applied, it takes each element
  ;; of the of the boxed list and plugs them into the places where
  ;; the nt corresponding from this rhs appeared in the original pattern. 
  ;; The number result is the number of times that the nt appeared in the pattern.
  (define (build-compatible-context-maker clang-ht pattern prefix)
    (let ([count 0])
      (values
       (let loop ([pattern pattern])
         (match pattern
           [`any (lambda (l) 'any)]
           [`number (lambda (l) 'number)]
           [`string (lambda (l) 'string)]
           [`variable (lambda (l) 'variable)] 
           [`(variable-except ,@(vars ...)) (lambda (l) pattern)]
           [`hole  (lambda (l) 'hole)]
           [`(hole ,(? symbol? hole-name)) (lambda (l) `(hole ,hole-name))]
           [(? string?) (lambda (l) pattern)]
           [(? symbol?) 
            (cond
              [(hash-table-get clang-ht pattern (lambda () #f))
               (set! count (+ count 1))
               (lambda (l)
                 (let ([fst (car (unbox l))])
                   (set-box! l (cdr (unbox l)))
                   (if fst
                       `(cross ,(symbol-append prefix '- pattern))
                       pattern)))]
              [else
               (lambda (l) pattern)])]
           [`(name ,name ,pat)
            (let ([patf (loop pat)])
              (lambda (l)
                `(name ,name ,(patf l))))]
           [`(in-hole ,context ,contractum)
            (let ([match-context (loop context)]
                  [match-contractum (loop contractum)])
              (lambda (l)
                `(in-hole ,(match-context l)
                          ,(match-contractum l))))]
           [`(in-named-hole ,hole-name ,context ,contractum)
            (let ([match-context (loop context)]
                  [match-contractum (loop contractum)])
              (lambda (l)
                `(in-named-hole ,hole-name
                                ,(match-context l)
                                ,(match-contractum l))))]
           [`(side-condition ,pat ,condition)
            (let ([patf (loop pat)])
              (lambda (l)
                `(side-condition ,(patf l) ,condition)))]
           [(? list?)
            (let ([fs (map loop pattern)])
              (lambda (l)
                (map (lambda (f) (f l)) fs)))]
           [else 
            (lambda (l) pattern)]))
       count)))
  
  ;; build-list-nt-label : lang -> hash-table[symbol -o> boolean]
  (define (build-list-nt-label lang)
    (build-nt-property 
     lang
     (lambda (pattern recur)
       (may-be-list-pattern?/internal pattern
                                      (lambda (sym) #f)
                                      recur))
     #t
     (lambda (lst) (ormap values lst))))
  
  (define (may-be-list-pattern? pattern list-nt-table)
    (let loop ([pattern pattern])
      (may-be-list-pattern?/internal
       pattern
       (lambda (sym)
         (hash-table-get list-nt-table
                         sym
                         (lambda () #f)))
       loop)))
  
  (define (may-be-list-pattern?/internal pattern handle-symbol recur)
    (match pattern
      [`any #t]
      [`number #f]
      [`string #f]
      [`variable #f] 
      [`(variable-except ,@(vars ...)) #f]
      [`hole  #t]
      [`(hole ,(? symbol? hole-name)) #t]
      [(? string?) #f]
      [(? symbol?) 
       (handle-symbol pattern)]
      [`(name ,name ,pat)
        (recur pat)]
      [`(in-hole ,context ,contractum)
        (recur context)]
      [`(in-named-hole ,hole-name ,context ,contractum)
        (recur context)]
      [`(side-condition ,pat ,condition)
        (recur pat)]
      [(? list?)
       #t]
      [else 
       ;; is this right?!
       (or (null? pattern) (pair? pattern))]))

  
  ;; build-non-list-nt-label : lang -> hash-table[symbol -o> boolean]
  (define (build-non-list-nt-label lang)
    (build-nt-property 
     lang
     (lambda (pattern recur)
       (may-be-non-list-pattern?/internal pattern
                                 (lambda (sym) #t)
                                 recur))
     #t
     (lambda (lst) (ormap values lst))))
  
  (define (may-be-non-list-pattern? pattern non-list-nt-table)
    (let loop ([pattern pattern])
      (may-be-non-list-pattern?/internal
       pattern
       (lambda (sym)
         (hash-table-get non-list-nt-table
                         sym
                         (lambda () #t)))
       loop)))
  
  (define (may-be-non-list-pattern?/internal pattern handle-sym recur)
    (match pattern
      [`any #t]
      [`number #t]
      [`string #t]
      [`variable #t] 
      [`(variable-except ,@(vars ...)) #t]
      [`hole #t]
      [`(hole ,(? symbol? hole-name)) #t]
      [(? string?) #t]
      [(? symbol?) (handle-sym pattern)]
      [`(name ,name ,pat)
        (recur pat)]
      [`(in-hole ,context ,contractum)
        (recur context)]
      [`(in-named-hole ,hole-name ,context ,contractum)
        (recur context)]
      [`(side-condition ,pat ,condition)
        (recur pat)]
      [(? list?)
       #f]
      [else 
       ;; is this right?!
       (not (or (null? pattern) (pair? pattern)))]))
  
  ;; match-pattern : compiled-pattern exp -> (union #f (listof bindings))
  (define (match-pattern compiled-pattern exp)
    (let ([results (compiled-pattern exp #f)])
      (and results
           (let ([filtered (filter-multiples results)])
             (and (not (null? filtered))
                  filtered)))))
  
  ;; filter-multiples : (listof mtch) -> (listof mtch)
  (define (filter-multiples matches)
    (let loop ([matches matches]
               [acc null])
      (cond
        [(null? matches) acc]
        [else
         (let ([merged (merge-multiples/remove (car matches))])
           (if merged
               (loop (cdr matches) (cons merged acc))
               (loop (cdr matches) acc)))])))
  
  ;; merge-multiples/remove : bindings -> (union #f bindings)
  ;; returns #f if all duplicate bindings don't bind the same thing
  ;; returns a new bindings 
  (define (merge-multiples/remove match)
    (let/ec fail
      (let ([ht (make-hash-table 'equal)]
            [ribs (bindings-table (mtch-bindings match))])
        (for-each
         (lambda (rib)
           (let/ec new
             (let ([name (rib-name rib)]
                   [exp (rib-exp rib)])
               (let ([previous-exp
                      (hash-table-get 
                       ht 
                       name
                       (lambda ()
                         (hash-table-put! ht name exp)
                         (new (void))))])
                 (unless (equal? exp previous-exp)
                   (fail #f))))))
         ribs)
        (make-mtch
         (make-bindings (hash-table-map ht make-rib))
         (mtch-context match)
         (mtch-hole match)))))
  
  (define underscore-allowed '(any number string variable))
  
  ;; compile-pattern : compiled-lang pattern -> compiled-pattern
  (define compile-pattern
    (opt-lambda (clang pattern)
      (let-values ([(pattern has-hole?) (compile-pattern/cross? clang pattern #t)])
        pattern)))
  
  ;; compile-pattern : compiled-lang pattern boolean -> (values compiled-pattern boolean)
  (define (compile-pattern/cross? clang pattern prefix-cross?)
    (define clang-ht (compiled-lang-ht clang))
    (define clang-list-ht (compiled-lang-list-ht clang))
    (define has-hole-ht (compiled-lang-has-hole-ht clang))
    (define across-ht (compiled-lang-across-ht clang))
    (define compiled-pattern-cache (compiled-lang-cache clang))
    
    (define (compile-pattern/cache pattern)
      (let ([compiled-cache (hash-table-get
                             compiled-pattern-cache
                             pattern
                             (lambda ()
                               (let-values ([(compiled-pattern has-hole?)
                                             (true-compile-pattern pattern)])
                                 (let ([val (cons (memoize compiled-pattern has-hole?) has-hole?)])
                                   (hash-table-put! compiled-pattern-cache pattern val)
                                   val))))])
        (values (car compiled-cache) (cdr compiled-cache))))
    
    ;; consult-compiled-pattern-cache : sexp[pattern] (-> compiled-pattern) -> compiled-pattern
    (define (consult-compiled-pattern-cache pattern calc)
      (hash-table-get 
       compiled-pattern-cache
       pattern
       (lambda ()
         (let ([res (calc)])
           (hash-table-put! compiled-pattern-cache pattern res)
           res))))
    
    (define (true-compile-pattern pattern)
      (match pattern
        [`any
          (values
           (lambda (exp hole-info) (list (make-mtch 
                                          (make-bindings null)
                                          (build-flat-context exp)
                                          none)))
           #f)]
        [`number 
          (values 
           (lambda (exp hole-info) (and (number? exp) (list (make-mtch
                                                             (make-bindings null)
                                                             (build-flat-context exp)
                                                             none))))
           #f)]
        [`string
          (values 
           (lambda (exp hole-info) (and (string? exp) (list (make-mtch
                                                             (make-bindings null)
                                                             (build-flat-context exp)
                                                             none))))
           #f)]
        [`variable 
          (values
           (lambda (exp hole-info)
             (and (symbol? exp) (list (make-mtch (make-bindings null) 
                                                 (build-flat-context exp)
                                                 none))))
           #f)]
        [`(variable-except ,@(vars ...))
          (values
           (lambda (exp hole-info)
             (and (symbol? exp)
                  (not (memq exp vars))
                  (list (make-mtch (make-bindings null)
                                   (build-flat-context exp)
                                   none))))
           #f)]
        [`hole (values (match-hole none) #t)]
        [`(hole ,hole-id) (values (match-hole hole-id) #t)]
        [(? string?) 
         (values
          (lambda (exp hole-info)
            (and (string? exp)
                 (string=? exp pattern)
                 (list (make-mtch (make-bindings null)
                                  (build-flat-context exp)
                                  none))))
          #f)]
        [(? symbol?)
         (cond
           [(hash-table-maps? clang-ht pattern)
            (values
             (lambda (exp hole-info)
               (match-nt (hash-table-get clang-list-ht pattern)
                         (hash-table-get clang-ht pattern)
                         pattern exp hole-info))
             (hash-table-get has-hole-ht pattern))]
           [(has-underscore? pattern)
            (let ([before (split-underscore pattern)])
              (unless (or (hash-table-maps? clang-ht before)
                          (memq before underscore-allowed))
                (error 'compile-pattern "before underscore must be either a non-terminal or a built-in pattern, found ~a in ~s" 
                       before
                       pattern))
              (compile-pattern/cache `(name ,pattern ,before)))]
           [else
            (values
             (lambda (exp hole-info) (and (eq? exp pattern) (list (make-mtch (make-bindings null)
                                                                             (build-flat-context exp)
                                                                             none))))
             #f)])]
        
        [`(cross ,(? symbol? pre-id))
          (let ([id (if prefix-cross?
                        (symbol-append pre-id '- pre-id)
                        pre-id)])
            (cond
              [(hash-table-maps? across-ht id)
               (values
                (lambda (exp hole-info)
                  (let ([rhs-list (hash-table-get across-ht id)])
                    (match-nt rhs-list rhs-list id exp hole-info)))
                #t)]
              [else
               (error 'compile-pattern "unknown cross reference ~a" id)]))]
        
        [`(name ,name ,pat)
          (let-values ([(match-pat has-hole?) (compile-pattern/cache pat)])
            (values
             (lambda (exp hole-info)
               (let ([matches (match-pat exp hole-info)])
                 (and matches 
                      (map (lambda (match)
                             (make-mtch
                              (make-bindings (cons (make-rib name (mtch-context match))
                                                   (bindings-table (mtch-bindings match))))
                              (mtch-context match)
                              (mtch-hole match)))
                           matches))))
             has-hole?))]
        [`(in-hole ,context ,contractum) 
          (let-values ([(match-context ctxt-has-hole?) (compile-pattern/cache context)]
                       [(match-contractum contractum-has-hole?) (compile-pattern/cache contractum)])
            (values
             (match-in-hole context contractum exp match-context match-contractum none)
             (or ctxt-has-hole? contractum-has-hole?)))]
        [`(in-named-hole ,hole-id ,context ,contractum) 
          (let-values ([(match-context ctxt-has-hole?) (compile-pattern/cache context)]
                       [(match-contractum contractum-has-hole?) (compile-pattern/cache contractum)])
            (values
             (match-in-hole context contractum exp match-context match-contractum hole-id)
             (or ctxt-has-hole? contractum-has-hole?)))]
        
        [`(side-condition ,pat ,condition)
          (let-values ([(match-pat has-hole?) (compile-pattern/cache pat)])
            (values
             (lambda (exp hole-info)
               (let ([matches (match-pat exp hole-info)])
                 (and matches
                      (let ([filtered (filter (λ (m) (condition (mtch-bindings m))) matches)])
                        (if (null? filtered)
                            #f
                            filtered)))))
             has-hole?))]
        [(? list?)
         (let-values ([(rewritten has-hole?) (rewrite-ellipses pattern compile-pattern/cache)])
           (values
            (lambda (exp hole-info)
              (match-list rewritten exp hole-info))
            has-hole?))]
        
        ;; an already comiled pattern
        [(? procedure?)
         ;; return #t here as a failsafe; no way to check better.
         (values pattern 
                 #t)]
        
        [else 
         (values
          (lambda (exp hole-info)
            (and (eqv? pattern exp)
                 (list (make-mtch (make-bindings null)
                                  (build-flat-context exp)
                                  none))))
          #f)]))
    
    (compile-pattern/cache pattern))
  
  ;; split-underscore : symbol -> symbol
  ;; returns the text before the underscore in a symbol (as a symbol)
  ;; raise an error if there is more than one underscore in the input
  (define (split-underscore sym)
    (string->symbol
     (list->string
      (let loop ([chars (string->list (symbol->string sym))])
        (cond
          [(null? chars) (error 'split-underscore "bad")]
          [else
           (let ([c (car chars)])
             (cond
               [(char=? c #\_)
                (when (memq #\_ (cdr chars))
                  (error 'compile-pattern "found a symbol with multiple underscores: ~s" sym))
                null]
               [else (cons c (loop (cdr chars)))]))])))))
  
  ;; has-underscore? : symbol -> boolean
  (define (has-underscore? sym)
    (memq #\_ (string->list (symbol->string sym))))
  
  (define (memoize f needs-all-args?)
    (if needs-all-args?
        (memoize2 f)
        (memoize1 f)))
  
  ; memoize1 : (x y -> w) -> x y -> w
  ; memoizes a function of two arguments under the assumption
  ; that the function is constant w.r.t the second
  (define (memoize1 f) (memoize/key f (lambda (x y) x) nohole))
  (define (memoize2 f) (memoize/key f cons w/hole))
  
  (define (memoize/key f key-fn statsbox)
    (let ([ht (make-hash-table 'equal)]
          [entries 0])
      (lambda (x y)
        (set-cache-stats-hits! statsbox (add1 (cache-stats-hits statsbox)))
        (let* ([key (key-fn x y)]
               [compute/cache
                (lambda ()
                  (set! entries (+ entries 1))
                  (set-cache-stats-hits! statsbox (sub1 (cache-stats-hits statsbox)))
                  (set-cache-stats-misses! statsbox (add1 (cache-stats-misses statsbox)))
                  (let ([res (f x y)])
                    (hash-table-put! ht key res)
                    res))])
          (unless (< entries 10000)
            (set! entries 0)
            (set! ht (make-hash-table 'equal)))
          (hash-table-get ht key compute/cache)))))
  
  (define-struct cache-stats (name misses hits))
  (define (new-cache-stats name) (make-cache-stats name 0 0))
  
  (define w/hole (new-cache-stats "hole"))
  (define nohole (new-cache-stats "no-hole"))
  
  (define (print-stats)
    (let ((stats (list w/hole nohole)))
      (for-each 
       (lambda (s) 
         (when (> (+ (cache-stats-hits s) (cache-stats-misses s)) 0)
           (printf "~a has ~a hits, ~a misses (~a% miss rate)\n" 
                   (cache-stats-name s)
                   (cache-stats-hits s)
                   (cache-stats-misses s)
                   (floor
                    (* 100 (/ (cache-stats-misses s)
                              (+ (cache-stats-hits s) (cache-stats-misses s))))))))
       stats)
      (let ((overall-hits (apply + (map cache-stats-hits stats)))
            (overall-miss (apply + (map cache-stats-misses stats))))
        (printf "---\nOverall hits: ~a\n" overall-hits)
        (printf "Overall misses: ~a\n" overall-miss)
        (when (> (+ overall-hits overall-miss) 0)
          (printf "Overall miss rate: ~a%\n" 
                  (floor (* 100 (/ overall-miss (+ overall-hits overall-miss)))))))))
  
  ;; match-hole : (union #f symbol) -> compiled-pattern
  (define (match-hole hole-id)
    (lambda (exp hole-info)
      (and hole-info
           (eq? hole-id hole-info)
           (list (make-mtch (make-bindings '())
                            hole
                            exp)))))
  
  ;; match-in-hole : sexp sexp sexp compiled-pattern compiled-pattern hole-info -> compiled-pattern
  (define (match-in-hole context contractum exp match-context match-contractum hole-info)
    (lambda (exp old-hole-info)
      (let ([mtches (match-context exp hole-info)])
        (and mtches
             (let loop ([mtches mtches]
                        [acc null])
               (cond
                 [(null? mtches) acc]
                 [else 
                  (let* ([mtch (car mtches)]
                         [bindings (mtch-bindings mtch)]
                         [hole-exp (mtch-hole mtch)]
                         [contractum-mtches (match-contractum hole-exp old-hole-info)])
                    (if contractum-mtches
                        (let i-loop ([contractum-mtches contractum-mtches]
                                     [acc acc])
                          (cond
                            [(null? contractum-mtches) (loop (cdr mtches) acc)]
                            [else (let* ([contractum-mtch (car contractum-mtches)]
                                         [contractum-bindings (mtch-bindings contractum-mtch)])
                                    (i-loop
                                     (cdr contractum-mtches)
                                     (cons
                                      (make-mtch (make-bindings
                                                  (append (bindings-table contractum-bindings)
                                                          (bindings-table bindings)))
                                                 (build-nested-context 
                                                  (mtch-context mtch)
                                                  (mtch-context contractum-mtch))
                                                 (mtch-hole contractum-mtch))
                                      acc)))]))
                        (loop (cdr mtches) acc)))]))))))
  
  ;; match-list : (listof (union repeat compiled-pattern)) sexp hole-info -> (union #f (listof bindings))
  (define (match-list patterns exp hole-info)
    (let (;; raw-match : (listof (listof (listof mtch)))
          [raw-match (match-list/raw patterns exp hole-info)])
      
      (and (not (null? raw-match))
           
           (let* (;; combined-matches : (listof (listof mtch))
                  ;; a list of complete possibilities for matches 
                  ;; (analagous to multiple matches of a single non-terminal)
                  [combined-matches (map combine-matches raw-match)]
                  
                  ;; flattened-matches : (union #f (listof bindings))
                  [flattened-matches (if (null? combined-matches)
                                         #f
                                         (apply append combined-matches))])
             flattened-matches))))
  
  ;; match-list/raw : (listof (union repeat compiled-pattern)) 
  ;;                  sexp
  ;;                  hole-info
  ;;               -> (listof (listof (listof mtch)))
  ;; the result is the raw accumulation of the matches for each subpattern, as follows:
  ;;  (listof (listof (listof mtch)))
  ;;  \       \       \-------------/  a match for one position in the list (failures don't show up)
  ;;   \       \-------------------/   one element for each position in the pattern list
  ;;    \-------------------------/    one element for different expansions of the ellipses
  ;; the failures to match are just removed from the outer list before this function finishes
  ;; via the `fail' argument to `loop'.
  (define (match-list/raw patterns exp hole-info)
    (let/ec k
      (let loop ([patterns patterns]
                 [exp exp]
                 ;; fail : -> alpha
                 ;; causes one possible expansion of ellipses to fail
                 ;; initially there is only one possible expansion, so
                 ;; everything fails.
                 [fail (lambda () (k null))])
        (cond
          [(pair? patterns)
           (let ([fst-pat (car patterns)])
             (cond
               [(repeat? fst-pat)
                (if (or (null? exp) (pair? exp))
                    (let ([r-pat (repeat-pat fst-pat)]
                          [r-mt (make-mtch (make-bindings (repeat-empty-bindings fst-pat))
                                           (build-flat-context '())
                                           none)])
                      (apply 
                       append
                       (cons (let/ec k
                               (let ([mt-fail (lambda () (k null))])
                                 (map (lambda (pat-ele) (cons (list r-mt) pat-ele))
                                      (loop (cdr patterns) exp mt-fail))))
                             (let r-loop ([exp exp]
                                          ;; past-matches is in reverse order
                                          ;; it gets reversed before put into final list
                                          [past-matches (list r-mt)])
                               (cond
                                 [(pair? exp)
                                  (let* ([fst (car exp)]
                                         [m (r-pat fst hole-info)])
                                    (if m
                                        (let* ([combined-matches (collapse-single-multiples m past-matches)]
                                               [reversed (reverse-multiples combined-matches)])
                                          (cons 
                                           (let/ec fail-k
                                             (map (lambda (x) (cons reversed x))
                                                  (loop (cdr patterns) 
                                                        (cdr exp)
                                                        (lambda () (fail-k null)))))
                                           (r-loop (cdr exp) combined-matches)))
                                        (list null)))]
                                 ;; what about dotted pairs?
                                 [else (list null)])))))
                    (fail))]
               [else
                (cond
                  [(pair? exp)
                   (let* ([fst-exp (car exp)]
                          [match (fst-pat fst-exp hole-info)])
                     (if match
                         (let ([exp-match (map (λ (mtch) (make-mtch (mtch-bindings mtch)
                                                                    (build-list-context (mtch-context mtch))
                                                                    (mtch-hole mtch)))
                                               match)])
                           (map (lambda (x) (cons exp-match x))
                                (loop (cdr patterns) (cdr exp) fail)))
                         (fail)))]
                  [else
                   (fail)])]))]
          [else
           (if (null? exp)
               (list null)
               (fail))]))))
  
  ;; collapse-single-multiples : (listof mtch) (listof mtch[to-lists]) -> (listof mtch[to-lists])
  (define (collapse-single-multiples bindingss multiple-bindingss)
    (apply append 
           (map
            (lambda (multiple-match)
              (let ([multiple-bindings (mtch-bindings multiple-match)])
                (map
                 (lambda (single-match)
                   (let ([single-bindings (mtch-bindings single-match)])
                     (let ([ht (make-hash-table 'equal)])
                       (for-each
                        (lambda (multiple-rib)
                          (hash-table-put! ht (rib-name multiple-rib) (rib-exp multiple-rib)))
                        (bindings-table multiple-bindings))
                       (for-each
                        (lambda (single-rib)
                          (let* ([key (rib-name single-rib)]
                                 [rst (hash-table-get ht key (lambda () null))])
                            (hash-table-put! ht key (cons (rib-exp single-rib) rst))))
                        (bindings-table single-bindings))
                       (make-mtch (make-bindings (hash-table-map ht make-rib))
                                  (build-cons-context
                                   (mtch-context single-match)
                                   (mtch-context multiple-match))
                                  (pick-hole (mtch-hole single-match)
                                             (mtch-hole multiple-match))))))
                 bindingss)))
            multiple-bindingss)))
  
  ;; pick-hole : (union none sexp) (union none sexp) -> (union none sexp)
  (define (pick-hole s1 s2)
    (cond
      [(eq? none s1) s2]
      [(eq? none s2) s1]
      [(error 'matcher.ss "found two holes in list pattern ~s ~s" s1 s2)]))
  
  ;; reverse-multiples : (listof mtch[to-lists]) -> (listof mtch[to-lists])
  ;; reverses the rhs of each rib in the bindings and reverses the context.
  (define (reverse-multiples matches)
    (map (lambda (match)
           (let ([bindings (mtch-bindings match)])
             (make-mtch
              (make-bindings
               (map (lambda (rib)
                      (make-rib (rib-name rib)
                                (reverse (rib-exp rib))))
                    (bindings-table bindings)))
              (reverse-context (mtch-context match))
              (mtch-hole match))))
         matches))
  
  ;; match-nt : (listof compiled-rhs) (listof compiled-rhs) sym exp hole-info
  ;;        -> (union #f (listof bindings))
  (define (match-nt list-rhs non-list-rhs nt term hole-info)
    (let loop ([rhss (if (or (null? term) (pair? term))
                         list-rhs
                         non-list-rhs)]
               [anss null])
      (cond
        [(null? rhss) (if (null? anss) #f (apply append anss))]
        [else
         (let ([mth (remove-bindings/filter ((car rhss) term hole-info))])
           (if mth
               (loop (cdr rhss) (cons mth anss))
               (loop (cdr rhss) anss)))])))
  
  ;; remove-bindings/filter : (union #f (listof mtch)) -> (union #f (listof mtch))
  (define (remove-bindings/filter matches)
    (and matches
         (let ([filtered (filter-multiples matches)])
           (and (not (null? filtered))
                (map (λ (match)
                       (make-mtch (make-bindings '())
                                  (mtch-context match)
                                  (mtch-hole match)))
                     matches)))))
  
  ;; rewrite-ellipses : (listof pattern) 
  ;;                    (pattern -> (values compiled-pattern boolean))
  ;;                 -> (values (listof (union repeat compiled-pattern)) boolean)
  ;; moves the ellipses out of the list and produces repeat structures
  (define (rewrite-ellipses pattern compile)
    (let loop ([exp-eles pattern]
               [fst dummy])
      (cond
        [(null? exp-eles)
         (if (eq? fst dummy)
             (values empty #f)
             (let-values ([(compiled has-hole?) (compile fst)])
               (values (list compiled) has-hole?)))]
        [else
         (let ([exp-ele (car exp-eles)])
           (cond
             [(eq? '... exp-ele)
              (when (eq? fst dummy)
                (error 'match-pattern "bad ellipses placement: ~s" pattern))
              (let-values ([(compiled has-hole?) (compile fst)]
                           [(rest rest-has-hole?) (loop (cdr exp-eles) dummy)])
                (values
                 (cons (make-repeat compiled (extract-empty-bindings fst)) rest)
                 (or has-hole? rest-has-hole?)))]
             [(eq? fst dummy)
              (loop (cdr exp-eles) exp-ele)]
             [else
              (let-values ([(compiled has-hole?) (compile fst)]
                           [(rest rest-has-hole?) (loop (cdr exp-eles) exp-ele)])
                (values
                 (cons compiled rest)
                 (or has-hole? rest-has-hole?)))]))])))
  
  (define dummy (box 0))
  
  ;; extract-empty-bindings : pattern -> (listof rib)
  (define (extract-empty-bindings pattern)
    (let loop ([pattern pattern]
               [ribs null])
      (match pattern
        [`any ribs]
        [`number ribs] 
        [`variable ribs] 
        [`(variable-except ,@(vars ...)) ribs]
        
        [`hole (error 'match-pattern "cannot have a hole inside an ellipses")]
        [(? symbol?) 
         (cond
           [(has-underscore? pattern)
            (let ([before (split-underscore pattern)])
              (loop `(name ,pattern ,before) ribs))]
           [else ribs])]
        [`(name ,name ,pat) (loop pat (cons (make-rib name '()) ribs))]
        [`(in-hole ,context ,contractum) (loop context (loop contractum ribs))]
        [`(in-named-hole ,hole-name ,context ,contractum) (loop context (loop contractum ribs))]
        [`(side-condition ,pat ,test) (loop pat ribs)]
        [(? list?)
         (let-values ([(rewritten has-hole?) (rewrite-ellipses pattern (lambda (x) (values x #f)))])
           (let i-loop ([r-exps rewritten]
                        [ribs ribs])
             (cond
               [(null? r-exps) ribs]
               [else (let ([r-exp (car r-exps)])
                       (cond
                         [(repeat? r-exp) 
                          (i-loop
                           (cdr r-exps)
                           (append (repeat-empty-bindings r-exp) ribs))]
                         [else
                          (i-loop 
                           (cdr r-exps)
                           (loop (car r-exps) ribs))]))])))]
        [else ribs])))
  
  ;; combine-matches : (listof (listof mtch)) -> (listof mtch)
  ;; input is the list of bindings corresonding to a piecewise match
  ;; of a list. produces all of the combinations of complete matches
  (define (combine-matches matchess)
    (let loop ([matchess matchess])
      (cond
        [(null? matchess) (list (make-mtch (make-bindings null) (build-flat-context '()) none))]
        [else (combine-pair (car matchess) (loop (cdr matchess)))])))
  
  ;; combine-pair : (listof mtch) (listof mtch) -> (listof mtch)
  (define (combine-pair fst snd)
    (let ([mtchs null])
      (for-each 
       (lambda (mtch1)
         (for-each
          (lambda (mtch2)
            (set! mtchs (cons (make-mtch 
                               (make-bindings (append (bindings-table (mtch-bindings mtch1))
                                                      (bindings-table (mtch-bindings mtch2))))
                               (build-append-context (mtch-context mtch1) (mtch-context mtch2))
                               (pick-hole (mtch-hole mtch1) 
                                          (mtch-hole mtch2)))
                              mtchs)))
          snd))
       fst)
      mtchs))
  
  (define (hash-table-maps? ht key)
    (let/ec k
      (hash-table-get ht key (lambda () (k #f)))
      #t))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; context adt
  ;;
  
  #|
  ;; This version of the ADT isn't right yet -- 
  ;; need to figure out what to do about (name ...) patterns.

  (define-values (struct:context make-context context? context-ref context-set!)
    (make-struct-type 'context #f 1 0 #f '() #f 0))
  (define hole values)
  (define (build-flat-context exp) (make-context (lambda (x) exp)))
  (define (build-cons-context c1 c2) (make-context (lambda (x) (cons (c1 x) (c2 x)))))
  (define (build-append-context l1 l2) (make-context (lambda (x) (append (l1 x) (l2 x)))))
  (define (build-list-context l) (make-context (lambda (x) (list (l x)))))
  (define (build-nested-context c1 c2) (make-context (lambda (x) (c1 (c2 x)))))
  (define (plug exp hole-stuff) (exp hole-stuff))
  (define (reverse-context c) (make-context (lambda (x) (reverse (c x)))))

|#
  (define (context? x) #t)
  (define hole
    (let ()
      (define-struct hole ())
      (make-hole)))
  
  (define (build-flat-context exp) exp)
  (define (build-cons-context e1 e2) (cons e1 e2))
  (define (build-append-context e1 e2) (append e1 e2))
  (define (build-list-context x) (list x))
  (define (reverse-context x) (reverse x))
  (define (build-nested-context c1 c2) (plug c1 c2))
  (define (plug exp hole-stuff)
    (let loop ([exp exp])
      (cond
        [(pair? exp) (cons (loop (car exp)) (loop (cdr exp)))]
        [(eq? exp hole) hole-stuff]
        [else exp])))
  
  ;;
  ;; end context adt
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (provide/contract
   (match-pattern (compiled-pattern any/c . -> . (union false/c (listof mtch?))))
   (compile-pattern (compiled-lang? any/c . -> . compiled-pattern))
   
   (make-bindings ((listof rib?) . -> . bindings?))
   (bindings-table (bindings? . -> . (listof rib?)))
   (bindings? (any/c . -> . boolean?))
   
   (mtch? (any/c . -> . boolean?))
   (make-mtch (bindings? any/c any/c . -> . mtch?))
   (mtch-bindings (mtch? . -> . bindings?))
   (mtch-context (mtch? . -> . any/c))
   (mtch-hole (mtch? . -> . (union none? any/c)))
   
   (make-rib (symbol? any/c . -> . rib?))
   (rib? (any/c . -> . boolean?))
   (rib-name (rib? . -> . symbol?))
   (rib-exp (rib? . -> . any/c))
   
   (print-stats (-> void?)))
  
  ;; for test suite
  (provide build-cons-context
           build-flat-context
           context?)
  
  (provide (struct nt (name rhs))
           (struct rhs (pattern))
           (struct compiled-lang (lang ht across-ht has-hole-ht cache))
           
           lookup-binding
           compile-language
           
           compiled-pattern
           
           plug
           none? none
           
           make-repeat
           
           hole
           
           rewrite-ellipses
           build-compatible-context-language))