#lang racket/base

(require "matcher.rkt"
         "struct.rkt"
         "term.rkt"
         "fresh.rkt"
         "loc-wrapper.rkt"
         "error.rkt"
         "judgment-form.rkt"
         "term-fn.rkt"
         "search.rkt"
         "lang-struct.rkt"
         (for-syntax "cycle-check.rkt"
                     setup/path-to-relative)
         racket/trace
         racket/contract
         racket/list
         racket/set
         data/union-find
         (rename-in racket/match (match match:)))

(require (for-syntax syntax/name
                     "loc-wrapper-ct.rkt"
                     "rewrite-side-conditions.rkt"
                     "term-fn.rkt"
                     "underscore-allowed.rkt"
                     syntax/boundmap
                     syntax/id-table
                     racket/base
                     racket/list
                     syntax/parse
                     syntax/parse/experimental/contract
                     syntax/name
                     racket/syntax))

(define (language-nts lang)
  (hash-map (compiled-lang-ht lang) (λ (x y) x)))

(define-for-syntax (term-matcher orig-stx make-matcher)
  (syntax-case orig-stx ()
    [(form-name lang [pattern rhs] ...)
     (begin
       (unless (identifier? #'lang)
         (raise-syntax-error (syntax-e #'form-name) "expected an identifier in the language position" orig-stx #'lang))
       (with-syntax ([((syncheck-expr side-conditions-rewritten (names ...) (names/ellipses ...)) ...)
                      (map (λ (x) (rewrite-side-conditions/check-errs #'lang (syntax-e #'form-name) #t x))
                           (syntax->list (syntax (pattern ...))))]
                     [(cp-x ...) (generate-temporaries #'(pattern ...))]
                     [make-matcher make-matcher])
         (with-syntax ([(mtch-procs ...)
                        (for/list ([names/ellipses (in-list (syntax->list #'((names/ellipses ...) ...)))]
                                   [names (in-list (syntax->list #'((names ...) ...)))]
                                   [rhs (in-list (syntax->list #'(rhs ...)))])
                          (with-syntax ([(names ...) names])
                            #`(λ (match)
                                #,(bind-pattern-names 
                                   #'form-name 
                                   names/ellipses
                                   #'((lookup-binding (mtch-bindings match) 'names) ...)
                                   rhs))))])
           #`(begin
               syncheck-expr ...
               (make-matcher
                'form-name lang 
                (list 'pattern ...)
                (list (compile-pattern lang `side-conditions-rewritten #t) ...)
                (list mtch-procs ...))))))]))

(define-syntax (term-match/single stx)
  (term-matcher stx #'term-match/single/proc))
(define-syntax (term-match stx)
  (term-matcher stx #'term-match/proc))

(define ((term-match/proc form-name lang ps cps rhss) term)
  (append-map
   (λ (cp rhs)
     (let ([matches (match-pattern cp term)])
       (if matches
           (map rhs matches)
           '())))
   cps rhss))

(define ((term-match/single/proc form-name lang ps0 cps rhss) term)
  (let loop ([ps ps0] [cps cps] [rhss rhss])
    (if (null? ps)
        (redex-error form-name 
                     (if (null? (cdr ps0))
                         (format "term ~s does not match pattern ~s" term (car ps0))
                         (format "no patterns matched ~s" term)))
        (let ([match (match-pattern (car cps) term)])
          (if match
              (begin
                (unless (null? (cdr match))
                  (redex-error
                   form-name
                   "pattern ~s matched term ~s multiple ways"
                   (car ps)
                   term))
                ((car rhss) (car match)))
              (loop (cdr ps) (cdr cps) (cdr rhss)))))))

(define-syntaxes (redex-let redex-let*)
  (let ()
    (define-syntax-class binding
      #:description "binding clause"
      (pattern (lhs:expr rhs:expr)))
    (define-syntax-class (bindings extract)
      #:description (if extract
                        "sequence of disjoint binding clauses"
                        "sequence of binding clauses")
      (pattern (b:binding ...)
               #:fail-when (and extract
                                (check-duplicate-identifier
                                 (apply append (map extract (syntax->list #'(b.lhs ...))))))
               "duplicate pattern variable"
               #:with (lhs ...) #'(b.lhs ...)
               #:with (rhs ...) #'(b.rhs ...)))
    
    (define (redex-let stx)
      (define-values (form-name nts)
        (syntax-case stx ()
          [(name lang . _) 
           (values (syntax-e #'name)
                   (language-id-nts #'lang (syntax-e #'name)))]))
      (define (pattern-variables pattern)
        (let-values ([(names _) (extract-names nts form-name #t pattern)])
          names))
      (syntax-parse stx
        [(name lang (~var bs (bindings pattern-variables)) body ...+)
         (with-syntax ([(t ...) (generate-temporaries #'bs)])
           #`(let ([t bs.rhs] ...)
               #,(nested-lets #'lang #'([bs.lhs t] ...) #'(body ...) #'name)))]))
    
    (define (redex-let* stx)
      (syntax-parse stx
        [(name lang (~var bs (bindings #f)) body ...+)
         (nested-lets #'lang #'bs #'(body ...) #'name)]))
    
    (define (nested-lets lang bindings bodies name)
      (syntax-case bindings ()
        [()
         #`(let () #,@bodies)]
        [([lhs rhs] . bindings)
         (with-syntax ([rest-lets (nested-lets lang #'bindings bodies name)])
           #`(#,(term-matcher #`(#,name #,lang [lhs rest-lets]) 
                              #'term-match/single/proc) 
              rhs))]))
    
    (values redex-let redex-let*)))

(define-syntax (compatible-closure stx)
  (syntax-case stx ()
    [(_ red lang nt)
     (identifier? (syntax nt))
     (with-syntax ([(syncheck-expr side-conditions-rewritten (names ...) (names/ellipses ...))
                    (rewrite-side-conditions/check-errs #'lang
                                                        'compatible-closure 
                                                        #t
                                                        (syntax (cross nt)))])
       (syntax (begin syncheck-expr
                      (do-context-closure red lang `side-conditions-rewritten 'compatible-closure))))]
    [(_ red lang nt)
     (raise-syntax-error 'compatible-closure "expected a non-terminal as last argument" stx (syntax nt))]))

(define-syntax (context-closure stx)
  (syntax-case stx ()
    [(_ red lang pattern)
     (with-syntax ([(syncheck-expr side-conditions-rewritten (names ...) (names/ellipses ...))
                    (rewrite-side-conditions/check-errs #'lang
                                                        'context-closure
                                                        #t
                                                        (syntax pattern))])
       (syntax
        (begin
          syncheck-expr
          (do-context-closure
           red
           lang
           `side-conditions-rewritten
           'context-closure))))]))

(define (do-context-closure red lang pat name)
  (unless (reduction-relation? red)
    (error name "expected <reduction-relation> as first argument, got ~e" red))
  (unless (compiled-lang? lang)
    (error name "expected <lang> as second argument, got ~e" lang))
  
  (let ([lang-nts (language-nts lang)])
    (for ([red-lang-nt (in-list (language-nts (reduction-relation-lang red)))])
      (unless (member red-lang-nt lang-nts)
        (error 
         name 
         "language argument does not contain a definition of the non-terminal ~a, needed by the reduction-relation"
         red-lang-nt))))
  
  (build-reduction-relation
   #f
   lang
   (map
    (λ (make-proc)
      (make-rewrite-proc
       (λ (lang)
         (define f (make-proc lang))
         (define cp (compile-pattern
                     lang
                     `(in-hole (name ctxt ,pat)
                               (name exp any))
                     #f))
         (λ (main-exp exp extend acc)
           (let loop ([ms (or (match-pattern cp exp) '())]
                      [acc acc])
             (cond
               [(null? ms) acc]
               [else
                (let* ([mtch (car ms)]
                       [bindings (mtch-bindings mtch)])
                  (loop (cdr ms)
                        (f main-exp
                           (lookup-binding bindings 'exp)
                           (λ (x) (extend (plug (lookup-binding bindings 'ctxt) x)))
                           acc)))]))))
       (rewrite-proc-name make-proc)
       (rewrite-proc-lhs make-proc)
       (rewrite-proc-lhs-src make-proc)
       (rewrite-proc-id make-proc)))
    (reduction-relation-make-procs red))
   (reduction-relation-rule-names red)
   (reduction-relation-lws red)
   (let ([orig-pat (reduction-relation-domain-pat red)])
     (cond
       [(equal? orig-pat `any)
        ;; special case for backwards compatibility:
        ;; if there was no #:domain argument, then we
        ;; probably should let the compatible closure also
        ;; not have a domain
        `any]
       [else
        `(in-hole ,pat ,orig-pat)]))))

(define (apply-reduction-relation/tagged p v)
  (let loop ([procs (reduction-relation-procs p)]
             [acc '()])
    (cond
      [(null? procs) acc]
      [else 
       (loop (cdr procs)
             ((car procs) v acc))])))

(define (apply-reduction-relation/tag-with-names p v) (map cdr (apply-reduction-relation/tagged p v)))
(define (apply-reduction-relation p v) (map caddr (apply-reduction-relation/tagged p v)))

(define-syntax (-reduction-relation stx)
  (syntax-case stx ()
    [(_ lang args ...)
     (with-syntax ([orig-stx stx])
       (syntax/loc stx (do-reduction-relation orig-stx reduction-relation empty-reduction-relation #f lang args ...)))]))

(define-syntax (extend-reduction-relation stx)
  (syntax-case stx ()
    [(_ orig-reduction-relation lang args ...)
     (with-syntax ([orig-stx stx])
       (syntax/loc stx (do-reduction-relation orig-stx extend-reduction-relation orig-reduction-relation #t lang args ...)))]))

(define-for-syntax (generate-binding-constraints names names/ellipses bindings syn-err-name)
  (define (id/depth stx)
    (syntax-case stx ()
      [(s (... ...))
       (let ([r (id/depth #'s)])
         (make-id/depth (id/depth-id r) (add1 (id/depth-depth r))))]
      [s (make-id/depth #'s 0)]))
  (define temporaries (generate-temporaries names))
  (values
   (for/fold ([cs '()])
     ([n names]
      [w/e names/ellipses]
      [x temporaries])
     (cond [(hash-ref bindings (syntax-e n) #f)
            => (λ (b) 
                 (let ([b-id/depth (id/depth b)]
                       [n-id/depth (id/depth w/e)])
                   (if (= (id/depth-depth b-id/depth) (id/depth-depth n-id/depth))
                       (cons #`(equal? #,x (term #,b)) cs)
                       (raise-ellipsis-depth-error
                        syn-err-name
                        (id/depth-id n-id/depth) (id/depth-depth n-id/depth)
                        (id/depth-id b-id/depth) (id/depth-depth b-id/depth)))))]
           [else cs]))
   temporaries
   (for/fold ([extended bindings])
     ([name names] 
      [w/ellipses names/ellipses])
     (hash-set extended (syntax-e name) w/ellipses))))

(define-for-syntax (check-judgment-arity stx judgment)
  (syntax-case judgment ()
    [(form-name pat ...)
     (judgment-form-id? #'form-name)
     (let ([expected (length (judgment-form-mode (lookup-judgment-form-id #'form-name)))]
           [actual (length (syntax->list #'(pat ...)))])
       (unless (= actual expected)
         (raise-syntax-error 
          #f 
          (format "mode specifies a ~a-ary relation but use supplied ~a term~a" 
                  expected actual (if (= actual 1) "" "s"))
          judgment)))]
    [(form-name pat ...)
     (raise-syntax-error #f "expected a judgment form name" stx #'form-name)]))

(define-syntax (do-reduction-relation stx)
  (define (do-reduction-relation/proc stx)
    (syntax-case stx ()
      [(_ orig-stx id orig-reduction-relation allow-zero-rules? lang . w/domain-args)
       (identifier? #'lang)
       (prune-syntax
        (let-values ([(domain-pattern main-arrow args)
                      (parse-keywords #'orig-stx #'id #'w/domain-args)])
          (with-syntax ([(rules ...) (before-with args)]
                        [(shortcuts ...) (after-with args)])
            (with-syntax ([(lws ...) (map rule->lws (syntax->list #'(rules ...)))])
              (reduction-relation/helper 
               #'orig-stx
               (syntax-e #'id)
               #'orig-reduction-relation
               (syntax lang)
               (syntax->list (syntax (rules ...)))
               (syntax->list (syntax (shortcuts ...)))
               #'(list lws ...)
               (syntax-e #'allow-zero-rules?)
               domain-pattern
               main-arrow)))))]
      [(_ orig-stx id orig-reduction-relation allow-zero-rules? lang args ...)
       (raise-syntax-error (syntax-e #'id) 
                           "expected an identifier for the language name"
                           #'lang)]))
  
  (define default-arrow #'-->)
  
  (define (parse-keywords stx id args)
    (let ([domain-contract #'any]
          [main-arrow default-arrow])
      
      ;; ensure no duplicate keywords
      (let ([ht (make-hash)]
            [known-keywords '(#:arrow #:domain)])  ;; #:arrow not yet implemented
        (for-each (λ (kwd/stx)  ;; (not necc a keyword)
                    (let ([kwd (syntax-e kwd/stx)])
                      (when (keyword? kwd)
                        (unless (member kwd known-keywords)
                          (raise-syntax-error (syntax-e id)
                                              "unknown keyword"
                                              stx
                                              kwd/stx))
                        (when (hash-ref ht kwd #f)
                          (raise-syntax-error (syntax-e id)
                                              "duplicate keywords"
                                              stx
                                              kwd/stx
                                              (list (hash-ref ht kwd))))
                        (hash-set! ht kwd kwd/stx))))
                  (syntax->list args)))
      
      (let loop ([args args])
        (syntax-case args ()
          [(#:domain pat args ...)
           (begin (set! domain-contract #'pat)
                  (loop #'(args ...)))]
          [(#:domain)
           (raise-syntax-error (syntax-e id) 
                               "expected a domain after #:domain"
                               stx)]
          [(#:arrow arrow . args)
           (identifier? #'arrow)
           (begin (set! main-arrow #'arrow)
                  (loop #'args))]
          [(#:arrow arrow . args)
           (raise-syntax-error (syntax-e id) 
                               "expected an arrow after #:arrow, not a compound expression"
                               stx
                               #'arrow)]
          [(#:arrow)
           (raise-syntax-error (syntax-e id) 
                               "expected an arrow after #:arrow"
                               stx)]
          [_
           (begin
             (values domain-contract main-arrow args))]))))

  
  (define (before-with stx)
    (let loop ([lst (syntax->list stx)])
      (cond
        [(null? lst) null]
        [else
         (let ([fst (car lst)])
           (syntax-case (car lst) (with)
             [with null]
             [else (cons (car lst) (loop (cdr lst)))]))])))
  
  (define (after-with stx)
    (let loop ([lst (syntax->list stx)])
      (cond
        [(null? lst) null]
        [else
         (let ([fst (car lst)])
           (syntax-case (car lst) (with)
             [with (cdr lst)]
             [else (loop (cdr lst))]))])))
  
  (define (name-pattern-lws/rr pat)
    (for/list ([lw-pair (name-pattern-lws pat)])
      (if (pair? lw-pair)
          #`(cons #,(car lw-pair) #,(cdr lw-pair))
          (error 'name-pattern-lws/rr "ack"))))
  
  (define (rule->lws rule)
    (syntax-case rule ()
      [(arrow lhs rhs stuff ...)
       (let-values ([(label computed-label scs/withs fvars)
                     (let loop ([stuffs (syntax->list #'(stuff ...))]
                                [label #f]
                                [computed-label #f]
                                [scs/withs null]
                                [fvars null])
                       (cond
                         [(null? stuffs) (values label computed-label (reverse scs/withs) (reverse fvars))]
                         [else
                          (syntax-case (car stuffs) (where where/hidden
                                                           side-condition side-condition/hidden
                                                           fresh variable-not-in
                                                           computed-name
                                                           judgment-holds)
                            [(fresh xs ...) 
                             (loop (cdr stuffs)
                                   label
                                   computed-label
                                   scs/withs
                                   (append 
                                    (reverse (map (λ (x)
                                                    (to-lw/proc
                                                     (syntax-case x ()
                                                       [x
                                                        (identifier? #'x)
                                                        #'x]
                                                       [(x whatever)
                                                        (identifier? #'x)
                                                        #'x]
                                                       [((y dots) (x dots2))
                                                        (datum->syntax 
                                                         #f 
                                                         `(,(syntax->datum #'y) ...) 
                                                         #'y)]
                                                       [((y dots) (x dots2) whatever)
                                                        (datum->syntax 
                                                         #f 
                                                         `(,(syntax->datum #'y) ...) 
                                                         #'y)])))
                                                  (syntax->list #'(xs ...))))
                                    fvars))]
                            [(where x e)
                             (loop (cdr stuffs)
                                   label
                                   computed-label
                                   (cons #`(cons #,(to-lw/proc #'x) #,(to-lw/proc #'e))
                                         (append (name-pattern-lws/rr #'x) scs/withs))
                                   fvars)]
                            [(where/hidden x e)
                             (loop (cdr stuffs) label computed-label scs/withs fvars)]
                            [(side-condition sc)
                             (loop (cdr stuffs)
                                   label
                                   computed-label
                                   (cons (to-lw/uq/proc #'sc) scs/withs)
                                   fvars)]
                            [(side-condition/hidden sc)
                             (loop (cdr stuffs) label computed-label scs/withs fvars)]
                            [x
                             (identifier? #'x)
                             (loop (cdr stuffs)
                                   #''x
                                   computed-label
                                   scs/withs
                                   fvars)]
                            [x
                             (string? (syntax-e #'x))
                             (loop (cdr stuffs)
                                   #'(string->symbol x)
                                   computed-label
                                   scs/withs
                                   fvars)]
                            [(computed-name e)
                             (loop (cdr stuffs)
                                   label
                                   #'e
                                   scs/withs
                                   fvars)]
                            [(judgment-holds (form-name . pieces))
                             (judgment-form-id? #'form-name)
                             (loop (cdr stuffs)
                                   label
                                   computed-label
                                   (let*-values ([(mode) (judgment-form-mode (lookup-judgment-form-id #'form-name))]
                                                 [(_ outs) (split-by-mode (syntax->list #'pieces) mode)])
                                     (cons (to-lw/proc #'(form-name . pieces))
                                           (for/fold ([binds scs/withs]) ([out outs])
                                             (append (name-pattern-lws/rr out) binds))))
                                   fvars)]
                            [_
                             ;; just skip over junk here, and expect a syntax error to be raised elsewhere
                             (loop (cdr stuffs) label computed-label scs/withs fvars)])]))])
         (with-syntax ([(scs/withs ...) scs/withs]
                       [(fvars ...) fvars]
                       [((bind-id . bind-pat) ...) 
                        (extract-pattern-binds #'lhs)]
                       [((tl-id . tl-pat) ...)
                       (extract-term-let-binds #'rhs)])
           #`(make-rule-pict 'arrow
                             #,(to-lw/proc #'lhs)
                             #,(to-lw/proc #'rhs)
                             #,label
                             #,(and computed-label 
                                    (to-lw/proc #`,#,computed-label))
                             (list scs/withs ...
                                   #,@(map (λ (bind-id bind-pat)
                                             #`(cons #,(to-lw/proc bind-id)
                                                     #,(to-lw/proc bind-pat)))
                                           (syntax->list #'(bind-id ...))
                                           (syntax->list #'(bind-pat ...)))
                                   #,@(map (λ (tl-id tl-pat)
                                             #`(cons #,(to-lw/proc tl-id)
                                                     #,(to-lw/uq/proc tl-pat)))
                                           (syntax->list #'(tl-id ...))
                                           (syntax->list #'(tl-pat ...))))
                             (list fvars ...))))]
      ;; just skip over junk here, since syntax error checks elsewhere will catch this
      [_ #f]))
  
  (define (reduction-relation/helper stx orig-name orig-red-expr lang-id rules shortcuts 
                                     lws 
                                     allow-zero-rules?
                                     domain-pattern
                                     main-arrow)
    (let ([ht (make-module-identifier-mapping)]
          [all-top-levels '()]
          [withs (make-module-identifier-mapping)])
      (for-each (λ (shortcut)
                  (syntax-case shortcut ()
                    [((rhs-arrow rhs-from rhs-to)
                      (lhs-arrow a b))
                     (not (identifier? #'a))
                     (raise-syntax-error
                      orig-name
                      "malformed shortcut, expected identifier"
                      shortcut #'a)]
                    [((rhs-arrow rhs-from rhs-to)
                      (lhs-arrow a b))
                     (not (identifier? #'b))
                     (raise-syntax-error
                      orig-name
                      "malformed shortcut, expected identifier"
                      shortcut #'b)]
                    [((rhs-arrow rhs-from rhs-to)
                      (lhs-arrow lhs-from lhs-to))
                     (begin
                       (table-cons! withs #'lhs-arrow #'rhs-arrow)
                       (table-cons! ht (syntax rhs-arrow) shortcut))]
                    [((a b c) d)
                     (raise-syntax-error 
                      orig-name
                      "malformed shortcut, expected right-hand side to have three sub-expressions"
                      stx (syntax d))]
                    [(a b)
                     (raise-syntax-error 
                      orig-name
                      "malformed shortcut, expected left-hand side to have three sub-expressions"
                      stx (syntax a))]
                    [(a b c d ...)
                     (raise-syntax-error 
                      orig-name
                      "malformed shortcut, expected only two subparts for a shortcut definition, found an extra one"
                      stx
                      (syntax c))]
                    [_ (raise-syntax-error orig-name
                                           "malformed shortcut"
                                           stx shortcut)]))
                shortcuts)
      (for ([rule (in-list rules)]) 
        (syntax-case rule ()
          [(arrow . rst)
           (begin
             (unless (identifier? #'arrow)
               (raise-syntax-error orig-name "expected a reduction relation arrow" stx #'arrow))
             (set! all-top-levels (cons #'arrow all-top-levels))
             (table-cons! ht (syntax arrow) rule))]))
        
        ;; signal a syntax error if there are shortcuts defined, but no rules that use them
        (unless (null? shortcuts)
          (unless (module-identifier-mapping-get ht main-arrow (λ () #f))
            (raise-syntax-error orig-name 
                                (format "no ~a rules" (syntax-e main-arrow))
                                stx)))
        
        (for-each (λ (tl)
                    (let loop ([id tl])
                      (unless (free-identifier=? main-arrow id)
                        (let ([nexts
                               (module-identifier-mapping-get
                                withs id 
                                (λ () 
                                  (raise-syntax-error 
                                   orig-name
                                   (format "the ~s relation is not defined"
                                           (syntax->datum id))
                                   stx
                                   id)))])
                          (for-each loop nexts)))))
                  all-top-levels)
        
        (let ([name-table (make-hasheq)])
          (hash-set! name-table #f 0)
          ;; name table maps symbols for the rule names to their syntax objects and to a counter indicating what
          ;; order the names were encountered in. The current value of the counter is stored in the table at key '#f'.
          (with-syntax ([lang-id lang-id]
                        [(top-level ...) (get-choices stx orig-name ht lang-id main-arrow
                                                      name-table lang-id allow-zero-rules?)]
                        [(rule-names ...) 
                         (begin
                           (hash-remove! name-table #f)
                           (map car (sort (hash-map name-table (λ (k v) (list k (list-ref v 1)))) < #:key cadr)))]
                        [lws lws]
                        
                        [(domain-syncheck-expr domain-pattern-side-conditions-rewritten (names ...) (names/ellipses ...))
                         (rewrite-side-conditions/check-errs
                          lang-id
                          orig-name
                          #f
                          domain-pattern)])
            #`(begin
                domain-syncheck-expr
                (build-reduction-relation
                 #,orig-red-expr
                 lang-id
                 (list top-level ...)
                 '(rule-names ...)
                 lws
                 `domain-pattern-side-conditions-rewritten))))))
    
    #|    
    ;; relation-tree = 
    ;;   leaf
    ;;  (make-node id[frm] pat[frm] id[to] pat[to] (listof relation-tree))
    (define-struct node (frm-id frm-pat to-id to-pat))
    (define-struct leaf (frm-pat to-pat))
  |#  
    ;; get-choices : stx[original-syntax-object] bm lang identifier ht[sym->syntax] identifier[language-name] -> (listof relation-tree)
    (define (get-choices stx orig-name bm lang id name-table lang-id allow-zero-rules?)
      (reverse
       (apply 
        append
        (map (λ (x) (get-tree stx orig-name bm lang x name-table lang-id allow-zero-rules?))
             (module-identifier-mapping-get 
              bm id
              (λ ()
                (if allow-zero-rules?
                    '()
                    (raise-syntax-error orig-name 
                                        (format "no rules use ~a" (syntax->datum id))
                                        stx 
                                        (if (equal? id default-arrow) #f id)))))))))
    
    (define (get-tree stx orig-name bm lang case-stx name-table lang-id allow-zero-rules?)
      (syntax-case case-stx ()
        [(arrow from to extras ...)
         (list (do-leaf stx 
                        orig-name
                        lang 
                        name-table
                        (syntax from) 
                        (syntax to) 
                        (syntax->list (syntax (extras ...)))
                        lang-id))]
        [((rhs-arrow rhs-from rhs-to) (lhs-arrow lhs-frm-id lhs-to-id))
         (let ([lang-nts (language-id-nts lang-id 'reduction-relation)]
               [rewrite-side-conds
                (λ (pat) (rewrite-side-conditions/check-errs lang-id orig-name #t pat))])
           (with-syntax ([(lhs-syncheck-expr side-conditions-rewritten (names ...) (names/ellipses ...))
                          (rewrite-side-conds
                           (rewrite-node-pat (syntax-e (syntax lhs-frm-id))
                                             (syntax rhs-from)))]
                         [(rhs-syncheck-expr fresh-rhs-from (fresh-names ...) (fresh-names/ellipses ...)) 
                          (rewrite-side-conds 
                           (freshen-names #'rhs-from #'lhs-frm-id lang-nts orig-name))]
                         [lang lang])
             (map
              (λ (child-proc)
                #`(begin
                    lhs-syncheck-expr
                    rhs-syncheck-expr
                    (do-node-match
                     'lhs-frm-id
                     'lhs-to-id
                     `side-conditions-rewritten
                     (λ (bindings rhs-binder) 
                       (term-let ([lhs-to-id rhs-binder])
                                 #,(bind-pattern-names 'reduction-relation
                                                       #'(names/ellipses ...)
                                                       #'((lookup-binding bindings 'names) ...)
                                                       #'(term rhs-to #:lang lang))))
                     #,child-proc
                     `fresh-rhs-from)))
              (get-choices stx orig-name bm #'lang
                           (syntax lhs-arrow) 
                           name-table lang-id 
                           allow-zero-rules?))))]))
    (define (rewrite-node-pat id term)
      (let loop ([t term])
        (syntax-case t (side-condition)
          [(side-condition p c)
           #`(side-condition #,(loop #'p) c)]
          [(p ...)
           (map loop (syntax->list #'(p ...)))]
          [else 
           (if (and (identifier? t) (eq? id (syntax-e t)))
               `(name ,id any)
               t)])))
    
    (define (freshen-names pat hole-id nts what)
      (define (fresh x)
        (gensym
         (if (or (memq x nts) (memq x underscore-allowed))
             (string-append (symbol->string x) "_")
             x)))
      (let-values ([(bound _) (extract-names nts what #t pat 'binds-anywhere)])
        (let ([renames (make-bound-identifier-mapping)])
          (for-each 
           (λ (x)
             (unless (bound-identifier=? x hole-id)
               (bound-identifier-mapping-put! renames x (fresh (syntax-e x)))))
           bound)
          (let recur ([p pat])
            (syntax-case p (side-condition)
              [(side-condition p c)
               #`(side-condition 
                  #,(recur #'p)
                  (term-let (#,@(bound-identifier-mapping-map renames (λ (x y) #`(#,x (term #,y)))))
                            c))]
              [(p ...)
               #`(#,@(map recur (syntax->list #'(p ...))))]
              [else
               (if (identifier? p)
                   (bound-identifier-mapping-get renames p (λ () p))
                   p)])))))
    
    (define (do-leaf stx orig-name lang name-table from to extras lang-id)
      (define lang-nts (language-id-nts lang-id 'reduction-relation))
      (define (rw-sc pat) (rewrite-side-conditions/check-errs lang-id orig-name #t pat))
      (define-values (name computed-name sides/withs/freshs) (process-extras stx orig-name name-table extras))
      (define rt-lang-id (car (generate-temporaries (list lang))))
      (with-syntax ([(from-syncheck-expr side-conditions-rewritten (names ...) (names/ellipses ...)) (rw-sc from)])
        (define body-code
          (bind-withs orig-name 
                      #'main-exp
                      rt-lang-id
                      lang-nts
                      lang-id
                      sides/withs/freshs
                      'flatten
                      #`(begin
                          from-syncheck-expr
                          (list (cons #,(or computed-name #'none)
                                      (term #,to #:lang #,lang))))
                      (syntax->list #'(names ...))
                      (syntax->list #'(names/ellipses ...))
                      #t
                      #f))
        (define test-case-body-code
          ;; this contains some redundant code
          (bind-withs orig-name
                      #'#t 
                      #'lang-id2
                      lang-nts
                      lang-id
                      sides/withs/freshs
                      'predicate
                      #'#t
                      (syntax->list #'(names ...))
                      (syntax->list #'(names/ellipses ...))
                      #t
                      #f))
        (with-syntax ([(lhs-syncheck-expr lhs-w/extras (w/extras-names ...) (w/extras-names/ellipses ...))
                       (rw-sc #`(side-condition #,from #,test-case-body-code))]
                      [lhs-source (format "~a:~a:~a"
                                          (and (path? (syntax-source from))
                                               (path->relative-string/library (syntax-source from)))
                                          (syntax-line from)
                                          (syntax-column from))]
                      [name name]
                      [lang lang]
                      [body-code body-code])
          #`(begin
              lhs-syncheck-expr
              (build-rewrite-proc/leaf 
               `side-conditions-rewritten
               (λ (#,rt-lang-id)
                 (λ (main-exp bindings)
                   #,(bind-pattern-names 'reduction-relation
                                         #'(names/ellipses ...)
                                         #'((lookup-binding bindings 'names) ...)
                                         #'body-code)))
               lhs-source
               name
               (λ (lang-id2) `lhs-w/extras))))))
    
    (define (process-extras stx orig-name name-table extras)
      (let* ([the-name #f]
             [the-name-stx #f]
             [computed-name-stx #f]
             [sides/withs/freshs 
              (let loop ([extras extras])
                (cond
                  [(null? extras) '()]
                  [else
                   (syntax-case (car extras) (fresh computed-name judgment-holds)
                     [name 
                      (or (identifier? (car extras))
                          (string? (syntax-e (car extras))))
                      (begin
                        (let* ([raw-name (syntax-e (car extras))]
                               [name-sym
                                (if (symbol? raw-name)
                                    raw-name
                                    (string->symbol raw-name))])
                          (when (hash-ref name-table name-sym #f)
                            (raise-syntax-errors orig-name 
                                                 "same name on multiple rules"
                                                 stx
                                                 (list (car (hash-ref name-table name-sym))
                                                       (syntax name))))
                          (let ([num (hash-ref name-table #f)])
                            (hash-set! name-table #f (+ num 1))
                            (hash-set! name-table name-sym (list (syntax name) num)))
                          
                          (when the-name
                            (raise-syntax-errors orig-name
                                                 "expected only a single name" 
                                                 stx
                                                 (list the-name-stx (car extras))))
                          (set! the-name (if (symbol? raw-name)
                                             (symbol->string raw-name)
                                             raw-name))
                          (set! the-name-stx (car extras))
                          (loop (cdr extras))))]
                     [(fresh var ...)
                      (append (map (λ (x)
                                     (syntax-case x ()
                                       [x
                                        (identifier? #'x)
                                        #'(fresh x)]
                                       [(x name)
                                        (identifier? #'x)
                                        #'(fresh x name)]
                                       [((ys dots2) (xs dots1))
                                        (and (eq? (syntax-e #'dots1) (string->symbol "..."))
                                             (eq? (syntax-e #'dots2) (string->symbol "...")))
                                        #'(fresh (ys) (xs dots1))]
                                       [((ys dots2) (xs dots1) names)
                                        (and (eq? (syntax-e #'dots1) (string->symbol "..."))
                                             (eq? (syntax-e #'dots2) (string->symbol "...")))
                                        #'(fresh (ys) (xs dots1) names)]
                                       [x
                                        (raise-syntax-error orig-name 
                                                            "malformed fresh variable clause"
                                                            stx
                                                            #'x)]))
                                   (syntax->list #'(var ...)))
                              (loop (cdr extras)))]
                     [(-side-condition exp ...)
                      (side-condition-keyword? #'-side-condition)
                      (cons (car extras) (loop (cdr extras)))]
                     [(-where x e)
                      (where-keyword? #'-where)
                      (cons (car extras) (loop (cdr extras)))]
                     [(-where . x)
                      (where-keyword? #'-where)
                      (raise-syntax-error orig-name "malformed where clause" stx (car extras))]
                     [(computed-name e)
                      (if computed-name-stx
                          (raise-syntax-errors orig-name "expected at most one computed-name clause"
                                               stx (list computed-name-stx #'e))
                          (set! computed-name-stx #'e))
                      (loop (cdr extras))]
                     [(computed-name . _)
                      (raise-syntax-error orig-name "malformed computed-name clause" stx (car extras))]
                     [(judgment-holds judgment)
                      (begin
                        (check-judgment-arity stx #'judgment)
                        (cons #'judgment (loop (cdr extras))))]
                     [_
                      (raise-syntax-error orig-name "unknown extra" stx (car extras))])]))])
        (values the-name computed-name-stx sides/withs/freshs)))
    
    ;; table-cons! hash-table sym any -> void
    ;; extends ht at key by `cons'ing hd onto whatever is alrady bound to key (or the empty list, if nothing is)
    (define (table-cons! ht key hd)
      (module-identifier-mapping-put! ht key (cons hd (module-identifier-mapping-get ht key (λ () '())))))
    
    (define (raise-syntax-errors sym str stx stxs)
      (raise (make-exn:fail:syntax 
              (string->immutable-string (format "~a: ~a~a" 
                                                sym 
                                                str
                                                (if (error-print-source-location)
                                                    (string-append ":" (stxs->list stxs))
                                                    "")))
              (current-continuation-marks)
              stxs)))
    
    (define (stxs->list stxs)
      (apply
       string-append
       (let loop ([stxs stxs])
         (cond
           [(null? stxs) '()]
           [else 
            (cons (format " ~s" (syntax->datum (car stxs)))
                  (loop (cdr stxs)))]))))
  
  (do-reduction-relation/proc stx))

(define (build-rewrite-proc/leaf side-conditions-rewritten 
                                 build-really-matched/lang-arg
                                 lhs-source
                                 name
                                 lhs-w/extras-proc)
  (let ([case-id (gensym)])
    (make-rewrite-proc
     (λ (lang-id)
       (define build-really-matched (build-really-matched/lang-arg lang-id))
       (let ([cp (compile-pattern lang-id side-conditions-rewritten #t)])
         (λ (main-exp exp f other-matches)
           (let ([mtchs (match-pattern cp exp)])
             (if mtchs
                 (let loop ([mtchs mtchs]
                            [acc other-matches])
                   (cond
                     [(null? mtchs) acc]
                     [else 
                      (let* ([mtch (car mtchs)]
                             [bindings (mtch-bindings mtch)]
                             [really-matched (build-really-matched main-exp bindings)])
                        (cond
                          [really-matched
                           (for-each
                            (λ (c)
                              (let ([r (coverage-relation c)])
                                (when (and (reduction-relation? r)
                                           (memf (λ (r) (eq? case-id (rewrite-proc-id r)))
                                                 (reduction-relation-make-procs r)))
                                  (cover-case case-id c))))
                            (relation-coverage))
                           (loop (cdr mtchs) 
                                 (map/mt (λ (x) (list name
                                                      (if (none? (car x)) 
                                                          name
                                                          (format "~a" (car x)))
                                                      (f (cdr x)))) 
                                         really-matched acc))]
                          [else 
                           (loop (cdr mtchs) acc)]))]))
                 other-matches)))))
     name
     lhs-w/extras-proc
     lhs-source
     case-id)))

(define (union-reduction-relations fst snd . rst)
  (let ([name-ht (make-hasheq)]
        [counter 0]
        [lst (list* fst snd rst)]
        [first-lang (reduction-relation-lang fst)])
    (for-each
     (λ (red)
       (unless (eq? first-lang (reduction-relation-lang red))
         (error 'union-reduction-relations 
                "expected all of the reduction relations to use the same language"))
       (for-each (λ (name)
                   (when (hash-ref name-ht name #f)
                     (error 'union-reduction-relations "multiple rules with the name ~s" name))
                   (hash-set! name-ht name counter)
                   (set! counter (+ counter 1)))
                 (reduction-relation-rule-names red)))
     (reverse lst)) ;; reverse here so the names get put into the hash in the proper (backwards) order
    (make-reduction-relation
     first-lang
     (reverse (apply append (map reduction-relation-make-procs lst)))
     (map car (sort (hash-map name-ht list) < #:key cadr))
     (apply append (map reduction-relation-lws lst)) 
     (reverse (apply append (map reduction-relation-procs lst)))
     ;; not clear what the contract is here.
     `any)))

(define (do-node-match lhs-frm-id lhs-to-id pat rhs-proc child-make-proc rhs-from)
  (define (subst from to in)
    (let recur ([p in])
      (cond [(eq? from p) to]
            [(pair? p) (map recur p)]
            [else p])))
  ;; need call to make-rewrite-proc
  ;; also need a test case here to check duplication of names.
  (make-rewrite-proc
   (λ (lang)
     (let ([cp (compile-pattern lang pat #t)]
           [child-proc (child-make-proc lang)])
       (λ (main-exp exp f other-matches)
         (let ([mtchs (match-pattern cp exp)])
           (if mtchs
               (let o-loop ([mtchs mtchs]
                            [acc other-matches])
                 (cond
                   [(null? mtchs) acc]
                   [else
                    (let ([sub-exp (lookup-binding (mtch-bindings (car mtchs)) lhs-frm-id)])
                      (o-loop (cdr mtchs) 
                              (child-proc main-exp
                                          sub-exp
                                          (λ (x) (f (rhs-proc (mtch-bindings (car mtchs)) x)))
                                          acc)))]))
               other-matches)))))
   (rewrite-proc-name child-make-proc)
   (λ (lang) (subst lhs-frm-id ((rewrite-proc-lhs child-make-proc) lang) rhs-from))
   (rewrite-proc-lhs-src child-make-proc)
   (rewrite-proc-id child-make-proc)))

(define relation-coverage (make-parameter null))

(define (cover-case id cov)
  (hash-update! (coverage-counts cov) id 
                (λ (c) (cons (car c) (add1 (cdr c))))))

(define (covered-cases cov)
  (sort (hash-map (coverage-counts cov) (λ (k v) v))
        string<=?
        #:key car))

(define-struct coverage (relation counts))

(define-syntax (fresh-coverage stx)
  (syntax-case stx ()
    [(name subj-stx)
     (with-syntax ([subj
                    (cond [(and (identifier? (syntax subj-stx))
                                (let ([tf (syntax-local-value (syntax subj-stx) (λ () #f))])
                                  (and (term-fn? tf) (term-fn-get-id tf))))
                           => values]
                          [else (syntax (let ([r subj-stx])
                                          (if (reduction-relation? r)
                                              r
                                              (raise-type-error 'name "reduction-relation" r))))])])
       (syntax
        (let ([h (make-hasheq)])
          (cond [(metafunc-proc? subj)
                 (for-each
                  (λ (c) (hash-set! h (metafunc-case-id c) (cons (metafunc-case-src-loc c) 0)))
                  (metafunc-proc-cases subj))]
                [(reduction-relation? subj)
                 (for-each 
                  (λ (rwp) 
                    (hash-set! h (rewrite-proc-id rwp) (cons (or (rewrite-proc-name rwp) (rewrite-proc-lhs-src rwp)) 0)))
                  (reduction-relation-make-procs subj))])
          (make-coverage subj h))))]))

(define-syntax (test-match stx) (test-match/both stx #f))
(define-syntax (test-match? stx) (test-match/both stx #t))

(define-for-syntax (test-match/both stx boolean-only?)
  (syntax-case stx ()
    [(form-name lang-exp pattern)
     (identifier? #'lang-exp)
     (let ()
       (define what (syntax-e #'form-name))
       (with-syntax ([(syncheck-expr side-condition-rewritten (vars ...) (ids/depths ...))
                      (rewrite-side-conditions/check-errs #'lang-exp what #t #'pattern)])
         (with-syntax ([binders (map syntax-e (syntax->list #'(vars ...)))]
                       [name (syntax-local-infer-name stx)])
           #`(begin
               syncheck-expr
               (do-test-match lang-exp `side-condition-rewritten 'binders 'name #,boolean-only?)))))]
    [(form-name lang-exp pattern expression)
     (identifier? #'lang-exp)
     (syntax 
      ((form-name lang-exp pattern) expression))]
    [(_ a b c)
     (raise-syntax-error 'redex-match "expected an identifier (bound to a language) as first argument" stx #'a)]
    [(_ a b)
     (raise-syntax-error 'redex-match "expected an identifier (bound to a language) as first argument" stx #'a)]))

(define-struct match (bindings) #:inspector #f)

(define (do-test-match lang pat binders context-name boolean-only?)
  (unless (compiled-lang? lang)
    (error 'redex-match "expected first argument to be a language, got ~e" lang))
  (define name (or context-name
                   (and (symbol? pat)
                        pat)))
  (define cpat (compile-pattern lang pat #t))
  (define redex-match-proc
    (if boolean-only?
        (λ (exp) (match-pattern? cpat exp))
        (λ (exp)
          (let ([ans (match-pattern cpat exp)])
            (and ans
                 (map (λ (m) (make-match (sort-bindings 
                                          (filter (λ (x) (and (memq (bind-name x) binders)
                                                              (not-ellipsis-name (bind-name x))))
                                                  (bindings-table (mtch-bindings m))))))
                      ans))))))
  (if name
      (procedure-rename redex-match-proc name)
      redex-match-proc))
(define (not-ellipsis-name x)
  (not (regexp-match? #rx"^[.][.][.]" (symbol->string x))))

(define (sort-bindings bnds)
  (sort
   bnds
   (λ (x y) (string-ci<=? (symbol->string (bind-name x))
                          (symbol->string (bind-name y))))))

(define-struct metafunction (proc))

(define-struct metafunc-case (lhs rhs lhs+ src-loc id) #:transparent)

(define-syntax (in-domain? stx)
  (syntax-case stx ()
    [(_ (name exp ...))
     (begin
       (unless (identifier? #'name)
         (raise-syntax-error #f "expected an identifier" stx #'name))
       #'(in-domain?/proc (metafunction-form name) (term (exp ...))))]))

(define (in-domain?/proc mf exp)
  (let ([mp (metafunction-proc mf)])
    ((metafunc-proc-in-dom? mp)
     exp)))

(define-for-syntax (lhs-lws clauses)
  (with-syntax ([((lhs-for-lw _ ...) ...) clauses])
    (map (λ (x) (to-lw/proc (datum->syntax #f (cdr (syntax-e x)) x)))
         (syntax->list #'(lhs-for-lw ...)))))

;                                                                                                          
;                                                                                                          
;                                                                                                          
;                            ;              ;;;                                 ;    ;;                    
;                           ;;             ;;;;                                ;;    ;;                    
;  ;;;;;;; ;;;;    ;;;    ;;;;; ;;;;;;;   ;;;;; ;;;; ;;;; ;;;; ;;;    ;;;;;  ;;;;;        ;;;;   ;;;; ;;;  
;  ;;;;;;;;;;;;;  ;;;;;  ;;;;;; ;;;;;;;;  ;;;;  ;;;; ;;;; ;;;;;;;;;  ;;;;;; ;;;;;; ;;;;  ;;;;;;  ;;;;;;;;; 
;  ;;;; ;;; ;;;; ;;;; ;;  ;;;;      ;;;; ;;;;;; ;;;; ;;;; ;;;; ;;;; ;;;;;;;  ;;;;  ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;; ;;; ;;;; ;;;;;;;  ;;;;   ;;;;;;; ;;;;;; ;;;; ;;;; ;;;; ;;;; ;;;;     ;;;;  ;;;; ;;;; ;;; ;;;; ;;;; 
;  ;;;; ;;; ;;;; ;;;;;    ;;;;; ;;  ;;;;  ;;;;  ;;;; ;;;; ;;;; ;;;; ;;;;;;;  ;;;;; ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;; ;;; ;;;;  ;;;;;;  ;;;;; ;;;;;;;;  ;;;;  ;;;;;;;;; ;;;; ;;;;  ;;;;;;  ;;;;; ;;;;  ;;;;;;  ;;;; ;;;; 
;  ;;;; ;;; ;;;;   ;;;;    ;;;;  ;; ;;;;  ;;;;   ;;; ;;;; ;;;; ;;;;   ;;;;;   ;;;; ;;;;   ;;;;   ;;;; ;;;; 
;                                                                                                          
;                                                                                                          
;                                                                                                          

(define-syntax (define-metafunction stx)
  (syntax-case stx ()
    [(_ . rest)
     (internal-define-metafunction stx #f #'rest)]))

(define-syntax (define-metafunction/extension stx)
  (syntax-case stx ()
    [(_ prev . rest)
     (identifier? #'prev)
     (internal-define-metafunction stx #'prev #'rest)]))

(define-for-syntax (internal-define-metafunction orig-stx prev-metafunction stx)
  (not-expression-context orig-stx)
  (syntax-case stx ()
    [(lang . rest)
     (let ([syn-error-name (if prev-metafunction
                               'define-metafunction/extension
                               'define-metafunction)])
       ;; keep this near the beginning, so it signals the first error (PR 10062)
       (definition-nts #'lang orig-stx syn-error-name)
       (when (null? (syntax-e #'rest))
         (raise-syntax-error syn-error-name "no clauses" orig-stx))
       (when prev-metafunction
         (syntax-local-value 
          prev-metafunction
          (λ ()
            (raise-syntax-error 
             syn-error-name 
             "expected a previously defined metafunction" orig-stx prev-metafunction))))
       (let*-values ([(contract-name dom-ctcs pre-condition 
                                     codom-contracts codomain-separators post-condition
                                     pats)
                      (split-out-contract orig-stx syn-error-name #'rest #f)]
                     [(name _) (defined-name (list contract-name) pats orig-stx)])
         (when (and prev-metafunction (eq? (syntax-e #'name) (syntax-e prev-metafunction)))
           (raise-syntax-error syn-error-name
                               "the extended and extending metafunctions cannot share a name"
                               orig-stx
                               prev-metafunction))
         (with-syntax ([(name2 name-predicate) (generate-temporaries (list name name))]
                       [name name])
           (with-syntax ([name2+prop (syntax-property #'name2
                                                      'undefined-error-name
                                                      (syntax-e #'name))])
             (with-syntax ([defs #`(begin
                                     (define-values (name2+prop name-predicate)
                                       (generate-metafunction #,orig-stx
                                                              lang
                                                              #,prev-metafunction
                                                              name
                                                              name-predicate
                                                              #,dom-ctcs
                                                              #,(if pre-condition
                                                                    (list pre-condition)
                                                                    #f)
                                                              #,codom-contracts
                                                              '#,codomain-separators
                                                              #,(if post-condition
                                                                    (list post-condition)
                                                                    #f)
                                                              #,pats
                                                              #,syn-error-name))
                                     (term-define-fn name name2))])
               (if (eq? 'top-level (syntax-local-context))
                   ; Introduce the names before using them, to allow
                   ; metafunction definition at the top-level.
                   (syntax 
                    (begin 
                      (define-syntaxes (name2+prop name-predicate) (values))
                      defs))
                   (syntax defs)))))))]))

(define-for-syntax (extract-clause-names stuffss)
  (for/list ([stuffs (in-list (syntax->list stuffss))])
    (define the-clause-name #f)
    (define stuff-without-clause-name
      (for/fold ([stuffs '()]) ([stuff+name (in-list (syntax->list stuffs))])
        (syntax-case* stuff+name (clause-name) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
          [(clause-name id)
           (begin
             (unless (or (identifier? #'id)
                         (string? (syntax-e #'id)))
               (raise-syntax-error 'define-metafunction 
                                   "expected an identifier or a string"
                                   #'id))
             (when the-clause-name
               (raise-syntax-error 'define-metafunction
                                   "multiple names in a single clause"
                                   #f
                                   #f
                                   (list the-clause-name #'id)))
             (set! the-clause-name #'id)
             stuffs)]
          [_ (cons stuff+name stuffs)])))
    (cons (cond
            [(not the-clause-name) #f]
            [(identifier? the-clause-name) (symbol->string (syntax-e the-clause-name))]
            [else the-clause-name])
          (reverse stuff-without-clause-name))))

(define-syntax (generate-metafunction stx)
  (syntax-case stx ()
    [(_ orig-stx lang prev-metafunction-stx
        name name-predicate
        dom-ctcs-stx pre-condition-stx
        codom-contracts-stx codomain-separators-stx post-condition-stx
        pats-stx syn-error-name)
     (let ()
       (define (condition-or-false s)
         (define info (syntax-e s))
         (cond
           [info
            (unless (pair? info) (error 'condition-or-false "~s" s))
            (car info)]
           [else #f]))
       (define prev-metafunction (and (syntax-e #'prev-metafunction-stx) #'prev-metafunction-stx))
       (define dom-ctcs (syntax-e #'dom-ctcs-stx))
       (define pre-condition (condition-or-false #'pre-condition-stx))
       (define codom-contracts (syntax-e #'codom-contracts-stx))
       (define codomain-separators (syntax-e #'codomain-separators-stx))
       (define post-condition (condition-or-false #'post-condition-stx))
       (define pats (syntax-e #'pats-stx))
       (define syn-error-name (syntax-e #'syn-error-name))
       (define lang-nts
         (definition-nts #'lang #'orig-stx syn-error-name))
       (with-syntax ([(((original-names lhs-clauses ...) raw-rhses ...) ...) pats]
                     [(lhs-for-lw ...) (lhs-lws pats)])
         (with-syntax ([((rhs stuff+names ...) ...) #'((raw-rhses ...) ...)]
                       [(lhs ...) #'((lhs-clauses ...) ...)])
           (with-syntax ([((clause-name stuff ...) ...) (extract-clause-names #'((stuff+names ...) ...))])
             (parse-extras #'((stuff ...) ...))
             (with-syntax ([((syncheck-expr side-conditions-rewritten lhs-names lhs-namess/ellipses) ...) 
                            (map (λ (x) (rewrite-side-conditions/check-errs
                                         #'lang
                                         syn-error-name
                                         #t
                                         x))
                                 (syntax->list (syntax (lhs ...))))])
               (with-syntax ([(rhs/wheres ...)
                              (map (λ (sc/b rhs names names/ellipses)
                                     (bind-withs
                                      syn-error-name '()  
                                      #'effective-lang lang-nts #'lang
                                      sc/b 'flatten
                                      #`(list (term #,rhs #:lang lang))
                                      (syntax->list names) 
                                      (syntax->list names/ellipses)
                                      #t
                                      #f))
                                   (syntax->list #'((stuff ...) ...))
                                   (syntax->list #'(rhs ...))
                                   (syntax->list #'(lhs-names ...))
                                   (syntax->list #'(lhs-namess/ellipses ...)))]
                             [(rg-rhs/wheres ...)
                              (map (λ (sc/b rhs names names/ellipses) 
                                     (bind-withs
                                      syn-error-name '()  
                                      #'effective-lang lang-nts #'lang
                                      sc/b 'predicate
                                      #`#t
                                      (syntax->list names)
                                      (syntax->list names/ellipses)
                                      #t
                                      #f))
                                   (syntax->list #'((stuff ...) ...))
                                   (syntax->list #'(rhs ...))
                                   (syntax->list #'(lhs-names ...))
                                   (syntax->list #'(lhs-namess/ellipses ...)))])
                 (with-syntax ([((rg-syncheck-expr rg-side-conditions-rewritten rg-names rg-names/ellipses ...) ...)
                                (map (λ (x) (rewrite-side-conditions/check-errs
                                             #'lang
                                             syn-error-name
                                             #t
                                             x))
                                     (syntax->list (syntax ((side-condition lhs rg-rhs/wheres) ...))))]
                               [(clause-src ...)
                                (map (λ (lhs)
                                       (format "~a:~a:~a"
                                               (and (path? (syntax-source lhs))
                                                    (path->relative-string/library (syntax-source lhs)))
                                               (syntax-line lhs)
                                               (syntax-column lhs)))
                                     pats)]
                               [(dom-syncheck-expr dom-side-conditions-rewritten 
                                                   (dom-names ...)
                                                   dom-names/ellipses)
                                (if dom-ctcs
                                    (rewrite-side-conditions/check-errs
                                     #'lang
                                     syn-error-name
                                     #f
                                     dom-ctcs)
                                    #'((void) any () ()))]
                               [((codom-syncheck-expr 
                                  codom-side-conditions-rewritten
                                  codom-names 
                                  codom-names/ellipses) ...)
                                (map (λ (codom-contract)
                                       (rewrite-side-conditions/check-errs
                                        #'lang
                                        syn-error-name
                                        #f
                                        (if post-condition
                                            #`(side-condition (#,dom-ctcs #,codom-contract)
                                                              (term #,post-condition))
                                            codom-contract)))
                                     codom-contracts)]
                               [(rhs-fns ...)
                                (map (λ (names names/ellipses rhs/where)
                                       (with-syntax ([(names ...) names]
                                                     [(names/ellipses ...) names/ellipses]
                                                     [rhs/where rhs/where])
                                         (syntax
                                          (λ (name bindings)
                                            (term-let-fn ((name name))
                                                         (term-let ([names/ellipses (lookup-binding bindings 'names)] ...)
                                                                   rhs/where))))))
                                     (syntax->list #'(lhs-names ...))
                                     (syntax->list #'(lhs-namess/ellipses ...))
                                     (syntax->list (syntax (rhs/wheres ...))))]
                               [(gen-clause ...)
                                (make-mf-clauses (syntax->list #'(lhs ...))
                                                 (syntax->list #'(rhs ...))
                                                 (syntax->list #'((stuff ...) ...))
                                                 lang-nts syn-error-name #'name #'lang)])
                   (syntax-property
                    (prune-syntax
                     #`(let ([sc `(side-conditions-rewritten ...)]
                             [dsc `dom-side-conditions-rewritten])
                         syncheck-expr ... rg-syncheck-expr ... dom-syncheck-expr codom-syncheck-expr ...
                         (let ([cases (map (λ (pat rhs-fn rg-lhs src)
                                             (make-metafunc-case
                                              (λ (effective-lang) (compile-pattern effective-lang pat #t))
                                              rhs-fn
                                              rg-lhs src (gensym)))
                                           sc
                                           (list (λ (effective-lang) rhs-fns) ...)
                                           (list (λ (effective-lang) `rg-side-conditions-rewritten) ...)
                                           `(clause-src ...))]
                               [parent-cases 
                                #,(if prev-metafunction
                                      #`(metafunc-proc-cases #,(term-fn-get-id (syntax-local-value prev-metafunction)))
                                      #'null)])
                           (build-metafunction 
                            lang
                            cases
                            parent-cases
                            (λ (f/dom)
                              (make-metafunc-proc
                               (let ([name (lambda (x) (f/dom x))]) name)
                               '(clause-name ...)
                               (list
                                ;; mf contract
                                #,(if (and dom-ctcs codom-contracts)
                                      #`(list
                                         #,(with-syntax ([(dom-ctc ...) dom-ctcs])
                                             #`(list (to-lw dom-ctc) ...))
                                         #,(with-syntax ([(codom-ctc ...) codom-contracts])
                                             #`(list (to-lw codom-ctc) ...))
                                         #,codomain-separators)
                                      #'#f)
                                
                                ;; body of mf
                                (generate-lws #f
                                              (lhs ...)
                                              (lhs-for-lw ...)
                                              ((stuff ...) ...)
                                              (rhs ...)
                                              #t))
                               lang
                               #t ;; multi-args?
                               'name
                               (let ([name (lambda (x) (name-predicate x))]) name)
                               dsc
                               (append cases parent-cases)
                               #,(cond
                                   [prev-metafunction
                                    #`(extend-mf-clauses #,(term-fn-get-id (syntax-local-value prev-metafunction))
                                                         (λ ()
                                                           (add-mf-dqs #,(check-pats #'(list gen-clause ...)))))]
                                   [else
                                    #`(memoize0
                                       (λ ()
                                         (add-mf-dqs #,(check-pats #'(list gen-clause ...)))))])))
                            #,(if dom-ctcs #'dsc #f)
                            (λ (bindings) 
                              #,(bind-pattern-names 'define-metafunction
                                                    #'dom-names/ellipses
                                                    #'((lookup-binding bindings 'dom-names) ...)
                                                    #`(term #,pre-condition)))
                            `(codom-side-conditions-rewritten ...)
                            #,(and post-condition #t)
                            'name))))
                    'disappeared-use
                    (map syntax-local-introduce 
                         (syntax->list #'(original-names ...)))))))))))]))

(define (extend-mf-clauses old-mf new-clauses)
  (memoize0
   (λ ()
     (define new-cs (new-clauses))
     (define new-lhss
       (for/list ([c new-cs])
         (match: c
           [(clause `(list ,c-lhs ,c-rhs) c-eq/dqs c-prems c-lang c-name)
            c-lhs])))
     (define new-old-cs
       (for/list ([old-c (in-list ((metafunc-proc-gen-clauses old-mf)))])
         (match: old-c
           [(clause `(list ,c-lhs ,c-rhs) c-eq/dqs c-prems c-lang c-name)
            (define new-dqs (make-clause-dqs c-lhs new-lhss (length new-lhss)))
            (struct-copy clause old-c
                         [eq/dqs (append new-dqs c-eq/dqs)])])))
     (append new-cs new-old-cs))))

(define uniq (gensym))
(define (memoize0 t)
  (let ([ans uniq])
    (λ ()
      (when (eq? ans uniq)
        (set! ans (t)))
      ans)))

(define-for-syntax (make-mf-clauses lhss rhss extrass nts err-name name lang)
  (define rev-clauses
    (for/fold ([clauses '()]) 
      ([lhs (in-list lhss)] [rhs (in-list rhss)] [extras (in-list extrass)])
      (with-syntax ([(lhs-syncheck-expr lhs-pat (names ...) (names/ellipses ...)) (rewrite-side-conditions/check-errs lang err-name #t lhs)])
        (define-values (ps-rw extra-eqdqs p-names) 
          (rewrite-prems #f (syntax->list extras) (syntax->datum #'(names ...)) lang 'define-metafunction))
        (define-values (rhs-pats mf-clausess) (rewrite-terms (list rhs) p-names)) 
        (define clause-stx
          (with-syntax ([(prem-rw ...) ps-rw]
                        [(eqs ...) extra-eqdqs]
                        [(mf-clauses ...) mf-clausess]
                        [(rhs-pat) rhs-pats])
            #`(begin
                lhs-syncheck-expr
                (clause '(list lhs-pat rhs-pat)
                        (list eqs ...)
                        (list prem-rw ... mf-clauses ...)
                        #,lang
                        '#,name))))
        (cons clause-stx clauses))))
  (reverse rev-clauses))

(define (add-mf-dqs clauses)
  (define-values (new-clauses _)
    (for/fold ([new-cs '()] [prev-lhss '()])
      ([c clauses])
      (match: c
        [(clause `(list ,c-lhs ,c-rhs) c-eq/dqs c-prems c-lang c-name)
         (define new-dqs (make-clause-dqs c-lhs prev-lhss))
         (define new-c (struct-copy clause c
                                    [eq/dqs (append new-dqs c-eq/dqs)]))
         (values (cons new-c new-cs)
                 (cons c-lhs prev-lhss))])))
  (reverse new-clauses))

(define (make-clause-dqs rhs-pat prev-lhs-pats [n 0])
  (define rhs-vs (pat-vars rhs-pat))
  (define fixed-lhss
    (for/list ([lhs (in-list prev-lhs-pats)])
      (begin0
        (let recur ([p lhs])
          (match: p
            [`(name ,v ,p)
             (define new-v (string->symbol (format "~s_lhs~s" v n)))
             (let loop ([new-v new-v])
               (if (set-member? rhs-vs new-v)
                   (loop (string->symbol (format "~s:" new-v)))
                   `(name ,new-v ,(recur p))))]
            [`(list ,ps ...)
             `(list ,@(map recur ps))]
            [_ p]))
        (set! n (add1 n)))))
  (for/list ([plhs (in-list fixed-lhss)])
    (define lhs-vs (pat-vars plhs))
    (dqn (set->list lhs-vs) plhs rhs-pat)))

(define (pat-vars p)
  (match: p
    [`(name ,v ,p)
     (set-add (pat-vars p) v)]
    [`(list ,ps ...)
     (apply set-union (set) (map pat-vars ps))]
    [_
     (set)]))
  
(define-for-syntax (check-arity-consistency mode contracts full-def)
  (when (and contracts (not (= (length mode) (length contracts))))
    (raise-syntax-error 
     #f "mode and contract specify different numbers of positions" full-def)))

(define-for-syntax (defined-name declared-names clauses orig-stx)
  (with-syntax ([(((used-names _ ...) _ ...) ...) clauses])
    (define-values (the-name other-names)
      (let ([present (filter values declared-names)])
        (if (null? present)
            (values (car (syntax->list #'(used-names ...)))
                    (cdr (syntax->list #'(used-names ...))))
            (values (car present) 
                    (append (cdr present) (syntax->list #'(used-names ...)))))))
    (let loop ([others other-names])
      (cond
        [(null? others) (values the-name other-names)]
        [else
         (unless (eq? (syntax-e the-name) (syntax-e (car others)))
           (raise-syntax-error 
            #f
            "expected the same name in both positions"
            orig-stx
            the-name (list (car others))))
         (loop (cdr others))]))))


(define-for-syntax (parse-extras extras)
  (for-each
   (λ (stuffs)
     (for-each
      (λ (stuff)
        (syntax-case stuff (where side-condition where/hidden side-condition/hidden judgment-holds)
          [(side-condition tl-side-conds ...) 
           (void)]
          [(side-condition/hidden tl-side-conds ...) 
           (void)]
          [(where x e)
           (void)]
          [(where/hidden x e)
           (void)]
          [(where . args)
           (raise-syntax-error 'define-metafunction 
                               "malformed where clause"
                               stuff)]
          [(where/hidden . args)
           (raise-syntax-error 'define-metafunction 
                               "malformed where/hidden clause"
                               stuff)]
          [(judgment-holds (form-name . _))
           (unless (judgment-form-id? #'form-name)
             (raise-syntax-error 'define-metafunction
                                 "expected the name of a judgment-form"
                                 #'form-name))]
          [_
           (raise-syntax-error 'define-metafunction 
                               "expected a side-condition, where, or clause-name"
                               stuff)]))
      (syntax->list stuffs)))
   (syntax->list extras)))


(define (build-metafunction lang cases parent-cases 
                            wrap
                            dom-contract-pat pre-condition
                            codom-contract-pats post-condition?
                            name)
  (let* ([dom-compiled-pattern (and dom-contract-pat (compile-pattern lang dom-contract-pat #f))]
         [codom-compiled-patterns (map (λ (codom-contract-pat) (compile-pattern lang codom-contract-pat #f))
                                       codom-contract-pats)]
         [all-cases (append cases parent-cases)]
         [lhss-at-lang (map (λ (case) ((metafunc-case-lhs case) lang)) all-cases)]
         [rhss-at-lang (map (λ (case) ((metafunc-case-rhs case) lang)) all-cases)]
         [ids (map metafunc-case-id all-cases)])
    (values
     (wrap
      (letrec ([cache (make-hash)]
               [cache-entries 0]
               [not-in-cache (gensym)]
               [cache-result (λ (arg res case)
                               (when (caching-enabled?)
                                 (when (>= cache-entries cache-size)
                                   (set! cache (make-hash))
                                   (set! cache-entries 0))
                                 (hash-set! cache arg (cons res case))
                                 (set! cache-entries (add1 cache-entries))))]
               [log-coverage (λ (id)
                               (when id
                                 (for-each 
                                  (λ (c)
                                    (let ([r (coverage-relation c)])
                                      (when (and (metafunc-proc? r)
                                                 (findf (λ (c) (eq? id (metafunc-case-id c)))
                                                        (metafunc-proc-cases r)))
                                        (cover-case id c))))
                                  (relation-coverage))))]
               [metafunc
                (λ (exp)
                  (let ([cache-ref (hash-ref cache exp not-in-cache)])
                    (cond
                      [(or (not (caching-enabled?)) (eq? cache-ref not-in-cache))
                       (define dom-match-result 
                         (if dom-compiled-pattern
                             (match-pattern dom-compiled-pattern exp)
                             '()))
                       (when dom-compiled-pattern
                         (unless dom-match-result
                           (redex-error name
                                        "~s is not in my domain"
                                        `(,name ,@exp)))
                         (unless (for/and ([mtch (in-list dom-match-result)])
                                   (pre-condition (mtch-bindings mtch)))
                           (redex-error name
                                        "~s is not in my domain"
                                        `(,name ,@exp))))
                       (let loop ([ids ids]
                                  [lhss lhss-at-lang]
                                  [rhss rhss-at-lang]
                                  [num (- (length parent-cases))])
                         (cond
                           [(null? ids) 
                            (redex-error name "no clauses matched for ~s" `(,name . ,exp))]
                           [else
                            (define pattern (car lhss))
                            (define rhs (car rhss))
                            (define id (car ids))
                            (define (continue) (loop (cdr ids) (cdr lhss) (cdr rhss) (+ num 1)))
                            (define mtchs (match-pattern pattern exp))
                            (cond
                              [(not mtchs) (continue)]
                              [else
                               (define anss
                                 (apply append
                                        (filter values
                                                (map (λ (mtch) (rhs traced-metafunc (mtch-bindings mtch)))
                                                     mtchs))))
                               (define ht (make-hash))
                               (for-each (λ (ans) (hash-set! ht ans #t)) anss)
                               (cond
                                 [(null? anss)
                                  (continue)]
                                 [(not (= 1 (hash-count ht)))
                                  (redex-error name "~a matched ~s ~a returned different results" 
                                               (if (< num 0)
                                                   "a clause from an extended metafunction"
                                                   (format "clause #~a (counting from 0)" num))
                                               `(,name ,@exp)
                                               (if (= 1 (length mtchs))
                                                   "but"
                                                   (format "~a different ways and "
                                                           (length mtchs))))]
                                 [else
                                  (define ans (car anss))
                                  (unless (for/or ([codom-compiled-pattern 
                                                    (in-list codom-compiled-patterns)])
                                            (match-pattern codom-compiled-pattern 
                                                           (if post-condition?
                                                               (list exp ans)
                                                               ans)))
                                    (redex-error name
                                                 "codomain test failed for ~s, call was ~s"
                                                 ans 
                                                 `(,name ,@exp)))
                                  (cache-result exp ans id)
                                  (log-coverage id)
                                  ans])])]))]
                      [else 
                       (log-coverage (cdr cache-ref))
                       (car cache-ref)])))]
               [ot (current-trace-print-args)]
               [otr (current-trace-print-results)]
               [traced-metafunc (lambda (exp)
                                  (if (or (eq? (current-traced-metafunctions) 'all)
                                          (memq name (current-traced-metafunctions)))
                                      (parameterize ([current-trace-print-args
                                                      (λ (name args kws kw-args level)
                                                        (if (or (not (caching-enabled?))
                                                                (eq? not-in-cache (hash-ref cache exp not-in-cache)))
                                                            (display " ")
                                                            (display "c"))
                                                        (ot name (car args) kws kw-args level))]
                                                     [current-trace-print-results
                                                      (λ (name results level)
                                                        (display " ")
                                                        (otr name results level))]
                                                     [print-as-expression #f])
                                        (trace-call name metafunc exp))
                                      (metafunc exp)))])
        traced-metafunc))
     (if dom-compiled-pattern
         (λ (exp) (and (match-pattern dom-compiled-pattern exp) #t))
         (λ (exp) (and (ormap (λ (lhs) (match-pattern lhs exp)) lhss-at-lang) 
                       #t))))))

(define-syntax (metafunction-form stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (let ([v (syntax-local-value #'id (lambda () #f))])
       (if (term-fn? v)
           (syntax-property
            #`(make-metafunction #,(term-fn-get-id v))
            'disappeared-use
            (list (syntax-local-introduce #'id)))
           (raise-syntax-error
            #f
            "not bound as a metafunction"
            stx
            #'id)))]))

(define-syntax (::= stx)
  (raise-syntax-error #f "cannot be used outside a language definition" stx))

(define-for-syntax (parse-non-terminals nt-defs stx)
  (define (parse-non-terminal def)
    (define (delim? stx)
      (and (identifier? stx) (free-identifier=? stx #'::=)))
    (define-values (left delim right)
      (syntax-case def ()
        [(_ _ ...)
         (let split ([xs def])
           (syntax-case xs (::=)
             [() (values '() #f '())]
             [(x . prods)
              (delim? #'x)
              (values '() #'x (syntax->list #'prods))]
             [(x . xs)
              (let-values ([(l d r) (split #'xs)])
                (values (cons #'x l) d r))]))]
        [_ (raise-syntax-error #f "expected non-terminal definition" stx def)]))
    (define (check-each xs bad? msg)
      (define x (findf bad? xs))
      (when x (raise-syntax-error #f msg stx x)))
    (define-values (names prods)
      (if delim
          (begin
            (when (null? left)
              (raise-syntax-error #f "expected preceding non-terminal names" stx delim))
            (values left right))
          (values (syntax-case (car left) ()
                    [(x ...) (syntax->list #'(x ...))]
                    [x (list #'x)])
                  (cdr left))))
    
    (check-each names (λ (x) (not (identifier? x)))
                "expected non-terminal name")
    (check-each names (λ (x) (memq (syntax-e x) (cons 'name underscore-allowed)))
                "cannot use pattern language keyword as a non-terminal name")
    (check-each names (λ (x) (regexp-match? #rx"_" (symbol->string (syntax-e x))))
                "cannot use _ in a non-terminal name")
    (check-each names (λ (x) (regexp-match? #rx"^[.][.][.]$" (symbol->string (syntax-e x))))
                "cannot name a non-terminal `...'")
    (check-each names (λ (x) (regexp-match? #rx"^[.][.][.]_" (symbol->string (syntax-e x))))
                "cannot start a non-terminal name with `..._'")
    
    (when (null? prods)
      (raise-syntax-error #f "expected at least one production to follow" 
                          stx (or delim (car left))))
    (check-each prods delim? "expected production")
    (cons names prods))
  (define parsed (map parse-non-terminal (syntax->list nt-defs)))
  (define defs (make-hash))
  (for ([p parsed])
    (define ns (car p))
    (for ([n ns])
      (define m (hash-ref defs (syntax-e n) #f))
      (if m
          (raise-syntax-error #f "same non-terminal defined twice"
                              stx n (list m))
          (hash-set! defs (syntax-e n) n))))
  parsed)

(define-syntax (define-language stx)
  (not-expression-context stx)
  (syntax-case stx ()
    [(form-name lang-name . nt-defs)
     (begin
       (unless (identifier? #'lang-name)
         (raise-syntax-error #f "expected an identifier" stx #'lang-name))
       (with-syntax ([(define-language-name) (generate-temporaries #'(lang-name))])
         (define non-terms (parse-non-terminals #'nt-defs stx))
         (with-syntax ([((names prods ...) ...) non-terms]
                       [(all-names ...) (apply append (map car non-terms))]
                       [(nt-ids ...) 
                        (for/list ([nt-def (in-list (syntax->list #'nt-defs))])
                          (syntax-case nt-def ()
                            [(x . whatever) #'x]))])
           (with-syntax ([bindings
                          (let loop ([nt-ids (syntax->list #'(nt-ids ...))]
                                     [stx #'(void)])
                            (cond
                              [(null? nt-ids) stx]
                              [else 
                               (define old (syntax-property stx 'disappeared-binding))
                               (define new (syntax-local-introduce (car nt-ids)))
                               (loop (cdr nt-ids)
                                     (syntax-property stx
                                                      'disappeared-binding
                                                      (if old (cons new old) new)))]))])
                                          
             (quasisyntax/loc stx
               (begin
                 bindings
                 (define-syntax lang-name
                   (make-set!-transformer
                    (make-language-id
                     (λ (stx)
                       (syntax-case stx (set!)
                         [(set! x e) (raise-syntax-error (syntax-e #'form-name) "cannot set! identifier" stx #'e)]
                         [(x e (... ...))
                          #'(define-language-name e (... ...))]
                         [x 
                          (identifier? #'x)
                          #'define-language-name]))
                     '(all-names ...)
                     (to-table #'(nt-ids ...)))))
                 (define define-language-name
                   #,(syntax/loc stx (language form-name lang-name (all-names ...) (names prods ...) ...)))))))))]))

(define-for-syntax (to-table x)
  (for/hash ([id (in-list (syntax->list x))])
    (values (syntax-e id) id)))

(define-struct binds (source binds))
  
(define-syntax (language stx)
  (syntax-case stx ()
    [(_ form-name lang-id (all-names ...) (name rhs ...) ...)
     (prune-syntax
      (let ()
        (let ([all-names (syntax->list #'(all-names ...))])
          (with-syntax ([(((r-syncheck-expr r-rhs r-names r-names/ellipses) ...) ...) 
                         ;; r-syncheck-expr has nothing interesting, I believe, so drop it
                         (map (lambda (rhss) 
                                (map (lambda (rhs)
                                       (rewrite-side-conditions/check-errs
                                        #'lang-id
                                        (syntax-e #'form-name)
                                        #f
                                        rhs)) 
                                     (syntax->list rhss)))
                              (syntax->list (syntax ((rhs ...) ...))))]
                        [((rhs/lw ...) ...) 
                         (map (lambda (rhss) (map to-lw/proc (syntax->list rhss)))
                              (syntax->list (syntax ((rhs ...) ...))))]
                        [(refs ...)
                         (let loop ([stx (syntax ((rhs ...) ...))])
                           (cond
                             [(identifier? stx)
                              (if (ormap (λ (x) (bound-identifier=? x stx)) 
                                         all-names)
                                  (list stx)
                                  '())]
                             [(syntax? stx)
                              (loop (syntax-e stx))]
                             [(pair? stx)
                              (append (loop (car stx))
                                      (loop (cdr stx)))]
                             [else '()]))])
            (check-for-cycles stx #'(name ...) #'((r-rhs ...) ...))
            (with-syntax ([(the-stx ...) (cdr (syntax-e stx))]
                          [(all-names ...) all-names]
                          [((uniform-names ...) ...)
                           (map (λ (x) (if (identifier? x) (list x) x))
                                (syntax->list (syntax (name ...))))]
                          [(first-names ...)
                           (map (λ (x) (if (identifier? x) x (car (syntax->list x))))
                                (syntax->list (syntax (name ...))))]
                          [((new-name orig-name) ...)
                           (apply
                            append
                            (map (λ (name-stx)
                                   (if (identifier? name-stx)
                                       '()
                                       (let ([l (syntax->list name-stx)])
                                         (map (λ (x) (list x (car l)))
                                              (cdr l)))))
                                 (syntax->list #'(name ...))))])
              
              ;; note: when there are multiple names for a single non-terminal,
              ;; we build equivalent non-terminals by redirecting all except the
              ;; first non-terminal to the first one, and then make the first one
              ;; actually have all of the productions. This should produce better
              ;; caching behavior and should compile faster than duplicating the
              ;; right-hand sides.
              (syntax/loc stx
                (begin
                  (let ([all-names 1] ...)
                    (begin (void) refs ...))
                  (compile-language (list (list '(uniform-names ...) rhs/lw ...) ...)
                                    (list (make-nt 'first-names (list (make-rhs `r-rhs) ...)) ...
                                          (make-nt 'new-name (list (make-rhs '(nt orig-name)))) ...)
                                    (mk-uf-sets '((uniform-names ...) ...))))))))))]))


(define-syntax (define-extended-language stx)
  (syntax-case stx ()
    [(_ name orig-lang . nt-defs)
     (begin
       (unless (identifier? (syntax name))
         (raise-syntax-error 'define-extended-language "expected an identifier" stx #'name))
       (unless (identifier? (syntax orig-lang))
         (raise-syntax-error 'define-extended-language "expected an identifier" stx #'orig-lang))
       (let ([old-names (language-id-nts #'orig-lang 'define-extended-language)]
             [non-terms (parse-non-terminals #'nt-defs stx)])
         (with-syntax ([((names prods ...) ...) non-terms]
                       [(all-names ...)
                        ;; The names may have duplicates if the extended language
                        ;; extends non-terminals in the parent language. They need
                        ;; to be removed for `define-union-language`
                        (remove-duplicates
                         (apply append old-names (map car non-terms))
                         (λ (n1 n2)
                           (let ([n1 (if (syntax? n1) (syntax-e n1) n1)]
                                 [n2 (if (syntax? n2) (syntax-e n2) n2)])
                             (eq? n1 n2))))]
                       [(define-language-name) (generate-temporaries #'(name))])
           #'(begin
               (define define-language-name (extend-language orig-lang (all-names ...) (names prods ...) ...))
               (define-syntax name
                 (make-set!-transformer
                  (make-language-id
                   (λ (stx)
                     (syntax-case stx (set!)
                       [(set! x e) (raise-syntax-error 'define-extended-language "cannot set! identifier" stx #'e)]
                       [(x e (... ...)) #'(define-language-name e (... ...))]
                       [x 
                        (identifier? #'x)
                        #'define-language-name]))
                   '(all-names ...)
                   (to-table #'()))))))))]))

(define-syntax (extend-language stx)
  (syntax-case stx ()
    [(_ lang (all-names ...) (name rhs ...) ...)
     (with-syntax ([(((r-syncheck-expr r-rhs r-names r-names/ellipses) ...) ...)
                    (map (lambda (rhss) (map (λ (x) (rewrite-side-conditions/check-errs
                                                     (append (language-id-nts #'lang 'define-extended-language)
                                                             (map syntax-e
                                                                  (syntax->list #'(all-names ...))))
                                                     'define-extended-language
                                                     #f
                                                     x))
                                             (syntax->list rhss)))
                         (syntax->list (syntax ((rhs ...) ...))))]
                   [((rhs/lw ...) ...) (map (lambda (rhss) (map to-lw/proc (syntax->list rhss)))
                                            (syntax->list (syntax ((rhs ...) ...))))]
                   [((uniform-names ...) ...)
                    (map (λ (x) (if (identifier? x) (list x) x))
                         (syntax->list (syntax (name ...))))])
       (syntax/loc stx
         (do-extend-language (begin r-syncheck-expr ... ... lang)
                             (list (make-nt '(uniform-names ...) (list (make-rhs `r-rhs) ...)) ...)
                             (list (list '(uniform-names ...) rhs/lw ...) ...))))]))

(define extend-nt-ellipses '(....))

;; do-extend-language : compiled-lang (listof (listof nt)) ? -> compiled-lang
;; note: the nts that come here are an abuse of the `nt' struct; they have
;; lists of symbols in the nt-name field.
(define (do-extend-language old-lang new-nts new-pict-infos)
  (unless (compiled-lang? old-lang)
    (error 'define-extended-language "expected a language as first argument, got ~e" old-lang))
  
  (let ([old-nts (compiled-lang-lang old-lang)]
        [old-ht (make-hasheq)]
        [new-ht (make-hasheq)])
    
    
    (for-each (λ (nt) 
                (hash-set! old-ht (nt-name nt) nt)
                (hash-set! new-ht (nt-name nt) nt))
              old-nts)
    
    (for-each (λ (raw-nt)
                (let* ([names (nt-name raw-nt)]
                       [rhs (nt-rhs raw-nt)]
                       [primary-names (map (λ (name) (find-primary-nt name old-lang)) names)]
                       [main-primary (car primary-names)])
                  
                  ;; error checking
                  (when (and (ormap not primary-names)
                             (ormap symbol? primary-names))
                    (error 'define-extended-language "new language extends old non-terminal ~a and also adds new shortcut ~a"
                           (ormap (λ (x y) (and (symbol? x) y)) primary-names names)
                           (ormap (λ (x y) (and (not x) y)) primary-names names)))
                  
                  ;; error checking
                  (when (andmap symbol? primary-names)
                    (let ([main-orig (car names)])
                      (let loop ([primary-names (cdr primary-names)]
                                 [names (cdr names)])
                        (cond
                          [(null? primary-names) void]
                          [else 
                           (unless (eq? main-primary (car primary-names))
                             (error 'define-extended-language
                                    (string-append 
                                     "new language does not have the same non-terminal aliases as the old,"
                                     " non-terminal ~a was not in the same group as ~a in the old language")
                                    (car names)
                                    main-orig))
                           (loop (cdr primary-names) (cdr names))]))))
                                  
                  
                  ;; rebind original nt
                  (let ([nt (make-nt (or main-primary (car names)) rhs)])
                    (cond
                      [(ormap (λ (rhs) (member (rhs-pattern rhs) extend-nt-ellipses))
                              (nt-rhs nt))
                       (unless (hash-ref old-ht (nt-name nt) #f)
                         (error 'define-extended-language
                                "the language extends the ~s non-terminal, but that non-terminal is not in the old language"
                                (nt-name nt)))
                       (hash-set! new-ht 
                                  (nt-name nt)
                                  (make-nt
                                   (nt-name nt)
                                   (append (nt-rhs (hash-ref old-ht (nt-name nt)))
                                           (filter (λ (rhs) (not (member (rhs-pattern rhs) extend-nt-ellipses)))
                                                   (nt-rhs nt)))))]
                      [else
                       (hash-set! new-ht (nt-name nt) nt)]))
                  
                  ;; add new shortcuts (if necessary)
                  (unless main-primary
                    (for-each (λ (shortcut-name)
                                (hash-set! new-ht 
                                           shortcut-name 
                                           (make-nt shortcut-name (list (make-rhs `(nt ,(car names)))))))
                              (cdr names)))))
                  
              new-nts)
    
    (compile-language (vector (compiled-lang-pict-builder old-lang)
                              new-pict-infos)
                      (hash-map new-ht (λ (x y) y))
                      (compiled-lang-nt-map old-lang))))

(define-syntax (define-union-language stx)
  (syntax-case stx ()
    [(_ name orig-langs ...)
     (begin
       (unless (identifier? (syntax name))
         (raise-syntax-error 'define-extended-language "expected an identifier" stx #'name))
       (when (null? (syntax->list #'(orig-langs ...)))
         (raise-syntax-error 'define-union-language "expected at least one additional language" stx))
       ;; normalized-orig-langs : (listof (list string[prefix] id (listof symbol)[nts] stx[orig clause in union]))
       (define normalized-orig-langs
         (for/list ([orig-lang (in-list (syntax->list #'(orig-langs ...)))])
           (syntax-case orig-lang ()
             [x (identifier? #'x) (list #f #'x (language-id-nts #'x 'define-union-language) orig-lang)]
             [(prefix lang)
              (and (identifier? #'prefix)
                   (identifier? #'lang))
              (list (symbol->string (syntax-e #'prefix)) #'lang (language-id-nts #'lang 'define-union-language) orig-lang)]
             [else (raise-syntax-error 'define-union-language 
                                       "malformed additional language"
                                       stx orig-lang)])))
       
       ;; ht : sym -o> (listof stx)
       ;; maps each non-terminal (with its prefix) to a
       ;; list of syntax objects that they come from in the original
       ;; define-union-language declaration 
       (define names-table (make-hash))
       
       (for ([normalized-orig-lang (in-list normalized-orig-langs)])
         (define prefix (list-ref normalized-orig-lang 0))
         (for ([no-prefix-nt (in-list (list-ref normalized-orig-lang 2))])
           (define nt (string->symbol (string-append (or prefix  "")
                                                     (symbol->string no-prefix-nt))))
           (let ([prev (hash-ref names-table nt '())])
             (hash-set! names-table nt (cons (list-ref normalized-orig-lang 3) prev)))))
       
       (with-syntax ([(all-names ...) (sort (hash-map names-table (λ (x y) x)) string<=? #:key symbol->string)]
                     [((prefix old-lang _1 _2) ...) normalized-orig-langs]
                     [(define-language-name) (generate-temporaries #'(name))])
         #'(begin
             (define define-language-name (union-language (list (list 'prefix old-lang) ...)))
             (define-syntax name
               (make-set!-transformer
                (make-language-id
                 (λ (stx)
                   (syntax-case stx (set!)
                     [(set! x e) (raise-syntax-error 'define-extended-language "cannot set! identifier" stx #'e)]
                     [(x e (... ...)) #'(define-language-name e (... ...))]
                     [x 
                      (identifier? #'x)
                      #'define-language-name]))
                 '(all-names ...)
                 (to-table #'())))))))]))

(define (union-language old-langs/prefixes)
  
  (define (add-prefix prefix sym)
    (if prefix
        (string->symbol
         (string-append prefix
                        (symbol->string sym)))
        sym))
  
  ;; nt-maps-with-prefixes : (listof hash[symbol -o> uf-set?])
  ;; add prefixes on the canonical elements and in the
  ;; hash-table domains
  (define nt-maps-with-prefixes
    (for/list ([old-pr (in-list old-langs/prefixes)])
      (define prefix (list-ref old-pr 0))
      (define nt-map (compiled-lang-nt-map (list-ref old-pr 1)))
      (cond
        [prefix
         (define already-prefixed '())
         (for/hash ([(nt a-uf-set) (in-hash nt-map)])
           (define already-prefixed?
             (for/or ([x (in-list already-prefixed)])
               (uf-same-set? x a-uf-set)))
           (unless already-prefixed?
             (uf-set-canonical! a-uf-set (add-prefix prefix (uf-find a-uf-set)))
             (set! already-prefixed (cons a-uf-set already-prefixed)))
           (values (add-prefix prefix nt) a-uf-set))]
        [else nt-map])))

  ;; combine all of the nt-maps into a single one,
  ;; unioning the sets as approrpriate
  (define new-nt-map (car nt-maps-with-prefixes))
  (for ([nt-map (in-list (cdr nt-maps-with-prefixes))])
    (for ([(k this-uf-set) (in-hash nt-map)])
      (define final-uf-set (hash-ref new-nt-map k #f))
      (cond
        [final-uf-set 
         (uf-union! final-uf-set this-uf-set)]
        [else
         (set! new-nt-map (hash-set new-nt-map k this-uf-set))])))
  
  (define names-table (make-hash))
  (for ([old-lang/prefix (in-list old-langs/prefixes)])
    (define prefix (list-ref old-lang/prefix 0))
    (define lang (compiled-lang-lang (list-ref old-lang/prefix 1)))
    (for ([nt (in-list lang)])
      (define name (add-prefix prefix (nt-name nt)))
      (define new-rhses
        (for/set ([rhs (in-list (nt-rhs nt))])
          (if prefix 
              (make-rhs (prefix-nts prefix (rhs-pattern rhs)))
              rhs)))
      (hash-set! names-table 
                 name
                 (set-union new-rhses (hash-ref names-table name (set))))))
  
  (compile-language #f
                    (hash-map names-table (λ (name set) (make-nt name (set->list set))))
                    new-nt-map))


;; find-primary-nt : symbol lang -> symbol or #f
;; returns the primary non-terminal for a given nt, or #f if `nt' isn't bound in the language.
(define (find-primary-nt nt lang)
  (define uf/f (hash-ref (compiled-lang-nt-map lang) nt #f))
  (and uf/f
       (uf-find uf/f)))

(define (apply-reduction-relation* reductions exp 
                                   #:cache-all? [cache-all? (current-cache-all?)]
                                   #:stop-when [stop-when (λ (x) #f)])
  (let-values ([(results cycle?) (traverse-reduction-graph reductions exp
                                                           #:cache-all? cache-all?
                                                           #:stop-when stop-when)])
    results))

(struct search-success ())
(struct search-failure (cutoff?))

;; traverse-reduction-graph : 
;;  reduction-relation term #:goal (-> any boolean?) #:steps number? #:visit (-> any/c void?) -> (or/c search-success? search-failure?)
;;  reduction-relation term #:goal #f                #:steps number? #:visit (-> any/c void?) -> (values (listof any/c) boolean?)
(define (traverse-reduction-graph reductions start #:goal [goal? #f] #:steps [steps +inf.0] #:visit [visit void] 
                                  #:cache-all? [cache-all? (current-cache-all?)]
                                  #:stop-when [stop-when (λ (x) #f)])
  (define visited (and cache-all? (make-hash)))
  (let/ec return
    (let ([answers (make-hash)]
          [cycle? #f]
          [cutoff? #f])
      (let loop ([term start]
                 ;; It would be better to record all visited terms, to avoid traversing
                 ;; any part of the graph multiple times. Results from 
                 ;;    collects/redex/trie-experiment
                 ;; in commit
                 ;;    152084d5ce6ef49df3ec25c18e40069950146041
                 ;; suggest that a hash works better than a trie.
                 [path (make-immutable-hash '())]
                 [more-steps steps])
        (if (and goal? (goal? term))
            (return (search-success))
            (cond
              [(hash-ref path term #f)
               (set! cycle? #t)]
              [else
               (visit term)
               (cond
                 [(stop-when term)
                  (unless goal? 
                    (hash-set! answers term #t))]
                 [else
                  (define nexts (apply-reduction-relation reductions term))
                  (cond
                    [(null? nexts) 
                     (unless goal? 
                       (hash-set! answers term #t))]
                    [else (if (zero? more-steps)
                              (set! cutoff? #t)
                              (for ([next (in-list (remove-duplicates nexts))])
                                (when (or (not visited)
                                          (not (hash-ref visited next #f)))
                                  (when visited (hash-set! visited next #t))
                                  (loop next 
                                        (hash-set path term #t) 
                                        (sub1 more-steps)))))])])])))
      (if goal?
          (search-failure cutoff?)
          (values (sort (hash-map answers (λ (x y) x))
                        string<=?
                        #:key (λ (x) (format "~s" x)))
                  cycle?)))))

(define current-cache-all? (make-parameter #f))

;; map/mt : (a -> b) (listof a) (listof b) -> (listof b)
;; map/mt is like map, except
;;  a) it uses the last argument instead of the empty list
;;  b) if `f' returns #f, that is not included in the result
(define (map/mt f l mt-l)
  (let loop ([l l])
    (cond
      [(null? l) mt-l]
      [else
       (let ([this-one (f (car l))])
         (if this-one
             (cons this-one (loop (cdr l)))
             (loop (cdr l))))])))

(define (reduction-relation->rule-names x) 
  (reverse (reduction-relation-rule-names x)))


;                                                                               
;                                                                               
;                                                                               
;      ;                     ;                         ;;     ;                 
;     ;;                    ;;                         ;;    ;;                 
;   ;;;;;   ;;;    ;;;;;  ;;;;;      ;;;;; ;;;; ;;;;       ;;;;;   ;;;    ;;;;; 
;  ;;;;;;  ;;;;;  ;;;;;; ;;;;;;     ;;;;;; ;;;; ;;;; ;;;; ;;;;;;  ;;;;;  ;;;;;; 
;   ;;;;  ;;;; ;; ;;;;    ;;;;      ;;;;   ;;;; ;;;; ;;;;  ;;;;  ;;;; ;; ;;;;   
;   ;;;;  ;;;;;;;  ;;;;   ;;;;       ;;;;  ;;;; ;;;; ;;;;  ;;;;  ;;;;;;;  ;;;;  
;   ;;;;; ;;;;;     ;;;;  ;;;;;       ;;;; ;;;; ;;;; ;;;;  ;;;;; ;;;;;     ;;;; 
;   ;;;;;  ;;;;;; ;;;;;;  ;;;;;     ;;;;;; ;;;;;;;;; ;;;;  ;;;;;  ;;;;;; ;;;;;; 
;    ;;;;   ;;;;  ;;;;;    ;;;;     ;;;;;   ;;; ;;;; ;;;;   ;;;;   ;;;;  ;;;;;  
;                                                                               
;                                                                               
;                                                                               

(define tests 0)
(define test-failures 0)
(define (inc-failures) (set! test-failures (+ test-failures 1)))
(define (inc-tests) (set! tests (+ tests 1)))

(define (test-results)
  (cond
    [(= tests 0)
     (printf "No tests run.\n")]
    [(= test-failures 0)
     (cond
       [(= tests 1)
        (printf "One test passed.\n")]
       [(= tests 2)
        (printf "Both tests passed.\n")]
       [else
        (printf "All ~a tests passed.\n" tests)])]
    [else
     (printf "~a test~a failed (out of ~a total).\n"
             test-failures
             (if (= test-failures 1) "" "s")
             tests)])
  (set! tests 0)
  (set! test-failures 0))

(define-for-syntax (get-srcloc stx)
  #`(list 
     '#,(and (path? (syntax-source stx))
             (path->relative-string/library (syntax-source stx)))
     '#,(syntax-line stx)
     '#,(syntax-column stx)
     '#,(syntax-position stx)))

(define-for-syntax test-equiv-ctc
  #'(-> any/c any/c any/c))
(define-for-syntax test-equiv-name
  "#:equiv argument")
(define-for-syntax test-equiv-default
  #'(default-equiv))

(define-syntax (test-->> stx)
  (syntax-parse stx
    [(form red:expr
           (~or (~optional (~seq (~and #:cycles-ok (~bind [cycles-ok? #t])))
                           #:defaults ([cycles-ok? #f])
                           #:name "#:cycles-ok argument")
                (~optional (~seq #:equiv equiv?)
                           #:defaults ([equiv?.c test-equiv-default])
                           #:name test-equiv-name)
                (~optional (~seq #:pred pred)
                           #:defaults ([pred #f])
                           #:name "#:pred argument"))
           ...
           e1:expr
           e2:expr ...)
     #:declare equiv? (expr/c test-equiv-ctc #:name test-equiv-name)
     #`(test-->>/procs 'test-->> red e1 (list e2 ...) 
                       traverse-reduction-graph
                       #,(attribute cycles-ok?)
                       equiv?.c
                       #,(attribute pred)
                       #,(get-srcloc stx))]))

(define-syntax (test--> stx)
  (syntax-parse stx
    [(form red:expr
           (~optional (~seq #:equiv equiv?)
                      #:defaults ([equiv?.c test-equiv-default]))
           e1:expr
           e2:expr ...)
     #:declare equiv? (expr/c test-equiv-ctc #:name test-equiv-name)
     #`(test-->>/procs 'test--> red e1 (list e2 ...) apply-reduction-relation/dummy-second-value #t equiv?.c #f #,(get-srcloc stx))]))

(define (apply-reduction-relation/dummy-second-value red arg #:visit visit)
  (values (apply-reduction-relation red arg) #f))

(define (test-->>/procs name red arg expected apply-red cycles-ok? equiv? pred srcinfo)
  (unless (reduction-relation? red)
    (error name "expected a reduction relation as first argument, got ~e" red))
  (when pred
    (unless (and (procedure? pred)
                 (procedure-arity-includes? pred 1))
      (error 'test-->> "expected a procedure that accepted one argument for the #:pred, got ~e" pred)))
  (inc-tests)
  (define visit-already-failed? #f)
  (define (visit t)
    (when pred
      (unless visit-already-failed?
        (unless (pred t)
          (set! visit-already-failed? #t)
          (inc-failures)
          (print-failed srcinfo)
          (eprintf "found a term that failed #:pred: ~v\n" t)))))
  (let-values ([(got got-cycle?) (apply-red red arg #:visit visit)])
    
    (cond
      [(and got-cycle?
            (not cycles-ok?))
       (inc-failures)
       (print-failed srcinfo)
       (eprintf "found a cycle in the reduction graph\n")]
      [else
       (unless visit-already-failed?
         (let* ([⊆ (λ (s1 s2) (andmap (λ (x1) (memf (λ (x) (equiv? x1 x)) s2)) s1))]
                [set-equal? (λ (s1 s2) (and (⊆ s1 s2) (⊆ s2 s1)))])
           (unless (set-equal? expected got)
             (inc-failures)
             (print-failed srcinfo)
             (for-each
              (λ (v2) (eprintf "expected: ~v\n" v2))
              expected)
             (if (empty? got)
                 (eprintf "got nothing\n")
                 (for-each
                  (λ (v1) (eprintf "  actual: ~v\n" v1))
                  got)))))])))

(define-syntax (test-->>∃ stx)
  (syntax-parse stx
    [(form (~optional (~seq #:steps steps) #:defaults ([steps.c #'1000]))
           relation
           start:expr
           goal)
     #:declare relation (expr/c #'reduction-relation? 
                                #:name "reduction relation expression")
     #:declare goal (expr/c #'(or/c (-> any/c any/c) (not/c procedure?)) 
                            #:name "goal expression")
     #:declare steps (expr/c #'(or/c natural-number/c +inf.0) 
                             #:name "steps expression")
     #`(test-->>∃/proc relation.c start goal.c steps.c #,(get-srcloc stx))]))

(define (test-->>∃/proc relation start goal steps srcinfo)
  (let ([result (traverse-reduction-graph 
                 relation
                 start 
                 #:goal (if (procedure? goal) goal (λ (x) (equal? goal x)))
                 #:steps steps)])
    (inc-tests)
    (when (search-failure? result)
      (print-failed srcinfo)
      (inc-failures)
      (begin
        (if (procedure? goal)
            (eprintf "no term satisfying ~a reachable from ~a" goal start)
            (eprintf "term ~a not reachable from ~a" goal start))
        (when (search-failure-cutoff? result)
          (eprintf " (within ~a steps)" steps))
        (newline (current-error-port))))))

(define-syntax (test-predicate stx)
  (syntax-case stx ()
    [(_ p arg)
     #`(test-predicate/proc p arg #,(get-srcloc stx))]))

(define (test-predicate/proc pred arg srcinfo)
  (inc-tests)
  (unless (pred arg)
    (inc-failures)
    (print-failed srcinfo)
    (eprintf "  ~v does not hold for\n  ~v\n" 
             pred arg)))

(define default-equiv (make-parameter equal?))

(define-syntax (test-equal stx)
  (syntax-case stx ()
    [(_ e1 e2)
     #`(test-equal/proc e1 e2 #,(get-srcloc stx) #,test-equiv-default)]
    [(_ e1 e2 #:equiv ~equal?)
     #`(test-equal/proc e1 e2 #,(get-srcloc stx) ~equal?)]))

(define (test-equal/proc v1 v2 srcinfo equal?)
  (inc-tests)
  (unless (equal? v1 v2)
    (inc-failures)
    (print-failed srcinfo)
    (eprintf "  actual: ~v\n" v1)
    (eprintf "expected: ~v\n" v2)))

(define (print-failed srcinfo)
  (let ([file (list-ref srcinfo 0)]
        [line (list-ref srcinfo 1)]
        [column (list-ref srcinfo 2)]
        [pos (list-ref srcinfo 3)])
    (eprintf "FAILED ~a~a\n"
             (cond
               [(string? file) file]
               [else ""])
             (cond
               [(and line column)
                (format ":~a.~a" line column)]
               [pos 
                (format "::~a" pos)]
               [else #f]))))

(provide (rename-out [-reduction-relation reduction-relation])
         ::=
         reduction-relation->rule-names
         extend-reduction-relation
         reduction-relation?
         union-reduction-relations
         
         compatible-closure
         context-closure
         
         define-language
         define-extended-language
         define-union-language
         
         define-metafunction
         define-metafunction/extension
         define-relation
         
         (rename-out [metafunction-form metafunction])
         metafunction? metafunction-proc
         in-domain?
         metafunc-proc-lang
         metafunc-proc-clause-names
         metafunc-proc-pict-info
         metafunc-proc-name
         metafunc-proc-multi-arg?
         metafunc-proc-in-dom?
         metafunc-proc-dom-pat
         metafunc-proc-cases
         metafunc-proc?
         (struct-out metafunc-case)
         
         (struct-out binds))

(provide test-match
         test-match?
         term-match
         term-match/single
         redex-let 
         redex-let*
         make-bindings bindings-table bindings?
         match? match-bindings
         make-bind bind? bind-name bind-exp
         make-match
         
         test-equal
         test-->>
         test-->
         test-->>∃ (rename-out [test-->>∃ test-->>E])
         test-predicate
         test-results
         default-equiv)


(provide language-nts
         apply-reduction-relation
         apply-reduction-relation/tag-with-names
         apply-reduction-relation/tagged
         apply-reduction-relation*
         current-cache-all?
         variable-not-in
         variables-not-in)

(provide relation-coverage
         covered-cases
         (rename-out [fresh-coverage make-coverage])
         coverage?)

(provide do-test-match)
