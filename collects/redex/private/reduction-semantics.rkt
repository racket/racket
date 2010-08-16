#lang scheme/base

(require "matcher.ss"
         "struct.ss"
         "term.ss"
         "fresh.ss"
         "loc-wrapper.ss"
	 "error.ss"
         mzlib/trace
         (lib "list.ss")
         (lib "etc.ss")
         (for-syntax syntax/parse))

(require (for-syntax (lib "name.ss" "syntax")
                     "loc-wrapper-ct.ss"
                     "rewrite-side-conditions.ss"
                     "term-fn.ss"
                     "underscore-allowed.ss"
                     (lib "boundmap.ss" "syntax")
                     scheme/base
                     (prefix-in pattern- scheme/match)))

(define (language-nts lang)
  (hash-map (compiled-lang-ht lang) (λ (x y) x)))

(define-for-syntax (prune-syntax stx)
  (datum->syntax
   (identifier-prune-lexical-context #'whatever '(#%app #%datum))
   (let loop ([stx stx])
     (syntax-case stx ()
       [(a . b)
        (datum->syntax (identifier-prune-lexical-context #'whatever '(#%app))
                       (cons (loop #'a) (loop #'b))
                       stx)]
       [x 
        (identifier? #'x)
        (identifier-prune-lexical-context #'x)]
       [() (datum->syntax #f '() stx)]
       [_ (datum->syntax (identifier-prune-lexical-context #'whatever '(#%datum))
                         (syntax->datum stx) stx)]))))

(define-syntax (term-match/single stx)
  (syntax-case stx ()
    [(_ lang [pattern rhs] ...)
     (begin
       (unless (identifier? #'lang)
         (raise-syntax-error 'term-match/single "expected an identifier in the language position" stx #'lang))
       (let ([lang-nts (language-id-nts #'lang 'term-match/single)])
         (with-syntax ([(((names ...) (names/ellipses ...)) ...)
                        (map (λ (x) (let-values ([(names names/ellipses) (extract-names lang-nts 'term-match/single #t x)])
                                      (list names names/ellipses)))
                             (syntax->list (syntax (pattern ...))))]
                       [(side-conditions-rewritten ...)
                        (map (λ (x) (rewrite-side-conditions/check-errs lang-nts 'term-match #t x))
                             (syntax->list (syntax (pattern ...))))]
                       [(cp-x ...) (generate-temporaries #'(pattern ...))])
           #'(let ([lang-x lang])
               (let ([cp-x (compile-pattern lang-x `side-conditions-rewritten #t)] ...)
                 (λ (exp)
                   ((let/ec k
                      (let ([match (match-pattern cp-x exp)])
                        (when match
                          (unless (null? (cdr match))
                            (redex-error
                             'term-match/single
                             "pattern ~s matched term ~e multiple ways"
                             'pattern
                             exp))
                          (k (λ ()
                               (term-let/error-name 
                                term-match/single
                                ([names/ellipses (lookup-binding (mtch-bindings (car match)) 'names)] ...)
                                rhs)))))
                      ...
                      (redex-error 'term-match/single "no patterns matched ~e" exp)))))))))]))

(define-syntax (term-match stx)
  (syntax-case stx ()
    [(_ lang [pattern rhs] ...)
     (begin
       (unless (identifier? #'lang)
         (raise-syntax-error 'term-match "expected an identifier" stx #'lang))
       (let ([lang-nts (language-id-nts #'lang 'term-match)])
         (with-syntax ([(((names ...) (names/ellipses ...)) ...)
                        (map (λ (x) (let-values ([(names names/ellipses) (extract-names lang-nts 'term-match #t x)])
                                      (list names names/ellipses)))
                             (syntax->list (syntax (pattern ...))))]
                       [(side-conditions-rewritten ...)
                        (map (λ (x) (rewrite-side-conditions/check-errs lang-nts 'term-match #t x))
                             (syntax->list (syntax (pattern ...))))]
                       [(cp-x ...) (generate-temporaries #'(pattern ...))])
           #'(let ([lang-x lang])
               (let ([cp-x (compile-pattern lang-x `side-conditions-rewritten #t)] ...)
                 (λ (exp)
                   (append
                    (let ([matches (match-pattern cp-x exp)])
                      (if matches
                          (map (λ (match)
                                 (term-let/error-name
                                  term-match
                                  ([names/ellipses (lookup-binding (mtch-bindings match) 'names)] ...)
                                   rhs))
                               matches)
                          '())) ...)))))))]))

(define-syntax (compatible-closure stx)
  (syntax-case stx ()
    [(_ red lang nt)
     (identifier? (syntax nt))
     (with-syntax ([side-conditions-rewritten
                    (rewrite-side-conditions/check-errs (language-id-nts #'lang 'compatible-closure)
                                                        'compatible-closure 
                                                        #t
                                                        (syntax (cross nt)))])
       (syntax (do-context-closure red lang `side-conditions-rewritten 'compatible-closure)))]
    [(_ red lang nt)
     (raise-syntax-error 'compatible-closure "expected a non-terminal as last argument" stx (syntax nt))]))

(define-syntax (context-closure stx)
  (syntax-case stx ()
    [(_ red lang pattern)
     (with-syntax ([side-conditions-rewritten
                    (rewrite-side-conditions/check-errs (language-id-nts #'lang 'context-closure)
                                                        'context-closure
                                                        #t
                                                        (syntax pattern))])
       (syntax
        (do-context-closure
         red
         lang
         `side-conditions-rewritten
         'context-closure)))]))

(define (do-context-closure red lang pat name)
  (unless (reduction-relation? red)
    (error name "expected <reduction-relation> as first argument, got ~e" red))
  (unless (compiled-lang? lang)
    (error name "expected <lang> as second argument, got ~e" lang))
  (let ([cp (compile-pattern
             lang
             `(in-hole (name ctxt ,pat)
                       (name exp any))
             #f)])
    (build-reduction-relation
     #f
     lang
     (map
      (λ (make-proc)
        (make-rewrite-proc
         (λ (lang)
           (let ([f (make-proc lang)])
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
                               acc)))])))))
         (rewrite-proc-name make-proc)
         (rewrite-proc-lhs make-proc)
         (rewrite-proc-lhs-src make-proc)
         (rewrite-proc-id make-proc)))
      (reduction-relation-make-procs red))
     (reduction-relation-rule-names red)
     (reduction-relation-lws red)
     `any)))

(define-syntax (--> stx) (raise-syntax-error '--> "used outside of reduction-relation"))
(define-syntax (fresh stx) (raise-syntax-error 'fresh "used outside of reduction-relation"))
(define-syntax (with stx) (raise-syntax-error 'with "used outside of reduction-relation"))

(define (apply-reduction-relation/tag-with-names p v)
  (let loop ([procs (reduction-relation-procs p)]
             [acc '()])
    (cond
      [(null? procs) acc]
      [else 
       (loop (cdr procs)
             ((car procs) v v values acc))])))

(define (apply-reduction-relation p v) (map cadr (apply-reduction-relation/tag-with-names p v)))

(define-for-syntax (extract-pattern-binds lhs)
  (let loop ([lhs lhs])
    (syntax-case* lhs (name) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
      [(name id expr)
       (identifier? #'id)
       (cons (cons #'id #'expr) (loop #'expr))]
      ;; FIXME: should follow the grammar of patterns!
      [(a . b)
       (append (loop #'a) (loop #'b))]
      [_else null])))

(define-for-syntax (extract-term-let-binds lhs)
  (let loop ([lhs lhs])
    (syntax-case* lhs (term-let) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
      [(term-let ((x e1) ...) e2 ...)
       (append (map cons
                    (syntax->list #'(x ...))
                    (syntax->list #'(e1 ...)))
               (loop #'(e2 ...)))]
      ;; FIXME: should follow the grammar of patterns!
      [(a . b)
       (append (loop #'a) (loop #'b))]
      [_else null])))

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

;; the withs, freshs, and side-conditions come in backwards order
(define-for-syntax (bind-withs orig-name main lang lang-nts stx where-mode body names w/ellipses)
  (let loop ([stx stx]
             [to-not-be-in main]
             [env (make-immutable-hash
                   (map (λ (x e) (cons (syntax-e x) e))
                        names w/ellipses))])
    (syntax-case stx (fresh)
      [() body]
      [((-where x e) y ...)
       (or (free-identifier=? #'-where #'where)
           (free-identifier=? #'-where #'where/hidden))
       (let-values ([(names names/ellipses) (extract-names lang-nts 'reduction-relation #t #'x)])
         (let ([env+ (for/fold ([env env])
                       ([name names] 
                        [w/ellipses names/ellipses])
                       (hash-set env (syntax-e name) w/ellipses))]
               [temporaries (generate-temporaries names)])
           (define (id/depth stx)
             (syntax-case stx ()
               [(s (... ...))
                (let ([r (id/depth #'s)])
                  (make-id/depth (id/depth-id r) (add1 (id/depth-depth r))))]
               [s (make-id/depth #'s 0)]))
           (with-syntax ([(binding-constraints ...)
                          (for/fold ([cs '()])
                            ([n names]
                             [w/e names/ellipses]
                             [x temporaries])
                            (cond [(hash-ref env (syntax-e n) #f)
                                   => (λ (b) 
                                        (let ([b-id/depth (id/depth b)]
                                              [n-id/depth (id/depth w/e)])
                                          (if (= (id/depth-depth b-id/depth) (id/depth-depth n-id/depth))
                                              (cons #`(equal? #,x (term #,b)) cs)
                                              (raise-ellipsis-depth-error
                                               orig-name
                                               (id/depth-id n-id/depth) (id/depth-depth n-id/depth)
                                               (id/depth-id b-id/depth) (id/depth-depth b-id/depth)))))]
                                  [else cs]))])
             (with-syntax ([side-conditions-rewritten (rewrite-side-conditions/check-errs
                                                       lang-nts
                                                       'reduction-relation
                                                       #f
                                                       #'x)]
                           [(names ...) names]
                           [(names/ellipses ...) names/ellipses]
                           [(x ...) temporaries])
               (let ([rest-body (loop #'(y ...) #`(list x ... #,to-not-be-in) env+)])
                 #`(let* ([mtchs (match-pattern (compile-pattern #,lang `side-conditions-rewritten #t) (term e))]
                          [result (λ (mtch)
                                    (let ([bindings (mtch-bindings mtch)])
                                      (let ([x (lookup-binding bindings 'names)] ...)
                                        (and binding-constraints ...
                                             (term-let ([names/ellipses x] ...) 
                                                       #,rest-body)))))])
                     (if mtchs
                         #,
                         (case where-mode
                           [(flatten)
                            #`(for/fold ([r '()]) ([m mtchs])
                                (let ([s (result m)])
                                  (if s (append s r) r)))]
                           [(predicate) 
                            #`(ormap result mtchs)]
                           [else (error 'unknown-where-mode "~s" where-mode)])
                         #f)))))))]
      [((-side-condition s ...) y ...)
       (or (free-identifier=? #'-side-condition #'side-condition)
           (free-identifier=? #'-side-condition #'side-condition/hidden))
       #`(and s ... #,(loop #'(y ...) to-not-be-in env))]
      [((fresh x) y ...)
       (identifier? #'x)
       #`(term-let ([x (variable-not-in #,to-not-be-in 'x)]) 
                   #,(loop #'(y ...) #`(list (term x) #,to-not-be-in) env))]
      [((fresh x name) y ...)
       (identifier? #'x)
       #`(term-let ([x (let ([the-name (term name)])
                         (verify-name-ok '#,orig-name the-name)
                         (variable-not-in #,to-not-be-in the-name))])
                   #,(loop #'(y ...) #`(list (term x) #,to-not-be-in) env))]
      [((fresh (y) (x ...)) z ...)
       #`(term-let ([(y #,'...)
                     (variables-not-in #,to-not-be-in 
                                       (map (λ (_ignore_) 'y)
                                            (term (x ...))))])
                   #,(loop #'(z ...) #`(list (term (y #,'...)) #,to-not-be-in) env))]
      [((fresh (y) (x ...) names) z ...)
       #`(term-let ([(y #,'...)
                     (let ([the-names (term names)]
                           [len-counter (term (x ...))])
                       (verify-names-ok '#,orig-name the-names len-counter)
                       (variables-not-in #,to-not-be-in the-names))])
                   #,(loop #'(z ...) #`(list (term (y #,'...)) #,to-not-be-in) env))])))

(define-syntax-set (do-reduction-relation)
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
  
  (define (rule->lws rule)
    (syntax-case rule ()
      [(arrow lhs rhs stuff ...)
       (let-values ([(label scs/withs fvars)
                     (let loop ([stuffs (syntax->list #'(stuff ...))]
                                [label #f]
                                [scs/withs null]
                                [fvars null])
                       (cond
                         [(null? stuffs) (values label (reverse scs/withs) (reverse fvars))]
                         [else
                          (syntax-case (car stuffs) (where where/hidden
                                                           side-condition side-condition/hidden
                                                           fresh variable-not-in)
                            [(fresh xs ...) 
                             (loop (cdr stuffs)
                                   label
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
                                   (cons #`(cons #,(to-lw/proc #'x) #,(to-lw/proc #'e))
                                         scs/withs)
                                   fvars)]
                            [(where/hidden x e)
                             (loop (cdr stuffs) label scs/withs fvars)]
                            [(side-condition sc)
                             (loop (cdr stuffs)
                                   label
                                   (cons (to-lw/uq/proc #'sc) scs/withs)
                                   fvars)]
                            [(side-condition/hidden sc)
                             (loop (cdr stuffs) label scs/withs fvars)]
                            [x
                             (identifier? #'x)
                             (loop (cdr stuffs)
                                   #''x
                                   scs/withs
                                   fvars)]
                            [x
                             (string? (syntax-e #'x))
                             (loop (cdr stuffs)
                                   #'x
                                   scs/withs
                                   fvars)])]))])
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
                             (list fvars ...))))]))
  
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
                     (raise-syntax-error orig-name
                                         "malformed shortcut, expected only two subparts for a shortcut definition, found an extra one"
                                         stx
                                         (syntax c))]
                    [_ (raise-syntax-error orig-name
                                           "malformed shortcut"
                                           stx shortcut)]))
                shortcuts)
            
      (for-each (λ (rule)
                  (syntax-case rule ()
                    [(arrow . rst)
                     (begin
                       (set! all-top-levels (cons #'arrow all-top-levels))
                       (table-cons! ht (syntax arrow) rule))]))
                rules)
      
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
      
      (let ([name-table (make-hasheq)]
            [lang-nts (language-id-nts lang-id orig-name)])
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
                      
                      [domain-pattern-side-conditions-rewritten
                       (rewrite-side-conditions/check-errs
                        lang-nts
                        orig-name
                        #f
                        domain-pattern)])
                      
          #`(build-reduction-relation
             #,orig-red-expr
             lang-id
             (list top-level ...)
             '(rule-names ...)
             lws
             `domain-pattern-side-conditions-rewritten)))))
  
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
       (let* ([lang-nts (language-id-nts lang-id orig-name)]
              [rewrite-side-conds
               (λ (pat) (rewrite-side-conditions/check-errs lang-nts orig-name #t pat))])
         (let-values ([(names names/ellipses) (extract-names lang-nts orig-name #t (syntax rhs-from))])
           (with-syntax ([(names ...) names]
                         [(names/ellipses ...) names/ellipses]
                         [side-conditions-rewritten (rewrite-side-conds
                                                     (rewrite-node-pat (syntax-e (syntax lhs-frm-id))
                                                                       (syntax rhs-from)))]
                         [fresh-rhs-from (rewrite-side-conds 
                                          (freshen-names #'rhs-from #'lhs-frm-id lang-nts orig-name))]
                         [lang lang])
             (map
              (λ (child-proc)
                #`(do-node-match
                   'lhs-frm-id
                   'lhs-to-id
                   `side-conditions-rewritten
                   (λ (bindings rhs-binder)
                     (term-let ([lhs-to-id rhs-binder]
                                [names/ellipses (lookup-binding bindings 'names)] ...)
                               (term rhs-to)))
                   #,child-proc
                   `fresh-rhs-from))
              (get-choices stx orig-name bm #'lang
                           (syntax lhs-arrow) 
                           name-table lang-id 
                           allow-zero-rules?)))))]))
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
    (let-values ([(bound _) (extract-names nts what #t pat #f)])
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
    (let* ([lang-nts (language-id-nts lang-id orig-name)]
           [rw-sc (λ (pat) (rewrite-side-conditions/check-errs lang-nts orig-name #t pat))])
      (let-values ([(name sides/withs/freshs) (process-extras stx orig-name name-table extras)])
        (let*-values ([(names names/ellipses) (extract-names lang-nts orig-name #t from)]
                      [(body-code)
                       (bind-withs orig-name 
                                   #'main-exp
                                   lang
                                   lang-nts
                                   sides/withs/freshs
                                   'flatten
                                   #`(list (term #,to))
                                   names names/ellipses)]
                      [(test-case-body-code)
                       ;; this contains some redundant code
                       (bind-withs orig-name
                                   #'#t 
                                   #'lang-id2
                                   lang-nts
                                   sides/withs/freshs
                                   'predicate
                                   #'#t
                                   names names/ellipses)])
          (with-syntax ([side-conditions-rewritten (rw-sc from)]
                        [lhs-w/extras (rw-sc #`(side-condition #,from #,test-case-body-code))]
                        [lhs-source (format "~a:~a:~a"
                                            (syntax-source from)
                                            (syntax-line from)
                                            (syntax-column from))]
                        [name name]
                        [lang lang]
                        [(names ...) names]
                        [(names/ellipses ...) names/ellipses]
                        [body-code body-code])
            #`
            (let ([case-id (gensym)])
              (make-rewrite-proc
               (λ (lang-id)
                 (let ([cp (compile-pattern lang-id `side-conditions-rewritten #t)])
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
                                       [really-matched 
                                        (term-let ([names/ellipses (lookup-binding bindings 'names)] ...)
                                                  body-code)])
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
                                           (map/mt (λ (x) (list name (f x))) really-matched acc))]
                                    [else 
                                     (loop (cdr mtchs) acc)]))]))
                           other-matches)))))
               name
               (λ (lang-id2) `lhs-w/extras)
               lhs-source
               case-id)))))))
  
  (define (process-extras stx orig-name name-table extras)
    (let* ([the-name #f]
           [the-name-stx #f]
           [sides/withs/freshs 
            (let loop ([extras extras])
              (cond
                [(null? extras) '()]
                [else
                 (syntax-case (car extras) (fresh)
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
                    (or (free-identifier=? #'-side-condition #'side-condition)
                        (free-identifier=? #'-side-condition #'side-condition/hidden))
                    (cons (car extras) (loop (cdr extras)))]
                   [(-where x e)
                    (or (free-identifier=? #'-where #'where)
                        (free-identifier=? #'-where #'where/hidden))
                    (cons (car extras) (loop (cdr extras)))]
                   [(-where . x)
                    (or (free-identifier=? #'-where #'where)
                        (free-identifier=? #'-where #'where/hidden))
                    (raise-syntax-error orig-name "malformed where clause" stx (car extras))]
                   [_
                    (raise-syntax-error orig-name "unknown extra" stx (car extras))])]))])
      (values the-name sides/withs/freshs)))
  

  
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
                (loop (cdr stxs)))])))))

(define (substitute from to pat)
  (let recur ([p pat])
    (syntax-case p (side-condition)
      [(side-condition p c)
       #`(side-condition #,(recur #'p) c)]
      [(p ...)
       #`(#,@(map recur (syntax->list #'(p ...))))]
      [else
       (if (and (identifier? p) (bound-identifier=? p from))
           to
           p)])))

(define (verify-name-ok orig-name the-name)
  (unless (symbol? the-name)
    (error orig-name "expected a single name, got ~s" the-name)))

(define (verify-names-ok orig-name the-names len-counter)
  (unless (and (list? the-names)
               (andmap symbol? the-names))
    (error orig-name
           "expected a sequence of names, got ~s"
           the-names))
  (unless (= (length len-counter)
             (length the-names))
    (error orig-name
           "expected the length of the sequence of names to be ~a, got ~s"
           (length len-counter)
           the-names)))

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
    (build-reduction-relation
     #f
     first-lang
     (reverse (apply append (map reduction-relation-make-procs lst)))
     (map car (sort (hash-map name-ht list) < #:key cadr))
     (apply append (map reduction-relation-lws lst))
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
  (hash-map (coverage-counts cov) (λ (k v) v)))

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

(define-syntax (test-match stx)
  (syntax-case stx ()
    [(_ lang-exp pattern)
     (identifier? #'lang-exp)
     (with-syntax ([side-condition-rewritten (rewrite-side-conditions/check-errs 
                                              (language-id-nts #'lang-exp 'redex-match) 
                                              'redex-match 
                                              #t
                                              (syntax pattern))])
       (syntax 
        (do-test-match lang-exp `side-condition-rewritten)))]
    [(_ lang-exp pattern expression)
     (identifier? #'lang-exp)
     (syntax 
      ((test-match lang-exp pattern) expression))]
    [(_ a b c)
     (raise-syntax-error 'redex-match "expected an identifier (bound to a language) as first argument" stx #'a)]
    [(_ a b)
     (raise-syntax-error 'redex-match "expected an identifier (bound to a language) as first argument" stx #'a)]))

(define-struct match (bindings) #:inspector #f)

(define (do-test-match lang pat)
  (unless (compiled-lang? lang)
    (error 'redex-match "expected first argument to be a language, got ~e" lang))
  (let ([cpat (compile-pattern lang pat #t)])
    (λ (exp)
      (let ([ans (match-pattern cpat exp)])
        (and ans
             (map (λ (m) (make-match (sort-bindings (bindings-table (mtch-bindings m)))))
                  ans))))))

(define (sort-bindings bnds)
  (sort
   bnds
   (λ (x y) (string-ci<=? (symbol->string (bind-name x))
                          (symbol->string (bind-name y))))))

(define-values (struct:metafunc-proc make-metafunc-proc metafunc-proc? metafunc-proc-ref metafunc-proc-set!)
  (make-struct-type 'metafunc-proc #f 8 0 #f null (current-inspector) 0))
(define metafunc-proc-pict-info (make-struct-field-accessor metafunc-proc-ref 1))
(define metafunc-proc-lang (make-struct-field-accessor metafunc-proc-ref 2))
(define metafunc-proc-multi-arg? (make-struct-field-accessor metafunc-proc-ref 3))
(define metafunc-proc-name (make-struct-field-accessor metafunc-proc-ref 4))
(define metafunc-proc-in-dom? (make-struct-field-accessor metafunc-proc-ref 5))
(define metafunc-proc-dom-pat (make-struct-field-accessor metafunc-proc-ref 6))
(define metafunc-proc-cases (make-struct-field-accessor metafunc-proc-ref 7))

(define-struct metafunction (proc))

(define-struct metafunc-case (cp rhs lhs-pat src-loc id))

;; Intermediate structures recording clause "extras" for typesetting.
(define-struct metafunc-extra-side-cond (expr))
(define-struct metafunc-extra-where (lhs rhs))
(define-struct metafunc-extra-fresh (vars))

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

(define-syntax-set (define-metafunction define-metafunction/extension define-relation)
  
  (define (define-metafunction/proc stx)
    (syntax-case stx ()
      [(_ . rest)
       (internal-define-metafunction stx #f #'rest #f)]))
  
  (define (define-relation/proc stx)
    (syntax-case stx ()
      [(_ . rest)
       ;; need to rule out the contracts for this one
       (internal-define-metafunction stx #f #'rest #t)]))
  
  (define (define-metafunction/extension/proc stx)
    (syntax-case stx ()
      [(_ prev . rest)
       (identifier? #'prev)
       (internal-define-metafunction stx #'prev #'rest #f)]))
  
  (define (internal-define-metafunction orig-stx prev-metafunction stx relation?)
    (syntax-case stx ()
      [(lang . rest)
       (let ([syn-error-name (if relation?
                                 'define-relation
                                 (if prev-metafunction
                                     'define-metafunction/extension
                                     'define-metafunction))])
         (unless (identifier? #'lang)
           (raise-syntax-error syn-error-name "expected an identifier in the language position" orig-stx #'lang))
         (when (null? (syntax-e #'rest))
           (raise-syntax-error syn-error-name "no clauses" orig-stx))
         (when prev-metafunction
           (syntax-local-value 
            prev-metafunction
            (λ ()
              (raise-syntax-error syn-error-name "expected a previously defined metafunction" orig-stx prev-metafunction))))
         (prune-syntax
          (let ([lang-nts (language-id-nts #'lang 'define-metafunction)])  ;; keep this near the beginning, so it signals the first error (PR 10062)
          (let-values ([(contract-name dom-ctcs codom-contract pats)
                        (split-out-contract orig-stx syn-error-name #'rest relation?)])
            (with-syntax ([(((original-names lhs-clauses ...) raw-rhses ...) ...) pats]
                          [(lhs-for-lw ...)
                           (with-syntax ([((lhs-for-lw _ ...) ...) pats])
                             (map (λ (x) (to-lw/proc (datum->syntax #f (cdr (syntax-e x)) x)))
                                  (syntax->list #'(lhs-for-lw ...))))])
              (with-syntax ([((rhs stuff ...) ...) (if relation?
                                                       #'((,(and (term raw-rhses) ...)) ...)
                                                       #'((raw-rhses ...) ...))])
              (parameterize ([is-term-fn? 
                              (let ([names (syntax->list #'(original-names ...))])
                                (λ (x) (and (not (null? names))
                                            (identifier? (car names))
                                            (free-identifier=? x (car names)))))])
                (with-syntax ([(lhs ...) #'((lhs-clauses ...) ...)]
                              [name (let loop ([name (if contract-name
                                                         contract-name
                                                         (car (syntax->list #'(original-names ...))))]
                                               [names (if contract-name
                                                          (syntax->list #'(original-names ...))
                                                          (cdr (syntax->list #'(original-names ...))))])
                                      (cond
                                        [(null? names) name]
                                        [else
                                         (unless (eq? (syntax-e name) (syntax-e (car names)))
                                           (raise
                                            (make-exn:fail:syntax
                                             (if contract-name
                                                 "define-metafunction: expected each clause and the contract to use the same name"
                                                 "define-metafunction: expected each clause to use the same name")
                                             (current-continuation-marks)
                                             (list name
                                                   (car names)))))
                                         (loop name (cdr names))]))])
                  (when (and prev-metafunction (eq? (syntax-e #'name) (syntax-e prev-metafunction)))
                    (raise-syntax-error syn-error-name "the extended and extending metafunctions cannot share a name" orig-stx prev-metafunction))
                  (parse-extras #'((stuff ...) ...))
                  (let*-values ([(lhs-namess lhs-namess/ellipsess)
                                 (let loop ([lhss (syntax->list (syntax (lhs ...)))])
                                   (if (null? lhss)
                                       (values null null)
                                       (let-values ([(namess namess/ellipsess)
                                                     (loop (cdr lhss))]
                                                    [(names names/ellipses)
                                                     (extract-names lang-nts syn-error-name #t (car lhss))])
                                         (values (cons names namess)
                                                 (cons names/ellipses namess/ellipsess)))))])
                    (with-syntax ([(rhs/wheres ...)
                                   (map (λ (sc/b rhs names names/ellipses)
                                          (bind-withs
                                           syn-error-name '()  
                                           #'lang lang-nts
                                           sc/b 'flatten
                                           #`(list (term #,rhs))
                                           names names/ellipses))
                                        (syntax->list #'((stuff ...) ...))
                                        (syntax->list #'(rhs ...))
                                        lhs-namess lhs-namess/ellipsess)]
                                  [(rg-rhs/wheres ...)
                                   (map (λ (sc/b rhs names names/ellipses) 
                                          (bind-withs
                                           syn-error-name '()  
                                           #'lang lang-nts
                                           sc/b 'predicate
                                           #`#t
                                           names names/ellipses))
                                        (syntax->list #'((stuff ...) ...))
                                        (syntax->list #'(rhs ...))
                                        lhs-namess lhs-namess/ellipsess)])
                      (with-syntax ([(side-conditions-rewritten ...) 
                                     (map (λ (x) (rewrite-side-conditions/check-errs
                                                  lang-nts
                                                  syn-error-name
                                                  #t
                                                  x))
                                          (syntax->list (syntax (lhs ...))))]
                                    [(rg-side-conditions-rewritten ...) 
                                     (map (λ (x) (rewrite-side-conditions/check-errs
                                                  lang-nts
                                                  syn-error-name
                                                  #t
                                                  x))
                                          (syntax->list (syntax ((side-condition lhs rg-rhs/wheres) ...))))]
                                    [(clause-src ...)
                                     (map (λ (lhs)
                                            (format "~a:~a:~a"
                                                    (syntax-source lhs)
                                                    (syntax-line lhs)
                                                    (syntax-column lhs)))
                                          pats)]
                                    [dom-side-conditions-rewritten
                                     (and dom-ctcs
                                          (rewrite-side-conditions/check-errs
                                           lang-nts
                                           syn-error-name
                                           #f
                                           dom-ctcs))]
                                    [codom-side-conditions-rewritten
                                     (rewrite-side-conditions/check-errs
                                      lang-nts
                                      syn-error-name
                                      #f
                                      codom-contract)]
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
                                          lhs-namess lhs-namess/ellipsess
                                          (syntax->list (syntax (rhs/wheres ...))))]
                                    [(name2 name-predicate) (generate-temporaries (syntax (name name)))]
                                    
                                    ;; See "!!" below for information on the `seq-' bindings:
                                    [seq-of-rhs #'(rhs ...)]
                                    [seq-of-lhs #'(lhs ...)]
                                    [seq-of-tl-side-cond/binds #'((stuff ...) ...)]
                                    [seq-of-lhs-for-lw #'(lhs-for-lw ...)])
                        (with-syntax ([defs #`(begin
                                                (define-values (name2 name-predicate)
                                                  (let ([sc `(side-conditions-rewritten ...)]
                                                        [dsc `dom-side-conditions-rewritten])
                                                    (let ([cases (map (λ (pat rhs-fn rg-lhs src)
                                                                        (make-metafunc-case
                                                                         (compile-pattern lang pat #t) rhs-fn rg-lhs src (gensym)))
                                                                      sc
                                                                      (list rhs-fns ...)
                                                                      `(rg-side-conditions-rewritten ...)
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
                                                          ;; !! This code goes back to phase 1 to call `to-lw', but it's delayed
                                                          ;;    through `let-syntax' instead of `unsyntax' so that `to-lw' isn't called
                                                          ;;    until all metafunction definitions have been processed.
                                                          ;;    It gets a little complicated because we want to use sequences from the
                                                          ;;    original `define-metafunction' (step 1) and sequences that are generated within
                                                          ;;    `let-syntax' (step 2). So we quote all the `...' in the `let-syntax' form ---
                                                          ;;    and also have to quote all uses step-1 pattern variables in case they produce
                                                          ;;    `...', which should be treated as literals at step 2. Hece the `seq-' bindings
                                                          ;;    above and a quoting `...' on each use of a `seq-' binding.
                                                          (...
                                                           (let-syntax 
                                                               ([generate-lws
                                                                 (lambda (stx)
                                                                   (with-syntax
                                                                       ([(rhs/lw ...) (map to-lw/proc (syntax->list #'(... seq-of-rhs)))]
                                                                        [(((bind-id/lw . bind-pat/lw) ...) ...)
                                                                         ;; Also for pict, extract pattern bindings
                                                                         (map (λ (x) (map (λ (x) (cons (to-lw/proc (car x)) (to-lw/proc (cdr x))))
                                                                                          (extract-pattern-binds x)))
                                                                              (syntax->list #'(... seq-of-lhs)))]
                                                                        
                                                                        [((where/sc/lw ...) ...)
                                                                         ;; Also for pict, extract where bindings
                                                                         (map (λ (hm)
                                                                                (map
                                                                                 (λ (lst)
                                                                                   (syntax-case lst (unquote side-condition where)
                                                                                     [(where pat (unquote (f _ _)))
                                                                                      (and (or (identifier? #'pat)
                                                                                               (andmap identifier? (syntax->list #'pat)))
                                                                                           (or (free-identifier=? #'f #'variable-not-in)
                                                                                               (free-identifier=? #'f #'variables-not-in)))
                                                                                      (with-syntax ([(ids ...)
                                                                                                     (map to-lw/proc
                                                                                                          (if (identifier? #'pat)
                                                                                                              (list #'pat)
                                                                                                              (syntax->list #'pat)))])
                                                                                        #`(make-metafunc-extra-fresh
                                                                                           (list ids ...)))]
                                                                                     [(where pat exp)
                                                                                      #`(make-metafunc-extra-where
                                                                                         #,(to-lw/proc #'pat) #,(to-lw/proc #'exp))]
                                                                                     [(side-condition x)
                                                                                      #`(make-metafunc-extra-side-cond
                                                                                         #,(to-lw/uq/proc #'x))]))
                                                                                 (reverse 
                                                                                  (filter (λ (lst)
                                                                                            (syntax-case lst (where/hidden
                                                                                                              side-condition/hidden)
                                                                                              [(where/hidden pat exp) #f]
                                                                                              [(side-condition/hidden x) #f]
                                                                                              [_ #t])) 
                                                                                          (syntax->list hm)))))
                                                                              (syntax->list #'(... seq-of-tl-side-cond/binds)))]
                                                                        
                                                                        [(((rhs-bind-id/lw . rhs-bind-pat/lw/uq) ...) ...)
                                                                         ;; Also for pict, extract pattern bindings
                                                                         (map (λ (x) (map (λ (x) (cons (to-lw/proc (car x)) (to-lw/uq/proc (cdr x))))
                                                                                          (extract-term-let-binds x)))
                                                                              (syntax->list #'(... seq-of-rhs)))]
                                                                        
                                                                        [(x-lhs-for-lw ...) #'(... seq-of-lhs-for-lw)])
                                                                     #'(list (list x-lhs-for-lw
                                                                                   (list (make-metafunc-extra-where bind-id/lw bind-pat/lw) ...
                                                                                         (make-metafunc-extra-where rhs-bind-id/lw rhs-bind-pat/lw/uq) ...
                                                                                         where/sc/lw ...)
                                                                                   rhs/lw)
                                                                             ...)))])
                                                             (generate-lws)))
                                                          lang
                                                          #t ;; multi-args?
                                                          'name
                                                          (let ([name (lambda (x) (name-predicate x))]) name)
                                                          dsc
                                                          (append cases parent-cases)))
                                                       dsc
                                                       `codom-side-conditions-rewritten
                                                       'name
                                                       #,relation?))))
                                                (term-define-fn name name2))])
                          (syntax-property
                           (if (eq? 'top-level (syntax-local-context))
                               ; Introduce the names before using them, to allow
                               ; metafunction definition at the top-level.
                               (syntax 
                                (begin 
                                  (define-syntaxes (name2 name-predicate) (values))
                                  defs))
                               (syntax defs))
                           'disappeared-use
                           (map syntax-local-introduce (syntax->list #'(original-names ...))))))))))))))))]
      [(_ prev-metafunction name lang clauses ...)
       (begin
         (unless (identifier? #'name)
           (raise-syntax-error 'define-metafunction "expected the name of a language" stx #'name))
         (unless (identifier? #'lang)
           (raise-syntax-error 'define-metafunction "expected the name of a language" stx #'lang))
         (for-each 
          (λ (clause)
            (syntax-case clause ()
              [(a b) (void)]
              [else
               (raise-syntax-error 'define-metafunction "expected a lhs and rhs clause" stx clause)]))
          (syntax->list (syntax (clauses ...))))
         (raise-syntax-error 'define-metafunction "missing error check for bad syntax" stx))]))

  (define (split-out-contract stx syn-error-name rest relation?)
    ;; initial test determines if a contract is specified or not
    (cond
      [(pair? (syntax-e (car (syntax->list rest))))
       (values #f #f #'any (check-clauses stx syn-error-name (syntax->list rest) relation?))]
      [else
       (syntax-case rest ()
         [(id colon more ...)
          (begin
            (unless (eq? ': (syntax-e #'colon))
              (raise-syntax-error syn-error-name "expected a colon to follow the meta-function's name" stx #'colon))
            (let loop ([more (syntax->list #'(more ...))]
                       [dom-pats '()])
              (cond
                [(null? more)
                 (raise-syntax-error syn-error-name "expected an ->" stx)]
                [(eq? (syntax-e (car more)) '->)
                 (when (null? (cdr more))
                   (raise-syntax-error syn-error-name "expected a range contract to follow the arrow" stx (car more)))
                 (let ([doms (reverse dom-pats)]
                       [codomain (cadr more)]
                       [clauses (check-clauses stx syn-error-name (cddr more) relation?)])
                   (values #'id doms codomain clauses))]
                [else
                 (loop (cdr more) (cons (car more) dom-pats))])))]
         [_
          (raise-syntax-error
           syn-error-name
           "expected the name of the meta-function, followed by its contract (or no name and no contract)"
           stx
           rest)])]))
           
  (define (check-clauses stx syn-error-name rest relation?)
    (syntax-case rest ()
      [([(lhs ...) roc1 roc2 ...] ...)
       rest]
      [([(lhs ...) rhs ...] ...)
       (if relation?
           rest
           (begin
             (for-each 
              (λ (clause)
                (syntax-case clause ()
                  [(a b) (void)]
                  [x (raise-syntax-error syn-error-name "expected a pattern and a right-hand side" stx clause)]))
              rest)
             (raise-syntax-error syn-error-name "error checking failed.3" stx)))]
      [([x roc ...] ...)
       (begin
         (for-each 
          (λ (x)
            (syntax-case x ()
              [(lhs ...) (void)]
              [x (raise-syntax-error syn-error-name "expected a function prototype" stx #'x)]))
          (syntax->list #'(x ...)))
         (raise-syntax-error syn-error-name "error checking failed.1" stx))]
      [(x ...)
       (begin
         (for-each 
          (λ (x)
            (syntax-case x ()
              [(stuff ...) (void)]
              [x (raise-syntax-error syn-error-name "expected a metafunction clause" stx #'x)]))
          (syntax->list #'(x ...)))
         (raise-syntax-error syn-error-name "error checking failed.2" stx))]))
  
  (define (parse-extras extras)
    (for-each
     (λ (stuffs)
       (for-each
        (λ (stuff)
          (syntax-case stuff (where side-condition where/hidden side-condition/hidden)
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
            [_
             (raise-syntax-error 'define-metafunction 
                                 "expected a side-condition or where clause"
                                 stuff)]))
        (syntax->list stuffs)))
     (syntax->list extras))))

(define (build-metafunction lang cases parent-cases wrap dom-contract-pat codom-contract-pat name relation?)
  (let ([dom-compiled-pattern (and dom-contract-pat (compile-pattern lang dom-contract-pat #f))]
        [codom-compiled-pattern (compile-pattern lang codom-contract-pat #f)])
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
                       (when dom-compiled-pattern
                         (unless (match-pattern dom-compiled-pattern exp)
                           (redex-error name
                                        "~s is not in my domain"
                                        `(,name ,@exp))))
                       (let loop ([cases (append cases parent-cases)]
                                  [num (- (length parent-cases))])
                         (cond
                           [(null? cases) 
                            (if relation?
                                (begin 
                                  (cache-result exp #f #f)
                                  #f)
                                (redex-error name "no clauses matched for ~s" `(,name . ,exp)))]
                           [else
                            (let ([pattern (metafunc-case-cp (car cases))]
                                  [rhs (metafunc-case-rhs (car cases))]
                                  [id (metafunc-case-id (car cases))])
                              (let ([mtchs (match-pattern pattern exp)])
                                (cond
                                  [(not mtchs) (loop (cdr cases) (+ num 1))]
                                  [relation? 
                                   (let ([ans
                                          (ormap (λ (mtch) (ormap values (rhs traced-metafunc (mtch-bindings mtch))))
                                                 mtchs)])
                                     (unless (match-pattern codom-compiled-pattern ans)
                                       (redex-error name "codomain test failed for ~s, call was ~s" ans `(,name ,@exp)))
                                     (cond
                                       [ans 
                                        (cache-result exp #t id)
                                        (log-coverage id)
                                        #t]
                                       [else
                                        (loop (cdr cases) (+ num 1))]))]
                                  [else
                                   (let ([anss (apply append
                                                      (filter values
                                                              (map (λ (mtch) (rhs traced-metafunc (mtch-bindings mtch)))
                                                                   mtchs)))]
                                         [ht (make-hash)])
                                     (for-each (λ (ans) (hash-set! ht ans #t)) anss)
                                     (cond
                                       [(null? anss)
                                        (loop (cdr cases) (+ num 1))]
                                       [(not (= 1 (hash-count ht)))
                                        (redex-error name "~a matched ~s ~a different ways and returned different results" 
                                                     (if (< num 0)
                                                         "a clause from an extended metafunction"
                                                         (format "clause #~a (counting from 0)" num))
                                                     `(,name ,@exp)
                                                     (length mtchs))]
                                       [else
                                        (let ([ans (car anss)])
                                          (unless (match-pattern codom-compiled-pattern ans)
                                            (redex-error name
                                                         "codomain test failed for ~s, call was ~s"
                                                         ans 
                                                         `(,name ,@exp)))
                                          (cache-result exp ans id)
                                          (log-coverage id)
                                          ans)]))])))]))]
                      [else 
                       (log-coverage (cdr cache-ref))
                       (car cache-ref)])))]
               [ot (current-trace-print-args)]
               [traced-metafunc (lambda (exp)
                                  (if (or (eq? (current-traced-metafunctions) 'all)
                                          (memq name (current-traced-metafunctions)))
                                      (parameterize ([current-trace-print-args
                                                      (λ (name args kws kw-args level)
                                                        (ot name (car args) kws kw-args level))])
                                        (trace-call name metafunc exp))
                                      (metafunc exp)))])
        traced-metafunc))
     (if dom-compiled-pattern
         (λ (exp) (and (match-pattern dom-compiled-pattern exp) #t))
         (λ (exp) (and (ormap (λ (case) (match-pattern (metafunc-case-cp case) exp)) cases) 
                       #t))))))

(define current-traced-metafunctions (make-parameter '()))

(define-syntax (metafunction-form stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (let ([v (syntax-local-value #'id (lambda () #f))])
       (if (term-fn? v)
           #`(make-metafunction #,(term-fn-get-id v))
           (raise-syntax-error
            #f
            "not bound as a metafunction"
            stx
            #'id)))]))

;; pull-out-names : symbol syntax -> list-of-syntax[identifier identifier-or-false]
(define-for-syntax (pull-out-names form stx ids)
  (let loop ([names (syntax->list ids)]
             [acc '()])
    (cond
      [(null? names) acc]
      [else
       (let* ([name (car names)]
              [lst (syntax->list name)])
         (cond
           [(identifier? name) (loop (cdr names) (cons #`(#,(syntax-e name) #f) acc))]
           [(and (list? lst)
                 (andmap identifier? lst))
            (loop (cdr names) (append 
                               (list #`(#,(car lst) #f))
                               (map (λ (x) #`(#,(syntax-e x) #,(car lst)))
                                    (cdr lst))
                               acc))]
           [(list? lst)
            (for-each (λ (x) (unless (identifier? x)
                               (raise-syntax-error form "expected an identifier" stx x)))
                      lst)]
           [else
            (raise-syntax-error form 
                                "expected an identifier or a sequence of identifiers"
                                stx
                                name)]))])))

;; check-rhss-not-empty : syntax (listof syntax) -> void
(define-for-syntax (check-rhss-not-empty def-lang-stx nt-def-stxs)
  (for-each 
   (λ (nt-def-stx)
     (when (null? (cdr (syntax-e nt-def-stx)))
       (raise-syntax-error 'define-language "non-terminal with no productions" def-lang-stx nt-def-stx)))
   nt-def-stxs))

(define-syntax (define-language stx)
  (syntax-case stx ()
    [(_ name (names rhs ...) ...)
     (identifier? (syntax name))
     (begin
       (check-rhss-not-empty stx (cddr (syntax->list stx)))
       (with-syntax ([((nt-names orig) ...) (pull-out-names 'define-language stx #'(names ...))])
         (with-syntax ([(subst-names ...) (generate-temporaries (syntax->list #'(nt-names ...)))])
           (syntax/loc stx
             (begin
               (define-syntax name
                 (make-set!-transformer
                  (make-language-id
                   (case-lambda
                     [(stx)
                      (syntax-case stx (set!)
                        [(set! x e) (raise-syntax-error 'define-language "cannot set! identifier" stx #'e)]
                        [(x e (... ...)) #'(define-language-name e (... ...))]
                        [x 
                         (identifier? #'x)
                         #'define-language-name])])
                   '(nt-names ...))))
               (define define-language-name (language name (names rhs ...) ...)))))))]))

(define-struct binds (source binds))

(define-syntax (language stx)
  (syntax-case stx ()
    [(_ lang-id (name rhs ...) ...)
     (prune-syntax
      (let ()
        
        ;; verify `name' part has the right shape
        (for-each
         (λ (name)
           (cond
             [(identifier? name) (void)]
             [else
              (let ([lst (syntax->list name)])
                (cond 
                  [(list? lst) 
                   (when (null? lst)
                     (raise-syntax-error 'language 
                                         "expected a sequence of identifiers with at least one identifier"
                                         stx
                                         name))
                   (for-each (λ (x) (unless (identifier? x)
                                      (raise-syntax-error 'language 
                                                          "expected an identifier"
                                                          stx
                                                          x)))
                             lst)]
                  [else
                   (raise-syntax-error 'language 
                                       "expected a sequence of identifiers"
                                       stx
                                       lst)]))]))
         (syntax->list #'(name ...)))
        (let ([all-names (apply append (map (λ (x) (if (identifier? x) (list x) (syntax->list x))) 
                                            (syntax->list #'(name ...))))])
          ;; verify the names are valid names
          (for-each 
           (λ (name) 
             (let ([x (syntax->datum name)])
               (when (memq x '(any number string variable natural integer real variable-except variable-prefix hole name in-hole hide-hole side-condition cross ...))
                 (raise-syntax-error 'language 
                                     (format "cannot use pattern language keyword ~a as non-terminal"
                                             x)
                                     stx
                                     name))
               (when (regexp-match #rx"_" (symbol->string x))
                 (raise-syntax-error 'language
                                     "non-terminals cannot have _ in their names"
                                     stx
                                     name))))
           all-names)
          
          (with-syntax ([((r-rhs ...) ...) 
                         (map (lambda (rhss) 
                                (map (lambda (rhs)
                                       (rewrite-side-conditions/check-errs
                                        (map syntax-e all-names)
                                        'language
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
                                          (make-nt 'new-name (list (make-rhs 'orig-name))) ...)
                                    '((uniform-names ...) ...)))))))))]
    [(_ (name rhs ...) ...)
     (for-each
      (lambda (name)
        (unless (identifier? name)
          (raise-syntax-error 'language "expected name" stx name)))
      (syntax->list (syntax (name ...))))]
    [(_ x ...)
     (for-each
      (lambda (x)
        (syntax-case x ()
          [(name rhs ...)
           (void)]
          [_
           (raise-syntax-error 'language "malformed non-terminal" stx x)]))
      (syntax->list (syntax (x ...))))]))

(define-syntax (define-extended-language stx)
  (syntax-case stx ()
    [(_ name orig-lang (names rhs ...) ...)
     (begin
       (unless (identifier? (syntax name))
         (raise-syntax-error 'define-extended-language "expected an identifier" stx #'name))
       (unless (identifier? (syntax orig-lang))
         (raise-syntax-error 'define-extended-language "expected an identifier" stx #'orig-lang))
       (check-rhss-not-empty stx (cdddr (syntax->list stx)))
       (let ([old-names (language-id-nts #'orig-lang 'define-extended-language)])
         (with-syntax ([((new-nt-names orig) ...) (append (pull-out-names 'define-language stx #'(names ...)) 
                                                          (map (λ (x) #`(#,x #f)) old-names))])
           #'(begin
               (define define-language-name (extend-language orig-lang (names rhs ...) ...))
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
                   '(new-nt-names ...))))))))]))

(define-syntax (extend-language stx)
  (syntax-case stx ()
    [(_ lang (name rhs ...) ...)
     (and (identifier? #'lang)
          (andmap (λ (names) 
                    (syntax-case names ()
                      [(name1 name2 ...)
                       (and (identifier? #'name1)
                            (andmap identifier? (syntax->list #'(name2 ...))))
                       #t]
                      [name
                       (identifier? #'name) 
                       #t]
                      [_ #f]))
                  (syntax->list (syntax/loc stx (name ...)))))
     (with-syntax ([((r-rhs ...) ...) (map (lambda (rhss) (map (λ (x) (rewrite-side-conditions/check-errs
                                                                       (append (language-id-nts #'lang 'define-extended-language)
                                                                               (syntax->datum #'(name ...)))
                                                                       'define-extended-language
                                                                       #f
                                                                       x))
                                                               (syntax->list rhss)))
                                           (syntax->list (syntax ((rhs ...) ...))))]
                   [((rhs/lw ...) ...) (map (lambda (rhss) (map to-lw/proc (syntax->list rhss)))
                                            (syntax->list (syntax ((rhs ...) ...))))]
                   [(first-names ...)
                    (map (λ (x) (if (identifier? x) x (car (syntax->list x))))
                         (syntax->list (syntax (name ...))))]
                   [((uniform-names ...) ...)
                    (map (λ (x) (if (identifier? x) (list x) x))
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
       (syntax/loc stx
         (do-extend-language lang 
                             (list (make-nt '(uniform-names ...) (list (make-rhs `r-rhs) ...)) ...)
                             (list (list '(uniform-names ...) rhs/lw ...) ...))))]
    [(_ lang (name rhs ...) ...)
     (begin
       (unless (identifier? #'lang)
         (error 'define-extended-language "expected the name of a language" stx #'lang))
       (for-each
        (lambda (name)
          (unless (syntax-case name ()
                    [(name1 name2 ...)
                     (and (identifier? #'name1)
                          (andmap identifier? #'(name2 ...)))
                     #t]
                    [name
                     (identifier? #'name)
                     #t]
                    [else #f])
            (raise-syntax-error 'define-extended-language "expected a name or a non-empty sequence of names" stx name)))
        (syntax->list (syntax (name ...)))))]
    [(_ lang x ...)
     (for-each
      (lambda (x)
        (syntax-case x ()
          [(name rhs ...)
           (void)]
          [_
           (raise-syntax-error 'define-extended-language "malformed non-terminal" stx x)]))
      (syntax->list (syntax (x ...))))]))

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
                                           (make-nt shortcut-name (list (make-rhs (car names))))))
                              (cdr names)))))
                  
              new-nts)
    
    (compile-language (vector (compiled-lang-pict-builder old-lang)
                              new-pict-infos)
                      (hash-map new-ht (λ (x y) y))
                      (compiled-lang-nt-map old-lang))))

;; find-primary-nt : symbol lang -> symbol or #f
;; returns the primary non-terminal for a given nt, or #f if `nt' isn't bound in the language.
(define (find-primary-nt nt lang)
  (let ([combined (find-combined-nts nt lang)])
    (and combined
         (car combined))))

;; find-combined-nts : symbol lang -> (listof symbol) or #f
;; returns the combined set of non-terminals for 'nt' from lang
(define (find-combined-nts nt lang)
  (ormap (λ (nt-line)
           (and (member nt nt-line)
                nt-line))
         (compiled-lang-nt-map lang)))

(define (apply-reduction-relation* reductions exp)
  (let-values ([(results cycle?) (apply-reduction-relation*/cycle? reductions exp)])
    results))

(define (apply-reduction-relation*/cycle? reductions exp)
  (let ([answers (make-hash)]
        [cycle? #f])
    (let loop ([exp exp]
               [path (make-immutable-hash '())])
      (cond
        [(hash-ref path exp #f)
         (set! cycle? #t)]
        [else
         (let ([nexts (apply-reduction-relation reductions exp)])
           (cond
             [(null? nexts) (hash-set! answers exp #t)]
             [else (for-each 
                    (λ (next) (loop next (hash-set path exp #t)))
                    nexts)]))]))
    (values (sort (hash-map answers (λ (x y) x))
                  string<=?
                  #:key (λ (x) (format "~s" x)))
            cycle?)))

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
     '#,(syntax-source stx)
     '#,(syntax-line stx)
     '#,(syntax-column stx)
     '#,(syntax-position stx)))

(define (check-equiv-pred form p)
  (unless (and (procedure? p) (procedure-arity-includes? p 2))
    (raise-type-error form "procedure (arity 2)" p)))

(define-syntax (test-->> stx)
  (syntax-parse stx
    [(form red:expr
           (~or (~optional (~seq (~and #:cycles-ok (~bind [cycles-ok? #t])))
                           #:defaults ([cycles-ok? #f])
                           #:name "#:cycles-ok keyword")
                (~optional (~seq #:equiv equiv?:expr)
                           #:defaults ([equiv? #'equal?])
                           #:name "#:equiv keyword"))
           ...
           e1:expr
           e2:expr ...)
     #`(let ([=? equiv?])
         (check-equiv-pred 'form =?)
         (test-->>/procs red e1 (list e2 ...) apply-reduction-relation*/cycle? #,(attribute cycles-ok?) =? #,(get-srcloc stx)))]))

(define-syntax (test--> stx)
  (syntax-parse stx
    [(form red:expr
           (~optional (~seq #:equiv equiv?:expr)
                      #:defaults ([equiv? #'equal?]))
           e1:expr
           e2:expr ...)
     #`(let ([=? equiv?])
         (check-equiv-pred 'form =?)
         (test-->>/procs red e1 (list e2 ...) apply-reduction-relation/dummy-second-value #t =? #,(get-srcloc stx)))]))

(define (apply-reduction-relation/dummy-second-value red arg)
  (values (apply-reduction-relation red arg) #f))

(define (test-->>/procs red arg expected apply-red cycles-ok? equiv? srcinfo)
  (unless (reduction-relation? red)
    (error 'test--> "expected a reduction relation as first argument, got ~e" red))
  (let-values ([(got got-cycle?) (apply-red red arg)])
    (inc-tests)
    
    (cond
      [(and got-cycle?
            (not cycles-ok?))
       (inc-failures)
       (print-failed srcinfo)
       (fprintf (current-error-port) "found a cycle in the reduction graph\n")]
      [else
       (let* ([⊆ (λ (s1 s2) (andmap (λ (x1) (memf (λ (x) (equiv? x1 x)) s2)) s1))]
              [set-equal? (λ (s1 s2) (and (⊆ s1 s2) (⊆ s2 s1)))])
         (unless (set-equal? expected got)
           (inc-failures)
           (print-failed srcinfo)
           (for-each
            (λ (v2) (fprintf (current-error-port) "expected: ~v\n" v2))
            expected)
           (if (empty? got)
               (fprintf (current-error-port) "got nothing\n")
               (for-each
                (λ (v1) (fprintf (current-error-port) "  actual: ~v\n" v1))
                got))))])))

(define-syntax (test-predicate stx)
  (syntax-case stx ()
    [(_ p arg)
     #`(test-predicate/proc p arg #,(get-srcloc stx))]))

(define (test-predicate/proc pred arg srcinfo)
  (inc-tests)
  (unless (pred arg)
    (inc-failures)
    (print-failed srcinfo)
    (fprintf (current-error-port) "  ~v does not hold for\n  ~v\n" 
             pred arg)))

(define-syntax (test-equal stx)
  (syntax-case stx ()
    [(_ e1 e2)
     #`(test-equal/proc e1 e2 #,(get-srcloc stx))]))

(define (test-equal/proc v1 v2 srcinfo)
  (inc-tests)
  (unless (equal? v1 v2)
    (inc-failures)
    (print-failed srcinfo)
    (fprintf (current-error-port) "  actual: ~v\n" v1)
    (fprintf (current-error-port) "expected: ~v\n" v2)))

(define (print-failed srcinfo)
  (let ([file (list-ref srcinfo 0)]
        [line (list-ref srcinfo 1)]
        [column (list-ref srcinfo 2)]
        [pos (list-ref srcinfo 3)])
    (fprintf (current-error-port)
             "FAILED ~a~a\n"
             (cond
               [(path? file) 
                (let-values ([(base name dir) (split-path file)])
                  (path->string name))]
               [else ""])
             (cond
               [(and line column)
                (format ":~a.~a" line column)]
               [pos 
                (format "::~a" pos)]
               [else #f]))))

(provide (rename-out [-reduction-relation reduction-relation])
         --> fresh with ;; keywords for reduction-relation
         reduction-relation->rule-names
         extend-reduction-relation
         reduction-relation?
         union-reduction-relations
         
         compatible-closure
         context-closure
         
         define-language
         define-extended-language
         
         define-metafunction
         define-metafunction/extension
         define-relation
         
         (rename-out [metafunction-form metafunction])
         metafunction? metafunction-proc
         in-domain?
         current-traced-metafunctions
         metafunc-proc-lang
         metafunc-proc-pict-info
         metafunc-proc-name
         metafunc-proc-multi-arg?
         metafunc-proc-in-dom?
         metafunc-proc-dom-pat
         metafunc-proc-cases
         metafunc-proc?
         (struct-out metafunc-case)
         
         (struct-out metafunc-extra-side-cond)
         (struct-out metafunc-extra-where)
         (struct-out metafunc-extra-fresh)
         
         (struct-out binds))

(provide test-match
         term-match
         term-match/single
         make-bindings bindings-table bindings?
         match? match-bindings
         make-bind bind? bind-name bind-exp
         make-match
         
         test-equal
         test-->>
         test-->
         test-predicate
         test-results)


(provide language-nts
         apply-reduction-relation
         apply-reduction-relation/tag-with-names
         apply-reduction-relation*
         variable-not-in
         variables-not-in)

(provide relation-coverage
         covered-cases
         (rename-out [fresh-coverage make-coverage])
         coverage?)
