#lang scheme/base

(require "matcher.ss"
         "struct.ss"
         "term.ss"
         "loc-wrapper.ss"
	 "error.ss"
         (lib "list.ss")
         (lib "etc.ss"))

(require (for-syntax (lib "name.ss" "syntax")
                     "rewrite-side-conditions.ss"
                     "term-fn.ss"
                     (lib "boundmap.ss" "syntax")
                     scheme/base))

(define (language-nts lang)
  (hash-map (compiled-lang-ht lang) (λ (x y) x)))

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
                   (let/ec k
                     (let ([match (match-pattern cp-x exp)])
                       (when match
                         (unless (null? (cdr match))
                           (redex-error
			    'term-match/single
			    "pattern ~s matched term ~e multiple ways"
			    'pattern
			    exp))
                         (k (term-let ([names/ellipses (lookup-binding (mtch-bindings (car match)) 'names)] ...)
                              rhs))))
                     ...
                     (redex-error 'term-match/single "no patterns matched ~e" exp))))))))]))

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
                                 (term-let ([names/ellipses (lookup-binding (mtch-bindings match) 'names)] ...)
                                   rhs))
                               matches)
                          '())) ...)))))))]))

(define-syntax (compatible-closure stx)
  (syntax-case stx ()
    [(_ red lang nt)
     (identifier? (syntax nt))
     (with-syntax ([side-conditions-rewritten (rewrite-side-conditions/check-errs (language-id-nts #'lang 'compatible-closure)
                                                                                  'compatible-closure 
                                                                                  #t
                                                                                  (syntax (cross nt)))])
       (syntax (do-context-closure red lang `side-conditions-rewritten 'compatible-closure)))]
    [(_ red lang nt)
     (raise-syntax-error 'compatible-closure "expected a non-terminal as last argument" stx (syntax nt))]))

(define-syntax (context-closure stx)
  (syntax-case stx ()
    [(_ red lang pattern)
     (with-syntax ([side-conditions-rewritten (rewrite-side-conditions/check-errs (language-id-nts #'lang 'context-closure)
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
                              acc)))]))))))
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
     #'(do-reduction-relation reduction-relation empty-reduction-relation #f lang args ...)]))

(define-syntax (extend-reduction-relation stx)
  (syntax-case stx ()
    [(_ orig-reduction-relation lang args ...)
     #'(do-reduction-relation extend-reduction-relation orig-reduction-relation #t lang args ...)]))

(define-struct successful (result))

(define-syntax-set (do-reduction-relation)
  (define (do-reduction-relation/proc stx)
    (syntax-case stx ()
      [(_ id orig-reduction-relation allow-zero-rules? lang w/domain-args ...)
       (identifier? #'lang)
       (let-values ([(args domain-pattern)
                     (syntax-case #'(w/domain-args ...) ()
                       ;; commented out this case to diable domain specifications
                       #;
                       [(#:domain pat args ...)
                        (values (syntax (args ...)) #'pat)]
                       [else
                        (values (syntax (w/domain-args ...)) #'any)])])
         (with-syntax ([(rules ...) (before-with args)]
                       [(shortcuts ...) (after-with args)])
           (with-syntax ([(lws ...) (map rule->lws (syntax->list #'(rules ...)))])
             (reduction-relation/helper 
              stx
              (syntax-e #'id)
              #'orig-reduction-relation
              (syntax lang)
              (syntax->list (syntax (rules ...)))
              (syntax->list (syntax (shortcuts ...)))
              #'(list lws ...)
              (syntax-e #'allow-zero-rules?)
              domain-pattern))))]
      [(_ id orig-reduction-relation lang args ...)
       (raise-syntax-error (syntax-e #'id) 
                           "expected an identifier for the language name"
                           stx 
                           #'lang)]))
  
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
       (let-values ([(label scs fvars withs)
                     (let loop ([stuffs (syntax->list #'(stuff ...))]
                                [label #f]
                                [scs null]
                                [fvars null]
                                [withs null])
                       (cond
                         [(null? stuffs) (values label (reverse scs) (reverse fvars) (reverse withs))]
                         [else
                          (syntax-case (car stuffs) (where fresh variable-not-in)
                            [(fresh xs ...) 
                             (loop (cdr stuffs)
                                   label
                                   scs
                                   (append 
                                    (reverse (map (λ (x)
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
                                                        #'y)]))
                                                  (syntax->list #'(xs ...))))
                                    fvars)
                                   withs)]
                            [(where x e)
                             (loop (cdr stuffs)
                                   label
                                   scs
                                   fvars
                                   (cons #'(x e) withs))]
                            [(side-condition sc)
                             (loop (cdr stuffs)
                                   label
                                   (cons #'sc scs)
                                   fvars
                                   withs)]
                            [x
                             (identifier? #'x)
                             (loop (cdr stuffs)
                                   #''x
                                   scs
                                   fvars
                                   withs)]
                            [x
                             (string? (syntax-e #'x))
                             (loop (cdr stuffs)
                                   #'x
                                   scs
                                   fvars
                                   withs)])]))])
         (with-syntax ([(scs ...) scs]
                       [(fvars ...) fvars]
                       [((where-id where-expr) ...) withs]
                       [((bind-id . bind-pat) ...) 
                        (append (extract-pattern-binds #'lhs)
                                (extract-term-let-binds #'rhs))])
           #`(make-rule-pict 'arrow
                             (to-lw lhs)
                             (to-lw rhs)
                             #,label
                             (list (to-lw/uq scs) ...)
                             (list (to-lw fvars) ...)
                             (list (cons (to-lw bind-id)
                                         (to-lw bind-pat))
                                   ...
                                   (cons (to-lw where-id)
                                         (to-lw where-expr))
                                   ...))))]))
  
  (define (reduction-relation/helper stx orig-name orig-red-expr lang-id rules shortcuts lws allow-zero-rules? domain-pattern)
    (let ([ht (make-module-identifier-mapping)]
          [all-top-levels '()]
          [withs (make-module-identifier-mapping)])
      (for-each (λ (shortcut)
                  (syntax-case shortcut ()
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
        (unless (module-identifier-mapping-get ht (syntax -->) (λ () #f))
          (raise-syntax-error orig-name "no --> rules" stx)))
      
      (for-each (λ (tl)
                  (let loop ([id tl])
                    (unless (free-identifier=? #'--> id)
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
      
      (let ([name-ht (make-hasheq)]
            [lang-nts (language-id-nts lang-id orig-name)])
        (with-syntax ([lang-id lang-id]
                      [(top-level ...) (get-choices stx orig-name ht lang-id (syntax -->) name-ht lang-id allow-zero-rules?)]
                      [(rule-names ...) (hash-map name-ht (λ (k v) k))]
                      [lws lws]
                      
                      [domain-pattern-side-conditions-rewritten
                       (rewrite-side-conditions/check-errs
                        lang-nts
                        orig-name
                        #t
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
                                      id))))))))
  
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
       (let ([lang-nts (language-id-nts lang-id orig-name)])
         (let-values ([(names names/ellipses) (extract-names lang-nts orig-name #t (syntax rhs-from))])
           (with-syntax ([(names ...) names]
                         [(names/ellipses ...) names/ellipses]
                         [side-conditions-rewritten (rewrite-side-conditions/check-errs
                                                     lang-nts
                                                     orig-name
                                                     #t
                                                     (rewrite-node-pat (syntax-e (syntax lhs-frm-id))
                                                                       (syntax->datum (syntax rhs-from))))]
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
                   #,child-proc))
              (get-choices stx orig-name bm #'lang
                           (syntax lhs-arrow) 
                           name-table lang-id 
                           allow-zero-rules?)))))]))  
  (define (rewrite-node-pat id term)
    (let loop ([term term])
      (cond
        [(eq? id term) `(name ,id any)]
        [(pair? term) (cons (loop (car term))
                            (loop (cdr term)))]
        [else term])))
  
  (define (do-leaf stx orig-name lang name-table from to extras lang-id)
    (let ([lang-nts (language-id-nts lang-id orig-name)])
      (let-values ([(name fresh-vars side-conditions/withs) (process-extras stx orig-name name-table extras)])
        (let-values ([(names names/ellipses) (extract-names lang-nts orig-name #t from)])
          (with-syntax ([side-conditions-rewritten 
                         (rewrite-side-conditions/check-errs 
                          lang-nts
                          orig-name
                          #t
                          from)]
                        [to to]
                        [name name]
                        [lang lang]
                        [(names ...) names]
                        [(names/ellipses ...) names/ellipses]
                        [(fresh-var-clauses ...) 
                         (map (λ (fv-clause)
                                (syntax-case fv-clause ()
                                  [x
                                   (identifier? #'x)
                                   #'[x (variable-not-in main 'x)]]
                                  [(x name)
                                   (identifier? #'x)
                                   #'[x (let ([the-name (term name)])
                                          (verify-name-ok '#,orig-name the-name)
                                          (variable-not-in main the-name))]]
                                  [((y) (x ...))
                                   #`[(y #,'...)
                                      (variables-not-in main 
                                                        (map (λ (_ignore_) 'y)
                                                             (term (x ...))))]]
                                  [((y) (x ...) names)
                                   #`[(y #,'...)
                                      (let ([the-names (term names)]
                                            [len-counter (term (x ...))])
                                        (verify-names-ok '#,orig-name the-names len-counter)
                                        (variables-not-in main the-names))]]))
                              fresh-vars)])
            #`(do-leaf-match
               name
               `side-conditions-rewritten
               (λ (main bindings)
                 ;; nested term-let's so that the bindings for the variables 
                 ;; show up in the `fresh' side-conditions, the bindings for the variables
                 ;; show up in the withs, and the withs show up in the 'fresh' side-conditions
                 (term-let ([names/ellipses (lookup-binding bindings 'names)] ...)
                   (term-let (fresh-var-clauses ...)
                     #,(bind-withs side-conditions/withs
                                    #'(make-successful (term to))))))))))))
  
  ;; the withs and side-conditions come in backwards order
  (define (bind-withs stx body)
    (let loop ([stx stx]
               [body body])
      (syntax-case stx (side-condition where)
        [() body]
        [((where x e) y ...) 
         (loop #'(y ...) #`(term-let ([x (term e)]) #,body))]
        [((side-condition s ...) y ...)
         (loop #'(y ...) #`(and s ... #,body))])))
  
  (define (process-extras stx orig-name name-table extras)
    (let ([the-name #f]
          [the-name-stx #f]
          [fresh-vars '()]
          [side-conditions/withs '()])
      (let loop ([extras extras])
        (cond
          [(null? extras) (values the-name fresh-vars side-conditions/withs)]
          [else
           (syntax-case (car extras) (side-condition fresh where)
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
                                         (list (hash-ref name-table name-sym)
                                               (syntax name))))
                  (hash-set! name-table name-sym (syntax name))
                  
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
              (begin
                (set! fresh-vars 
                      (append 
                       (map (λ (x)
                              (syntax-case x ()
                                [x
                                 (identifier? #'x)
                                 #'x]
                                [(x name)
                                 (identifier? #'x)
                                 #'(x name)]
                                [((ys dots2) (xs dots1))
                                 (and (eq? (syntax-e #'dots1) (string->symbol "..."))
                                      (eq? (syntax-e #'dots2) (string->symbol "...")))
                                 #'((ys) (xs dots1))]
                                [((ys dots2) (xs dots1) names)
                                 (and (eq? (syntax-e #'dots1) (string->symbol "..."))
                                      (eq? (syntax-e #'dots2) (string->symbol "...")))
                                 #'((ys) (xs dots1) names)]
                                [x
                                 (raise-syntax-error orig-name 
                                                     "malformed fresh variable clause"
                                                     stx
                                                     #'x)]))
                            (syntax->list #'(var ...)))
                       fresh-vars))
                (loop (cdr extras)))]
             [(side-condition exp ...)
              (begin 
                (set! side-conditions/withs (cons (car extras) side-conditions/withs))
                (loop (cdr extras)))]
             [(where x e)
              (begin
                (set! side-conditions/withs (cons (car extras) side-conditions/withs))
                (loop (cdr extras)))]
             [(where . x)
              (raise-syntax-error orig-name "malformed where clause" stx (car extras))]
             [_
              (raise-syntax-error orig-name "unknown extra" stx (car extras))])]))))
  
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
                   (hash-set! name-ht name #t))
                 (reduction-relation-rule-names red)))
     lst)
    (build-reduction-relation
     #f
     first-lang
     (reverse (apply append (map reduction-relation-make-procs lst)))
     (hash-map name-ht (λ (k v) k))
     (apply append (map reduction-relation-lws lst))
     `any)))

(define (do-node-match lhs-frm-id lhs-to-id pat rhs-proc child-make-proc)
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
   (rewrite-proc-name child-make-proc)))

(define (do-leaf-match name pat proc)
  (make-rewrite-proc
   (λ (lang)
     (let ([cp (compile-pattern lang pat #t)])
       (λ (main-exp exp f other-matches)
         (let ([mtchs (match-pattern cp exp)])
           (if mtchs
               (map/mt (λ (mtch) 
                         (let ([really-matched (proc main-exp (mtch-bindings mtch))])
                           (and really-matched
                                (list name (f (successful-result really-matched))))))
                       mtchs
                       other-matches)
               other-matches)))))
   name))

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
  (make-struct-type 'metafunc-proc #f 9 0 #f null (current-inspector) 0))
(define metafunc-proc-pict-info (make-struct-field-accessor metafunc-proc-ref 1))
(define metafunc-proc-lang (make-struct-field-accessor metafunc-proc-ref 2))
(define metafunc-proc-multi-arg? (make-struct-field-accessor metafunc-proc-ref 3))
(define metafunc-proc-name (make-struct-field-accessor metafunc-proc-ref 4))
(define metafunc-proc-cps (make-struct-field-accessor metafunc-proc-ref 5))
(define metafunc-proc-rhss (make-struct-field-accessor metafunc-proc-ref 6))
(define metafunc-proc-in-dom? (make-struct-field-accessor metafunc-proc-ref 7))
(define metafunc-proc-dom-pat (make-struct-field-accessor metafunc-proc-ref 8))
(define-struct metafunction (proc))

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


(define-syntax-set (define-metafunction define-metafunction/extension)
  
  (define (define-metafunction/proc stx)
    (syntax-case stx ()
      [(_ . rest)
       (internal-define-metafunction stx #f #'rest)]))
  
  (define (define-metafunction/extension/proc stx)
    (syntax-case stx ()
      [(_ prev . rest)
       (identifier? #'prev)
       (internal-define-metafunction stx #'prev #'rest)]))
  
  (define (internal-define-metafunction orig-stx prev-metafunction stx)
    (syntax-case stx ()
      [(lang . rest)
       (let ([syn-error-name (if prev-metafunction
                                 'define-metafunction/extension
                                 'define-metafunction)])
         (unless (identifier? #'lang)
           (raise-syntax-error syn-error-name "expected an identifier in the language position" orig-stx #'lang))
         (when (null? (syntax-e #'rest))
           (raise-syntax-error syn-error-name "no clauses" orig-stx))
         (let-values ([(contract-name dom-ctcs codom-contract pats)
                       (split-out-contract orig-stx syn-error-name #'rest)])
           (with-syntax ([(((name lhs-clauses ...) rhs stuff ...) ...) pats]
                         [(lhs-for-lw ...)
                          (with-syntax ([((lhs-for-lw _ _ ...) ...) pats])
                            (map (λ (x) (datum->syntax #f (cdr (syntax-e x)) x))
                                 (syntax->list #'(lhs-for-lw ...))))])
             (with-syntax ([(lhs ...) #'((lhs-clauses ...) ...)]
                           [name (let loop ([name (if contract-name
                                                      contract-name
                                                      (car (syntax->list #'(name ...))))]
                                            [names (if contract-name
                                                       (syntax->list #'(name ...))
                                                       (cdr (syntax->list #'(name ...))))])
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
               
               (with-syntax ([(((tl-side-conds ...) ...) 
                               (tl-bindings ...))
                              (extract-side-conditions (syntax-e #'name) stx #'((stuff ...) ...))])
                 (let ([lang-nts (language-id-nts #'lang 'define-metafunction)])
                   (with-syntax ([(side-conditions-rewritten ...) 
                                  (map (λ (x) (rewrite-side-conditions/check-errs
                                               lang-nts
                                               #t
                                               'define-metafunction
                                               x))
                                       (syntax->list (syntax ((side-condition lhs (and tl-side-conds ...)) ...))))]
                                 [dom-side-conditions-rewritten
                                  (and dom-ctcs
                                       (rewrite-side-conditions/check-errs
                                        lang-nts
                                        'define-metafunction
                                        #f
                                        dom-ctcs))]
                                 [codom-side-conditions-rewritten
                                  (rewrite-side-conditions/check-errs
                                   lang-nts
                                   'define-metafunction
                                   #f
                                   codom-contract)]
                                 [(rhs-fns ...)
                                  (map (λ (lhs rhs bindings)
                                         (let-values ([(names names/ellipses) (extract-names lang-nts 'define-metafunction #t lhs)])
                                           (with-syntax ([(names ...) names]
                                                         [(names/ellipses ...) names/ellipses]
                                                         [rhs rhs]
                                                         [((tl-var tl-exp) ...) bindings])
                                             (syntax 
                                              (λ (name bindings)
                                                (term-let ([names/ellipses (lookup-binding bindings 'names)] ...)
                                                          (term-let ([tl-var (term tl-exp)] ...)
                                                                    (term-let-fn ((name name))
                                                                                 (term rhs)))))))))
                                       (syntax->list (syntax (lhs ...)))
                                       (syntax->list (syntax (rhs ...)))
                                       (syntax->list (syntax (tl-bindings ...))))]
                                 [(name2 name-predicate) (generate-temporaries (syntax (name name)))]
                                 [((side-cond ...) ...)
                                  ;; For generating a pict, separate out side conditions wrapping the LHS and at the top-level
                                  (map (lambda (lhs scs)
                                         (append
                                          (let loop ([lhs lhs])
                                            (syntax-case lhs (side-condition term)
                                              [(side-condition pat (term sc))
                                               (cons #'sc (loop #'pat))]
                                              [_else null]))
                                          scs))
                                       (syntax->list #'(lhs ...))
                                       (syntax->list #'((tl-side-conds ...) ...)))]
                                 [(((bind-id . bind-pat) ...) ...)
                                  ;; Also for pict, extract pattern bindings
                                  (map extract-pattern-binds (syntax->list #'(lhs ...)))])
                     #`(begin
                         (define-values (name2 name-predicate)
                           (build-metafunction 
                            lang
                            (list `side-conditions-rewritten ...)
                            (list rhs-fns ...)
                            #,(if prev-metafunction
                                  (let ([term-fn (syntax-local-value prev-metafunction)])
                                    #`(metafunc-proc-cps #,(term-fn-get-id term-fn)))
                                  #''())
                            #,(if prev-metafunction
                                  (let ([term-fn (syntax-local-value prev-metafunction)])
                                    #`(metafunc-proc-rhss #,(term-fn-get-id term-fn)))
                                  #''())
                            (λ (f/dom cps rhss) 
                              (make-metafunc-proc
                               (let ([name (lambda (x) (f/dom x))]) name)
                               (list (list (to-lw lhs-for-lw)
                                           (list (to-lw/uq side-cond) ...)
                                           (list (cons (to-lw bind-id)
                                                       (to-lw bind-pat))
                                                 ...)
                                           (to-lw rhs))
                                     ...)
                               lang
                               #t ;; multi-args?
                               'name
                               cps
                               rhss
                               (let ([name (lambda (x) (name-predicate x))]) name)
                               `dom-side-conditions-rewritten))
                            `dom-side-conditions-rewritten
                            `codom-side-conditions-rewritten
                            'name))
                         (term-define-fn name name2)))))))))]
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

  (define (split-out-contract stx syn-error-name rest)
    ;; initial test determines if a contract is specified or not
    (cond
      [(pair? (syntax-e (car (syntax->list rest))))
       (values #f #f #'any (check-clauses stx syn-error-name rest))]
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
                       [clauses (check-clauses stx syn-error-name (cddr more))])
                   (values #'id doms codomain clauses))]
                [else
                 (loop (cdr more) (cons (car more) dom-pats))])))]
         [_
          (raise-syntax-error
           syn-error-name
           "expected the name of the meta-function, followed by its contract (or no name and no contract)"
           stx
           rest)])]))
           
  (define (check-clauses stx syn-error-name rest)
    (syntax-case rest ()
      [([(lhs ...) roc ...] ...)
       rest]
      [([x roc ...] ...)
       (for-each 
        (λ (x)
          (syntax-case x ()
            [(lhs ...) (void)]
            [x (raise-syntax-error syn-error-name "expected a function prototype" stx #'x)]))
        (syntax->list #'(x ...)))
       (raise-syntax-error syn-error-name "error checking failed.1" stx)]
      [(x ...)
       (for-each 
        (λ (x)
          (syntax-case x ()
            [(stuff ...) (void)]
            [x (raise-syntax-error syn-error-name "expected a metafunction clause" stx #'x)]))
        (syntax->list #'(x ...)))
       (raise-syntax-error syn-error-name "error checking failed.2" stx)]))
        
  
  (define (extract-side-conditions name stx stuffs)
    (let loop ([stuffs (syntax->list stuffs)]
               [side-conditionss '()]
               [bindingss '()])
      (cond
        [(null? stuffs) (list (reverse side-conditionss)
                              (reverse bindingss))]
        [else 
         (let s-loop ([stuff (syntax->list (car stuffs))]
                      [side-conditions '()]
                      [bindings '()])
           (cond
             [(null? stuff) (loop (cdr stuffs)
                                  (cons (reverse side-conditions) side-conditionss)
                                  (cons (reverse bindings) bindingss))]
             [else 
              (syntax-case (car stuff) (side-condition)
                [(side-condition tl-side-conds ...) 
                 (s-loop (cdr stuff)
                         (append (syntax->list #'(tl-side-conds ...)) side-conditions)
                         bindings)]
                [(where x e)
                 (s-loop (cdr stuff)
                         side-conditions
                         (cons #'(x e) bindings))]
                [_
                 (raise-syntax-error 'define-metafunction 
                                     "expected a side-condition or where clause"
                                     (car stuff))])]))]))))

(define (build-metafunction lang patterns rhss old-cps old-rhss wrap dom-contract-pat codom-contract-pat name)
  (let ([compiled-patterns (append old-cps
                                   (map (λ (pat) (compile-pattern lang pat #t)) patterns))]
        [dom-compiled-pattern (and dom-contract-pat (compile-pattern lang dom-contract-pat #f))]
        [codom-compiled-pattern (compile-pattern lang codom-contract-pat #f)])
    (values
     (wrap
      (letrec ([cache (make-hash)]
               [not-in-cache (gensym)]
               [metafunc
                (λ (exp)
                  (let ([cache-ref (hash-ref cache exp not-in-cache)])
                    (cond
                      [(eq? cache-ref not-in-cache)
                       (when dom-compiled-pattern
                         (unless (match-pattern dom-compiled-pattern exp)
                           (redex-error name
                                        "~s is not in my domain"
                                        `(,name ,@exp))))
                       (let loop ([patterns compiled-patterns]
                                  [rhss (append old-rhss rhss)]
                                  [num (- (length old-cps))])
                         (cond
                           [(null? patterns) 
                            (redex-error name "no clauses matched for ~s" `(,name . ,exp))]
                           [else
                            (let ([pattern (car patterns)]
                                  [rhs (car rhss)])
                              (let ([mtchs (match-pattern pattern exp)])
                                (cond
                                  [(not mtchs) (loop (cdr patterns)
                                                     (cdr rhss)
                                                     (+ num 1))]
                                  [(not (null? (cdr mtchs)))
                                   (redex-error name "~a matched ~s ~a different ways" 
                                                (if (< num 0)
                                                    "a clause from an extended metafunction"
                                                    (format "clause ~a" num))
                                                `(,name ,@exp)
                                                (length mtchs))]
                                  [else
                                   (let ([ans (rhs metafunc (mtch-bindings (car mtchs)))])
                                     (unless (match-pattern codom-compiled-pattern ans)
                                       (redex-error name "codomain test failed for ~s, call was ~s" ans `(,name ,@exp)))
                                     (hash-set! cache exp ans)
                                     ans)])))]))]
                      [else 
                       cache-ref])))])
        metafunc)
      compiled-patterns
      rhss)
     (if dom-compiled-pattern
         (λ (exp) (and (match-pattern dom-compiled-pattern exp) #t))
         (λ (exp) (and (ormap (λ (pat) (match-pattern pat exp)) compiled-patterns) 
                       #t))))))

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
       (check-rhss-not-empty stx (cddr (syntax-e stx)))
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
     (let ()
       
       ;; collect-binds-clauses : syntax syntax (cons syntax (listof syntax)) -> (values syntax (listof syntax))
       ;; extracts the #:binds part of a production and returns them (if any) as well as returning the
       ;; list of syntax objects that follow the binds clause.
       ;; production is the original production that this #:binds clause is modifying,
       ;; and lang is the name of the language
       (define (collect-binds-clauses production lang rhss)
         (let loop ([binds '()]
                    [rhss rhss])
           (cond
             [(or (null? (cdr rhss))
                  (not (equal? (syntax-e (cadr rhss)) '#:binds)))
              (values #`(list #,@(reverse binds)) (cdr rhss))]
             [else
              (unless (>= (length rhss) 3)
                (raise-syntax-error #f 
                                    "found a #:binds clause without two following expressions"
                                    stx
                                    (cadr rhss)))
              (let ([binds-keyword (list-ref rhss 1)]
                    [var (list-ref rhss 2)]
                    [nt (list-ref rhss 3)])
                (unless (identifier? var)
                  (raise-syntax-error #f 
                                      "the first argument to #:binds must be a non-terminal occurring in this right-hand side"
                                      stx
                                      var))
                (unless (identifier? nt)
                  (raise-syntax-error #f 
                                      "the second argument to #:binds must be a non-terminal occurring in this right-hand side"
                                      stx
                                      nt))
                (loop (cons #`(make-binds 
                               ;; thunking like this means that the pattern is compiled each time the fn
                               ;; runs, ie inefficient
                               '#,var
                               '#,nt)
                            binds)
                      (cdddr rhss)))])))
       
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
              (when (memq x '(any number string variable variable-except variable-prefix hole name in-hole in-named-hole hide-hole side-condition cross ...))
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
         
         (with-syntax ([(((r-rhs var-info) ...) ...) 
                        (map (lambda (rhss) 
                               (let loop ([rhss (syntax->list rhss)])
                                 (cond
                                   [(null? rhss) '()]
                                   [else 
                                    (let ([x (car rhss)])
                                      (let-values ([(var-info rest) (collect-binds-clauses x #'lang rhss)])
                                        (cons (list (rewrite-side-conditions/check-errs
                                                     (map syntax-e all-names)
                                                     'language
                                                     #f
                                                     x)
                                                    var-info)
                                              (loop rest))))])))
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
                 (compile-language (list (list '(uniform-names ...) (to-lw rhs) ...) ...)
                                   (list (make-nt 'first-names (list (make-rhs `r-rhs var-info) ...)) ...
                                         (make-nt 'new-name (list (make-rhs 'orig-name '()))) ...)
                                   '((uniform-names ...) ...))))))))]
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
         (raise-syntax-error 'define-extended-langauge "expected an identifier" stx #'name))
       (unless (identifier? (syntax orig-lang))
         (raise-syntax-error 'define-extended-langauge "expected an identifier" stx #'orig-lang))
       (check-rhss-not-empty stx (cdddr (syntax-e stx)))
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
                                                                       (language-id-nts #'lang 'extend-language)
                                                                       'extend-language
                                                                       #f
                                                                       x))
                                                               (syntax->list rhss)))
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
                             (list (make-nt 'first-names (list (make-rhs `r-rhs '()) ...)) ...
                                   (make-nt 'new-name (list (make-rhs 'orig-name '()))) ...)
                             (list (list '(uniform-names ...) (to-lw rhs) ...) ...))))]
    [(_ lang (name rhs ...) ...)
     (begin
       (unless (identifier? #'lang)
         (error 'extend-language "expected the name of a language" stx #'lang))
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
            (raise-syntax-error 'extend-language "expected a name or a non-empty sequence of names" stx name)))
        (syntax->list (syntax (name ...)))))]
    [(_ lang x ...)
     (for-each
      (lambda (x)
        (syntax-case x ()
          [(name rhs ...)
           (void)]
          [_
           (raise-syntax-error 'extend-language "malformed non-terminal" stx x)]))
      (syntax->list (syntax (x ...))))]))

(define extend-nt-ellipses '(....))

;; do-extend-language : compiled-lang (listof (listof nt)) ? -> compiled-lang
(define (do-extend-language old-lang new-nts new-pict-infos)
  (unless (compiled-lang? old-lang)
    (error 'extend-language "expected a language as first argument, got ~e" old-lang))
  (let ([old-nts (compiled-lang-lang old-lang)]
        [old-ht (make-hasheq)]
        [new-ht (make-hasheq)])
    (for-each (λ (nt) 
                (hash-set! old-ht (nt-name nt) nt)
                (hash-set! new-ht (nt-name nt) nt))
              old-nts)
    
    (let ([extended-nts '()])
      (for-each (λ (raw-nt)
                  (let ([primary-name (find-primary-nt (nt-name raw-nt) old-lang)])
                    (when (and primary-name (member primary-name extended-nts))
                      (error 'extend-language "the non-terminal ~s was extended twice" primary-name))
                    (let ([nt (make-nt (or primary-name (nt-name raw-nt))
                                       (nt-rhs raw-nt))])
                      (cond
                        [(ormap (λ (rhs) (member (rhs-pattern rhs) extend-nt-ellipses))
                                (nt-rhs nt))
                         (unless (hash-ref old-ht (nt-name nt) #f)
                           (error 'extend-language
                                  "the language extends the ~s non-terminal, but that non-terminal is not in the old language"
                                  (nt-name raw-nt)))
                         (hash-set! new-ht 
                                    (nt-name nt)
                                    (make-nt
                                     (nt-name nt)
                                     (append (nt-rhs (hash-ref old-ht (nt-name nt)))
                                             (filter (λ (rhs) (not (member (rhs-pattern rhs) extend-nt-ellipses)))
                                                     (nt-rhs nt)))))]
                        [else
                         (hash-set! new-ht (nt-name nt) nt)]))))
                new-nts))
    
    (compile-language (vector (compiled-lang-pict-builder old-lang)
                              new-pict-infos)
                      (hash-map new-ht (λ (x y) y))
                      (compiled-lang-nt-map old-lang))))

;; find-primary-nt : symbol lang -> symbol or #f
;; returns the primary non-terminal for a given nt, or #f if `nt' isn't bound in the language.
(define (find-primary-nt nt lang)
  (ormap (λ (nt-line)
           (and (member nt nt-line)
                (car nt-line)))
         (compiled-lang-nt-map lang)))

(define (apply-reduction-relation* reductions exp)
  (let ([answers (make-hash)])
    (let loop ([exp exp])
      (let ([nexts (apply-reduction-relation reductions exp)])
        (cond
          [(null? nexts) (hash-set! answers exp #t)]
          [else (for-each loop nexts)])))
    (hash-map answers (λ (x y) x))))


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

(define re:gen-d #rx".*[^0-9]([0-9]+)$")
(define (variable-not-in sexp var)
  (let* ([var-str (symbol->string var)]
         [var-prefix (let ([m (regexp-match #rx"^(.*[^0-9])[0-9]+$" var-str)])
                       (if m
                           (cadr m)
                           var-str))]
         [found-exact-var? #f]
         [nums (let loop ([sexp sexp]
                          [nums null])
                 (cond
                   [(pair? sexp) (loop (cdr sexp) (loop (car sexp) nums))]
                   [(symbol? sexp) 
                    (when (eq? sexp var)
                      (set! found-exact-var? #t))
                    (let* ([str (symbol->string sexp)]
                           [match (regexp-match re:gen-d str)])
                      (if (and match
                               (is-prefix? var-prefix str))
                          (cons (string->number (cadr match)) nums)
                          nums))]
                   [else nums]))])
    (cond
      [(not found-exact-var?) var]
      [(null? nums) (string->symbol (format "~a1" var))]
      [else (string->symbol (format "~a~a" var-prefix (find-best-number nums)))])))

(define (find-best-number nums)
  (let loop ([sorted (sort nums <)]
             [i 1])
    (cond
      [(empty? sorted) i]
      [else 
       (let ([fst (car sorted)])
         (cond
           [(< i fst) i]
           [(> i fst) (loop (cdr sorted) i)]
           [(= i fst) (loop (cdr sorted) (+ i 1))]))])))

(define (variables-not-in sexp vars)
  (let loop ([vars vars]
             [sexp sexp])
    (cond
      [(null? vars) null]
      [else 
       (let ([new-var (variable-not-in sexp (car vars))])
         (cons new-var
               (loop (cdr vars)
                     (cons new-var sexp))))])))

(define (is-prefix? str1 str2)
  (and (<= (string-length str1) (string-length str2))
       (equal? str1 (substring str2 0 (string-length str1)))))


;; The struct selector extracts the reduction relation rules, which 
;; are in reverse order compared to the way the reduction relation was written
;; in the program text. So reverse them. 
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
     (if (= tests 1)
         (printf "One test passed.\n")
         (printf "All ~a tests passed.\n" tests))]
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

(define-syntax (test--> stx)
  (syntax-case stx ()
    [(_ red e1 e2 ...)
     #`(test-->/procs red e1 (list e2 ...) #,(get-srcloc stx))]))

(define (test-->/procs red arg expected srcinfo)
  (let ([got (apply-reduction-relation* red arg)])
    (inc-tests)
    (unless (set-equal? expected got)
      (inc-failures)
      (print-failed srcinfo)
      (for-each
       (λ (v2) (fprintf (current-error-port) "expected: ~v\n" v2))
       expected)
      (for-each
       (λ (v1) (fprintf (current-error-port) "  actual: ~v\n" v1))
       got))))

(define (set-equal? s1 s2)
  (define (⊆ s1 s2) (andmap (λ (x1) (member x1 s2)) s1))
  (and (⊆ s1 s2)
       (⊆ s2 s1)))

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
         
         (rename-out [metafunction-form metafunction])
         metafunction? metafunction-proc
         in-domain?
         metafunc-proc-lang
         metafunc-proc-pict-info
         metafunc-proc-name
         metafunc-proc-multi-arg?
         metafunc-proc-cps
         metafunc-proc-rhss
         metafunc-proc-in-dom?
         metafunc-proc-dom-pat
         
         (struct-out binds))

(provide test-match
         term-match
         term-match/single
         make-bindings bindings-table bindings?
         match? match-bindings
         make-bind bind? bind-name bind-exp
         
         test-equal
         test-->
         test-predicate
         test-results)


(provide language-nts
         apply-reduction-relation
         apply-reduction-relation/tag-with-names
         apply-reduction-relation*
         variable-not-in
         variables-not-in)
