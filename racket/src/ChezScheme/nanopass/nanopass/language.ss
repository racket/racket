;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

;;; Producs are : record defs, parser, meta parser, lang 
;;; may need to use meta define meta-parser.
;;; 
;;; TODO:
;;;   - add facility to allow for functional transformations while unparsing
;;;     (instead of just the pattern ones available now).  this should be
;;;     clearer than the old definitions form.
;;;   - re-investigate how language extensions work and see if there is a
;;;     cleaner way to do this
;;;   - better comparison of alts then simple symbolic equality
;;;   - checking for language output to make sure constructed languages are
;;;     internally consistent:
;;;     - check to make sure metas are unique
(library (nanopass language)
  (export define-language language->s-expression diff-languages prune-language define-pruned-language)
  (import (rnrs)
          (nanopass helpers)
          (nanopass language-helpers)
          (nanopass records)
          (nanopass unparser)
          (nanopass meta-parser))
 
  (define-syntax define-language
    (syntax-rules ()
      [(_ ?L ?rest ...)
       (let-syntax ([a (syntax-rules ()
                         [(_ ?XL)
                          (x-define-language ?XL ((... ...) ?rest) ...)])])
         (a ?L))]))

  (define-syntax x-define-language
    (lambda (x) 
      ;; This function tests equality of tspecs
      ;; tspecs are considered to be equal when the lists of metas are 
      ;; identical (same order too) and when they represent the same terminal 
      ; TODO: think about a better way of doing equality here... right now we get a weird
      ; error message when the original had (fixnum (x y z)) and our extension has (fixnum (x y))
      (define tspec=?
        (lambda (ts1 ts2)
          (and (equal? (syntax->datum (tspec-meta-vars ts1)) 
                       (syntax->datum (tspec-meta-vars ts2)))
               (eq? (syntax->datum (tspec-type ts1)) 
                    (syntax->datum (tspec-type ts2)))))) 

      ;; This function tests the equality of ntspecs
      ;; ntspecs are considered to be equal when they are ntspecs of 
      ;; the same nonterminal and the intersection of their alternatives is 
      ;; not null
      (define ntspec=?
        (lambda (p1 p2)
          (eq? (syntax->datum (ntspec-name p1))
               (syntax->datum (ntspec-name p2)))))
 
      ;; It is enough to check for same syntax because the record-decls of the
      ;; new alternative will be different because they are parsed again
      (define alt=?
        (lambda (a1 a2)
          (equal? (syntax->datum (alt-syn a1)) (syntax->datum (alt-syn a2)))))

      (define fresh-tspec
        (lambda (tspec)
          (make-tspec
            (tspec-type tspec)
            (tspec-meta-vars tspec)
            (tspec-handler tspec))))

      (define-who fresh-alt
        (lambda (alt)
          ((cond
             [(pair-alt? alt) make-pair-alt]
             [(terminal-alt? alt) make-terminal-alt]
             [(nonterminal-alt? alt) make-nonterminal-alt]
             [else (error who "unexpected alt" alt)])
            (alt-syn alt) (alt-pretty alt) (alt-pretty-procedure? alt))))

      (define fresh-ntspec
        (lambda (ntspec)
          (make-ntspec
            (ntspec-name ntspec)
            (ntspec-meta-vars ntspec)
            (map fresh-alt (ntspec-alts ntspec)))))

      ;; Doing a little extra work here to make sure that we are able to track
      ;; errors.  The basic idea is that we want to go through the list of
      ;; existing tspecs, and when we keep them, make a new copy (so that
      ;; language specific information can be updated in them), and when they
      ;; are being removed, we "mark" that we found the one to remove by
      ;; pulling it out of our removal list.  If any remain in the removal
      ;; list when we're done, we complain about it.
      (define freshen-objects
        (lambda (o=? fresh-o msg unpacker)
          (rec f
            (lambda (os os-)
              (cond
                [(and (null? os) (not (null? os-)))
                 (syntax-violation 'define-language msg (map unpacker os-))]
                [(null? os) '()]
                [else
                 (let g ([os- os-] [o (car os)] [checked-os- '()])
                   (cond
                     [(null? os-) (cons (fresh-o o) (f (cdr os) checked-os-))]
                     [(o=? o (car os-))
                      (f (cdr os) (append checked-os- (cdr os-)))]
                     [else (g (cdr os-) o (cons (car os-) checked-os-))]))])))))

      (define freshen-tspecs
        (freshen-objects tspec=? fresh-tspec "unrecognized tspecs" tspec-type))
      (define freshen-alts
        (freshen-objects alt=? fresh-alt "unrecognized alts" alt-syn))

      (define add-objects
        (lambda (o=? msg)
          (letrec ([f (lambda (os os+)
                        (if (null? os+)
                            os
                            (let ([o+ (car os+)])
                              (when (memp (lambda (x) (o=? o+ x)) os)
                                (syntax-violation 'define-language msg o+))
                              (f (cons o+ os) (cdr os+)))))])
            f)))

      (define add-tspecs (add-objects tspec=? "duplicate tspec in add"))
      (define add-alts (add-objects alt=? "duplicate alt in add"))

      (define freshen-ntspecs
        (lambda (ntspecs ntspecs-)
          (cond
            [(and (null? ntspecs) (not (null? ntspecs-)))
             (if (fx>? (length ntspecs-) 1)
                 (syntax-violation 'define-language
                   "multiple unrecognized ntspecs, including"
                   (ntspec-name (car ntspecs-)))
                 (syntax-violation 'define-language
                   "unrecognized ntspec" (ntspec-name (car ntspecs-))))]
            [(null? ntspecs) '()]
            [else
              (let g ([ntspecs- ntspecs-] [ntspec (car ntspecs)] [remaining '()])
                (if (null? ntspecs-)
                    (cons (fresh-ntspec ntspec) (freshen-ntspecs (cdr ntspecs) remaining))
                    (let ([ntspec- (car ntspecs-)])
                      (if (ntspec=? ntspec- ntspec)
                          (let ([alts (freshen-alts (ntspec-alts ntspec) (ntspec-alts ntspec-))])
                            (if (null? alts)
                                (freshen-ntspecs (cdr ntspecs) (append remaining (cdr ntspecs-)))
                                (cons (make-ntspec
                                        (ntspec-name ntspec-)
                                        (ntspec-meta-vars ntspec-)
                                        alts)
                                  (freshen-ntspecs (cdr ntspecs) (append remaining (cdr ntspecs-))))))
                          (g (cdr ntspecs-) ntspec (cons (car ntspecs-) remaining))))))])))

      (define add-ntspecs
        (lambda (ntspecs ntspecs+)
          (cond
            [(null? ntspecs) ntspecs+]
            [else
              (let g ([ntspecs+ ntspecs+] [ntspec (car ntspecs)] [remaining '()])
                (if (null? ntspecs+)
                    (cons ntspec (add-ntspecs (cdr ntspecs) remaining))
                    (let ([ntspec+ (car ntspecs+)])
                      (if (ntspec=? ntspec+ ntspec)
                          (let ([alts (add-alts (ntspec-alts ntspec) (ntspec-alts ntspec+))])
                            (cons (make-ntspec
                                    (ntspec-name ntspec+)
                                    (ntspec-meta-vars ntspec+)
                                    alts)
                              (add-ntspecs (cdr ntspecs) (append remaining (cdr ntspecs+)))))
                          (g (cdr ntspecs+) ntspec (cons (car ntspecs+) remaining))))))])))

      (define partition-terms
        (lambda (terms)
          (let f ([terms terms] [terms+ '()] [terms- '()])
            (syntax-case terms ()
              [() (values terms+ terms-)]
              [((+ t* ...) terms ...) (plus? #'+)
               (f #'(terms ...)
                  (append terms+ (parse-terms #'(t* ...))) terms-)]
              [((- t* ...) terms ...) (minus? #'-)
               (f #'(terms ...) terms+
                  (append terms- (parse-terms #'(t* ...))))]))))

      (define partition-ntspecs
        (lambda (ntspecs terminal-meta*)
          (let f ([ntspecs ntspecs] [ntspecs+ '()] [ntspecs- '()])
            (if (null? ntspecs)
                (values ntspecs+ ntspecs-) ;; lists returned are reversed (okay?)
                (let ([ntspec (car ntspecs)] [ntspecs (cdr ntspecs)])
                  (let g ([alts (cddr ntspec)] [alts+ '()] [alts- '()])
                    (syntax-case alts ()
                      [() (let ([name (car ntspec)] [metas (cadr ntspec)])
                            (f ntspecs
                               (if (null? alts+)
                                   ntspecs+
                                   (cons (make-ntspec name metas alts+)
                                     ntspecs+))
                               (if (null? alts-)
                                   ntspecs-
                                   (cons (make-ntspec name metas alts-)
                                     ntspecs-))))]
                      [((+ a* ...) alts ...) (plus? #'+)
                       (g #'(alts ...) (append alts+ (parse-alts #'(a* ...) terminal-meta*))
                          alts-)]
                      [((- a* ...) alts ...) (minus? #'-)
                       (g #'(alts ...) alts+
                          (append alts- (parse-alts #'(a* ...) terminal-meta*)))])))))))

      (define parse-alts
        (lambda (alt* terminal-meta*)
          (define make-alt
            (lambda (syn pretty pretty-procedure?)
              (syntax-case syn ()
                [(s s* ...) (make-pair-alt #'(s s* ...) pretty pretty-procedure?)]
                [(s s* ... . sr) (make-pair-alt #'(s s* ... . sr) pretty pretty-procedure?)]
                [s
                 (identifier? #'s)
                 (if (memq (meta-var->raw-meta-var (syntax->datum #'s)) terminal-meta*)
                     (make-terminal-alt #'s pretty pretty-procedure?)
                     (make-nonterminal-alt #'s pretty pretty-procedure?))])))
          (let f ([alt* alt*])
            (syntax-case alt* ()
              [() '()]
              [((=> syn pretty) . alt*) (double-arrow? #'=>)
               (cons (make-alt #'syn #'pretty #f) (f #'alt*))]
              [(syn => pretty . alt*) (double-arrow? #'=>)
               (cons (make-alt #'syn #'pretty #f) (f #'alt*))]
              [((-> syn prettyf) . alt*) (arrow? #'->)
               (with-implicit (-> with-extended-quasiquote)
                 (cons (make-alt #'syn #'(with-extended-quasiquote prettyf) #t) (f #'alt*)))]
              [(syn -> prettyf . alt*) (arrow? #'->)
               (with-implicit (-> with-extended-quasiquote)
                 (cons (make-alt #'syn #'(with-extended-quasiquote prettyf) #t) (f #'alt*)))]
              [(syn . alt*) (cons (make-alt #'syn #f #f) (f #'alt*))]
              [_ (syntax-violation 'define-language "unexpected alt" alt*)]))))

      (define parse-terms
        (lambda (term*)
          (syntax-case term* ()
            [() '()]
            [((=> (t (tmeta* ...)) handler) term* ...) (double-arrow? #'=>)
             (cons (make-tspec #'t #'(tmeta* ...) #'handler)
                   (parse-terms #'(term* ...)))]
            [((t (tmeta* ...)) => handler term* ...) (double-arrow? #'=>)
             (cons (make-tspec #'t #'(tmeta* ...) #'handler)
                   (parse-terms #'(term* ...)))]
            [((t (tmeta* ...)) term* ...) 
             (cons (make-tspec #'t #'(tmeta* ...))
                   (parse-terms #'(term* ...)))])))

      (define parse-language-and-finish
        (lambda (name ldef)
          (define parse-clauses
            (lambda (ldef)
              (let f ([ldef ldef] [base-lang #f] [found-entry #f]
                      [entry-ntspec #f] [first-ntspec #f] [terms '()] [ntspecs '()] [nongen-id #f])
                (syntax-case ldef (extends entry terminals nongenerative-id)
                  [() (values base-lang (if base-lang entry-ntspec (or entry-ntspec first-ntspec)) terms (reverse ntspecs) nongen-id)]
                  [((nongenerative-id ?id) . rest)
                   (identifier? #'?id)
                   (begin
                     (when nongen-id
                       (syntax-violation 'define-language
                         "only one nongenerative-id clause allowed in language definition"
                         #'(nongenerative-id ?id) name))
                     (f #'rest base-lang found-entry entry-ntspec first-ntspec terms ntspecs #'?id))]
                  [((extends ?L) . rest)
                   (identifier? #'?L)
                   (begin
                     (when base-lang
                       (syntax-violation 'define-language
                         "only one extends clause allowed in language definition"
                         #'(extends ?L) name))
                     (f #'rest #'?L found-entry entry-ntspec first-ntspec terms ntspecs nongen-id))]
                  [((entry ?P) . rest)
                   (identifier? #'?P)
                   (begin
                     (when found-entry
                       (syntax-violation 'define-language
                         "only one entry clause allowed in language definition"
                         #'(entry ?P) entry-ntspec))
                     (f #'rest base-lang #t #'?P first-ntspec terms ntspecs nongen-id))]
                  [((terminals ?t* ...) . rest)
                   (f #'rest base-lang found-entry entry-ntspec first-ntspec 
                      (append terms #'(?t* ...)) ntspecs nongen-id)]
                  [((ntspec (meta* ...) a a* ...) . rest)
                   (and (identifier? #'ntspec) (map identifier? #'(meta* ...)))
                   (f #'rest base-lang found-entry
                     entry-ntspec
                     (if first-ntspec first-ntspec #'ntspec)
                     terms (cons (cons* #'ntspec #'(meta* ...) #'a #'(a* ...)) ntspecs)
                     nongen-id)]
                  [(x . rest) (syntax-violation 'define-language "unrecognized clause" #'x)]
                  [x (syntax-violation 'define-language
                       "unrecognized rest of language clauses" #'x)]))))
            (let-values ([(base-lang entry-ntspec terms ntspecs nongen-id) (parse-clauses ldef)])
              (with-compile-time-environment (r)
                (if base-lang
                    (let ([base-pair (r base-lang)])
                      (unless (and (pair? base-pair)
                                   (language? (car base-pair))
                                   (procedure? (cdr base-pair)))
                        (syntax-violation 'define-language
                          "unrecognized base language" base-lang x))
                      (let ([base (car base-pair)])
                        (let ([entry-ntspec (or entry-ntspec (language-entry-ntspec base))])
                          (finish r nongen-id entry-ntspec name name
                            (let-values ([(terms+ terms-) (partition-terms terms)])
                              (let* ([tspecs (freshen-tspecs (language-tspecs base) terms-)]
                                     [tspecs (add-tspecs tspecs terms+)]
                                     [terminal-meta* (extract-terminal-metas tspecs)])
                                (let-values ([(ntspecs+ ntspecs-) (partition-ntspecs ntspecs terminal-meta*)])
                                  (let* ([ntspecs (freshen-ntspecs (language-ntspecs base) ntspecs-)]
                                         [ntspecs (add-ntspecs ntspecs ntspecs+)])
                                    (make-language name entry-ntspec tspecs ntspecs nongen-id)))))))))
                    (let* ([tspecs (parse-terms terms)]
                           [terminal-meta* (extract-terminal-metas tspecs)])
                      (finish r nongen-id entry-ntspec name name
                        (make-language name
                          entry-ntspec
                          tspecs
                          (map (lambda (ntspec)
                                 (make-ntspec (car ntspec) (cadr ntspec)
                                   (parse-alts (cddr ntspec) terminal-meta*)))
                            ntspecs)
                          nongen-id))))))))

      (define extract-terminal-metas
        (lambda (tspecs)
          (fold-left (lambda (metas tspec)
                       (append (syntax->datum (tspec-meta-vars tspec)) metas))
            '() tspecs)))

      (define finish 
        (lambda (r nongen-id ntname lang id desc) ; constructs the output
          (annotate-language! r desc id)
          (with-syntax ([(records ...) (language->lang-records desc)]
                        [(predicates ...) (language->lang-predicates desc)]
                        [unparser-name (construct-id id "unparse-" lang)]
                        [meta-parser (make-meta-parser desc)])
            #;(pretty-print (list 'unparser (syntax->datum lang) (syntax->datum #'unparser)))
            #;(pretty-print (list 'meta-parser (syntax->datum lang) (syntax->datum #'meta-parser)))
            #`(begin
                records ...
                predicates ...
                (define-syntax #,lang 
                  (make-compile-time-value
                    (cons '#,desc meta-parser)))
                #;(define-property #,lang meta-parser-property meta-parser)
                (define-unparser unparser-name #,lang)))))

      (syntax-case x ()
        [(_ ?L ?rest ...)
         (identifier? #'?L)
         (parse-language-and-finish #'?L #'(?rest ...))]
        [(_ (?L ?nongen-id) ?rest ...)
         (and (identifier? #'?L) (identifier? #'?nongen-id))
         (parse-language-and-finish #'?L #'(?rest ...))])))

  (define-syntax language->s-expression
    (lambda (x)
      (define who 'language->s-expression)
      (define doit
        (lambda (lang handler?)
          (define tspec->s-expression
            (lambda (t)
              (if (and handler? (tspec-handler t))
                  #`(=> (#,(tspec-type t) #,(tspec-meta-vars t))
                        #,(tspec-handler t))
                  #`(#,(tspec-type t) #,(tspec-meta-vars t)))))
          (define alt->s-expression
            (lambda (a)
              (if (and handler? (alt-pretty a))
                  #`(=> #,(alt-syn a) #,(alt-pretty a))
                  (alt-syn a))))
          (define ntspec->s-expression
            (lambda (p)
              #`(#,(ntspec-name p) #,(ntspec-meta-vars p)
                 #,@(map alt->s-expression (ntspec-alts p)))))
          (lambda (env)
            (let ([lang-pair (env lang)])
              (unless lang-pair (syntax-violation who "language not found" lang))
              (let ([lang (car lang-pair)])
                (with-syntax ([(ng ...) (let ([nongen-id (language-nongenerative-id lang)])
                                          (if nongen-id
                                              #`((nongenerative-id #,nongen-id))
                                              #'()))])
                  #`'(define-language #,(language-name lang)
                       ng ...
                       (entry #,(language-entry-ntspec lang))
                       (terminals #,@(map tspec->s-expression (language-tspecs lang)))
                       #,@(map ntspec->s-expression (language-ntspecs lang)))))))))
      (syntax-case x ()
        [(_ lang) (identifier? #'lang) (doit #'lang #f)]
        [(_ lang handler?) (identifier? #'lang) (doit #'lang (syntax->datum #'handler?))])))

  (define-syntax diff-languages
    (lambda (x)
      (define who 'diff-languages)
      (define combine
        (lambda (same removed added)
          (if (null? removed)
              (if (null? added)
                  '()
                  #`((+ #,@added)))
              (if (null? added)
                  #`((- #,@removed))
                  #`((- #,@removed) (+ #,@added))))))
      (define tspec->syntax
        (lambda (tspec)
          #`(#,(tspec-type tspec) #,(tspec-meta-vars tspec))))
      (define ntspec->syntax
        (lambda (ntspec)
          #`(#,(ntspec-name ntspec) #,(ntspec-meta-vars ntspec) #,@(map alt-syn (ntspec-alts ntspec)))))
      (define diff-meta-vars
        (lambda (mv0* mv1*)
          mv1*
          #;(let f ([mv0* mv0*] [mv1* mv1*] [same '()] [removed '()] [added '()])
            (cond
              [(and (null? mv0*) (null? mv1*)) (combine same removed added)]
              [(null? mv0*) (f mv0* (cdr mv1*) same removed (cons (car mv1*) added))]
              [else
               (let* ([mv0 (car mv0*)] [mv0-sym (syntax->datum mv0)])
                 (cond
                   [(find (lambda (mv1) (eq? (syntax->datum mv1) mv0-sym)) mv1*) =>
                    (lambda (mv1) (f (cdr mv0*) (remq mv1 mv1*) (cons mv1 same) removed added))]
                   [else (f (cdr mv0*) mv1* same (cons mv0 removed) added)]))]))))
      (define diff-terminals
        (lambda (t0* t1*)
          (let f ([t0* t0*] [t1* t1*] [same '()] [removed '()] [added '()])
            (cond
              [(and (null? t0*) (null? t1*)) (combine same removed added)]
              [(null? t0*) (f t0* (cdr t1*) same removed (cons (tspec->syntax (car t1*)) added))]
              [else
               (let* ([t0 (car t0*)] [t0-type (tspec-type t0)] [t0-type-sym (syntax->datum t0-type)])
                 (cond
                   [(find (lambda (t1) (eq? (syntax->datum (tspec-type t1)) t0-type-sym)) t1*) =>
                    (lambda (t1)
                      (with-syntax ([(meta-vars ...) (diff-meta-vars (tspec-meta-vars t0) (tspec-meta-vars t1))])
                        (f (cdr t0*) (remq t1 t1*) (cons #`(#,t0-type (meta-vars ...)) same) removed added)))]
                   [else (f (cdr t0*) t1* same (cons (tspec->syntax t0) removed) added)]))]))))
      (define diff-alts
        (lambda (a0* a1*)
          (let f ([a0* a0*] [a1* a1*] [same '()] [removed '()] [added '()])
            (cond
              [(and (null? a0*) (null? a1*)) (combine same removed added)]
              [(null? a0*) (f a0* (cdr a1*) same removed (cons (alt-syn (car a1*)) added))]
              [else
               (let* ([a0 (car a0*)] [a0-syn (alt-syn a0)] [a0-syn-s-expr (syntax->datum a0-syn)])
                 (cond
                   [(find (lambda (a1) (equal? (syntax->datum (alt-syn a1)) a0-syn-s-expr)) a1*) =>
                    (lambda (a1) (f (cdr a0*) (remq a1 a1*) (cons a0-syn same) removed added))]
                   [else (f (cdr a0*) a1* same (cons (alt-syn a0) removed) added)]))]))))
      (define diff-nonterminals
        (lambda (nt0* nt1*)
          (let f ([nt0* nt0*] [nt1* nt1*] [updated '()])
            (cond
              [(and (null? nt0*) (null? nt1*)) updated]
              [(null? nt0*)
               (f nt0* (cdr nt1*)
                  (let ([nt1 (car nt1*)])
                    (cons #`(#,(ntspec-name nt1) #,(ntspec-meta-vars nt1) (+ #,@(map alt-syn (ntspec-alts nt1))))
                      updated)))]
              [else
               (let* ([nt0 (car nt0*)] [nt0-name (ntspec-name nt0)] [nt0-name-sym (syntax->datum nt0-name)])
                 (cond
                   [(find (lambda (nt1) (eq? (syntax->datum (ntspec-name nt1)) nt0-name-sym)) nt1*) =>
                    (lambda (nt1)
                      (f (cdr nt0*) (remq nt1 nt1*)
                         (let ([alts (diff-alts (ntspec-alts nt0) (ntspec-alts nt1))])
                           (syntax-case alts ()
                             [() updated]
                             [(alts ...)
                              (with-syntax ([(meta-vars ...) (diff-meta-vars (ntspec-meta-vars nt0) (ntspec-meta-vars nt1))])
                                (cons #`(#,nt0-name (meta-vars ...) alts ...) updated))]))))]
                   [else (f (cdr nt0*) nt1* (cons #`(#,nt0-name #,(ntspec-meta-vars nt0) (- #,@(map alt-syn (ntspec-alts nt0)))) updated))]))]))))
      (syntax-case x ()
        [(_ lang0 lang1)
         (with-compile-time-environment (r)
           (let ([l0-pair (r #'lang0)] [l1-pair (r #'lang1)])
             (unless l0-pair (syntax-violation who "language not found" #'lang0))
             (unless l1-pair (syntax-violation who "language not found" #'lang1))
             (let ([l0 (car l0-pair)] [l1 (car l1-pair)])
               (with-syntax ([l1-entry (language-entry-ntspec l1)]
                             [(term ...) (diff-terminals (language-tspecs l0) (language-tspecs l1))]
                             [(nonterm ...) (diff-nonterminals (language-ntspecs l0) (language-ntspecs l1))]
                             [(ng ...) (let ([nongen-id (language-nongenerative-id l1)])
                                         (if nongen-id
                                             #`((nongenerative-id #,nongen-id))
                                             #'()))])
                 (syntax-case #'(term ...) ()
                   [() #''(define-language lang1 (extends lang0)
                            ng ...
                            (entry l1-entry)
                            nonterm ...)]
                   [(term ...) #''(define-language lang1 (extends lang0)
                                    ng ...
                                    (entry l1-entry)
                                    (terminals term ...)
                                    nonterm ...)])))))])))
  (define-syntax prune-language
    (lambda (x)
      (define who 'prune-language)
      (syntax-case x ()
        [(_ L)
         (with-compile-time-environment (r)
           (let ([l-pair (r #'L)])
             (unless l-pair (syntax-violation who "language not found" #'L))
             (let ([l (car l-pair)])
               (with-syntax ([((ts ...) (nts ...)) (prune-language-helper l)]
                             [entry-nt (language-entry-ntspec l)])
                 (syntax-case #'(ts ...) ()
                   [() #''(define-language L
                            (entry entry-nt)
                            nts ...)]
                   [(ts ...) #''(define-language L
                                  (entry entry-nt)
                                  (terminals ts ...)
                                  nts ...)])))))])))
  
  (define-syntax define-pruned-language
    (lambda (x)
      (define who 'define-pruned-language)
      (syntax-case x ()
        [(_ L new-name)
         (with-compile-time-environment (r)
           (let ([l-pair (r #'L)])
             (unless l-pair (syntax-violation who "language not found" #'L))
             (let ([l (car l-pair)])
               (with-syntax ([((ts ...) (nts ...)) (prune-language-helper l)]
                             [entry-nt (language-entry-ntspec l)])
                 #'(define-language new-name
                     (entry entry-nt)
                     (terminals ts ...)
                     nts ...)))))]))))

