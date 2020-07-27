;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details
(library (nanopass parser)
  (export define-parser trace-define-parser)
  (import (rnrs)
          (nanopass helpers)
          (nanopass records)
          (nanopass syntaxconvert)
          (nanopass nano-syntax-dispatch))

  (define-syntax parse-or
    (syntax-rules (on-error)
      [(_ (on-error ?err0)) ?err0]
      [(_ (on-error ?err0) ?e0 . ?e1)
       (let ([t0 ?e0])
         (if (eq? t0 np-parse-fail-token)
             (parse-or (on-error ?err0) . ?e1)
             t0))]))

  (define-syntax define-parser
    (syntax-rules ()
      [(_ . rest) (x-define-parser . rest)]))

  (define-syntax trace-define-parser
    (syntax-rules ()
      [(_ . rest) (x-define-parser trace . rest)]))

  (define-syntax x-define-parser
    (lambda (x)
      (define make-parser-name-assoc
        (lambda (tid)
          (lambda (ntspec)
            (let ([name-sym (syntax->datum (ntspec-name ntspec))])
              (cons name-sym (construct-unique-id tid "parse-" name-sym))))))
      (define make-parser
        (lambda (parser-name lang trace?)
          (with-compile-time-environment (r)
            (let ([who (if trace? 'trace-define-parser 'define-parser)]
                  [desc-pair (guard (c [else #f]) (r lang))])
              (unless desc-pair
                (syntax-violation who
                  (format "unknown language ~s" (syntax->datum lang))
                  parser-name x))
              (let* ([desc (car desc-pair)]
                     [lang-name (language-name desc)]
                     [ntspecs (language-ntspecs desc)]
                     [tspecs (language-tspecs desc)]
                     [parser-names (map (make-parser-name-assoc parser-name) ntspecs)])
                (define lookup-parser-name
                  (lambda (name)
                    (cond
                      [(assq (syntax->datum name) parser-names) => cdr]
                      [else (syntax-violation who
                              (format "unexpected nonterminal ~s in language ~s, expected one of ~s"
                                (syntax->datum name) (syntax->datum lang-name)
                                (map (lambda (nt) (syntax->datum (ntspec-name nt))) ntspecs))
                              parser-name x)])))
                (define make-parse-proc
                  (lambda (desc tspecs ntspecs ntspec lang-name)
                    (define parse-field
                      (lambda (m level maybe?)
                        (cond
                          [(meta-name->tspec m tspecs) m]
                          [(meta-name->ntspec m ntspecs) =>
                           (lambda (spec)
                             (with-syntax ([proc-name (lookup-parser-name (ntspec-name spec))])
                               (let f ([level level] [x m])
                                 (if (= level 0)
                                     (if maybe? #`(and #,x (proc-name #,x #t))  #`(proc-name #,x #t))
                                     #`(map (lambda (x) #,(f (- level 1) #'x)) #,x)))))]
                          [else (syntax-violation who
                                  (format "unrecognized meta-variable ~s in language ~s"
                                    (syntax->datum m) (syntax->datum lang-name))
                                  parser-name x)])))
                    (define make-term-clause
                      (lambda (alt)
                        (with-syntax ([term-pred?
                                       (cond
                                         [(meta-name->tspec (alt-syn alt) tspecs) => tspec-pred]
                                         [else (syntax-violation who
                                                 (format "unrecognized terminal meta-variable ~s in language ~s"
                                                   (syntax->datum (alt-syn alt)) (syntax->datum lang-name))
                                                 parser-name x)])])
                          #'[(term-pred? s-exp) s-exp])))

                    (define make-nonterm-clause
                      (lambda (alt)
                        (let ([spec (meta-name->ntspec (alt-syn alt) ntspecs)])
                          (unless spec
                            (syntax-violation who
                              (format "unrecognized nonterminal meta-variable ~s in language ~s"
                                (syntax->datum (alt-syn alt)) (syntax->datum lang-name))
                              parser-name x))
                          (with-syntax ([proc-name (lookup-parser-name (ntspec-name spec))])
                            #`(proc-name s-exp #f)))))

                    (define make-pair-clause
                      (lambda (alt)
                        (with-syntax ([maker (pair-alt-maker alt)]
                                      [(field-var ...) (pair-alt-field-names alt)])
                          (with-syntax ([(parsed-field ...)
                                         (map parse-field #'(field-var ...)
                                              (pair-alt-field-levels alt)
                                              (pair-alt-field-maybes alt))]
                                        [(msg ...) (map (lambda (x) #f) #'(field-var ...))]
                                        [field-pats (datum->syntax #'* (pair-alt-pattern alt))])
                            #`[#,(if (pair-alt-implicit? alt)
                                     #'(nano-syntax-dispatch s-exp 'field-pats)
                                     (with-syntax ([key (car (alt-syn alt))])
                                       #'(and (eq? 'key (car s-exp))
                                              (nano-syntax-dispatch (cdr s-exp) 'field-pats))))
                               =>
                               (lambda (ls)
                                 (apply
                                   (lambda (field-var ...)
                                     (let ([field-var parsed-field] ...)
                                       (maker who field-var ... msg ...))) ls))]))))

                    (partition-syn (ntspec-alts ntspec)
                      ([term-alt* terminal-alt?]
                       [nonterm-alt* nonterminal-alt?]
                       [pair-imp-alt* pair-alt-implicit?]
                       [pair-alt* otherwise])
                      (partition-syn nonterm-alt*
                        ([nonterm-imp-alt* (lambda (alt) (has-implicit-alt?  (nonterminal-alt-ntspec alt)))]
                         [nonterm-nonimp-alt* otherwise])
                        #`(lambda (s-exp at-top-parse?)
                            (parse-or
                              (on-error
                                (if at-top-parse?
                                    (error who (format "invalid syntax ~s" s-exp))
                                    np-parse-fail-token))
                              #,@(map make-nonterm-clause nonterm-nonimp-alt*)
                              (if (pair? s-exp)
                                  (cond
                                    #,@(map make-pair-clause pair-alt*)
                                    #,@(map make-pair-clause pair-imp-alt*)
                                    [else np-parse-fail-token])
                                  (cond
                                    #,@(map make-term-clause term-alt*)
                                    [else np-parse-fail-token]))
                              #,@(map make-nonterm-clause nonterm-imp-alt*)))))))
                (with-syntax ([(parse-name ...) (map cdr parser-names)]
                              [(parse-proc ...)
                               (map (lambda (ntspec)
                                      (make-parse-proc desc tspecs ntspecs ntspec lang-name))
                                    ntspecs)])
                  (with-syntax ([entry-proc-name (lookup-parser-name (language-entry-ntspec desc))]
                                [parser-name parser-name])
                    (with-syntax ([(lam-exp ...) (if trace? #'(trace-lambda parser-name) #'(lambda))]
                                  [def (if trace? #'trace-define #'define)])
                      #'(define-who parser-name
                          (lam-exp ... (s-exp)
                            (def parse-name parse-proc)
                            ...
                            (entry-proc-name s-exp #t)))))))))))
      (syntax-case x (trace)
        [(_ parser-name lang)
         (and (identifier? #'parser-name) (identifier? #'lang))
         (make-parser #'parser-name #'lang #f)]
        [(_ trace parser-name lang)
         (and (identifier? #'parser-name) (identifier? #'lang))
         (make-parser #'parser-name #'lang #t)]))))
