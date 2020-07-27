;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (nanopass unparser)
  (export define-unparser)
  (import (rnrs)
          (nanopass helpers)
          (nanopass records)
          (nanopass syntaxconvert)) 
  
  (define-syntax define-unparser
    (lambda (x)
      (define make-unparser-name-assoc
        (lambda (tid)
          (lambda (ntspec)
            (cons ntspec (construct-unique-id tid "unparse-" (syntax->datum (ntspec-name ntspec)))))))
      (define make-unparse-term-clause-body-assoc
        (lambda (tspec)
          (cons tspec
                (let ([h (tspec-handler tspec)])
                  (if h
                      #`(if raw? ir (#,h ir))
                      #'ir)))))
      (define make-unparser
        (lambda (unparser-name desc)
          (let* ([lang-name (language-name desc)]
                 [ntspecs (language-ntspecs desc)]
                 [tspecs (language-tspecs desc)]
                 [unparser-names (map (make-unparser-name-assoc unparser-name) ntspecs)]
                 [tspec-bodies (map make-unparse-term-clause-body-assoc tspecs)])
            (define (lookup-unparser ntspec)
              (cond
                [(assq ntspec unparser-names) => cdr]
                [else (syntax-violation 'define-unparser
                        (format "unexpected nonterminal ~s in language ~s, expected one of ~s"
                          (syntax->datum (ntspec-name ntspec)) (syntax->datum lang-name)
                          (map (lambda (nt) (syntax->datum (ntspec-name nt))) ntspecs))
                        unparser-name x)]))
            (define (lookup-tspec-body tspec)
              (cond
                [(assq tspec tspec-bodies) => cdr]
                [else (syntax-violation 'define-unparser
                        (format "unexpected terminal ~s in language ~s, expected one of ~s"
                          (syntax->datum (tspec-type tspec)) (syntax->datum lang-name)
                          (map (lambda (t) (syntax->datum (tspec-type t))) tspecs))
                        unparser-name x)]))
            (with-syntax ([unparser-name unparser-name]
                          [(proc-name ...) (map cdr unparser-names)]
                          [(ntspec? ...) (map ntspec-pred ntspecs)]
                          [(tspec? ...) (map tspec-pred tspecs)]
                          [(tspec-body ...) (map cdr tspec-bodies)])
              (define make-unparse-proc
                (lambda (ntspec)
                  ;; handles alts of the form: LambdaExpr where LambdaExpr is another
                  ;; non-terminal specifier with no surrounding markers.
                  (define make-nonterm-clause
                    (lambda (alt)
                      (let ([ntspec (nonterminal-alt-ntspec alt)])
                        (list #`((#,(ntspec-all-pred ntspec) ir)
                                 (#,(lookup-unparser ntspec) ir))))))
                  ;; handles alts of the form: x, c where x and c are meta-variables
                  ;; that refer to terminals, and have no surrounding marker.
                  (define-who make-term-clause ;; only atom alt cases
                    (lambda (alt)
                      (let ([tspec (terminal-alt-tspec alt)])
                        #`((#,(tspec-pred tspec) ir)
                           #,(lookup-tspec-body tspec)))))

                  (define strip-maybe
                    (lambda (tmpl)
                      (syntax-case tmpl (maybe)
                        [(maybe x) (and (identifier? #'x) (eq? (datum maybe) 'maybe)) #'x]
                        [(a . d) (with-syntax ([a (strip-maybe #'a)] [d (strip-maybe #'d)]) #'(a . d))]
                        [() tmpl]
                        [oth tmpl])))

                  (define build-accessor-expr
                    (lambda (acc level maybe?)
                      (let loop ([level level] [f #`(lambda (t) 
                                                      #,(if maybe?
                                                            #'(and t (unparser-name t raw?))
                                                            #'(unparser-name t raw?)))])
                        (if (fx=? level 0)
                            #`(#,f (#,acc ir))
                            (loop (fx- level 1) #`(lambda (t) (map #,f t)))))))

                  (define build-template-wrapper
                    (lambda (tmpl alt)
                      (with-syntax ([(e ...) (map build-accessor-expr
                                                  (pair-alt-accessors alt)
                                                  (pair-alt-field-levels alt)
                                                  (pair-alt-field-maybes alt))]
                                    [(fld ...) (pair-alt-field-names alt)]
                                    [tmpl tmpl])
                        #'(let ([fld e] ...)
                            (with-extended-quasiquote
                              (with-auto-unquote (fld ...) `tmpl))))))

                  (define make-pair-clause
                    (lambda (alt)
                      (with-syntax ([pred? (pair-alt-pred alt)]
                                    [raw-body (build-template-wrapper (strip-maybe (alt-syn alt)) alt)])
                        #`((pred? ir)
                           #,(let ([pretty (alt-pretty alt)])
                               (if pretty
                                   #`(if raw?
                                         raw-body
                                         #,(if (alt-pretty-procedure? alt)
                                               (with-syntax ([(acc ...) (pair-alt-accessors alt)])
                                                 #`(#,pretty unparser-name (acc ir) ...))
                                               (build-template-wrapper pretty alt)))
                                   #'raw-body))))))

                  ;; When one nonterminalA alternative is another nonterminalB, we
                  ;; expand all the alternatives of nonterminalB with the alternatives
                  ;; of nonterminalA However, nonterminalA and nonterminalB cannot
                  ;; (both) have an implicit case, by design.
                  (partition-syn (ntspec-alts ntspec)
                    ([term-alt* terminal-alt?] [nonterm-alt* nonterminal-alt?] [pair-alt* otherwise])
                    (partition-syn nonterm-alt*
                      ([nonterm-imp-alt* (lambda (alt)
                                           (has-implicit-alt?
                                             (nonterminal-alt-ntspec alt)))]
                       [nonterm-nonimp-alt* otherwise])
                      #`(lambda (ir)
                          (cond
                            #,@(map make-term-clause term-alt*)
                            #,@(map make-pair-clause pair-alt*)
                            ;; note: the following two can potentially be combined
                            #,@(apply append (map make-nonterm-clause nonterm-nonimp-alt*))
                            #,@(apply append (map make-nonterm-clause nonterm-imp-alt*))
                            [else (error who "invalid record" ir)]))))))
              (with-syntax ([(proc ...) (map make-unparse-proc ntspecs)])
                #'(define-who unparser-name
                    (case-lambda
                      [(ir) (unparser-name ir #f)]
                      [(ir raw?)
                       (define-who proc-name proc) ...
                       (cond
                         [(ntspec? ir) (proc-name ir)] ...
                         [(tspec? ir) tspec-body] ...
                         [else (error who "unrecognized language record" ir)])])))))))
      (syntax-case x ()
        [(_ name lang)
         (and (identifier? #'name) (identifier? #'lang))
         (with-compile-time-environment (r)
           (let ([l-pair (r #'lang)])
             (unless (pair? l-pair)
               (syntax-violation 'define-unparser "unknown language" #'lang x))
             (make-unparser #'name (car l-pair))))]))))
