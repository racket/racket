;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

;;; TODO:
;;; 1. write make-processors (based on make-processor, currently in meta-parsers
;;; 2. add else clause to processors
;;; Make sure the following are obeyed:
;;; 1. allow ir to be named
;;; 2. loosen up form of pass body
;;; 3. don't require () in pass body
;;; 4. add else clause
;;; 5. support Datum output
;;; 6. don't bind quasiquote with Datum output
;;; 7. make cata work with Datum output

(library (nanopass pass)
  (export define-pass trace-define-pass echo-define-pass with-output-language nanopass-case)
  (import (rnrs)
          (nanopass helpers)
          (nanopass records)
          (nanopass syntaxconvert)
          (nanopass meta-parser)
          (rnrs mutable-pairs))

  ;; NOTE: the following is less general then the with-output-language because it does not
  ;; support multiple return values.  It also generates nastier code for the expander to deal
  ;; with, though cp0 should clean it up.  It is possible that in the long run, we'll want to
  ;; have a separate pass-lambda form, or that we'll loosen up the body further to return
  ;; multiple values even when they aren't specified. For now, this is moth-balled.
  #;(define-syntax with-output-language
    (lambda (x)
      (syntax-case x ()
        [(k (lang type) b b* ...)
         (with-syntax ([pass (datum->syntax #'k 'pass)])
           #'(let ()
               (define-pass pass : * () -> (lang type) () (begin b b* ...))
               (pass)))]
        [(k lang b b* ...)
         (with-syntax ([pass (datum->syntax #'k 'pass)])
           #'(let ()
               (define-pass pass : * () -> lang  () (begin b b* ...))
               (pass)))])))

  (define-syntax with-output-language
    (lambda (x)
      (with-compile-time-environment (r)
        (syntax-case x ()
          [(id (lang type) b b* ...)
           (let* ([olang-pair (r #'lang)]
                  [olang (and olang-pair (car olang-pair))]
                  [meta-parser (and olang-pair (cdr olang-pair))])
             (unless (language? olang)
               (syntax-violation 'with-output-language "unrecognized language" #'lang))
             (unless (procedure? meta-parser)
               (syntax-violation 'with-output-language "missing meta parser for language" #'lang))
             (with-syntax ([in-context (datum->syntax #'id 'in-context)]
                           [quasiquote (datum->syntax #'id 'quasiquote)])
               #`(let-syntax ([quasiquote '#,(make-quasiquote-transformer
                                               #'id #'type olang
                                               meta-parser)]
                              [in-context '#,(make-in-context-transformer
                                               #'id olang
                                               meta-parser)])
                   b b* ...)))]
          [(id lang b b* ...)
           (let* ([olang-pair (r #'lang)]
                  [olang (and olang-pair (car olang-pair))]
                  [meta-parser (and olang-pair (cdr olang-pair))])
             (unless (language? olang)
               (syntax-violation 'with-output-language "unrecognized language" #'lang))
             (unless (procedure? meta-parser)
               (syntax-violation 'with-output-language "missing meta parser for language" #'lang))
             (with-syntax ([in-context (datum->syntax #'id 'in-context)])
               #`(let-syntax
                   ([in-context '#,(make-in-context-transformer #'id olang
                                     meta-parser)])
                   b b* ...)))]))))

  (define-syntax nanopass-case
    ; (nanopass-case (lang type) id ---) rebinds id so that it always holds the
    ; current ir even through cata recursion
    (lambda (x) 
      (syntax-case x (else)
        [(k (lang type) x cl ... [else b0 b1 ...])
         (identifier? #'x)
         (with-syntax ([quasiquote (datum->syntax #'k 'quasiquote)]) ; if we were in a rhs, pick-up the output quasiquote
           #'(let ()
               (define-pass p : (lang type) (x) -> * (val)
                 (proc : type (x) -> * (val) cl ... [else b0 b1 ...])
                 (proc x))
               (p x)))]
        [(k (lang type) e cl ... [else b0 b1 ...])
         #'(let ([ir e]) (k (lang type) ir cl ... [else b0 b1 ...]))]
        [(k (lang type) e cl ...)
         #`(k (lang type) e cl ...
             [else (error 'nanopass-case
                     ; TODO: we were using $strip-wrap here, should be something like
                     ; $make-source-oops, but at least pseudo r6rs portable if possible
                     #,(let ([si (syntax->source-info x)])
                         (if si
                             (format "empty else clause hit ~s ~a"
                               (syntax->datum x) si)
                             (format "empty else clause hit ~s"
                               (syntax->datum x)))))])])))

  (define-syntax trace-define-pass
    (lambda (x)
      (define unparser
        (lambda (lang)
          (cond
            [(eq? (syntax->datum lang) '*) #f]
            [(identifier? lang) (construct-id lang "unparse-" lang)]
            [else (syntax-case lang ()
                    [(lang type) (construct-id #'lang "unparse-" #'lang)])])))
      (syntax-case x ()
        [(_ name ?colon ilang (id ...) ?arrow olang (xtra ...) . body)
         (and (identifier? #'name) (eq? (datum ?arrow) '->) (eq? (datum ?colon) ':)
              (for-all identifier? #'(id ...)))
         (let ([iunparser (unparser #'ilang)] [ounparser (unparser #'olang)])
           #`(define name
               (lambda (id ...)
                 (define-pass name ?colon ilang (id ...) ?arrow olang (xtra ...) . body)
                 (let ([tpass name])
                   #,(if iunparser
                         (if ounparser
                             (with-syntax ([(ot xvals ...) (generate-temporaries #'(name xtra ...))]
                                            [(tid xargs ...) (generate-temporaries #'(id ...))]
                                            [(id id* ...) #'(id ...)])
                               #`(let ([result #f])
                                   (trace-let name ([tid (#,iunparser id #t)] [xargs id*] ...)
                                     (let-values ([(ot xvals ...) (tpass id id* ...)])
                                       (set! result (list ot xvals ...))
                                       (values (#,ounparser ot #t) xvals ...)))
                                   (apply values result)))
                             (with-syntax ([(xvals ...) (generate-temporaries #'(xtra ...))]
                                            [(tid xargs ...) (generate-temporaries #'(id ...))]
                                            [(id id* ...) #'(id ...)])
                               #`(trace-let name ([tid (#,iunparser id #t)] [xargs id*] ...)
                                   (tpass id id* ...))))
                         (if ounparser
                             (with-syntax ([(ot xvals ...) (generate-temporaries #'(name xtra ...))])
                               #`(let ([result #f])
                                   (trace-let name ([id id] ...)
                                     (let-values ([(ot xvals ...) (tpass id ...)])
                                       (set! result (list ot xvals ...))
                                       (values (#,ounparser ot #t) xvals ...)))
                                   (apply values result)))
                             #`(trace-let name ([id id] ...)
                                 (tpass id ...))))))))])))

  (define-syntax define-pass
    (syntax-rules ()
      [(_ . more) (x-define-pass . more)]))

  (define-syntax echo-define-pass
    (lambda (x)
      (define parse-options
        (lambda (body)
          (let loop ([rest body] [defn #f] [pass-options '()])
            (syntax-case rest ()
              [() (if defn
                      #`(#,pass-options #,defn)
                      #`(#,pass-options))]
              [((definitions . defn) . rest)
               (eq? (datum definitions) 'definitions)
               (loop #'rest #'(definitions . defn) pass-options)]
              [((?pass-options ?options ...) . rest)
               (eq? (datum ?pass-options) 'pass-options)
               (loop #'rest defn #'(?options ...))]
              [_ (if defn
                     #`(#,pass-options #,defn . #,rest)
                     #`(#,pass-options . #,rest))]))))
      (syntax-case x ()
        [(_ name ?colon ilang (fml ...) ?arrow olang (xval ...) . body)
         (and (identifier? #'name)
              (eq? (datum ?colon) ':)
              (or (identifier? #'ilang)
                  (syntax-case #'ilang ()
                    [(ilang itype) (and (identifier? #'ilang) (identifier? #'itype))]
                    [_ #f]))
              (or (identifier? #'olang)
                  (syntax-case #'olang ()
                    [(olang otype) (and (identifier? #'olang) (identifier? #'otype))]
                    [_ #f]))
              (for-all identifier? #'(fml ...)))
         (with-syntax ([((options ...) . body) (parse-options #'body)])
           #'(x-define-pass name ?colon ilang (fml ...) ?arrow olang (xval ...)
               (pass-options (echo #t) options ...) . body))])))

  (define-syntax x-define-pass
    (lambda (x)
      (define who 'define-pass)

      (define-record-type pass-options
        (nongenerative)
        (fields echo? generate-transformers?)
        (protocol
          (lambda (new)
            (case-lambda
              [() (new #f #t)]
              [(options)
               (let loop ([options options] [echo? #f] [gt? #t])
                 (syntax-case options ()
                   [() (new echo? gt?)]
                   [((?echo ?bool) . options)
                     (and (identifier? #'?echo)
                          (eq? (datum ?echo) 'echo)
                          (boolean? (datum ?bool)))
                     (loop #'options (datum ?bool) gt?)]
                   [((?generate-transformers ?bool) . options)
                     (and (identifier? #'?generate-transformers)
                          (eq? (datum ?generate-transformers) 'generate-transformers)
                          (boolean? (datum ?bool)))
                     (loop #'options echo? (datum ?bool))]
                   [(opt . options) (syntax-violation who "invalid pass option" x #'opt)]))]))))

      (define-record-type pass-desc
        (nongenerative)
        (fields name maybe-ilang maybe-olang (mutable pdesc*)))

      (define-record-type pdesc
        (nongenerative)
        (fields name maybe-itype fml* dflt* maybe-otype xval* body trace? echo?))

      (define-record-type pclause
        (nongenerative)
        (fields lhs guard id rhs-arg* rhs-lambda
          (mutable used? pclause-used? pclause-used-set!)
          (mutable related-alt*))
        (protocol
          (lambda (new)
            (lambda (lhs guard id rhs-arg* rhs-lambda)
              (new lhs guard id rhs-arg* rhs-lambda #f '())))))

      (define make-processors
        (lambda (pass-desc pass-options maybe-imeta-parser maybe-ometa-parser)
          (let loop ([pdesc* (pass-desc-pdesc* pass-desc)] [processor* '()])
            (if (null? pdesc*)
                (let ([pdesc* (let ([ls (pass-desc-pdesc* pass-desc)])
                                (list-head ls (fx- (length ls) (length processor*))))])
                  (if (null? pdesc*)
                      processor*
                      (loop pdesc* processor*)))
                (loop (cdr pdesc*)
                  (cons (make-processor pass-desc pass-options maybe-imeta-parser maybe-ometa-parser (car pdesc*))
                    processor*))))))

      (define make-processor
        (lambda (pass-desc pass-options maybe-imeta-parser maybe-ometa-parser pdesc)
          (define echo-processor
            (lambda (result)
              (when (pdesc-echo? pdesc)
                (printf "~s in pass ~s expanded into:\n"
                  (syntax->datum (pdesc-name pdesc))
                  (syntax->datum (pass-desc-name pass-desc)))
                (pretty-print (syntax->datum result)))
              result))
          (with-syntax ([lambda-expr (make-processor-lambda pass-desc pass-options maybe-imeta-parser maybe-ometa-parser pdesc)]
                        [name (pdesc-name pdesc)])
            (echo-processor    
              #`(define name
                  #,(if (pdesc-trace? pdesc)
                        (let ([maybe-ilang (pass-desc-maybe-ilang pass-desc)]
                              [maybe-olang (pass-desc-maybe-olang pass-desc)])    
                          (let ([iunparser (and maybe-ilang (pdesc-maybe-itype pdesc)
                                                (let ([ilang (language-name maybe-ilang)])
                                                  (construct-id ilang "unparse-" ilang)))]
                                [ounparser (and maybe-olang (pdesc-maybe-otype pdesc)
                                                (let ([olang (language-name maybe-olang)])
                                                  (construct-id olang "unparse-" olang)))])
                            (if iunparser
                                (if ounparser
                                    (with-syntax ([(fml fml* ...) (generate-temporaries (pdesc-fml* pdesc))]
                                                  [(ot xrt ...) (generate-temporaries (cons 'ot (pdesc-xval* pdesc)))]
                                                  [(tot txrt ...) (generate-temporaries (cons 'tot (pdesc-xval* pdesc)))])
                                      #`(lambda (fml fml* ...)
                                          (let ([tproc lambda-expr])
                                            (let ([ot #f] [xrt #f] ...)
                                              (trace-let name ([t (#,iunparser fml #t)] [fml* fml*] ...)
                                                (let-values ([(tot txrt ...) (tproc fml fml* ...)])
                                                  (set! ot tot)
                                                  (set! xrt txrt) ...
                                                  (values (#,ounparser tot #t) txrt ...)))
                                              (values ot xrt ...)))))
                                    (with-syntax ([(fml fml* ...) (generate-temporaries (pdesc-fml* pdesc))])
                                      #`(lambda (fml fml* ...)
                                          (let ([tproc lambda-expr])
                                            (trace-let name ([t (#,iunparser fml #t)] [fml* fml*] ...)
                                              (tproc fml fml* ...))))))
                                (if ounparser
                                    (with-syntax ([(fml ...) (generate-temporaries (pdesc-fml* pdesc))]
                                                  [(ot xrt ...) (generate-temporaries (cons 'ot (pdesc-xval* pdesc)))]
                                                  [(tot txrt ...) (generate-temporaries (cons 'tot (pdesc-xval* pdesc)))])
                                      #`(lambda (fml ...)
                                          (let ([tproc lambda-expr])
                                            (let ([ot #f] [xrt #f] ...)
                                              (trace-let name ([fml fml] ...)
                                                (let-values ([(tot txrt ...) (tproc fml ...)])
                                                  (set! ot tot)
                                                  (set! xrt txrt) ...
                                                  (values (#,ounparser tot #t) txrt ...)))
                                              (values ot xrt ...)))))
                                    (with-syntax ([(fml ...) (generate-temporaries (pdesc-fml* pdesc))])
                                      #'(lambda (fml ...)
                                          (let ([tproc lambda-expr])
                                            (trace-let name ([fml fml] ...)
                                              (tproc fml ...)))))))))
                        #'lambda-expr))))))
    
      (define make-processor-lambda
        (lambda (pass-desc pass-options maybe-imeta-parser maybe-ometa-parser pdesc)
          (let ([maybe-olang (pass-desc-maybe-olang pass-desc)]
                [maybe-otype (pdesc-maybe-otype pdesc)] ; HERE
                [tfml (car (generate-temporaries '(x)))]
                [fml* (pdesc-fml* pdesc)])
            #`(lambda #,fml*
                (let ([#,tfml #,(car fml*)])
                  #,@((lambda (forms)
                        (if maybe-olang
                            (list
                              (rhs-in-context-quasiquote (pass-desc-name pass-desc)
                                maybe-otype maybe-olang maybe-ometa-parser #`(begin #,@forms)))
                            forms))
                      (if (let ([maybe-itype (pdesc-maybe-itype pdesc)])
                            (and maybe-itype (nonterm-id->ntspec? maybe-itype
                                               (language-ntspecs
                                                 (pass-desc-maybe-ilang pass-desc)))))
                          (let-values  ([(body defn*)
                                         (syntax-case (pdesc-body pdesc) ()
                                           [((definitions defn* ...) . body)
                                            (eq? (datum definitions) 'definitions)
                                            (values #'body #'(defn* ...))]
                                           [body (values #'body '())])])
                            #`(#,@defn*
                                 #,(make-processor-clauses pass-desc pass-options tfml maybe-imeta-parser maybe-ometa-parser pdesc body)))
                          (pdesc-body pdesc))))))))

      (define make-processor-clauses
        (lambda (pass-desc pass-options tfml imeta-parser maybe-ometa-parser pdesc cl*)
          (let* ([itype (pdesc-maybe-itype pdesc)] ; HERE
                 [ilang (pass-desc-maybe-ilang pass-desc)]
                 [intspec* (language-ntspecs ilang)]
                 [maybe-otype (pdesc-maybe-otype pdesc)] ; HERE
                 [maybe-olang (pass-desc-maybe-olang pass-desc)]
                 [maybe-ontspec* (and maybe-otype (language-ntspecs maybe-olang))]
                 [fml* (pdesc-fml* pdesc)]
                 [fml tfml]
                 [xfml* (cdr fml*)])
            (define parse-clauses
              (lambda (cl*)
                (define nano-meta->fml*
                  (lambda (cl nm)
                    (let f ([nrec* (nano-meta-fields nm)] [fml* '()])
                      (fold-right
                        (rec g
                          (lambda (nrec fml*)
                            (cond
                              [(nano-dots? nrec) (g (nano-dots-x nrec) fml*)]
                              [(nano-unquote? nrec) (cons (nano-unquote-x nrec) fml*)]
                              [(nano-cata? nrec)
                               (let ([fml* (append
                                             (let ([outid* (nano-cata-outid* nrec)])
                                               (if (and maybe-olang (not (null? outid*))
                                                        (eq? (syntax->datum (car outid*)) '*))
                                                   (cdr outid*)
                                                   outid*))
                                             fml*)]
                                     [maybe-inid* (nano-cata-maybe-inid* nrec)])
                                 (if (and maybe-inid*
                                          (let ([id (car maybe-inid*)])
                                            (and (identifier? id)
                                                 (not (memp (lambda (fml)
                                                              (free-identifier=? fml id))
                                                        fml*)))))
                                     (cons (car maybe-inid*) fml*)
                                     fml*))]
                              [(nano-meta? nrec) (f (nano-meta-fields nrec) fml*)]
                              [(list? nrec) (f nrec fml*)]
                              [(nano-quote? nrec) (syntax-violation who "quoted terminals currently unsupported in match patterns" (nano-quote-x nrec) cl)]
                              [else (error who "unrecognized nano-rec" nrec)])))
                        fml* nrec*))))
                (define (helper cl lhs guard rhs rhs*)
                  (let ([nano-meta (imeta-parser itype lhs #t)])
                    (let ([fml* (nano-meta->fml* cl nano-meta)])
                      (unless (all-unique-identifiers? fml*)
                        (syntax-violation who "pattern binds one or more identifiers more then once" lhs))
                      (make-pclause nano-meta guard
                        (datum->syntax #'* (gensym "rhs"))
                        fml* #`(lambda #,fml* #,rhs #,@rhs*)))))
                (let f ([cl* cl*] [pclause* '()])
                  (if (null? cl*)
                      (values (reverse pclause*) #f #f)
                      (syntax-case (car cl*) (guard else)
                        [[else rhs0 rhs1 ...]
                         (null? (cdr cl*))
                         (values (reverse pclause*)
                           #'else-th #'(lambda () (begin rhs0 rhs1 ...)))]
                        [[lhs (guard g0 g1 ...) rhs0 rhs1 ...]
                         (f (cdr cl*)
                            (cons (helper (car cl*) #'lhs #'(and g0 g1 ...) #'rhs0 #'(rhs1  ...)) pclause*))]
                        [[lhs rhs0 rhs1 ...]
                         (f (cdr cl*) (cons (helper (car cl*) #'lhs #t #'rhs0 #'(rhs1  ...)) pclause*))]
                        [_ (syntax-violation (syntax->datum (pass-desc-name pass-desc))
                             "invalid processor clause" (pdesc-name pdesc) (car cl*))])))))
            (module (make-clause generate-system-clauses)
              (define make-system-clause
                (lambda (alt)
                  (define genmap
                    (lambda (proc level maybe? arg args)
                      (define add-maybe
                        (lambda (e-arg e)
                          (if maybe? #`(let ([t #,e-arg]) (and t #,e)) e)))
                      (cond
                        [(fx=? level 0) (add-maybe arg #`(#,proc #,arg #,@args))]
                        [(fx=? level 1) #`(map (lambda (m) #,(add-maybe #'m #`(#,proc m #,@args))) #,arg)]
                        [else
                          (genmap
                            #`(lambda (x) (map (lambda (m) #,(add-maybe #'m #`(#,proc m #,@args))) x))
                            (fx- level 1)
                            #f ; once we've applied the maybe turn it off, since we can have a
                               ; list of maybes but not maybe of a list.
                            arg '())])))
                  (define-who process-alt
                    (lambda (in-altsyn in-altrec out-altrec)
                      (define process-alt-field
                        (lambda (level maybe? fname aname ofname)
                          (if (and (nonterminal-meta? fname intspec*)
                                   (nonterminal-meta? ofname maybe-ontspec*))
                              (let ([callee-pdesc
                                     (find-proc pass-desc pass-options (pdesc-name pdesc)
                                       (syntax->datum (spec-type (find-spec fname ilang)))
                                       (syntax->datum (spec-type (find-spec ofname maybe-olang))) #t
                                       (lambda (id* dflt*)
                                         (for-all
                                           (lambda (req)
                                             (memp (lambda (x) (bound-identifier=? req x)) fml*))
                                           (list-head id* (fx- (length id*) (length dflt*)))))
                                       (lambda (dflt*)
                                         ; punting when there are return values for now
                                         (null? dflt*)))])
                                (genmap (pdesc-name callee-pdesc) level maybe? #`(#,aname #,fml)
                                  (let ([id* (cdr (pdesc-fml* callee-pdesc))]
                                        [dflt* (pdesc-dflt* callee-pdesc)])
                                    (let ([n (fx- (length id*) (length dflt*))])
                                      #`(#,@(list-head id* n)
                                          #,@(map (lambda (id dflt)
                                                    (if (memp (lambda (x) (bound-identifier=? id x))
                                                          (cdr fml*))
                                                        id
                                                        dflt))
                                               (list-tail id* n)
                                               dflt*))))))
                              (let ([callee-pdesc
                                      (find-proc pass-desc pass-options (pdesc-name pdesc)
                                        (syntax->datum (spec-type (find-spec fname ilang)))
                                        (syntax->datum (spec-type (find-spec ofname maybe-olang))) #f
                                        (lambda (id* dflt*)
                                          (for-all
                                            (lambda (req)
                                              (memp (lambda (x) (bound-identifier=? req x)) fml*))
                                            (list-head id* (fx- (length id*) (length dflt*)))))
                                        (lambda (dflt*)
                                          ; punting when there are return values for now
                                          (null? dflt*)))])
                                (if callee-pdesc
                                    (genmap (pdesc-name callee-pdesc) level maybe? #`(#,aname #,fml)
                                      (let ([id* (cdr (pdesc-fml* callee-pdesc))]
                                             [dflt* (pdesc-dflt* callee-pdesc)])
                                        (let ([n (fx- (length id*) (length dflt*))])
                                          #`(#,@(list-head id* n)
                                              #,@(map (lambda (id dflt)
                                                        (if (memp (lambda (x) (bound-identifier=? id x))
                                                              (cdr fml*))
                                                            id
                                                            dflt))
                                                   (list-tail id* n)
                                                   dflt*)))))
                                    (begin
                                      (when (or (nonterminal-meta? fname intspec*)
                                                (nonterminal-meta? ofname maybe-ontspec*))
                                        (syntax-violation who
                                          (format "unable to automatically translate ~s in ~s to ~s in ~s"
                                            (syntax->datum fname) (syntax->datum (alt-syn in-altrec))
                                            (syntax->datum ofname) (syntax->datum (alt-syn out-altrec)))
                                          (pass-desc-name pass-desc) (pdesc-name pdesc)))
                                      #`(#,aname #,fml)))))))
                      (cond
                        [(pair-alt? in-altrec)
                         (let* ([in-field-level* (pair-alt-field-levels in-altrec)]
                                [in-field-maybe* (pair-alt-field-maybes in-altrec)]
                                [in-acc* (pair-alt-accessors in-altrec)]
                                [in-field-name* (pair-alt-field-names in-altrec)]
                                [out-field-name* (pair-alt-field-names out-altrec)]
                                [out-field*
                                 (map process-alt-field
                                      in-field-level*
                                      in-field-maybe*
                                      in-field-name*
                                      in-acc*
                                      out-field-name*)])
                           ; always using the non-checking form here, because we are simply rebuilding;
                           ; TODO: terminals should be checked to be matching from the input language
                           ; to the output language, otherwise a check should be made here or the
                           ; checking version of the maker should be used.
                           ; AWK: this has been changed to use the checking alt, because we cannot
                           ; assume that other transformers will always create a valid element for
                           ; sub-parts of this particular maker.
                           ; TODO: Need to find a way to give a better error message in the checking maker
                           #`(#,(pair-alt-maker out-altrec)
                               '#,(pass-desc-name pass-desc)
                               #,@out-field*
                               #,@(map (lambda (x) (format "~s" x)) (syntax->datum in-field-name*))))]
                        [(terminal-alt? in-altrec) (error who "unexpected terminal alt" in-altrec)]
                        [(nonterminal-alt? in-altrec) (error who "unexpected nonterminal alt" in-altrec)])))
                  (cond
                    [(nonterminal-alt? alt)
                     (build-subtype-call (syntax->datum (ntspec-name (nonterminal-alt-ntspec alt))))]
                    [(terminal-alt? alt)
                     (let ([proc (find-proc pass-desc pass-options (pdesc-name pdesc)
                                   (syntax->datum (tspec-type (terminal-alt-tspec alt)))
                                   maybe-otype #f
                                   (lambda (id* dflt*) (fx<? (fx- (length id*) (length dflt*)) (length fml*)))
                                   (lambda (dflt*) (fx=? (length dflt*) (length (pdesc-xval* pdesc)))))]
                           [xval* (pdesc-xval* pdesc)])
                       (let ([alt-code (if proc (build-call fml* proc) fml)])
                         (if (null? xval*)
                             alt-code
                             #`(values #,alt-code #,@xval*))))]
                    [else
                     (let ([alt-syntax (alt-syn alt)])
                       (let ([oalt (exists-alt? alt (nonterm-id->ntspec who maybe-otype maybe-ontspec*))])
                         (if oalt
                             (let ([alt-code (process-alt alt-syntax alt oalt)]
                                    [xval* (pdesc-xval* pdesc)])
                               (if (null? xval*)
                                   alt-code
                                   #`(values #,alt-code #,@xval*)))
                             ; TODO: if there were no user provided clauses for this input alt,
                             ; we could raise a compile time error here, otherwise we have to rely
                             ; on the runtime error
                             #`(error '#,(pass-desc-name pass-desc)
                                 (format "no matching clause for input ~s in processor ~s"
                                   '#,alt-syntax
                                   '#,(pdesc-name pdesc))
                                 #,fml))))])))

              (define gen-binding (lambda (t v) (if (eq? t v) '() (list #`(#,t #,v)))))
              (define gen-t (lambda (acc) (if (identifier? acc) acc (gentemp))))
              (define gen-let1
                (lambda (t v e)
                  (cond [(eq? t v) e]
                    [(eq? e #t) #t]
                    [else #`(let ([#,t #,v]) #,e)])))
              ;; Note: gen-and DOES NOT actually function like and. For instance,
              ;; normally (and exp #t) would return #t, but with gen-and we get exp
              ;; so if exp does not evaluate to #t, the result is different.
              ;; This is used in the generated results.
              (define gen-and
                (lambda (e1 e2)
                  (cond [(eq? e1 #t) e2] [(eq? e2 #t) e1] [else #`(and #,e1 #,e2)])))
              (define gen-for-all
                (lambda (t v e)
                  (if (eq? e #t) #t #`(for-all (lambda (#,t) #,e) #,v))))

              ; TODO: Right now process-nano-fields and its helpers are generating a predicate
              ; on incoming records, and two bindings for each user specified unquote expression.
              ; I think the infrastructure should be assuming that the input is well structured
              ; (i.e. it should rely on the builder of the structure to do the checking and not
              ; check on input, and hence should not generate the temporary bindings, or the
              ; checks.)
              (define process-nano-fields
                (lambda (elt* acc-id aname* itype*)
                  (if (null? elt*)
                      (values #t '() '() '())
                      (let-values
                        ([(elt-ipred elt-tbinding* elt-ibinding* elt-obinding*)
                          (process-nano-elt (car elt*) #`(#,(car aname*) #,acc-id)
                            (car itype*))]
                         [(rest-ipred rest-tbinding* rest-ibinding* rest-obinding*)
                          (process-nano-fields (cdr elt*) acc-id (cdr aname*)
                            (cdr itype*))])
                        (values
                          (gen-and elt-ipred rest-ipred)
                          (append elt-tbinding* rest-tbinding*)
                          (append elt-ibinding* rest-ibinding*)
                          (append elt-obinding* rest-obinding*))))))

              (define gen-mvmap
                (lambda (who ids proc arg . args)
                  (with-syntax ([who who] [proc proc] [arg arg])
                    (with-syntax ([(arg* ...) args]
                                  [(ls2 ...) (generate-temporaries args)]
                                  [(id ...) (generate-temporaries ids)]
                                  [(id* ...) (generate-temporaries ids)])
                      (with-syntax ([(ls ...) #'(ls1 ls2 ...)])
                        #'(let ([p proc] [ls1 arg] [ls2 arg*] ...)
                            (unless (list? ls) (error 'who "not a proper list" ls))
                            ...
                            (let ([n (length ls1)])
                              (unless (and (= (length ls2) n) ...)
                                (error 'who "mismatched list lengths" ls1 ls2 ...)))
                            (let f ([ls1 ls1] [ls2 ls2] ...)
                              (if (null? ls1)
                                  (let ([id '()] ...) (values id ...))
                                  (let-values ([(id ...) (p (car ls1) (car ls2) ...)]
                                                [(id* ...) (f (cdr ls1) (cdr ls2) ...)])
                                    (values (cons id id*) ...))))))))))

              (define process-nano-dots
                (lambda (elt acc itype)
                  (let ([map-t (gentemp)])
                    (let-values ([(ipred tbinding* ibinding* obinding*)
                                  (process-nano-elt elt map-t itype)])
                      (let ([ls-t (gen-t acc)])
                        (values
                          (gen-for-all map-t acc ipred)
                          (gen-binding ls-t acc)
                          (map
                            (lambda (ibinding)
                              (syntax-case ibinding ()
                                [(id expr)
                                 (if (and (identifier? #'expr) (eq? map-t #'expr))
                                     #`(id #,ls-t)
                                     #`(id (map (lambda (#,map-t)
                                                  #,(if (null? tbinding*) 
                                                        #'expr
                                                        #`(let* #,tbinding* expr)))
                                                #,ls-t)))]))
                            ibinding*)
                          (map
                            (lambda (obinding)
                              ;; TODO: rather than tearing apart the code we've constructed
                              ;; in the nano-cata case to support dotted cata, the nano-cata
                              ;; should be constructed to just build the correct code in the first
                              ;; place.
                              (syntax-case obinding ()
                                [(ids (procexpr var args ...)) ;; contains expr itself
                                 #`(ids ((let ([p (let ([p procexpr]) (lambda (m) (p m args ...)))])
                                           (lambda (x)
                                             #,(cond
                                                 [(null? #'ids) #'(begin (for-each p x) (values))]
                                                 [(null? (cdr #'ids)) #'(map p x)]
                                                 [else (gen-mvmap (pass-desc-name pass-desc)
                                                                  #'ids #'p #'x)])))
                                         var))]))
                            obinding*)))))))

              (define process-nano-list
                (lambda (elt* acc itype)
                  (define helper
                    (lambda (elt* tail-acc)
                      (if (null? elt*)
                          (values #t '() '() '() 0 #f)
                          (let ([elt (car elt*)])
                            (if (nano-dots? elt)
                                (let ([t (gen-t tail-acc)] [n (length (cdr elt*))])
                                  (let-values
                                    ([(elt-ipred elt-tbinding* elt-ibinding* elt-obinding*)
                                      (process-nano-dots (nano-dots-x elt)
                                        (if (fx=? n 0)
                                            t
                                            #`(list-head #,t (fx- (length #,t) #,n)))
                                        itype)]
                                     [(rest-ipred rest-tbinding* rest-ibinding*
                                       rest-obinding* i dots?)
                                      (helper (cdr elt*)
                                        (if (fx=? n 0)
                                            t
                                            #`(list-tail #,t (fx- (length #,t) #,n))))])
                                    (values
                                      (gen-let1 t tail-acc
                                        (gen-and elt-ipred rest-ipred))
                                      (append (gen-binding t tail-acc)
                                        elt-tbinding* rest-tbinding*)
                                      (append elt-ibinding* rest-ibinding*)
                                      (append elt-obinding* rest-obinding*)
                                      i #t)))
                                (let ([t (gen-t tail-acc)])
                                  (let-values
                                    ([(elt-ipred elt-tbinding* elt-ibinding* elt-obinding*)
                                      (process-nano-elt elt #`(car #,t) itype)]
                                     [(rest-ipred rest-tbinding* rest-ibinding*
                                       rest-obinding* i dots?)
                                      (helper (cdr elt*) #`(cdr #,t))])
                                    (values
                                      (gen-let1 t tail-acc
                                        (gen-and elt-ipred rest-ipred))
                                      (append (gen-binding t tail-acc)
                                        elt-tbinding* rest-tbinding*)
                                      (append elt-ibinding* rest-ibinding*)
                                      (append elt-obinding* rest-obinding*)
                                      (fx+ i 1) dots?))))))))
                  (let ([t (gen-t acc)])
                    (let-values ([(ipred tbinding* ibinding* obinding* i dots?)
                                  (helper elt* t)])
                      (values
                        (gen-let1 t acc
                          (if dots?
                              (if (fx=? i 0)
                                  ipred
                                  (gen-and #`(fx>=? (length #,t) #,i) ipred))
                              (gen-and #`(fx=? (length #,t) #,i) ipred)))
                        (append (gen-binding t acc) tbinding*)
                        ibinding* obinding*)))))

              (define build-meta-variable-check
                (lambda (id acc itype)
                  (let ([spec (find-spec id ilang)])
                    ;; SYMBOLIC
                    (cond
                      [(eq? (syntax->datum (spec-type spec)) (syntax->datum itype)) #t]
                      [(nonterm-id->ntspec? itype (language-ntspecs ilang)) =>
                       (lambda (ntspec)
                         (if (subspec? spec ntspec)
                             #`(#,(spec-all-pred spec) #,acc)
                             (syntax-violation
                               (syntax->datum (pass-desc-name pass-desc))
                               (format
                                 "expected meta-variable for nonterminal ~s, but got"
                                 (syntax->datum itype))
                               id)))]
                      [(term-id->tspec? itype (language-tspecs ilang)) =>
                       (lambda (tspec)
                         (syntax-violation
                           (syntax->datum (pass-desc-name pass-desc))
                           (format
                             "expected meta-variable for terminal ~s, but got"
                             (syntax->datum itype))
                           id))]
                      [else (syntax-violation
                              (syntax->datum (pass-desc-name pass-desc))
                              (format
                                "NANOPASS INTERNAL ERROR: unable to find spec for type ~s"
                                (syntax->datum itype))
                              id)]))))

              (define process-nano-elt
                (lambda (elt acc itype)
                  (cond
                    [(nano-meta? elt)
                     (let ([t (gen-t acc)])
                       (let-values ([(ipred tbinding* ibinding* obinding*)
                                     (process-nano-meta elt t)])
                         (values
                           (gen-let1 t acc
                             (gen-and
                               ;; TODO: if the nt here doesn't have any terminals, then we only
                               ;; need to do the tag comparison.
                               #;#`(eqv? (nanopass-record-tag #,t) #,(pair-alt-tag (nano-meta-alt elt)))
                               #`(#,(pair-alt-pred (nano-meta-alt elt)) #,t)
                               ipred))
                           (append (gen-binding t acc) tbinding*)
                           ibinding* obinding*)))]
                    [(nano-quote? elt) 
                     (syntax-violation (syntax->datum (pass-desc-name pass-desc))
                       "quoted items are currently unsupported in patterns"
                       (nano-quote-x elt))]
                    [(nano-unquote? elt)
                     ; TODO: will break if two ids are same
                     (let ([id (nano-unquote-x elt)])
                       (values
                         (build-meta-variable-check id acc itype)
                         '()
                         (list #`(#,id #,acc))
                         '()))]
                    [(nano-cata? elt)
                     ; TODO: will break if two ids are same
                     ; HERE: if this is a cata for a (maybe x) field, it needs to not bother
                     ; parsing the #f
                     (let* ([maybe-inid* (nano-cata-maybe-inid* elt)]
                            [t (or (and maybe-inid* (car maybe-inid*)) (gentemp))]
                            [maybe? (nano-cata-maybe? elt)]
                            [itype (syntax->datum itype)])
                       (let-values ([(maybe-otype outid*)
                                     (let ([outid* (nano-cata-outid* elt)])
                                       (if maybe-olang
                                           (if (null? outid*)
                                               (values #f outid*)
                                               (if (eq? (syntax->datum (car outid*)) '*)
                                                   (values #f (cdr outid*))
                                                   (values
                                                     (syntax->datum
                                                       (spec-type
                                                         (find-spec (car outid*) maybe-olang)))
                                                     outid*)))
                                           (values #f outid*)))])
                         (define build-cata-call-1
                           (lambda (itype maybe-otype inid* outid*)
                             (build-call inid*
                               (find-proc pass-desc pass-options (nano-cata-syntax elt) itype maybe-otype #t
                                 (lambda (id* dflt*)
                                   (fx<? (fx- (length id*) (length dflt*)) (length inid*)))
                                 (lambda (dflt*)
                                   (fx=? (length dflt*)
                                     (length (if maybe-otype (cdr outid*) outid*)))))
                               maybe?)))
                         ; TODO: check pdesc-maybe-itype >= itype and pdesc-otype <= otype
                         (define pdesc-ok?
                           (lambda (pdesc outid*)
                             (and (for-all
                                    (lambda (req) (memp (lambda (x) (bound-identifier=? req x)) fml*))
                                    (list-head xfml* (fx- (length xfml*) (length (pdesc-dflt* pdesc)))))
                                  (fx=? (length (pdesc-xval* pdesc))
                                    ; TODO: when we don't have an otype for a processor, we may not have an otype here
                                    ; we should check this out to be sure.
                                    (length (if itype (cdr outid*) outid*))))))
                         (define build-cata-call-2
                           (lambda (callee-pdesc t)
                             (let ([id* (cdr (pdesc-fml* callee-pdesc))]
                                   [dflt* (pdesc-dflt* callee-pdesc)])
                               (with-syntax ([(earg* ...)
                                              (let* ([n (fx- (length id*) (length dflt*))])
                                                #`(#,@(list-head id* n)
                                                    #,@(map (lambda (id dflt)
                                                              (if (memp (lambda (x) (bound-identifier=? id x))
                                                                    fml*)
                                                                  id
                                                                  dflt))
                                                         (list-tail id* n)
                                                         dflt*)))])
                                 (if maybe?
                                     (with-syntax ([(t* ...) (generate-temporaries #'(earg* ...))])
                                       #`((lambda (#,t t* ...)
                                            (and #,t (#,(pdesc-name callee-pdesc) #,t t* ...)))
                                          #,t earg* ...))

                                     #`(#,(pdesc-name callee-pdesc) #,t earg* ...))))))
                         (define build-cata-call-3
                           (lambda (itype maybe-otype t outid*)
                             (let ([callee-pdesc
                                    (find-proc pass-desc pass-options (nano-cata-syntax elt) itype maybe-otype #t
                                      (lambda (id* dflt*)
                                        (for-all
                                          (lambda (req)
                                            (memp (lambda (x) (bound-identifier=? req x)) fml*))
                                          (list-head id* (fx- (length id*) (length dflt*)))))
                                      (lambda (dflt*)
                                        (fx=? (length dflt*)
                                          (let ([len (length outid*)])
                                            (if maybe-otype (fx- len 1) len)))))])
                               (let ([id* (cdr (pdesc-fml* callee-pdesc))]
                                     [dflt* (pdesc-dflt* callee-pdesc)])
                                 (with-syntax ([(earg* ...)
                                                (let ([n (fx- (length id*) (length dflt*))])
                                                  #`(#,@(list-head id* n)
                                                      #,@(map (lambda (id dflt)
                                                                (if (memp (lambda (x) (bound-identifier=? id x))
                                                                      fml*)
                                                                    id dflt))
                                                           (list-tail id* n)
                                                           dflt*)))])
                                   (if maybe?
                                       (with-syntax ([(t* ...) (generate-temporaries #'(earg* ...))])
                                         #`((lambda (#,t t* ...)
                                              (and #,t (#,(pdesc-name callee-pdesc) #,t t* ...)))
                                            #,t earg* ...))
                                       #`(#,(pdesc-name callee-pdesc) #,t earg* ...)))))))
                         ; check number of arguments when we have a maybe
                         (when (and maybe? (not (fx=? (length outid*) 1)))
                           (syntax-violation who
                             "cannot use cata-morphism that returns multiple values with a maybe field"
                             (nano-cata-syntax elt)))
                         (let ([procexpr (nano-cata-procexpr elt)])
                           (define build-procexpr-call
                             (lambda ()
                               (let ([inid* (or maybe-inid* (list t))])
                                 (if maybe?
                                     (with-syntax ([(t t* ...) (generate-temporaries inid*)])
                                       #`((lambda (t t* ...) (and t (#,procexpr t t* ...))) #,@inid*))
                                     #`(#,procexpr #,@inid*)))))
                           #;(unless procexpr
                             (unless (nonterm-id->ntspec? itype (language-ntspecs ilang))
                               (syntax-violation who
                                 "cannot use cata-morphism without specifying a procedure to call for an input terminal field"
                                 (nano-cata-syntax elt))))
                           #;(when maybe-otype
                             (unless (or procexpr (nonterm-id->ntspec? maybe-otype (language-ntspecs maybe-olang)))
                               (syntax-violation who
                                 "cannot use cata-morphism without specifying a procedure to call for an output terminal field"
                                 (nano-cata-syntax elt))))
                           ; when we are not given a processor, make sure our itype is valid
                           (values
                             ; input predicate check
                             (if maybe-inid*
                                 (build-meta-variable-check (car maybe-inid*)
                                   acc (nano-cata-itype elt))
                                 #t)
                             ; binding of temporaries
                             '()
                             ; binding of input variable from language record
                             (list #`(#,t #,acc))
                             ; binding of output variable(s)
                             (if maybe-inid*
                                 (if procexpr
                                     (list #`[#,outid* #,(build-procexpr-call)])
                                     (list #`[#,outid* #,(build-cata-call-1 itype maybe-otype maybe-inid* outid*)]))
                                 (cond
                                   [(and (identifier? procexpr)
                                         (find (lambda (pdesc)
                                                 (bound-identifier=? procexpr (pdesc-name pdesc)))
                                           (pass-desc-pdesc* pass-desc))) =>
                                     (lambda (callee-pdesc)
                                       (if (pdesc-ok? callee-pdesc outid*)
                                           (list #`[#,outid* #,(build-cata-call-2 callee-pdesc t)])
                                           (syntax-violation (syntax->datum (pass-desc-name pass-desc))
                                             (format "incorrect arguments for ~s in cata" (syntax->datum procexpr))
                                             (nano-cata-syntax elt))))]
                                   [procexpr (list #`[#,outid* #,(build-procexpr-call)])]
                                   [else (list #`[#,outid* #,(build-cata-call-3 itype maybe-otype t outid*)])]))))))]
                    [(list? elt) (process-nano-list elt acc itype)]
                    [else (values #`(equal? #,acc #,elt) '() '() '())])))

              (define-who process-nano-meta
                (lambda (x acc-id)
                  (let ([prec-alt (nano-meta-alt x)])
                    (if (pair-alt? prec-alt)
                        (process-nano-fields (nano-meta-fields x) acc-id
                          (pair-alt-accessors prec-alt)
                          (map (lambda (x) (spec-type (find-spec x ilang)))
                            (pair-alt-field-names prec-alt)))
                        (let ([elt (car (nano-meta-fields x))])
                         ; TODO: we'd like to more generally support cata for terminal and nonterminal-alt and
                         ; this code will have to change to support that.
                          (assert (nano-unquote? elt))
                          (let ([id (nano-unquote-x elt)])
                            (values #t '() (list #`(#,id #,acc-id)) '())))))))

              (define find-eq-constraints
                (lambda (ibinding*)
                  (let f ([ibinding* ibinding*] [id* '()])
                    (if (null? ibinding*)
                        (values '() #t)
                        (let* ([ibinding (car ibinding*)] [id (car ibinding)])
                          (if (bound-id-member? id id*)
                              (syntax-violation who "eq constraints are not supported" id)
                              #;(let-values ([(ibinding* ieqpred)
                                              (f (cdr ibinding*) id*)])
                                  (let ([t (gentemp)])
                                    (values
                                      #`((#,t #,(cadr ibinding)) #,@ibinding*)
                                      (gen-and #`(nano-equal? #,t #,id) ieqpred))))
                              (let-values ([(ibinding* ieqpred)
                                            (f (cdr ibinding*) (cons id id*))])
                                (values #`(#,ibinding #,@ibinding*) ieqpred))))))))

              (define make-user-clause
                (lambda (pclause k)
                  (let ([lhs-rec (pclause-lhs pclause)]
                        [guard-code (pclause-guard pclause)]
                        [rhs-id (pclause-id pclause)]
                        [rhs-arg* (pclause-rhs-arg* pclause)])
                    (let-values ([(ipred tbinding* ibinding* obinding*)
                                  (process-nano-meta lhs-rec fml)])
                      (let-values ([(ibinding* ieqpred)
                                    (find-eq-constraints ibinding*)])
                        (let ([guard-code (gen-and guard-code ieqpred)]
                              [body-code #`(let-values #,obinding* (#,rhs-id #,@rhs-arg*))])
                          (if (eq? ipred #t)
                              #`(let* (#,@tbinding* #,@ibinding*)
                                  #,(if (eq? guard-code #t)
                                        body-code
                                        #`(if #,guard-code #,body-code #,(k))))
                              (if (eq? guard-code #t)
                                  #`(if #,ipred
                                        (let* (#,@tbinding* #,@ibinding*)
                                          #,body-code)
                                        #,(k))
                                  #`(let ([next-th (lambda () #,(k))])
                                      (if #,ipred
                                          (let* (#,@tbinding* #,@ibinding*)
                                            (if #,guard-code #,body-code (next-th)))
                                          (next-th)))))))))))

              (define generate-system-clauses
                (lambda (alt*)
                  ; NB: don't use variants here to see how that impacts performance for testing purposes.
                  #;(let f ([alt* alt*] [rcond-cl* '()])
                    (if (null? alt*)
                        (reverse rcond-cl*)
                        (let* ([alt (car alt*)] [alt (if (pair? alt) (car alt) alt)])
                          (f (cdr alt*)
                             (cons 
                               #`[(#,(cond
                                       [(pair-alt? alt) (pair-alt-pred alt)]
                                       [(terminal-alt? alt) (tspec-pred (terminal-alt-tspec alt))]
                                       [else (ntspec-all-pred (nonterminal-alt-ntspec alt))])
                                    #,fml)
                                   #,(make-clause alt '() #f)]
                               rcond-cl*)))))
                  (let f ([alt* alt*] [rcond-rec-cl* '()] [rcond-case-cl* '()])
                    (if (null? alt*)
                        (values (reverse rcond-rec-cl*) (reverse rcond-case-cl*))
                        (let* ([alt (car alt*)] [alt (if (pair? alt) (car alt) alt)])
                          (with-syntax ([body (make-clause alt '() #f)])
                            (cond
                              [(pair-alt? alt)
                               (f (cdr alt*) rcond-rec-cl*
                                  (cons #`[(eqv? tag #,(pair-alt-tag alt)) body] rcond-case-cl*))]
                              [(terminal-alt? alt)
                               (let* ([tspec (terminal-alt-tspec alt)] [ttag (tspec-tag tspec)])
                                 (if ttag
                                     (f (cdr alt*) rcond-rec-cl*
                                        (cons 
                                          (if (tspec-parent? tspec)
                                              #`[(not (fxzero? (fxand tag #,ttag))) body]
                                              #`[(eqv? tag #,ttag) body])
                                          rcond-case-cl*))
                                     (f (cdr alt*)
                                        (cons
                                          #`[(#,(tspec-pred (terminal-alt-tspec alt)) #,fml) body]
                                          rcond-rec-cl*)
                                        rcond-case-cl*)))]
                              [else
                               (let ([ntspec (nonterminal-alt-ntspec alt)])
                                 (let ([maybe-term-pred? (ntspec-all-term-pred ntspec)])
                                   (f (cdr alt*)
                                      (if maybe-term-pred?
                                          (cons #`[(#,maybe-term-pred? #,fml) body] rcond-rec-cl*)
                                          rcond-rec-cl*)
                                      (with-syntax ([(all-tag ...) (ntspec-all-tag ntspec)])
                                        (cons #`[(let ([t (fxand tag #,(language-tag-mask ilang))]) (or (fx=? t all-tag) ...)) body] rcond-case-cl*)))))])))))))

             (define build-subtype-call
               (lambda (itype)
                 (build-call fml*
                   (find-proc pass-desc pass-options (pdesc-name pdesc) itype maybe-otype #t
                     (lambda (id* dflt*) (fx<? (fx- (length id*) (length dflt*)) (length fml*)))
                     (lambda (dflt*) (fx=? (length dflt*) (length (pdesc-xval* pdesc))))))))

              (define make-clause
                (lambda (alt pclause* else-id)
                  (let f ([pclause* pclause*])
                    (if (null? pclause*)
                        (cond
                          [else-id #`(#,else-id)]
                          ; TODO: Consider dropping the (not maybe-olang) and
                          ; building the subtype call even if there is no otype
                          ; for this.  (Need to make sure build-subtype-call
                          ; can handle this appropriately (possibly also need
                          ; to decide if a user-supplied sub-type call with an
                          ; output type is okay to call).)
                          [(and (or (and maybe-olang maybe-otype) (not maybe-olang)) (nonterminal-alt? alt))
                           (build-subtype-call (syntax->datum (ntspec-name (nonterminal-alt-ntspec alt))))]
                          [(and maybe-olang maybe-otype)
                           (make-system-clause alt)]
                          [else
                           (syntax-violation (syntax->datum (pass-desc-name pass-desc))
                             (format "missing ~s clause cannot be generated with no output type"
                               (syntax->datum (alt-syn alt)))
                             (pdesc-name pdesc))])
                        (let ([pclause (car pclause*)] [pclause* (cdr pclause*)])
                          (pclause-used-set! pclause #t)
                          (make-user-clause pclause (lambda () (f pclause*)))))))))

            (define maybe-add-lambdas
              (lambda (pclause* else-id else-body body)
                (with-syntax ([((id* rhs-body*) ...)
                               (fold-left (lambda (ls pclause)
                                            (if (pclause-used? pclause)
                                                (cons (list (pclause-id pclause)
                                                            (pclause-rhs-lambda pclause))
                                                      ls)
                                                ls))
                                 (if else-id
                                     (list (list else-id else-body))
                                     '())
                                 pclause*)])
                  #`(let ([id* rhs-body*] ...) #,body))))
            ; note: assumes grammar nonterminal clauses form a DAG
            ; TODO: reject grammars that have nonterminal clauses that don't form DAG
            ; TODO: should we build this structure up front? also is there a better DS for us
            ; to figure out how the various pclauses are interrelated while we process them
            (define-record-type nt-alt-info
              (fields alt (mutable up*) (mutable down*))
              (nongenerative)
              (protocol
                (lambda (new)
                  (lambda (alt)
                    (new alt '() '())))))

            (define build-ntspec-ht
              (lambda (ntspec)
                (let ([ht (make-eq-hashtable)])
                  (define set-cons (lambda (item ls) (if (memq item ls) ls (cons item ls))))
                  (define set-append
                    (lambda (ls1 ls2)
                      (cond
                        [(null? ls1) ls2]
                        [(null? ls2) ls1]
                        [else (fold-left (lambda (ls item) (set-cons item ls)) ls2 ls1)])))
                  (define discover-nt-alt-info!
                    (lambda (alt up*)
                      (let ([nt-alt-info (or (eq-hashtable-ref ht alt #f)
                                             (let ([nt-alt-info (make-nt-alt-info alt)])
                                               (eq-hashtable-set! ht alt nt-alt-info)
                                               nt-alt-info))])
                        (nt-alt-info-up*-set! nt-alt-info
                          (set-append up* (nt-alt-info-up* nt-alt-info)))
                        (let ([up* (cons alt up*)])
                          (let ([down* (fold-left
                                         (lambda (down* alt)
                                           (set-append (discover-nt-alt-info! alt up*) down*))
                                         (nt-alt-info-down* nt-alt-info)
                                         (filter nonterminal-alt? (ntspec-alts (nonterminal-alt-ntspec alt))))])
                            (nt-alt-info-down*-set! nt-alt-info down*)
                            (cons alt down*))))))
                  (for-each (lambda (alt) (discover-nt-alt-info! alt '()))
                    (filter nonterminal-alt? (ntspec-alts ntspec)))
                  ht)))
            (define build-alt-tree
              (lambda (ntspec)
                (let f ([alt* (ntspec-alts ntspec)] [ralt* '()])
                  (if (null? alt*)
                      (reverse ralt*)
                      (f (cdr alt*)
                        (cons
                          (let ([alt (car alt*)])
                            (if (nonterminal-alt? alt)
                                (cons alt (f (ntspec-alts (nonterminal-alt-ntspec alt)) '()))
                                alt))
                          ralt*))))))
            (define alt-tree->s-expr
              (lambda (tree)
                (let f ([alt* tree])
                  (if (null? alt*)
                      '()
                      (let ([alt (car alt*)])
                        (if (pair? alt)
                            (cons (f alt) (f (cdr alt*)))
                            (cons (syntax->datum (alt-syn alt)) (f (cdr alt*)))))))))
            (define remove-alt
              (lambda (covered-alt alt*)
                (let f ([alt* alt*])
                  (if (null? alt*)
                      '()
                      (let ([alt (car alt*)] [alt* (cdr alt*)])
                        (if (pair? alt)
                            (if (eq? (car alt) covered-alt)
                                alt*
                                (let ([calt* (f (cdr alt))])
                                  (if (null? calt*)
                                      alt*
                                      (cons (cons (car alt) calt*) (f alt*)))))
                            (if (eq? alt covered-alt)
                                alt*
                                (cons alt (f alt*)))))))))
            (define handle-pclause*
              (lambda (pclause* else-id alt-tree ht)
                (define partition-pclause*
                  (lambda (alt pclause pclause*)
                    (if (nonterminal-alt? alt)
                        (let* ([nt-alt-info (eq-hashtable-ref ht alt #f)]
                               [this-and-down* (cons alt (nt-alt-info-down* nt-alt-info))]
                               [up* (nt-alt-info-up* nt-alt-info)])
                          (let-values ([(matching-pclause* other-pclause*)
                                        (partition (lambda (pclause)
                                                     (memq (nano-meta-alt (pclause-lhs pclause)) this-and-down*))
                                          pclause*)])
                            (let ([related-pclause* (filter (lambda (pclause)
                                                              (memq (nano-meta-alt (pclause-lhs pclause)) up*))
                                                      other-pclause*)])
                              (values (cons pclause (append matching-pclause* related-pclause*)) other-pclause*))))
                        (let-values ([(matching-pclause* other-pclause*)
                                      (partition (lambda (pclause) (eq? (nano-meta-alt (pclause-lhs pclause)) alt))
                                        pclause*)])
                          (let ([related-pclause* (filter
                                                    (let ([nt-alt* (pclause-related-alt* pclause)])
                                                      (lambda (pclause)
                                                        (memq (nano-meta-alt (pclause-lhs pclause)) nt-alt*)))
                                                    pclause*)])
                            (values (cons pclause (append matching-pclause* related-pclause*)) other-pclause*))))))
                #;(let f ([pclause* pclause*] [alt-tree alt-tree] [rcond-cl* '()])
                  (if (null? pclause*)
                      (values (reverse rcond-cl*) alt-tree)
                      (let* ([pclause (car pclause*)] [alt (nano-meta-alt (pclause-lhs pclause))])
                        (let-values ([(related-pclause* other-pclause*)
                                      (partition-pclause* alt pclause (cdr pclause*))])
                          (f other-pclause*
                             (remove-alt alt alt-tree)
                             (cons
                               #`[(#,(cond
                                       [(pair-alt? alt) (pair-alt-pred alt)]
                                       [(terminal-alt? alt) (tspec-pred (terminal-alt-tspec alt))]
                                       [else (ntspec-all-pred (nonterminal-alt-ntspec alt))])
                                    #,fml)
                                   #,(make-clause alt related-pclause* else-id)]
                               rcond-cl*))))))
                (let f ([pclause* pclause*] [alt-tree alt-tree] [rcond-rec-cl* '()] [rcond-case-cl* '()])
                  (if (null? pclause*)
                      (values (reverse rcond-rec-cl*) (reverse rcond-case-cl*) alt-tree)
                      (let* ([pclause (car pclause*)] [alt (nano-meta-alt (pclause-lhs pclause))])
                        (let-values ([(related-pclause* other-pclause*)
                                      (partition-pclause* alt pclause (cdr pclause*))])
                          (with-syntax ([body (make-clause alt related-pclause* else-id)])
                            (cond
                              [(pair-alt? alt)
                               (f other-pclause* (remove-alt alt alt-tree) rcond-rec-cl*
                                  (cons #`[(eqv? tag #,(pair-alt-tag alt)) body] rcond-case-cl*))]
                              [(terminal-alt? alt)
                               (let* ([tspec (terminal-alt-tspec alt)] [ttag (tspec-tag tspec)])
                                 (if ttag
                                     (f other-pclause* (remove-alt alt alt-tree) rcond-rec-cl*
                                        (cons
                                          (if (tspec-parent? tspec)
                                              #`[(not (fxzero? (fxand tag #,ttag))) body]
                                              #`[(eqv? tag #,ttag) body])
                                          rcond-case-cl*))
                                     (f other-pclause* (remove-alt alt alt-tree)
                                       (cons #`[(#,(tspec-pred (terminal-alt-tspec alt)) #,fml) body] rcond-rec-cl*)
                                       rcond-case-cl*)))]
                              [else
                               (let ([ntspec (nonterminal-alt-ntspec alt)])
                                 (let ([maybe-term-pred? (ntspec-all-term-pred ntspec)])
                                   (f other-pclause* (remove-alt alt alt-tree)
                                      (if maybe-term-pred?
                                          (cons #`[(#,maybe-term-pred? #,fml) body] rcond-rec-cl*)
                                          rcond-rec-cl*)
                                      (with-syntax ([(all-tag ...) (ntspec-all-tag ntspec)])
                                        (cons #`[(let ([t (fxand tag #,(language-tag-mask ilang))]) (or (fx=? t all-tag) ...)) body] rcond-case-cl*)))))]))))))))
            (define annotate-pclause*!
              (lambda (pclause* ntspec ht)
                (let f ([pclause* pclause*]
                        [alt* (filter nonterminal-alt? (ntspec-alts ntspec))]
                        [curr-alt #f])
                  (if (or (null? alt*) (null? pclause*))
                      pclause*
                      (let ([alt (car alt*)])
                        (if (nonterminal-alt? alt)
                            (f (f pclause* (ntspec-alts (nonterminal-alt-ntspec alt)) alt) (cdr alt*) curr-alt)
                            (let-values ([(matching-pclause* other-pclause*)
                                          (partition (lambda (pclause)
                                                       (eq? (nano-meta-alt (pclause-lhs pclause)) alt))
                                            pclause*)])
                              (for-each
                                (lambda (pclause)
                                  (pclause-related-alt*-set! pclause
                                    (cons curr-alt (nt-alt-info-up* (eq-hashtable-ref ht curr-alt #f)))))
                                matching-pclause*)
                              (f other-pclause* (cdr alt*) curr-alt))))))))
            (let-values ([(pclause* else-id else-body) (parse-clauses cl*)])
              (let ([ntspec (nonterm-id->ntspec who itype intspec*)])
                (maybe-add-lambdas pclause* else-id else-body
                  (let ([ht (build-ntspec-ht ntspec)])
                    (annotate-pclause*! pclause* ntspec ht)
                    #;(let-values ([(user-clause* alt*)
                                  (handle-pclause* pclause* else-id
                                    (if else-id '() (build-alt-tree ntspec))
                                    ht)])
                      (let ([system-clause* (if else-id '() (generate-system-clauses alt*))])
                        #`(cond
                            #,@user-clause*
                            #,@system-clause*
                            [else #,(if else-id
                                        #`(#,else-id) 
                                        #`(error '#,(pass-desc-name pass-desc)
                                            #,(format "unexpected ~s" (syntax->datum itype))
                                            #,fml))])))
                    (let-values ([(user-rec-clause* user-case-clause* alt*)
                                  (handle-pclause* pclause* else-id
                                    (if else-id '() (build-alt-tree ntspec))
                                    ht)])
                      (let-values ([(system-rec-clause* system-case-clause*)
                                    (if else-id
                                        (values
                                          (if (ntspec-all-term-pred ntspec)
                                              #`([(not (nanopass-record? #,fml)) (#,else-id)])
                                              '())
                                          '())
                                        (generate-system-clauses alt*))])
                        #`(cond
                            #,@user-rec-clause*
                            #,@system-rec-clause*
                            [else 
                             (let ([tag (nanopass-record-tag #,fml)])
                               (cond
                                 #,@user-case-clause*
                                 #,@system-case-clause*
                                 [else #,(if else-id
                                             #`(#,else-id) 
                                             #`(error '#,(pass-desc-name pass-desc)
                                                 #,(format "unexpected ~s" (syntax->datum itype))
                                                 #,fml))]))]))))))))))

     ; build-call and find-proc need to work in concert, so they are located near eachother
     ; to increase the chance that we actually remember to alter both of them when the
     ; interface is effected by changing one.
      (define build-call
        (case-lambda
          [(caller-fml* callee-pdesc) (build-call caller-fml* callee-pdesc #f)]
          [(caller-fml* callee-pdesc maybe?)
           (define build-args
             (lambda (callee-fml* callee-init* caller-fml*)
               (let f ([required-cnt (fx- (length callee-fml*) (length callee-init*))]
                       [callee-fml* callee-fml*]
                       [callee-init* callee-init*]
                       [caller-fml* caller-fml*])
                 (cond
                   [(null? callee-fml*) '()]
                   [(and (fxzero? required-cnt) (null? caller-fml*))
                    (cons (car callee-init*)
                      (f required-cnt (cdr callee-fml*) (cdr callee-init*) caller-fml*))]
                   [(fxzero? required-cnt)
                    (cons (car caller-fml*)
                      (f required-cnt (cdr callee-fml*) (cdr callee-init*) (cdr caller-fml*)))]
                   [else (cons (car caller-fml*)
                           (f (fx- required-cnt 1) (cdr callee-fml*) callee-init*  (cdr caller-fml*)))]))))
           (with-syntax ([pname (pdesc-name callee-pdesc)]
                         [(arg* ...) (build-args (pdesc-fml* callee-pdesc) (pdesc-dflt* callee-pdesc) caller-fml*)])
             (if maybe?
                 (with-syntax ([(t t* ...) (generate-temporaries #'(arg* ...))])
                   #'((lambda (t t* ...) (and t (pname t t* ...))) arg* ...))
                 #'(pname arg* ...)))]))

      (define find-proc
        ; will never be asked to find a proc without an itype, so itype is never #f
        (lambda (pass-desc pass-options src-stx itype maybe-otype try-to-generate? xfmls-ok? xvals-ok?)
          (define (try-to-generate)
            (if (pass-options-generate-transformers? pass-options)
                (begin
                  (unless (and (xfmls-ok? '() '()) (xvals-ok? '()))
                    (syntax-violation who
                      (format "cannot find a transformer from ~s to ~s, \
                        and cannot generate one with extra formals or return values"
                        itype maybe-otype)
                      (pass-desc-name pass-desc) src-stx))
                    (unless (and (nonterm-id->ntspec? itype (language-ntspecs (pass-desc-maybe-ilang pass-desc)))
                                 (nonterm-id->ntspec? maybe-otype (language-ntspecs (pass-desc-maybe-olang pass-desc))))
                      (syntax-violation who
                        (format "cannot find a transformer from ~s to ~s, \
                          and cannot generate one when either the input or output type is a terminal"
                          itype maybe-otype)
                        (pass-desc-name pass-desc) src-stx))
                      (let ([pdesc (make-pdesc (datum->syntax #'* (gensym (format "~s->~s" itype maybe-otype)))
                                     itype (list #'ir) '() maybe-otype '() '() #f #f)])
                        (pass-desc-pdesc*-set! pass-desc
                          (cons pdesc (pass-desc-pdesc* pass-desc)))
                        pdesc))
                    (syntax-violation who
                      (format "cannot find a transformer from ~s to ~s that matches the expected signature"
                        itype maybe-otype)
                      (pass-desc-name pass-desc) src-stx)))
          (define find-subspecs
            (lambda (ospec sub-ospec*)
              (if (ntspec? ospec)
                  (let f ([alt* (ntspec-alts ospec)] [sub-ospec* sub-ospec*])
                    (if (null? alt*)
                        sub-ospec*
                        (let ([alt (car alt*)])
                          (cond
                            [(nonterminal-alt? alt)
                             (f (cdr alt*) (cons (nonterminal-alt-ntspec alt) sub-ospec*))]
                            [(terminal-alt? alt)
                             (f (cdr alt*) (cons (terminal-alt-tspec alt) sub-ospec*))]
                            [else (f (cdr alt*) sub-ospec*)]))))
                  sub-ospec*)))
          (define find-candidate
            (lambda (maybe-otype)
              (let loop ([pdesc* (pass-desc-pdesc* pass-desc)] [candidate #f])
                (if (null? pdesc*)
                    candidate
                    (loop (cdr pdesc*)
                      (let ([pdesc (car pdesc*)])
                        (if (and (eq? (pdesc-maybe-itype pdesc) itype) ; HERE
                                 (eq? (pdesc-maybe-otype pdesc) maybe-otype) ; HERE
                                 (xfmls-ok? (cdr (pdesc-fml* pdesc)) (pdesc-dflt* pdesc))
                                 (xvals-ok? (pdesc-xval* pdesc)))
                            (if candidate
                                (syntax-violation who
                                  (format "ambiguous target for implicit processor call from ~s to ~s"
                                    itype maybe-otype)
                                  (pass-desc-name pass-desc) src-stx)
                                pdesc)
                            candidate)))))))
          (when (identifier? maybe-otype)
            (syntax-violation 'find-proc "expected symbol otype, got identifier" maybe-otype))
         ; doing a breadth-first search of maybe-otype and its subtypes
         ; could go up to parent itype(s) on itype as well
          #;(printf "entering with itype ~s to otype ~s in ~s\n" itype maybe-otype
            (map (lambda (x) (list (syntax->datum (pdesc-name x)) ': (pdesc-maybe-itype x) '-> (pdesc-maybe-otype x)))
              (pass-desc-pdesc* pass-desc)))
          (if maybe-otype
              (let ospec-loop ([ospec* (list (id->spec maybe-otype (pass-desc-maybe-olang pass-desc)))]
                               [sub-ospec* '()])
                (if (null? ospec*)
                    (if (null? sub-ospec*)
                        (and try-to-generate? (try-to-generate))
                        (ospec-loop sub-ospec* '()))
                    (or (find-candidate (syntax->datum (spec-type (car ospec*))))
                        (ospec-loop (cdr ospec*) (find-subspecs (car ospec*) sub-ospec*)))))
              (or (find-candidate #f)
                  (syntax-violation who
                    (format "cannot find a processor that accepts input type ~s and no output type" itype)
                    (pass-desc-name pass-desc) src-stx)))))

        (define parse-proc
          (lambda (pass-name ilang olang)
            (lambda (x)
              (let loop ([x x] [trace? #f] [echo? #f])
                (syntax-case x ()
                  [(?echo ?not-colon . rest)
                   (and (eq? (datum ?echo) 'echo) (not (eq? (datum ?not-colon) ':)))
                   (loop #'(?not-colon . rest) trace? #t)]                  
                  [(?trace ?not-colon . rest)
                   (and (eq? (datum ?trace) 'trace) (not (eq? (datum ?not-colon) ':)))
                   (loop #'(?not-colon . rest) #t echo?)]
                  [(proc-name ?colon itype (arg ...) ?arrow otype (rv ...) body ...)
                   (let ([squawk (lambda (msg what) (syntax-violation (syntax->datum pass-name) msg what))])
                     (unless (identifier? #'proc-name) (squawk "invalid processor name" #'proc-name))
                     (unless (eq? (datum ?colon) ':) (squawk "expected colon" #'?colon))
                     (let ([maybe-itype
                            (syntax-case #'itype ()
                              [* (eq? (datum *) '*) #f]
                              [id
                               (identifier? #'id)
                               (if ilang
                                   (if (or (nonterm-id->ntspec? #'id (language-ntspecs ilang))
                                           (term-id->tspec? #'id (language-tspecs ilang)))
                                       (syntax->datum #'id)
                                       (squawk "unrecognized input non-terminal" #'id))
                                   (squawk "specified input non-terminal without input language" #'id))]
                              [_ (squawk "invalid input type specifier" #'itype)])])
                       (let ([arg* #'(arg ...)])
                         (when maybe-itype
                           (when (null? arg*) (squawk "expected non-empty argument list" arg*))
                           (unless (identifier? (car arg*)) (squawk "invalid first argument" (car arg*))))
                         (let-values ([(fml* init*)
                                       (let f ([arg* arg*] [dflt? #f])
                                         (if (null? arg*)
                                             (values '() '())
                                             (syntax-case (car arg*) ()
                                               [id
                                                (identifier? #'id)
                                                (if dflt?
                                                    (squawk "unexpected non-default formal after start of default formals" #'id)
                                                    (let-values ([(fml* init*) (f (cdr arg*) #f)])
                                                      (values (cons #'id fml*) init*)))]
                                               [[id expr]
                                                (identifier? #'id)
                                                (let-values ([(fml* init*) (f (cdr arg*) #t)])
                                                  (values (cons #'id fml*) (cons #'expr init*)))]
                                               [arg (squawk "invalid argument specifier" #'arg)])))])
                           (unless (eq? (datum ?arrow) '->) (squawk "expected arrow" #'?arrow))
                           (let ([maybe-otype (syntax-case #'otype ()
                                                [* (eq? (datum *) '*) #f]
                                                [id
                                                 (identifier? #'id)
                                                 (if olang
                                                     (if (or (nonterm-id->ntspec? #'id (language-ntspecs olang))
                                                             (term-id->tspec? #'id (language-tspecs olang)))
                                                         (syntax->datum #'id)
                                                         (squawk "unrecognized output non-terminal" #'id))
                                                     (squawk "specified output non-terminal without output language" #'id))]
                                                [_ (squawk "invalid output-type specifier" #'otype)])])
                             (make-pdesc #'proc-name maybe-itype fml* init*
                               maybe-otype #'(rv ...) #'(body ...) trace? echo?))))))])))))

      (define lookup-lang
        (lambda (pass-name r maybe-name)
          (if maybe-name
              (let* ([olang-pair (r maybe-name)]
                     [lang (and olang-pair (car olang-pair))]
                     [meta-parser (and olang-pair (cdr olang-pair))])
                (unless (language? lang)
                  (syntax-violation (syntax->datum pass-name) "unrecognized language" maybe-name))
                (unless (procedure? meta-parser)
                  (syntax-violation (syntax->datum pass-name) "missing meta parser for language" maybe-name))
                (values lang meta-parser))
              (values #f #f))))

      (define build-checked-body
        (lambda (pass-desc pass-options maybe-fml xval* maybe-itype maybe-otype maybe-ometa-parser maybe-body)
          (define generate-output-check
            (lambda (type x ntspec*)
              ((lambda (ls) (if (null? (cdr ls)) (car ls) #`(or #,@ls)))
               (let f ([ntspec (nonterm-id->ntspec who type ntspec*)] [test* '()])
                   (cons #`(#,(ntspec-all-pred ntspec) #,x)
                     (fold-left
                       (lambda (test* alt)
                         (if (nonterminal-alt? alt)
                             (f (nonterminal-alt-ntspec alt) test*)
                             test*))
                   test* (ntspec-alts ntspec)))))))
          (define generate-body
            (lambda (maybe-olang maybe-otype)
              (cond
                [(and maybe-body maybe-otype)
                 (rhs-in-context-quasiquote (pass-desc-name pass-desc) maybe-otype
                   maybe-olang maybe-ometa-parser maybe-body)]
                [maybe-body]
                [else
                 (unless (null? xval*)
                   (syntax-violation who "cannot auto-generate body for pass with extra return values"
                     (pass-desc-name pass-desc)))
                 (let ([ilang (pass-desc-maybe-ilang pass-desc)])
                   (unless ilang
                     (syntax-violation who "cannot auto-generate body without input language"
                       (pass-desc-name pass-desc)))
                   (let ([itype (or maybe-itype (syntax->datum (language-entry-ntspec ilang)))])
                     (let ([pdesc (find-proc pass-desc pass-options (pass-desc-name pass-desc) itype maybe-otype #t
                                    (lambda (id* dflt*) (fx=? (length dflt*) (length id*)))
                                    (lambda (dflt*) (fxzero? (length dflt*))))])
                       (let ([init* (pdesc-dflt* pdesc)] [rv* (pdesc-xval* pdesc)])
                         (if (null? rv*)
                             #`(#,(pdesc-name pdesc) #,maybe-fml #,@init*)
                             #`(let-values ([(result #,@(map (lambda (x) (gensym "rv")) rv*))
                                             (#,(pdesc-name pdesc) #,maybe-fml #,@init*)])
                                 result))))))])))
          (let ([olang (pass-desc-maybe-olang pass-desc)])
            (if olang
                (let ([otype (or maybe-otype (syntax->datum (language-entry-ntspec olang)))])
                  (with-syntax ([checked-body
                                 #`(unless #,(generate-output-check otype #'x (language-ntspecs olang))
                                     (error '#,(pass-desc-name pass-desc)
                                       (format "expected ~s but got ~s" '#,(datum->syntax #'* otype) x)))])
                    (if (null? xval*)
                        #`(let ([x #,(generate-body olang otype)])
                            checked-body
                            x)
                        (with-syntax ([(res* ...) (generate-temporaries xval*)])
                          #`(let-values ([(x res* ...) #,(generate-body olang otype)])
                              checked-body
                              (values x res* ...))))))
                (generate-body #f #f)))))

      (define do-define-pass
        (lambda (pass-name pass-options maybe-iname maybe-itype fml* maybe-oname maybe-otype xval* defn* p* maybe-body)
          (define echo-pass
            (lambda (x)
              (when (pass-options-echo? pass-options)
                (printf "pass ~s expanded into:\n" (syntax->datum pass-name))
                (pretty-print (syntax->datum x))
                (newline))
              x))
         (with-compile-time-environment (r)
            #;(unless (and maybe-iname (not (null? fml*)))
              (syntax-violation who "can't yet handle \"*\" iname" pass-name))
            (let-values ([(maybe-ilang maybe-imeta-parser) (lookup-lang pass-name r maybe-iname)]
                         [(maybe-olang maybe-ometa-parser) (lookup-lang pass-name r maybe-oname)])
              (when (and maybe-itype (not (nonterm-id->ntspec? maybe-itype (language-ntspecs maybe-ilang))))
                (syntax-violation who "unrecognized pass input non-terminal" pass-name maybe-itype))
              (when (and maybe-otype (not (nonterm-id->ntspec? maybe-otype (language-ntspecs maybe-olang))))
                (syntax-violation who "unrecognized pass output non-terminal" pass-name maybe-otype))
              (let* ([pdesc* (map (parse-proc pass-name maybe-ilang maybe-olang) p*)]
                     [pass-desc (make-pass-desc pass-name maybe-ilang maybe-olang pdesc*)]
                     [body (build-checked-body pass-desc pass-options (and (pair? fml*) (car fml*))
                             xval* (syntax->datum maybe-itype)
                             (syntax->datum maybe-otype) maybe-ometa-parser maybe-body)])
                (echo-pass
                  (with-syntax ([who (datum->syntax pass-name 'who)])
                    #`(define #,pass-name
                        (lambda #,fml*
                          (define who '#,pass-name)
                          (define-nanopass-record)
                          #,@defn*
                          #,@(make-processors pass-desc pass-options maybe-imeta-parser maybe-ometa-parser)
                          #,body)))))))))

      (syntax-case x ()
        [(_ pass-name ?colon iname (fml ...) ?arrow oname (xval ...) stuff ...)
         (let ([squawk (lambda (msg what) (syntax-violation who msg x what))])
           (unless (identifier? #'pass-name) (squawk "invalid pass name" #'pass-name))
           (unless (eq? (datum ?colon) ':) (squawk "expected colon" #'?colon))
           (let-values ([(maybe-iname maybe-itype)
                         (syntax-case #'iname ()
                           [* (eq? (datum *) '*) (values #f #f)]
                           [iname (identifier? #'iname) (values #'iname #f)]
                           [(iname itype)
                            (and (identifier? #'iname) (identifier? #'itype))
                            (values #'iname #'itype)]
                           [_ (squawk "invalid input language specifier" #'iname)])])
             (let ([fml* #'(fml ...)])
               (unless (for-all identifier? fml*) (squawk "expected list of identifiers" fml*))
               (when (and maybe-iname (null? fml*)) (squawk "expected non-empty list of formals" fml*))
               (unless (eq? (datum ?arrow) '->) (squawk "expected arrow" #'?arrow))
               (let-values ([(maybe-oname maybe-otype)
                             (syntax-case #'oname ()
                               [* (eq? (datum *) '*) (values #f #f)]
                               [id (identifier? #'id) (values #'id #f)]
                               [(oname otype)
                                (and (identifier? #'oname) (identifier? #'otype))
                                (values #'oname #'otype)]
                               [_ (squawk "invalid output-language specifier" #'oname)])])
                 (define (s1 stuff* defn* processor* pass-options)
                   (if (null? stuff*)
                       (s2 defn* processor* #f pass-options)
                       (let ([stuff (car stuff*)])
                         (if (let processor? ([stuff stuff] [mcount 0])                        
                               (syntax-case stuff ()
                                 [(pname ?colon itype (fml ...) ?arrow otype (xval ...) . more)
                                  (and  (eq? (datum ?colon) ':)
                                        (eq? (datum ?arrow) '->)
                                        (identifier? #'itype)
                                        (identifier? #'otype)
                                        (for-all (lambda (fml)
                                                   (or (identifier? fml)
                                                       (syntax-case fml ()
                                                         [[fml exp-val] (identifier? #'fml)])))
                                          #'(fml ...))
                                        #t)]
                                 [(?modifier ?not-colon . more)
                                  (and (memq (datum ?modifier) '(trace echo))
                                       (not (eq? (datum ?not-colon) ':))
                                       (< mcount 2))
                                  (processor? #'(?not-colon . more) (fx+ mcount 1))]
                                 [_ #f]))
                             (s1 (cdr stuff*) defn* (cons stuff processor*) pass-options)
                             (s2 defn* processor* #`(begin #,@stuff*) pass-options)))))
                 (define (s2 defn* processor* maybe-body pass-options)
                   (do-define-pass #'pass-name pass-options maybe-iname maybe-itype fml*
                     maybe-oname maybe-otype #'(xval ...) defn* (reverse processor*) maybe-body))
                 (let s0 ([stuff* #'(stuff ...)] [defn* '()] [pass-options #f])
                   (if (null? stuff*)
                       (s1 stuff* defn* '() (or pass-options (make-pass-options)))
                       (syntax-case (car stuff*) ()
                         [(definitions defn ...)
                          (eq? (datum definitions) 'definitions)
                          (s0 (cdr stuff*) #'(defn ...) pass-options)]
                         [(?pass-options . ?options)
                          (eq? (datum ?pass-options) 'pass-options)
                          (s0 (cdr stuff*) defn* (make-pass-options #'?options))]
                         [_ (s1 stuff* defn* '() (or pass-options (make-pass-options)))])))))))]
        [(_ . rest) (syntax-violation who "invalid syntax" #'(define-pass . rest))]))))
