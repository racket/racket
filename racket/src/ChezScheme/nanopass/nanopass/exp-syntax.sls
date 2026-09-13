(library (nanopass exp-syntax)
  (export
    define-language-exp
    inspect-language lookup-language
    Llanguage unparse-Llanguage
    Lannotated unparse-Lannotated
    language->s-expression-exp
    prune-language-exp
    define-pruned-language-exp
    diff-languages-exp
    define-language-node-counter-exp
    define-unparser-exp
    define-parser-exp
    )
  (import (rnrs) (nanopass) (nanopass experimental) (nanopass helpers)
    (only (chezscheme) make-compile-time-value trace-define-syntax unbox
      optimize-level enumerate with-output-to-string errorf))


  (define-syntax define-language-exp
    (lambda (x)
      (lambda (rho)
        (syntax-case x ()
          [(_ . rest)
           (let* ([lang (parse-np-source x 'define-language-exp)]
                  [lang (handle-language-extension lang 'define-language-exp rho)]
                  [lang (check-and-finish-language lang)]
                  [lang-annotated (annotate-language lang)])
             (nanopass-case (Llanguage Defn) lang
               [(define-language ,id ,cl* ...)
                #`(begin
                    (define-language . rest)
                    (define-property #,id experimental-language
                      (make-language-information '#,lang '#,lang-annotated))
                    (define-language-records #,id)
                    #;(define-language-predicates #,id))]))]))))

  (define-syntax inspect-language
    (lambda (x)
      (lambda (rho)
        (syntax-case x ()
          [(_ name)
           (let ([lang (rho #'name)])
             (if lang
                 (let ([l (language-information-language lang)]
                       [a (language-information-annotated-language lang)])
                   #`(list
                       '#,l
                       '#,(datum->syntax #'* (unparse-Llanguage l))
                       '#,a
                       '#,(datum->syntax #'* (unparse-Lannotated a))))
                 (syntax-violation 'inspect-language "no language found" #'name)))]))))

  (define (build-list-of-string level name)
    (with-output-to-string
      (lambda ()
        (let loop! ([level level])
          (if (fx=? level 0)
              (write name)
              (begin (display "list of ") (loop! (fx- level 1))))))))

  (define-syntax define-language-records
    (lambda (x)
      (define-pass construct-records : Lannotated (ir) -> * (stx)
        (definitions
          (define (build-field-check name mv level pred)
            #`(lambda (x msg)
                (define (squawk level x)
                  (if msg
                      (errorf who "expected ~a but received ~s in field ~s from ~a"
                        (build-list-of-string level '#,name) x '#,mv msg)
                      (errorf who "expected ~a but received ~s in field ~s"
                        (build-list-of-string level '#,name) x '#,mv)))
                #,(let f ([level level])
                    (if (fx=? level 0)
                        #`(lambda (x) (unless (#,pred x) (squawk #,level x)))
                        #`(lambda (x)
                            (let loop ([x x])
                              (cond
                                [(pair? x) (#,(f (fx- level 1)) (car x))]
                                [(null? x)]
                                [else (squawk #,level x)]))))))))
        (Defn : Defn (ir) -> * (stx)
          [(define-language ,id ,ref ,id0? ,rtd ,rcd ,tag-mask (,term* ...) ,[nt*] ...)
           #`(begin #,@nt*)])
        (Nonterminal : Nonterminal (ir) -> * (stx)
          [(,id (,id* ...) ,b ,rtd ,rcd ,tag ,pred ,all-pred ,all-term-pred ,prod* ...)
           (let ([stx* (map (lambda (prod) (Production prod rcd)) prod*)])
             #`(begin (define #,pred (record-predicate '#,rtd)) #,@stx*))])
        (Production : Production (ir nt-rcd) -> * (stx)
          [(production ,pattern ,pretty-prod? ,rtd ,tag ,pred ,maker ,[mv* acc* check*] ...)
           (with-syntax ([(mv* ...) mv*]
                         [(msg* ...) (generate-temporaries mv*)]
                         [(check* ...) check*]
                         [(acc* ...) acc*]
                         [(idx ...) (enumerate acc*)])
             #`(begin
                 (define #,maker
                   (let ()
                     (define maker
                       (record-constructor
                         (make-record-constructor-descriptor '#,rtd '#,nt-rcd
                           (lambda (pargs->new)
                             (lambda (mv* ...)
                               ((pargs->new #,tag) mv* ...))))))
                     (lambda (who mv* ... msg* ...)
                       #,@(if (fx=? (optimize-level) 3)
                              '()
                              #`((check* mv* msg*) ...))
                       (maker mv* ...))))
                 (define #,pred (record-predicate '#,rtd))
                 (define acc* (record-accessor '#,rtd idx)) ...))]
          [else #'(begin)])
        (Field : Field (ir) -> * (mv check acc)
          [(,[mv name pred] ,level ,accessor)
           (values mv accessor (build-field-check name mv level pred))]
          [(optional ,[mv name pred] ,level ,accessor)
           (values mv accessor 
             (build-field-check name mv level
               #`(lambda (x) (or (eq? x #f) (#,pred x)))))])
        (Reference : Reference (ir) -> * (mv name pred)
          [(term-ref ,id0 ,id1 ,b)
           (values id0 id1 (TerminalPred (unbox b)))]
          [(nt-ref ,id0 ,id1 ,b)
           (values id0 id1 (NonterminalPred (unbox b)))])
        (TerminalPred : Terminal (ir) -> * (name pred)
          [(,id (,id* ...) ,b ,handler? ,pred) pred])
        (NonterminalPred : Nonterminal (ir) -> * (name pred)
          [(,id (,id* ...) ,b ,rtd ,rcd ,tag ,pred ,all-pred ,all-term-pred ,prod* ...)
           all-pred])
        (Defn ir))
      (syntax-case x ()
        [(_ name)
         (lambda (rho)
           (let ([lang (lookup-language rho #'name)])
             (construct-records (language-information-annotated-language lang))))])))

  (define-syntax define-language-predicates
    (lambda (x)
      (define-pass language-predicates : Lannotated (ir) -> * (stx)
        (definitions
          (define (set-cons x ls)
            (if (memq x ls)
                ls
                (cons x ls))))
        (Defn : Defn (ir) -> * (stx)
          [(define-language ,id ,ref ,id0? ,rtd ,rcd ,tag-mask (,term* ...) ,nt* ...)
           (let loop ([nt* nt*] [ntpreddef* '()] [tpred* '()])
             (if (null? nt*)
                 (with-syntax ([pred (construct-id id id "?")]
                               [(tpred* ...) tpred*])
                   #`(begin
                       (define pred
                         (lambda (x)
                           (or ((record-predicate '#,rtd) x) (tpred* x) ...)))
                       #,@ntpreddef*))
                 (let-values ([(ntpreddef* tpred*) (Nonterminal (car nt*) ntpreddef* tpred*)])
                   (loop (cdr nt*) ntpreddef* tpred*))))])
        (Nonterminal : Nonterminal (nt ntpreddef* lang-tpred*) -> * (ntpreddef* lang-tpred*)
          [(,id (,id* ...) ,b ,rtd ,rcd ,tag ,pred ,all-pred ,all-term-pred ,prod* ...)
           (let loop ([prod* prod*] [pred* '()] [lang-tpred* lang-tpred*])
             (if (null? prod*)
                 (values
                   (cons
                     (with-syntax ([(pred* ...) pred*])
                       #`(define #,all-pred
                           (lambda (x)
                             (or ((record-predicate '#,rtd) x) (pred* x) ...))))
                     ntpreddef*)
                   lang-tpred*)
                 (let-values ([(tpred* lang-tpred*) (Production (car prod*) pred* lang-tpred*)])
                   (loop (cdr prod*) tpred* lang-tpred*))))])
        (Production : Production (ir pred* lang-tpred*) -> * (pred* lang-tpred*)
          [(terminal (term-ref ,id0 ,id1 ,b) ,pretty-prod?)
           (let ([pred (TerminalPred (unbox b))])
             (values (cons pred pred*) (set-cons pred lang-tpred*)))]
          [(nonterminal (nt-ref ,id0 ,id1 ,b) ,pretty-prod?)
           (values (cons (NonterminalPred (unbox b)) pred*) lang-tpred*)]
          [else (values pred* lang-tpred*)])
        (TerminalPred : Terminal (ir) -> * (pred)
          [(,id (,id* ...) ,b ,handler? ,pred) pred])
        (NonterminalPred : Nonterminal (ir) -> * (pred)
          [(,id (,id* ...) ,b ,rtd ,rcd ,tag ,pred ,all-pred ,all-term-pred ,prod* ...) all-pred])
        (Defn ir))
      (syntax-case x ()
        [(_ name)
         (lambda (rho)
           (let ([lang (lookup-language rho #'name)])
             (language-predicates (language-information-annotated-language lang))))])))

  (define-syntax language->s-expression-exp
    (lambda (x)
      (define-pass lang->sexp : Llanguage (ir) -> * (sexp)
        (Defn : Defn (ir) -> * (sexp)
          [(define-language ,id ,[cl*] ...)
           `(define-language ,(syntax->datum id) . ,cl*)])
        (Clause : Clause (ir) -> * (sexp)
          [(entry ,[sym]) `(entry ,sym)]
          [(nongenerative-id ,id)
           `(nongenerative-id ,(syntax->datum id))]
          [(terminals ,[term*] ...)
           `(terminals . ,term*)]
          [(,id (,id* ...) ,b ,[prod*] ...)
           `(,(syntax->datum id) ,(map syntax->datum id*) . ,prod*)])
        (Terminal : Terminal (ir) -> * (sexp)
          [,simple-term (SimpleTerminal simple-term)]
          [(=> ,[simple-term] ,handler)
           `(=> ,simple-term ,(syntax->datum handler))])
        (SimpleTerminal : SimpleTerminal (ir) -> * (sexp)
          [(,id (,id* ...) ,b)
           `(,(syntax->datum id) ,(map syntax->datum id*))])
        (Production : Production (ir) -> * (sexp)
          [,pattern (Pattern pattern)]
          [(=> ,[pattern0] ,[pattern1])
           `(=> ,pattern0 ,pattern1)]
          [(-> ,[pattern] ,handler)
           `(-> ,pattern ,(syntax->datum handler))])
        (Pattern : Pattern (ir) -> * (sexp)
          [(maybe ,[sym]) `(maybe ,sym)]
          [,ref (Reference ref)]
          [,id (syntax->datum id)]
          [(,[pattern0] ,dots . ,[pattern1])
           `(,pattern0 ... . ,pattern1)]
          [(,[pattern0] . ,[pattern1])
           `(,pattern0 . ,pattern1)]
          [,null '()])
        (Reference : Reference (ir) -> * (sym)
          [(term-ref ,id0 ,id1 ,b) (syntax->datum id0)]
          [(nt-ref ,id0 ,id1 ,b) (syntax->datum id0)])
        (Defn ir))
      (syntax-case x ()
        [(_ name)
         (lambda (rho)
           (let ([lang (lookup-language rho #'name)])
             #`'#,(datum->syntax #'*
                    (lang->sexp
                      (language-information-language lang)))))])))

  (define-syntax prune-language-exp
    (lambda (x)
      (syntax-case x ()
        [(_ name)
         (lambda (rho)
           (let ([lang (lookup-language rho #'name)])
             (with-syntax ([pl (prune-lang
                                 (language-information-annotated-language lang)
                                 'prune-language-exp
                                 #f)])
               #'(quote pl))))])))

  (define-syntax define-pruned-language-exp
    (lambda (x)
      (syntax-case x ()
        [(_ name new-name)
         (lambda (rho)
           (let ([lang (lookup-language rho #'name)])
             (prune-lang
               (language-information-annotated-language lang)
               'define-pruned-language-exp
               #'new-name)))])))

  (define-syntax diff-languages-exp
    (lambda (x)
      (syntax-case x ()
        [(_ name0 name1)
         (lambda (rho)
           (let ([lang0 (lookup-language rho #'name0)]
                 [lang1 (lookup-language rho #'name1)])
             (with-syntax ([diff (diff-langs
                                   (language-information-language lang0)
                                   (language-information-language lang1))])
               #'(quote diff))))])))

  (define-syntax define-language-node-counter-exp
    (lambda (x)
      (syntax-case x ()
        [(_ name lang)
         (lambda (rho)
           (let ([l (lookup-language rho #'lang)])
             (build-lang-node-counter (language-information-annotated-language l) #'name)))])))

  (define-syntax define-unparser-exp
    (lambda (x)
      (syntax-case x ()
        [(_ name lang)
         (lambda (rho)
           (let ([l (lookup-language rho #'lang)])
             (build-unparser (language-information-annotated-language l) #'name)))])))

  (define-syntax define-parser-exp
    (lambda (x)
      (syntax-case x ()
        [(_ name lang)
         (lambda (rho)
           (let ([l (lookup-language rho #'lang)])
             (build-parser (language-information-annotated-language l) #'name)))])))
  )
