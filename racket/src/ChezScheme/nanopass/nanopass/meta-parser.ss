;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (nanopass meta-parser)
  (export make-meta-parser rhs-in-context-quasiquote meta-parse-term
    make-quasiquote-transformer make-in-context-transformer
    output-records->syntax parse-cata)
  (import (rnrs)
          (nanopass helpers)
          (nanopass records)
          (nanopass syntaxconvert)
          (nanopass meta-syntax-dispatch))

  (define make-ntspec-meta-parser-assoc
    (lambda (tid)
      (lambda (ntspec)
        (cons ntspec (construct-unique-id tid "meta-parse-" (ntspec-name ntspec))))))

  (define make-meta-parser
    (lambda (desc)
      (let* ([lang-name (language-name desc)]
             [ntspecs (language-ntspecs desc)]
             [tspecs (language-tspecs desc)]
             [ntspec-meta-parsers (map (make-ntspec-meta-parser-assoc lang-name) ntspecs)])
        (define lookup-meta-parser
          (lambda (ntspec)
            (cond
              [(assq ntspec ntspec-meta-parsers) => cdr]
              [else (syntax-violation 'define-language
                      (format "unexpected nonterminal ~s in langauge ~s while building meta-parser, expected on of ~s"
                        (syntax->datum (ntspec-name ntspec)) (syntax->datum lang-name)
                        (map (lambda (nt) (syntax->datum (ntspec-name nt))) ntspecs))
                      lang-name)])))
        (define make-meta-parse-proc
          (lambda (ntspec cata?)
            (define parse-field
              (lambda (m level maybe?)
                (cond
                  [(meta-name->tspec m tspecs) =>
                   (lambda (name)
                     (let f ([level level] [x m])
                       (if (= level 0)
                           #`(meta-parse-term '#,name #,x #,cata? #,maybe?)
                           #`(map (lambda (x)
                                    (if (nano-dots? x)
                                        (make-nano-dots #,(f (- level 1)
                                                             #'(nano-dots-x x)))
                                        #,(f (- level 1) #'x)))
                                  #,x))))]
                  [(meta-name->ntspec m ntspecs) =>
                   (lambda (spec)
                     (with-syntax ([proc-name (lookup-meta-parser spec)])
                       (let f ([level level] [x m])
                         (if (= level 0)
                             #`(proc-name #,x #t #t #,maybe?)
                             #`(map (lambda (x)
                                      (if (nano-dots? x)
                                          (make-nano-dots #,(f (- level 1)
                                                               #'(nano-dots-x x)))
                                          #,(f (- level 1) #'x)))
                                    #,x)))))]
                  [else (syntax-violation 'define-language
                          (format "unrecognized meta variable ~s in language ~s, when building meta parser" m lang-name)
                          lang-name)])))
            (define make-term-clause
              (lambda (x)
                (lambda (alt)
                  #`[(memq (meta-var->raw-meta-var (syntax->datum #,x))
                           (quote #,(tspec-meta-vars (terminal-alt-tspec alt))))
                     (make-nano-meta '#,alt (list (make-nano-unquote #,x)))])))
            (define make-nonterm-unquote
              (lambda (x)
                (lambda (alt)
                  #`[(memq (meta-var->raw-meta-var (syntax->datum #,x))
                           (quote #,(ntspec-meta-vars (nonterminal-alt-ntspec alt))))
                     (make-nano-meta '#,alt (list (make-nano-unquote #,x)))])))
            (define make-nonterm-clause
              (lambda (x maybe?)
                (lambda (alt)
                  #`(#,(lookup-meta-parser (nonterminal-alt-ntspec alt)) #,x #f nested? maybe?))))
            (define make-pair-clause
              (lambda (stx first-stx rest-stx)
                (lambda (alt)
                  (with-syntax ([(field-var ...) (pair-alt-field-names alt)])
                    (with-syntax ([(parsed-field ...)
                                   (map parse-field #'(field-var ...)
                                        (pair-alt-field-levels alt)
                                        (pair-alt-field-maybes alt))]
                                  [field-pats (datum->syntax #'* (pair-alt-pattern alt))])
                      #`[#,(if (pair-alt-implicit? alt)
                               #`(meta-syntax-dispatch #,stx 'field-pats)
                               #`(and (eq? (syntax->datum #,first-stx) '#,(car (alt-syn alt)))
                                      (meta-syntax-dispatch #,rest-stx 'field-pats)))
                         => (lambda (ls)
                              (apply
                                (lambda (field-var ...)
                                  (make-nano-meta '#,alt (list parsed-field ...)))
                                ls))])))))
            (define separate-syn
              (lambda (ls)
                (let loop ([ls ls] [pair* '()] [pair-imp* '()] [term* '()] [imp* '()] [nonimp* '()])
                  (if (null? ls)
                      (values (reverse pair*) (reverse pair-imp*) (reverse term*) (reverse imp*) (reverse nonimp*))
                      (let ([v (car ls)])
                        (cond
                          [(nonterminal-alt? v)
                           (if (has-implicit-alt? (nonterminal-alt-ntspec v))
                               (loop (cdr ls) pair* pair-imp* term* (cons v imp*) nonimp*)
                               (loop (cdr ls) pair* pair-imp* term* imp* (cons v nonimp*)))]
                          [(terminal-alt? v) (loop (cdr ls) pair* pair-imp* (cons v term*) imp* nonimp*)]
                          [(pair-alt-implicit? v) (loop (cdr ls) pair* (cons v pair-imp*) term* imp* nonimp*)]
                          [else (loop (cdr ls) (cons v pair*) pair-imp* term* imp* nonimp*)]))))))
            (let-values ([(pair-alt* pair-imp-alt* term-alt* nonterm-imp-alt* nonterm-nonimp-alt*)
                          (separate-syn (ntspec-alts ntspec))])
              #`(lambda (stx error? nested? maybe?)
                  (or (syntax-case stx (unquote)
                        [(unquote id)
                         (identifier? #'id)
                         (if nested?
                             (make-nano-unquote #'id)
                             (cond
                               #,@(map (make-term-clause #'#'id) term-alt*)
                               ; TODO: right now we can match the meta for this item, but we
                               ; cannot generate the needed nano-meta because we have no
                               ; alt record to put into it.  (perhaps the current model is
                               ; just pushed as far as it can be right now, and we need to
                               ; rework it.)
                               #,@(map (make-nonterm-unquote #'#'id) nonterm-imp-alt*)
                               #,@(map (make-nonterm-unquote #'#'id) nonterm-nonimp-alt*)
                               [else #f]))]
                        [(unquote x)
                         (if nested?
                             (if #,cata?
                                 (parse-cata #'x '#,(ntspec-name ntspec) maybe?)
                                 (make-nano-unquote #'x))
                             (syntax-violation #f "cata unsupported at top-level of pattern" stx))]
                        [_ #f])
                      #,@(map (make-nonterm-clause #'stx #'maybe?) nonterm-nonimp-alt*)
                      (syntax-case stx ()
                        [(a . d)
                         (cond
                           #,@(map (make-pair-clause #'stx #'#'a #'#'d) pair-alt*)
                           #,@(map (make-pair-clause #'stx #'#'a #'#'d) pair-imp-alt*)
                           [else #f])]
                        ; if we find something here that is not a pair, assume it should
                        ; be treated as a quoted constant, and will be checked appropriately
                        ; by the run-time constructor check
                        [atom (make-nano-quote #''atom)])
                      #,@(map (make-nonterm-clause #'stx #'maybe?) nonterm-imp-alt*)
                      (and error? (syntax-violation who "unrecognized pattern or template" stx)))))))
        (with-syntax ([cata? (gentemp)])
          (with-syntax ([(ntspec-id ...) (map ntspec-name ntspecs)]
                        [(parse-name ...) (map cdr ntspec-meta-parsers)]
                        [(parse-proc ...)
                         (map (lambda (ntspec) (make-meta-parse-proc ntspec #'cata?)) ntspecs)])
            #`(lambda (ntspec-name stx input?)
                (let ([cata? input?])
                  (define-who parse-name parse-proc) ...
                  (case ntspec-name
                    [(ntspec-id) (parse-name stx #t (not input?) #f)] ...
                    [else (syntax-violation '#,(construct-id lang-name "meta-parse-" lang-name)
                            (format "unexpected nonterminal ~s passed to meta parser for language ~s while meta-parsing, expected one of ~s"
                              ntspec-name '#,lang-name '#,(map ntspec-name ntspecs))
                            stx)]))))))))

  ;; used to handle output of meta-parsers
  (define meta-parse-term
    (lambda (tname stx cata? maybe?)
      (syntax-case stx (unquote)
        [(unquote x)
         (if (and cata? (not (identifier? #'x)))
             (parse-cata #'x (tspec-type tname) maybe?)
             (make-nano-unquote #'x))]
        [(a . d)
         (syntax-violation 'meta-parse-term "invalid pattern or template" stx)]
        [stx
         ; treat everything else we find as ,'foo because if we don't
         ; `(primapp void) is interpreted as:
         ; `(primapp #<procedure void>)
         ; instead we want it to treat it as:
         ; `(primapp ,'void)
         ; which is how it would have to be written without this.
         ; Note that we don't care what literal expression we find here
         ; because at runtime it will be checked like every other element
         ; used to construct the output record, and anything invalid will
         ; be caught then. (If we check earlier, then it forces us to use
         ; the terminal predicates at compile-time, which means that can't
         ; be in the same library, and that is a bummer for other reasons,
         ; so better to be flexible and let something invalid go through
         ; here to be caught later.)
         (make-nano-quote #''stx)])))

  ;; used in the input meta parser to parse cata syntax
  ;; TODO: support for multiple input terms.
  (define parse-cata
    ; should be more picky if nonterminal is specified--see 10/08/2007 NOTES
    (lambda (x itype maybe?)
      (define (serror) (syntax-violation 'define-pass "invalid cata syntax" x))
      (define (s0 stuff)
        (syntax-case stuff ()
          [(: . stuff) (colon? #':) (s2 #f #'stuff)]
          [(-> . stuff) (arrow? #'->) (s4 #f #f '() #'stuff)]
          [(e . stuff) (s1 #'e #'stuff)]
          [() (make-nano-cata itype x #f #f '() maybe?)]
          [_ (serror)]))
      (define (s1 e stuff)
        (syntax-case stuff ()
          [(: . stuff) (colon? #':) (s2 e #'stuff)]
          [(-> . stuff)
           (and (arrow? #'->) (identifier? e))
           (s4 #f (list e) '() #'stuff)]
          [(expr . stuff)
          ; it is pre-mature to check for identifier here since these could be input exprs
           #;(and (identifier? #'id) (identifier? e))
           (identifier? e)
           (s3 #f (list #'expr e) #'stuff)]
          [() (identifier? e) (make-nano-cata itype x #f #f (list e) maybe?)]
          [_ (serror)]))
      (define (s2 f stuff)
        (syntax-case stuff ()
          [(-> . stuff)
           (arrow? #'->)
           (s4 f #f '() #'stuff)]
          [(id . stuff)
           (identifier? #'id)
           (s3 f (list #'id) #'stuff)]
          [_ (serror)]))
      (define (s3 f e* stuff)
        (syntax-case stuff ()
          [(-> . stuff)
           (arrow? #'->)
           (s4 f (reverse e*) '() #'stuff)]
          [(e . stuff)
           ; this check is premature, since these could be input expressions
           #;(identifier? #'id)
           (s3 f (cons #'e e*) #'stuff)]
          [()
           ; now we want to check if these are identifiers, because they are our return ids
           (for-all identifier? e*)
           (make-nano-cata itype x f #f (reverse e*) maybe?)]
          [_ (serror)]))
      (define (s4 f maybe-inid* routid* stuff)
        (syntax-case stuff ()
          [(id . stuff)
           (identifier? #'id)
           (s4 f maybe-inid* (cons #'id routid*) #'stuff)]
          [() (make-nano-cata itype x f maybe-inid* (reverse routid*) maybe?)]
          [_ (serror)]))
      (syntax-case x ()
        [(stuff ...) (s0 #'(stuff ...))])))

  ;; used in the output of the input metaparser and in the output of
  ;; define-pass
  (define rhs-in-context-quasiquote
    (lambda (id type omrec ometa-parser body)
      (if type
          (with-syntax ([quasiquote (datum->syntax id 'quasiquote)]
                        [in-context (datum->syntax id 'in-context)])
            #`(let-syntax ([quasiquote
                            '#,(make-quasiquote-transformer id type omrec ometa-parser)]
                           [in-context
                            '#,(make-in-context-transformer id omrec ometa-parser)])
                #,body))
          (with-syntax ([in-context (datum->syntax id 'in-context)])
            #`(let-syntax ([in-context
                             '#,(make-in-context-transformer id omrec ometa-parser)])
                #,body)))))

  ;; Done to do allow a programmer to specify what the context for
  ;; their quasiquote is, incase it is different from the current
  ;; expression.
  ;; bug fix #8 (not sure what this refers to)
  (define make-in-context-transformer
    (lambda (pass-name omrec ometa-parser)
      (lambda (x)
        (syntax-case x ()
          [(_ ntname stuff ...)
           (with-syntax ([quasiquote (datum->syntax pass-name 'quasiquote)])
             #`(let-syntax ([quasiquote '#,(make-quasiquote-transformer
                                             pass-name #'ntname
                                             omrec ometa-parser)])
                 stuff ...))]))))

  ;; Used to make quasiquote transformers in the in-context transformer
  ;; and in the normal right hand side transformer in do-define-pass and
  ;; make-rhs
  (define make-quasiquote-transformer
    (lambda (pass-name ntname omrec ometa-parser)
      (lambda (x)
        (syntax-case x ()
          [(_ stuff)
           ; TODO move error message like this into wherever the template doesn't match is
           (output-records->syntax pass-name ntname omrec ometa-parser
             (ometa-parser (syntax->datum ntname) #'stuff #f))
           #;(let ([stx #f])
             (trace-let quasiquote-transformer ([t (syntax->datum #'stuff)])
               (let ([t (output-records->syntax pass-name ntname omrec ometa-parser
                          (ometa-parser (syntax->datum ntname) #'stuff #f))])
                 (set! stx t)
                 (syntax->datum t)))
             stx)]))))

  ;; helper function used by the output metaparser in the meta-parsing
  ;; two step
  ;; TODO:
  ;; - defeated (for now) at getting rid of the unnecessary bindings.  still convinced this is possible and to be fixed.
  ;; - we are using bound-id-union to append lists of variables that are unique by construction (unless I am misreading the code) this is pointless.
  ;; - we are mapping over the field-names to find the specs for the fields. this seems waistful in a small way (building an unnecessary list) and a big way (lookup something that could be cached)
  ;; - we are always building the checking version of the pair-alt constructor here, but could probably be avoiding that.
  (define output-records->syntax
    (lambda (pass-name ntname omrec ometa-parser rhs-rec)
      (define id->msg
        (lambda (id)
          (cond
            [(fx=? (optimize-level) 3) #f]
            [(syntax->source-info id) =>
             (lambda (si) (format "expression ~s ~a" (syntax->datum id) si))]
            [else (format "expression ~s" (syntax->datum id))])))
      (define process-nano-fields
        (lambda (elt* spec* binding*)
          (if (null? elt*)
              (values '() '() '() binding*)
              (let-values ([(elt elt-id elt-var* binding*) (process-nano-elt (car elt*) (car spec*) binding*)])
                (let-values ([(elt* elt*-id elt*-var* binding*)
                              (process-nano-fields (cdr elt*) (cdr spec*) binding*)])
                  (values (cons elt elt*) (cons elt-id elt*-id) (bound-id-union elt-var* elt*-var*) binding*))))))
      (define process-nano-dots
        (lambda (orig-elt spec binding*)
          ; ought to check that each of var* are bound to proper lists
          ; and that they have the same lengths
          (let-values ([(elt id var* binding*) (process-nano-elt (nano-dots-x orig-elt) spec binding*)])
            (if (null? var*)
               ; TODO: store original syntax object in nano-dots record and use it here
                (syntax-violation (syntax->datum pass-name)
                  "no variables within ellipsis pattern"
                  (let f ([elt (nano-dots-x orig-elt)])
                    (cond
                      [(nano-meta? elt) (map f (nano-meta-fields elt))]
                      [(nano-quote? elt) (cadr (nano-quote-x elt))]
                      [(nano-unquote? elt) (nano-unquote-x elt)]
                      [(nano-cata? elt) (nano-cata-syntax elt)]
                      [(list? elt) (map f elt)]
                      [else elt])))
                (values
                  (if (null? (cdr var*))
                      (let ([t (car var*)])
                        (if (eq? t elt)
                            t
                            #`(map (lambda (#,t) #,elt) #,t)))
                      #`(map (lambda #,var* #,elt) #,@var*))
                  id var* binding*)))))
      (define process-nano-list
        (lambda (elt* spec binding*)
          (let f ([elt* elt*] [binding* binding*])
            (if (null? elt*)
                (values #''() '() '() binding*)
                (let ([elt (car elt*)] [elt* (cdr elt*)])
                  (if (nano-dots? elt)
                      (if (null? elt*)
                          (process-nano-dots elt spec binding*)
                          (let-values ([(elt elt-id elt-var* binding*)
                                        (process-nano-dots elt spec binding*)])
                            (let-values ([(elt* elt*-id* elt*-var* binding*) (f elt* binding*)])
                              (values #`(append #,elt #,elt*)
                                (cons elt-id elt*-id*)
                                (bound-id-union elt-var* elt*-var*)
                                binding*))))
                      (let-values ([(elt elt-id elt-var* binding*) (process-nano-elt elt spec binding*)])
                        (let-values ([(elt* elt*-id* elt*-var* binding*) (f elt* binding*)])
                          (values #`(cons #,elt #,elt*)
                            (cons elt-id elt*-id*)
                            (bound-id-union elt-var* elt*-var*)
                            binding*)))))))))
      (define process-nano-meta
        (lambda (x binding*)
          (let ([prec-alt (nano-meta-alt x)])
            (let-values ([(field* id* var* binding*)
                          (process-nano-fields (nano-meta-fields x)
                            (map (lambda (x) (find-spec x omrec)) (pair-alt-field-names prec-alt))
                            binding*)])
              (values
                #`(#,(pair-alt-maker prec-alt) '#,pass-name #,@field* #,@(map id->msg id*))
                #f var* binding*)))))
      (define process-nano-elt
        (lambda (elt spec binding*)
          (cond
            [(nano-meta? elt)
             (assert (pair-alt? (nano-meta-alt elt)))
             (process-nano-meta elt binding*)]
            [(nano-quote? elt) (let ([x (nano-quote-x elt)]) (values x x '() binding*))]
            [(nano-unquote? elt)
             (let ([x (nano-unquote-x elt)])
               (with-syntax ([expr (if (ntspec? spec)
                                       ; TODO: when we eventually turn these processors into named entities (either
                                       ; directly with meta define, define-syntax or some sort of property, replace
                                       ; this with the appropriate call.  In the meantime this should allow us to
                                       ; remove some of our in-contexts
                                       (with-syntax ([quasiquote (datum->syntax pass-name 'quasiquote)])
                                         #`(let-syntax ([quasiquote '#,(make-quasiquote-transformer
                                                                         pass-name (spec-type spec)
                                                                         omrec ometa-parser)])
                                             #,x))
                                       x)]
                             [tmp (car (generate-temporaries '(x)))])
               (values #'tmp x (list #'tmp) (cons #'(tmp expr) binding*))))]
            [(list? elt) (process-nano-list elt spec binding*)]
            [else (values elt elt '() binding*)])))
      (let-values ([(elt id var* binding*)
                    (process-nano-elt rhs-rec (nonterm-id->ntspec 'define-pass ntname (language-ntspecs omrec)) '())])
        #`(let #,binding* #,elt)))))
