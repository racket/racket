#lang racket/base
(require racket/contract/base
         (for-syntax racket/base
                     racket/require-transform
                     racket/provide-transform
                     syntax/stx
                     syntax/private/modcollapse-noctc))

(provide for-doc require/doc
         provide/doc ; not needed anymore
         thing-doc
         parameter-doc
         proc-doc
         proc-doc/names
         struct-doc
         struct*-doc
         generate-delayed-documents
         begin-for-doc)

(begin-for-syntax
 (define requires null)
 (define doc-body null)
 (define doc-exprs null)
 (define generated? #f)
 (define delayed? #f)

 (define (add-requires!/decl specs)
   (unless delayed?
     (syntax-local-lift-module-end-declaration
      #`(begin-for-syntax (add-relative-requires! (#%variable-reference)
                                                  (quote-syntax #,specs)))))
   (add-requires! (syntax-local-introduce specs)))

 (define (add-relative-requires! varref specs)
   (define mpi (variable-reference->module-path-index varref))
   (define-values (name base) (module-path-index-split mpi))
   (if name
       (add-requires!
        (with-syntax ([(spec ...) specs]
                      [rel-to (collapse-module-path-index 
                               mpi
                               (build-path (or (current-load-relative-directory)
                                               (current-directory))
                                           "here.rkt"))])
          #'((relative-in rel-to spec) ...)))
       (add-requires! specs)))

 (define (add-requires! specs)
   (set! requires (cons specs requires)))
 
 (define (generate-doc-submodule!)
   (unless generated?
     (set! generated? #t)
     (syntax-local-lift-module-end-declaration #'(doc-submodule)))))

(define-syntax for-doc
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ spec ...)
        (add-requires!/decl #'(spec ...))])
     (values null null))))

(define-syntax (doc-submodule stx)
  (with-syntax ([((req ...) ...)
                 (map syntax-local-introduce (reverse requires))]
                [(expr ...) (map syntax-local-introduce (reverse doc-exprs))]
                [doc-body
                 (map (lambda (s) (syntax-local-introduce
                                   (syntax-shift-phase-level s #f)))
                      (reverse doc-body))])
    ;; This module will be required `for-template':
    (if delayed?
        ;; delayed mode: return syntax objects to drop into context:
        #'(begin-for-syntax
           (module* srcdoc #f
             (require (for-syntax racket/base syntax/quote))
             (begin-for-syntax
              (provide get-docs)
              (define (get-docs)
                (list (quote-syntax (req ... ...))
                      (quote-syntax (expr ...))
                      (quote-syntax/keep-srcloc #:source 'doc doc-body))))))
        ;; normal mode: return an identifier that holds the document:
        (with-syntax ([((id d) ...) #'doc-body])
          #'(begin-for-syntax
             (module* srcdoc #f
               (require req ... ...)
               expr ...
               (define docs (list (cons 'id d) ...))
               (require (for-syntax racket/base))
               (begin-for-syntax
                (provide get-docs)
                (define (get-docs)
                  #'docs))))))))

(define-syntax (require/doc stx)
  (syntax-case stx ()
    [(_ spec ...)
     (add-requires!/decl #'(spec ...))
     #'(begin)]))

(define-for-syntax (do-provide/doc stx modes)
  (let ([forms (list stx)])
    (with-syntax ([((for-provide/contract (req ...) d id) ...)
                   (map (lambda (form)
                          (syntax-case form ()
                            [(id . _)
                             (identifier? #'id)
                             (let ([t (syntax-local-value #'id (lambda () #f))])
                               (unless (provide/doc-transformer? t)
                                 (raise-syntax-error
                                  #f
                                  "not bound as a provide/doc transformer"
                                  stx
                                  #'id))
                               (let* ([i (make-syntax-introducer)]
                                      [i2 (lambda (x) (syntax-local-introduce (i x)))])
                                 (let-values ([(p/c d req/d id)
                                               ((provide/doc-transformer-proc t)
                                                (i (syntax-local-introduce form)))])
                                   (list (i2 p/c) (i req/d) (i d) (i id)))))]
                            [_
                             (raise-syntax-error
                              #f
                              "not a provide/doc sub-form"
                              stx
                              form)]))
                        forms)])
      (with-syntax ([(p/c ...)
                     (map (lambda (form f)
                            (quasisyntax/loc form
                              (contract-out #,f)))
                          forms
                          (syntax->list #'(for-provide/contract ...)))])
        (generate-doc-submodule!)
        (set! doc-body (append (reverse (syntax->list #'((id d) ...)))
                               doc-body))
        (set! requires (cons #'(req ... ...) requires))
        (pre-expand-export #'(combine-out p/c ...) modes)))))

(define-syntax (begin-for-doc stx)
  (syntax-case stx ()
    [(_ d ...)
     (set! doc-exprs (append (reverse (syntax->list 
                                       (syntax-local-introduce
                                        #'(d ...))))
                             doc-exprs))
     #'(begin)]))

(define-syntax-rule (provide/doc form ...)
  (provide form ...))



(provide define-provide/doc-transformer
         (for-syntax
          provide/doc-transformer?
          provide/doc-transformer-proc))

(begin-for-syntax
 (define-struct provide/doc-transformer (proc)
   #:property 
   prop:provide-pre-transformer
   (lambda (self)
     (lambda (stx mode)
       (do-provide/doc stx mode)))))

(define-syntax-rule (define-provide/doc-transformer id rhs)
  (define-syntax id
    (make-provide/doc-transformer rhs)))

(module transformers racket/base
  (require (for-template racket/base racket/contract)
           racket/contract)
  (provide proc-doc-transformer proc-doc/names-transformer)
  
  (define (remove->i-deps stx-lst arg?)
    (let loop ([stx-lst stx-lst])
      (cond
        [(null? stx-lst) '()]
        [else
         (define fst (car stx-lst))
         (syntax-case fst ()
           [kwd
            (and arg? (keyword? (syntax-e #'kwd)))
            (let ()
              (when (null? (cdr stx-lst))
                (raise-syntax-error 'proc-doc "expected something to follow keyword" stx-lst))
              (define snd (cadr stx-lst))
              (syntax-case snd ()
                [(id (id2 ...) ctc)
                 (cons #'(kwd id ctc) (loop (cddr stx-lst)))]
                [(id ctc)
                 (cons #'(kwd id ctc) (loop (cddr stx-lst)))]
                [else
                 (raise-syntax-error 'proc-doc "unknown argument spec in ->i" snd)]))]
           [(id (id2 ...) ctc)
            (cons #'(id ctc) (loop (cdr stx-lst)))]
           [(id ctc)
            (cons #'(id ctc) (loop (cdr stx-lst)))]
           [else
            (raise-syntax-error 'proc-doc (if arg? "unknown argument spec in ->i" "unknown result spec in ->i") fst)])])))
  
  (define (proc-doc-transformer stx)
    (syntax-case stx ()
      [(_ id contract . desc+stuff)
       (let ()
         (define (one-desc desc+stuff)
           (syntax-case desc+stuff ()
             [(desc) #'desc]
             [() (raise-syntax-error 'proc-doc "expected a description expression" stx)]
             [(a b . c) (raise-syntax-error 'proc-doc "expected just a single description expression" stx #'a)]))
         (define (parse-opts opts desc+stuff)
           (syntax-case opts ()
             [() #`(() #,(one-desc desc+stuff))]
             [(opt ...)
              (with-syntax ([(opt ...) (remove->i-deps (syntax->list #'(opt ...)) #t)])
                (syntax-case desc+stuff ()
                  [((defaults ...) . desc+stuff)
                   (let ()
                     (define def-list (syntax->list #'(defaults ...)))                
                     (define opt-list (syntax->list #'(opt ...)))
                     (unless (= (length def-list) (length opt-list))
                       (raise-syntax-error 'proc-doc
                                           (format "expected ~a default values, but got ~a"
                                                   (length opt-list) (length def-list))
                                           stx
                                           opts))
                     #`(#,(for/list ([opt (in-list opt-list)]
                                     [def (in-list def-list)])
                            (syntax-case opt ()
                              [(id ctc)
                               #`(id ctc #,def)]
                              [(kwd id ctc)
                               #`(kwd id ctc #,def)]))
                        #,(one-desc #'desc+stuff)))]))]))
         (define-values (header result body-extras desc)
           (syntax-case #'contract (->d ->i -> values)
             [(->d (req ...) () (values [name res] ...))
              (values #'(id req ...) #'(values res ...) #'() (one-desc #'desc+stuff))]
             [(->d (req ...) () #:pre-cond condition (values [name res] ...))
              (values #'(id req ...) #'(values res ...) #'((bold "Pre-condition: ") (racket condition) "\n" "\n") (one-desc #'desc+stuff))]
             [(->d (req ...) () [name res])
              (values #'(id req ...) #'res #'() (one-desc #'desc+stuff))]
             [(->d (req ...) () #:pre-cond condition [name res])
              (values #'(id req ...) #'res #'((bold "Pre-condition: ")  (racket condition) "\n" "\n" ) (one-desc #'desc+stuff))]
             [(->d (req ...) () #:rest rest rest-ctc [name res])
              (values #'(id req ... [rest rest-ctc] (... ...)) #'res #'() (one-desc #'desc+stuff))]
             [(->d (req ...) (one more ...) whatever)
              (raise-syntax-error
               #f
               (format "unsupported ->d contract form for ~a, optional arguments non-empty, must use proc-doc/names"
                       (syntax->datum #'id))
               stx
               #'contract)]
             [(->d whatever ...) 
              (raise-syntax-error
               #f
               (format "unsupported ->d contract form for ~a" (syntax->datum #'id))
               stx
               #'contract)]
             
             [(->i (req ...) (opt ...) (values ress ...))
              (with-syntax ([(req ...) (remove->i-deps (syntax->list #'(req ...)) #t)]
                            [((opt ...) desc) (parse-opts #'(opt ...) #'desc+stuff)]
                            [([name res] ...) (remove->i-deps (syntax->list #'(req ...)) #f)])
                (values #'(id req ... opt ...) #'(values res ...) #'() #'desc))]
             [(->i (req ...) (opt ...) #:pre (pre-id ...) condition (values ress ...))
              (with-syntax ([(req ...) (remove->i-deps (syntax->list #'(req ...)) #t)]
                            [((opt ...) desc) (parse-opts #'(opt ...) #'desc+stuff)]
                            [([name res] ...) (remove->i-deps (syntax->list #'(req ...)) #f)])
                (values #'(id req ... opt ...) #'(values res ...) #'((bold "Pre-condition: ") (racket condition) "\n" "\n") #'desc))]
             [(->i (req ...) (opt ...) res)
              (with-syntax ([(req ...) (remove->i-deps (syntax->list #'(req ...)) #t)]
                            [((opt ...) desc) (parse-opts #'(opt ...) #'desc+stuff)]
                            [([name res]) (remove->i-deps (list #'res) #f)])
                (values #'(id req ... opt ...) #'res #'() #'desc))]
             [(->i (req ...) (opt ...) #:pre (pre-id ...) condition [name res])
              (with-syntax ([(req ...) (remove->i-deps (syntax->list #'(req ...)) #t)]
                            [((opt ...) desc) (parse-opts #'(opt ...) #'desc+stuff)]
                            [([name res]) (remove->i-deps (list #'res) #f)])
                (values #'(id req ... opt ...) #'res #'((bold "Pre-condition: ")  (racket condition) "\n" "\n" ) #'desc))]
             [(->i (req ...) (opt ...) #:rest rest res)
              (with-syntax ([(req ...) (remove->i-deps (syntax->list #'(req ...)) #t)]
                            [((opt ...) desc) (parse-opts #'(opt ...) #'desc+stuff)]
                            [([name-t rest-ctc]) (remove->i-deps (list #'rest) #t)]
                            [([name res]) (remove->i-deps (list #'res) #f)])
                (values #'(id req ... opt ... [name-t rest-ctc] (... ...)) #'res #'() #'desc))]
             [(->i whatever ...) 
              (raise-syntax-error
               #f
               (format "unsupported ->i contract form for ~a" (syntax->datum #'id))
               stx
               #'contract)]
             
             [(-> result)
              (values #'(id) #'result #'() (one-desc #'desc+stuff))]
             [(-> whatever ...) 
              (raise-syntax-error
               #f
               (format "unsupported -> contract form for ~a, must use proc-doc/names if there are arguments"
                       (syntax->datum #'id))
               stx
               #'contract)]
             [(id whatever ...)
              (raise-syntax-error
               #f
               (format "unsupported ~a contract form (unable to synthesize argument names)" (syntax->datum #'id))
               stx
               #'contract)]))
         (values
          #'[id contract]
          #`(defproc #,header #,result #,@body-extras #,@desc)
          #'(scribble/manual
             racket/base) ; for `...'
          #'id))]))
  
  (define (proc-doc/names-transformer stx)
    (syntax-case stx ()
      [(_ id contract names desc)
       (with-syntax ([header                      
                      (syntax-case #'(contract names) (->d -> ->* values case->)
                        [((-> ctcs ... result) (arg-names ...))
                         (begin
                           (unless (= (length (syntax->list #'(ctcs ...)))
                                      (length (syntax->list #'(arg-names ...))))
                             (raise-syntax-error #f "mismatched argument list and domain contract count" stx))
                           #'([(id (arg-names ctcs) ...) result]))]
                        
                        [((->* (mandatory ...) (optional ...) result) 
                          names)
                         (syntax-case #'names ()
                           [((mandatory-names ...)
                             ((optional-names optional-default) ...))
                            
                            (let ([build-mandatories/optionals
                                   (λ (names contracts extras)
                                     (let ([names-length (length names)]
                                           [contracts-length (length contracts)])
                                       (let loop ([contracts contracts]
                                                  [names names]
                                                  [extras extras])
                                         (cond
                                           [(and (null? names) (null? contracts)) '()]
                                           [(or (null? names) (null? contracts))
                                            (raise-syntax-error #f
                                                                (format "mismatched ~a argument list count and domain contract count (~a)"
                                                                        (if extras "optional" "mandatory")
                                                                        (if (null? names)
                                                                            "ran out of names"
                                                                            "ran out of contracts"))
                                                                stx)]
                                           [else
                                            (let ([fst-name (car names)]
                                                  [fst-ctc (car contracts)])
                                              (if (keyword? (syntax-e fst-ctc))
                                                  (begin
                                                    (unless (pair? (cdr contracts))
                                                      (raise-syntax-error #f
                                                                          "keyword not followed by a contract"
                                                                          stx))
                                                    (cons (if extras
                                                              (list fst-ctc fst-name (cadr contracts) (car extras))
                                                              (list fst-ctc fst-name (cadr contracts)))
                                                          (loop (cddr contracts)
                                                                (cdr names)
                                                                (if extras
                                                                    (cdr extras)
                                                                    extras))))
                                                  (cons (if extras 
                                                            (list fst-name fst-ctc (car extras))
                                                            (list fst-name fst-ctc))
                                                        (loop (cdr contracts) (cdr names) (if extras
                                                                                              (cdr extras)
                                                                                              extras)))))]))))])
                            
                              #`([(id #,@(build-mandatories/optionals (syntax->list #'(mandatory-names ...))
                                                                      (syntax->list #'(mandatory ...))
                                                                      #f)
                                      #,@(build-mandatories/optionals (syntax->list #'(optional-names ...))
                                                                      (syntax->list #'(optional ...))
                                                                      (syntax->list #'(optional-default ...))))
                                result]))]
                           [(mandatory-names optional-names)
                            (begin
                              (syntax-case #'mandatory-names ()
                                [(mandatory-names ...)
                                 (andmap identifier? (syntax->list #'(mandatory-names ...)))]
                                [x
                                 (raise-syntax-error #f "mandatory names should be a sequence of identifiers" 
                                                     stx 
                                                     #'mandatory-names)])
                              (syntax-case #'optional-names ()
                                [((x y) ...)
                                 (andmap identifier? (syntax->list #'(x ... y ...)))]
                                [((x y) ...)
                                 (for-each
                                  (λ (var) 
                                    (unless (identifier? var)
                                      (raise-syntax-error #f "expected an identifier in the optional names" stx var)))
                                  (syntax->list #'(x ... y ...)))]
                                [(a ...)
                                 (for-each
                                  (λ (a)
                                    (syntax-case stx ()
                                      [(x y) (void)]
                                      [other
                                       (raise-syntax-error #f "expected an sequence of two idenfiers" stx #'other)]))
                                  (syntax->list #'(a ...)))]))]
                           [x
                            (raise-syntax-error
                             #f
                             "expected two sequences, one of mandatory names and one of optionals"
                             stx
                             #'x)])]
                        [((case-> (-> doms ... rng) ...)
                          ((args ...) ...))
                         (begin
                           (unless (= (length (syntax->list #'((doms ...) ...)))
                                      (length (syntax->list #'((args ...) ...))))
                             (raise-syntax-error #f
                                                 "number of cases and number of arg lists do not have the same size"
                                                 stx))
                           (for-each
                            (λ (doms args)
                              (unless (= (length (syntax->list doms))
                                         (length (syntax->list args)))
                                (raise-syntax-error #f "mismatched case argument list and domain contract" stx
                                                    #f 
                                                    (list doms args))))
                            (syntax->list #'((doms ...) ...))
                            (syntax->list #'((args ...) ...)))
                           #'([(id (args doms) ...) rng] ...))]
                        [else
                         (raise-syntax-error
                          #f
                          "unsupported procedure contract form (no argument names)"
                          stx
                          #'contract)])])
         (values
          #'[id contract]
          #'(defproc* header . desc)
          #'((only-in scribble/manual defproc*))
          #'id))])))

(require (for-syntax (submod "." transformers)))
(define-provide/doc-transformer proc-doc proc-doc-transformer)
(define-provide/doc-transformer proc-doc/names proc-doc/names-transformer)

(define-provide/doc-transformer parameter-doc
  (lambda (stx)
    (syntax-case stx (parameter/c)
      [(_ id (parameter/c contract) arg-id desc)
       (begin
         (unless (identifier? #'arg-id)
           (raise-syntax-error 'parameter-doc 
                               "expected an identifier"
                               stx
                               #'arg-id))
         (unless (identifier? #'id)
           (raise-syntax-error 'parameter-doc 
                               "expected an identifier"
                               stx
                               #'id))
         (values
          #'[id (parameter/c contract)]
          #'(defparam id arg-id contract . desc)
          #'((only-in scribble/manual defparam))
          #'id))])))

(define-for-syntax (struct-doc-transformer stx result-form)
  (syntax-case stx ()
    [(_ struct-name ([field-name contract-expr-datum] ...) . stuff)
     (let ()
       (define the-name #f)
       (syntax-case #'struct-name ()
         [x (identifier? #'x) (set! the-name #'x)]
         [(x y) (and (identifier? #'x) (identifier? #'y)) 
                (set! the-name #'x)]
         [_
          (raise-syntax-error #f 
                              "expected an identifier or sequence of two identifiers"
                              stx
                              #'struct-name)])
       (for ([f (in-list (syntax->list #'(field-name ...)))])
         (unless (identifier? f)
           (raise-syntax-error #f 
                               "expected an identifier"
                               stx
                               f)))
       (define omit-constructor? #f)
       (define-values (ds-args desc)
         (let loop ([ds-args '()]
                    [stuff #'stuff])
           (syntax-case stuff ()
             [(#:mutable . more-stuff)
              (loop (cons (stx-car stuff) ds-args)
                    #'more-stuff)]
             [(#:inspector #f . more-stuff)
              (loop (list* (stx-car (stx-cdr stuff))
                           (stx-car stuff)
                           ds-args)
                    #'more-stuff)]
             [(#:prefab . more-stuff) 
              (loop (cons (stx-car stuff) ds-args)
                    #'more-stuff)]
             [(#:transparent . more-stuff)
              (loop (cons (stx-car stuff) ds-args)
                    #'more-stuff)]
             [(#:constructor-name id . more-stuff)
              (loop (list* (stx-car (stx-cdr stuff))
                           (stx-car stuff)
                           ds-args)
                    #'more-stuff)]
             [(#:extra-constructor-name id . more-stuff)
              (loop (list* (stx-car (stx-cdr stuff))
                           (stx-car stuff)
                           ds-args)
                    #'more-stuff)]
             [(#:omit-constructor . more-stuff)
              (begin
                (set! omit-constructor? #t)
                (loop (cons (stx-car stuff) ds-args)
                      #'more-stuff))]
             [(x . more-stuff)
              (keyword? (syntax-e #'x))
              (raise-syntax-error #f
                                  "unknown keyword"
                                  stx
                                  (stx-car stuff))]
             [(desc)
              (values (reverse ds-args) #'desc)]
             [_
              (raise-syntax-error #f "bad syntax" stx)])))
       (values
        #`(struct struct-name ((field-name contract-expr-datum) ...) 
            #,@(if omit-constructor?
                   '(#:omit-constructor)
                   '()))
        #`(#,result-form struct-name ([field-name contract-expr-datum] ...)
                         #,@(reverse ds-args)
                         #,@desc)
        #`((only-in scribble/manual #,result-form))
        the-name))]))

(define-provide/doc-transformer struct-doc
  (λ (stx) 
    (struct-doc-transformer stx #'defstruct)))
(define-provide/doc-transformer struct*-doc
  (λ (stx) 
    (struct-doc-transformer stx #'defstruct*)))

(define-provide/doc-transformer thing-doc
  (lambda (stx)
    (syntax-case stx ()
      [(_ id contract desc)
       (begin
         (unless (identifier? #'id)
           (raise-syntax-error 'parameter/doc 
                               "expected an identifier"
                               stx
                               #'id))
         (values
          #'[id contract]
          #'(defthing id contract . desc)
          #'((only-in scribble/manual defthing))
          #'id))])))

(define-syntax (generate-delayed-documents stx)
  (syntax-case stx () 
    [(_) 
     (begin
       (set! delayed? #t)
       #'(begin))]))


(module+ test
  (require (submod ".." transformers) 
           rackunit
           racket/contract)
  
  (define (try-docs transformer input) 
    (define-values (_0 docs _1 _2) (transformer input))
    (syntax->datum docs))
  
  (check-equal? (try-docs proc-doc-transformer #'(_ f (-> void?) ()))
                '(defproc (f) void?))
  (check-equal? (try-docs proc-doc-transformer #'(_ f (->i ([x integer?]) () [result void?]) ()))
                '(defproc (f [x integer?]) void?))
  (check-equal? (try-docs proc-doc-transformer #'(_ f (->i ([x integer?] #:y [y boolean?]) () [res void?]) ()))
                '(defproc (f [x integer?] [#:y y boolean?]) void?))
  (check-equal? (try-docs proc-doc-transformer #'(_ f (->i ([x integer?]) ([y boolean?] [z char?]) [result void?]) (#t #\x) ()))
                '(defproc (f [x integer?] [y boolean? #t] [z char? #\x]) void?))
  (check-equal? (try-docs proc-doc-transformer #'(_ f (->i ([x integer?] #:y [y boolean?]) ([z char?] #:w [w string?]) [res void?]) (#\a "b") ()))
                '(defproc (f [x integer?] [#:y y boolean?] [z char? #\a] [#:w w string? "b"]) void?))
  
  (check-equal? (try-docs proc-doc-transformer
                          #'(_ g
                               (->i ([str string?])
                                    ()
                                    #:rest [rest (listof any/c)]
                                    [res (str) integer?])
                               ()))
                '(defproc (g (str string?) (rest (listof any/c)) ...) integer?))
  
  (check-equal? (try-docs proc-doc/names-transformer #'(_ f (-> integer? char? boolean?) (a b) ()))
                '(defproc* (((f [a integer?] [b char?]) boolean?))))
  (check-equal? (try-docs proc-doc/names-transformer #'(_ f (->* (integer? char?) () boolean?) ((a b) ()) ()))
                '(defproc* (((f [a integer?] [b char?]) boolean?))))
  (check-equal? (try-docs proc-doc/names-transformer #'(_ f (->* (integer? char?) (string? number?) boolean?) ((a b) ((c "a") (d 11))) ()))
                '(defproc* (((f [a integer?] [b char?] [c string? "a"] [d number? 11]) boolean?))))
  (check-equal? (try-docs proc-doc/names-transformer #'(_ f (case-> (-> integer? char?) (-> string? number? boolean? void?)) ((a) (b c d)) ()))
                '(defproc* (((f [a integer?]) char?)
                            ((f [b string?] [c number?] [d boolean?]) void?)))))
