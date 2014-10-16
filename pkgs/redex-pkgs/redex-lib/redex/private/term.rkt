#lang racket/base

(require (for-syntax racket/base 
                     "term-fn.rkt"
                     syntax/boundmap
                     syntax/parse
                     racket/syntax
                     (only-in racket/list flatten)
                     "keyword-macros.rkt"
                     "matcher.rkt")
         (only-in "fresh.rkt" variable-not-in)
         syntax/datum
         "error.rkt"
         "lang-struct.rkt"
         "matcher.rkt")

(provide term term-let define-term
         hole in-hole
         term-let/error-name term-let-fn term-define-fn
         (for-syntax term-rewrite
                     term-temp->pat
                     currently-expanding-term-fn
                     judgment-form-id?))

(define-syntax (hole stx) (raise-syntax-error 'hole "used outside of term"))
(define-syntax (in-hole stx) (raise-syntax-error 'in-hole "used outside of term"))

(define (with-syntax* stx)
  (syntax-case stx ()
    [(_ () e) (syntax e)]
    [(_ (a b ...) e) (syntax (with-syntax (a) (with-syntax* (b ...) e)))]))

(define-for-syntax lang-keyword
  (list '#:lang #f))

(define-for-syntax (judgment-form-id? stx)
  (and (identifier? stx)
       (judgment-form? (syntax-local-value stx (λ () #f)))))

(define-syntax (term stx)
  (syntax-case stx ()
    [(term t . kw-args)
     (let ()
       (define lang-stx (car
                         (parse-kw-args (list lang-keyword)
                                        (syntax kw-args)
                                        stx
                                        (syntax-e #'form))))
       (cond
         [lang-stx
          (define-values (lang-nts lang-nt-ids)
            (let loop ([ls lang-stx])
              (define slv (syntax-local-value ls (λ () lang-stx)))
              (if (term-id? slv)
                  (loop (term-id-prev-id slv))
                  (values (language-id-nts ls 'term)
                          (language-id-nt-identifiers ls 'term)))))
          (quasisyntax/loc stx (term/nts t #,lang-nts #,lang-nt-ids))]
         [else
          (syntax/loc stx (term/nts t #f #f))]))]))

(define-syntax (term/nts stx)
  (syntax-case stx ()
    [(_ arg nts nt-ids)
     (syntax/loc stx (#%expression (term/private arg nts nt-ids)))]))

(define-for-syntax current-id-stx-table (make-parameter #f))

(define-syntax (term/private stx)
  (syntax-case stx ()
    [(_ arg-stx nts-stx id-stx-table)
     (parameterize ([current-id-stx-table (syntax-e #'id-stx-table)])
       (with-disappeared-uses
        (let-values ([(t a-mfs) (term-rewrite/private #'arg-stx #'nts-stx #f)])
          (term-temp->unexpanded-term t a-mfs))))]))

(define-for-syntax (term-rewrite t names)
  (let*-values ([(t-t a-mfs) (term-rewrite/private t #`#f names)]
                [(t-pat) (term-temp->pat t-t names)])
    t-pat))

(define-syntax (mf-apply stx)
  (syntax-case stx ()
    [(_ mf)
     (quasisyntax/loc stx (λ (x) #,(syntax/loc stx (mf x))))]))

(define-syntax (jf-apply stx)
  (syntax-case stx ()
    [(_ jf)
     (judgment-form-id? #'jf)
     (judgment-form-term-proc (syntax-local-value #'jf (λ () #f)))]))

(define-syntax (mf-map stx)
  (syntax-case stx ()
    [(_ inner-apps)
     #'(λ (l) (map inner-apps l))]))

(define-for-syntax currently-expanding-term-fn (make-parameter #f))


;; term-rewrite/private produces expressions from the following grammar:
;; (which get further processed by term-temp->unexpanded-term or term-temp->pat)
;;
;; term-template := `(term-template (,term-binding ...) ,term-datum)
;; term-binding  := `(,t-bind-pat (,mf-apps ,term-datum))
;; t-bind-pat    := id | (ref id) | `(,t-b-seq ...)
;; t-b-seq       := t-bind-pat | ellipsis
;; mf-apps       := `(mf-map ,mf-apps) | `(mf-apply ,metafunction-id) | `(jf-apply ,judgment-form-id)
;; term-datum    := `(quasidatum ,d)
;; d             := literal | pattern-variable | `(,d-seq ...) | ;; other (holes, undatum)
;; d-seq         := d | ellipsis

;; actually can be attached to anything that matches a variable in the language
;; is removed by the internal term rewriter
;; and expands into an error
;; *bound* things will be caught by the other rewrite/max-depth possibilities

(define-for-syntax (term-rewrite/private arg-stx nts-stx names)

  (define lang-nts (syntax->datum nts-stx))
  (define outer-bindings '())
  (define applied-metafunctions
    (make-free-identifier-mapping))
  
  (define (rewrite stx)
    (let-values ([(rewritten _) (rewrite/max-depth stx 0 #f #f)])
      rewritten))
  
  (define (rewrite-application fn args depth srcloc-stx)
    (let-values ([(rewritten max-depth) (rewrite/max-depth args depth #t #t)])
      (let ([result-id (car (generate-temporaries '(f-results)))])
        (with-syntax ([fn fn])
          (let loop ([func (if (judgment-form-id? #'fn)
                               (syntax/loc srcloc-stx (jf-apply fn))
                               (syntax/loc srcloc-stx (mf-apply fn)))]
                     [args-stx rewritten]
                     [res result-id]
                     [args-depth (min depth max-depth)])
            (with-syntax ([func func]
                          [args args-stx]
                          [res res])
              (if (zero? args-depth)
                  (begin
                    (set! outer-bindings 
                          (cons (syntax [res (func (quasidatum args))])
                                outer-bindings))
                    (values result-id (min depth max-depth)))
                  (with-syntax ([dots (datum->syntax #'here '... arg-stx)])
                    (loop (syntax (begin (mf-map func)))
                          (syntax/loc args-stx (args dots))
                          (syntax (res dots))
                          (sub1 args-depth))))))))))
  
  (define (rewrite/max-depth stx depth ellipsis-allowed? continuing-an-application?)
    (syntax-case stx (unquote unquote-splicing in-hole hole)
      [(metafunc-name arg ...)
       (and (not continuing-an-application?)
            (identifier? (syntax metafunc-name))
            (if names
                (not (memq (syntax->datum #'metafunc-name) names))
                #t)
            (term-fn? (syntax-local-value (syntax metafunc-name) (λ () #f))))
       (let ([f (term-fn-get-id (syntax-local-value/record (syntax metafunc-name) (λ (x) #t)))])
         (free-identifier-mapping-put! applied-metafunctions 
                                       (datum->syntax f (syntax-e f) #'metafunc-name)
                                       #t)
         (rewrite-application f (syntax/loc stx (arg ...)) depth stx))]
      [(jf-name arg ...)
       (and (not continuing-an-application?)
            (identifier? (syntax jf-name))
            (if names
                (not (memq (syntax->datum #'jf-name) names))
                #t)
            (judgment-form-id? #'jf-name))
       (begin
         (unless (not (memq 'O (judgment-form-mode (syntax-local-value #'jf-name))))
           (raise-syntax-error 'term 
                               "judgment forms with output mode (\"O\") positions disallowed"
                               arg-stx stx))
         (rewrite-application #'jf-name (syntax/loc stx (arg ...)) depth stx))]
      [f
       (and (identifier? (syntax f))
            (if names
                (not (memq (syntax->datum #'f) names))
                #t)
            (term-fn? (syntax-local-value (syntax f) (λ () #f))))
       (raise-syntax-error 'term "metafunction must be in an application" arg-stx stx)]
      [x
       (and (identifier? #'x)
            (term-id? (syntax-local-value #'x (λ () #f))))
       (let ([id (syntax-local-value/record #'x (λ (x) #t))])
         (define stx-result (datum->syntax (term-id-id id) (syntax-e (term-id-id id)) #'x))
         
         (define raw-sym (syntax-e #'x))
         (define raw-str (symbol->string raw-sym))
         (define m (regexp-match #rx"^([^_]*)_" raw-str))
         (define prefix-sym (if m
                                (string->symbol (list-ref m 1))
                                raw-sym))
         (check-id (syntax->datum (term-id-id id)) stx ellipsis-allowed? #t)
         
         (define new-id
           (build-disappeared-use (current-id-stx-table) 
                                  prefix-sym
                                  (syntax-local-introduce #'x)))
         (when new-id (record-disappeared-uses (list new-id)))
         (values stx-result
                 (term-id-depth id)))]
      [x
       (defined-term-id? #'x)
       (let ([ref (syntax-property
                   (defined-term-value (syntax-local-value #'x))
                   'disappeared-use 
                   (syntax-local-introduce #'x))])
         (check-id (syntax->datum #'x) stx ellipsis-allowed? #t)
         (with-syntax ([v #`(begin
                              #,(defined-check ref "term" #:external #'x)
                              #,ref)])
           (values #`(undatum v) 0)))]
      [(unquote x)
       (values (syntax (undatum x)) 0)]
      [(unquote . x)
       (raise-syntax-error 'term "malformed unquote" arg-stx stx)]
      [(unquote-splicing x)
       (values (syntax (undatum-splicing x)) 0)]
      [(unquote-splicing . x)
       (raise-syntax-error 'term "malformed unquote splicing" arg-stx stx)]
      [(in-hole id body)
       (rewrite-application (syntax (λ (x) (apply plug x))) (syntax/loc stx (id body)) depth stx)]
      [(in-hole . x)
       (raise-syntax-error 'term "malformed in-hole" arg-stx stx)]
      [hole (values (syntax (undatum the-hole)) 0)]
      [x
       (and (identifier? (syntax x))
            (check-id (syntax->datum #'x) stx ellipsis-allowed? #f))
       (values stx 0)]
      [() (values stx 0)]
      [(x ... . y)
       (not (null? (syntax->list #'(x ...))))
       (let-values ([(x-rewrite max-depth)
                     (let i-loop ([xs (syntax->list (syntax (x ...)))])
                       (cond
                         [(null? xs) (rewrite/max-depth #'y depth #t #f)]
                         [else
                          (let ([new-depth (if (and (not (null? (cdr xs)))
                                                    (identifier? (cadr xs))
                                                    (free-identifier=? (quote-syntax ...)
                                                                       (cadr xs)))
                                               (+ depth 1)
                                               depth)])
                            (let-values ([(fst fst-max-depth)
                                          (rewrite/max-depth (car xs) new-depth #t #f)]
                                         [(rst rst-max-depth)
                                          (i-loop (cdr xs))])
                              (values (cons fst rst)
                                      (max fst-max-depth rst-max-depth))))]))])
         (values (datum->syntax stx x-rewrite stx stx) max-depth))]
      
      [_ (values stx 0)]))
  
  (define (check-id id stx ellipsis-allowed? term-id?)
    (define m (regexp-match #rx"^([^_]*)_" (symbol->string id)))
    (cond
      [m
       (define before-underscore (string->symbol (list-ref m 1)))
       (when (and (not term-id?)
                  (equal? before-underscore '...))
         (raise-syntax-error 
          'term
          "ellipsis cannot have an underscore"
          arg-stx stx))
       (when lang-nts
         (unless (memq before-underscore (append pattern-symbols lang-nts))
           (raise-syntax-error
            'term 
            "before underscore must be either a non-terminal or a built-in pattern"
            arg-stx stx)))]
      [else
       (unless ellipsis-allowed?
         (when (equal? id '...) 
           (raise-syntax-error
            'term
            "misplaced ellipsis"
            arg-stx stx)))]))
       
  (values
   (with-syntax ([rewritten (rewrite arg-stx)])
     (with-syntax ([(outer-bs ...) (reverse outer-bindings)])
       #'(term-template
          (outer-bs ...)
          (quasidatum rewritten))))
   applied-metafunctions))

(define-for-syntax (term-temp->unexpanded-term term-stx applied-mfs)
  (syntax-case term-stx (term-template)
    [(term-template (outer-bs ...) t)
     (let ([outer-bindings (syntax->list #'(outer-bs ...))])
       #`(begin
           #,@(free-identifier-mapping-map
               applied-mfs
               (λ (f _) (defined-check f "metafunction")))
           #,(let loop ([bs outer-bindings])
               (cond
                 [(null? bs) (syntax t)]
                 [else (with-syntax ([rec (loop (cdr bs))]
                                     [fst (car bs)])
                         (syntax (with-datum (fst)
                                             rec)))]))))]))

(define-for-syntax (term-temp->pat t-t names)
  (syntax-case t-t (term-template)
    [(term-template (term-bindings ...) body-datum)
     (let loop ([t-bs-raw (syntax->list #'(term-bindings ...))]
                [t-bs '()]
                [ns names])
       (cond
         [(null? t-bs-raw)
          (with-syntax ([body-pat (term-datum->pat #'body-datum ns)]
                        [(bind-pats ...) (reverse t-bs)])
            #'(term-pattern (bind-pats ...) body-pat))]
         [else
          (with-syntax ([(bind-lhs (bind-mf-sig bind-term-datum)) (car t-bs-raw)])
            (let ([new-names (append ns (bind-lhs-name #'bind-lhs))])
              (with-syntax ([bind-rhs-pat (term-datum->pat #'bind-term-datum new-names)]
                            [bind-lhs-pat (d->pat #'bind-lhs new-names)]
                            [bind-mf-pat (bind-mf-sig->pat #'bind-mf-sig)])
                (loop (cdr t-bs-raw)
                      (cons #'(bind-lhs-pat (bind-mf-pat bind-rhs-pat)) t-bs)
                      new-names))))]))]))

(define-for-syntax (term-datum->pat t-d names)
  (syntax-case t-d ()
    [(quasidatum d)
     (d->pat #'d names)]))

(define-for-syntax (d->pat d names)
  (syntax-case d (... undatum in-hole undatum-splicing variable-not-in term quote)
    [()
     #'(list)]
    [(undatum (variable-not-in (term t) (quote s)))
     (with-syntax ([t-pat (d->pat #'t names)])
       #'(variable-not-in t-pat s))]
    [(undatum (variable-not-in (term t1) (term t2)))
     (with-syntax ([t1-pat (d->pat #'t1 names)]
                   [t2-pat (d->pat #'t2 names)])
       #'(variable-not-in t1-pat t2-pat))]
    [(undatum rest ...) ;; holes are also undatumed
     d]
    [(undatum-splicing rest ...)
     d]
    [(in-hole rest ...)
     d]
    [(r-dat (... ...) rest ...)
     (with-syntax ([r-pat (d->pat #'r-dat names)]
                   [(list rest-pats ...) (d->pat #'(rest ...) names)])
       #'(list (repeat r-pat #f #f) rest-pats ...))]
    [(d ds ...)
     (with-syntax ([p (d->pat #'d names)]
                   [(list ps ...) (d->pat #'(ds ...) names)])
       #'(list p ps ...))]
    [var
     (and (identifier? #'var)
          (memq (syntax->datum #'var) names))
     #'(name var any)]
    [literal
     #'literal]))

(define-for-syntax (bind-lhs-name blhs)
  (define name (filter (λ (n) (not (eq? n '...)))
                       (flatten (syntax->datum blhs))))
  (unless (equal? (length name) 1)
    (error 'term-rewrite "term function lhs binding had more than one name: ~s" (syntax->datum blhs)))
  name)

(define-for-syntax (bind-mf-sig->pat bmfs)
  (syntax-case bmfs ()
    ;; TODO : handle apps at ellipsis depth , handle judgment forms (I only)
    [(mf-apply f)
     (and (identifier? #'mf-apply)
          (eq? (syntax-e #'mf-apply) 'mf-apply))
     #'(metafunc f)]
    [(jf-apply f)
     (and (identifier? #'jf-apply)
          (eq? (syntax-e #'jf-apply) 'jf-apply))
     #'(jform f)]))

(define-syntax (term-let-fn stx)
  (syntax-case stx ()
    [(_ ([f rhs] ...) body1 body2 ...)
     (with-syntax ([(g ...) (generate-temporaries (syntax (f ...)))])
       (syntax 
        (let ([g rhs] ...)
          (let-syntax ([f (make-term-fn #'g)] ...)
            body1
            body2 ...))))]))

(define-syntax (term-define-fn stx)
  (syntax-case stx ()
    [(_ id exp)
     (with-syntax ([id2 (datum->syntax #'here (syntax-e #'id) #'id #'id)])
       (syntax
        (begin
          (define id2 exp)
          (define-syntax id
            (make-term-fn #'id2)))))]))

(define-syntax (term-let/error-name stx)
  (syntax-case stx ()
    [(_ error-name ([x1 rhs1] [x rhs] ...) body1 body2 ...)
     (let ()
       (unless (identifier? #'error-name)
         (raise-syntax-error 'term-let/error-name 
                             "expected an identifier as the first argument"
                             stx
                             #'error-name))
       (define-values (orig-names new-names depths new-x1)
         (let loop ([stx #'x1] [depth 0])
           (define ((combine orig-names new-names depths new-pat)
                    orig-names* new-names* depths* new-pat*)
             (values (append orig-names orig-names*)
                     (append new-names new-names*)
                     (append depths depths*)
                     (cons new-pat new-pat*)))
           (syntax-case stx (...)
             [x 
              (and (identifier? #'x)
                   (not (free-identifier=? (quote-syntax ...) #'x)))
              (let ([new-name (datum->syntax #'here (syntax-e #'x) #'x #'x)])
                (values (list #'x)
                        (list new-name)
                        (list depth)
                        new-name))]
             [(x (... ...) . xs)
              (let-values ([(orig-names new-names depths new-pat)
                            (call-with-values
                             (λ () (loop #'xs depth))
                             (call-with-values
                              (λ () (loop #'x (add1 depth)))
                              combine))])
                (values orig-names new-names depths 
                        (list* (car new-pat) #'(... ...) (cdr new-pat))))]
             [(x . xs)
              (call-with-values
               (λ () (loop #'xs depth))
               (call-with-values
                (λ () (loop #'x depth))
                combine))]
             [_
              (values '() '() '() stx)])))
       (with-syntax ([(orig-names ...) orig-names]
                     [(new-names ...) new-names]
                     [(depths ...) depths]
                     [new-x1 new-x1]
                     [no-match (syntax/loc (syntax rhs1)
                                 (error 'error-name "term ~s does not match pattern ~s" rhs1 'x1))])
         (syntax
          (datum-case rhs1 ()
                      [new-x1
                       (let-syntax ([orig-names (make-term-id #'new-names depths #'orig-names)] ...)
                         (term-let/error-name error-name ((x rhs) ...) body1 body2 ...))]
                      [_ no-match]))))]
    [(_ error-name () body1 body2 ...)
     (syntax
      (begin body1 body2 ...))]
    [(_ x)
     (raise-syntax-error 'term-let "expected at least one body" stx)]))

(define-syntax (term-let stx)
  (syntax-case stx ()
    [(_ () body1)
     #'body1]
    [(_ ([x rhs] ...) body1 body2 ...)
     (syntax
      (term-let/error-name term-let ((x rhs) ...) body1 body2 ...))]
    [(_ x)
     (raise-syntax-error 'term-let "expected at least one body" stx)]))

(define-syntax (define-term stx)
  (syntax-parse stx
                [(_ x:id t:expr)
                 (not-expression-context stx)
                 (with-syntax ([term-val (syntax-property (syntax/loc #'x term-val)
                                                          'undefined-error-name
                                                          (syntax-e #'x))])
                   #'(begin
                       (define term-val (term t))
                       (define-syntax x (defined-term #'term-val))))]))
