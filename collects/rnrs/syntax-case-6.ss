#lang scheme/base

(require (for-syntax scheme/base)
         r6rs/private/qq-gen
         scheme/stxparam
         scheme/mpair
         r6rs/private/exns
         (for-syntax r6rs/private/check-pattern))

(provide make-variable-transformer
         (rename-out [r6rs:syntax-case syntax-case]
                     [r6rs:syntax syntax])
         _ ...
         identifier?
         bound-identifier=?
         (rename-out [r6rs:free-identifier=? free-identifier=?]
                     [r6rs:datum->syntax datum->syntax]
                     [r6rs:syntax->datum syntax->datum]
                     [r6rs:generate-temporaries generate-temporaries]
                     [r6rs:with-syntax with-syntax]
                     [r6rs:quasisyntax quasisyntax])
         unsyntax unsyntax-splicing
         syntax-violation)

(define syntax-violation 
  (let ([go (lambda (who message form subforms)
              (let ([exn (with-handlers ([exn:fail:syntax? (lambda (x) x)])
                           (apply raise-syntax-error 
                                  (if (string? who)
                                      (string->symbol who)
                                      who)
                                  message
                                  (convert-mpairs form)
                                  (map convert-mpairs subforms)))])
                (raise
                 (make-exn:fail:syntax:r6rs
                  (exn-message exn)
                  (exn-continuation-marks exn)
                  (exn:fail:syntax-exprs exn)
                  message
                  (or who
                      (cond
                       [(identifier? form) (syntax-e form)]
                       [(and (pair? form) (identifier? (car form)))
                        (syntax-e (car form))]
                       [(and (syntax? form) (pair? (syntax-e form))
                             (identifier? (car (syntax-e form))))
                        (syntax-e (car (syntax-e form)))]
                       [else #f]))
                  form
                  (and (pair? subforms) (car subforms))))))])
    (case-lambda
     [(who message form subform)
      (go who message form (list subform))]
     [(who message form)
      (go who message form null)])))

(define (r6rs:free-identifier=? a b)
  (free-identifier=? a b))

(define (r6rs:datum->syntax id datum)
  (unless (identifier? id)
    (raise-type-error 'datum->syntax "identifier?" id))
  (let loop ([d datum])
    (cond
     [(syntax? d) (raise-type-error
                   'datum->syntax
                   "datum"
                   datum)]
     [(pair? d) (loop (car d)) (loop (cdr d))]
     [(mpair? d) (loop (mcar d)) (loop (mcdr d))]
     [(vector? d) (for-each loop (vector->list d))]))
  (datum->syntax id (convert-mpairs datum)))

(define (r6rs:syntax->datum orig-stx)
  (let loop ([stx orig-stx])
    (cond
     [(syntax? stx)
      (convert-pairs (syntax->datum stx))]
     [(mpair? stx) (mcons (loop (mcar stx))
                          (loop (mcdr stx)))]
     [(vector? stx) (list->vector
                     (map loop (vector->list stx)))]
     [(symbol? stx) (raise-type-error 
                     'syntax->datum 
                     (format "syntax (symbol '~s disallowed)" stx)
                     orig-stx)]
     [else stx])))

(define (r6rs:generate-temporaries l)
  (list->mlist
   (generate-temporaries (let loop ([l l])
                           (cond
                            [(null? l) null]
                            [(mpair? l) (cons (mcar l)
                                              (loop (mcdr l)))]
                            [(syntax? l) (loop (syntax-e l))]
                            [(pair? l) (cons (car l)
                                             (loop (cdr l)))]
                            [else
                             (raise-type-error
                              'generate-temporaries
                              "list or list-structured syntax object"
                              l)])))))

(define (make-variable-transformer proc)
  (make-set!-transformer proc))

(define unwrapped-tag (gensym))

(define (wrap expr)
  (datum->syntax #f
                 (convert-mpairs expr)
                 (list unwrapped-tag #f #f #f #f)))

(define (convert-mpairs expr)
  (cond
   [(mpair? expr)
    (cons (convert-mpairs (mcar expr))
          (convert-mpairs (mcdr expr)))]
   [(vector? expr)
    (list->vector (map convert-mpairs (vector->list expr)))]
   [else expr]))

(define (convert-pairs expr)
  (cond
   [(pair? expr)
    (mcons (convert-pairs (car expr))
           (convert-pairs (cdr expr)))]
   [(vector? expr)
    (list->vector (map convert-pairs (vector->list expr)))]
   [else expr]))

;; R6RS syntax-case has different wrapping rules than PLT Scheme for
;; the result of `syntax'. We have to recognize pattern variables
;; to unwrap appropriately.

;; Also, R6RS doesn't have (... <tmpl>) quoting in patterns --- only
;; in templates. <<<< FIXME

(define-syntax-parameter pattern-vars null)

(provide pattern-vars)

(define-for-syntax (add-pattern-vars ids)
  (append (syntax->list ids)
          (syntax-parameter-value (quote-syntax pattern-vars))))

;; ----------------------------------------

(define-for-syntax (extract-pattern-ids stx lits)
  (syntax-case stx ()
    [(a . b) (append (extract-pattern-ids #'a lits)
                     (extract-pattern-ids #'b lits))]
    [#(a ...) (apply append
                     (map (lambda (a)
                            (extract-pattern-ids a lits))
                          (syntax->list #'(a ...))))]
    [a
     (identifier? #'a)
     (if (or (ormap (lambda (lit)
                      (free-identifier=? lit #'a))
                    lits)
             (free-identifier=? #'a #'(... ...))
             (free-identifier=? #'a #'_))
         null
         (list #'a))]
    [_ null]))

(define-syntax (r6rs:syntax-case stx)
  (syntax-case stx ()
    [(_ expr (lit ...) clause ...)
     (let ([lits (syntax->list #'(lit ...))])
       (for-each (lambda (lit)
                   (unless (identifier? lit)
                     (raise-syntax-error #f
                                         "bad literal"
                                         stx
                                         lit))
                   (when (free-identifier=? lit #'(... ...))
                     (raise-syntax-error #f
                                         "ellipses cannot be literal"
                                         stx
                                         lit))
                   
                   (when (free-identifier=? lit #'_)
                     (raise-syntax-error #f
                                         "underscore cannot be literal"
                                         stx
                                         lit)))
                 lits)
       (quasisyntax/loc stx
         (syntax-case (wrap expr) (lit ...)
           . #,(map (lambda (clause)
                      (syntax-case clause ()
                        [(pat val)
                         (with-syntax ([pat-ids (extract-pattern-ids #'pat lits)])
                           ((check-pat-ellipses stx) #'pat)
                           #`(pat (syntax-parameterize ([pattern-vars
                                                         (add-pattern-vars #'pat-ids)])
                                    val)))]
                        [(pat fender val)
                         (with-syntax ([pat-ids (extract-pattern-ids #'pat lits)])
                           ((check-pat-ellipses stx) #'pat)
                           #`(pat (syntax-parameterize ([pattern-vars
                                                         (add-pattern-vars #'pat-ids)])
                                    fender)
                                  (syntax-parameterize ([pattern-vars
                                                         (add-pattern-vars #'pat-ids)])
                                    val)))]
                        [else clause]))
                    (syntax->list #'(clause ...))))))]
    [(_ . rest) (syntax/loc stx (syntax-case . rest))]))

;; ----------------------------------------

(define-for-syntax (make-unwrap-map tmpl pattern-vars)
  (let loop ([tmpl tmpl]
             [in-ellipses? #f]
             [counting? #f])
    (syntax-case tmpl ()
      [(ellipses expr)
       (and (not in-ellipses?)
            (identifier? #'ellipses)
            (free-identifier=? #'ellipses #'(... ...)))
       (loop #'expr #t #f)]
      [(expr ellipses . rest)
       (and (not in-ellipses?)
            (identifier? #'ellipses)
            (free-identifier=? #'ellipses #'(... ...)))
       (box (cons (loop #'expr #f #f)
                  (let rloop ([rest #'rest])
                    (syntax-case rest ()
                      [(ellipses . rest)
                       (and (identifier? #'ellipses)
                            (free-identifier=? #'ellipses #'(... ...)))
                       ;; keep going:
                       (rloop #'rest)]
                      [else (loop rest #f #t)]))))]
      [(a . b) (let ([a (loop #'a in-ellipses? #f)]
                     [b (loop #'b in-ellipses? counting?)])
                 (if (or a b counting?)
                     (cons a b)
                     #f))]
      [#(a ...) (let ([as (loop (syntax->list #'(a ...))
                                in-ellipses?
                                #f)])
                  (and as (vector as)))]
      [a
       (identifier? #'a)
       (ormap (lambda (pat-var)
                (free-identifier=? #'a pat-var))
              pattern-vars)]
      [_ #f])))

(define-for-syntax (group-ellipses tmpl umap)
  (define (stx-cdr s) (if (syntax? s) (cdr (syntax-e s)) (cdr s)))
  (let loop ([tmpl tmpl][umap umap])
    (if (not umap)
        tmpl
        (syntax-case tmpl ()
          [(ellipses expr)
           (and (identifier? #'ellipses)
                (free-identifier=? #'ellipses #'(... ...)))
           tmpl]
          [(expr ellipses . rest)
           (and (identifier? #'ellipses)
                (free-identifier=? #'ellipses #'(... ...)))
           (let rloop ([rest (stx-cdr (stx-cdr tmpl))]
                       [accum (list #'ellipses (loop #'expr
                                                     (car (unbox umap))))])
             (syntax-case rest ()
               [(ellipses . _)
                (and (identifier? #'ellipses)
                     (free-identifier=? #'ellipses #'(... ...)))
                ;; keep going:
                (rloop (stx-cdr rest) (cons #'ellipses accum))]
               [_ (cons (datum->syntax #f (reverse accum))
                        (loop rest (cdr (unbox umap))))]))]
          [(a . b) (let ([n (cons (loop #'a (car umap))
                                  (loop (cdr (if (syntax? tmpl)
                                                 (syntax-e tmpl)
                                                 tmpl))
                                        (cdr umap)))])
                     (if (syntax? tmpl)
                         (datum->syntax tmpl n tmpl tmpl tmpl)
                         n))]
          [#(a ...) (datum->syntax 
                     tmpl
                     (list->vector (loop (syntax->list #'(a ...))
                                         (vector-ref umap 0)))
                     tmpl
                     tmpl
                     tmpl)]
          [_ tmpl]))))

(define (unwrap stx mapping)
  (cond
   [(not mapping)
    ;; In case stx is a pair, explicitly convert
    (datum->syntax #f (convert-mpairs stx))]
   [(eq? mapping #t)
    ;; was a pattern var; unwrap based on srcloc:
    (let loop ([v stx])
      (cond
       [(syntax? v)
        (if (eq? (syntax-source v) unwrapped-tag)
            (loop (syntax-e v))
            v)]
       [(pair? v) (mcons (loop (car v))
                         (loop (cdr v)))]
       [(vector? v) (list->vector
                     (map loop (vector->list v)))]
       [else v]))]
   [(pair? mapping)
    (let ([p (if (syntax? stx)
                 (syntax-e stx)
                 stx)])
      (mcons (unwrap (car p) (car mapping))
             (unwrap (cdr p) (cdr mapping))))]
   [(vector? mapping)
    (list->vector (let loop ([v (unwrap (vector->list (syntax-e stx))
                                        (vector-ref mapping 0))])
                    (cond
                     [(null? v) null]
                     [(mpair? v) (cons (mcar v) (loop (mcdr v)))]
                     [(syntax? v) (syntax->list v)])))]
   [(null? mapping) null]
   [(box? mapping)
    ;; ellipses
    (let* ([mapping (unbox mapping)]
           [rest-mapping (cdr mapping)]
           [p (if (syntax? stx) (syntax-e stx) stx)]
           [repeat-stx (car p)]
           [rest-stx (cdr p)])
      (let ([repeats (list->mlist
                      (map (lambda (rep)
                             (unwrap rep (car mapping)))
                           (syntax->list repeat-stx)))]
            [rest-mapping 
             ;; collapse #fs to single #f:
             (if (let loop ([rest-mapping rest-mapping])
                   (if (pair? rest-mapping)
                       (if (not (car rest-mapping))
                           (loop (cdr rest-mapping))
                           #f)
                       (not rest-mapping)))
                 #f
                 rest-mapping)])
                                        
        (if (and (not rest-mapping)
                 (or (null? rest-stx)
                     (and (syntax? rest-stx)
                          (null? (syntax-e rest-stx)))))
            repeats
            (mappend repeats
                     (unwrap rest-stx rest-mapping)))))]
   [else (error 'unwrap "strange unwrap mapping: ~e" mapping)]))

(define-syntax (r6rs:syntax stx)
  (syntax-case stx ()
    [(_ tmpl)
     (let ([umap (make-unwrap-map #'tmpl
                                  (syntax-parameter-value #'pattern-vars))])
       (quasisyntax/loc stx
         (unwrap (if #f
                     ;; Process tmpl first, so that syntax errors are reported
                     ;; usinf the original source.
                     #,(syntax/loc stx (syntax tmpl))
                     ;; Convert tmpl to group ...-created repetitions together,
                     ;;  so that `unwrap' can tell which result came from which
                     ;;  template:
                     #,(with-syntax ([tmpl (group-ellipses #'tmpl umap)])
                         (syntax/loc stx (syntax tmpl))))
                 '#,umap)))]
    [(_ . rest) (syntax/loc stx (syntax . rest))]))

;; ----------------------------------------

;; Implementation from R6RS --- which gives the following
;; strange behavior:
;;
;;  > (with-syntax ([a 10][... 11]) #'(a ...))
;;  (10 11)

(define-syntax r6rs:with-syntax
  (syntax-rules ()
    [(_ [(p e0) ...] e1 e2 ...)
     (r6rs:syntax-case (mlist e0 ...) ()
       [(p ...) (let () e1 e2 ...)])]))

(define-syntax (r6rs:quasisyntax stx)
  (syntax-case stx ()
    [(_ tmpl)
     (let loop ([stx #'tmpl]
                [src stx]
                [depth 0]
                [to-splice? #f]
                [k (lambda (template pats exprs)
                     (with-syntax ([(pat ...) pats]
                                   [(expr ...) exprs]
                                   [template template])
                       (syntax/loc stx
                         (r6rs:with-syntax ([pat expr] ...)
                                           (r6rs:syntax template)))))])
       (cond
        [(and (identifier? stx)
              (or (free-identifier=? #'unsyntax stx)
                  (free-identifier=? #'unsyntax-splicing stx)))
         (raise-syntax-error #f
                             "misplaced within quasitemplate"
                             stx)]
        [(syntax? stx)
         (loop (syntax-e stx)
               stx
               depth
               to-splice?
               (lambda (t pats exprs)
                 (k (if to-splice?
                        (map (lambda (t)
                               (datum->syntax stx t stx stx))
                             t)
                        (datum->syntax stx t stx stx))
                    pats
                    exprs)))]
        [(pair? stx)
         (cond
          [(and (identifier? (car stx))
                (or (free-identifier=? #'unsyntax (car stx))
                    (free-identifier=? #'unsyntax-splicing (car stx))))
           (let ([l (syntax->list (datum->syntax #f (cdr stx)))]
                 [splice? (free-identifier=? #'unsyntax-splicing (car stx))])
             (unless l
               (raise-syntax-error #f
                                   "bad syntax"
                                   (datum->syntax src stx src src)))
             (if (zero? depth)
                 ;; Escape:
                 (let ([id (car (generate-temporaries '(un)))])
                   (when (or splice? (not (= 1 (length l))))
                     (unless to-splice?
                       (raise-syntax-error #f
                                           "not in a splicing context"
                                           (datum->syntax src stx src src))))
                   (if (= (length l) 1)
                       ;; Normal substitution:
                       (k (if to-splice? 
                              (if splice?
                                  (list id (quote-syntax ...))
                                  (list id))
                              id)
                          (list (if splice?
                                    (list id (quote-syntax ...))
                                    id))
                          (list (car l)))
                       ;; Splicing (or double-splicing) substitution:
                       (k (if splice?
                              (list id (quote-syntax ...) (quote-syntax ...))
                              (list id (quote-syntax ...)))
                          (list
                           (if splice?
                               (list (list id (quote-syntax ...)) (quote-syntax ...))
                               (list id (quote-syntax ...))))
                          (list (if splice?
                                    #`(map convert-mpairs (list . #,(cdr stx)))
                                    #`(list . #,(cdr stx)))))))
                 ;; Not an escape -- just decrement depth:
                 (loop (cdr stx)
                       src
                       (sub1 depth)
                       #f
                       (lambda (t pats exprs)
                         (k (let ([v (cons (car stx) t)])
                              (if to-splice?
                                  (list v)
                                  v))
                            pats
                            exprs)))))]
          [(and (identifier? (car stx))
                (free-identifier=? #'r6rs:quasisyntax (car stx)))
           (loop (cdr stx)
                 src
                 (add1 depth)
                 #f
                 (lambda (t pats exprs)
                   (k (let ([v (cons (car stx) t)])
                        (if to-splice?
                            (list v)
                            v))
                      pats
                      exprs)))]
          [else
           ;; a pair
           (loop (car stx)
                 src
                 depth
                 #t
                 (lambda (a a-pats a-exprs)
                   (loop (cdr stx)
                         src
                         depth
                         #f
                         (lambda (b b-pats b-exprs)
                           (k (let ([v (append a b)])
                                (if to-splice?
                                    (list v)
                                    v))
                              (append a-pats b-pats)
                              (append a-exprs b-exprs))))))])]
        [(vector? stx)
         (loop (vector->list stx)
               src
               depth
               #f
               (lambda (t pats exprs)
                 (k (let ([v (list->vector t)])
                      (if to-splice?
                          (list v)
                          v))
                    pats
                    exprs)))]
        [else (k (if to-splice? (list stx) stx) null null)]))]))
