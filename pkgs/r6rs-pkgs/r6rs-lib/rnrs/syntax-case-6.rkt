#lang scheme/base

(require (for-syntax scheme/base)
         r6rs/private/qq-gen
         r6rs/private/reconstruct
         scheme/mpair
         r6rs/private/exns
         (for-syntax syntax/template
                     r6rs/private/check-pattern)
         (for-template (only-in scheme/base set!)
                       r6rs/private/no-set))

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
  (make-set!-transformer
   (lambda (stx)
     (syntax-case* stx (set!) free-template-identifier=?
       [(set! . rest)
        (proc (syntax/loc stx (r6rs:set! . rest)))]
       [else (proc stx)]))))

(define unwrapped-tag (gensym))
(define unwrapped-srcloc (list unwrapped-tag #f #f #f #f))

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
         (syntax-case (add-wrap expr) (lit ...)
           . #,(map (lambda (clause)
                      (syntax-case clause ()
                        [(pat val)
                         (begin
                           ((check-pat-ellipses stx) #'pat)
                           #`(pat val))]
                        [(pat fender val)
                         (begin
                           ((check-pat-ellipses stx) #'pat)
                           #`(pat fender val))]
                        [else clause]))
                    (syntax->list #'(clause ...))))))]
    [(_ . rest) (syntax/loc stx (syntax-case . rest))]))

(define-syntax (add-wrap stx)
  (syntax-case stx ()
    [(_ expr)
     #`(wrap expr (quote-syntax #,(datum->syntax #'expr 'ctx)) unwrapped-srcloc #f)]))

;; ----------------------------------------

(define (unwrap-reconstructed data stx datum)
  (when (mpair? datum)
    (hash-set! reconstruction-memory datum (datum->syntax stx 'memory stx)))
  datum)

(define (unwrap-pvar data stx)
  ;; unwrap based on srcloc:
  (let loop ([v stx])
    (cond
     [(syntax? v)
      (if (eq? (syntax-source v) unwrapped-tag)
          (let ([r (loop (syntax-e v))])
            (when (mpair? r)
              (hash-set! reconstruction-memory r (datum->syntax v 'memory v)))
            r)
          v)]
     [(pair? v) (mcons (loop (car v))
                       (loop (cdr v)))]
     [(vector? v) (list->vector
                   (map loop (vector->list v)))]
     [else v])))

(define (leaf-to-syntax datum)
  (datum->syntax #f datum))

(define (ellipses-end stx)
  ;; R6RS says that (x ...) must be a list, so we need a special rule
  (if (and (syntax? stx) (null? (syntax-e stx)))
      null
      stx))

(define-for-syntax (no-data x) #f)

(define-syntax (r6rs:syntax stx)
  (syntax-case stx ()
    [(_ template)
     (transform-template #'template
                         #:constant-as-leaf? #t
                         #:save (lambda (x) #f)
                         #:restore-stx #'unwrap-reconstructed
                         #:leaf-datum-stx #'leaf-to-syntax
                         #:pvar-restore-stx #'unwrap-pvar
                         #:cons-stx #'mcons
                         #:ellipses-end-stx #'ellipses-end)]))

;; ----------------------------------------

;; Implementation from R6RS --- which gives the following
;; strange behavior:
;;
;;  > (with-syntax ([a 10][... 11]) #'(a ...))
;;  (10 11)

(define-syntax r6rs:with-syntax
  (syntax-rules ()
    [(_ [(p e0) ...] e1 e2 ...)
     (r6rs:syntax-case (mlist (add-wrap e0) ...) ()
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
