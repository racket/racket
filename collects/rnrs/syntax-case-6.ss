#lang scheme/base

(require (for-syntax scheme/base)
         r6rs/private/qq-gen
         scheme/stxparam
         scheme/mpair)

(provide make-variable-transformer
         (rename-out [r6rs:syntax-case syntax-case]
                     [r6rs:syntax syntax])
         _ ...
         identifier?
         bound-identifier=?
         (rename-out [r6rs:free-identifier=? free-identifier=?]
                     [r6rs:datum->syntax datum->syntax]
                     [r6rs:syntax->datum syntax->datum])
         generate-temporaries
         (rename-out [r6rs:with-syntax with-syntax]
                     [r6rs:quasisyntax quasisyntax])
         unsyntax unsyntax-splicing
         (rename-out [raise-syntax-error syntax-violation]))

(define (r6rs:free-identifier=? a b)
  (free-identifier=? a a))

(define (r6rs:datum->syntax id datum)
  (unless (identifier? id)
    (raise-type-error 'datum->syntax "identifier?" id))
  (datum->syntax id (convert-mpairs datum)))

(define (r6rs:syntax->datum stx)
  (convert-pairs (syntax->datum stx)))

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
                           #`(pat (syntax-parameterize ([pattern-vars
                                                         (add-pattern-vars #'pat-ids)])
                                    val)))]
                        [(pat fender val)
                         (with-syntax ([pat-ids (extract-pattern-ids #'pat lits)])
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
                  (loop #'rest #f #t)))]
      [(a . b) (let ([a (loop #'a in-ellipses? #f)]
                     [b (loop #'b in-ellipses? counting?)])
                 (if (or a b counting?)
                     (cons a b)
                     #f))]
      [#(a ...) (let ([as (map (lambda (a)
                                 (loop a in-ellipses? #f))
                               (syntax->list #'(a ...)))])
                  (if (ormap values as)
                      (list->vector as)
                      #f))]
      [a
       (identifier? #'a)
       (ormap (lambda (pat-var)
                (free-identifier=? #'a pat-var))
              pattern-vars)]
      [_ #f])))

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
    (list->vector (unwrap (vector->list (syntax-e stx)) (vector->list mapping)))]
   [(box? mapping)
    ;; ellipses
    (let* ([mapping (unbox mapping)]
           [rest-mapping (cdr mapping)]
           [rest-size 
            ;; count number of cons cells we need at the end:
            (let loop ([m rest-mapping])
              (if (pair? m)
                  (add1 (loop (cdr m)))
                  0))]
           [repeat-stx (reverse
                        (list-tail (let loop ([stx stx][accum null])
                                     (let ([p (if (syntax? stx)
                                                  (syntax-e stx)
                                                  stx)])
                                       (if (pair? p)
                                           (loop (cdr p) (cons (car p) accum))
                                           accum)))
                                   rest-size))]
           [rest-stx (let loop ([stx stx][size (length repeat-stx)])
                       (if (zero? size)
                           stx
                           (let ([p (if (syntax? stx)
                                        (syntax-e stx)
                                        stx)])
                             (loop (cdr p) (sub1 size)))))])
      (let ([repeats (list->mlist
                      (map (lambda (rep)
                             (unwrap rep (car mapping)))
                           repeat-stx))]
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
   [else (error 'unwrap "srtange unwrap mapping: ~e" mapping)]))

(define-syntax (r6rs:syntax stx)
  (syntax-case stx ()
    [(_ tmpl)
     (quasisyntax/loc stx
       (unwrap #,(syntax/loc stx (syntax tmpl))
               '#,(make-unwrap-map #'tmpl
                                   (syntax-parameter-value #'pattern-vars))))]
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

(define-generalized-qq r6rs:quasisyntax
  quasisyntax unsyntax unsyntax-splicing convert-mpairs)
