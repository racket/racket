#lang racket/base
(require (for-syntax racket/base)
         syntax/stx)
(provide stx-disarm
         stx-car*
         stx-cdr*
         syntax-e*
         stx->list*
         stx->datum
         syntaxish?
         syntax-copier)

;; Update for syntax taint: On get, disarm stx on the way, but don't
;; disarm final stx. On replace, disarm and rearm along the way.

(define (stx-disarm stx)
  (if (syntax? stx) (syntax-disarm stx (current-code-inspector)) stx))

(define (stx-car* stx)
  (let ([stx (stx-disarm stx)]) (stx-car stx)))

(define (stx-cdr* stx)
  (let ([stx (stx-disarm stx)]) (stx-cdr stx)))

(define (syntax-e* stx)
  (syntax-e (stx-disarm stx)))

(define (stx->list* stx)
  (if (stx-list? stx)
      (let loop ([stx stx])
        (cond [(syntax? stx)
               (loop (syntax-e* stx))]
              [(pair? stx)
               (cons (car stx) (loop (cdr stx)))]
              [else stx]))
      #f))

;; ----

(define (stx->datum x)
  (syntax->datum (datum->syntax #f x)))

(define (syntaxish? x)
  (or (syntax? x)
      (null? x)
      (and (pair? x)
           (syntaxish? (car x))
           (syntaxish? (cdr x)))))

;; ----

(define-syntax (syntax-copier stx)
  (syntax-case stx ()
    [(syntax-copier hole expr pattern)
     #'(let ([expr-var expr])
         (lambda (in-the-hole)
           (with-syntax ([pattern expr-var])
             (with-syntax ([hole in-the-hole])
               (syntax/restamp pattern #'pattern expr-var)))))]))

(define-syntax syntax/skeleton
  (syntax-rules ()
    [(syntax/skeleton old-expr pattern)
     (syntax/restamp pattern #'pattern old-expr)]))

;; FIXME: Need to avoid turning syntax lists into syntax pairs
(define-syntax (syntax/restamp stx)
  (syntax-case stx (...)
    [(syntax/restamp (pa (... ...)) new-expr old-expr)
     #`(let ([new-parts (stx->list new-expr)]
             [old-parts (stx->list old-expr)])
         ;; FIXME 
         (unless (= (length new-parts) (length old-parts))
           (printf "** syntax/restamp\n~s\n" (quote-syntax #,stx))
           (printf "pattern : ~s\n" (syntax->datum #'(pa (... ...))))
           (printf "old parts: ~s\n" (map syntax->datum old-parts))
           (printf "new parts: ~s\n" (map syntax->datum new-parts)))
         (d->so
          old-expr
          (map (lambda (new old) (syntax/restamp pa new old))
               new-parts
               old-parts)))]
    [(syntax/restamp (pa . pb) new-expr old-expr)
     ;; FIXME 
     #'(begin
         (unless (and (stx-pair? new-expr) (stx-pair? old-expr))
           (printf "** syntax/restamp\n~s\n" (quote-syntax #,stx))
           (printf "pattern : ~s\n" (syntax->datum (quote-syntax (pa . pb))))
           (printf "old parts: ~s\n" old-expr)
           (printf "new parts: ~s\n" new-expr))
         (let ([na (stx-car new-expr)]
               [nb (stx-cdr new-expr)]
               [oa (stx-car old-expr)]
               [ob (stx-cdr old-expr)])
           (d->so old-expr
                  (cons (syntax/restamp pa na oa)
                        (syntax/restamp pb nb ob)))))]
    [(syntax/restamp pvar new-expr old-expr)
     #'new-expr]))

(define (d->so template datum)
  (if (syntax? template)
      (datum->syntax template datum template template)
      datum))
