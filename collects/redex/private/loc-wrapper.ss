#lang scheme/base

(require (lib "etc.ss")
         "term.ss"
         scheme/contract)
(require (for-syntax "term-fn.ss" scheme/base))

(define (init-loc-wrapper e line column quoted?)
  (make-lw e line #f column #f (not quoted?) #f))

;; lw = (union 'spring loc-wrapper)

;; e : (union string symbol #f (listof lw))
;; line, line-span, column, column-span : number
(define-struct lw (e line line-span column column-span unq? metafunction?) 
  #:mutable
  #:inspector (make-inspector))

;; build-lw is designed for external consumption
(define (build-lw e line line-span column column-span)
  (make-lw e line line-span column column-span #f #f))

(define curly-quotes-for-strings (make-parameter #t))

(define (rewrite-quotes s)
  (if (curly-quotes-for-strings)
      (string-append "“"
                     (substring s 1 (- (string-length s) 1))
                     "”")
      s))

(define-syntax-set (to-lw to-lw/uq)
  (define (process-arg stx quote-depth)
    (define quoted? (quote-depth . > . 0))
    (define-values (op cl)
      (if (syntax? stx)
          (case (syntax-property stx 'paren-shape)
            [(#\{) (values "{" "}")]
            [(#\[) (values "[" "]")]
            [else (values "(" ")")])
          (values #f #f)))
    (define (reader-shorthand arg qd-delta mrk)
      #`(init-loc-wrapper 
         (list (init-loc-wrapper #,mrk
                                 #,(syntax-line stx)
                                 #,(syntax-column stx)
                                 #,quoted?)
               'spring
               #,(process-arg arg (+ quote-depth qd-delta)))
         #,(syntax-line stx) 
         #,(syntax-column stx)
         #,quoted?))
    (define (handle-sequence qd-delta)
      #`(init-loc-wrapper 
         (list (init-loc-wrapper #,op #,(syntax-line stx) #,(syntax-column stx) #,quoted?)
               #,@(map (λ (x) (process-arg x (+ qd-delta quote-depth))) (syntax->list stx))
               (init-loc-wrapper #,cl #f #f #,quoted?))
         #,(syntax-line stx) 
         #,(syntax-column stx)
         #,quoted?))
    (syntax-case* stx (name unquote quote unquote-splicing term) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
      ['a (reader-shorthand #'a +1 (if (= quote-depth 0) "" "'"))]
      [,a (reader-shorthand #'a -1 (if (= quote-depth 1) "" ","))]
      [,@a (reader-shorthand #'a -1 (if (= quote-depth 1) "" ",@"))]
      [(term a)
       (if (= quote-depth 0)
           #`(init-loc-wrapper 
              (list (init-loc-wrapper "" #,(syntax-line stx) #,(syntax-column stx) #,quoted?)
                    'spring
                    #,(process-arg (cadr (syntax->list stx)) (+ quote-depth 1)))
              #,(syntax-line stx) 
              #,(syntax-column stx)
              #,quoted?)
           (handle-sequence +1))]
      [(a ...)
       (handle-sequence 0)]
      [(a b ... . c)
       #`(init-loc-wrapper 
          (list (init-loc-wrapper #,op #,(syntax-line stx) #,(syntax-column stx) #,quoted?)
                #,@(map (λ (x) (process-arg x quote-depth)) (syntax->list (syntax (a b ...))))
                (init-loc-wrapper #," . " #f #f #,quoted?)
                #,(process-arg #'c quote-depth)
                (init-loc-wrapper #,cl #f #f #,quoted?))
          #,(syntax-line stx) 
          #,(syntax-column stx)
          #,quoted?)]
      [x 
       (and (identifier? #'x)
            (term-fn? (syntax-local-value #'x (λ () #f))))
       #`(make-lw
          '#,(syntax-e #'x)
          #,(syntax-line stx) 
          #f
          #,(syntax-column stx)
          #f
          #f
          #t)]
      [x 
       (identifier? #'x)
       #`(init-loc-wrapper 
          '#,(syntax-e #'x)
          #,(syntax-line stx) 
          #,(syntax-column stx)
          #,quoted?)]
      [x 
       #`(init-loc-wrapper 
          #,(let ([base (syntax-e #'x)])
              (if (string? base)
                  #`(rewrite-quotes #,(format "~s" base))
                  (format "~s" (syntax-e #'x))))
          #,(syntax-line stx) 
          #,(syntax-column stx)
          #,quoted?)]))
  
  (define (to-lw/proc stx)
    (syntax-case stx ()
      [(_ stx)
       #`(add-spans #,(process-arg #'stx 1))]))
  (define (to-lw/uq/proc stx)
    (syntax-case stx ()
      [(_ stx)
       #`(add-spans #,(process-arg #'stx 0))])))

(define (add-spans lw)
  (define (add-spans/lw lw line col)
    (cond
      [(eq? lw 'spring) (values line col col)]
      [else
       (let ([start-line (or (lw-line lw) line)]
             [start-column (or (lw-column lw) col)])
         (when (and start-line  ;; if we don't have src loc info, just give up.
                    start-column)
           (let-values ([(last-line first-column last-column)
                         (add-spans/obj (lw-e lw) start-line start-column)])
             (unless (lw-line lw)
               (set-lw-line! lw line))
             (set-lw-line-span! lw (- last-line start-line))
             
             (unless (lw-column lw)
               (set-lw-column! lw col))
             (let ([new-col (min (lw-column lw)
                                 first-column)])
               (set-lw-column! lw new-col)
               (set-lw-column-span! lw (- last-column new-col)))
             
             (values last-line first-column last-column))))]))
  (define (add-spans/obj e line col)
    (cond
      [(string? e) 
       (values line col (+ col (string-length e)))]
      [(symbol? e)
       (values line col (+ col (string-length (symbol->string e))))]
      [(not e) (values line col col)]
      [else 
       (let loop ([lws e]
                  [line line]
                  [first-column col]
                  [last-column col]
                  [current-col col])
         (cond
           [(null? lws) (values line first-column last-column)]
           [else 
            (let-values ([(last-line inner-first-column inner-last-column)
                          (add-spans/lw (car lws) line current-col)])
              (if (= last-line line)
                  (loop (cdr lws)
                        last-line
                        (min inner-first-column first-column)
                        (max inner-last-column last-column)
                        inner-last-column)
                  (loop (cdr lws)
                        last-line
                        (min inner-first-column first-column)
                        inner-last-column
                        inner-last-column)))]))]))
  
  (add-spans/lw lw #f #f)
  lw)

(define pnum (and/c number? (or/c zero? positive?)))

(provide/contract
 (struct lw ((e any/c)
             (line pnum)
             (line-span pnum)
             (column pnum)
             (column-span pnum)
             (unq? boolean?)
             (metafunction? boolean?)))
 [build-lw (-> any/c pnum pnum pnum pnum lw?)])

(provide to-lw
         to-lw/uq
         curly-quotes-for-strings)
