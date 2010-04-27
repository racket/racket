#lang scheme/base

;; this is the runtime code for loc-wrapper-ct.ss.
;; it isn't really its own module, but separated
;; out in order to get the phases right.
(provide (all-defined-out))

(require (lib "etc.ss")
         "term.ss")

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


(define (add-spans lw)
  (define line-seen-so-far 0)
  
  (define (add-spans/lw lw line col)
    (cond
      [(eq? lw 'spring) (values line col col)]
      [else
       (let ([start-line (or (lw-line lw) line line-seen-so-far)]
             [start-column (or (lw-column lw) col 0)])
         (set! line-seen-so-far (max line-seen-so-far start-line))
         (unless (lw-line lw) (set-lw-line! lw line-seen-so-far))
         (unless (lw-column lw) (set-lw-column! lw start-column))
         (let-values ([(last-line first-column last-column)
                       (add-spans/obj (lw-e lw) start-line start-column)])
           (set-lw-line-span! lw (- last-line start-line))
           (let ([new-col (min/f (lw-column lw)
                                 first-column)])
             (set-lw-column! lw new-col)
             (set-lw-column-span! lw (- last-column new-col)))
           (values last-line first-column last-column)))]))
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

(define (min/f a b)
  (cond
    [(and a b) (min a b)]
    [a a]
    [b b]
    [else 0]))


