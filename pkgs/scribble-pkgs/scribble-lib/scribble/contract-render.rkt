#lang racket/base
(require racket/class racket/match
         (prefix-in text: "text-render.rkt")
         "base-render.rkt"
         "core.rkt"
         file/convertible
         racket/serialize)
(provide override-render-mixin-single
         override-render-mixin-multi)

(define (override-render-mixin multi?)
  (mixin (render<%>) ()
    (super-new)
    (define/override (render srcs dests ri)
      (super render srcs dests ri)
      
      (for ([part (in-list srcs)]
            [dest (in-list dests)])
        (define p (open-output-string))
        (define index-table (make-hash))
        (port-count-lines! p)
        (parameterize ([the-renderer text-renderer]
                       [the-part part]
                       [the-ri ri]
                       [the-text-p p])
          (r-part part 'block index-table))
        (define table-str (format "~s\n" (serialize index-table)))
        (define cb.rktd 
          (cond
            [multi?
             (build-path dest "blueboxes.rktd")]
            [else
             (define-values (base name dir?) (split-path dest))
             (build-path base "blueboxes.rktd")]))
        (call-with-output-file cb.rktd
          (λ (port)
            (fprintf port "~a\n" (string-utf-8-length table-str))
            (display table-str port)
            (display (get-output-string p) port))
          #:exists 'truncate)))
    
    (inherit get-dest-directory)
    (define text-renderer (new (text:render-mixin render%)
                               [dest-dir (get-dest-directory)]))))

(define the-renderer (make-parameter #f))
(define the-part (make-parameter #f))
(define the-ri (make-parameter #f))
(define the-text-p (make-parameter #f))

;; mode is either
;;     'block -- search for the blue blocks
;;  or (cons number number) -- search for tags in a block
(define (r-parts parts mode index-table)
  (for ([part (in-list parts)])
    (r-part part mode index-table)))

(define (r-part part mode index-table)
  (r-blocks (part-blocks part) mode index-table)
  (r-parts (part-parts part) mode index-table))

(define (r-blocks blocks mode index-table)
  (for ([block (in-list blocks)])
    (r-block block mode index-table)))

(define (r-block block mode index-table)
  (match block
    [(struct nested-flow (style blocks))
     (check-and-continue style block mode index-table r-blocks blocks)]
    [(struct compound-paragraph (style blocks)) 
     (check-and-continue style block mode index-table r-blocks blocks)]
    [(paragraph style content)
     (check-and-continue style block mode index-table r-content content)]
    [(itemization style blockss)
     (check-and-continue style block mode index-table r-blockss blockss)]
    [(table style cells)
     (check-and-continue style block mode index-table r-blockss+cont cells)]
    [(delayed-block resolve) 
     (r-block (delayed-block-blocks block (the-ri)) mode index-table)]))

(define (check-and-continue style block mode index-table sub-f sub-p)
  (cond
    [(and (pair? mode) (equal? (style-name style) "RBackgroundLabelInner"))
     (define background-label-port (car mode))
     (parameterize ([current-output-port background-label-port])
       (send (the-renderer) render-block block (the-part) (the-ri) #f))
     (sub-f sub-p mode index-table)]
    [(and (eq? mode 'block) (eq? (style-name style) 'boxed) (table? block))
     (cond
       [(for/and ([cells (in-list (table-blockss block))])
          (and (not (null? cells))
               (null? (cdr cells))
               (let ([fst (car cells)])
                 (and (table? fst)
                      (equal? (style-name (table-style fst)) "together")))))
        (for ([cells (in-list (table-blockss block))])
          (handle-one-block style (car cells) mode index-table r-block (car cells)))]
       [else 
        (handle-one-block style block mode index-table sub-f sub-p)])]
    [else
     (sub-f sub-p mode index-table)]))

(define (handle-one-block style block mode index-table sub-f sub-p)
  ;(printf "-----\n") ((dynamic-require 'racket/pretty 'pretty-write) block)
  (define block-port (open-output-string))
  (define background-label-port (open-output-string))
  (define ents (make-hash))
  (define new-mode (cons background-label-port ents))
  (port-count-lines! block-port)
  (port-count-lines! background-label-port)
  (parameterize ([current-output-port block-port])
    (send (the-renderer) render-block block (the-part) (the-ri) #f))
  (sub-f sub-p new-mode index-table)
  
  ;; we just take the first one here
  (define background-label-p (open-input-string (get-output-string background-label-port)))
  (define background-label-line (read-line background-label-p))
  
  (define text-p (the-text-p))
  (define-values (before-line _1 _2) (port-next-location text-p))
  (define before-position (file-position text-p))
  (fprintf text-p "~a\n"
           (if (eof-object? background-label-line)
               ""
               background-label-line))
  
  ;; dump content of block-port into text-p, but first trim 
  ;; the spaces that appear at the ends of the lines
  (let ([p (open-input-string (get-output-string block-port))])
    (let loop ()
      (define l (read-line p))
      (unless (eof-object? l)
        (display (regexp-replace #rx" *$" l "") text-p)
        (newline text-p)
        (loop))))
  
  (define-values (after-line _3 _4) (port-next-location text-p))
  (define txt-loc (cons before-position (- after-line before-line)))
  (define ri (the-ri))
  (for ([(k v) (in-hash ents)])
    (let ([k (tag-key k ri)])
      (hash-set! index-table k (cons txt-loc (hash-ref index-table k '()))))))

(define (r-blockss+cont blockss mode index-table)
  (for ([blocks (in-list blockss)])
    (for ([block (in-list blocks)])
      (unless (eq? block 'cont)
        (r-block block mode index-table)))))

(define (r-blockss blockss mode index-table)
  (for ([blocks (in-list blockss)])
    (r-blocks blocks mode index-table)))

(define (r-content content mode index-table)
  (cond
    [(element? content) (r-element content mode index-table)]
    [(list? content)
     (for ([content (in-list content)])
       (r-content content mode index-table))]
    [(string? content) (void)]
    [(symbol? content) (void)]
    [(convertible? content) (void)]
    [(delayed-element? content)
     (r-content (delayed-element-content content (the-ri)) mode index-table)]
    [(traverse-element? content)
     (r-content (traverse-element-content content (the-ri)) mode index-table)]
    [(part-relative-element? content) 
     (r-content (part-relative-element-content content (the-ri)) mode index-table)]
    [(multiarg-element? content)
     (r-content (multiarg-element-contents content) mode index-table)]
    [else (error 'r-content "unknown content: ~s\n" content)]))

(define (r-element element mode index-table)
  (when (index-element? element)
    (when (pair? mode)
      (define ents (cdr mode))
      (define key (index-element-tag element))
      (hash-set! ents (tag-key key (the-ri)) #t)))
  (r-content (element-content element) mode index-table))


(define override-render-mixin-multi (override-render-mixin #t))
(define override-render-mixin-single (override-render-mixin #f))
