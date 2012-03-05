#lang racket/base
(require racket/gui/base
         racket/contract
         mrlib/graph
         racket/pretty
         racket/class
         framework
         "size-snip.rkt")

(provide show-differences find-differences)

(define (all-but-last l)
  (let loop ([l l])
    (cond
      [(null? (cdr l)) null]
      [else (cons (car l) (loop (cdr l)))])))

(define (record-differences sexp1 sexp2)
  (let ([ht (make-hasheq)])
    ;; loop's result indicates if the sexps are different
    (let loop ([sexp1 sexp1]
               [sexp2 sexp2])
      (cond
        [(eq? sexp1 sexp2) #f]
        [(and (pair? sexp1)
              (pair? sexp2)
              (equal? (d-length sexp1)
                      (d-length sexp2)))
         (let ([subs-same (map/d loop sexp1 sexp2)])
           (if (and (andmap values subs-same)
                    (not (= 1 (d-length sexp1))))
               (begin
                 (hash-set! ht sexp1 #t)
                 (hash-set! ht sexp2 #t)
                 #t)
               #f))]
        [(equal? sexp1 sexp2) #f]
        [else 
         (hash-set! ht sexp1 #t)
         (hash-set! ht sexp2 #t)
         #t]))
    ht))

(define (unwrap s)
  (cond
    [(pair? s) (cons (unwrap (car s))
                     (unwrap (cdr s)))]
    [(wrap? s) (wrap-content s)]
    [else s]))

(define (unkink s)
  (let loop ([s s])
    (cond
      [(pair? s) (cons (loop (car s))
                       (if (null? (cdr s))
                           '()
                           (loop (cdr s))))]
      [(vector? s)
       (list->vector (map loop (vector->list s)))]
      [(box? s)
       (box (loop (unbox s)))]
      [(syntax? s) (datum->syntax s (unkink (loop (syntax-e s))) s)]
      [(number? s) (make-wrap s)]
      [(symbol? s) (make-wrap s)]
      [(null? s) (make-wrap s)]
      [(boolean? s) (make-wrap s)]
      [(string? s) (make-wrap s)]
      [(bytes? s) (make-wrap s)]
      [(regexp? s) (make-wrap s)]
      [else s])))

(define-struct wrap (content) #:inspector (make-inspector))

(define (show-differences orig-s1 orig-s2 columns)
  (let-values ([(to-color-s1 to-color-s2)
                (find-differences orig-s1 orig-s2 columns columns)])
    (define f (new frame% [label ""] [width 600] [height 500]))
    (define hp (new horizontal-panel% [parent f]))
    (define t1 (new text:basic%))
    (define t2 (new text:basic%))
    (define c1 (new editor-canvas% 
                    [parent hp]
                    [editor t1]))
    (define c2 (new editor-canvas% 
                    [parent hp]
                    [editor t2]))
    (render-sexp/colors orig-s1 to-color-s1 t1 columns)
    (render-sexp/colors orig-s2 to-color-s2 t2 columns)
    (send f show #t)))

(define (find-differences orig-s1 orig-s2 columns1 columns2)
  (let ([s1 (unkink orig-s1)]
        [s2 (unkink orig-s2)])
    (define diff-ht (record-differences s1 s2))
    (values (find-coloring s1 diff-ht columns1)
            (find-coloring s2 diff-ht columns2))))

;; render-sexp/colors : sexp ht text -> void
(define (render-sexp/colors sexp to-color text columns)
  (let ([start '()])
    ((pretty-print-parameters)
     (λ ()
       (parameterize ([pretty-print-columns columns])
         (pretty-write sexp (open-output-text-editor text)))))
    (for-each 
     (λ (p) (send text highlight-range (car p) (cdr p) (send the-color-database find-color "NavajoWhite")))
     to-color)
    (send text change-style 
          (make-object style-delta% 'change-family 'modern)
          0
          (send text last-position))))

(define (find-coloring sexp diff-ht columns)
  (let* ([start '()]
         [to-color '()]
         [pending-bytes (bytes)]
         [position 0]
         [counting-port
          (make-output-port 'counting-port
                            always-evt
                            (λ (bs start end can-block? breaks?)
                              (cond
                                [(= 0 (bytes-length bs))
                                 0]
                                [else
                                 (set! pending-bytes (bytes-append pending-bytes (bytes (bytes-ref bs start))))
                                 (let ([str (with-handlers ([exn:fail:contract? (λ (x) #f)])
                                              (bytes->string/utf-8 pending-bytes))])
                                   (when str
                                     (set! position (+ position (string-length str)))
                                     (set! pending-bytes (bytes))))
                                 1]))
                            void)])
    ((pretty-print-parameters)
     (λ ()
       (parameterize ([pretty-print-columns columns]
                      [pretty-print-remap-stylable
                       (λ (val)
                         (and (wrap? val)
                              (symbol? (wrap-content val))
                              (wrap-content val)))]
                      [pretty-print-size-hook
                       (λ (val dsp? port)
                         (if (wrap? val)
                             (or (default-pretty-printer-size-hook (wrap-content val) dsp? port)
                                 (string-length (format "~s" (wrap-content val))))
                             (default-pretty-printer-size-hook val dsp? port)))]
                      [pretty-print-print-hook
                       (λ (val dsp? port)
                         (let ([unwrapped (if (wrap? val) (wrap-content val) val)])
                           (default-pretty-printer-print-hook unwrapped dsp? port)))]
                      [pretty-print-pre-print-hook
                       (λ (obj port)
                         (when (hash-ref diff-ht obj #f)
                           (flush-output port)
                           (set! start (cons position start))))]
                      [pretty-print-post-print-hook
                       (λ (obj port)
                         (when (hash-ref diff-ht obj #f)
                           (flush-output port)
                           (set! to-color (cons (cons (car start) position) to-color))
                           (set! start (cdr start))))])
         (pretty-write sexp counting-port))))
    to-color))

;; does a map-like operation, but if the list is dotted, flattens the results into an actual list.
(define (map/d f l1 l2)
  (let loop ([l1 l1]
             [l2 l2])
    (cond
      [(pair? l1)
       (cons (f (car l1) (car l2))
             (loop (cdr l1) (cdr l2)))]
      [(null? l1) null]
      [else (list (f l1 l2))])))

(define (d-length l1)
  (let loop ([l1 l1]
             [n 0])
    (cond
      [(pair? l1) (loop (cdr l1) (+ n 1))]
      [(null? l1) n]
      [else (cons 'dotted (+ n 1))])))
