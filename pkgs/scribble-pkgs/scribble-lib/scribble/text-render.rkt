#lang racket/base
(require "core.rkt" 
         "base-render.rkt"
         "private/render-utils.rkt"
         racket/class racket/port racket/list racket/string
         scribble/text/wrap)
(provide render-mixin)

(define current-preserve-spaces (make-parameter #f))

(define current-indent (make-parameter 0))
(define (make-indent amt)
  (+ amt (current-indent)))
(define (indent)
  (define i (current-indent))
  (unless (zero? i) (display (make-string i #\space))))
(define (indented-newline)
  (newline)
  (indent))

(define render-mixin
  (mixin (render<%>) ()

    (define/override (current-render-mode)
      '(text))

    (define/override (get-substitutions)
      '((#rx"---" "\U2014")
        (#rx"--" "\U2013")
        (#rx"``" "\U201C")
        (#rx"''" "\U201D")
        (#rx"'" "\U2019")))

    (inherit render-block
             format-number)

    (define/override (render-part d ht)
      (let ([number (collected-info-number (part-collected-info d ht))])
        (unless (part-style? d 'hidden)
          (let ([s (format-number number '())])
            (unless (null? s)
              (printf "~a.~a" 
                      (car s)
                      (if (part-title-content d)
                          " "
                          "")))
            (when (part-title-content d)
              (render-content (part-title-content d) d ht))
            (when (or (pair? number) (part-title-content d))
              (newline)
              (newline))))
        (render-flow (part-blocks d) d ht #f)
        (let loop ([pos 1]
                   [secs (part-parts d)]
                   [need-newline? (pair? (part-blocks d))])
          (unless (null? secs)
            (when need-newline? (newline))
            (render-part (car secs) ht)
            (loop (add1 pos) (cdr secs) #t)))))

    (define/override (render-flow f part ht starting-item?)
      (if (null? f)
          null
          (append*
           (render-block (car f) part ht starting-item?)
           (for/list ([p (in-list (cdr f))])
             (indented-newline)
             (render-block p part ht #f)))))

    (define/override (render-intrapara-block p part ri first? last? starting-item?)
      (unless first? (indented-newline))
      (super render-intrapara-block p part ri first? last? starting-item?))

    (define/override (render-table i part ht inline?)
      (define flowss (table-blockss i))
      (if (null? flowss)
          null
          (let* ([strs (map (lambda (flows)
                              (map (lambda (d)
                                     (if (eq? d 'cont)
                                         d
                                         (let ([o (open-output-string)])
                                           (parameterize ([current-indent 0]
                                                          [current-output-port o])
                                             (render-block d part ht #f))
                                           (regexp-split
                                            #rx"\n"
                                            (regexp-replace #rx"\n$" (get-output-string o) "")))))
                                   flows))
                            flowss)]
                 [extract-align
                  (lambda (s)
                    (define p (style-properties s))
                    (cond
                     [(member 'right p) 'right]
                     [(member 'center p) 'center]
                     [else 'left]))]
                 [alignss
                  (cond
                   [(ormap (lambda (v) (and (table-cells? v) v)) (style-properties (table-style i)))
                    => (lambda (tc)
                         (for/list ([l (in-list (table-cells-styless tc))])
                           (for/list ([s (in-list l)])
                             (extract-align s))))]
                   [(ormap (lambda (v) (and (table-columns? v) v)) (style-properties (table-style i)))
                    => (lambda (tc)
                         (make-list
                          (length flowss)
                          (for/list ([s (in-list (table-columns-styles tc))])
                            (extract-align s))))]
                   [else
                    (if (null? flowss)
                        null
                        (make-list (length flowss) (make-list (length (car flowss)) 'left)))])]
                 [extract-border
                  (lambda (s)
                    (define p (style-properties s))
                    (cond
                     [(memq 'border p) '#(#t #t #t #t)]
                     [else
                      (vector (memq 'left-border p) (memq 'right-border p)
                              (memq 'top-border p) (memq 'bottom-border p))]))]
                 [borderss
                  ;; A border is (vector left? right? top? bottom?)
                  (cond
                   [(ormap (lambda (v) (and (table-cells? v) v)) (style-properties (table-style i)))
                    => (lambda (tc)
                         (for/list ([l (in-list (table-cells-styless tc))])
                           (for/list ([s (in-list l)])
                             (extract-border s))))]
                   [(ormap (lambda (v) (and (table-columns? v) v)) (style-properties (table-style i)))
                    => (lambda (tc)
                         (make-list
                          (length flowss)
                          (for/list ([s (in-list (table-columns-styles tc))])
                            (extract-border s))))]
                   [else
                    (if (null? flowss)
                        null
                        (make-list (length flowss) (make-list (length (car flowss)) '#(#f #f #f #f))))])]
                 [border-left? (lambda (v) (vector-ref v 0))]
                 [border-right? (lambda (v) (vector-ref v 1))]
                 [border-top? (lambda (v) (vector-ref v 2))]
                 [border-bottom? (lambda (v) (vector-ref v 3))]
                 [col-borders ; has only left and right
                  (for/list ([i (in-range (length (car borderss)))])
                    (for/fold ([v '#(#f #f)]) ([borders (in-list borderss)])
                      (define v2 (list-ref borders i))
                      (vector (or (border-left? v) (border-left? v2))
                              (or (border-right? v) (border-right? v2)))))]
                 [widths (map (lambda (col)
                                (for/fold ([d 0]) ([i (in-list col)])
                                  (if (eq? i 'cont)
                                      d
                                      (apply max d (map string-length i)))))
                              (apply map list strs))]
                 [x-length (lambda (col) (if (eq? col 'cont) 0 (length col)))])

            (define (show-row-border prev-borders borders)
              (when (for/or ([prev-border (in-list prev-borders)]
                             [border (in-list borders)])
                      (or (border-bottom? prev-border)
                          (border-top? border)))
                (define-values (end-h-border? end-v-border?)
                  (for/fold ([left-border? #f]
                             [prev-border? #f])
                      ([w (in-list widths)]
                       [prev-border (in-list prev-borders)]
                       [border (in-list borders)]
                       [col-border (in-list col-borders)])
                    (define border? (or (and prev-border (border-bottom? prev-border))
                                        (border-top? border)))
                    (when (or left-border? (border-left? col-border))
                      (display (if (or prev-border? border?) "-" " ")))
                    (display (make-string w (if border? #\- #\space)))
                    (values (border-right? col-border) border?)))
                (when end-h-border?
                  (display (if end-v-border? "-" " ")))
                (newline)))

            (define-values (last-indent? last-borders)
              (for/fold ([indent? #f] [prev-borders #f]) ([row (in-list strs)]
                                                          [aligns (in-list alignss)]
                                                          [borders (in-list borderss)])
                (values
                 (let ([h (apply max 0 (map x-length row))])
                   (let ([row* (for/list ([i (in-range h)])
                                 (for/list ([col (in-list row)])
                                   (if (i . < . (x-length col))
                                       (list-ref col i)
                                       (if (eq? col 'cont)
                                           'cont
                                           ""))))])
                     (for/fold ([indent? indent?]) ([sub-row (in-list row*)]
                                                    [pos (in-naturals)])
                       (when indent? (indent))

                       (when (zero? pos)
                         (show-row-border (or prev-borders (map (lambda (b) '#(#f #f #f #f)) borders))
                                          borders))

                       (define-values (end-border? end-col-border?)
                         (for/fold ([left-border? #f] [left-col-border? #f])
                             ([col (in-list sub-row)]
                              [w (in-list widths)]
                              [align (in-list aligns)]
                              [border (in-list borders)]
                              [col-border (in-list col-borders)])
                           (when (or left-col-border? (border-left? col-border))
                             (display (if (and (or left-border? (border-left? border))
                                               (not (eq? col 'cont)))
                                          "|"
                                          " ")))
                           (let ([col (if (eq? col 'cont) "" col)])
                             (define gap (max 0 (- w (string-length col))))
                             (case align
                               [(right) (display (make-string gap #\space))]
                               [(center) (display (make-string (quotient gap 2) #\space))])
                             (display col)
                             (case align
                               [(left) (display (make-string gap #\space))]
                               [(center) (display (make-string (- gap (quotient gap 2)) #\space))]))
                           (values (border-right? border)
                                   (border-right? col-border))))
                       (when end-col-border?
                         (display (if end-border? "|" " ")))
                       (newline)
                       #t)))
                 borders)))

            (show-row-border last-borders (map (lambda (b) '#(#f #f #f #f)) last-borders))

            null)))

    (define/override (render-itemization i part ht)
      (let ([flows (itemization-blockss i)])
        (if (null? flows)
            null
            (append*
             (begin (printf "* ")
                    (parameterize ([current-indent (make-indent 2)])
                      (render-flow (car flows) part ht #t)))
             (for/list ([d (in-list (cdr flows))])
               (indented-newline)
               (printf "* ")
               (parameterize ([current-indent (make-indent 2)])
                 (render-flow d part ht #f)))))))

    (define/override (render-paragraph p part ri)
      (define o (open-output-string))
      (parameterize ([current-output-port o])
        (super render-paragraph p part ri))
      (define to-wrap (regexp-replace* #rx"\n" (get-output-string o) " "))
      (define lines (wrap-line (string-trim to-wrap) (- 72 (current-indent))))
      (write-string (car lines))
      (for ([line (in-list (cdr lines))])
        (newline) (indent) (write-string line))
      (newline)
      null)

    (define/override (render-content i part ri)
      (if (and (element? i)
               (let ([s (element-style i)])
                 (or (eq? 'hspace s)
                     (and (style? s)
                          (eq? 'hspace (style-name s))))))
          (parameterize ([current-preserve-spaces #t])
            (super render-content i part ri))
          (super render-content i part ri)))

    (define/override (render-nested-flow i part ri starting-item?)
      (define s (nested-flow-style i))
      (unless (memq 'decorative (style-properties s))
        (if (and s (or (eq? (style-name s) 'inset)
                       (eq? (style-name s) 'code-inset)))
            (begin (printf "  ")
                   (parameterize ([current-indent (make-indent 2)])
                     (super render-nested-flow i part ri starting-item?)))
            (super render-nested-flow i part ri starting-item?))))

    (define/override (render-other i part ht)
      (cond
        [(symbol? i)
         (display (case i
                    [(mdash) "\U2014"]
                    [(ndash) "\U2013"]
                    [(ldquo) "\U201C"]
                    [(rdquo) "\U201D"]
                    [(lsquo) "\U2018"]
                    [(rsquo) "\U2019"]
                    [(lang) ">"]
                    [(rang) "<"]
                    [(rarr) "->"]
                    [(nbsp) "\uA0"]
                    [(prime) "'"]
                    [(alpha) "\u03B1"]
                    [(infin) "\u221E"]
                    [else (error 'text-render "unknown element symbol: ~e" i)]))]
        [(string? i) (if (current-preserve-spaces)
                         (display (regexp-replace* #rx" " i "\uA0"))
                         (display i))]
        [else (write i)])
      null)

    (super-new)))
