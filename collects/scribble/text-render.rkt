#lang racket/base
(require "core.rkt" "base-render.rkt"
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

    (inherit render-block)

    (define/override (render-part d ht)
      (let ([number (collected-info-number (part-collected-info d ht))])
        (for ([n (in-list (reverse number))] #:when n) (printf "~s." n))
        (when (part-title-content d)
          (when (ormap values number) (printf " "))
          (render-content (part-title-content d) d ht))
        (when (or (ormap values number) (part-title-content d))
          (newline)
          (newline))
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
                 [widths (map (lambda (col)
                                (for/fold ([d 0]) ([i (in-list col)])
                                  (if (eq? i 'cont)
                                      0
                                      (apply max d (map string-length i)))))
                              (apply map list strs))]
                 [x-length (lambda (col) (if (eq? col 'cont) 0 (length col)))])
            (for/fold ([indent? #f]) ([row (in-list strs)])
              (let ([h (apply max 0 (map x-length row))])
                (let ([row* (for/list ([i (in-range h)])
                              (for/list ([col (in-list row)])
                                (if (i . < . (x-length col))
                                    (list-ref col i)
                                    "")))])
                  (for/fold ([indent? indent?]) ([sub-row (in-list row*)])
                    (when indent? (indent))
                    (for/fold ([space? #f])
                              ([col (in-list sub-row)]
                               [w (in-list widths)])
                      ;; (when space? (display " "))
                      (let ([col (if (eq? col 'cont) "" col)])
                        (display col)
                        (display (make-string (max 0 (- w (string-length col))) #\space)))
                      #t)
                    (newline)
                    #t)))
              #t)
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
