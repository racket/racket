#lang racket/base
(require "core.rkt" "base-render.rkt"
         racket/class racket/port racket/list racket/string racket/match
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

(define table-ticks-depth (make-parameter 0))
(define phrase-ticks-depth (make-parameter 0))
(define note-depth (make-parameter 0))

(define (render-mixin %)
  (class %
    
    (define/override (current-render-mode)
      '(markdown))

    (define/override (get-suffix) #".md")

    (define/override (get-substitutions)
      '((#rx"---" "\U2014")
        (#rx"--" "\U2013")
        (#rx"``" "\U201C")
        (#rx"''" "\U201D")
        (#rx"'" "\U2019")))

    (inherit render-block)

    (define/override (render-part d ht)
      (let ([number (collected-info-number (part-collected-info d ht))])
        (unless (zero? (length number))
          (printf (make-string (length number) #\#))
          (printf " "))
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
      (unless (null? flowss)
        ;; Set table-ticks-depth prior to render-block calls
        (define tick? (member (style-name (table-style i))
                              (list 'boxed "defmodule" "RktBlk")))
        (when tick?
          (table-ticks-depth (add1 (table-ticks-depth))))
        (define strs (map (lambda (flows)
                            (map (lambda (d)
                                   (if (eq? d 'cont)
                                       d
                                       (let ([o (open-output-string)])
                                         (parameterize ([current-indent 0]
                                                        [current-output-port o])
                                           (render-block d part ht #f))
                                         (regexp-split
                                          #rx"\n"
                                          (regexp-replace #rx"\n$"
                                                          (get-output-string o)
                                                          "")))))
                                 flows))
                          flowss))
        (define widths (map (lambda (col)
                              (for/fold ([d 0]) ([i (in-list col)])
                                (if (eq? i 'cont)
                                    0
                                    (apply max d (map string-length i)))))
                            (apply map list strs)))
        (define x-length (lambda (col) (if (eq? col 'cont) 0 (length col))))
        (when tick?
          (displayln (string-append "```racket")))
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
                  (let ([col (if (eq? col 'cont) "" col)])
                    (display (regexp-replace* #rx"\uA0" col " "))
                    (display (make-string (max 0 (- w (string-length col))) #\space)))
                  #t)
                (newline)
                #t)))
          #t)
        (when tick?
          (displayln "```")
          (table-ticks-depth (sub1 (table-ticks-depth)))))
      null)

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
      (define (write-note)
        (write-string (make-string (note-depth) #\>))
        (unless (zero? (note-depth))
          (write-string " ")))
      (define o (open-output-string))
      (parameterize ([current-output-port o])
        (super render-paragraph p part ri))
      (define to-wrap (regexp-replace* #rx"\n" (get-output-string o) " "))
      (define lines (wrap-line (string-trim to-wrap) (- 72 (current-indent))))
      (write-note)
      (write-string (car lines))
      (for ([line (in-list (cdr lines))])
        (newline) (indent) (write-note) (write-string line))
      (newline)
      null)

    (define/private (content-style e)
      (cond
       [(element? e) (element-style e)]
       [(multiarg-element? e) (multiarg-element-style e)]
       [else #f]))

    (define/override (render-content i part ri)
      (define tick?
        (and (zero? (table-ticks-depth))
             (element? i)
             (let ([s (element-style i)])
               (or (eq? 'tt s)
                   (and (style? s)
                        (style-name s)
                        (regexp-match? #rx"^Rkt[A-Z]" (style-name s)))))))
      (when tick?
        (when (zero? (phrase-ticks-depth))
          (display "`"))
        (phrase-ticks-depth (add1 (phrase-ticks-depth))))
      (define properties (let ([s (content-style i)])
                           (if (style? s) (style-properties s) '())))
      (define targ (for/or ([p properties])
                    (if (target-url? p) p #f)))
      (define url (and targ (target-url-addr targ)))
      (begin0
          (cond [url (define new-i
                       (match (element-content i)
                         [(list (? string? s))
                          (element (element-style i)
                            (list (format "[~a](~a)" s url)))]
                         [else i]))
                     (super render-content new-i part ri)]
                [(and (element? i)
                      (let ([s (element-style i)])
                        (or (eq? 'hspace s)
                            (and (style? s)
                                 (eq? 'hspace (style-name s))))))
                 (parameterize ([current-preserve-spaces #t])
                   (super render-content i part ri))]
                [else (define style (and (element? i) (element-style i)))
                      (define bold?   (eq? style 'bold))
                      (define italic? (eq? style 'italic))
                      (cond [bold?   (display "**")]
                            [italic? (display "_")])
                      (begin0
                          (super render-content i part ri)
                        (cond [bold?   (display "**")]
                              [italic? (display "_")]))])
        (when tick?
          (phrase-ticks-depth (sub1 (phrase-ticks-depth)))
          (when (zero? (phrase-ticks-depth))
            (display "`")))))

    (define/override (render-nested-flow i part ri starting-item?)
      (define s (nested-flow-style i))
      (unless (memq 'decorative (style-properties s))
        (define note? (equal? (style-name s) "refcontent"))
        (when note?
          (note-depth (add1 (note-depth))))
        (begin0 (super render-nested-flow i part ri starting-item?)
          (when note?
            (note-depth (sub1 (note-depth)))))))

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
                    [else (error 'markdown-render "unknown element symbol: ~e"
                                 i)]))]
        [(string? i)
         (let* ([i (if (or (not (zero? (phrase-ticks-depth)))
                           (not (zero? (table-ticks-depth))))
                       (regexp-replace** i '([#rx"``" . "\U201C"]
                                             [#rx"''" . "\U201D"]))
                       (regexp-replace* #px"([#_*`]{1})" i "\\\\\\1"))]
                [i (if (current-preserve-spaces)
                       (regexp-replace* #rx" " i "\uA0")
                       i)])
           (display i))]
        [else (write i)])
      null)

    (super-new)))

(define (regexp-replace** str ptns&reps)
  (for/fold ([str str])
            ([ptn (map car ptns&reps)]
             [rep (map cdr ptns&reps)])
    (regexp-replace* ptn str rep)))

