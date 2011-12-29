#lang at-exp racket/base

(require scribble/html)

;; list of a header paragraphs and sub paragraphs (don't use `p' since it looks
;; like they should not be nested)
(provide parlist)
(define (parlist first . rest)
  (list (div class: 'parlisttitle first)
        (map (λ (p) (div class: 'parlistitem p)) rest)))

;; a div that is centered, but the text is still left-justified
(provide center-div)
(define (center-div . text)
  (let-values ([(attrs body) (split-attributes+body text)])
    (apply div align: 'center
           (append attrs
                   (list (div align: 'left style: "display: inline-block;"
                              body))))))

;; a grayish tt text
(provide TT)
(define (TT . xs)
  @tt[style: "background-color: #d8d8e8;"]{@xs})
(provide PRE)
(define (PRE . xs)
  @pre[style: "background-color: #d8d8e8;"]{@xs})

;; some tags with convenient separators
(provide make-separated-tag (rename-out [the-separator ~])
         p* ul* ol* dl*)
(struct separator ())
(define the-separator (separator))
(define (split-list-by-separator list)
  ;; The idea is to drop all whitespace around the separator, and then drop the
  ;; common leading all-space prefixes in each chunk, so the separators are
  ;; effectively ignored for indentation in the output.  This is too much for
  ;; html output (leaving the whitespaces in what this is used for is
  ;; harmless), but it might be useful for some future application.
  (define (drop-ws list left?)
    (if (and (pair? list) (string? (car list)))
      (let ([str (regexp-replace (if left? #rx"^[ \t\r\n]+" #rx"[ \t\r\n]+$")
                                 (car list) "")])
        (if (equal? "" str) (drop-ws (cdr list) left?) (cons str (cdr list))))
      list))
  (define (drop-indentation/reverse orig-text)
    (define N #f)
    (if (null? orig-text)
      orig-text
      (let loop ([text orig-text] [r '()])
        (cond [(null? (cdr text))
               (if N (cons (car text) r) (reverse orig-text))]
              [(not (equal? "\n" (cadr text)))
               (loop (cdr text) (cons (car text) r))]
              [(not (and (string? (car text))
                         (regexp-match? #rx"^ +$" (car text))))
               (reverse orig-text)]
              [else (let ([len (string-length (cadr text))])
                      (set! N (if N (min len N) len))
                      (loop (cddr text)
                            (list* (λ () (make-string (- len N) #\space))
                                   "\n" r)))]))))
  (let loop ([list (drop-ws list #t)] [cur '()] [r '()])
    (define (get-r) (cons (drop-indentation/reverse (drop-ws cur #f)) r))
    (cond [(null? list) (reverse (get-r))]
          [(separator? (car list)) (loop (drop-ws (cdr list) #t) '() (get-r))]
          [else (loop (cdr list) (cons (car list) cur) r)])))
(define ((make-separated-tag wrapper #:newlines? [nls? #t] . tags) . body)
  (let* ([chunks (split-list-by-separator body)]
         [chunks (if (null? (car chunks)) (cdr chunks) chunks)]
         [body (for/list ([text (in-list chunks)]
                          [tag  (in-cycle (in-list tags))])
                 (apply tag text))]
         [body (if nls? (add-newlines body) body)])
    (wrapper body)))
(define p*  (make-separated-tag values p))
(define ul* (make-separated-tag ul li))
(define ol* (make-separated-tag ol li))
(define dl* (make-separated-tag dl dt dd))

;; conditional display on screen or print
(provide printonly screenonly)
(define (printonly  . body) (apply div class: 'printonly  body))
(define (screenonly . body) (apply div class: 'screenonly body))

;; (sections) defines a `section' function and spits out a (delayed) table of
;; contents for all its future uses in the page.
(provide sections)
(require (for-syntax racket/base))
(define (section->label title)
  (regexp-replace* #rx"[^a-z0-9_]+" (string-downcase title) "_"))
(define (make-sectioner #:toc? [toc? #t]
                        #:newpages? [newpages? #f]
                        #:show-section-in-subtitle [sec-in-subsec? #t])
  (define sections '())
  (define cur-sec #f)
  (define subsections '())
  (define (->li/reverse items [more-style #f])
    (ul style: more-style (add-newlines (map li (reverse items)))))
  (define (collect-subs)
    (when (pair? subsections)
      (set! sections
            (cons (list (car sections)
                        (->li/reverse subsections "font-size: small;"))
                  (cdr sections)))
      (set! subsections '())))
  (define ((add-section sub?) #:newpage? [newpage? newpages?] . title)
    (let* ([title* (if sub? (list cur-sec ": " title) title)]
           [label  (section->label (xml->string title*))])
      (unless sub? (collect-subs) (set! cur-sec title))
      (let ([title (a href: (list "#" label) style: "text-decoration: none;"
                      title)])
        (if sub?
          (set! subsections (cons title subsections))
          (set! sections    (cons title sections))))
      ((if sub? h2 h1)
       (a name: label
          style: (and newpage? (pair? (cdr sections))
                      "page-break-before: always;")
          (if sec-in-subsec? title* title)))))
  (values (add-section #f) (add-section #t)
          (and toc? (λ () (collect-subs) (->li/reverse sections)))))
(define-syntax (sections stx)
  (define (make-it stx args)
    (with-syntax ([sec (datum->syntax stx 'section)]
                  [sub (datum->syntax stx 'subsection)]
                  [(x ...) args])
      #'(begin (define-values [sec sub toc] (make-sectioner x ...))
               toc)))
  (syntax-case stx ()
    [(s x ...)           (make-it #'s #'(x ...))]
    [_ (identifier? stx) (make-it stx #'())]))
