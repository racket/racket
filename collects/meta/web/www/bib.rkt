#lang meta/web

(require "shared.rkt")

;; bib values are hash tables mapping field names (symbols) to strings.
;; Keywords can also be used for the field names, which makes them meta-fields
;; that are not included in the usual bib printout.  Two of them are required:
;;   - #:type  the type of the entry (a symbol: 'article, 'techreport, etc)
;;   - #:key   the label for the entry

(define bib-fields
  '(author editor title booktitle journal
    edition volume number series
    chapter pages
    type
    school institution organization
    publisher howpublished
    address
    month year
    key
    crossref
    url
    note
    eprint))

(define meta-field? keyword?)

(define key->number
  (let ([t (for/hash ([k bib-fields] [i (in-naturals)]) (values k i))])
    (lambda (key)
      (hash-ref t key (lambda ()
                        (error 'key->number "unknown field name: ~e" key))))))

;; converts the hash to an alist with the order specified by bib-fields
(define (bib->alist bib)
  (sort (filter-not (compose meta-field? car) (hash-map bib cons))
        < #:key (compose key->number car) #:cache-keys? #t))

(define (display* . xs)
  (for-each display xs))

(define (display-attr attr)
  (let* ([prefix (format "  ~a = {" (car attr))]
         [sep (delay (string-append "\n" (make-string (string-length prefix)
                                                      #\space)))])
    (display* prefix
              (if (regexp-match? #rx"\n" (cdr attr))
                (regexp-replace* #rx"\n" (cdr attr) (force sep))
                (cdr attr))
              "}")))

(provide display-bib)
(define (display-bib bib)
  (display* "@" (hash-ref bib '#:type) "{" (hash-ref bib '#:key))
  (for ([attr (bib->alist bib)]) (display* ",\n") (display-attr attr))
  (display* "\n}\n"))

(provide with-braces without-braces)
(define (with-braces str) (regexp-replace* #px"\\b\\w+[A-Z]\\w*" str "{\\0}"))
(define (without-braces str) (regexp-replace* #rx"[{}]+" str ""))

(provide bib-author)
(define (bib-author bib)
  (let ([authors (regexp-split #rx"[ \t\n]+and[ \t\n]+"
                               (hash-ref bib 'author))])
    (case (length authors)
      [(0) "???"]
      [(1) (car authors)]
      [(2) (apply format "~a and ~a" authors)]
      [(3) (apply format "~a, ~a, and ~a" authors)]
      [else (format "~a et al" (car authors))])))

;; ----------------------------------------------------------------------------

;; processes the (key val ...) alist to a hash of (key . val) by combining the
;; possibly multiple values for each key (each value becomes a line)
(provide bib)
(define (bib type key attrs)
  (define t (make-hasheq))
  (hash-set! t '#:type type)
  (hash-set! t '#:key  key)
  (for ([a attrs])
    (define (err) (error 'make-bib "bad attribute: ~e" a))
    (unless (and (pair? a) (pair? (cdr a)) (list? (cdr a))) (err))
    (let ([key (car a)])
      (unless (hash-ref t key #f) ; previous keys take precedence
        (cond [(symbol? key)
               ;; turn non-strings to strings, join multiple strings, normalize
               ;; spaces
               (let* ([val (cdr a)]
                      [val (map (lambda (x) (if (string? x) x (format "~a" x)))
                                val)]
                      [val (string-append* (add-between val "\n"))]
                      [val (regexp-replace* #rx"\t" val " ")]
                      [val (regexp-replace* #rx"  +" val " ")]
                      [val (regexp-replace #rx"^ +" val "")]
                      [val (regexp-replace #rx" +$" val "")]
                      [val (regexp-replace* #rx"(?: *\r?\n *)+" val "\n")])
                 (hash-set! t key val))]
              [(and (meta-field? key) (null? (cddr a)))
               (hash-set! t key (cadr a))]
              [else (err)]))))
  t)

;; ----------------------------------------------------------------------------

#|
In the future, it might be good to do some field verification, etc.  From the
bibtext thing:
  article: An article from a journal or magazine.
    req: author, title, journal, year
    opt: volume, number, pages, month, note
  book: A book with an explicit publisher.
    req: author or editor, title, publisher, year
    opt: volume or number, series, address, edition, month, note
  booklet: A work that is printed and bound, but without a named publisher or
           sponsoring institution.
    req: title
    opt: author, howpublished, address, month, year, note
  conference: The same as `inproceedings', included for Scribe compatibility.
  inbook: A part of a book, which may be a chapter (or section or whatever)
          and/or a range of pages.
    req: author or editor, title, chapter and/or pages, publisher, year
    opt: volume or number, series, type, address, edition, month, note
  incollection: A part of a book having its own title.
    req: author, title, booktitle, publisher, year
    opt: editor, volume or number, series, type, chapter, pages, address,
         edition, month, note
  inproceedings: An article in a conference proceedings.
    req: author, title, booktitle, year
    opt: editor, volume or number, series, pages, address, month, organization,
         publisher, note
  manual: Technical documentation.
    req: title
    opt: author, organization, address, edition, month, year, note
  mastersthesis: A Master's thesis.
    req: author, title, school, year
    opt: type, address, month, note
  misc: Use this type when nothing else fits.
    opt: author, title, howpublished, month, year, note
  phdthesis: A PhD thesis.
    req: author, title, school, year
    opt: type, address, month, note
  proceedings: The proceedings of a conference.
    req: title, year
    opt: editor, volume or number, series, address, month, organization,
         publisher, note
  techreport: A report published by a school or other institution, usually
              numbered within a series.
    req: author, title, institution, year
    opt: type, number, address, month, note
  unpublished: A document having an author and title, but not formally
               published.
    req: author, title, note
    opt: month, year
|#
