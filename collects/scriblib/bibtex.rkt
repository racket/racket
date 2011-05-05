#lang racket/base
(require racket/function
         racket/match
         racket/list)

(define-syntax-rule
  (define-bibtex this-generate-bib this-cite bib-pth)
  (begin
    (define bibtex-db (path->bibdb bib-pth))
    (define this-generate-bib
      (curry generate-bib bibtex-db))
    (define this-cite
      (curry cite bibtex-db))))

(define (bibtex-parse ip)
  (define (read-while pred ip)
    (match (peek-char ip)
      [(? pred)
       (read-char ip)
       (read-while pred ip)]
      [_
       (void)]))
  
  (define (read-until pred ip)
    (list->string
     (let loop ()
      (match (peek-char ip)
        [(? pred)
         empty]
        [_
         (cons (read-char ip) (loop))]))))
  
  (define (slurp-whitespace ip)
    (read-while (λ (c) (and (char? c) (char-whitespace? c))) ip))
    
  (define (read-entries ip)
    (slurp-whitespace ip)
    (match (read-char ip)
      [#\%
       (read-line ip)
       (read-entries ip)]
      [#\@
       (cons (read-entry ip)
             (read-entries ip))]
      [(? eof-object?)
       empty]
      [c
       (error 'read-entries "Expected % or @, got ~v" c)]))
  
  (define (read-entry ip)
    (match (read-until (λ (c) (char=? c #\{)) ip)
      [(app string-downcase "string")
       (slurp-whitespace ip)
       (read-char ip)
       (define tag (read-tag ip))
       (printf "string tag ~v\n" tag)
       (slurp-whitespace ip)
       (match (read-char ip)
         [#\=
          (slurp-whitespace ip)
          (define string (read-value ip))
          (printf "string (~v,~v)\n" tag string)
          (slurp-whitespace ip)
          (match (read-char ip)
            [#\}
             (list 'string tag string)]
            [c
             (error 'read-entry "Parsing string, expected }, got ~v" c)])]
         [c
          (error 'read-entry "Parsing string, expected =, got ~v" c)])]
      [(app string-downcase "comment")
       (read-char ip)
       (let loop ()
         (read-until (λ (c) (or (char=? c #\{) (char=? c #\}))) ip)
         (match (read-char ip)
           [#\{
            (loop) (loop)]
           [#\}
            (void)]))]
      [typ
       (read-char ip)
       (slurp-whitespace ip)
       (define label (read-until (λ (c) (char=? c #\,)) ip))
       (read-char ip)
       (printf "entry label ~v\n" label)
       (define alist
         (let loop ()
           (slurp-whitespace ip)
           (define atag (read-tag ip))
           (slurp-whitespace ip)
           (match (read-char ip)
            [#\=
             (slurp-whitespace ip)
             (define aval (read-value ip))
             (define e (cons atag aval))
             (match (read-char ip)
               [#\,
                (cons e (loop))]
               [#\}
                (list e)]
               [c
                (error 'read-entry "Parsing entry, expected , or }, got ~v" c)])]
            [c
             (error 'read-entry "Parsing entry, expected =, got ~v" c)])))
       (list 'entry typ label alist)]))
  
  (define (read-tag ip)
    (slurp-whitespace ip)
    (read-until char-whitespace? ip))
  
  (define (read-value ip)
    (slurp-whitespace ip)
    (match (peek-char ip)
      [#\{
       (read-char ip)
       (let loop ()
         (define first-part (read-until (λ (c) (or (char=? c #\{) (char=? c #\})))
                                        ip))
         (match (peek-char ip)
           [#\{
            (string-append first-part (read-value ip) (loop))]
           [#\}
            (read-char ip)
            first-part]))]
      [(? char-numeric?)
       (read-while char-numeric? ip)]
      [(? char-alphabetic?)
       ; XXX string ref
       (read-until (λ (c) (char=? c #\,)) ip)]
      [c
       (error 'read-value "Parsing value, expected {, got ~v" c)]))
  
  (with-handlers
      ([exn? (λ (x)
               (printf "~v\n" (read-string 100 ip))
               (raise x))])
    (read-entries ip)))

(define (path->bibdb pth)
  (define bibdb
    (with-input-from-file
        pth
      (λ ()
        (bibtex-parse (current-input-port)))))
  (printf "~v\n" (length bibdb))
  (error 'path->bibdb pth)
  #f)

(path->bibdb "/Users/jay/Dev/scm/github.jeapostrophe/work/papers/etc/all.bib")

(define (generate-bib db style)
  "XXX")

(define (cite db . keys)
  "XXX")

(provide define-bibtex)