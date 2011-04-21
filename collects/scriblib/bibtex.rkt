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
       (read-until pred ip)]
      [_
       (void)]))
  (define (read-until pred ip)
    (list->string
     (let loop ()
      (match (peek-char ip)
        [(? pred)
         (cons (read-char ip) (loop))]
        [_
         empty]))))
  
  (define (slurp-whitespace ip)
    (read-while char-whitespace? ip))
    
  (define (read-entries ip)
    (slurp-whitespace ip)
    (match (read-char ip)
      [#\%
       (read-line ip)
       (read-entries ip)]
      [#\@
       (cons (read-entry ip)
             (read-entries ip))]
      [c
       (error 'read-entries "Expected % or @, got ~a" c)]))
  
  (define (read-entry ip)
    (match (peek-string 6 0 ip)
      [(app string-downcase "string")
       (read-string 6 ip)
       (slurp-whitespace ip)
       (match (read-char ip)
         [#\{
          (slurp-whitespace ip)
          (define tag (read-tag ip))
          (printf "tag ~a\n" tag)
          (slurp-whitespace ip)
          (match (read-char ip)
            [#\=
             (slurp-whitespace ip)
             (define string (read-value ip))
             (printf "string (~a,~a)\n" tag string)
             (slurp-whitespace ip)
             (match (read-char ip)
               [#\}
                (cons tag string)]
               [c
                (error 'read-entry "Parsing string, expected }, got ~a" c)])]
            [c
             (error 'read-entry "Parsing string, expected =, got ~a" c)])]
         [c
          (error 'read-entry "Parsing string, expected {, got ~a" c)])]))
  
  (define (read-tag ip)
    (slurp-whitespace ip)
    (read-until char-whitespace? ip))
  
  (define (read-value ip)
    (slurp-whitespace ip)
    (match (peek-char ip)
      [#\{
       (read-char ip)
       (define first-part (read-until (λ (c) (or (char=? c #\{) (char=? c #\}))) ip))
       (match (peek-char ip)
         [#\{
          (printf "Inner read: ~a\n" first-part)
          (string-append first-part (read-value ip))]
         [#\}
          (read-char ip)
          first-part])]
      [c
       (error 'read-value "Parsing value, expected {, got ~a" c)]))
  
  (with-handlers
      ([exn? (λ (x)
               (printf "~a\n" (read-string 100 ip))
               (raise x))])
    (read-entries ip)))

(define (path->bibdb pth)
  (printf "~a\n"
          (with-input-from-file
              pth
            (λ ()
              (bibtex-parse (current-input-port)))))
  (error 'path->bibdb pth)
  #f)

(define (generate-bib db style)
  "XXX")

(define (cite db . keys)
  "XXX")

(provide define-bibtex)