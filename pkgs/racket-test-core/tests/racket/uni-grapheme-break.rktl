(require racket/match
         racket/string
         (only-in racket/list flatten)
         (only-in net/url get-pure-port string->url)
         (only-in racket/port copy-port))

(load-relative "loadtest.rktl")

(Section 'uni-grapheme-break)

(define (get-test-file)
  (define name "GraphemeBreakTest.txt")
  (define base "http://www.unicode.org/Public/14.0.0/ucd/auxiliary/")
  (define here (current-load-relative-directory))
  (or (for/or ([dir (list here (current-directory))])
              (define path (build-path dir name))
              (and (file-exists? path) path))
      (let ([path (build-path here name)])
        (define (try)
          (with-handlers ([exn:fail? (lambda (x) #f)])
            (with-output-to-file path #:exists 'replace
              (lambda ()
                (copy-port (get-pure-port (string->url (string-append base name)))
                           (current-output-port))))))
        (for/or ([n 5])
                (unless (zero? n)
                  (sleep 0.1))
                (try))
        path)))

(struct test-case [text segments description] #:transparent)

(define (parse-test str)
  (map
   (λ (s)
     (map (λ (s) (string->number (string-trim s) 16))
          (string-split s "×")))
   (filter (λ (s) (> (string-length s) 0))
           (string-split (string-trim str) "÷"))))

(define (segments->text segments)
  (list->string (map integer->char (flatten segments))))

(define (string->segment str)
  (map char->integer (string->list str)))

(printf "Reading tests...\n")
(define test-cases
  (let*
      ([test-file (get-test-file)]
       [data
        (with-input-from-file test-file
          (λ ()
            (define first-line (read-line))
            (cond
              [(or (eof-object? first-line)
                   (not (regexp-match #rx"^# GraphemeBreakTest-" first-line)))
               "Bad test-file contents"]
              [else
               (for/list ([line (in-lines)]
                          #:when (char=? #\÷ (string-ref line 0)))
                 (match line
                   [(regexp #px"^([^#]+)#(.*)$" (list _ test-str desc))
                    (define segments (parse-test test-str))
                    (define text (segments->text segments))
                    (test-case text segments desc)]))])))])
    (cond
      [(pair? data)
       data]
      [(string? data)
       (delete-file test-file)
       (log-warning data)
       null]
      [else
       (raise-argument-error 'test-cases "(or/c pair? string?)")])))

(for ([t (in-list test-cases)])
  (match-define (test-case text segments desc) t)
  (define forwards-graphemes
    (let loop ([pos 0] [res null])
      (cond
        [(= pos (string-length text))
         (reverse res)]
        [else
         (define next-pos (string-next-grapheme-boundary text pos))
         (define grapheme (substring text pos next-pos))
         (loop next-pos (cons grapheme res))])))
  (define backwards-graphemes
    (let loop ([pos (string-length text)] [res null])
      (cond
        [(= pos 0)
         (reverse res)]
        [else
         (define prev-pos
           (string-previous-grapheme-boundary text pos))
         (define grapheme (substring text prev-pos pos))
         (loop prev-pos (cons grapheme res))])))

  (test #t equal? segments (map string->segment forwards-graphemes))
  (test #t equal? (reverse segments)
        (map string->segment backwards-graphemes)))

(report-errs)
