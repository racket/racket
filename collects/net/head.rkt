#lang racket/base

(require racket/date racket/string)

(provide empty-header
         validate-header
         extract-field
         remove-field
         insert-field
         replace-field
         extract-all-fields
         append-headers
         standard-message-header
         data-lines->data
         extract-addresses
         assemble-address-field)

;; NB: I've done a copied-code adaptation of a number of these definitions
;; into "bytes-compatible" versions.  Finishing the rest will require some
;; kind of interface decision---that is, when you don't supply a header,
;; should the resulting operation be string-centric or bytes-centric?
;; Easiest just to stop here.
;; -- JBC 2006-07-31

(define CRLF (string #\return #\newline))
(define CRLF/bytes #"\r\n")

(define empty-header CRLF)
(define empty-header/bytes CRLF/bytes)

(define re:field-start (regexp "^[^ \t\n\r\v:\001-\032\"]*:"))
(define re:field-start/bytes #rx#"^[^ \t\n\r\v:\001-\032\"]*:")

(define re:continue (regexp "^[ \t\v]"))
(define re:continue/bytes #rx#"^[ \t\v]")

(define (validate-header s)
  (if (bytes? s)
    ;; legal char check not needed per rfc 2822, IIUC.
    (let ([len (bytes-length s)])
      (let loop ([offset 0])
        (cond
          [(and (= (+ offset 2) len)
                (bytes=? CRLF/bytes (subbytes s offset len)))
           (void)] ; validated
          [(= offset len) (error 'validate-header "missing ending CRLF")]
          [(or (regexp-match re:field-start/bytes s offset)
               (regexp-match re:continue/bytes s offset))
           (let ([m (regexp-match-positions #rx#"\r\n" s offset)])
             (if m
               (loop (cdar m))
               (error 'validate-header "missing ending CRLF")))]
          [else (error 'validate-header "ill-formed header at ~s"
                       (subbytes s offset (bytes-length s)))])))
    ;; otherwise it should be a string:
    (begin
      (let ([m (regexp-match #rx"[^\000-\377]" s)])
        (when m
          (error 'validate-header "non-Latin-1 character in string: ~v" (car m))))
      (let ([len (string-length s)])
        (let loop ([offset 0])
          (cond
            [(and (= (+ offset 2) len)
                  (string=? CRLF (substring s offset len)))
             (void)] ; validated
            [(= offset len) (error 'validate-header "missing ending CRLF")]
            [(or (regexp-match re:field-start s offset)
                 (regexp-match re:continue s offset))
             (let ([m (regexp-match-positions #rx"\r\n" s offset)])
               (if m
                 (loop (cdar m))
                 (error 'validate-header "missing ending CRLF")))]
            [else (error 'validate-header "ill-formed header at ~s"
                         (substring s offset (string-length s)))]))))))

(define (make-field-start-regexp field)
  (regexp (format "(^|[\r][\n])(~a: *)" (regexp-quote field #f))))

(define (make-field-start-regexp/bytes field)
  (byte-regexp (bytes-append #"(^|[\r][\n])("(regexp-quote field #f) #": *)")))

(define (extract-field field header)
  (if (bytes? header)
    (let ([m (regexp-match-positions (make-field-start-regexp/bytes field)
                                     header)])
      (and m
           (let ([s (subbytes header
                              (cdaddr m)
                              (bytes-length header))])
             (let ([m (regexp-match-positions #rx#"[\r][\n][^: \r\n\"]*:" s)])
               (if m
                 (subbytes s 0 (caar m))
                 ;; Rest of header is this field, but strip trailing CRLFCRLF:
                 (regexp-replace #rx#"\r\n\r\n$" s ""))))))
    ;; otherwise header & field should be strings:
    (let ([m (regexp-match-positions (make-field-start-regexp field)
                                     header)])
      (and m
           (let ([s (substring header
                               (cdaddr m)
                               (string-length header))])
             (let ([m (regexp-match-positions #rx"[\r][\n][^: \r\n\"]*:" s)])
               (if m
                 (substring s 0 (caar m))
                 ;; Rest of header is this field, but strip trailing CRLFCRLF:
                 (regexp-replace #rx"\r\n\r\n$" s ""))))))))

(define (replace-field field data header)
  (if (bytes? header)
    (let ([m (regexp-match-positions (make-field-start-regexp/bytes field)
                                     header)])
      (if m
        (let* ([pre (subbytes header 0 (caaddr m))]
               [s (subbytes header (cdaddr m))]
               [m (regexp-match-positions #rx#"[\r][\n][^: \r\n\"]*:" s)]
               [rest (if m (subbytes s (+ 2 (caar m))) empty-header/bytes)])
          (bytes-append pre (if data (insert-field field data rest) rest)))
        (if data (insert-field field data header) header)))
    ;; otherwise header & field & data should be strings:
    (let ([m (regexp-match-positions (make-field-start-regexp field) header)])
      (if m
        (let* ([pre (substring header 0 (caaddr m))]
               [s (substring header (cdaddr m))]
               [m (regexp-match-positions #rx"[\r][\n][^: \r\n\"]*:" s)]
               [rest (if m (substring s (+ 2 (caar m))) empty-header)])
          (string-append pre (if data (insert-field field data rest) rest)))
        (if data (insert-field field data header) header)))))

(define (remove-field field header)
  (replace-field field #f header))

(define (insert-field field data header)
  (if (bytes? header)
    (let ([field (bytes-append field #": "data #"\r\n")])
      (bytes-append field header))
    ;; otherwise field, data, & header should be strings:
    (let ([field (format "~a: ~a\r\n" field data)])
      (string-append field header))))

(define (append-headers a b)
  (if (bytes? a)
    (let ([alen (bytes-length a)])
      (if (> alen 1)
        (bytes-append (subbytes a 0 (- alen 2)) b)
        (error 'append-headers "first argument is not a header: ~a" a)))
    ;; otherwise, a & b should be strings:
    (let ([alen (string-length a)])
      (if (> alen 1)
        (string-append (substring a 0 (- alen 2)) b)
        (error 'append-headers "first argument is not a header: ~a" a)))))

(define (extract-all-fields header)
  (if (bytes? header)
    (let ([re #rx#"(^|[\r][\n])(([^\r\n:\"]*): *)"])
      (let loop ([start 0])
        (let ([m (regexp-match-positions re header start)])
          (if m
            (let ([start (cdaddr m)]
                  [field-name (subbytes header (caaddr (cdr m))
                                        (cdaddr (cdr m)))])
              (let ([m2 (regexp-match-positions
                         #rx#"\r\n[^: \r\n\"]*:"
                         header
                         start)])
                (if m2
                  (cons (cons field-name
                              (subbytes header start (caar m2)))
                        (loop (caar m2)))
                  ;; Rest of header is this field, but strip trailing CRLFCRLF:
                  (list
                   (cons field-name
                         (regexp-replace #rx#"\r\n\r\n$"
                                         (subbytes header start (bytes-length header))
                                         ""))))))
            ;; malformed header:
            null))))
    ;; otherwise, header should be a string:
    (let ([re #rx"(^|[\r][\n])(([^\r\n:\"]*): *)"])
      (let loop ([start 0])
        (let ([m (regexp-match-positions re header start)])
          (if m
            (let ([start (cdaddr m)]
                  [field-name (substring header (caaddr (cdr m)) (cdaddr (cdr m)))])
              (let ([m2 (regexp-match-positions
                         #rx"\r\n[^: \r\n\"]*:" header start)])
                (if m2
                  (cons (cons field-name
                              (substring header start (caar m2)))
                        (loop (caar m2)))
                  ;; Rest of header is this field, but strip trailing CRLFCRLF:
                  (list
                   (cons field-name
                         (regexp-replace #rx"\r\n\r\n$"
                                         (substring header start (string-length header))
                                         ""))))))
            ;; malformed header:
            null))))))

;; It's slightly less obvious how to generalize the functions that don't
;; accept a header as input; for lack of an obvious solution (and free time),
;; I'm stopping the string->bytes translation here.  -- JBC, 2006-07-31

(define (standard-message-header from tos ccs bccs subject)
  (let ([h (insert-field
            "Subject" subject
            (insert-field
             "Date" (parameterize ([date-display-format 'rfc2822])
                      (date->string (seconds->date (current-seconds)) #t))
             CRLF))])
    ;; NOTE: bccs don't go into the header; that's why they're "blind"
    (let ([h (if (null? ccs)
               h
               (insert-field "CC" (assemble-address-field ccs) h))])
      (let ([h (if (null? tos)
                 h
                 (insert-field "To" (assemble-address-field tos) h))])
        (insert-field "From" from h)))))

(define (splice l sep)
  (if (null? l)
    ""
    (format "~a~a"
            (car l)
            (apply string-append
                   (map (lambda (n) (format "~a~a" sep n))
                        (cdr l))))))

(define (data-lines->data datas)
  (splice datas "\r\n\t"))

;; Extracting Addresses ;;

(define blank "[ \t\n\r\v]")
(define nonblank "[^ \t\n\r\v]")
(define re:all-blank (regexp (format "^~a*$" blank)))
(define re:quoted (regexp "\"[^\"]*\""))
(define re:parened (regexp "[(][^)]*[)]"))
(define re:comma (regexp ","))
(define re:comma-separated (regexp "([^,]*),(.*)"))

(define (extract-addresses s form)
  (unless (memq form '(name address full all))
    (raise-type-error 'extract-addresses
                      "form: 'name, 'address, 'full, or 'all"
                      form))
  (if (or (not s) (regexp-match re:all-blank s))
    null
    (let loop ([prefix ""][s s])
      ;; Which comes first - a quote or a comma?
      (let* ([mq1 (regexp-match-positions re:quoted s)]
             [mq2 (regexp-match-positions re:parened s)]
             [mq (if (and mq1 mq2)
                   (if (< (caar mq1) (caar mq2)) mq1 mq2)
                   (or mq1 mq2))]
             [mc (regexp-match-positions re:comma s)])
        (if (and mq mc (< (caar mq) (caar mc) (cdar mq)))
          ;; Quote contains a comma
          (loop (string-append
                 prefix
                 (substring s 0 (cdar mq)))
                (substring s (cdar mq) (string-length s)))
          ;; Normal comma parsing:
          (let ([m (regexp-match re:comma-separated s)])
            (if m
              (let ([n (extract-one-name (string-append prefix (cadr m)) form)]
                    [rest (extract-addresses (caddr m) form)])
                (cons n rest))
              (let ([n (extract-one-name (string-append prefix s) form)])
                (list n)))))))))

(define (select-result form name addr full)
  (case form
    [(name) name]
    [(address) addr]
    [(full) full]
    [(all) (list name addr full)]))

(define (one-result form s)
  (select-result form s s s))

(define re:quoted-name (regexp (format "^~a*(\"[^\"]*\")(.*)" blank)))
(define re:parened-name (regexp (format "(.*)[(]([^)]*)[)]~a*$" blank)))
(define re:simple-name (regexp (format "^~a*(~a.*)(<.*>)~a*$" blank nonblank blank)))
(define re:normal-name (regexp (format "~a*<([^>]*)>~a*" blank blank)))
(define re:double-less (regexp "<.*<"))
(define re:double-greater (regexp ">.*>"))
(define re:bad-chars (regexp "[,\"()<>]"))
(define re:tail-blanks (regexp (format "~a+$" blank)))
(define re:head-blanks (regexp (format "^~a+" blank)))

(define (extract-one-name orig form)
  (let loop ([s orig][form form])
    (cond
      ;; ?!?!? Where does the "addr (name)" standard come from ?!?!?
      [(regexp-match re:parened-name s)
       => (lambda (m)
            (let ([name (caddr m)]
                  [all (loop (cadr m) 'all)])
              (select-result
               form
               (if (string=? (car all) (cadr all)) name (car all))
               (cadr all)
               (format "~a (~a)" (caddr all) name))))]
      [(regexp-match re:quoted-name s)
       => (lambda (m)
            (let ([name (cadr m)]
                  [addr (extract-angle-addr (caddr m) s)])
              (select-result form name addr
                             (format "~a <~a>" name addr))))]
      [(regexp-match re:simple-name s)
       => (lambda (m)
            (let ([name (regexp-replace (format "~a*$" blank) (cadr m) "")]
                  [addr (extract-angle-addr (caddr m) s)])
              (select-result form name addr
                             (format "~a <~a>" name addr))))]
      [(or (regexp-match "<" s) (regexp-match ">" s))
       (one-result form (extract-angle-addr s orig))]
      [else (one-result form (extract-simple-addr s orig))])))

(define (extract-angle-addr s orig)
  (if (or (regexp-match re:double-less s) (regexp-match re:double-greater s))
    (error 'extract-address "too many angle brackets: ~a" s)
    (let ([m (regexp-match re:normal-name s)])
      (if m
        (extract-simple-addr (cadr m) orig)
        (error 'extract-address "cannot parse address: ~a" orig)))))

(define (extract-simple-addr s orig)
  (cond [(regexp-match re:bad-chars s)
         (error 'extract-address "cannot parse address: ~a" orig)]
        [else
         ;; final whitespace strip
         (regexp-replace re:tail-blanks
                         (regexp-replace re:head-blanks s "")
                         "")]))

(define (assemble-address-field addresses)
  (if (null? addresses)
    ""
    (let loop ([addresses (cdr addresses)]
               [s (car addresses)]
               [len (string-length (car addresses))])
      (if (null? addresses)
        s
        (let* ([addr (car addresses)]
               [alen (string-length addr)])
          (if (<= 72 (+ len alen))
            (loop (cdr addresses)
                  (format "~a,~a~a~a~a"
                          s #\return #\linefeed
                          #\tab addr)
                  alen)
            (loop (cdr addresses)
                  (format "~a, ~a" s addr)
                  (+ len alen 2))))))))
