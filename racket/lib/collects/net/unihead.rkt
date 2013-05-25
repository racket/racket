#lang racket/base
(require net/base64 net/qp)

(provide encode-for-header decode-for-header generalize-encoding)

(define re:non-ascii #rx"[^\u0-\u7F]")

(define (encode-for-header s)
  (cond [(not (regexp-match? re:non-ascii s)) s]
        [(not (regexp-match? #rx"[\r\n]" s)) (encode-line-for-header s)] ; speed
        [else (regexp-replace* #rx"[^\r\n]+" s encode-line-for-header)]))

;; Note: the following two encoder wrappers remove newlines from the
;; encoded strings.  This avoids producing invalid strings, but it's not
;; complete: rfc2047 (section 2) specifies that encoded words should not
;; be longer than 75 characters, and longer words should be split for
;; encoding with a separator of CRLF SPACE between them.  The problem is
;; that doing this properly requires changing the encoders to get a
;; length limit and have them return also the leftover unencoded string.
;; Instead of doing all of that, do something simpler: if the string to
;; be encoded is longer than 70 characters, then split it.  (This is
;; done in `encode-line-for-header' below.)  It's possible to get longer
;; encodings with this, but it seems that sendmail's limit on line
;; lengths is sufficiently larger that it works fine in practice.  (BTW,
;; when sendmail gets lines that are too long it splits them with the
;; dreaded "!\n ", and it looks like there is no sane way to avoid that
;; behavior -- so splitting the input is needed.)

(define (base64-encode-header s)
  (regexp-replace* #rx#"[\r\n]+" (base64-encode s) #""))

(define (qp-encode-header s)
  ;; rfc2047 (section 4.2) calls this "Q encoding", which is different
  ;; from the usual QP encoding: encode underlines and question marks,
  ;; and replace spaces by underlines; also remove soft-newlines.
  (regexp-replace* #rx#"[ ?_]"
                   (regexp-replace* #rx#"=\r?\n" (qp-encode s) #"")
                   (λ (b)
                     (case (bytes-ref b 0)
                       [(32) #"_"]   ; " "
                       [(63) #"=3F"] ; "?"
                       [(95) #"=5F"] ; "_"
                       [else (error 'qp-encode-header "internal error")]))))

(define (encode-line-for-header s)
  (define (do-encode s string->bytes charset encode encoding)
    (let loop ([s s])
      (cond
        ;; Find ASCII (and no "=") prefix before a space
        [(regexp-match #rx"^([\u0-\u3c\u3e-\u7F]* )(.*)$" s)
         => (λ (m) (string-append (cadr m) (loop (caddr m))))]
        ;; Find ASCII (and no "=") suffix after a space
        [(regexp-match #rx"^(.*?)( [\u0-\u3c\u3e-\u7F]*)$" s)
         => (λ (m) (string-append (loop (cadr m)) (caddr m)))]
        [else
         ;; Split lines longer than 70 chars and re-assemble (see above
         ;; comment).  Note that the proper separator should use CRLF,
         ;; but we're sending this to a sendmail process that will take
         ;; care of that level.
         (let loop ([bytes (string->bytes s)])
           (if ((bytes-length bytes) . > . 70)
             (string-append (loop (subbytes bytes 0 70))
                            "\n "
                            (loop (subbytes bytes 70)))
             (format "=?~a?~a?~a?=" charset encoding (encode bytes))))])))
  (cond
    ;; ASCII - do nothing
    [(not (regexp-match? re:non-ascii s)) s]
    ;; Not Latin-1, so use UTF-8
    [(regexp-match? #rx"[^\u0-\uFF]" s)
     (do-encode s string->bytes/utf-8 "UTF-8" base64-encode-header "B")]
    ;; use Latin-1
    [else
     (do-encode s string->bytes/latin-1 "ISO-8859-1" qp-encode-header "Q")]))

;; ----------------------------------------

(define re:encoded #rx#"^(.*?)=[?]([^?]+)[?]([qQbB])[?](.*?)[?]=(.*)$")

(define (generalize-encoding encoding)
  ;; Treat Latin-1 as Windows-1252 and also threat GB and GB2312
  ;; as GBK, because some mailers are broken.
  (cond [(or (regexp-match? #rx#"^(?i:iso-8859-1)$" encoding)
             (regexp-match? #rx#"^(?i:us-ascii)$" encoding))
         (if (bytes? encoding) #"WINDOWS-1252" "WINDOWS-1252")]
        [(regexp-match? #rx#"^(?i:gb(?:2312)?)$" encoding)
         (if (bytes? encoding) #"GBK" "GBK")]
        [(regexp-match? #rx#"^(?i:ks_c_5601-1987)$" encoding)
         (if (bytes? encoding) #"CP949" "CP949")]
        [else encoding]))

(define (decode-for-header s)
  (and s
       (let ([m (regexp-match re:encoded
                              (string->bytes/latin-1 s (char->integer #\?)))])
         (if m
           (let ([s ((if (member (cadddr m) '(#"q" #"Q"))
                       ;; quoted-printable, with special _ handling
                       (λ (x) (qp-decode (regexp-replace* #rx#"_" x #" ")))
                       ;; base64:
                       base64-decode)
                     (cadddr (cdr m)))]
                 [encoding (caddr m)])
             (string-append
              (decode-for-header (bytes->string/latin-1 (cadr m)))
              (let ([encoding (generalize-encoding encoding)])
                (cond
                  [(regexp-match? #rx#"^(?i:utf-8)$" encoding)
                   (bytes->string/utf-8 s #\?)]
                  [else (let ([c (bytes-open-converter
                                  (bytes->string/latin-1 encoding)
                                  "UTF-8")])
                          (if c
                            (let-values ([(r got status)
                                          (bytes-convert c s)])
                              (bytes-close-converter c)
                              (if (eq? status 'complete)
                                (bytes->string/utf-8 r #\?)
                                (bytes->string/latin-1 s)))
                            (bytes->string/latin-1 s)))]))
              (let ([rest (cadddr (cddr m))])
                (let ([rest
                       ;; A CR-LF-space-encoding sequence means that we
                       ;; should drop the space.
                       (if (and (> (bytes-length rest) 4)
                                (= 13 (bytes-ref rest 0))
                                (= 10 (bytes-ref rest 1))
                                (= 32 (bytes-ref rest 2))
                                (let ([m (regexp-match-positions
                                          re:encoded rest)])
                                  (and m (= (caaddr m) 5))))
                         (subbytes rest 3)
                         rest)])
                  (decode-for-header (bytes->string/latin-1 rest))))))
           s))))
