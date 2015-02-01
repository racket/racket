#lang racket/base

(require racket/contract
         racket/class ; for cookie-jar interface & class
         racket/list
         racket/match
         (only-in racket/bytes bytes-join) ; for building the Cookie: header
         srfi/19
         "common.rkt"
         ;web-server/http/request-structs
         ; The above is commented out because, although it'd be clean to reuse
         ; header structs, I don't want to create a dependency on the
         ; web-server-lib package. I'm leaving it in as comments, in case
         ; net/head acquires a similar facility at some point.
         net/url ; used in path matching
         (only-in racket/date date->seconds)
         (only-in racket/string string-join string-trim string-split)
         (only-in srfi/13 string-index-right)
         )

(struct ua-cookie [name value domain path
                        expiration-time creation-time [access-time #:mutable]
                        persistent? host-only? secure-only? http-only?]
  #:transparent)

(provide (contract-out
          (struct ua-cookie ([name            cookie-name?]
                             [value           cookie-value?]
                             [domain          domain-value?]
                             [path            path/extension-value?]
                             [expiration-time integer?]
                             [creation-time   (and/c integer? positive?)]
                             [access-time     (and/c integer? positive?)]
                             [persistent?     boolean?]
                             [host-only?      boolean?]
                             [secure-only?    boolean?]
                             [http-only?      boolean?]))

          [extract-and-save-cookies!
           (->* ((listof (cons/c bytes? bytes?))
                 url?)
                ((-> bytes? string?))
               void?)]
          [save-cookie! (->* (ua-cookie?) (boolean?) void?)]
          [cookie-header (->* (url?)
                              ((-> string? bytes?)
                               #:filter-with (-> ua-cookie? boolean?))
                              (or/c bytes? #f))]

          [current-cookie-jar (parameter/c (is-a?/c cookie-jar<%>))]
          [list-cookie-jar% 
           (class/c [save-cookies! (->*m ((listof ua-cookie?)) (boolean?) void?)]
                    [save-cookie!  (->*m (ua-cookie?)          (boolean?) void?)]
                    [cookies-matching
                     (->*m (url?) (boolean?) (listof ua-cookie?))])]

          [extract-cookies
           (->* ((listof (cons/c bytes? bytes?))
                 ;(listof (or/c header? (cons/c bytes? bytes?)))
                 url?)
                ((-> bytes? string?))
                (listof ua-cookie?))]
          [parse-cookie (-> bytes? url? (or/c ua-cookie? #f))]

          [default-path (-> url? string?)]

          [min-cookie-seconds (and/c integer? negative?)]
          [max-cookie-seconds (and/c integer? positive?)]
          [parse-date    (-> string? (or/c date? #f))]
          )
         cookie-jar<%>
         )

;;;;;;;;;;;;;;;;;;;;; Storing Cookies ;;;;;;;;;;;;;;;;;;;;;

;; for saving all cookies from a Set-Cookie header
(define (extract-and-save-cookies! headers url [decode bytes->string/utf-8])
  (send (current-cookie-jar)
        save-cookies! (extract-cookies headers url decode)
        (and (member (url-scheme url) '("http" "https")) #t)))

;; ua-cookie? [boolean?] -> void?
;; for saving a single cookie (already parsed), received via an HTTP API
;; iff via-http? is #t.
(define (save-cookie! c [via-http? #t])
  (send (current-cookie-jar) save-cookie! c via-http?))

;; url? (-> string? bytes?) #:filter-with [ua-cookie? -> boolean?] -> bytes?
;; for producing a header from the cookie jar, for requests to given url.
;; NOTE: this produces only the VALUE portion of the header, not the ``Cookie:''
;; part; I'm not sure if users will prefer to use web-server/http/request-structs
;; or net/head to construct their headers, or manually construct them to feed
;; to http-sendrecv and friends.
(define (cookie-header url
                       [encode string->bytes/utf-8]
                       #:filter-with [ok? (lambda (x) #t)])
  (define (make-cookie-pair c)
    (bytes-append (encode (ua-cookie-name c))
                  #"=" (encode (ua-cookie-value c))))
  (define cookie-pairs
    (for/list ([c (in-list
                   (filter ok?
                           (send (current-cookie-jar) cookies-matching url)))])
      (make-cookie-pair c)))
  (and (not (null? cookie-pairs)) (bytes-join cookie-pairs #"; ")))

;;;; The cookie jar:

(define cookie-jar<%>
  (interface ()
    ; TODO: Modify the below to take optional URL
    [save-cookie!  (->*m (ua-cookie?)          (boolean?) void?)]
    [save-cookies! (->*m ((listof ua-cookie?)) (boolean?) void?)]
    [cookies-matching (->m url? (listof ua-cookie?))]))

;; ua-cookie [Int+] -> Boolean
(define (cookie-expired? cookie [current-time (current-seconds)])
  (> current-time (ua-cookie-expiration-time cookie)))

;; Represents the cookie jar as a list of cookies, sorted in ascending order
;; by length of path, with ties broken by later-ctime-first.
(define list-cookie-jar%
  (class* object% (cookie-jar<%>)
    (super-new)
    (field [cookies '()])

    (define/public (save-cookie! c [via-http? #t])
      (set! cookies (insert c cookies via-http?)))

    (define/public (save-cookies! cs [via-http? #t])
      (for ([c cs]) (save-cookie! c via-http?)))

    ;; insert : ua-cookie? (listof ua-cookie?) [boolean?] -> (listof ua-cookie?)
    ;; Inserts new-cookie into the given list, maintaining sort order, unless
    ;; it was received via a non-HTTP API (as indicated by via-http?) and should
    ;; be ignored per section 5.3 of RFC6265.
    (define (insert new-cookie jar via-http?)
      (match-define (ua-cookie name _ dom path _ ctime _ _ _ _ http-only?)
        new-cookie)
      (if (and http-only? (not via-http?))   ; ignore -- see Sec 5.3.10
          jar
          (let insert-into ([jar jar]) ; != Binks
            (cond
              [(null? jar) (if (cookie-ok? new-cookie) (list new-cookie) '())]
              [else
               (match-define (ua-cookie name2 _ dom2 path2 _ ctime2 _ _ _ _ ho2?)
                 (car jar))
               (cond
                 [(and (string=? name name2) (string=? dom dom2)
                       (string=? path path2)) ; Replace this cookie.
                  (filter cookie-ok?
                          (if (and ho2? (not via-http?))
                              jar  ; ignore new cookie -- see Sec 5.3.11.2.
                              (cons (struct-copy ua-cookie new-cookie
                                                 [creation-time ctime2])
                                    (cdr jar))))]
                 [(let ([plen  (string-length path)]
                        [plen2 (string-length path2)])
                    (or (< plen plen2) (and (= plen plen2) (> ctime ctime2))))
                  ;; Shorter path, or eq path and later ctime, comes first.
                  (filter cookie-ok? (cons new-cookie jar))]
                 [(cookie-ok? (car jar))
                  (cons (car jar) (insert-into (cdr jar)))]
                 [else (insert-into (cdr jar))])]))))

    (define (cookie-ok? c) (not (cookie-expired? c)))

    (define/public (cookies-matching url
                                     [secure? (equal? (url-scheme url) "https")])
      (define host (url-host url))
      (define (match? cookie)
        (and (domain-match? (ua-cookie-domain cookie) host)
             (path-match? (ua-cookie-path cookie) url)
             (or secure? (not (ua-cookie-secure-only? cookie)))))
      ;; Produce the cookies in reverse order (ie, desc by path length):
      (for/fold ([cs '()]) ([c (in-list cookies)])
        (if (match? c) (cons c cs) cs)))
    ))

;; The cookie jar that will be used for saving new cookies, and for choosing
;; cookies to send to the server.
(define current-cookie-jar (make-parameter (new list-cookie-jar%)))

;;;;;;;;;;;;;;;;;;;;; Reading the Set-Cookie header ;;;;;;;;;;;;;;;;;;;;;

;; given a list of all the headers received in a response,
;; produce a list of cookies corresponding to all the Set-Cookie headers
;; present. TODO: tests
(define (extract-cookies headers url [decode bytes->string/utf-8])
  (define (set-cookie? x) (string-ci=? (decode x) "set-cookie"))
  (define (header->maybe-cookie hdr)
    (match hdr
      [(cons (? set-cookie?) value) value]
      ;[(header (? set-cookie?) value) value]
      [_ #f]))
  (filter (lambda (x) x)
          (for/list ([header-value (filter-map header->maybe-cookie headers)])
            (parse-cookie header-value url decode))))

;; parse-cookie : bytes? url? [(bytes? -> string?)] -> (Option ua-cookie?)
;; Given the value from a Set-Cookie: header, produce a ua-cookie, or #f
;; if the byte-string doesn't contain an adequately well-formed cookie.
(define (parse-cookie set-cookie-bytes url [decode bytes->string/utf-8])
  (let/ec esc
    (define (ignore-this-Set-Cookie) (esc #f))
    (define now (current-seconds))
    
    (match-define (list-rest nvpair unparsed-attributes)
      (string-split (decode set-cookie-bytes) ";"))
    
    (define-values (name value)
      (match (regexp-match nvpair-regexp nvpair)
        [(list all "" v) (ignore-this-Set-Cookie)]
        [(list all n v)  (values n v)]
        [#f (ignore-this-Set-Cookie)]))

    ;;; parsing the unparsed-attributes
    (define-values (domain-attribute path expires max-age secure? http-only?)
      (parse-cookie-attributes unparsed-attributes url))   

    (define-values (host-only? domain)
      (let ([request-host (url-host url)])
        (cond
          [domain-attribute
           (when (or (string=? domain-attribute "")
                     (not (domain-match? domain-attribute request-host)))
             (ignore-this-Set-Cookie))
           (values #f domain-attribute)]
          [else
           (values #t request-host)])))
    (define-values (persistent? expiry-time)
      (cond [max-age (values #t (if (positive? max-age)
                                    (+ now max-age)
                                    min-cookie-seconds))]
            [expires (values #t (max min-cookie-seconds
                                     (min max-cookie-seconds
                                          (date->seconds expires))))]
            [else    (values #f max-cookie-seconds)]))

    (ua-cookie name value
               ;; TODO: allow UA to "reject public suffixes", sec 5.3
               domain
               (or path (default-path url))
               expiry-time now now
               persistent? host-only? secure? http-only?)))

;; parse-cookie-attributes :
;;   bytes? url? -> (values (Option string?) (Option string?)
;;                          (Option Nat) (Option Nat)
;;                           Bool Bool)
(define (parse-cookie-attributes unparsed-attributes url)
  (for/fold ([domain #f] [path #f] [expires #f] [max-age #f]
             [secure? #f] [http-only? #f])
            ([cookie-av unparsed-attributes])
    (cond
      [(equal? cookie-av "") ; skip blank a/v pairs
       (values domain path expires max-age secure? http-only?)]
      [else
       (define-values (name value)
         (match (regexp-match nvpair-regexp cookie-av)
           [(list all n v) (values n v)]
           [#f             (values (string-trim cookie-av) "")]))
       (case (string-downcase name)
         [("expires")
          (values domain path (parse-date value) max-age secure? http-only?)]
         [("max-age")
          (values domain path expires
                  (if (regexp-match #px"^-?\\d+$" value)
                      (string->number value)
                      max-age)
                  secure? http-only?)]
         [("domain")
          (values (cond
                    [(string=? value "") domain] ; don't set domain now
                    [(char=? (string-ref value 0) #\.)
                     (string-downcase (substring value 1))]
                     [else (string-downcase value)])
                  path expires max-age secure? http-only?)]
         [("path")
          (values domain
                  (if (or (string=? value "")
                          (not (char=? (string-ref value 0) #\/)))
                      path ; skip setting path this iteration
                      value)
                  expires max-age secure? http-only?)]
         [("secure")
          (values domain path expires max-age #t http-only?)]
         [("httponly")
          (values domain path expires max-age secure? #t)]
         [else
          (values domain path expires max-age secure? http-only?)])])))

;; Regexp for matching an equals-sign-delimited name-value pair,
;; and trimming it of whitespace:
(define nvpair-regexp #px"^\\s*(.*?)\\s*=\\s*(.*)\\s*$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Constant defs for date parsing ;;;;

;; Greatest and least dates this cookie library accepts:
(define max-cookie-seconds (- (expt 2 32) 1))
(define min-cookie-seconds (- max-cookie-seconds))

;; Characters used as delimiters between parts of a date string
;; used in the "Expires" attribute.
(define (range+ a b) (cons b (range a b)))
(define delimiter `(#x09 ,@(range+ #x20 #x2F)
                         ,@(range+ #x3B #x40)
                         ,@(range+ #x5B #x60)
                         ,@(range+ #x7B #x7E)))

(define month-names
  `("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))

;; string? -> date?
;; As specified in section 5.1.1 of RFC6265.
(define (parse-date str)
  (let/ec escape
    (define (fail) (escape #f))

    (define tokens
      (let ()
        (define-values (acc current)
          (for/fold ([acc null] [current null])
                    ([ch (in-string str)])
            (if (memv (char->integer ch) delimiter)
                (values (cons (list->string (reverse current)) acc) null)
                (values acc (cons ch current)))))
        (reverse (if (null? current)
                     acc
                     (cons (list->string (reverse current)) acc)))))

    ;; String -> (Option (List Int[0,23] Int[0,59] Int[0,59]))
    (define (parse-time str)
      (match (regexp-match #px"^(\\d\\d?):(\\d\\d?):(\\d\\d?)\\D*$" str)
        [(list _ hs ms ss)
         (define-values (h m s)
           (apply values (map string->number (list hs ms ss))))
         (if (and (<= h 23) (<= m 59) (<= s 59))
             (list h m s)
             (fail))] ; malformed time
        [_ #f]))

    (define (parse-day str) ; String -> (Option Int[1,31])
      (match (regexp-match #px"^(\\d\\d?)\\D*$" str)
        [(list _ day) (string->number day)]
        [_ #f]))

    (define (parse-year str) ; String -> (Option Int[>= 1601])
      (match (regexp-match #px"^(\\d\\d\\d?\\d?)\\D*$" str)
        [(list _ year/s)
         (define year (string->number year/s))
         (cond [(<= 70 year 99) (+ year 1900)]
               [(<= 0 year 69)  (+ year 2000)]
               [(< year 1601)   (fail)]
               [else            year])]
        [_ #f]))

    (define (parse-month str) ; String -> Int[1,12]
      (cond
        [(>= (string-length str) 3)
         (define prefix (string-downcase (substring str 0 3)))
         (for/or ([m (in-list month-names)]
                  [n (in-naturals)])
           (if (string=? m prefix) (add1 n) #f))]
        [else #f]))

    (define-values (time day month year)
      (for/fold ([time #f] [day #f] [month #f] [year #f])
                ([token (in-list tokens)])
        (cond
          [(and (not time) (parse-time token))
           => (λ (time)  (values time day month year))]
          [(and (not day) (parse-day token))
           => (λ (day)   (values time day month year))]
          [(and (not month) (parse-month token))
           => (λ (month) (values time day month year))]
          [(and (not year) (parse-year token))
           => (λ (year)  (values time day month year))]
          [else (values time day month year)])))

    (if time
       (let-values ([(hour minute second) (apply values time)])
         ;; Last check: fail if day is not OK for given month:
         (and day month year
              (<= day (case month
                        [(1 3 5 7 8 10 12)  31]
                        [(2)                29]
                        [(4 6 9 11)         30]
                        [else           (fail)]))
              (date second minute hour day month
                    year
                    0 0 #f 0)))
       #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Domains ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; String String -> Boolean
;; As specified in section 5.1.3: "A string domain-matches a given
;; domain string if at least one of the following conditions hold..."
;; domain is the "domain string", and host is the string being tested.
(define (domain-match? domain host)
  (define diff (- (string-length host) (string-length domain)))
  (and (diff . >= . 0)
       (string=? domain (substring host diff))
       (or (= diff 0) (char=? (string-ref host (sub1 diff)) #\.))
       (not (regexp-match #px"\\.\\d\\d?\\d?$" host))))

;;;; As spec'd in section 5.1.4:

;; url? -> string?
;; compute the default-path of a cookie, for use in creating the ua-cookie struct
;; when parsing a Set-Cookie header.
(define (default-path url)
  (define uri-path
    (string-append "/" (string-join (map path/param-path (url-path url)) "/")))
  (if (or (= (string-length uri-path) 0)
          (not (char=? (string-ref uri-path 0) #\/)))
      "/"
      (let ([last-slash-pos (string-index-right uri-path #\/)])
        (if (= last-slash-pos 0) ; uri-path contains only one slash
            "/"
            (substring uri-path 0 last-slash-pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; path-match? : String (U URL String) -> Boolean
;; Does the URL's (typically the one to which the UA is sending the request)
;; request-path path-match the given cookie-path?
(define (path-match? cookie-path url)
  (define (url-full-path url)
    (cond
      [(url? url)
       (string-append "/"
                      (string-join (map path/param-path (url-path url)) "/"))]
      [else (url-full-path (string->url url))]))
  (define request-path
    (cond
      [(string? url) url]
      [(url? url)    (url-full-path url)]))
  (define cookie-len (string-length cookie-path))
  (define request-path-len (string-length request-path))
  
  (and (<= cookie-len request-path-len)
       (string=? (substring request-path 0 cookie-len) cookie-path)
       (or (char=? (string-ref cookie-path (sub1 cookie-len)) #\/)
           (and (< cookie-len request-path-len)
                (char=? (string-ref request-path cookie-len) #\/)))))


