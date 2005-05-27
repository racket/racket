(module response mzscheme
  (require (lib "list.ss")
           (lib "contract.ss")
           (lib "port.ss")
           (lib "pretty.ss")
           (lib "xml.ss" "xml")
           (lib "13.ss" "srfi")
           "connection-manager.ss")

  ;; **************************************************
  ;; DATA DEF for response
  ;; (make-response/basic number string number string  (listof (cons symbol string)))
  (define-struct response/basic (code message seconds mime extras))
  ;; (make-response/full ... (listof string))
  (define-struct (response/full response/basic) (body))
  ;; (make-response/incremental ... ((string* -> void) -> void))
  (define-struct (response/incremental response/basic) (generator))

  ; response = (cons string (listof string)), where the first string is a mime-type
  ;          | x-expression
  ;          | (make-response/full ... (listof string))
  ;          | (make-response/incremental ... ((string* -> void) -> void))

  ;; **************************************************
  ;; response?: any -> boolean
  ;; Determine if an object is a response
  (define (response? x)
    (or (and (response/basic? x)
             (number? (response/basic-code x))
             (string? (response/basic-message x))
             (number? (response/basic-seconds x))
             (bytes? (response/basic-mime x))
             (and (list? (response/basic-extras x))
                  (andmap
                    (lambda (p)
                      (and (pair? p)
                           (symbol? (car p))
                           (string? (cdr p))))
                    (response/basic-extras x))))
                                        ; this could fail for dotted lists - rewrite andmap
        (and (pair? x) (pair? (cdr x)) (andmap
                                        (lambda (x)
                                          (or (string? x)
                                              (bytes? x)))
                                        x))
                                        ; insist that the xexpr has a root element
        (and (pair? x) (xexpr? x))))


  ;; Weak contracts for output-response because the response? is checked inside
  ;; output-response, handled, etc.
  (provide/contract
   [struct (response/full response/basic)
           ([code number?]
                  [message string?]
                  [seconds number?]
                  [mime bytes?]
                  [extras (listof (cons/c symbol? string?))]
                  [body (listof (union string?
                                       bytes?))])]
   [struct (response/incremental response/basic)
           ([code number?]
            [message string?]
            [seconds number?]
            [mime bytes?]
            [extras (listof (cons/c symbol? string?))]
            [generator ((() (listof (union bytes? string?)) . ->* . any) . ->
                        . any)]
            )]
   [response? (any/c . -> . boolean?)]
   [output-response (connection? any/c . -> . any)]
   [output-response/method (connection? response? symbol? . -> . any)]
   [output-file (connection? path? symbol? bytes? . -> . any)]
   [TEXT/HTML-MIME-TYPE bytes?]
   )




  ;; Table 1. head responses:
; ------------------------------------------------------------------------------
; |method | close? | x-fer coding || response actions
; |-----------------------------------------------------------------------------
; |-----------------------------------------------------------------------------
; |head   |   #t   | chunked      || 1. Output the headers only.
; |-------------------------------|| 2. Add the special connection-close header.
; |head   |   #t   | not-chunked  ||
; |-----------------------------------------------------------------------------
; |head   |   #f   | chunked      || 1. Output the headers only.
; |-------------------------------|| 2. Don't add the connection-close header.
; |head   |   #f   | not-chunked  ||
; |-----------------------------------------------------------------------------

  ;; Table 2. get responses:
; ------------------------------------------------------------------------------
; |method | x-fer-coding | close? || response actions
; |-----------------------------------------------------------------------------
; |-----------------------------------------------------------------------------
; | get   | chunked      |   #t   || 1. Output headers as above.
; |       |              |        || 2. Generate trivial chunked response.
; |-----------------------------------------------------------------------------
; | get   | chunked      |   #f   || 1. Output headers as above.
; |       |              |        || 2. Generate chunks as per RFC 2616 sec. 3.6
; |-----------------------------------------------------------------------------
; | get   | not chunked  |   #t   || 1. Output headers as above.
; |-------------------------------|| 2. Generate usual non-chunked response.
; | get   | not chunked  |   #f   ||
; |-----------------------------------------------------------------------------

  ;; Notes:
  ;; 1. close? is a boolean which corresponds roughly to the protocol version.
  ;;    #t |-> 1.0 and #f |-> 1.1. See function close-connection? in
  ;;    #request-parsing.ss
  ;;
  ;; 2. In the case of a chunked response when close? = #f, then the response
  ;;    must be compliant with http 1.0. In this case the chunked response is
  ;;    simply turned into a non-chunked one.

  (define TEXT/HTML-MIME-TYPE #"text/html")


  ;;**************************************************
  ;; output-headers: connection number string (listof (listof String))
  ;;                 number string -> void
  ;; Write the headers portion of a response to an output port.
  ;; NOTE: According to RFC 2145 the server should write HTTP/1.1
  ;;       header for *all* clients.
  (define (output-headers conn code message extras seconds mime)
    (let ([o-port (connection-o-port conn)])
      (for-each
       (lambda (line)
         (for-each
          (lambda (word) (display word o-port))
          line)
         (fprintf o-port "\r\n"))
       (list* `("HTTP/1.1 " ,code " " ,message)
              `("Date: " ,(seconds->gmt-string (current-seconds)))
              `("Last-Modified: " ,(seconds->gmt-string seconds))
              `("Server: PLT Scheme")
              `("Content-type: " ,mime)
              (if (connection-close? conn)
                  (cons `("Connection: close") extras)
                  extras)))
      (fprintf o-port "\r\n")))

; seconds->gmt-string : Nat -> String
; format is rfc1123 compliant according to rfc2068 (http/1.1)
  (define (seconds->gmt-string s)
    (let* ([local-date (seconds->date s)]
           [date (seconds->date (- s
                                   (date-time-zone-offset local-date)
                                   (if (date-dst? local-date) 3600 0)))])
      (format "~a, ~a ~a ~a ~a:~a:~a GMT"
              (vector-ref DAYS (date-week-day date))
              (two-digits (date-day date))
              (vector-ref MONTHS (sub1 (date-month date)))
              (date-year date)
              (two-digits (date-hour date))
              (two-digits (date-minute date))
              (two-digits (date-second date)))))

; two-digits : num -> str
  (define (two-digits n)
    (let ([str (number->string n)])
      (if (< n 10) (string-append "0" str) str)))

  (define MONTHS
    #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

  (define DAYS
    #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))



  ;; **************************************************
  ;; output-response: connection response -> void
  (define (output-response conn resp)
    (cond
     [(response/full? resp)
      (output-response/basic
       conn resp (response/full->size resp)
       (lambda (o-port)
         (for-each
          (lambda (str) (display str o-port))
          (response/full-body resp))))]
     [(response/incremental? resp)
      (output-response/incremental conn resp)]
     [(and (pair? resp) (string? (car resp)))
      (output-response/basic
       conn
       (make-response/basic 200 "Okay" (current-seconds) (car resp) '())
       (apply + (map
                 (lambda (c)
                   (if (string? c)
                       (string-length c)
                       (bytes-length c)))
                 (cdr resp)))
       (lambda (o-port)
         (for-each
          (lambda (str) (display str o-port))
          (cdr resp))))]
     [else
      ;; TODO: make a real exception for this.
      (with-handlers
          ([exn:invalid-xexpr?
            (lambda (exn)
              (output-response/method
                conn
                (xexpr-exn->response exn resp)
                'ignored))]
           [exn? (lambda (exn)
                   (raise exn))])
        (let ([str (and (validate-xexpr resp) (xexpr->string resp))])
          (output-response/basic
           conn
           (make-response/basic 200
                                "Okay"
                                (current-seconds)
                                TEXT/HTML-MIME-TYPE
                                '())
           (add1 (string-length str))
           (lambda (o-port)
             (display str o-port)
             (newline o-port)))))]))

  ;; response/full->size: response/full -> number
  ;; compute the size for a response/full
  (define (response/full->size resp/f)
    (apply + (map
              (lambda (c)
                (if (string? c)
                    (string-length c)
                    (bytes-length c)))
              (response/full-body resp/f))))

  ;; **************************************************
  ;; output-file: connection path symbol bytes -> void
  (define (output-file conn file-path method mime-type)
    (output-headers conn 200 "Okay"
                    `(("Content-length: " ,(file-size file-path)))
                    (file-or-directory-modify-seconds file-path)
                    mime-type)
    (when (eq? method 'get)
      (call-with-input-file file-path
        (lambda (i-port) (copy-port i-port (connection-o-port conn))))))

  ;; **************************************************
  ;; output-response/method: connection response/full symbol -> void
  ;; If it is a head request output headers only, otherwise output as usual
  (define (output-response/method conn resp meth)
    (cond
     [(eqv? meth 'head)
      (output-headers/response conn resp `(("Content-length: "
                                            ,(response/full->size resp))))]
     [else
      (output-response conn resp)]))

  ;; **************************************************
  ;; output-headers/response: connection response (listof (listof string)) -> void
  ;; Write the headers for a response to an output port
  (define (output-headers/response conn resp extras)
    (output-headers conn
                    (response/basic-code resp)
                    (response/basic-message resp)
                    extras
                    (response/basic-seconds resp)
                    (response/basic-mime resp)))

  ;; **************************************************
  ;; output-response/basic: connection response number (o-port -> void) -> void
  ;; Write a normal response to an output port
  (define (output-response/basic conn resp size responder)
    (output-headers/response conn resp
                             `(("Content-length: " ,size)
                               . ,(extras->strings resp)))
    (responder (connection-o-port conn)))

  ;; **************************************************
  ;; output-response/incremental: connection response/incremental -> void
  ;; Write a chunked response to an output port.
  (define (output-response/incremental conn resp/inc)
    (let ([o-port (connection-o-port conn)])
      (cond
       [(connection-close? conn)
        (output-headers/response conn resp/inc '())
        ((response/incremental-generator resp/inc)
         (lambda chunks
           (for-each (lambda (chunk) (display chunk o-port)) chunks)))]
       [else
        (output-headers/response conn resp/inc
                                 `(("Transfer-Encoding: chunked")
                                   . ,(extras->strings resp/inc)))
        ((response/incremental-generator resp/inc)
         (lambda chunks
           (fprintf o-port "~x\r\n"
                    (foldl
                     (lambda (c acc)
                       (if (string? c)
                           (+ (string-length c) acc)
                           (+ (bytes-length c) acc)))
                     0 chunks))
           (for-each (lambda (chunk) (display chunk o-port)) chunks)
           (fprintf o-port "\r\n")))
           ; one \r\n ends the last (empty) chunk and the second \r\n ends the (non-existant) trailers
        (fprintf o-port "0\r\n\r\n")])))

  ;; extras->strings: response/basic -> (listof (listof string))
  ;; convert the response/basic-extras to the form used by output-headers
  (define (extras->strings r/bas)
    (map
      (lambda (xtra)
        (list (symbol->string (car xtra)) ": " (cdr xtra)))
      (response/basic-extras r/bas)))

  ;; Turn an exn:invalid-xexpr into a response.
  (define (xexpr-exn->response exn x)
    ;;; Does it matter what number I use for pretty-print-size-hook?
    (pretty-print-size-hook (lambda (v display? op) 20))
    (pretty-print-print-hook (pretty-print-hook/web-errors
                               (exn:invalid-xexpr-code exn)))
    (make-response/full
      500 "Servlet Error"
      (current-seconds)
      #"text/html"
      '()
      (list
        (string-append
          "<html><head><title>Erroneous Xexpr</title></head>"
          "<body><h1>Erroneous Xexpr</h1>"
          "<p>An Xexpr in the servlet is malformed. The exact error is</p>"
          "<pre>" (exn-message exn) "</pre>"
          "<h2>The Full Xexpr Is</h2>"
          "<pre>"
          (let ((o (open-output-string)))
            (pretty-print x o)
            (get-output-string o))
          "</pre>"))))

  ;; Format everything normally, except for the erroneous data.
  (define (pretty-print-hook/web-errors err)
    (letrec ((f-aux (lambda (v)
                      (cond
                        ((equal? v err)
                         (format
                           (string-append
                             "<span style='font-weight: bold; "
                             "color: red; background-color: white;'>"
                             "~a</span>") v))
                        ((list? v) (string-append "("
                                                  (string-join (map f-aux v)
                                                               " ")
                                                  ")"))
                        (else (format "~a" v))))))
      (lambda (v display? op)
        ((if display? display write) (f-aux v) op))))

  )
