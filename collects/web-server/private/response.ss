(module response mzscheme
  (require (lib "contract.ss")
           (lib "port.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "xml.ss" "xml")
           "connection-manager.ss"
           "../private/request-structs.ss"
           "../private/response-structs.ss"
           "util.ss")
  
  (provide/contract
   [rename ext:output-response output-response (connection? response? . -> . void)]
   [rename ext:output-response/method output-response/method (connection? response? symbol? . -> . void)]
   [rename ext:output-file output-file (connection? path? symbol? bytes? integer? integer? . -> . void)])
  
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
  ;;    private/request.ss
  ;;
  ;; 2. In the case of a chunked response when close? = #f, then the response
  ;;    must be compliant with http 1.0. In this case the chunked response is
  ;;    simply turned into a non-chunked one.
  
  (define (output-response conn resp)
    (output-response/method conn resp 'get))
  
  ; XXX Check method in response
  (define (output-response/method conn resp meth)
    (define bresp (response->response/basic (connection-close? conn) resp))
    (output-headers+response/basic conn bresp)
    (unless (eq? meth 'head)
      (output-response/basic conn bresp)))
  
  (define (response->response/basic close? resp)
    (cond
      [(response/full? resp)
       (make-response/full 
        (response/basic-code resp)
        (response/basic-message resp)
        (response/basic-seconds resp)
        (response/basic-mime resp)
        (list* (make-header #"Content-Length" (string->bytes/utf-8 (number->string (response/full->size resp))))
               (response/basic-headers resp))
        (response/full-body resp))]
      [(response/incremental? resp)
       (if close?
           resp
           (make-response/incremental 
            (response/basic-code resp)
            (response/basic-message resp)
            (response/basic-seconds resp)
            (response/basic-mime resp)
            (list* (make-header #"Transfer-Encoding" #"chunked")
                   (response/basic-headers resp))
            (response/incremental-generator resp)))]
      [(and (pair? resp) (bytes? (car resp)))
       (response->response/basic
        close?
        (make-response/full 200 "Okay" (current-seconds) (car resp) empty
                            (cdr resp)))]
      [else
       (response->response/basic
        close?
        (make-response/full 200 "Okay" (current-seconds) TEXT/HTML-MIME-TYPE empty
                            (list (xexpr->string resp))))]))
  
  ;; Write the headers portion of a response to an output port.
  ;; NOTE: According to RFC 2145 the server should write HTTP/1.1
  ;;       header for *all* clients.
  (define (output-headers+response/basic conn bresp)
    (define o-port (connection-o-port conn))
    (fprintf o-port "HTTP/1.1 ~a ~a\r\n" (response/basic-code bresp) (response/basic-message bresp))
    (for-each (match-lambda
                [(struct header (field value))
                 (fprintf o-port "~a: ~a\r\n" field value)])
              (list* (make-header #"Date" (string->bytes/utf-8 (seconds->gmt-string (current-seconds))))
                     (make-header #"Last-Modified" (string->bytes/utf-8 (seconds->gmt-string (response/basic-seconds bresp))))
                     (make-header #"Server" #"PLT Scheme")
                     (make-header #"Content-Type" (response/basic-mime bresp))
                     (append (if (connection-close? conn)
                                 (list (make-header #"Connection" #"close"))
                                 empty)
                             (response/basic-headers bresp))))
    (fprintf o-port "\r\n"))
  
  (define (output-response/basic conn bresp)
    (define o-port (connection-o-port conn))
    (match bresp
      [(? response/full?)
       (for-each
        (lambda (str) (display str o-port))
        (response/full-body bresp))]
      [(? response/incremental?)
       (if (connection-close? conn)
           ((response/incremental-generator bresp)
            (lambda chunks
              (for-each (lambda (chunk) (display chunk o-port)) chunks)))
           (begin
             ((response/incremental-generator bresp)
              (lambda chunks
                (fprintf o-port "~x\r\n"
                         (apply + 0 (map data-length chunks)))                     
                (for-each (lambda (chunk) (display chunk o-port)) chunks)
                (fprintf o-port "\r\n")))
             ; one \r\n ends the last (empty) chunk and the second \r\n ends the (non-existant) trailers
             (fprintf o-port "0\r\n\r\n")))]))
  
  (define (data-length x)
    (if (string? x)
        (data-length (string->bytes/utf-8 x))
        (bytes-length x)))
  
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
  
  (define (ext:wrap f)
    (lambda (conn . args)
      (with-handlers ([exn? (lambda (exn)
                              (kill-connection! conn)
                              (raise exn))])
        (apply f conn args)
        (flush-output (connection-o-port conn)))))
  
  (define ext:output-response
    (ext:wrap output-response))
  
  ;; response/full->size: response/full -> number
  (define (response/full->size resp)
    (apply + (map data-length (response/full-body resp))))
  
  ;; output-file: connection path symbol bytes integer integer -> void
  (define (output-file conn file-path method mime-type
                       start end-or-inf)
    (define total-len (file-size file-path))
    (define end (if (equal? +inf.0 end-or-inf)
                    total-len
                    end-or-inf))
    (define len (- end start))
    (define bresp 
      (make-response/basic 206 "Okay" (file-or-directory-modify-seconds file-path) mime-type 
                           (list (make-header #"Content-Length" (string->bytes/utf-8 (number->string len)))
                                 ; XXX Remove on non-gets?
                                 (make-header #"Content-Range" (string->bytes/utf-8 (format "bytes ~a-~a/~a" start end total-len))))))
    (output-headers+response/basic conn bresp)
    (when (eq? method 'get)
      ; XXX Move out?
      (adjust-connection-timeout! conn len) ; Give it one second per byte.
      (with-handlers ([void (lambda (e) (network-error 'output-file/partial (exn-message e)))])
        (call-with-input-file file-path
          (lambda (i-port)
            (define _ (file-position i-port start))
            (define i-port/end (make-limited-input-port i-port end #t))
            (copy-port i-port/end (connection-o-port conn))
            (close-input-port i-port/end))))))
  
  (define ext:output-file
    (ext:wrap output-file))  
  
  (define ext:output-response/method
    (ext:wrap output-response/method)))