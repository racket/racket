;; Mike Burns, July 8th, 2004, netgeek@speakeasy.net
;; Useful assertions
(module assertions mzscheme
  (require (lib "test.ss" "schemeunit")
           (lib "web-server.ss" "web-server")
           (lib "configuration.ss" "web-server")
           (lib "url.ss" "net")
           (lib "head.ss" "net")
           (lib "contract.ss"))

  (provide assert-serve assert-serve/string assert-status-number assert-with-server)
  (provide/contract
    (start-server (-> (-> any)))
    (full-url (string? . -> . url?))
    (web-root path?)
    (input-port-equal? (input-port? input-port? . -> . boolean?))
    (THE-PORT number?)
    (THE-IP string?))

  (define THE-PORT 8135)

  (define THE-IP "127.0.0.1")

  (define web-root (build-path (collection-path "tests")
                               "web-server"
                               "scheme-units"
                               "test-web-root"))

  (define-simple-assertion (assert-serve url-path file-path content-type)
    (assert-with-server
      url-path
      (lambda (http-port)
        (and (content-type-equal? (purify-port http-port) content-type)
             (call-with-input-file
               file-path
               (lambda (f-port)
                 (input-port-equal? http-port f-port)))))))

  (define-simple-assertion (assert-serve/string url-path str content-type)
    (assert-with-server
      url-path
      (lambda (http-port)
        (and (content-type-equal? (purify-port http-port) content-type)
             (let ((is (open-input-string str)))
               (input-port-equal? http-port is))))))

  (define-simple-assertion (assert-status-number url-path status-number)
    (assert-with-server
      url-path
      (lambda (http-port)
        (regexp-match
          (format "^HTTP/... ~a" status-number)
          (read-line http-port)))))

  (define-simple-assertion (assert-with-server url-path assertion)
    ;; Ordering matters, so use let*
    (let* ((stop-server (start-server))
           (http-port (get-impure-port (full-url url-path))))
      (begin0
        (assertion http-port)
        (stop-server))))

  ;; Format a URL
  (define (full-url url-path)
    (string->url (format "http://~a:~a~a" THE-IP THE-PORT url-path)))

  ;; Start the Web server
  (define (start-server)
      (serve (load-configuration (expand-path "configuration-table")) THE-PORT THE-IP))

  ;; It is a HTML MIME header if the Content-type header exists as a string,
  ;; and is "text/plain". In practice, it can be anything matching the regexp
  ;; "^text/plain;*.*", but not in the PLT Web server.
  (define/contract content-type-equal?
    (string? string? . -> . boolean?)
    (lambda (header content-type)
      (let ((header-content-type (extract-field "Content-type" header)))
        (and header-content-type
             (string? header-content-type)
             (string=? header-content-type content-type)))))

  ;; Two input ports are equal if each line read from them are equal, and they
  ;; are the same size.
  (define (input-port-equal? a b)
    (let ((a-line (read-line a))
          (b-line (read-line b)))
      (cond
        ((eof-object? a-line) (eof-object? b-line))
        ((eof-object? b-line) (eof-object? a-line))
        ((equal? a-line b-line) (input-port-equal? a b))
        (else #f))))

  )
