;; Mike Burns, July 28th, 2004, netgeek@speakeasy.net
;; Test serving files that require authentication.
(module test-authentication mzscheme
  (require (lib "contract.ss")
           (lib "test.ss" "schemeunit")
           (lib "url.ss" "net")
           (lib "head.ss" "net")
           (lib "base64.ss" "net")
           "assertions.ss"
           )

  (provide/contract
    (test-authentication test-suite?))

  ;; Check for 403.
  (define-simple-assertion (assert-no-serve url-path)
    ;; Ordering matters, so use let*
    (let* ((stop-server (start-server))
           (http-port (get-impure-port (full-url url-path))))
      (begin0
        (not (equal? (read-line http-port) "HTTP/1.1 200 Okay\r"))
        (stop-server))))

  (define-simple-assertion (assert-auth url-path auth-string)
     (let* ((stop-server (start-server))
            (http-port (get-impure-port (full-url url-path)
                                        (auth-headers auth-string))))
       (begin0
         (equal? (read-line http-port) "HTTP/1.1 200 Okay\r")
         (stop-server))))

  ;; Create the headers for an authorization string.
  (define/contract auth-headers
    (bytes? . -> . (listof string?))
    (lambda (auth-string)
      (list (format "authorization: Basic ~a"
                    (base64-encode auth-string)))))

  (define test-authentication
    (make-test-suite
      (make-test-case
        "Authorization-only file without providing authorization, implicit file"
        (assert-no-serve "/secret/"))
      (make-test-case
        "Authorization-only file with provided authorization, implicit file"
        (assert-auth "/secret/" #"bubba:bbq"))
      (make-test-case
        "Authorization-only file without providing authorization, explicit file"
        (assert-no-serve "/secret/index.html"))
      (make-test-case
        "Authorization-only file with provided authorization, explicit file"
        (assert-auth "/secret/index.html" #"bubba:bbq"))))

  ;;; TODO
  ;;; browser requests file,
  ;;; browser gives 403,
  ;;; browser provides creditentials,
  ;;; server provides file

  ;;; browser requests file,
  ;;; browser gives 403,
  ;;; browser provides creditentials,
  ;;; creditentials are bogus,
  ;;; server does not provide file
  
  )
