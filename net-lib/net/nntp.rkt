#lang racket/base

(require racket/tcp)

(provide (struct-out communicator)
         connect-to-server connect-to-server* disconnect-from-server
         authenticate-user open-news-group
         head-of-message body-of-message
         newnews-since generic-message-command
         make-desired-header extract-desired-headers

         (struct-out nntp)
         (struct-out unexpected-response)
         (struct-out bad-status-line)
         (struct-out premature-close)
         (struct-out bad-newsgroup-line)
         (struct-out non-existent-group)
         (struct-out article-not-in-group)
         (struct-out no-group-selected)
         (struct-out article-not-found)
         (struct-out authentication-rejected))

;; sender : oport
;; receiver : iport
;; server : string
;; port : number

(define-struct communicator (sender receiver server port))

;; code : number
;; text : string
;; line : string
;; communicator : communicator
;; group : string
;; article : number

(define-struct (nntp exn) ())
(define-struct (unexpected-response nntp) (code text))
(define-struct (bad-status-line nntp) (line))
(define-struct (premature-close nntp) (communicator))
(define-struct (bad-newsgroup-line nntp) (line))
(define-struct (non-existent-group nntp) (group))
(define-struct (article-not-in-group nntp) (article))
(define-struct (no-group-selected nntp) ())
(define-struct (article-not-found nntp) (article))
(define-struct (authentication-rejected nntp) ())

;; signal-error :
;; (exn-args ... -> exn) x format-string x values ... ->
;;   exn-args -> ()

;; - throws an exception

(define (signal-error constructor format-string . args)
  (lambda exn-args
    (raise (apply constructor
                  (apply format format-string args)
                  (current-continuation-marks)
                  exn-args))))

;; default-nntpd-port-number :
;; number

(define default-nntpd-port-number 119)

;; connect-to-server*:
;; input-port output-port -> communicator

(define connect-to-server*
  (case-lambda
   [(receiver sender)
    (connect-to-server* receiver sender "unspecified" "unspecified")]
   [(receiver sender server-name port-number)
    (file-stream-buffer-mode sender 'line)
    (let ([communicator (make-communicator sender receiver server-name
                                           port-number)])
      (let-values ([(code response)
                    (get-single-line-response communicator)])
        (case code
          [(200 201) communicator]
          [else ((signal-error make-unexpected-response
                               "unexpected connection response: ~s ~s"
                               code response)
                 code response)])))]))

;; connect-to-server :
;; string [x number] -> commnicator

(define connect-to-server
  (lambda (server-name (port-number default-nntpd-port-number))
    (let-values ([(receiver sender)
                  (tcp-connect server-name port-number)])
      (connect-to-server* receiver sender server-name port-number))))

;; close-communicator :
;; communicator -> ()

(define (close-communicator communicator)
  (close-input-port (communicator-receiver communicator))
  (close-output-port (communicator-sender communicator)))

;; disconnect-from-server :
;; communicator -> ()

(define (disconnect-from-server communicator)
  (send-to-server communicator "QUIT")
  (let-values ([(code response)
                (get-single-line-response communicator)])
    (case code
      [(205)
       (close-communicator communicator)]
      [else
       ((signal-error make-unexpected-response
                      "unexpected dis-connect response: ~s ~s"
                      code response)
        code response)])))

;; authenticate-user :
;; communicator x user-name x password -> ()
;; the password is not used if the server does not ask for it.

(define (authenticate-user communicator user password)
  (define (reject code response)
    ((signal-error make-authentication-rejected
                   "authentication rejected (~s ~s)"
                   code response)))
  (define (unexpected code response)
    ((signal-error make-unexpected-response
                   "unexpected response for authentication: ~s ~s"
                   code response)
     code response))
  (send-to-server communicator "AUTHINFO USER ~a" user)
  (let-values ([(code response) (get-single-line-response communicator)])
    (case code
      [(281) (void)] ; server doesn't ask for a password
      [(381)
       (send-to-server communicator "AUTHINFO PASS ~a" password)
       (let-values ([(code response)
                     (get-single-line-response communicator)])
         (case code
           [(281) (void)] ; done
           [(502) (reject code response)]
           [else (unexpected code response)]))]
      [(502) (reject code response)]
      [else (reject code response)
            (unexpected code response)])))

;; send-to-server :
;; communicator x format-string x list (values) -> ()

(define (send-to-server communicator message-template . rest)
  (let ([sender (communicator-sender communicator)])
    (apply fprintf sender
           (string-append message-template "\r\n")
           rest)
    (flush-output sender)))

;; parse-status-line :
;; string -> number x string

(define (parse-status-line line)
  (if (eof-object? line)
      ((signal-error make-bad-status-line "eof instead of a status line")
       line)
      (let ([match (cdr (or (regexp-match #rx"([0-9]+) (.*)" line)
                            ((signal-error make-bad-status-line
                                           "malformed status line: ~s" line)
                             line)))])
        (values (string->number (car match))
                (cadr match)))))

;; get-one-line-from-server :
;; iport -> string

(define (get-one-line-from-server server->client-port)
  (read-line server->client-port 'return-linefeed))

;; get-single-line-response :
;; communicator -> number x string

(define (get-single-line-response communicator)
  (let* ([receiver (communicator-receiver communicator)]
         [status-line (get-one-line-from-server receiver)])
    (parse-status-line status-line)))

;; get-rest-of-multi-line-response :
;; communicator -> list (string)

(define (get-rest-of-multi-line-response communicator)
  (let ([receiver (communicator-receiver communicator)])
    (let loop ([r '()])
      (let ([l (get-one-line-from-server receiver)])
        (cond
          [(eof-object? l)
           ((signal-error make-premature-close
                          "port prematurely closed during multi-line response")
            communicator)]
          [(string=? l ".")  (reverse r)]
          [(string=? l "..") (loop (cons "." r))]
          [else              (loop (cons l r))])))))

;; get-multi-line-response :
;; communicator -> number x string x list (string)

;; -- The returned values are the status code, the rest of the status
;; response line, and the remaining lines.

(define (get-multi-line-response communicator)
  (let* ([receiver (communicator-receiver communicator)]
         [status-line (get-one-line-from-server receiver)])
    (let-values ([(code rest-of-line)
                  (parse-status-line status-line)])
      (values code rest-of-line (get-rest-of-multi-line-response communicator)))))

;; open-news-group :
;; communicator x string -> number x number x number

;; -- The returned values are the number of articles, the first
;; article number, and the last article number for that group.

(define (open-news-group communicator group-name)
  (send-to-server communicator "GROUP ~a" group-name)
  (let-values ([(code rest-of-line)
                (get-single-line-response communicator)])
    (case code
      [(211)
       (let ([match (map string->number
                         (cdr
                          (or (regexp-match #rx"([0-9]+) ([0-9]+) ([0-9]+)" rest-of-line)
                              ((signal-error make-bad-newsgroup-line
                                             "malformed newsgroup open response: ~s"
                                             rest-of-line)
                               rest-of-line))))])
         (let ([number-of-articles (car match)]
               [first-article-number (cadr match)]
               [last-article-number (caddr match)])
           (values number-of-articles
                   first-article-number
                   last-article-number)))]
      [(411)
       ((signal-error make-non-existent-group
                      "group ~s does not exist on server ~s"
                      group-name (communicator-server communicator))
        group-name)]
      [else
       ((signal-error make-unexpected-response
                      "unexpected group opening response: ~s" code)
        code rest-of-line)])))

;; generic-message-command :
;; string x number -> communicator x (number U string) -> list (string)

(define (generic-message-command command ok-code)
  (lambda (communicator message-index)
    (send-to-server communicator (string-append command " ~a")
                    (if (number? message-index)
                        (number->string message-index)
                        message-index))
    (let-values ([(code response)
                  (get-single-line-response communicator)])
      (if (= code ok-code)
          (get-rest-of-multi-line-response communicator)
          (case code
            [(423)
             ((signal-error make-article-not-in-group
                            "article id ~s not in group" message-index)
              message-index)]
            [(412)
             ((signal-error make-no-group-selected
                            "no group selected"))]
            [(430)
             ((signal-error make-article-not-found
                            "no article id ~s found" message-index)
              message-index)]
            [else
             ((signal-error make-unexpected-response
                            "unexpected message access response: ~s" code)
              code response)])))))

;; head-of-message :
;; communicator x (number U string) -> list (string)

(define head-of-message
  (generic-message-command "HEAD" 221))

;; body-of-message :
;; communicator x (number U string) -> list (string)

(define body-of-message
  (generic-message-command "BODY" 222))

;; newnews-since :
;; communicator x (number U string) -> list (string)

(define newnews-since
  (generic-message-command "NEWNEWS" 230))

;; make-desired-header :
;; string -> desired

(define (make-desired-header raw-header)
  (regexp
   (string-append
    "^"
    (list->string
     (apply append
            (map (lambda (c)
                   (cond
                    [(char-lower-case? c)
                     (list #\[ (char-upcase c) c #\])]
                    [(char-upper-case? c)
                     (list #\[ c (char-downcase c) #\])]
                    [else
                     (list c)]))
                 (string->list raw-header))))
    ":")))

;; extract-desired-headers :
;; list (string) x list (desired) -> list (string)

(define (extract-desired-headers headers desireds)
  (filter (lambda (header)
            (ormap (lambda (matcher) (regexp-match matcher header))
                   desireds))
          headers))
