#lang racket/base

;; Implements RFC 1939, Post Office Protocol - Version 3, Myers & Rose

#|
> (require net/pop3)
> (define c (connect-to-server "cs.rice.edu"))
> (authenticate/plain-text "scheme" "********" c)
> (get-mailbox-status c)
100
177824
> (get-message/headers c 100)
("Date: Thu, 6 Nov 1997 12:34:18 -0600 (CST)"
 "Message-Id: <199711061834.MAA11961@new-world.cs.rice.edu>"
 "From: Shriram Krishnamurthi <shriram@cs.rice.edu>"
 ...
 "Status: RO")
> (get-message/complete  c 100)
("Date: Thu, 6 Nov 1997 12:34:18 -0600 (CST)"
 "Message-Id: <199711061834.MAA11961@new-world.cs.rice.edu>"
 "From: Shriram Krishnamurthi <shriram@cs.rice.edu>"
 ...
 "Status: RO")
("some body" "text" "goes" "." "here" "." "")
> (disconnect-from-server c)
|#

(require racket/tcp)

(provide (struct-out communicator)
         connect-to-server connect-to-server* disconnect-from-server
         authenticate/plain-text
         get-mailbox-status
         get-message/complete get-message/headers get-message/body
         delete-message
         get-unique-id/single get-unique-id/all

         make-desired-header extract-desired-headers

         (struct-out pop3)
         (struct-out cannot-connect)
         (struct-out username-rejected)
         (struct-out password-rejected)
         (struct-out not-ready-for-transaction)
         (struct-out not-given-headers)
         (struct-out illegal-message-number)
         (struct-out cannot-delete-message)
         (struct-out disconnect-not-quiet)
         (struct-out malformed-server-response))

;; sender : oport
;; receiver : iport
;; server : string
;; port : number
;; state : symbol = (disconnected, authorization, transaction)

(define-struct communicator (sender receiver server port [state #:mutable]))

(define-struct (pop3 exn) ())
(define-struct (cannot-connect pop3) ())
(define-struct (username-rejected pop3) ())
(define-struct (password-rejected pop3) ())
(define-struct (not-ready-for-transaction pop3) (communicator))
(define-struct (not-given-headers pop3) (communicator message))
(define-struct (illegal-message-number pop3) (communicator message))
(define-struct (cannot-delete-message exn) (communicator message))
(define-struct (disconnect-not-quiet pop3) (communicator))
(define-struct (malformed-server-response pop3) (communicator))

;; signal-error :
;; (exn-args ... -> exn) x format-string x values ... ->
;;   exn-args -> ()

(define (signal-error constructor format-string . args)
  (lambda exn-args
    (raise (apply constructor
                  (apply format format-string args)
                  (current-continuation-marks)
                  exn-args))))

;; signal-malformed-response-error :
;; exn-args -> ()

;; -- in practice, it takes only one argument: a communicator.

(define signal-malformed-response-error
  (signal-error make-malformed-server-response
                "malformed response from server"))

;; confirm-transaction-mode :
;; communicator x string -> ()

;; -- signals an error otherwise.

(define (confirm-transaction-mode communicator error-message)
  (unless (eq? (communicator-state communicator) 'transaction)
    ((signal-error make-not-ready-for-transaction error-message)
     communicator)))

;; default-pop-port-number :
;; number

(define default-pop-port-number 110)

(define-struct server-responses ())
(define-struct (+ok server-responses) ())
(define-struct (-err server-responses) ())

;; connect-to-server*:
;; input-port output-port -> communicator

(define connect-to-server*
  (case-lambda
   [(receiver sender) (connect-to-server* receiver sender "unspecified" "unspecified")]
   [(receiver sender server-name port-number)
    (let ([communicator (make-communicator sender receiver server-name port-number
                                           'authorization)])
      (let ([response (get-status-response/basic communicator)])
        (cond
         [(+ok? response) communicator]
         [(-err? response)
          ((signal-error make-cannot-connect
                         "cannot connect to ~a on port ~a"
                         server-name port-number))])))]))

;; connect-to-server :
;; string [x number] -> communicator

(define connect-to-server
  (lambda (server-name (port-number default-pop-port-number))
    (let-values ([(receiver sender) (tcp-connect server-name port-number)])
      (connect-to-server* receiver sender server-name port-number))))

;; authenticate/plain-text :
;; string x string x communicator -> ()

;; -- if authentication succeeds, sets the communicator's state to
;; transaction.

(define (authenticate/plain-text username password communicator)
  (let ([sender (communicator-sender communicator)])
    (send-to-server communicator "USER ~a" username)
    (let ([status (get-status-response/basic communicator)])
      (cond
       [(+ok? status)
        (send-to-server communicator "PASS ~a" password)
        (let ([status (get-status-response/basic communicator)])
          (cond
           [(+ok? status)
            (set-communicator-state! communicator 'transaction)]
           [(-err? status)
            ((signal-error make-password-rejected
                           "password was rejected"))]))]
       [(-err? status)
        ((signal-error make-username-rejected
                       "username was rejected"))]))))

;; get-mailbox-status :
;; communicator -> number x number

;; -- returns number of messages and number of octets.

(define (get-mailbox-status communicator)
  (confirm-transaction-mode
   communicator
   "cannot get mailbox status unless in transaction mode")
  (send-to-server communicator "STAT")
  (apply values
         (map string->number
              (let-values ([(status result)
                            (get-status-response/match
                             communicator
                             #rx"([0-9]+) ([0-9]+)"
                             #f)])
                result))))

;; get-message/complete :
;; communicator x number -> list (string) x list (string)

(define (get-message/complete communicator message)
  (confirm-transaction-mode
   communicator
   "cannot get message headers unless in transaction state")
  (send-to-server communicator "RETR ~a" message)
  (let ([status (get-status-response/basic communicator)])
    (cond
     [(+ok? status)
      (split-header/body (get-multi-line-response communicator))]
     [(-err? status)
      ((signal-error make-illegal-message-number
                     "not given message ~a" message)
       communicator message)])))

;; get-message/headers :
;; communicator x number -> list (string)

(define (get-message/headers communicator message)
  (confirm-transaction-mode
   communicator
   "cannot get message headers unless in transaction state")
  (send-to-server communicator "TOP ~a 0" message)
  (let ([status (get-status-response/basic communicator)])
    (cond
     [(+ok? status)
      (let-values ([(headers body)
                    (split-header/body
                     (get-multi-line-response communicator))])
        headers)]
     [(-err? status)
      ((signal-error make-not-given-headers
                     "not given headers to message ~a" message)
       communicator message)])))

;; get-message/body :
;; communicator x number -> list (string)

(define (get-message/body communicator message)
  (let-values ([(headers body) (get-message/complete communicator message)])
    body))

;; split-header/body :
;; list (string) -> list (string) x list (string)

;; -- returns list of headers and list of body lines.

(define (split-header/body lines)
  (let loop ([lines lines] [header null])
    (if (null? lines)
        (values (reverse header) null)
        (let ([first (car lines)]
              [rest (cdr lines)])
          (if (string=? first "")
              (values (reverse header) rest)
              (loop rest (cons first header)))))))

;; delete-message :
;; communicator x number -> ()

(define (delete-message communicator message)
  (confirm-transaction-mode
   communicator
   "cannot delete message unless in transaction state")
  (send-to-server communicator "DELE ~a" message)
  (let ([status (get-status-response/basic communicator)])
    (cond
     [(-err? status)
      ((signal-error make-cannot-delete-message
                     "no message numbered ~a available to be deleted" message)
       communicator message)]
     [(+ok? status)
      'deleted])))

;; regexp for UIDL responses

(define uidl-regexp #rx"([0-9]+) (.*)")

;; get-unique-id/single :
;; communicator x number -> string

(define (get-unique-id/single communicator message)
  (confirm-transaction-mode
   communicator
   "cannot get unique message id unless in transaction state")
  (send-to-server communicator "UIDL ~a" message)
  (let-values ([(status result)
                (get-status-response/match communicator uidl-regexp ".*")])
    ;; The server response is of the form
    ;; +OK 2 QhdPYR:00WBw1Ph7x7
    (cond
     [(-err? status)
      ((signal-error make-illegal-message-number
                     "no message numbered ~a available for unique id" message)
       communicator message)]
     [(+ok? status)
      (cadr result)])))

;; get-unique-id/all :
;; communicator -> list(number x string)

(define (get-unique-id/all communicator)
  (confirm-transaction-mode communicator
                            "cannot get unique message ids unless in transaction state")
  (send-to-server communicator "UIDL")
  (let ([status (get-status-response/basic communicator)])
    ;; The server response is of the form
    ;; +OK
    ;; 1 whqtswO00WBw418f9t5JxYwZ
    ;; 2 QhdPYR:00WBw1Ph7x7
    ;; .
    (map (lambda (l)
           (let ([m (regexp-match uidl-regexp l)])
             (cons (string->number (cadr m)) (caddr m))))
         (get-multi-line-response communicator))))

;; close-communicator :
;; communicator -> ()

(define (close-communicator communicator)
  (close-input-port (communicator-receiver communicator))
  (close-output-port (communicator-sender communicator)))

;; disconnect-from-server :
;; communicator -> ()

(define (disconnect-from-server communicator)
  (send-to-server communicator "QUIT")
  (set-communicator-state! communicator 'disconnected)
  (let ([response (get-status-response/basic communicator)])
    (close-communicator communicator)
    (cond
     [(+ok? response) (void)]
     [(-err? response)
      ((signal-error make-disconnect-not-quiet
                     "got error status upon disconnect")
       communicator)])))

;; send-to-server :
;; communicator x format-string x list (values) -> ()

(define (send-to-server communicator message-template . rest)
  (apply fprintf (communicator-sender communicator)
         (string-append message-template "\r\n")
         rest)
  (flush-output (communicator-sender communicator)))

;; get-one-line-from-server :
;; iport -> string

(define (get-one-line-from-server server->client-port)
  (read-line server->client-port 'return-linefeed))

;; get-server-status-response :
;; communicator -> server-responses x string

;; -- provides the low-level functionality of checking for +OK
;; and -ERR, returning an appropriate structure, and returning the
;; rest of the status response as a string to be used for further
;; parsing, if necessary.

(define (get-server-status-response communicator)
  (let* ([receiver (communicator-receiver communicator)]
         [status-line (get-one-line-from-server receiver)]
         [r (regexp-match #rx"^\\+OK(.*)" status-line)])
    (if r
        (values (make-+ok) (cadr r))
        (let ([r (regexp-match #rx"^\\-ERR(.*)" status-line)])
          (if r
              (values (make--err) (cadr r))
              (signal-malformed-response-error communicator))))))

;; get-status-response/basic :
;; communicator -> server-responses

;; -- when the only thing to determine is whether the response
;; was +OK or -ERR.

(define (get-status-response/basic communicator)
  (let-values ([(response rest)
                (get-server-status-response communicator)])
    response))

;; get-status-response/match :
;; communicator x regexp x regexp -> (status x list (string))

;; -- when further parsing of the status response is necessary.
;; Strips off the car of response from regexp-match.

(define (get-status-response/match communicator +regexp -regexp)
  (let-values ([(response rest)
                (get-server-status-response communicator)])
    (if (and +regexp (+ok? response))
        (let ([r (regexp-match +regexp rest)])
          (if r (values response (cdr r))
              (signal-malformed-response-error communicator)))
        (if (and -regexp (-err? response))
            (let ([r (regexp-match -regexp rest)])
              (if r (values response (cdr r))
                  (signal-malformed-response-error communicator)))
            (signal-malformed-response-error communicator)))))

;; get-multi-line-response :
;; communicator -> list (string)

(define (get-multi-line-response communicator)
  (let ([receiver (communicator-receiver communicator)])
    (let loop ()
      (let ([l (get-one-line-from-server receiver)])
        (cond
         [(eof-object? l)
          (signal-malformed-response-error communicator)]
         [(string=? l ".")
          '()]
         [(and (> (string-length l) 1)
               (char=? (string-ref l 0) #\.))
          (cons (substring l 1 (string-length l)) (loop))]
         [else
          (cons l (loop))])))))

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
  (let loop ([headers headers])
    (if (null? headers) null
        (let ([first (car headers)]
              [rest (cdr headers)])
          (if (ormap (lambda (matcher)
                       (regexp-match matcher first))
                     desireds)
              (cons first (loop rest))
              (loop rest))))))
