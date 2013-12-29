#lang racket/base
(require web-server/servlet-env
         web-server/dispatch
         web-server/http
         net/url
         net/url-connect
         racket/port
         db)

(module test racket/base)

#|
This program tests the combination of virtual connections and
connection pools in the context of web servlets, where servlet threads
may be killed unexpectedly if the client hangs up.

Errors to watch for:
 - connection-pool-lease: cannot obtain connection; connection pool limit reached
   Might mean a connection has become locked. Or could be a race condition, pool
   connection not released in time. (Seen in 5.2.1, not near-5.3 versions.)
 - tcp-write: error writing (Broken pipe)
   Probably okay, seems to come from web-server trying to write response.
 - TIMEOUT (and programs exits)
   This would be bad; all connections are locked up. I haven't seen it happen.
|#

(define NOISY? #f)

(define c (virtual-connection
           (connection-pool
            (lambda () (dsn-connect 'pg))
            #:max-connections 12)))

(unless (table-exists? c "foo")
  (query-exec c "create table foo (n integer, s text)"))

(define (add req n s)
  (query-exec c "insert into foo (n, s) values ($1, $2)" n s)
  (response/xexpr
   `(html (body (p "Thanks, there are now "
                   ,(format "~a" (query-value c "select count(s) from foo where n = $1" n))
                   " values associated with " ,(number->string n) ".")))))

(define (ask req n)
  (response/xexpr
   `(html (body (p "These are the strings associated with "
                   ,(number->string n)
                   ":")
                (ol
                 ,@(for/list ([s (in-query c "select s from foo where n = $1" n)])
                     `(li ,(format "~a" s))))))))

(define (find req ss)
  (response/xexpr
   `(html (body (p "These are the numbers containing "
                   ,(format "~s" ss)
                   ":")
                (ol
                 ,@(for/list ([n (query-list c "select n from foo where strpos(s, $1) > 0 order by n" ss)])
                     `(li ,(format "~a" n))))))))

(define-values (dispatch to-url0)
  (dispatch-rules
   [("ask" (integer-arg)) ask]
   [("find" (string-arg)) find]
   [("add" (integer-arg) (string-arg)) add]))

(define (to-url . args)
  (string->url
   (string-append "http://localhost:10000" (apply to-url0 args))))

(thread
 (lambda ()
   (serve/servlet dispatch
                  #:port 10000
                  #:servlet-path "/ask/0"
                  #:servlet-regexp #rx"")))

(sleep 1)

(define (random-letter _)
  (integer->char (+ (char->integer #\a) (random 26))))

(define (get url)
  (let* ([c (make-custodian)]
         [fast? (zero? (random 5))]
         [t
          (parameterize ((current-custodian c))
            (thread
             (lambda ()
               (let* ([ts1 (current-milliseconds)]
                      [result (call/input-url url get-pure-port port->string)]
                      [ts2 (current-milliseconds)])
                 (when NOISY? (eprintf "~s ms\n" (inexact->exact (floor (- ts2 ts1)))))))))])
    (if fast?
        (or (sync/timeout (+ 0.005 (* 0.001 (random 5))) t)
            (begin (custodian-shutdown-all c)
                   (when NOISY? (eprintf "fast timeout\n"))))
        (or (sync/timeout 1 t)
            (begin (custodian-shutdown-all c)
                   (eprintf "TIMEOUT\n")
                   (exit 1))))))

(define workers
  (for/list ([t 10])
    (thread
     (lambda ()
       (for ([r 1000])
         (case (random 3)
           ((0) (get (to-url ask (random 10))))
           ((1) (get (to-url find (build-string 3 random-letter))))
           ((2) (get (to-url add (random 10) (build-string 40 random-letter))))))))))

(for-each thread-wait workers)
(eprintf "Whew, done.\n")
(sleep 5)
