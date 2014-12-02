#lang racket
(require net/websocket/client
         net/websocket/server
         net/websocket/conn
         net/websocket/handshake
         racket/async-channel
         net/url
         rackunit
         tests/net/available
         tests/eli-tester)

(define RANDOM-K 100)

(provide tests)
(module+ main (tests))
(define (tests)
  (test
   (for ([i (in-range RANDOM-K)])
     (define o (random 256))
     (define t (random 256))
     (define bot (if (o . < . t) o t))
     (define top (if (o . < . t) t o))
     (define botc (integer->char bot))
     (define topc (integer->char top))
     (test #:failure-prefix (format "~a / ~a" botc topc)
           (<= bot (char->integer (random-char-between botc topc)) top)))

   (for ([i (in-range RANDOM-K)])
     (test (char-alphabetic? (random-alpha-char))))

   (count-spaces "") => 0
   (count-spaces "   ") => 3
   (count-spaces (make-string RANDOM-K #\space)) => RANDOM-K

   (count-spaces "18x 6]8vM;54 *(5:  {   U1]8  z [  8") => 12
   (count-spaces "1_ tx7X d  <  nw  334J702) 7]o}` 0") => 10

   (for ([i (in-range RANDOM-K)])
     (define len (add1 i))
     (define s (make-string len #\0))
     (define how-many (random len))
     (test (count-spaces (add-spaces how-many s)) => how-many))

   (remove-alphas "A0A") => "0"
   (remove-alphas "0") => "0"
   (remove-alphas (make-string RANDOM-K #\A)) => ""

   (remove-alphas "18x 6]8vM;54 *(5:  {   U1]8  z [  8") => "1868545188"
   (remove-alphas "1_ tx7X d  <  nw  334J702) 7]o}` 0") => "1733470270"

   (for ([i (in-range RANDOM-K)])
     (define s (number->string i))
     (test (remove-alphas (add-alphas s)) => s))

   (key->number "18x 6]8vM;54 *(5:  {   U1]8  z [  8") => 155712099
   (key->number "1_ tx7X d  <  nw  334J702) 7]o}` 0") => 173347027

   (for ([i (in-range RANDOM-K)])
     (test (key->number (number->key i)) => i))

   (for ([i (in-range RANDOM-K)])
     (define-values (k1 k2 k3 ans) (generate-key))
     (test (handshake-solution k1 k2 k3) => ans))

   (handshake-solution "18x 6]8vM;54 *(5:  {   U1]8  z [  8"
                       "1_ tx7X d  <  nw  334J702) 7]o}` 0"
                       #"Tm[K T2u")
   => #"fQJ,fN/4F4!~K~MH"

   (local [(define (test-echo-server)
             (define r (number->string (random 1000)))
             (define confirm (make-async-channel))
             (define shutdown!
               (ws-serve #:port 0
                         #:confirmation-channel confirm
                         (λ (wsc _)
                           (let loop ()
                             (define m (ws-recv wsc))
                             (unless (eof-object? m)
                               (ws-send! wsc m)
                               (loop))))))             
             (define p (async-channel-get confirm))
             (define conn
               (ws-connect (string->url (format "ws://localhost:~a" p))))
             (test (ws-send! conn r)
                   (ws-recv conn) => r
                   (ws-send! conn "a")
                   (ws-recv conn) => "a"
                   (ws-close! conn))
             (test (shutdown!)))]
          (when (tcp-localhost-available?)
            (test #:failure-prefix "old"
                  (parameterize ([framing-mode 'old]) (test-echo-server))
                  #:failure-prefix "new"
                  (parameterize ([framing-mode 'new]) (test-echo-server)))))))

(module+ test (require (submod ".." main))) ; for raco test & drdr
