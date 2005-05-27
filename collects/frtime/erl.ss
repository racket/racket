(module erl mzscheme
  
  (require (lib "list.ss")
           (lib "thread.ss")
           (lib "etc.ss")
           (lib "dns.ss" "net")
           "mymatch.ss")
  
  (define (with-semaphore s thunk)
    (semaphore-wait s)
    (let ([result (thunk)])
      (semaphore-post s)
      result))
    
  (define free-cons-cells
    (box empty))
  
  (define alloc-sem (make-semaphore 1))
  
  (define (mcons a d)
    (with-semaphore
     alloc-sem
     (lambda ()
       (let ([start (unbox free-cons-cells)])
         (if (empty? start)
             (cons a d)
             (begin
               (set-box! free-cons-cells (rest start))
               (set-car! start a)
               (set-cdr! start d)
               start))))))
  
  (define (release c)
    (set-cdr! c (unbox free-cons-cells))
    (set-box! free-cons-cells c))

#|  
  (define mcons cons)
  (define release void)
|#
  
  ; for thread ids, port is the TCP port number (not to be confused with MzScheme ports)
  (define-values (listener port)
    ; find first free port after 1178
    (let loop ([port 1178])
      (with-handlers
          ([exn:fail:network? (lambda (_) (loop (add1 port)))])
        (values (tcp-listen port) port))))
  
  (define ip-address
    (let*-values
        ([(sub-proc in-p dummy1 dummy2) (subprocess #f #f #f "/bin/hostname" "-i")]
         [(ip-address) (read in-p)])
      (subprocess-wait sub-proc)
      (if (eof-object? ip-address)
          '127.0.0.1
          ip-address)))
  
  (define my-ip:port
    (string->symbol (format "~a:~a" ip-address port)))
  
  (define dns
    (dns-find-nameserver))
  
  (define ip-regexp
    (regexp "[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?"))
  
  ; a tid is a (vector 'tid symbol(ip:port) symbol(local-id))
  
  (define make-tid
    (case-lambda
      [(thr) (vector 'tid my-ip:port thr)]
      [(port thr) (vector 'tid (string->symbol (format "~a:~a" ip-address port)) thr)]
      [(host port thr) (vector 'tid
                               (string->symbol
                                (format
                                 "~a:~a"
                                 (if (regexp-match ip-regexp (symbol->string host))
                                     host
                                     (string->symbol (dns-get-address dns (symbol->string host))))
                                 port))
                               thr)]))
  
  (define (tid-ip tid)
    (vector-ref tid 1))
  
  (define (tid-lid tid)
    (vector-ref tid 2))
  
  (define (tid? x)
    (and (vector? x)
         (= (vector-length x) 3)
         (eq? (vector-ref x 0) 'tid)
         (symbol? (vector-ref x 1))
         (symbol? (vector-ref x 2))))
  
  ; We need a mapping from MzScheme's tids to our tids (just for `self')
  ; and a mapping from symbols to mailboxes (for local threads).
  ; A special thread is responsible for all communication with external threads.
  ; All processes spawned on a node have same ip-address.
  
  (define tids
    (make-hash-table 'weak))
  
  (define mailboxes
    (make-hash-table))
  
  (define-struct mailbox (old-head old-last head tail sem-count sem-space lock-enqueue))
  
  (define (try-extract m l)
    (let loop ([prev l] [cur (rest l)])
      (if (empty? (rest cur))
          match-fail
          (let ([v (m (first cur))])
            (if (eq? v match-fail)
                (loop cur (rest cur))
                (begin
                  (set-rest! prev (rest cur))
                  (release cur)
                  v))))))
  
  (define (receive-help timeout timeout-thunk matcher)
    (if (and timeout (negative? timeout))
        (timeout-thunk)
        (let* ([start-time (current-milliseconds)]
               [mb (hash-table-get mailboxes (tid-lid (self)))]
               [val (try-extract matcher (mailbox-old-head mb))])
          (if (eq? val match-fail)
              (let loop ()
                (let* ([elapsed (- (current-milliseconds) start-time)]
                       [wait-time (cond
                                    [(not timeout) false]
                                    [(> elapsed timeout) 0]
                                    [else (/ (- timeout elapsed) 1000.0)])]
                       [val (sync/timeout wait-time (mailbox-sem-count mb))])
                  (if val
                      (let* ([oldhead (mailbox-head mb)]
                             [msg (first oldhead)]
                             [val (begin
                                    (set-mailbox-head! mb (rest oldhead))
                                    (release oldhead)
                                    (semaphore-post (mailbox-sem-space mb))
                                    (matcher msg))])
                        (if (eq? val match-fail)
                            (let ([new-last (mcons empty empty)]
                                  [old-last (mailbox-old-last mb)])
                              (set-first! old-last msg)
                              (set-rest! old-last new-last)
                              (set-mailbox-old-last! mb new-last)
                              (loop))
                            (val)))
                      (timeout-thunk))))
              (val)))))
  
  (define-syntax receive
    (syntax-rules (after)
      [(_ (after timeout to-expr ...) (pat expr ...) ...)
       (let* ([matcher (match-lambda (pat (lambda () expr ...)) ...)]
              [timeout-thunk (lambda () to-expr ...)])
         (receive-help timeout timeout-thunk matcher))]
      [(_ clause ...) (receive (after false (void)) clause ...)]))
  
  ; must ensure name not already taken
  (define (spawn/name-help thunk name)
    (if (hash-table-get mailboxes name (lambda () #f))
        #f
        (let ([new-tid (make-tid name)]
              [parent-tid (self)])
          (thread
           (lambda ()
             (hash-table-put! tids (current-thread) new-tid)
             (hash-table-put! mailboxes name (new-mailbox))
             (! parent-tid new-tid)
             (thunk)))
          (receive [(? (lambda (m) (equal? m new-tid))) new-tid]))))
  
  (define last-thread 1)
  
  (define next-thread
    (let ([lock (make-semaphore 1)])
      (lambda ()
        (with-semaphore
         lock
         (lambda ()
           (begin0
             last-thread
             (set! last-thread (add1 last-thread))))))))
  
  (define-syntax spawn
    (syntax-rules ()
      [(_ expr ...) (spawn/name-help (lambda () expr ...)
                                     (string->symbol
                                      (string-append "thread" (number->string (next-thread)))))]))
  
  (define-syntax spawn/name
    (syntax-rules ()
      [(_ name expr ...) (spawn/name-help (lambda () expr ...) name)]))
  
  (define (new-mailbox)
    (let* ([sentinel (mcons empty empty)]
           [old-sentinel (mcons empty empty)]
           [old-head (mcons empty old-sentinel)])
      (make-mailbox old-head
                    old-sentinel
                    sentinel
                    sentinel
                    (make-semaphore)
                    (make-semaphore 1000)
                    (make-semaphore 1))))
  
  (define main (make-tid 'main))
  (hash-table-put! tids (current-thread) main)
  (hash-table-put! mailboxes (tid-lid main) (new-mailbox))
  
  (define forward-mailbox (new-mailbox))
  
  (define (split-string-at str c)
    (let loop ([i 0])
      (if (char=? (string-ref str i) c)
          (values (substring str 0 i) (substring str (add1 i)))
          (loop (add1 i)))))

  (define (report-exn exn)
    (fprintf (current-error-port) "erl.ss: ~a (~a)~n" exn (exn-message exn)))
  
  ; forwarder for remote communication
  (thread
   (lambda ()
     (let* ([in-ports (make-hash-table)] ; set of input ports
            [out-ports (make-hash-table)] ; symbol(ip:port) -> output port
            [mk-wait-set (lambda () (apply choice-evt
                                           (hash-table-map in-ports (lambda (key val) key))))]
            [try-connect (lambda (ip:port)
                           (with-handlers ([exn? (lambda (exn) (report-exn exn) false)])
                             (let*-values ([(ip-str port-str) (split-string-at
                                                               (symbol->string ip:port)
                                                               #\:)]
                                           [(in-p out-p)
                                            (tcp-connect ip-str (string->number port-str))])
                               (hash-table-put! in-ports in-p ip:port)
                               (hash-table-put! out-ports ip:port out-p)
                               (write (list my-ip:port) out-p)
                               out-p)))])
       (let loop ([wait-set (mk-wait-set)])
         ;(printf "have connections to ~a~n" (hash-table-map in-ports (lambda (k v) k)))
         (let ([val (sync (mailbox-sem-count forward-mailbox)
                                          listener wait-set)])
           (cond
             [(tcp-listener? val)
              (with-handlers ([exn? (lambda (exn) (loop wait-set))])
                (let*-values ([(in-p out-p) (tcp-accept listener)]
                              [(remote-ip:port) (first (read in-p))])
                  (hash-table-put! out-ports remote-ip:port out-p)
                  (hash-table-put! in-ports in-p remote-ip:port))
                (loop (mk-wait-set)))]
             [(input-port? val)
              (match (with-handlers ([exn? (lambda (exn) (report-exn exn) eof)])
                       (read val))
                [(lid msg)
                 ; forward to local mailbox
                 (let ([mb (hash-table-get mailboxes lid (lambda () false))])
                   (when mb (send-msg mb msg)))
                 (loop wait-set)]
                [(? eof-object?)
                 ; close input port, remove from hash table
                 (close-input-port val)
                 (hash-table-remove! in-ports val)
                 (loop (mk-wait-set))])]
             [else ; val was the mailbox semaphore
              (match (first (mailbox-head forward-mailbox))
                ;['quit (void)]
                [(#('tid ip:port lid) msg)
                 (let inner ([out-p (hash-table-get
                                     out-ports ip:port
                                     (lambda ()
                                       (begin0
                                         (try-connect ip:port)
                                         (set! wait-set (mk-wait-set)))))])
                   (when out-p
                     ; need to deal with closed ports here too
                     (with-handlers ([exn:fail?
                                      (lambda (_)
                                        (hash-table-remove! out-ports ip:port)
                                        (let ([res (try-connect ip:port)])
                                          (set! wait-set (mk-wait-set))
                                          (inner res)))])
                       (write (list lid msg) out-p))))
                 (set-mailbox-head! forward-mailbox (rest (mailbox-head forward-mailbox)))
                 (semaphore-post (mailbox-sem-space forward-mailbox))
                 (loop wait-set)])]))))))
  
  #|
  (define (stop-network)
    (when network-up?
      (send-msg forward-mailbox 'quit)
      (set! network-up? #f)))
  |#
  
  (define (local? tid)
    (symbol=? (tid-ip tid) my-ip:port))
  
  (define (! tid msg)
    (if (local? tid)
        (let ([mb (hash-table-get mailboxes (tid-lid tid) (lambda () false))])
          (when mb
            (send-msg mb msg)))
        (send-msg forward-mailbox (list tid msg)))) ; forward via special thread
  
  (define (send-msg mbox msg)
    (with-semaphore
     (mailbox-lock-enqueue mbox)
     (lambda ()
       (let ([newtail (mcons empty empty)]
             [oldtail (mailbox-tail mbox)])
         (set-first! oldtail msg)
         (set-rest! oldtail newtail)
         (set-mailbox-tail! mbox newtail)
         (semaphore-wait (mailbox-sem-space mbox))
         (semaphore-post (mailbox-sem-count mbox))))))
  
  (define (self)
    (hash-table-get tids (current-thread)
                    ; allows thread not created by spawn to receive messages
                    (lambda ()
                      (let* ([name (string->symbol
                                    (string-append "thread" (number->string (next-thread))))]
                             [new-tid (make-tid name)])
                        (hash-table-put! tids (current-thread) new-tid)
                        (hash-table-put! mailboxes name (new-mailbox))
                        new-tid))))
  
  (define (!! msg)
    (let ([mb (hash-table-get mailboxes (tid-lid (self)) (lambda () false))])
      (if mb
          (let ([new-last (mcons empty empty)]
                [old-last (mailbox-old-last mb)])
            (set-first! old-last msg)
            (set-rest! old-last new-last)
            (set-mailbox-old-last! mb new-last)))))
  
  (define (mybox)
    (hash-table-get mailboxes (self)))
  
  (provide
   ;   mailboxes
   ;   mybox
   ;   (struct mailbox (old-head old-last channel))
   ; allocations
   ; free-cons-cells
   ; my-ip:port
   make-tid
   tid?
   spawn
   spawn/name
   !
   !!
   receive
   self))

#|
(require erl)

(define (send-loop n)
  (let ([me (self)])
    (let loop ([i 0])
      (if (>= i n)
          void
          (begin
            (! me true)
            (loop (+ i 1)))))))

(define (send-loop2 n)
  (let loop ([i 0])
    (if (>= i n)
        void
        (begin
          (!! true)
          (loop (+ i 1))))))

(define (flush-queue)
  (let recur ()
    (receive [after 0 void]
             [_ (recur)])))

(define (mybox) (hash-table-get mailboxes (self)))
|#