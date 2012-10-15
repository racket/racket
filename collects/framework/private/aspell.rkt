#lang racket/base
(require racket/system
         racket/match
         racket/contract)

(provide/contract
 [query-aspell (-> (and/c string? (not/c #rx"[\n]")) (listof (list/c number? number?)))])

(define aspell-candidate-paths
  '("/usr/bin" 
    "/usr/local/bin"
    "/bin"))

(define aspell-req-chan (make-channel))
(define aspell-thread #f)
(define (start-aspell-thread)
  (unless aspell-thread
    (set! aspell-thread
          (thread
           (λ () 
             (define aspell-proc #f)
             (define already-attempted-aspell? #f)
             
             (define (fire-up-aspell)
               (unless already-attempted-aspell?
                 (set! already-attempted-aspell? #t)
                 (define asp (or (find-executable-path "aspell")
                                 (find-executable-path "ispell")
                                 (for/or ([cp aspell-candidate-paths])
                                   (define c1 (build-path cp "aspell"))
                                   (define c2 (build-path cp "ispell"))
                                   (or (and (file-exists? c1)
                                            c1)
                                       (and (file-exists? c2)
                                            c2)))))
                 (define aspell? (regexp-match? #rx"aspell" (path->string asp)))
                 (when asp
                   (set! aspell-proc (apply process* asp "-a" (if aspell? '("--encoding=utf-8") '())))
                   (define line (read-line (list-ref aspell-proc 0)))
                   (log-info (format "framework: started speller: ~a" line))
                   
                   (when (regexp-match #rx"[Aa]spell" line)
                     ;; put aspell in "terse" mode 
                     (display "!\n" (list-ref aspell-proc 1))
                     (flush-output (list-ref aspell-proc 1)))
                   
                   (define stderr (list-ref aspell-proc 3))
                   (thread
                    (λ ()
                      (let loop ()
                        (define l (with-handlers ((exn:fail? void))
                                    (read-line stderr)))
                        (when (string? l)
                          (log-warning (format "aspell-proc stderr: ~a" l))
                          (loop))))))))
             
             (define (shutdown-aspell why)
               (log-warning (format "aspell.rkt: shutdown connection to aspell: ~a" why))
               (define proc (list-ref aspell-proc 4))
               (close-input-port (list-ref aspell-proc 0))
               (close-output-port (list-ref aspell-proc 1))
               (close-input-port (list-ref aspell-proc 3))
               (proc 'kill)
               (set! aspell-proc #f))
             
             (let loop ()
               (sync
                (handle-evt
                 aspell-req-chan
                 (match-lambda
                   [(list line resp-chan nack-evt)
                    (unless aspell-proc (fire-up-aspell))
                    (define (send-resp resp)
                      (sync (channel-put-evt resp-chan resp)
                            nack-evt))
                    (cond
                      [aspell-proc
                       (define stdout (list-ref aspell-proc 0))
                       (define stdin (list-ref aspell-proc 1))
                       
                       ;; always lead with a ^ character so aspell
                       ;; doesn't interpret anything as a command
                       (display "^" stdin)
                       (display line stdin) 
                       (newline stdin)
                       (flush-output stdin)
                       (let loop ([resp '()])
                         (define check-on-aspell (sync/timeout .5 stdout))
                         (cond
                           [check-on-aspell
                            (define l (read-line stdout))
                            (cond
                              [(eof-object? l) 
                               (send-resp '())
                               (shutdown-aspell "got eof from process")]
                              [(equal? l "") (send-resp (reverse resp))]
                              [(regexp-match #rx"^[*]" l) (loop resp)]
                              [(regexp-match #rx"^[&] ([^ ]*) [0-9]+ ([0-9]+)" l) 
                               =>
                               (λ (m)
                                 (define word-len (string-length (list-ref m 1)))
                                 ;; subtract one to correct for the leading ^
                                 (define word-start (- (string->number (list-ref m 2)) 1))
                                 (loop (cons (list word-start word-len) resp)))]
                              [(regexp-match #rx"^[#] ([^ ]*) ([0-9]+)" l) 
                               =>
                               (λ (m)
                                 (define word-len (string-length (list-ref m 1)))
                                 ;; subtract one to correct for the leading ^
                                 (define word-start (- (string->number (list-ref m 2)) 1))
                                 (loop (cons (list word-start word-len) resp)))]
                              [else
                               (send-resp '())
                               (shutdown-aspell (format "could not parse aspell output line: ~s" l))])]
                           [else
                            (send-resp '())
                            (shutdown-aspell "interaction timed out")]))]
                      [else (send-resp '())])
                    (loop)])))))))))

(define (query-aspell line)
  (start-aspell-thread)
  (sync
   (nack-guard-evt
    (λ (nack-evt)
      (define resp (make-channel))
      (channel-put aspell-req-chan (list line resp nack-evt))
      resp))))
