#lang racket/base
(require racket/system
         racket/match
         racket/contract
         racket/port
         string-constants)

(provide/contract
 [query-aspell (->* ((and/c string? (not/c #rx"[\n]")))
                    ((or/c #f string?))
                    (listof (list/c number? number?)))]

 ;; may return #f when aspell is really ispell or when
 ;; something goes wrong trying to get the list of dictionaries
 [get-aspell-dicts (-> (or/c #f (listof string?)))]
 
 [find-aspell-binary-path (-> (or/c path? #f))]
 [aspell-problematic? (-> (or/c string? #f))])

(define aspell-candidate-paths
  '("/usr/bin" 
    "/bin"
    "/usr/local/bin"
    "/opt/local/bin/"))

(define (find-aspell-binary-path)
  (define aspell (if (eq? (system-type) 'windows) "aspell.exe" "aspell"))
  (define ispell (if (eq? (system-type) 'windows) "ispell.exe" "ispell"))
  (or (find-executable-path aspell)
      (find-executable-path ispell)
      (for/or ([cp aspell-candidate-paths])
        (define c1 (build-path cp aspell))
        (define c2 (build-path cp ispell))
        (or (and (file-exists? c1)
                 c1)
            (and (file-exists? c2)
                 c2)))))

(define (start-aspell asp dict)
  (define aspell? (regexp-match? #rx"aspell" (path->string asp)))
  (apply process* asp 
         (append
          '("-a")
          (if dict
              (list "-d" dict)
              '())
          (if aspell? '("--encoding=utf-8") '()))))

(define problematic 'dont-know)
(define (aspell-problematic?)
  (when (eq? problematic 'dont-know)
    (set! problematic (do-aspell-problematic))
    (asp-log (format "set problematic to ~s" problematic)))
  problematic)

(define (do-aspell-problematic)
  (define asp (find-aspell-binary-path))
  (cond
    [(not asp)
     (string-constant cannot-find-ispell-or-aspell-path)]
    [else
     (define proc-lst (start-aspell asp #f))
     (define stdout (list-ref proc-lst 0))
     (define stderr (list-ref proc-lst 3))
     (close-output-port (list-ref proc-lst 1)) ;; close stdin
     (close-input-port stdout)
     (define sp (open-output-string))
     (copy-port stderr sp)
     (define errmsg (get-output-string sp))
     (close-input-port stderr)
     (cond
       [(not (equal? errmsg ""))
        (string-append
         (format (string-constant spell-program-wrote-to-stderr-on-startup) asp)
         "\n\n"
         errmsg)]
       [else #f])]))

(define asp-logger (make-logger 'framework/aspell (current-logger)))
(define-syntax-rule 
  (asp-log arg)
  (when (log-level? asp-logger 'debug)
    (asp-log/proc arg)))
(define (asp-log/proc arg)
  (log-message asp-logger 'debug arg (current-continuation-marks)))

(define aspell-req-chan (make-channel))
(define change-dict-chan (make-channel))
(define aspell-thread #f)
(define (start-aspell-thread)
  (unless aspell-thread
    (set! aspell-thread
          (thread
           (λ () 
             (define aspell-proc #f)
             (define already-attempted-aspell? #f)
             (define current-dict #f)
             
             (define (fire-up-aspell)
               (unless already-attempted-aspell?
                 (set! already-attempted-aspell? #t)
                 (define asp (find-aspell-binary-path))
                 (when asp
                   (set! aspell-proc (start-aspell asp current-dict))
                   (define line (with-handlers ((exn:fail? exn-message))
                                  (read-line (list-ref aspell-proc 0))))
                   (asp-log (format "framework: started speller: ~a" line))
                   
                   (when (and (string? line)
                              (regexp-match #rx"[Aa]spell" line))
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
                          (asp-log (format "aspell-proc stderr: ~a" l))
                          (loop))))))))
             
             (define (shutdown-aspell why)
               (asp-log (format "aspell.rkt: shutdown connection to aspell: ~a" why))
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
                   [(list line new-dict resp-chan nack-evt)
                    (unless (equal? new-dict current-dict)
                      (when aspell-proc
                        (shutdown-aspell "changing dictionary")
                        (set! already-attempted-aspell? #f))
                      (set! current-dict new-dict))
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
                            (define (handle-err x)
                              (asp-log
                               (format "error reading stdout of aspell process: ~a"
                                       (exn-message x)))
                              eof)
                            (define l (with-handlers ((exn:fail? handle-err))
                                        (read-line stdout)))
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

(define (query-aspell line [dict #f])
  (cond
    [(aspell-problematic?)
     '()]
    [else
     (when dict
       (unless (member dict (get-aspell-dicts))
         (set! dict #f)))
     
     (start-aspell-thread)
     (sync
      (nack-guard-evt
       (λ (nack-evt)
         (define resp (make-channel))
         (channel-put aspell-req-chan (list line dict resp nack-evt))
         resp)))]))

(define aspell-dicts #f)
(define (get-aspell-dicts)
  (unless (aspell-problematic?)
    (unless aspell-dicts
      (define asp (find-aspell-binary-path))
      (when (regexp-match? #rx"aspell" (path->string asp))
        (define proc-lst (process* asp "dump" "dicts"))
        (define stdout (list-ref proc-lst 0))
        (define stderr (list-ref proc-lst 3))
        (close-output-port (list-ref proc-lst 1)) ;; close stdin
        (close-input-port stderr)
        (set! aspell-dicts
              (let loop ()
                (define l (read-line stdout))
                (cond
                  [(eof-object? l) '()]
                  [else (cons l (loop))]))))))
  aspell-dicts)
