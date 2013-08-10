#lang racket/base
(require (for-syntax racket/base)
         racket/place
         "test-util.rkt")

(define parallel 1)
(let ([argv (current-command-line-arguments)])
  (unless (= (vector-length argv) 0)
    (define howmany (vector-ref argv 0))
    (if (string->number howmany)
        (set! parallel (string->number howmany))
        (raise-user-error 'all.rkt "expected a number on the command-line got ~a" howmany))))

(module capturing-io racket/base
  (provide capture-io)
  (define (capture-io thunk)
    (define-values (out-stdout in-stdout) (make-pipe))
    (define-values (out-stderr in-stderr) (make-pipe))
    (define out-char (make-channel))
    (define in-char (make-channel))
    (define io-done-chan (make-channel))
    (define (mk-listen-loop port what chan)
      (thread (λ () 
                (let loop ()
                  (define c (read-char port))
                  (unless (eof-object? c)
                    (channel-put chan (cons what c))
                    (loop)))
                (channel-put io-done-chan #t))))
    (mk-listen-loop out-stdout 'out out-char)
    (mk-listen-loop out-stderr 'err out-char)
    (define done-chan (make-channel))
    (thread
     (λ () 
       (let loop ([chars '()])
         (sync
          (handle-evt
           out-char 
           (λ (c) 
             (loop (cons c chars))))
          (handle-evt 
           done-chan
           (λ (c) (channel-put c (reverse chars))))))))
    (let/ec k
      (parameterize ([current-output-port in-stdout]
                     [current-error-port in-stderr]
                     [error-escape-handler k])
        (thunk)))
    (close-output-port in-stdout)
    (close-output-port in-stderr)
    (channel-get io-done-chan)
    (channel-get io-done-chan)
    (let ([c (make-channel)])
      (channel-put done-chan c)
      (channel-get c))))
(require (submod "." capturing-io))

(module run-one racket/base
  (provide run-one)
  (require racket/place (submod ".." capturing-io))
  (define (run-one chan)
    (let loop ()
      (define fn (place-channel-get chan))
      (when fn
        (define io
          (capture-io
           (λ ()
             (dynamic-require (car fn) #f))))
        (place-channel-put chan io)
        (loop)))))

(define places
  (and (not (= 1 parallel))
       (for/list ([i (in-range parallel)])
         (dynamic-place `(submod ,(build-path (this-dir) "all.rkt") run-one) 'run-one))))

(define-syntax (this-dir stx)
  (define src (syntax-source stx))
  (cond
    [(path? src)
     (define-values (base name dir?) (split-path src))
     #`'#,base]
    [else #f]))

(define files 
  (for/list ([file (in-list (directory-list (this-dir)))]
             #:when 
             (and (regexp-match #rx"[.]rkt$" (path->string file))
                  (not (member (path->string file)
                               '("test-util.rkt" "all.rkt")))))
    file))

(define (find-deps file)
  (define deps #f)
  
  (define (remove-quote x)
    (cond
      [(and (pair? x) (eq? (car x) 'quote))
       (cadr x)]
      [else
       (error 'find-deps "found non-quote in argument to make-basic-contract-namespace in ~a" file)]))
  
  (let loop ([exp 
              (parameterize ([read-accept-reader #t])
                (call-with-input-file file read))])
    (cond
      [(and (list? exp)
            (pair? exp)
            (eq? (car exp) 'make-basic-contract-namespace))
       (when deps 
         (error 'find-deps 
                "found two calls to make-basic-contract-namespace in ~a"
                file))
       (set! deps (map remove-quote (cdr exp)))]
      [(list? exp)
       (for-each loop exp)]
      [else (void)]))
  deps)

(define (dep<? a b)
  (set! a (or a '()))
  (set! b (or b '()))
  (define (subset? a b)
    (for/and ([x (in-list a)])
      (member x b)))
  (or (and (subset? a b)
           (not (subset? b a)))
      (< (length a) (length b))))

(define files-to-run
  (sort 
   (sort 
    (for/list ([file (in-list files)])
      (list (path->string file)
            (find-deps (build-path (this-dir) file))))
    string<=?
    #:key car)
   dep<?
   #:key cadr))

(define (main)
  (cond
    [places
     (struct running (pc to-run resp-chan))
     (let loop ([runnings '()]
                [free places]
                [to-run files-to-run])
       (cond
         [(and (pair? free) (pair? to-run))
          (define c (make-channel))
          (define pc (car free))
          (thread (λ ()
                    (place-channel-put pc (car to-run))
                    (channel-put c (place-channel-get pc))))
          (loop (cons (running pc (car to-run) c) runnings)
                (cdr free) 
                (cdr to-run))]
         [(null? runnings)
          (void)]
         [else
          (apply sync
                 (map (λ (a-running)
                        (handle-evt
                         (running-resp-chan a-running)
                         (λ (io) 
                           (replay-io (running-to-run a-running) io)
                           (loop (remove a-running runnings)
                                 (cons (running-pc a-running) free)
                                 to-run))))
                      runnings))]))]
    [else
     (for ([file (in-list files-to-run)])
       (replay-io
        file
        (capture-io
         (λ ()
           (dynamic-require (build-path (this-dir) (car file)) #f)))))]))
  
(define (replay-io file-to-run io)
  (flush-output (current-output-port))
  (flush-output (current-error-port))
  (printf "FINISHED ~a\n" (car file-to-run))
  (for ([pr (in-list io)])
    (display (cdr pr)
             ((case (car pr)
                [(out) current-output-port]
                [(err) current-error-port]))))
  (flush-output (current-output-port))
  (flush-output (current-error-port)))

(main)

(flush-output (current-output-port))
(flush-output (current-error-port))
(fprintf (if (zero? failures)
             (current-output-port)
             (current-error-port))
         "ran ~a tests, ~a\n"
         test-cases
         (cond
           [(zero? failures) "all passed"]
           [else (format "~a failed" failures)]))
