#lang racket/base
;; stress tests for place-channels
(require (prefix-in pp: "place-processes.rkt")
         (prefix-in pu: tests/racket/place-utils))
(require racket/place
         racket/path
         racket/system)

(define (splat txt fn)
  (call-with-output-file fn #:exists 'replace
      (lambda (out)
        (fprintf out "~a" txt))))

(define (print-out msg B/sE)
  (displayln
   (list msg
         (exact->inexact B/sE) 'bytes-per-second
         (exact->inexact (/ B/sE (* 1024 1024)))
         'MB-per-second)))

(pp:place/base 
 byte-message
 (bo ch)
 (define message-size (* 4024 1024))
 (define count 10)
 (define fourk-b-message (make-bytes message-size 66))
 (for ([i (in-range count)])
   (place-channel-get ch)
   (place-channel-put ch fourk-b-message)))

(define (processes-byte-message-test)
  (let ([pl (pp:dynamic-place (pu:here-submod byte-message) 'bo)])
    (define message-size (* 4024 1024))
    (define four-k-message (make-bytes message-size 65))
    (define count 10)
    (define-values (r t1 t2 t3)
      (time-apply
       (lambda ()
         (for ([i (in-range count)])
           (pp:place-channel-put pl four-k-message)
           (pp:place-channel-get pl))) null))


    (print-out "processes-emulated-places" (/ (* 2 count message-size) (/ t2 1000)))
    (pp:place-wait pl)))


(define (byte-message-test)
  (splat
  #<<END
  (module pct1 racket/base
    (require racket/place)
    (provide place-main)

    (define (place-main ch)
      (define message-size (* 4024 1024))
      (define count 150)
      (define fourk-b-message (make-bytes message-size 66))
      (for ([i (in-range count)])
       (place-channel-get ch)
       (place-channel-put ch fourk-b-message)))
  )
END
  "pct1.rkt")

  (let ([pl (dynamic-place "pct1.rkt" 'place-main)])
    (define message-size (* 4024 1024))
    (define four-k-message (make-bytes message-size 65))
    (define count 150)
    (define-values (r t1 t2 t3)
      (time-apply
       (lambda ()
         (for ([i (in-range count)])
           (place-channel-put pl four-k-message)
           (place-channel-get pl))) null))


    (print-out "places" (/ (* 2 count message-size) (/ t2 1000)))
    (place-wait pl))

  (delete-file "pct1.rkt"))

(define (cons-tree-test)
  (splat
  #<<END
  (module pct1 racket/base
    (require racket/place)
    (provide place-main)

    (define (place-main ch)
      (define count 500)
      (for ([i (in-range count)])
       (place-channel-put ch (place-channel-get ch))))
  )
END
  "pct1.rkt")

  (let ([pl (dynamic-place "pct1.rkt" 'place-main)])
    (define tree
      (let loop ([depth 8])
        (if (depth . <= . 0)
            1
            (cons (loop (sub1 depth)) (loop (sub1 depth))))))
    (define count 500)
    (define-values (r t1 t2 t3)
      (time-apply
       (lambda ()
         (for ([i (in-range count)])
           (place-channel-put pl tree)
           (place-channel-get pl))) null))

    (define s (* (- (expt 2 9) 1) 4 8 count))
    (printf "cons-tree ~a ~a ~a ~a\n" t1 t2 t3  (exact->inexact (/ t2 1000)))
    (print-out "cons-tree" (/ s (/ t2 1000)))

    (place-wait pl))

  (delete-file "pct1.rkt"))

(define (current-executable-path) 
 (parameterize ([current-directory (find-system-path 'orig-dir)])
  (find-executable-path (find-system-path 'exec-file) #f)))

(define (current-collects-path)
 (let ([p (find-system-path 'collects-dir)])
  (if (complete-path? p)
      p
      (path->complete-path p (or (path-only (current-executable-path))
                                 (find-system-path 'orig-dir))))))

(define (process-pipe-test)
  (define worker-cmdline-list (list (current-executable-path) "-X" (path->string (current-collects-path)) "-e" "(eval(read))"))
  (let-values ([(_process-handle _out _in _err) (apply subprocess #f #f (current-error-port) worker-cmdline-list)])
    (define message-size (* 4024 1024))
    (define four-k-message (make-bytes message-size 65))
    (define count 10)
    (define-values (r t1 t2 t3)
      (begin
        (write
          `(for ([x (in-range ,count)])
            (define k (read-bytes (* 4024 1024)))
            (write-bytes k)
            (flush-output)) _in)
        (flush-output _in)
        (time-apply 
          (lambda ()
            (for ([i (in-range count)])
                 (write-bytes four-k-message _in)
                 (flush-output _in)
                 (read-bytes message-size _out))) 
          null)))
    (subprocess-wait _process-handle)
    (printf "~a ~a ~a ~a\n" r t1 t2 t3)

    (print-out "process-pipe" (/ (* 2 count message-size) (/ (+ t2 1)  1000)))))

(define (say-system cmd)
  (displayln cmd)
  (system cmd))

(byte-message-test)
(processes-byte-message-test)
(process-pipe-test)
(say-system "dd if=/dev/zero of=/dev/null count=10000000")
(say-system "dd if=/dev/zero count=10000000 |dd of=/dev/null")

(cons-tree-test)

