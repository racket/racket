#lang racket/base
(require racket/match
         racket/place
         rackunit)

(define (racket-subprocess o i e . args)
  (define (current-executable-path)
    (parameterize ([current-directory (find-system-path 'orig-dir)])
      (find-executable-path (find-system-path 'exec-file) #f)))

  (apply subprocess o i e (current-executable-path) args))

(module+ test
  (main))

(define (main)
  (test-case
    "test file descriptors copied across place channesl"
;; write out "fdt.rkt"
    (with-output-to-file "fdt.rkt" #:exists 'replace (lambda ()
      (display
#<<END
#lang racket/base
(define (write-flush x [p (current-output-port)])
  (write x p)
  (flush-output p))

(sleep 3)
(with-handlers ([exn? (lambda (e) (eprintf "Child Read1 Exception Caught ~e\n" e))])
  (define r (read))
  (log-debug (format "Child Read1 ~a\n" r)))
(with-handlers ([exn? (lambda (e) (eprintf "Child Read2 Exception Caught ~e\n" e))])
  (define r (read))
  (log-debug (format "Child Read2 ~a\n" r)))
;(close-input-port)

(sleep 3)
(with-handlers ([exn? (lambda (e) (eprintf "Child Write StdOut Exception Caught ~e\n" e))])
  (write-flush "ByeO"))
(with-handlers ([exn? (lambda (e) (eprintf "Child Write StdErr Exception Caught ~e\n" e))])
  (log-debug "ByeE"))
END
)))

    (define (write-flush x [port (current-output-port)])
      (write x port)
      (flush-output port))

    (define p
      (place ch
        (match (place-channel-get ch)
          [(list in out)
           (with-handlers ([exn? (lambda (e) (printf "Place Write Exception Caught ~e\n" e) (raise e))])
             (write "BFrom Place" out)
             (flush-output out))
             (close-output-port out)])))

    (let ()
      (define-values (s o i e) (racket-subprocess #f #f (current-error-port) "fdt.rkt"))
      (place-channel-put p (list o i))
      ;(close-output-port i)
      ;(close-input-port o)
      (place-wait p)
      (with-handlers ([exn? (lambda (e) (printf "Parent Write Exception Caught ~e\n" e) (raise e))])
        (write-flush "AFrom Parent" i))
      (with-handlers ([exn? (lambda (e) (printf "Parent Read Exception Caught ~e\n" e) (raise e))])
        (printf "ParentRead ~v\n" (read o)))
      (subprocess-wait s)
      (displayln (subprocess-status s)))))

