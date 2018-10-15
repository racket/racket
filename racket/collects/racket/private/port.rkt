#lang racket/base
;; A few simple port functions are needed in pretty.rkt, which is
;;  used by contract.rkt, which is used by port.rkt --- so we
;;  break the cycle with this module.
;;
;; copy-port is used by racket/private/streams.rkt, which is used by
;;  racket/place.rkt, which we want to load without loading contracts
;;  --- so copy port is place in this module.

(provide copy-port
         open-output-nowhere
         relocate-output-port
         transplant-output-port
         transplant-to-relocate)

(define open-output-nowhere
  (lambda ([name 'nowhere] [specials-ok? #t])
    (make-output-port
     name
     always-evt
     (lambda (s start end non-block? breakable?) (- end start))
     void
     (and specials-ok?
          (lambda (special non-block? breakable?) #t))
     (lambda (s start end) (wrap-evt
                            always-evt
                            (lambda (x)
                              (- end start))))
     (and specials-ok?
          (lambda (special)
            (wrap-evt always-evt (lambda (x) #t)))))))

(define (transplant-to-relocate transplant p line col pos close? name)
  (let-values ([(init-l init-c init-p) (port-next-location p)])
    (transplant
     p
     (lambda ()
       (let-values ([(l c p) (port-next-location p)])
         (values (and l init-l (+ l (- init-l) line))
                 (and c init-c (if (equal? l init-l)
                                   (+ c (- init-c) col)
                                   c))
                 (and p init-p (+ p (- init-p) pos)))))
     pos
     close?
     #:name name)))

(define relocate-output-port
  (lambda (p line col pos [close? #t] #:name [name (object-name p)])
    (transplant-to-relocate
     transplant-output-port
     p line col pos close? name)))

(define transplant-output-port
  (lambda (p location-proc pos [close? #t] [count-lines!-proc void] #:name [name (object-name p)])
    (make-output-port
     name
     p
     p ; `write' just redirects to `p'
     ;; Here's the slow way to redirect:
     #;
     (lambda (s start end nonblock? breakable?)
       (if (= start end)
           (parameterize-break
            breakable?
            (flush-output p)
            0)
           (let ([v (if nonblock?
                        (write-bytes-avail* s p start end)
                        (if breakable?
                            (parameterize-break
                             #t
                             (write-bytes s p start end))
                            (write-bytes s p start end)))])
             (if (and (zero? v) (not (= start end)))
                 (wrap-evt p (lambda (x) #f))
                 v))))
     (lambda ()
       (when close?
         (close-output-port p)))
     (and (port-writes-special? p)
          p ; `write-special' just redirects to `p'
          ;; Here's the slow way to redirect:
          #;
          (lambda (special nonblock? breakable?)
            ((if nonblock?
                 write-special-avail*
                 (if breakable?
                     (lambda (spec p)
                       (parameterize-break #t
                         (write-special spec p)))
                     write-special))
             special p)))
     (and (port-writes-atomic? p)
          (lambda (s start end)
            (write-bytes-avail-evt s p start end)))
     (and (port-writes-atomic? p)
          (port-writes-special? p)
          (lambda (spec)
            (write-special-evt spec p)))
     location-proc
     count-lines!-proc
     (let ([delta (- pos (or (file-position* p) pos))])
       (if (= delta 1)
           p
           (lambda ()
             (define v (file-position* p))
             (and v (max 1 (+ delta v))))))
     (case-lambda
      [(mode) (file-stream-buffer-mode p mode)]
      [() (file-stream-buffer-mode p)]))))

(define (copy-port src dest . dests*)
  (unless (input-port? src)
    (raise-type-error 'copy-port "input-port" src))
  (for-each
   (lambda (dest)
     (unless (output-port? dest)
       (raise-type-error 'copy-port "output-port" dest)))
   (cons dest dests*))

  (define sz 4096)
  (define s (make-bytes sz))
  (define dests (cons dest dests*))

  (let loop ()
    (define c (read-bytes-avail! s src))
    (cond
      [(number? c)
       (for ([dest (in-list dests)])
         (let write-loop ([bytes-written 0])
           (unless (= bytes-written c)
             (define c2 (write-bytes-avail s dest bytes-written c))
             (write-loop (+ bytes-written c2)))))
       (loop)]
      [(procedure? c)
       (define-values (l col p) (port-next-location src))
       (define v (c (object-name src) l col p))
       (for ([dest (in-list dests)])
         (write-special v dest))
       (loop)]
      [else
       ;; Must be EOF
       (void)])))
