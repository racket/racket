#lang racket
(require unstable/srcloc)

#|
Ryan:
  Shouldn't this be called read-bytes/avail instead? (parallel existing names)
  Changed to eliminate thread-unsafe buffer.
|#
(define (read-available-bytes [port (current-input-port)])
  (read-available-bytes/offset port (make-bytes 1024) 0))

(define (read-available-bytes/offset port buffer offset)
  (let* ([result (read-bytes-avail!* buffer port offset)])
    (if (eof-object? result)
        (if (zero? offset) result (subbytes buffer 0 offset))
        (let ([new-offset (+ offset result)])
          (if (= new-offset (bytes-length buffer))
              (let ([new-buffer (bytes-append buffer buffer)])
                (read-available-bytes/offset port new-buffer new-offset))
              (subbytes buffer 0 new-offset))))))

(define (port->srcloc port [source (object-name port)] [span 0])
  (let*-values ([(line col pos) (port-next-location port)])
    (make-srcloc source line col pos span)))

(define read-all
  (case-lambda
    [() (read-all read)]
    [(reader)
     (let loop ()
       (match (reader)
         [(? eof-object?) null]
         [term (cons term (loop))]))]
    [(reader port)
     (parameterize ([current-input-port port])
       (read-all reader))]))

(define read-all-syntax
  (case-lambda
    [() (read-all-syntax read-syntax)]
    [(reader) (read-all-syntax reader (current-input-port))]
    [(reader port)
     (define start (port->srcloc port))
     (define terms (read-all reader port))
     (define end (port->srcloc port))
     (datum->syntax #f terms (build-source-location-list start end))]))

(provide/contract
 [read-all (->* [] [(-> any/c) input-port?] list?)]
 [read-all-syntax
  (->* [] [(-> (or/c syntax? eof-object?)) input-port?]
       (syntax/c list?))]
 [read-available-bytes (->* [] [input-port?] (or/c bytes? eof-object?))]
 [port->srcloc (->* [port?] [any/c exact-nonnegative-integer?] srcloc?)])
