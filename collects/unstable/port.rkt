#lang racket/base
(require racket/match
         racket/contract/base
         syntax/srcloc)

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
       (syntax/c list?))])
