#lang racket/base
(require racket/match)

(provide check-for-cycles)

;; doesn't check for cycles via things like:
;;  a ::= hole
;;  b ::= (in-hole a b)
(define (check-for-cycles stx ntss/stx prodss/stx)
  
  (define ntss (syntax->datum ntss/stx))
  (define prodss (syntax->datum prodss/stx))
  
  ;; hash[sym[nt] -o> (listof sym[nt])
  (define table (make-hash))
  
  ;; build the graph
  (for ([nts (in-list ntss)]
        [prods (in-list prodss)])
    (define base-nt (car nts))
    (for ([nt (in-list (cdr nts))])
      (hash-set! table nt (list base-nt)))
    (hash-set! table base-nt '())
    (for ([prod (in-list prods)])
      (match prod
        [`(nt ,name)
         (hash-set! table base-nt (cons name (hash-ref table base-nt)))]
        [_ (void)])))
  
  ;; traverse the graph looking for cycles
  (define cycle
    (for/or ([(nt neighbors) (in-hash table)])
      (define visited (make-hash))
      (for/or ([neighbor (in-list neighbors)]) 
        (let loop ([current-node neighbor])
          (cond
            [(eq? current-node nt) nt]
            [(hash-ref visited current-node #f) #f]
            [else
             (hash-set! visited current-node #t)
             (for/or ([neighbor (in-list (hash-ref table current-node))])
               (loop neighbor))])))))
  
  (when cycle
    (raise-syntax-error 'define-language
                        (format
                         "found a cycle that includes the non-terminal ~a"
                         cycle)
                        stx)))
