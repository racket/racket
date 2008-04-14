#lang scheme/base

(require (for-syntax scheme/base))
(provide (all-defined-out))

;; ============================================================
;; PREFIX DISPATCHER
;; Code to determine the entry specified by an arbitrary
;; (unambiguous) prefix of a set of possible entries

(define-struct (exn:prefix-dispatcher exn:fail) ())
(define-struct (exn:unknown-command exn:prefix-dispatcher) (entry))
(define-struct (exn:ambiguous-command exn:prefix-dispatcher) (possibilities))

;; get-prefix-dispatcher : (listof (list string A)) -> string -> A
;; gets the 
(define (get-prefix-dispatcher options)
  ;; implementation strategy is dumb regexp-filter. It is possible to do a trie or something fancy like that,
  ;; but it would cost more to build than it would be worth, and we're only expecting lists of a few items anyway
  (let ([pre/full (get-prefix-and-suffix (map car options))])
    (when pre/full
      (error 'get-prefix-dispatcher "No element may be a strict prefix of any other element; given ~a and ~a" 
             (car pre/full)
             (cadr pre/full))))
  
  (λ (target)
    (let* ([re (format "^~a" (regexp-quote target))]
           [matches (filter (λ (x) (regexp-match re (car x))) options)])
      (cond
        [(length=? matches 1) (cadr (car matches))]
        [(null? matches)
         (raise (make-exn:unknown-command (format "Unknown command: ~a" target)
                                          (current-continuation-marks)
                                          target))]
        [else
         (raise (make-exn:ambiguous-command (format "Ambiguous command: ~a" target)
                                            (current-continuation-marks)
                                            (map car matches)))]))))
;; length=? : list nat -> boolean
;; determines if the given list has the given length. Running time is proportional
;; to the shorter of the magnitude of the number or the actual length of the list
(define (length=? lst len)
  (cond
    [(and (null? lst) (zero? len)) #t]
    [(null? lst)                   #f]
    [(zero? len)                   #f]
    [else                          (length=? (cdr lst) (sub1 len))]))
      
;; get-prefix-and-suffix : (listof string) -> (list string string) | #f
;; returns a pair of strings in the given list such that the first string is a prefix of the second,
;; or #f if no such pair exists
(define (get-prefix-and-suffix strs)
  (cond
    [(null? strs) #f]
    [else
     (sorted-nelist-contains-prefix? (sort strs string<?))]))

;; sorted-nelist-contains-prefix? : (nonempty-listof string) -> (list string string) | #f
;; given a lexicographically-sorted, nonempty list of strings, returns either
;; two strings from the list such that the first is a prefix of the second, or #f if 
;; no such pair exists
(define (sorted-nelist-contains-prefix? nel)
  (cond
    [(null? (cdr nel)) #f]
    [(prefix? (car nel) (cadr nel))
     (list (car nel) (cadr nel))]
    [else (sorted-nelist-contains-prefix? (cdr nel))]))

;; prefix? : string string -> boolean
;; determins if s1 is a prefix of s2
(define (prefix? s1 s2)
  (and (<= (string-length s1) (string-length s2))
       (string=? s1 (substring s2 0 (string-length s1)))))


(define-syntax (prefix-case stx)
  (syntax-case stx (else)
    [(_ elt
      [option result] ...
      [else alternative])
     #'(with-handlers ([exn:prefix-dispatcher? (λ (e) alternative)])
         (((get-prefix-dispatcher (list (list option (λ () result)) ...))
           elt)))]
    [(_ elt [option result] ...)
     #'(let ([e elt]) (prefix-case e [option result] ... [else (error 'prefix-case "element ~e was not a prefix" e)]))]))
   
    
    
  
