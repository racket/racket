#lang racket/base
(require racket/list
         racket/contract)

; A dispatch pattern is either
; - a string
; - a bidi match expander
; - ...

(define (...? stx)
  (eq? '... (syntax->datum stx)))

(define (string-syntax? stx)
  (string? (syntax->datum stx)))

(define (dispatch-pattern? stx)
  (define (dispatch/no-...? stx)
    (syntax-case stx ()
      [() #t]
      [((bidi arg ...) . rest-stx)
       (dispatch/...? #'rest-stx)]
      [(string . rest-stx)
       (string-syntax? #'string)
       (dispatch/no-...? #'rest-stx)]))
  (define (dispatch/...? stx)
    (syntax-case stx ()
      [() #t]
      [((bidi arg ...) . rest-stx)
       (dispatch/...? #'rest-stx)]
      [(string . rest-stx)
       (string-syntax? #'string)
       (dispatch/no-...? #'rest-stx)]
      [((... ...) . rest-stx)
       (dispatch/no-...? #'rest-stx)]))    
  (dispatch/no-...? stx))

(define (dispatch-pattern/ids? stx)
  (define (dispatch/no-...? stx)
    (syntax-case stx ()
      [() #t]
      [((bidi arg ... id) . rest-stx)
       (identifier? #'id)
       (dispatch/...? #'rest-stx)]
      [(string . rest-stx)
       (string-syntax? #'string)
       (dispatch/no-...? #'rest-stx)]))
  (define (dispatch/...? stx)
    (syntax-case stx ()
      [() #t]
      [((bidi arg ... id) . rest-stx)
       (identifier? #'id)
       (dispatch/...? #'rest-stx)]
      [(string . rest-stx)
       (string-syntax? #'string)
       (dispatch/no-...? #'rest-stx)]
      [((... ...) . rest-stx)
       (dispatch/no-...? #'rest-stx)]))    
  (dispatch/no-...? stx))

(define (dispatch-pattern-not-... stx)
  (filter (compose not ...?) 
          (syntax->list stx)))

(define (dispatch-pattern-next-...? stx)
  (let loop ([l (syntax->list stx)])
    (cond
      [(empty? l)
       empty]
      [(empty? (rest l))
       (list #f)]
      [(...? (second l))
       (list* #t (loop (rest (rest l))))]
      [else
       (list* #f (loop (rest l)))])))

(define (dispatch-pattern->dispatch-pattern/ids pps)
  (map (lambda (pp ppi)
         (cond
           [(string-syntax? pp)
            pp]
           [(...? pp)
            pp]
           [else
            (with-syntax ([(bidi-id arg ...) pp]
                          [id ppi])
              (syntax/loc pp (bidi-id arg ... id)))]))
       (syntax->list pps)
       (generate-temporaries pps)))

(provide/contract
 [string-syntax? (syntax? . -> . boolean?)]
 [dispatch-pattern-next-...? (syntax? . -> . (listof boolean?))]
 [dispatch-pattern-not-... (syntax? . -> . (listof syntax?))]
 [dispatch-pattern->dispatch-pattern/ids (syntax? . -> . (listof syntax?))]
 [dispatch-pattern? (syntax? . -> . boolean?)]
 [dispatch-pattern/ids? (syntax? . -> . boolean?)])
