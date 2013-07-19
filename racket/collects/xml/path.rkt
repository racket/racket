#lang racket/base
(require racket/contract
         racket/match
         racket/dict
         racket/function
         racket/list
         xml)

(define keyword->symbol (compose string->symbol keyword->string))
(define (se-path/xexpr p x)
  (match p
    [(list)
     (list x)]
    [(list-rest (? symbol? s) r)
     (match x
       [(list-rest (? (curry equal? s)) rs)
        (se-path/tag-body r rs)]
       [_
        empty])]
    [_
     empty]))
(define (se-path/tag-body p x)
  (match p
    [(list)
     (match x
       [(list) empty]
       [(list-rest (list (list (? symbol?) (? string?)) ...) rs)
        (se-path/tag-body p rs)]
       [(? list?)
        x]
       [_ 
        empty])]
    [(list-rest (? symbol?) _)
     (match x
       [(list-rest (list (list (? symbol?) (? string?)) ...) rs)
        (append-map (curry se-path/xexpr p) rs)]
       [(? list?)
        (append-map (curry se-path/xexpr p) x)]
       [_
        empty])]
    [(list (? keyword? k))
     (match x
       [(list-rest (and attrs (list (list (? symbol?) (? string?)) ...)) rs)
        (se-path/attr (keyword->symbol k) attrs)]
       [_
        empty])]
    [_
     empty]))
(define (se-path/attr k attrs)
  (dict-ref attrs k empty))
(define (se-path*/list p x)
  (append (se-path/xexpr p x)
          (match x
            [(list (? symbol? tag) (list (list (? symbol?) (? string?)) ...) rs ...)
             (append-map (curry se-path*/list p) rs)]
            [(list (? symbol? tag) rs ...)
             (append-map (curry se-path*/list p) rs)]
            [_
             empty])))
(define (se-path* p x)
  (match (se-path*/list p x)
    [(list) #f]
    [(list-rest f rs) f]))

(define se-path?
  (match-lambda
    [(list) #t]
    [(list (? keyword?)) #t]
    [(list-rest (? symbol?) l) (se-path? l)]
    [_ #f]))

(provide/contract
 [se-path? contract?]
 [se-path*
  (-> se-path? xexpr?
      ; XXX maybe this shouldn't be any/c
      any/c)]
 [se-path*/list
  (-> se-path? xexpr?
      ; XXX see above
      (listof any/c))])
