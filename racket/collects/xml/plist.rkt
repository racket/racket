#lang racket/base
(require racket/list
         racket/contract
         xml)

(define (plist-dict? v)
  (and (list? v)
       (pair? v)
       (eq? (car v) 'dict)
       (andmap (lambda (v)
                 (and (list? v)
                      (= 3 (length v))
                      (eq? (car v) 'assoc-pair)
                      (string? (cadr v))
                      (plist-value? (caddr v))))
               (cdr v))))

(define (plist-value? v)
  (or (string? v)
      (and (pair? v)
           (case (car v)
             [(true) (null? (cdr v))]
             [(false) (null? (cdr v))]
             [(integer) (and (= (length v) 2)
                             (exact-integer? (cadr v)))]
             [(real) (and (= (length v) 2)
                          (real? (cadr v)))]
             [(array) (andmap plist-value? (cdr v))]
             [(data) (and (= (length v) 2) (string? (cadr v)))]
             [(date) (and (= (length v) 2) (string? (cadr v)))]
             [else (plist-dict? v)]))))

; raise-plist-exn : string mark-set xexpr symbol -> ???
(define (raise-plist-exn tag mark-set xexpr type)
  (raise (make-exn:fail:contract (string-append "badly formed '" tag "'")
                                 mark-set)))

; expand-dict : xexpr -> xexpr
(define (expand-dict x)
  (cond [(and (eq? (car x) 'dict)
              (map expand-assoc-pair (cdr x)))
         =>
         (lambda (x) `(dict ,@(apply append x)))]
        [else
         (raise-plist-exn "dict" (current-continuation-marks) x 'plist:dict)]))

; expand-assoc-pair : xexpr -> (list xexpr xexpr)
(define (expand-assoc-pair x)
  (cond [(and (eq? (car x) 'assoc-pair)
              (string? (cadr x))
              (expand-value (caddr x)))
         =>
         (lambda (z) `((key ,(cadr x))
                       ,z))]
        [else
         (raise-plist-exn "assoc-pair" (current-continuation-marks) x 'plist:assoc-pair)]))

; expand-value : xexpr -> xexpr
(define (expand-value x)
  (cond [(string? x)
         `(string ,x)]
        [(or (equal? x '(true))
             (equal? x '(false)))
         x]
        [(and (eq? (car x) 'integer)
              (expand-integer x))
         => 
         (lambda (x) x)]
        [(and (eq? (car x) 'real)
              (expand-real x))
         =>
         (lambda (x) x)]
        [(and (eq? (car x) 'dict)
              (expand-dict x))
         =>
         (lambda (x) x)]
        [(and (eq? (car x) 'date)
              (expand-date x))
         =>
         (lambda (x) x)]
        [(and (eq? (car x) 'data)
              (expand-data x))
         =>
         (lambda (x) x)]
        [(and (eq? (car x) 'array)
              (expand-array x))
         =>
         (lambda (x) x)]
        [else
         (raise-plist-exn "value" (current-continuation-marks) x 'plist:value)]))

; expand-real : xexpr -> xexpr
(define (expand-real x)
  (cond [(and (eq? (car x) 'real)
              (real? (cadr x)))
         `(real ,(number->string (cadr x)))]
        [else
         (raise-plist-exn "real" (current-continuation-marks) x 'plist:real)]))

; expand-integer : xexpr -> xexpr
(define (expand-integer x)
  (cond [(and (eq? (car x) 'integer)
              (integer? (cadr x)))
         `(integer ,(number->string (cadr x)))]
        [else
         (raise-plist-exn "integer" (current-continuation-marks) x 'plist:integer)]))

(define (expand-date x)
  (cond [(and (eq? (car x) 'date)
              (string? (cadr x)))
         `(date ,(cadr x))]
        [else
         (raise-plist-exn "date" (current-continuation-marks) x 'plist:date)]))

(define (expand-data x)
  (cond [(and (eq? (car x) 'data)
              (string? (cadr x)))
         `(data ,(cadr x))]
        [else
         (raise-plist-exn "data" (current-continuation-marks) x 'plist:data)]))

; expand-array : xexpr -> xexpr
(define (expand-array x)
  (cond [(and (eq? (car x) 'array)
              (map expand-value (cdr x)))
         =>
         (lambda (x)
           `(array ,@x))]
        [else
         (raise-plist-exn "array" (current-continuation-marks) x 'plist:array)]))

; dict? tst -> boolean
(define (dict? x)
  (with-handlers [(exn:fail:contract? (lambda (exn) #f))]
    (expand-dict x)
    #t))

; write-plist : xexpr port -> (void)
(define (write-plist xexpr port)
  (write-xml 
   (make-document (make-prolog (list (make-p-i #f #f 'xml "version=\"1.0\" encoding=\"UTF-8\"")) 
                               (make-document-type 'plist
                                                   (make-external-dtd/system
                                                    "http://www.apple.com/DTDs/PropertyList-1.0.dtd")
                                                   #f)
                               empty)
                  (xexpr->xml `(plist ((version "0.9"))
                                      ,(expand-value xexpr)))
                  null)
   port))


; collapse-dict : xexpr -> dict
(define (collapse-dict x)
  `(dict ,@(collapse-assoc-pairs (cdr x))))

; collapse-assoc-pairs : (listof xexpr) -> (listof assoc-pairs)
(define (collapse-assoc-pairs args)
  (if (null? args)
      null
      (let ([key (car args)]
            [value (cadr args)]
            [rest (cddr args)])
        (cons `(assoc-pair ,(cadr key) ,(collapse-value value))
              (collapse-assoc-pairs rest)))))

; collapse-value : xexpr -> value
(define (collapse-value value)
  (case (car value)
    [(string) (cadr value)]
    [(true false) value]
    [(integer real) (list (car value) (string->number (cadr value)))]
    [(dict) (collapse-dict value)]
    [(array) (collapse-array value)]
    [(date) value]
    [(data) value]))

; collapse-array : xexpr -> array
(define (collapse-array xexpr)
  `(array ,@(map collapse-value (cdr xexpr))))

(define tags-without-whitespace
  '(plist dict array))

; read-plist : port -> value
(define (read-plist port)
  (let* ([xml-doc (read-xml port)]
         [content (parameterize ([xexpr-drop-empty-attributes #t])
                    (xml->xexpr
                     ((eliminate-whitespace tags-without-whitespace (lambda (x) x))
                      (document-element xml-doc))))])
    (unless (eq? (car content) 'plist)
      (error 'read-plist "xml expression is not a plist: ~a" content))
    (collapse-value (caddr content))))

(provide
 (contract-out
  [plist-dict? (any/c . -> . boolean?)]
  [plist-value? (any/c . -> . boolean?)]
  [read-plist (input-port? . -> . plist-value?)]
  [write-plist (plist-value? output-port? . -> . void?)]))

