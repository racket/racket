#lang racket/base
(require racket/class
         racket/contract/base
         "lock.rkt"
         "font-syms.rkt")

(provide font-name-directory<%>
         the-font-name-directory
         get-family-builtin-face)

(define font-name-directory%
  (class object%

    (define table (make-hash))
    (define reverse-table (make-hash))
    (define ps-table (make-hash))
    (define screen-table (make-hash))

    (define/private (intern val)
      (atomically
       (hash-ref table val (lambda ()
                             (let ([n (add1 (hash-count table))])
                               (hash-set! table val n)
                               (hash-set! reverse-table n val)
                               n)))))

    (for-each (lambda  (s) (intern s))
              '(default decorative roman script
                 swiss modern symbol system))

    (define/public (find-family-default-font-id family)
      (intern family))

    (define/public (find-or-create-font-id name family)
      (intern (cons name family)))

    (define/public (get-face-name id)
      (let ([v (atomically (hash-ref reverse-table id #f))])
        (and v (pair? v) (car v))))
    
    (define/public (get-family id)
      (let ([v (atomically (hash-ref reverse-table id #f))])
        (or (and (pair? v) (cdr v))
            (and (symbol? v) v)
            'default)))

    (define/public (get-font-id name family)
      (atomically (hash-ref table (cons string family) 0)))

    (define/private (default-font s)
      (case s
        [(modern) (case (system-type)
                    [(windows macosx) "Courier New"]
                    [else "Monospace"])]
        [(roman) (case (system-type)
                   [(windows) "Times New Roman"]
                   [(macosx) "Times"]
                   [else "Serif"])]
        [(decorative swiss) (case (system-type)
                              [(windows) "Arial"]
                              [else "Helvetica"])]
        [(script) (case (system-type)
                    [(macosx) "Apple Chancery, Italic"]
		    [(windows) "Palatino Linotype, Italic"]
                    [else "Chancery"])]
        [(symbol) "Symbol"]
        [else (case (system-type)
		[(windows) "Tahoma"]
                [(macosx) "Lucida Grande"]
                [else "Sans"])]))

    (define/public (get-post-script-name id w s)
      (let ([s (atomically
                (or (hash-ref ps-table (list id w s) #f)
                    (hash-ref reverse-table id #f)))])
        (cond
         [(pair? s) (car s)]
         [(symbol? s) (default-font s)]
         [else "Serif"])))

    (define/public (get-screen-name id w s)
      (let ([s (atomically
                (or (hash-ref screen-table (list id w s) #f)
                    (hash-ref reverse-table id #f)))])
        (cond
         [(pair? s) (car s)]
         [(symbol? s) (default-font s)]
         [else "Serif"])))

    (define/public (set-post-script-name id w s name)
      (atomically (hash-set! ps-table (list id w s) name)))

    (define/public (set-screen-name id w s name)
      (atomically (hash-set! screen-table (list id w s) name)))

    (super-new)))

(define font-name-directory<%>
  (interface ()
    [find-family-default-font-id (->m font-family/c exact-integer?)]
    [fint-or-create-font-id (->m string? font-family/c exact-integer?)]
    [get-face-name (->m exact-integer? (or/c string? #f))]
    [get-family (->m exact-integer? font-family/c)]
    [get-font-id (->m string? font-family/c exact-integer?)]
    [get-post-script-name
     (->m exact-integer? font-weight/c font-style/c (or/c string? #f))]
    [get-screen-name
     (->m exact-integer? font-weight/c font-style/c (or/c string? #f))]
    [set-post-script-name
     (->m exact-integer? font-weight/c font-style/c string? any)]
    [set-screen-name
     (->m exact-integer? font-weight/c font-style/c string? any)]))

(define the-font-name-directory (new font-name-directory%))

(define (get-family-builtin-face family)
  (unless (memq family '(default decorative roman script swiss modern system symbol))
    (raise-type-error 'get-family-builtin-face "family symbol" family))
  (let ([id (send the-font-name-directory find-family-default-font-id family)])
    (send the-font-name-directory get-screen-name id 'normal 'normal)))

