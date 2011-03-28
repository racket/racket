#lang racket/base
(require racket/class
         "syntax.rkt"
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
      (hash-ref table val (lambda ()
                            (let ([n (add1 (hash-count table))])
                              (hash-set! table val n)
                              (hash-set! reverse-table n val)
                              n))))

    (for-each (lambda  (s) (intern s))
              '(default decorative roman script
                 swiss modern symbol system))

    (def/public (find-family-default-font-id [family-symbol? family])
      (intern family))

    (def/public (find-or-create-font-id [string? name]
                                        [family-symbol? family])
      (intern (cons name family)))

    (def/public (get-face-name [exact-integer? id])
      (let ([v (hash-ref reverse-table id #f)])
        (and v (pair? v) (car v))))
    
    (def/public (get-family [exact-integer? id])
      (let ([v (hash-ref reverse-table id #f)])
        (or (and (pair? v) (cdr v))
            (and (symbol? v) v)
            'default)))

    (def/public (get-font-id [string? name]
                             [family-symbol? family])
      (hash-ref table (cons string family) 0))

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
                    [(macosx) "Apple Chancery"]
                    [else "Chancery"])]
        [(symbol) "Symbol"]
        [else (case (system-type)
		[(windows) "Tahoma"]
                [(macosx) "Lucida Grande"]
                [else "Sans"])]))

    (def/public (get-post-script-name [exact-integer? id]
                                      [weight-symbol? w]
                                      [style-symbol? s])
      (let ([s (or (hash-ref ps-table (list id w s) #f)
                   (hash-ref reverse-table id #f))])
        (cond
         [(pair? s) (car s)]
         [(symbol? s) (default-font s)]
         [else "Serif"])))

    (def/public (get-screen-name [exact-integer? id]
                                 [weight-symbol? w]
                                 [style-symbol? s])
      (let ([s (or (hash-ref screen-table (list id w s) #f)
                   (hash-ref reverse-table id #f))])
        (cond
         [(pair? s) (car s)]
         [(symbol? s) (default-font s)]
         [else "Serif"])))

    (def/public (set-post-script-name [exact-integer? id]
                                      [weight-symbol? w]
                                      [style-symbol? s]
                                      [string? name])
      (hash-set! ps-table (list id w s) name))

    (def/public (set-screen-name [exact-integer? id]
                                 [weight-symbol? w]
                                 [style-symbol? s]
                                 [string? name])
      (hash-set! screen-table (list id w s) name))

    (super-new)))

(define font-name-directory<%>
  (class->interface font-name-directory%))

(define the-font-name-directory (new font-name-directory%))

(define (get-family-builtin-face family)
  (unless (memq family '(default decorative roman script swiss modern system symbol))
    (raise-type-error 'get-family-builtin-face "family symbol" family))
  (let ([id (send the-font-name-directory find-family-default-font-id family)])
    (send the-font-name-directory get-screen-name id 'normal 'normal)))

