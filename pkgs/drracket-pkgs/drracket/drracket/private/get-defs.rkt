#lang racket/base

(require racket/class
         racket/function
         racket/list
         string-constants)

(provide (struct-out defn)
         get-definitions)

;; defn = (make-defn number string number number)
(define-struct defn (indent name start-pos end-pos) #:mutable)

;; get-definitions : string boolean text -> (listof defn)
(define (get-definitions tag-string indent? text)
  (let* ([min-indent 0]
         [defs (let loop ([pos 0])
                 (let ([defn-pos (send text find-string tag-string 'forward pos 'eof #t #f)])
                   (cond
                     [(not defn-pos) null]
                     [(in-semicolon-comment? text defn-pos)
                      (loop (+ defn-pos (string-length tag-string)))]
                     [else
                      (let ([indent (get-defn-indent text defn-pos)]
                            [name (get-defn-name text (+ defn-pos (string-length tag-string)))])
                        (set! min-indent (min indent min-indent))
                        (cons (make-defn indent name defn-pos defn-pos)
                              (loop (+ defn-pos (string-length tag-string)))))])))])
    
    ;; update end-pos's based on the start pos of the next defn
    (unless (null? defs)
      (let loop ([first (car defs)]
                 [defs (cdr defs)])
        (cond
          [(null? defs) 
           (set-defn-end-pos! first (send text last-position))]
          [else (set-defn-end-pos! first (max (- (defn-start-pos (car defs)) 1)
                                              (defn-start-pos first)))
                (loop (car defs) (cdr defs))])))
    
    (when indent?
      (for-each (λ (defn)
                  (set-defn-name! defn
                                  (string-append
                                   (apply string
                                          (vector->list
                                           (make-vector 
                                            (- (defn-indent defn) min-indent) #\space)))
                                   (defn-name defn))))
                defs))
    defs))

;; in-semicolon-comment: text number -> boolean
;; returns #t if `define-start-pos' is in a semicolon comment and #f otherwise
(define (in-semicolon-comment? text define-start-pos)
  (let* ([para (send text position-paragraph define-start-pos)]
         [start (send text paragraph-start-position para)])
    (let loop ([pos start])
      (cond
        [(pos . >= . define-start-pos) #f]
        [(char=? #\; (send text get-character pos)) #t]
        [else (loop (+ pos 1))]))))

;; get-defn-indent : text number -> number
;; returns the amount to indent a particular definition
(define (get-defn-indent text pos)
  (let* ([para (send text position-paragraph pos)]
         [para-start (send text paragraph-start-position para #t)])
    (let loop ([c-pos para-start]
               [offset 0])
      (if (< c-pos pos)
          (let ([char (send text get-character c-pos)])
            (cond
              [(char=? char #\tab)
               (loop (+ c-pos 1) (+ offset (- 8 (modulo offset 8))))]
              [else
               (loop (+ c-pos 1) (+ offset 1))]))
          offset))))

;; whitespace-or-paren?
(define (whitespace-or-paren? char)
  (or (char=? #\) char)
      (char=? #\( char)
      (char=? #\] char)
      (char=? #\[ char)
      (char-whitespace? char)))

;; skip : text number (char -> bool) -> number
;; Skips characters recognized by `skip?'
(define (skip text pos skip?)
  (let loop ([pos pos])
    (if (>= pos (send text last-position))
        (send text last-position)
        (let ([char (send text get-character pos)])
          (cond
            [(skip? char)
             (loop (+ pos 1))]
            [else pos])))))

;; skip-to-whitespace/paren : text number -> number
;; skips to the next parenthesis or whitespace after `pos', returns that position.
(define (skip-to-whitespace/paren text pos)
  (skip text pos (negate whitespace-or-paren?)))

;; skip-whitespace/paren : text number -> number
;; skips past any parenthesis or whitespace
(define (skip-whitespace/paren text pos)
  (skip text pos whitespace-or-paren?))

;; skip-past-non-whitespace : text number -> number
;; skips to the first whitespace character after the first non-whitespace character
(define (skip-past-non-whitespace text pos)
  (skip text (skip text pos char-whitespace?) (negate char-whitespace?)))

;; get-defn-name : text number -> string
;; returns the name of the definition starting at `define-pos',
;; assuming that the bound name is the first symbol to appear
;; after the one beginning at `define-pos'. This heuristic 
;; usually finds the bound name, but it breaks for Redex
;; metafunction definitions (thus the hack below).
(define (get-defn-name text define-pos)
  (let* ([end-suffix-pos (skip-to-whitespace/paren text define-pos)]
         [suffix (send text get-text define-pos end-suffix-pos)]
         [end-header-pos 
          (cond [(regexp-match #rx"^-metafunction(/extension)?$" suffix)
                 => (λ (m)
                      (let ([extension? (second m)])
                        (skip-past-non-whitespace
                         text
                         (if extension?
                             (skip-past-non-whitespace text end-suffix-pos)
                             end-suffix-pos))))]
                [else end-suffix-pos])]
         [start-name-pos (skip-whitespace/paren text end-header-pos)]
         [end-name-pos (skip-to-whitespace/paren text start-name-pos)])
    (if (>= end-suffix-pos (send text last-position))
        (string-constant end-of-buffer-define)
        (send text get-text start-name-pos end-name-pos))))
