#lang scheme/base

(provide get-string/style-desc)
(require scheme/gui/base
         scheme/class)

;; get-string/style-desc : text -> (listof str/ann)
(define (get-string/style-desc text [start 0] [end (send text last-position)])
  (let* ([snips (get-snips text start end)]
         [str/ann (map snip->str/ann snips)]
         [joined-str/ann (join-like str/ann)])
    joined-str/ann))

;; get-snips : text -> (listof snip)
;; extracts the snips from a text
(define (get-snips text start end)
  (send text split-snip start)
  (send text split-snip end)
  (let loop ([snip (send text find-snip start 'after-or-none)])
    (cond
      [(not snip) null]
      [(< (send text get-snip-position snip) end)
       (cons snip (loop (send snip next)))]
      [else null])))

;; snip->str/ann : snip -> str/ann
;; extracts the style type from the snip
(define (snip->str/ann snip)
  (let* ([str (cond
                [(is-a? snip string-snip%)
                 (send snip get-text 0 (send snip get-count))]
                [(is-a? snip image-snip%)
                 'image]
                [else 'unknown])]
         [style (send snip get-style)]
         [style-name (send style get-name)]
         [style-desc (if style-name
                         (translate-name style-name)
                         (describe-style style))])
    (list str style-desc)))

;; describe-style : style -> (listof symbol)
(define (describe-style style)
  (list
   'alignment (send style get-alignment)
   'background (symbolic-color (send style get-background))
   'face (send style get-face)
   'family (send style get-family)
   'foreground (symbolic-color (send style get-foreground))
   'size (send style get-size)
   'underlined (send style get-underlined)))

(define (symbolic-color color)
  (list (send color red)
        (send color green)
        (send color blue)))

;; translate-name : (union #f string) -> symbol
;; translates the style name to a symbol
(define (translate-name str)
  (and str
       (let ([m (regexp-match re:translate-name str)])
         (and m
              (string->symbol (cadr m))))))

;; re:translate-name : regexp
(define re:translate-name (regexp "^.*:([^:]*)$"))

;; join-like : (listof str/ann) -> (listof str/ann)
;; joins same styles to form largest groups
(define (join-like str/anns)
  (cond
    [(null? str/anns) null]
    [else 
     (let loop ([first (car str/anns)]
                [rest (cdr str/anns)])
       (cond
         [(null? rest) (list first)]
         [else
          (let ([second (car rest)])
            (if (and (equal? (cadr first) (cadr second))
                     (string? (car first))
                     (string? (car second)))
                (loop (list (string-append (car first) (car second))
                            (cadr first))
                      (cdr rest))
                (cons first
                      (loop second
                            (cdr rest)))))]))]))
