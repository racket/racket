#lang scheme/base
(require "../struct.rkt"
         "../base.rkt"
         (only-in "../core.rkt"
                   content?)
         racket/contract/base
         scheme/list)

(provide doc-prefix)
(provide/contract
 [spacer element?]
 [to-flow (content? . -> . flow?)]
 [flow-spacer flow?]
 [flow-spacer/n (-> exact-nonnegative-integer? flow?)]
 [flow-empty-line flow?]
 [make-table-if-necessary (content? list? . -> . (list/c (or/c omitable-paragraph? table?)))]
 [current-display-width (parameter/c exact-nonnegative-integer?)])

(define spacer (hspace 1))

(define (to-flow e)
  (make-flow (list (make-omitable-paragraph (list e)))))
(define flow-spacer (to-flow spacer))
(define (flow-spacer/n n) (to-flow (hspace n)))
(define flow-empty-line (to-flow (tt 'nbsp)))

(define (make-table-if-necessary style content)
  (if (= 1 (length content))
    (let ([paras (append-map flow-paragraphs (car content))])
      (if (andmap paragraph? paras)
        (list (make-omitable-paragraph (append-map paragraph-content paras)))
        (list (make-table style content))))
    (list (make-table style content))))

(define current-display-width (make-parameter 65))
