#lang scheme/base
(require "../struct.ss"
         "../decode.ss"
         "../base.ss"
         (only-in "../core.ss"
                   content?)
         scheme/contract
         scheme/list)

(provide doc-prefix)
(provide/contract
 [spacer element?]
 [to-flow (content? . -> . flow?)]
 [flow-spacer flow?]
 [flow-empty-line flow?]
 [make-table-if-necessary (content? list? . -> . (list/c (or/c omitable-paragraph? table?)))]
 [max-proto-width exact-nonnegative-integer?])

(define spacer (hspace 1))

(define (to-flow e)
  (make-flow (list (make-omitable-paragraph (list e)))))
(define flow-spacer (to-flow spacer))
(define flow-empty-line (to-flow (tt 'nbsp)))

(define (make-table-if-necessary style content)
  (if (= 1 (length content))
    (let ([paras (append-map flow-paragraphs (car content))])
      (if (andmap paragraph? paras)
        (list (make-omitable-paragraph (append-map paragraph-content paras)))
        (list (make-table style content))))
    (list (make-table style content))))

(define max-proto-width 65)
