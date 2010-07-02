#lang at-exp s-exp meta/web/html

;; list of a header paragraphs and sub paragraphs (don't use `p' since it looks
;; like they should not be nested)
(provide parlist)
(define (parlist first . rest)
  (list (div class: 'parlisttitle first)
        (map (lambda (p) (div class: 'parlistitem p)) rest)))

;; a div that is centered, but the text is still left-justified
(provide center-div)
(define (center-div . text)
  (let-values ([(attrs body) (split-attributes+body text)])
    (apply div align: 'center
           (append attrs
                   (list (div align: 'left style: "display: inline-block;"
                              body))))))

;; a grayish tt text
(provide TT)
(define (TT . xs)
  @tt[style: "background-color: #dde;"]{@xs})
