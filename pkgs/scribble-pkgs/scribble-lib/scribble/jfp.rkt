#lang scheme/base
(require setup/main-collects
         scribble/core
         (except-in scribble/base author)
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         (for-syntax scheme/base))

(provide abstract include-abstract
         author author/short
         affiliation affiliation-mark affiliation-sep)

(define jfp-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scribble" "jfp")))])
    (list
     (make-css-addition (abs "jfp.css"))
     (make-tex-addition (abs "jfp.tex")))))

;; ----------------------------------------
;; Abstracts:

(define abstract-style (make-style "abstract" jfp-extras))

(define (abstract . strs)
  (make-nested-flow
   abstract-style
   (decode-flow strs)))

(define (extract-abstract p)
  (unless (part? p)
    (error 'include-abstract "doc binding is not a part: ~e" p))
  (unless (null? (part-parts p))
    (error 'include-abstract "abstract part has sub-parts: ~e" (part-parts p)))
  (when (part-title-content p)
    (error 'include-abstract "abstract part has title content: ~e" (part-title-content p)))
  (part-blocks p))

(define-syntax-rule (include-abstract mp)
  (begin
    (require (only-in mp [doc abstract-doc]))
    (make-nested-flow abstract-style (extract-abstract abstract-doc))))

;; ----------------------------------------
;; Author

(define (author . long)
  (apply (apply author/short long) long))

(define ((author/short . short) . long)
  (make-paragraph
   (make-style 'author jfp-extras)
   (list
    (make-multiarg-element "JFPAuthor" (list (decode-content short)
                                             (decode-content long))))))

(define (affiliation . txt)
  (make-element (make-style "Affiliation" jfp-extras) (decode-content txt)))

(define (affiliation-mark . txt)
  (make-element (make-style "MarkSuperscript" jfp-extras) (decode-content txt)))

(define (affiliation-sep)
  (hspace 2))
