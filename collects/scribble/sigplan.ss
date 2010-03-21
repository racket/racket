#lang scheme/base
(require setup/main-collects
         scribble/core
         scribble/base
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         (for-syntax scheme/base))

(provide preprint 10pt
         abstract include-abstract
         authorinfo
         conferenceinfo copyrightyear copyrightdata
         category terms keywords)

(define-syntax (preprint stx)
  (raise-syntax-error #f
                      "option must appear on the same line as `#lang scribble/sigplan'"
                      stx))
(define-syntax (10pt stx)
  (raise-syntax-error #f
                      "option must appear on the same line as `#lang scribble/sigplan'"
                      stx))

(define sigplan-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (build-path (collection-path "scribble") "sigplan" s)))])
    (list
     (make-css-addition (abs "sigplan.css"))
     (make-tex-addition (abs "sigplan.tex")))))

;; ----------------------------------------
;; Abstracts:

(define abstract-style (make-style "abstract" sigplan-extras))

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
;; Authors and conference info:

(define (authorinfo name affiliation e-mail)
  (author
   (make-multiarg-element
    (make-style "SAuthorinfo"sigplan-extras)
    (list
     (make-element #f (decode-content (list name)))
     (make-element (make-style "SAuthorPlace" sigplan-extras)
                   (decode-content (list affiliation)))
     (make-element (make-style "SAuthorEmail" sigplan-extras)
                   (decode-content (list e-mail)))))))

(define (conferenceinfo what where)
  (make-paragraph
   (make-style 'pretitle null)
   (make-multiarg-element
    (make-style "SConferenceInfo" sigplan-extras)
    (list
     (make-element #f (decode-content (list what)))
     (make-element #f (decode-content (list where)))))))

(define (copyrightyear . when)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "SCopyrightYear" sigplan-extras)
    (decode-content when))))

(define (copyrightdata . what)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "SCopyrightData" sigplan-extras)
    (decode-content what))))

;; ----------------------------------------
;; Categories, terms, and keywords:

(define (category sec title sub [more #f])
  (make-multiarg-element
   (make-style (format "SCategory~a" (if more "Plus" "")) sigplan-extras)
   (append
    (list
     (make-element #f (decode-content (list sec)))
     (make-element #f (decode-content (list title)))
     (make-element #f (decode-content (list sub))))
    (if more
        (list (make-element #f (decode-content (list more))))
        null))))

(define (terms . str)
  (make-element (make-style "STerms" sigplan-extras) (decode-content str)))

(define (keywords . str)
  (make-element (make-style "SKeywords" sigplan-extras) (decode-content str)))
