#lang racket/base
(require racket/contract/base)

;; Core structures needed for `xml/xexpr'

(provide (all-defined-out))

; permissive-xexprs : parameter bool
(define permissive-xexprs (make-parameter #f))

; Source = (make-source Location Location)
(define-struct source (start stop) #:prefab)

; Comment = (make-comment String)
(define-struct comment (text) #:prefab)

; Processing-instruction = (make-p-i Location Location String String)
; also represents XMLDecl
(define-struct (p-i source) (target-name instruction) #:prefab)

; Pcdata = (make-pcdata Location Location String)
(define-struct (pcdata source) (string) #:prefab)

; Cdata = (make-cdata Location Location String)
(define-struct (cdata source) (string) #:prefab)

; Section 2.2 of XML 1.1
; (XML 1.0 is slightly different and more restrictive)
(define (valid-char? i)
  (and (exact-nonnegative-integer? i)
       (or (<= #x1     i #xD7FF)
           (<= #xE000  i #xFFFD)
           (<= #x10000 i #x10FFFF))))
