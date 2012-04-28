#lang scheme/base
(require "../decode.rkt"
         "../struct.rkt"
         "../basic.rkt"
         "../manual-struct.rkt"
         "manual-ex.rkt"
         "manual-style.rkt"
         "manual-scheme.rkt"
         (for-syntax scheme/base)
         (for-label scheme/base))

(provide defmodule defmodule* 
         defmodulelang defmodulelang* 
         defmodulereader defmodulereader*
         defmodule*/no-declare defmodulelang*/no-declare defmodulereader*/no-declare
         declare-exporting)

;; ---------------------------------------------------------------------------------------------------
(provide deprecated)

(require (only-in scribble/core make-style make-background-color-property)
         (only-in scribble/base para nested))

;; @deprecated[Precontent]{Precontent ... }
;; produces a nested paragraph with a yellow NOTE label to warn readers of deprecated modules 
(define-syntax-rule
  (deprecated replacement-library additional-notes ...)
  ;; ==> 
  (nested #:style 'inset
          (para (yellow (bold "NOTE:"))
                " This library is deprecated. Use "
                replacement-library
                " instead. "
                additional-notes ...)))

(define (yellow . content)
  (make-element (make-style #f (list (make-background-color-property "yellow"))) content))
;; ---------------------------------------------------------------------------------------------------

(define spacer (hspace 1))

(define-syntax defmodule*/no-declare
  (syntax-rules ()
    [(_ #:require-form req (name ...) . content)
     (*defmodule (list (racketmodname name) ...)
                 #f
                 #f
                 (list . content)
                 req)]
    [(_ (name ...) . content)
     (defmodule*/no-declare #:require-form (racket require) (name ...)
       . content)]))

(define-syntax defmodule*
  (syntax-rules ()
    [(_ #:require-form req (name ...) #:use-sources (pname ...) . content)
     (begin (declare-exporting name ... #:use-sources (pname ...))
            (defmodule*/no-declare #:require-form req (name ...) . content))]
    [(_ #:require-form req (name ...) . content)
     (defmodule* #:require-form req (name ...) #:use-sources () . content)]
    [(_ (name ...) #:use-sources (pname ...) . content)
     (defmodule* #:require-form (racket require) (name ...) #:use-sources (pname ...)
       . content)]
    [(_ (name ...) . content)
     (defmodule* (name ...) #:use-sources () . content)]))

(define-syntax defmodule
  (syntax-rules ()
    [(_ #:require-form req name . content)
     (defmodule* #:require-form req (name) . content)]
    [(_ name . content)
     (defmodule* (name) . content)]))

(define-syntax defmodulelang*/no-declare
  (syntax-rules ()
    [(_ (lang ...) #:module-paths (modpath ...) . content)
     (*defmodule (list lang ...)
                 (list (racketmodname modpath) ...)
                 #t (list . content) #f)]
    [(_ (lang ...) . content)
     (*defmodule (list (racketmodname lang) ...)
                 #f #t (list . content) #f)]))

(define-syntax defmodulelang*
  (syntax-rules ()
    [(_ (name ...) #:module-paths (modpath ...)
        #:use-sources (pname ...)
        . content)
     (begin (declare-exporting modpath ... #:use-sources (pname ...))
            (defmodulelang*/no-declare (name ...)
              #:module-paths (modpath ...)
              . content))]
    [(_ (name ...) #:module-paths (modpath ...) . content)
     (defmodulelang* (name ...)
       #:module-paths (modpath ...)
       #:use-sources () . content)]
    [(_ (name ...) #:use-sources (pname ...) . content)
     (defmodulelang* ((racketmodname name) ...)
       #:module-paths (name ...)
       #:use-sources (pname ...) . content)]
    [(_ (name ...) . content)
     (defmodulelang* (name ...) #:use-sources () . content)]))

(define-syntax defmodulelang
  (syntax-rules ()
    [(_ lang #:module-path modpath . content)
     (defmodulelang* (lang) #:module-paths (modpath) . content)]
    [(_ lang . content)
     (defmodulelang* (lang) . content)]))

(define-syntax-rule (defmodulereader*/no-declare (lang ...) . content)
  (*defmodule (list (racketmodname lang) ...)
              #f 'reader (list . content) #f))

(define-syntax defmodulereader*
  (syntax-rules ()
    [(_ (name ...) #:use-sources (pname ...) . content)
     (begin (declare-exporting name ... #:use-sources (pname ...))
            (defmodulereader*/no-declare (name ...) . content))]
    [(_ (name ...) . content)
     (defmodulereader* (name ...) #:use-sources () . content)]))

(define-syntax-rule (defmodulereader lang . content)
  (defmodulereader* (lang) . content))

(define (*defmodule names modpaths lang content req)
  (let ([modpaths (or modpaths names)])
    (make-splice
     (cons
      (make-table
       "defmodule"
       (map
        (lambda (name modpath)
          (list
           (make-flow
            (list
             (make-omitable-paragraph
              (cons
               spacer
               (case lang
                 [(#f)
                  (list (racket (#,req #,(make-defracketmodname name modpath))))]
                 [(#t)
                  (list (hash-lang) spacer (make-defracketmodname name modpath))]
                 [(reader)
                  (list (racketmetafont "#reader") spacer (make-defracketmodname name modpath))]
                 [(just-lang)
                  (list (hash-lang) spacer (make-defracketmodname name modpath))])))))))
        names
        modpaths))
      (append (map (lambda (modpath)
                     (make-part-tag-decl 
                      (intern-taglet
                       `(mod-path ,(datum-intern-literal
                                    (element->string modpath))))))
                   modpaths)
              (flow-paragraphs (decode-flow content)))))))

(define the-module-path-index-desc (make-module-path-index-desc))

(define (make-defracketmodname mn mp)
  (let ([name-str (datum-intern-literal (element->string mn))]
        [path-str (datum-intern-literal (element->string mp))])
    (make-index-element #f
                        (list mn)
                        (intern-taglet `(mod-path ,path-str))
                        (list name-str)
                        (list mn)
                        the-module-path-index-desc)))

(define-syntax (declare-exporting stx)
  (syntax-case stx ()
    [(_ lib ... #:use-sources (plib ...))
     (let ([libs (syntax->list #'(lib ... plib ...))])
       (for ([l libs])
         (unless (module-path? (syntax->datum l))
           (raise-syntax-error #f "not a module path" stx l)))
       (when (null? libs)
         (raise-syntax-error #f "need at least one module path" stx))
       #'(*declare-exporting '(lib ...) '(plib ...)))]
    [(_ lib ...) #'(*declare-exporting '(lib ...) '())]))

(define (*declare-exporting libs source-libs)
  (make-splice
   (list
    (make-part-collect-decl
     (make-collect-element
      #f null
      (lambda (ri) (collect-put! ri '(exporting-libraries #f) libs))))
    (make-part-collect-decl
     (make-exporting-libraries #f null (and (pair? libs) libs) source-libs)))))
