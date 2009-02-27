#lang scheme/base
(require "../decode.ss"
         "../struct.ss"
         "../basic.ss"
         "../manual-struct.ss"
         "manual-ex.ss"
         "manual-style.ss"
         "manual-scheme.ss"
         scheme/string
         scheme/list
         (for-syntax scheme/base)
         (for-label scheme/base))

(provide defmodule defmodule* defmodulelang defmodulelang* defmodulereader defmodulereader*
         defmodule*/no-declare defmodulelang*/no-declare defmodulereader*/no-declare
         declare-exporting)

(define spacer (hspace 1))

(define-syntax defmodule*/no-declare
  (syntax-rules ()
    [(_ #:require-form req (name ...) . content)
     (*defmodule (list (schememodname name) ...)
                 #f
                 (list . content)
                 req)]
    [(_ (name ...) . content)
     (defmodule*/no-declare #:require-form (scheme require) (name ...) . content)]))

(define-syntax defmodule*
  (syntax-rules ()
    [(_ #:require-form req (name ...) #:use-sources (pname ...) . content)
     (begin (declare-exporting name ... #:use-sources (pname ...))
            (defmodule*/no-declare #:require-form req (name ...) . content))]
    [(_ #:require-form req (name ...) . content)
     (defmodule* #:require-form req (name ...) #:use-sources () . content)]
    [(_ (name ...) #:use-sources (pname ...) . content)
     (defmodule* #:require-form (scheme require) (name ...) #:use-sources (pname ...) . content)]
    [(_ (name ...) . content)
     (defmodule* (name ...) #:use-sources () . content)]))

(define-syntax defmodule
  (syntax-rules ()
    [(_ #:require-form req name . content)
     (defmodule* #:require-form req (name) . content)]
    [(_ name . content)
     (defmodule* (name) . content)]))

(define-syntax-rule (defmodulelang*/no-declare (lang ...) . content)
  (*defmodule (list (schememodname lang) ...) #t (list . content) #f))

(define-syntax defmodulelang*
  (syntax-rules ()
    [(_ (name ...) #:use-sources (pname ...) . content)
     (begin (declare-exporting name ... #:use-sources (pname ...))
            (defmodulelang*/no-declare (name ...) . content))]
    [(_ (name ...) . content)
     (defmodulelang* (name ...) #:use-sources () . content)]))

(define-syntax-rule (defmodulelang lang . content)
  (defmodulelang* (lang) . content))

(define-syntax-rule (defmodulereader*/no-declare (lang ...) . content)
  (*defmodule (list (schememodname lang) ...) 'reader (list . content) #f))

(define-syntax defmodulereader*
  (syntax-rules ()
    [(_ (name ...) #:use-sources (pname ...) . content)
     (begin (declare-exporting name ... #:use-sources (pname ...))
            (defmodulereader*/no-declare (name ...) . content))]
    [(_ (name ...) . content)
     (defmodulereader* (name ...) #:use-sources () . content)]))

(define-syntax-rule (defmodulereader lang . content)
  (defmodulereader* (lang) . content))

(define (*defmodule names lang content req)
  (make-splice
   (cons
    (make-table
     "defmodule"
     (map
      (lambda (name)
        (list
         (make-flow
          (list
           (make-omitable-paragraph
            (cons
             spacer
             (if lang
                 (if (eq? lang 'reader)
                     (list (schememetafont "#reader") spacer (make-defschememodname name))
                     (list (hash-lang) spacer (make-defschememodname name)))
                 (list (scheme (#,req #,(make-defschememodname name)))))))))))
      names))
    (append (map (lambda (name)
                   (make-part-tag-decl `(mod-path ,(element->string name))))
                 names)
            (flow-paragraphs (decode-flow content))))))

(define (make-defschememodname mn)
  (let ([name-str (element->string mn)])
    (make-index-element #f
                        (list mn)
                        `(mod-path ,name-str)
                        (list name-str)
                        (list mn)
                        (make-module-path-index-desc))))

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
