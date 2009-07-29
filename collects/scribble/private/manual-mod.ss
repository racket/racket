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

(provide defmodule defmodule* 
         defmodulelang defmodulelang* 
         defmodulereader defmodulereader*
         defmodule*/no-declare defmodulelang*/no-declare defmodulereader*/no-declare
         declare-exporting)

(define spacer (hspace 1))

(define-syntax defmodule*/no-declare
  (syntax-rules ()
    [(_ #:require-form req (name ...) . content)
     (*defmodule (list (schememodname name) ...)
                 #f
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

(define-syntax defmodulelang*/no-declare
  (syntax-rules ()
    [(_ (lang ...) #:module-paths (modpath ...) . content)
     (*defmodule (list lang ...) (list (schememodname modpath) ...) #t (list . content) #f)]
    [(_ (lang ...) . content)
     (*defmodule (list (schememodname lang) ...) #f #t (list . content) #f)]))

(define-syntax defmodulelang*
  (syntax-rules ()
    [(_ (name ...) #:module-paths (modpath ...) #:use-sources (pname ...) . content)
     (begin (declare-exporting modpath ... #:use-sources (pname ...))
            (defmodulelang*/no-declare (name ...) #:module-paths (modpath ...) . content))]
    [(_ (name ...) #:module-paths (modpath ...) . content)
     (defmodulelang* (name ...) #:module-paths (modpath ...) #:use-sources () . content)]
    [(_ (name ...) #:use-sources (pname ...) . content)
     (defmodulelang* ((schememodname name) ...) #:module-paths (name ...) #:use-sources (pname ...) . content)]
    [(_ (name ...) . content)
     (defmodulelang* (name ...) #:use-sources () . content)]))

(define-syntax defmodulelang
  (syntax-rules ()
    [(_ lang #:module-path modpath . content)
     (defmodulelang* (lang) #:module-paths (modpath) . content)]
    [(_ lang . content)
     (defmodulelang* (lang) . content)]))

(define-syntax-rule (defmodulereader*/no-declare (lang ...) . content)
  (*defmodule (list (schememodname lang) ...) #f 'reader (list . content) #f))

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
                  (list (scheme (#,req #,(make-defschememodname name modpath))))]
                 [(#t)
                  (list (hash-lang) spacer (make-defschememodname name modpath))]
                 [(reader)
                  (list (schememetafont "#reader") spacer (make-defschememodname name modpath))]
                 [(just-lang)
                  (list (hash-lang) spacer (make-defschememodname name modpath))])))))))
        names
        modpaths))
      (append (map (lambda (modpath)
                     (make-part-tag-decl `(mod-path ,(element->string modpath))))
                   modpaths)
              (flow-paragraphs (decode-flow content)))))))

(define (make-defschememodname mn mp)
  (let ([name-str (element->string mn)]
        [path-str (element->string mp)])
    (make-index-element #f
                        (list mn)
                        `(mod-path ,path-str)
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
