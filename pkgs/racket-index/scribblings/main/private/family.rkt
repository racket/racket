#lang at-exp racket/base
(require racket/string
         scribble/manual
         scribble/core
         scribble/html-properties
         setup/language-family)

(provide make-family-page)

(define family-js (collection-file-path "family.js" "scribblings/main/private"))

(define (make-family-page mode)
  (define fams
    (sort
     (get-language-families #:user? (eq? mode 'user))
     (lambda (a b)
       (define ao (hash-ref a 'order 0))
       (define bo (hash-ref b 'order 0))
       (if (= ao bo)
           (string<? (hash-ref a 'fam "???") (hash-ref b 'fam "???"))
           (> ao bo)))))
  
  (list
   @title[#:style (style #f (list (js-addition family-js)))]{Language Family}

   @para{The Racket ecosystem supports multiple languages, all with the same
             notion of modules so that a program can be implemented with
             multiple languages at once. A @defterm{language family} represents
             a group of languages that broadly share syntactic conventions.
             Documentation similarly covers multiple languages.
             @defterm{Navigating} with respect to a language family
             affects how search results are sorted and may affect how
             @onscreen{top} and @onscreen{up} links work.}

   @para{Select a language family for navigation:}

   @(for/list ([fam (in-list fams)])
      (define famroot (hash-ref fam 'famroot #f))
      (define href (format "../~aindex.html?fam=~a~a"
                           (if famroot (format "~a/" famroot) "")
                           (hash-ref fam 'fam)
                           (if famroot
                               (format "&famroot=~a" famroot)
                               "")))
      (define desc
        (cond
          [(hash-ref fam 'doc #f)
           => (lambda (doc) @elem{ --- see @other-doc[doc]})]
          [else null]))
      @para{@hspace[1]@hyperlink[#:style "famlink" href]{@(hash-ref fam 'fam "???")}@desc})))
