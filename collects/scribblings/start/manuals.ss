#lang scheme/base

(require scribble/manual
         scribble/struct
         scribble/decode
         setup/getinfo
         setup/main-collects
         setup/dirs)

(provide build-contents)

(define (resolve s)
  (resolved-module-path-name
   (module-path-index-resolve
    (module-path-index-join `(lib ,(string-append s ".scrbl")
                                  "scribblings"
                                  ,s)
                            #f))))

(define-struct sec (cat label))

(define sections
  (list (make-sec 'getting-started
                  "Getting Started")
        (make-sec 'language
                  "Languages")
        (make-sec 'tool
                  "Tools")
        (make-sec 'library
                  "Libraries")
        (make-sec 'foreign
                  "Low-Level APIs")
        (make-sec 'legacy
                  "Legacy Languages and Libraries")
        (make-sec 'other
                  "Other")))

(define (main-collects? dir)
  (pair? (path->main-collects-relative dir)))

(define (to-toc target label)
  (make-toc-element
   #f
   null
   (list (link target
               #:underline? #f
               (make-element "tocsubseclink"
                             (list label))))))

(define (make-spacer)
  (make-toc-element
   #f
   null
   (list 'nbsp)))

(define (build-contents all?)
  (let* ([dirs (find-relevant-directories '(scribblings))]
         [infos (map get-info/full dirs)]
         [docs (apply append
                      (map (lambda (i dir)
                             (if (or all?
                                     (main-collects? dir))
                                 (let ([s (i 'scribblings)])
                                   (apply
                                    append
                                    (map (lambda (d cat)
                                           (if (and (not all?)
                                                    (pair? (cdr d))
                                                    (or (memq 'user-doc (cadr d))
                                                        (memq 'user-doc-root (cadr d))))
                                               null
                                               (let ([new-cat (if (or (symbol? cat)
                                                                      (and (list? cat)
                                                                           (= 2 (length cat))
                                                                           (symbol? (car cat))
                                                                           (real? (cadr cat))))
                                                                  cat
                                                                  'unknown)])
                                                 (list
                                                  (list 
                                                   ;; Category
                                                   (let ([the-cat (if (list? new-cat)
                                                                      (car new-cat)
                                                                      new-cat)])
                                                     (case the-cat
                                                       [(getting-started language tool library foreign legacy other omit)
                                                        the-cat]
                                                       [else 
                                                        (fprintf (current-error-port)
                                                                 "WARNING: bad base category: ~e from: ~e\n"
                                                                 cat
                                                                 dir)]))
                                                   ;; Priority
                                                   (if (list? new-cat)
                                                       (cadr new-cat)
                                                       0)
                                                   ;; Path
                                                   (if (pair? d)
                                                       (build-path dir (car d))
                                                       (build-path dir "???")))))))
                                         s
                                         (i 'doc-categories (lambda ()
                                                              (map (lambda (i) 'library) s))))))
                                 null))
                           infos
                           dirs))]
         [plain-line
          (lambda content
            (list (make-flow (list (make-paragraph content)))))]
         [line
          (lambda (doc)
            (plain-line (hspace 2)
                        (other-manual doc #:underline? #f)))])
    (make-splice
     (list
      (make-delayed-block
       (lambda (renderer part resolve-info)
         (make-table
          #f
          (cdr
           (apply append
                  (map (lambda (sec)
                         (let ([docs (filter (lambda (doc)
                                               (eq? (car doc) (sec-cat sec)))
                                             docs)])
                           (list*
                            (plain-line (hspace 1))
                            (plain-line (sec-label sec))
                            (map
                             cdr
                             (sort
                              (map (lambda (doc) (cons (cadr doc)
                                                       (line  (caddr doc))))
                                   docs)
                              (lambda (ad bd)
                                (let ([a (cadr (paragraph-content (car (flow-paragraphs (cadr ad)))))]
                                      [b (cadr (paragraph-content (car (flow-paragraphs (cadr bd)))))])
                                  (if (= (car ad) (car bd))
                                      (begin
                                        (string-ci<? (element->string a renderer part resolve-info)
                                                     (element->string b renderer part resolve-info)))
                                      (> (car ad) (car bd))))))))))
                       sections))))))

      (to-toc "master-index/index.html"
              "Master Index")
      (make-spacer)
      (to-toc (build-path (find-doc-dir) "license/index.html")
              "License")
      (to-toc (build-path (find-doc-dir) "acks/index.html")
              "Acknowledgments")
      (to-toc (build-path (find-doc-dir) "release/index.html")
              "Release Notes")
      (make-spacer)
      (to-toc (format "http://bugs.plt-scheme.org/?v=~a" (version))
              "Report a Bug")))))

