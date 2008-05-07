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
        (make-sec 'gui-library
                  "GUI and Graphics Libraries")
        (make-sec 'net-library
                  "Network Libraries")
        (make-sec 'parsing-library
                  "Parsing Libraries")
        (make-sec 'tool-library
                  "Tool Libraries")
        (make-sec 'foreign
                  "Low-Level APIs")
        (make-sec 'interop
                  "Interoperability")
        (make-sec 'library
                  "Miscellaneous Libraries")
        (make-sec 'experimental
                  "Experimental Languages and Libraries")
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

(define (add-sections cat mk-sep l)
  (if (null? l)
      null
      (let loop ([l l][key (if (equal? "" (caddar l))
                               (caar l)
                               +inf.0)])
        (cond
         [(null? l) null]
         [(equal? (caar l) key)
          (cons (cadar l) (loop (cdr l) key))]
         [else
          (let ([lbl (caddar l)] ; currently always ""
                [l (cons (cadar l) (loop (cdr l) (caar l)))]
                [sep? (not (= (truncate (/ key 10))
                              (truncate (/ (caar l) 10))))])
            (if sep?
                (cons (mk-sep lbl) l)
                l))]))))

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
                                    (map (lambda (d)
                                           (if (and (not all?)
                                                    (pair? (cdr d))
                                                    (or (memq 'user-doc (cadr d))
                                                        (memq 'user-doc-root (cadr d))))
                                               null
                                               (let* ([new-cat (if ((length d) . > . 2)
                                                                   (caddr d)
                                                                   '(library))]
                                                      [sub-cat (and (list? new-cat)
                                                                    ((length new-cat) . > . 1)
                                                                    (cadr new-cat))])
                                                 (list
                                                  (list 
                                                   ;; Category
                                                   (let ([the-cat (if (pair? new-cat)
                                                                      (car new-cat)
                                                                      'unknown)])
                                                     (or (and (eq? the-cat 'omit) the-cat)
                                                         (ormap (lambda (sec)
                                                                  (and (eq? the-cat (sec-cat sec))
                                                                       the-cat))
                                                                sections)
                                                         'library))
                                                   ;; Priority
                                                   (if (and sub-cat
                                                            (real? sub-cat))
                                                       sub-cat
                                                       0)
                                                   ;; Priority label (not used):
                                                   ""
                                                   ;; Path
                                                   (if (pair? d)
                                                       (build-path dir (car d))
                                                       (build-path dir "???")))))))
                                         s)))
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
                            (add-sections
                             (sec-cat sec)
                             (lambda (str)
                               (plain-line (make-element (if (string=? str "")
                                                             "sepspace" 
                                                             "septitle")
                                                         (list 'nbsp str))))
                             (sort
                              (map (lambda (doc) (list (cadr doc)
                                                       (line (cadddr doc))
                                                       (caddr doc)))
                                   docs)
                              (lambda (ad bd)
                                (let ([a (cadr (paragraph-content (car (flow-paragraphs (caadr ad)))))]
                                      [b (cadr (paragraph-content (car (flow-paragraphs (caadr bd)))))])
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

