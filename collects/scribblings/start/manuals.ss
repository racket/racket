#lang scheme/base

(require scribble/manual
         scribble/struct
         setup/getinfo
         setup/main-collects)

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
        (make-sec 'other
                  "Other")))

(define (main-collects? dir)
  (pair? (path->main-collects-relative dir)))

(define (build-contents all?)
  (let* ([dirs (find-relevant-directories '(scribblings))]
         [infos (map get-info/full dirs)]
         [docs (apply append
                      (map (lambda (i dir)
                             (if (or all?
                                     (main-collects? dir))
                                 (let ([s (i 'scribblings)])
                                   (map (lambda (d cat)
                                          (let ([new-cat (if (or (symbol? cat)
                                                                 (and (list? cat)
                                                                      (= 2 (length cat))
                                                                      (symbol? (car cat))
                                                                      (real? (cadr cat))))
                                                             cat
                                                             'unknown)])
                                            (list 
                                             ;; Category
                                             (let ([the-cat (if (list? new-cat)
                                                                (car new-cat)
                                                                new-cat)])
                                               (case the-cat
                                                 [(getting-started language tool library foreign other omit)
                                                  the-cat]
                                                 [else 
                                                  (fprintf (current-error-port)
                                                           "WARNING: base category: ~e from: ~e"
                                                           cat
                                                           dir)]))
                                             ;; Priority
                                             (if (list? new-cat)
                                                 (cadr new-cat)
                                                 0)
                                             ;; Path
                                             (if (pair? d)
                                                 (build-path dir (car d))
                                                 (build-path dir "???")))))
                                        s
                                        (i 'doc-categories (lambda ()
                                                             (map (lambda (i) 'library) s)))))
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
    (make-delayed-flow-element
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
                     sections))))))))

