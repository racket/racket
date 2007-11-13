#lang scribble/doc
@require[scribble/manual
         scribble/struct
         setup/getinfo]

@title{PLT Scheme Documentation}

@begin[
(define (resolve s)
  (resolved-module-path-name
   (module-path-index-resolve
    (module-path-index-join `(lib ,(string-append s ".scrbl")
                                  "scribblings"
                                  ,s)
                            #f))))

(define initial-ones
  (list (resolve "quick")
        'blank
        (resolve "guide")
        (resolve "reference")
        'blank
        (resolve "gui")
        'blank))

(let* ([dirs (find-relevant-directories '(scribblings))]
       [infos (map get-info/full dirs)]
       [docs (apply append
                    (map (lambda (i dir)
                           (let ([s (i 'scribblings)])
                             (map (lambda (d)
                                    (if (pair? d)
                                        (build-path dir (car d))
                                        (build-path dir "???")))
                                  s)))
                         infos
                         dirs))]
       [line
        (lambda (doc)
          (list (make-flow (list (make-paragraph (list 
                                                  (if (eq? doc 'blank)
                                                      (hspace 1)
                                                      (secref #:doc doc "top"))))))))])

  (make-delayed-flow-element
   (lambda (renderer part resolve-info)
     (make-table
      #f
      (append (map line initial-ones)
              (sort
               (map line
                    (remove* initial-ones 
                          (remove (resolve "start") 
                                  docs)))
               (lambda (a b)
                 (let ([a (car (paragraph-content (car (flow-paragraphs (car a)))))]
                       [b (car (paragraph-content (car (flow-paragraphs (car b)))))])
                   (string-ci<? (element->string a renderer part resolve-info)
                                (element->string b renderer part resolve-info))))))))))
]
