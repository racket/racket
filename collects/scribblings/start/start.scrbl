#lang scribble/doc
@require[scribble/manual
         scribble/struct
         setup/getinfo
         setup/main-collects]

@title{PLT Scheme Documentation}

@begin[
(define initial-ones
  '("(collects . scribblings/quick/quick.scrbl):top"
    blank
    "(collects . scribblings/guide/guide.scrbl):top"
    "(collects . scribblings/reference/reference.scrbl):top"
    blank))

(let* ([dirs (find-relevant-directories '(scribblings))]
       [infos (map get-info/full dirs)]
       [docs (apply append
                    (map (lambda (i dir)
                           (let ([s (i 'scribblings)])
                             (map (lambda (d)
                                    (if (pair? d)
                                        (format "~a:top"
                                                (path->main-collects-relative
                                                 (build-path dir (car d))))
                                        (format "bad: ~s" d)))
                                  s)))
                         infos
                         dirs))])
  (make-table
   #f
   (map (lambda (doc)
          (list (make-flow (list (make-paragraph (list 
                                                  (if (eq? doc 'blank)
                                                      (hspace 1)
                                                      (secref doc))))))))
        (append initial-ones
                (remove* initial-ones 
                         (remove "(collects . scribblings/start/start.scrbl):top"
                                 docs))))))
]
