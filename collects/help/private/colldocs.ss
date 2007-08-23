(module colldocs mzscheme
  (require (lib "list.ss")
           (lib "getinfo.ss" "setup")
           (lib "contract.ss"))
  
  (define (colldocs)
    (let loop ([dirrecs
                (sort (find-relevant-directory-records '(doc.txt) 'all-available)
                      (lambda (a b)
                        (bytes<? (path->bytes (directory-record-path a))
                                 (path->bytes (directory-record-path b)))))]
               [docs null]
               [names null])
      (cond
        [(null? dirrecs) (values (reverse docs) (reverse names))]
        [else
         (let* ([dirrec (car dirrecs)]
                [dir (directory-record-path dirrec)]
                [info-proc (get-info/full dir)])
           (if info-proc
               (let ([doc.txt-path (info-proc 'doc.txt (lambda () #f))]
                     [name (info-proc 'name (lambda () #f))])
                 (if (and (path-string? doc.txt-path)
                          (string? name))
                     (loop (cdr dirrecs)
                           (cons (list dir (string->path doc.txt-path))
                                 docs)
                           (cons (pleasant-name name dirrec) names))
                     (loop (cdr dirrecs) docs names)))
               (loop (cdr dirrecs) docs names)))])))

  (define (pleasant-name name dirrec)
    (case (car (directory-record-spec dirrec))
      ((lib)
       (format "~a collection" name))
      ((planet)
       (format "~a package ~s"
               name
               `(,@(cdr (directory-record-spec dirrec))
                 ,(directory-record-maj dirrec)
                 ,(directory-record-min dirrec))))))
  
  (provide/contract 
   [colldocs (-> (values (listof (list/c path? path?))
                         (listof string?)))]))
