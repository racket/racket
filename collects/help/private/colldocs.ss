(module colldocs mzscheme
  (require (lib "list.ss")
           (lib "getinfo.ss" "setup")
           (lib "contract.ss"))

  ;; find-doc-directory-records : -> (list-of directory-record)
  ;; Returns directory records containing doc.txt files, sorted first
  ;; by lib/planet, then by path.
  (define (find-doc-directory-records)
    (define allrecs
      (find-relevant-directory-records '(doc.txt) 'all-available))
    (define (rec<? a b)
      (bytes<? (path->bytes (directory-record-path a))
               (path->bytes (directory-record-path b))))
    (define (librec? dirrec)
      (let ([spec (directory-record-spec dirrec)])
        (and (pair? spec) (eq? (car spec) 'lib))))
    (append (sort (filter librec? allrecs) rec<?)
            (sort (filter (lambda (x) (not (librec? x))) allrecs) rec<?)))

  ;; colldocs : -> (values (list-of (list string path)) (list-of string))
  ;; Returns two lists having equal length. Each item in the first list
  ;; contains a list containing a string (the directory) and a path (to
  ;; the doc.txt file). The second list contains the corresponding descriptive
  ;; names.
  (define (colldocs)
    (let loop ([dirrecs (find-doc-directory-records)]
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
                           (cons (pleasant-name name dirrec)
                                 names))
                     (loop (cdr dirrecs) docs names)))
               (loop (cdr dirrecs) docs names)))])))

  ;; pleasant-name : string directory-record -> string
  ;; Generates a descriptive name for the collection/package.
  (define (pleasant-name name dirrec)
    (let ([spec (directory-record-spec dirrec)])
      (if (and (pair? spec) (list? spec))
          (case (car spec)
            ((lib) (format "~a collection" name))
            ((planet) (format "~a package ~s"
                              name
                              `(,@(cdr spec)
                                ,(directory-record-maj dirrec)
                                ,(directory-record-min dirrec)))))
          name)))
  
  (provide/contract 
   [colldocs (-> (values (listof (list/c path? path?))
                         (listof string?)))]))
