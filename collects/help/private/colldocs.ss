(module colldocs mzscheme
  (require (lib "list.ss")
           (lib "getinfo.ss" "setup")
           (lib "contract.ss"))
  
  (provide/contract 
   [colldocs (-> (values (listof (list/c path? path?))
                         (listof string?)))])

  (define (colldocs)
    (let loop ([dirs (find-relevant-directories '(doc.txt) 'all-available)]
               [docs null]
               [names null])
      (cond
        [(null? dirs)
         (values docs names)]
        [else (let* ([dir (car dirs)]
                     [info-proc (get-info/full dir)])
                (if info-proc
                    (let ([doc.txt-path (info-proc 'doc.txt (lambda () #f))]
                          [name (info-proc 'name (lambda () #f))])
                      (if (and (path-string? doc.txt-path)
                               (string? name))
                          (loop (cdr dirs)
                                (cons (list dir
                                            (string->path doc.txt-path))
                                      docs)
                                (cons name names))
                          (loop (cdr dirs) docs names)))
                    (loop (cdr dirs) docs names)))]))))