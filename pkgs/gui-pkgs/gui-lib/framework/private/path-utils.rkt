#lang scheme/unit
  (require "sig.rkt")
  
  (import)
  (export framework:path-utils^)
  
  (define (generate-autosave-name name)
    (let-values ([(base name dir?)
                  (if name
                      (split-path name)
                      (values (find-system-path 'doc-dir)
                              (bytes->path-element #"mredauto")
                              #f))])
      (let* ([base (if (path? base)
                       base
                       (current-directory))]
             [path (if (relative-path? base)
                       (build-path (current-directory) base)
                       base)])
        (let loop ([n 1])
          (let* ([numb (string->bytes/utf-8 (number->string n))]
                 [new-name
                  (build-path path
                              (if (eq? (system-type) 'windows)
                                  (bytes->path-element
                                   (bytes-append (regexp-replace #rx#"\\..*$" 
                                                                 (path-element->bytes name)
                                                                 #"")
                                                 #"."
                                                 numb))
                                  (bytes->path-element
                                   (bytes-append #"#"
                                                 (path-element->bytes name)
                                                 #"#"
                                                 numb
                                                 #"#"))))])
            (if (file-exists? new-name)
                (loop (add1 n))
                new-name))))))
  
  (define (generate-backup-name full-name)
    (let-values ([(pre-base name dir?) (split-path full-name)])
      (let ([base (if (path? pre-base)
                      pre-base
                      (current-directory))])
        (let ([name-bytes (path-element->bytes name)])
          (cond
            [(and (eq? (system-type) 'windows)
                  (regexp-match #rx#"(.*)\\.[^.]*" name-bytes))
             =>
             (λ (m)
               (build-path base (bytes->path-element (bytes-append (cadr m) #".bak"))))]
            [(eq? (system-type) 'windows)
             (build-path base (bytes->path-element (bytes-append name-bytes #".bak")))]
            [else
             (build-path base (bytes->path-element (bytes-append name-bytes #"~")))])))))

