#lang scheme/base

(provide find-version)

(define (find-version base-path vers)
  (let-values ([(dir name _) (split-path (bytes->path base-path))])
    (let ([files (with-handlers ([exn:fail:filesystem? (lambda (exn) null)])
                   (directory-list dir))])
      (and files
           (let* ([p (path-element->bytes name)]
                  [len (bytes-length p)]
                  [candidate-versions
                   (filter
                    values
                    (map
                     (lambda (file)
                       (let ([s (path-element->bytes file)])
                         (and 
                          (and (len . < . (bytes-length s))
                               (bytes=? p (subbytes s 0 len))) 
                          (let ([ext (let ([m (regexp-match #rx#"([.][a-z]+)?[.](rkt|ss|sls)$" 
                                                            (subbytes s len))])
                                       (and m 
                                            (or (not (cadr m))
                                                (bytes=? (cadr m) #".mzscheme"))
                                            (car m)))])
                            (and ext
                                 (or (and (= (bytes-length s) (+ len (bytes-length ext)))
                                          (cons null ext))
                                     (let ([vers (subbytes s len (- (bytes-length s) (bytes-length ext)))])
                                       (and (regexp-match #rx#"^(-[0-9]+)+$" vers)
                                            (cons 
                                             (map string->number
                                                  (cdr
                                                   (map bytes->string/latin-1
                                                        (regexp-split #rx#"-" vers))))
                                             ext)))))))))
                     files))]
                  [versions
                   (let* ([eo '(#".mzscheme.ss" #".mzscheme.sls" #".ss" #".sls" #".rkt")]
                          [ext< (lambda (a b)
                                  (> (length (member a eo)) (length (member b eo))))])
                     (sort candidate-versions
                           (lambda (a b)
                             (if (equal? (car a) (car b))
                               (ext< (cdr a) (cdr b))
                               (let loop ([a (car a)] [b (car b)])
                                 (cond
                                   [(null? a) #t]
                                   [(null? b) #f]
                                   [(> (car a) (car b)) #t]
                                   [(< (car a) (car b)) #f]
                                   [else (loop (cdr a) (cdr b))]))))))])
             (ormap (lambda (candidate-version)
                      (and (version-match? (car candidate-version) vers)
                           (cons (car candidate-version)
                                 (bytes->string/latin-1 (cdr candidate-version)))))
                    versions))))))

(define (version-match? cand vers)
  (cond
   [(null? vers) #t]
   [(null? cand) #f]
   [(eq? (car vers) 'and)
    (andmap (lambda (v)
              (version-match? cand v))
            (cdr vers))]
   [(eq? (car vers) 'or)
    (ormap (lambda (v)
             (version-match? cand v))
           (cdr vers))]
   [(eq? (car vers) 'not)
    (not (version-match? cand (cadr vers)))]
   [(sub-version-match? (car cand) (car vers))
    (version-match? (cdr cand) (cdr vers))]
   [else #f]))

(define (sub-version-match? cand subvers)
  (cond
   [(number? subvers) (= cand subvers)]
   [else (case (car subvers)
           [(>=) (>= cand (cadr subvers))]
           [(<=) (<= cand (cadr subvers))]
           [(and) (andmap (lambda (sv)
                            (sub-version-match? cand sv))
                          (cdr subvers))]
           [(or) (ormap (lambda (sv)
                          (sub-version-match? cand sv))
                        (cdr subvers))]
           [(not) (not (sub-version-match? cand (cadr subvers)))]
           [else (error "bad subversion")])]))

