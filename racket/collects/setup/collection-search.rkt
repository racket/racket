#lang racket/base
(require racket/list
         racket/string)

(provide collection-search
         normalized-lib-module-path?)
           
(define (collection-search mp
                           #:combine [combine (lambda (r v) v)]
                           #:init [result #f]
                           #:break? [break? (lambda (r) #f)]
                           #:all-possible-roots? [all-possible-roots? #f])
  (unless (normalized-lib-module-path? mp)
    (error 'collection-search "normalized-lib-module-path?" mp))
  (define els (string-split (cadr mp) "/"))
  (define coll-str (car els))
  (define coll-sym (string->symbol coll-str))
  (define subpath (apply build-path els))
  (define subsubpath (apply build-path (cdr els)))
  (define (build-path* b p) (simplify-path (build-path b p)))
  (define (check-root p result all-possible-roots?)
    (define coll-dir (build-path* p coll-str))
    (if (or all-possible-roots?
            (directory-exists? coll-dir))
        (combine result (build-path coll-dir subsubpath))
        result))
  (for/fold ([result result]) ([p (in-list (current-library-collection-links))]
                               #:break (break? result))
    (cond
     [(not p)
      (for/fold ([result result]) ([r (in-list (current-library-collection-paths))]
                                   #:break (break? result))
        (check-root r result all-possible-roots?))]
     [(path? p)
      (define content
        (with-handlers ([exn:fail? (lambda (exn)
                                     (log-error "error attempting to read links file: ~a"
                                                (exn-message exn))
                                     null)])
          (if (file-exists? p)
              (call-with-default-reading-parameterization
               (lambda ()
                 (call-with-input-file* p read)))
              null)))
      (define-values (links-dir base dir?) (split-path p))
      (for/fold ([result result]) ([e (in-list content)]
                                   #:break (break? result)
                                   #:when
                                   (and (list? e)
                                        (or (= (length e) 2)
                                            (and (= (length e) 3)
                                                 (regexp? (caddr e))))
                                        (path-string? (cadr e))
                                        (or (null? (cddr e))
                                            (regexp-match? (caddr e) (version)))))
        (let ([a (car e)]
              [p (path->complete-path (cadr e) links-dir)])
          (cond
           [(or (eq? 'root a)
                (eq? 'static-root a))
            (check-root p result (and (eq? 'root a)
                                      all-possible-roots?))]
           [(equal? coll-str a)
            (combine result (build-path* p subsubpath))]
           [else result])))]
     [(hash? p)
      (or (for/fold ([result result]) ([r (in-list (hash-ref p coll-sym null))]
                                       #:break (break? result))
            (combine result (build-path* r subsubpath)))
          (for/fold ([result result]) ([r (in-list (hash-ref p #f null))]
                                       #:break (break? result))
            (check-root r result all-possible-roots?)))]
     [else
      (error 'collection-search
             "internal error: unexpected collection-link list element")])))
    
(define (normalized-lib-module-path? mp)
  (and (module-path? mp)
       (pair? mp)
       (eq? 'lib (car mp))
       (= 2 (length mp))
       (regexp-match? #rx"/" (cadr mp))))

