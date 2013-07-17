#lang racket/base
(require setup/getinfo
         scribble/core
         (except-in scribble/base url)
         scribble/html-properties
         net/url
         racket/path)

(provide release-items)

(struct notes (pos label path subs)
  #:transparent)

(define (check-release-note-files l dir)
  (if (and (list? l)
           (for/and ([v (in-list l)])
             (and ((length v) . >= . 2)
                  (string? (car v))
                  (path-string? (cadr v))
                  (or (null? (cddr v))
                      (exact-integer? (caddr v))
                      (or (null? (cdddr v))
                          (list? (cadddr v))
                          (for/and ([i (in-list (cadddr v))])
                            (and (list? i)
                                 (= (length i) 2)
                                 (string? (car i))
                                 (path-string? (cadr i)))))))))
      #t
      (begin
        (log-error "bad `release-note-files' entry in: ~a value: ~e"
                   (build-path dir "info.rkt")
                   l)
        #f)))

(define (build-release-list which)
  (sort
   (for*/list ([dir (in-list (find-relevant-directories '(release-note-files) which))]
               [rel (let ([i (get-info/full dir)])
                      (if i
                          (let ([v (i 'release-note-files (lambda () null))])
                            (if (check-release-note-files v dir)
                                v
                                null))
                          null))])
     (define label (car rel))
     (define file (cadr rel))
     (define pos (if (null? (cddr rel))
                     0
                     (caddr rel)))
     (define subs (if (or (null? (cddr rel))
                          (null? (cdddr rel)))
                      null
                      (list-ref rel 3)))
     (notes pos label (path->complete-path file dir)
            (for/list ([sub (in-list subs)])
              (cons (car sub) (path->complete-path (cadr sub) dir)))))
   (lambda (a b)
     (cond
      [((notes-pos a) . < . (notes-pos b)) #t]
      [((notes-pos a) . > . (notes-pos b)) #f]
      [else (string<? (notes-label a) (notes-label b))]))))

(define (item-link lbl p)
  (elem #:style (style #f (list (link-resource p)))
        lbl))

(define (release-items which)
  (apply itemlist #:style "compact"
         (for/list ([n (in-list (build-release-list which))])
           (item (item-link (notes-label n) (notes-path n))
                 (if (null? (notes-subs n))
                     null
                     (apply itemlist #:style "compact"
                            (for/list ([s (in-list (notes-subs n))])
                              (item (item-link (car s) (cdr s))))))))))

