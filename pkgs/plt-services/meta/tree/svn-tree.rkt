#lang scheme/base

(provide get-svn-tree get-plt-svn-tree)

(require "tree.rkt" scheme/system scheme/match xml)

(define svn-exe (find-executable-path "svn"))
(define (svn . args)
  ;; runs an svn command, just returns its stdout to be used to its end
  (define-values [p pout pin perr]
    (apply subprocess #f #f (current-error-port) svn-exe args))
  (close-output-port pin)
  pout)

;; ----------------------------------------------------------------------------
;; Reading a tree from a subversion url

(define ((starts-with sym) x)
  (and (pair? x) (eq? sym (car x)) x))

(define (get-svn-tree url)
  (let* ([data (read-xml (svn "ls" "-R" "--xml" url))]
         [data (xml->xexpr (document-element data))]
         [data (ormap (starts-with 'list) (cddr data))]
         [data (filter (starts-with 'entry) (cddr data))]
         [data (map (lambda (x)
                      (match x
                        [(list _ `([kind ,kind]) _ ... `(name () ,name) _ ...)
                         (cons (string->symbol kind)
                               (regexp-split #rx#"/"
                                             (string->bytes/utf-8 name)))]))
                    data)])
    ;; utilities
    (define (list<? l1 l2)
      (cond [(null? l1) #t]
            [(null? l2) #f]
            [else (let ([x1 (car l1)] [x2 (car l2)])
                    (if (bytes=? x1 x2)
                      (list<? (cdr l1) (cdr l2))
                      (bytes<? x1 x2)))]))
    (define (subtract-prefix l1 l2)
      ;; if l1 has l2 as a prefix, return the following tail, otherwise #f
      (cond [(null? l2) l1]
            [(null? l1) #f]
            [else (and (bytes=? (car l1) (car l2))
                       (subtract-prefix (cdr l1) (cdr l2)))]))
    ;; the data is properly sorted, so just verify that it is while pulling it
    ;; out
    (define (pop!)
      (let ([r (car data)])
        (set! data (cdr data))
        (unless (or (null? data) (list<? (cdr r) (cdar data)))
          (error 'get-svn-tree "svn returned an unsorted list"))))
    (let loop ([name #""]
               [pathlist '()]
               [path (regexp-replace #rx"/?$" (string->bytes/utf-8 url) #"/")])
      (let dloop ([items '()])
        (let ([tail (and (pair? data) (subtract-prefix (cdar data) pathlist))])
          (if tail
            (let* ([kind (caar data)]
                   [pathlist (cdar data)]
                   [name (if (= 1 (length tail))
                           (car tail)
                           (error 'get-svn-tree
                                  "got an element without parent dir: ~e"
                                  (cdar data)))]
                   [name (if (eq? 'dir kind) (bytes-append name #"/") name)]
                   [path (bytes-append path name)])
              (pop!)
              (dloop (cons (case kind
                             [(dir) (loop name pathlist path)]
                             [(file) (make-tree name #f path)]
                             [else (error 'get-svn-tree
                                          "got an element with ~a: ~e"
                                          "an unexpected kind"
                                          kind)])
                           items)))
            (make-tree name (reverse items) path)))))))

;; ----------------------------------------------------------------------------
;; Reading the PLT tree

(define (get-plt-svn-tree)
  (get-svn-tree "http://svn.plt-scheme.org/plt/trunk/"))

#|
(tree-for-each (lambda (t) (printf "~a\n" (tree-path t)))
               (time (get-svn-tree "file:///home/svn/plt/trunk/")))
|#
