#lang racket/base
(require racket/path
         setup/getinfo
         "desc.rkt"
         "../name.rkt"
         "dirs.rkt")

(provide find-adjacent-dependency
         close-over-adjacent)

(define (find-adjacent-dependency dep desc destdir link-dirs?)
  (define-values (name type)
    (package-source->name+type (pkg-desc-source desc) (pkg-desc-type desc)
                               #:link-dirs? link-dirs?))
  (define (adjacent source)
    (struct-copy pkg-desc desc
                 [source source]
                 [name dep]
                 [checksum #f]
                 [auto? #t]
                 [extra-path #f]))
  (or (case type
        [(dir link static-link)
         (define-values (base name dir?) (split-path (pkg-desc-source desc)))
         (define path (if (path? base)
                          (build-path base dep)
                          (string->path dep)))
         (and (directory-exists? path)
              (adjacent (path->string path)))]
        [(attach)
         (define dir (build-path (or destdir (pkg-installed-dir)) dep))
         (and (directory-exists? dir)
              (adjacent dep))]
        [(file)
         (define-values (base name dir?) (split-path (pkg-desc-source desc)))
         (define ext (path-get-extension name))
         (define new-name (if ext (path-add-extension dep ext #".") dep))
         (define new-path (if (path? base)
                              (build-path base new-name)
                              (string->path new-name)))
         (and (file-exists? new-path)
              (adjacent (path->string new-path)))]
        [else #f])
      dep))

(define (close-over-adjacent names source package-name-and-dir-for-create)  
  (define info-ns (make-base-namespace))
  (define (find-adjacents name)
    (define-values (pkg-name dir) (package-name-and-dir-for-create #f name source))
    (cond
      [(directory-exists? dir)
       (define get-info (get-info/full dir #:namespace info-ns))
       (define deps
         (if get-info
             (append (get-info 'deps (lambda () null))
                     (get-info 'build-deps (lambda () null)))
             null))
       (define-values (base dir-name dir?) (split-path dir))
       (for/list ([dep (in-list deps)]
                  #:do [(define name (if (pair? dep) (car dep) dep))
                        (define dir (build-path base name))]
                  #:when (directory-exists? dir))
         (if (eq? source 'name)
             name
             dir))]
      [else null]))
  (let loop ([names names] [seen #hash()])
    (cond
      [(null? names) null]
      [else
       (define name (car names))
       (cond
         [(hash-ref seen name #f)
          (loop (cdr names) seen)]
         [else
          (cons name
                (loop (append (find-adjacents name) names)
                      (hash-set seen name #t)))])])))
