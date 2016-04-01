#lang racket/base

#|

This file is used by the contract system's
implementation, so it does not have contracts.
Use syntax/modcollapse instead.

|#

(require racket/string
         racket/list
         "modhelp.rkt")

(define (collapse-module-path s relto-mp)
  ;; relto-mp should be a path, '(lib relative-path collection) or symbol,
  ;;   '(planet ...), '(file path), '(submod <relto-mp> symbol ...),
  ;;   or a thunk that produces one of those

  (define relto-submod '())
  
  ;; Used for 'lib paths, so it's always Unix-style
  (define (attach-to-relative-path-string elements relto)
    (let ([elem-str
           (substring
            (apply string-append
                   (map (lambda (i)
                          (string-append
                           "/"
                           (cond [(bytes? i) (bytes->string/locale i)]
                                 [(path? i) (path->string i)]
                                 [(eq? i 'up) ".."]
                                 [else i])))
                        (filter (lambda (x) (not (eq? x 'same)))
                                elements)))
            1)])
      (if (or (regexp-match #rx"^[.]/+[^/]*" relto)
              (not (regexp-match #rx"/" relto)))
          elem-str
          (let ([m (regexp-match #rx"^(.*/)/*[^/]*$" relto)])
            (string-append (cadr m) elem-str)))))
  
  (define (simpler-relpath path)
    (let loop ([s (regexp-replace* #px"(?<![.])[.]/" path "")])
      (let ([s2 (regexp-replace #rx"([^/.]*)/[.][.]/" s "")])
        (if (equal? s s2)
            s
            (loop s2)))))
  
  (define (add-main s)
    (if (regexp-match #rx"[.][^/]*$" s)
        s
        (string-append s "/main.rkt")))

  (define (ss->rkt s)
    (let ([len (string-length s)])
      (if (and (len . >= . 3)
               (string=? ".ss" (substring s (- len 3))))
          (string-append (substring s 0 (- len 3)) ".rkt")
          s)))
  
  (define (path-ss->rkt p)
    (let-values ([(base name dir?) (split-path p)])
      (if (regexp-match #rx"[.]ss$" (path->bytes name))
          (path-replace-suffix p #".rkt")
          p)))

  (define (flatten-relto-mp!)
    (when (procedure? relto-mp) (set! relto-mp (relto-mp)))
    (when (and (pair? relto-mp) (eq? 'submod (car relto-mp)))
      (set! relto-submod (cddr relto-mp))
      (set! relto-mp (cadr relto-mp)))
    (when (symbol? relto-mp) (set! relto-mp `(lib ,(symbol->string relto-mp)))))
  
  (define (combine-relative-elements elements)
    
    (define (extract-base relto)
      (let-values ([(base n d?) (split-path relto)])
        (if (eq? base 'relative) 
            'same 
            (if (not base)
                relto ; strange case: relto is a root directory
                base))))
    
    ;; Used for 'file paths, so it's platform specific:
    (define (attach-to-relative-path relto)
      (apply build-path
             (extract-base relto)
             (map (lambda (i) (if (bytes? i) (bytes->path i) i))
                  elements)))
    
    (flatten-relto-mp!)
    (cond
      [(or (path? relto-mp) (and (string? relto-mp) (ormap path? elements)))
       (path-ss->rkt
        (apply build-path
               (extract-base relto-mp)
               (map (lambda (x) (if (bytes? x) (bytes->path x) x))
                    elements)))]
      [(string? relto-mp)
       (ss->rkt
        (bytes->string/locale
         (apply
          bytes-append
          (cond [(regexp-match #rx#"^(.*)/[^/]*$"
                               (string->bytes/locale relto-mp))
                 => cadr]
                [else #"."])
          (map (lambda (e)
                 (cond [(eq? e 'same) #"/."]
                       [(eq? e 'up) #"/.."]
                       [else (bytes-append
                              #"/" (if (path? e) (path->bytes e) e))]))
               elements))))]
      [(eq? (car relto-mp) 'file)
       (let ([path ((if (ormap path? elements) values path->string)
                    (path-ss->rkt (attach-to-relative-path (cadr relto-mp))))])
         (if (path? path) path `(file ,path)))]
      [(eq? (car relto-mp) 'lib)
       (let ([relto-mp (if (null? (cddr relto-mp))
                           ;; old style => add 'mzlib
                           ;; new style => add main.rkt or split
                           (let ([m (regexp-match-positions #rx"[/]" (cadr relto-mp))])
                             (if m
                                 ;; new style: split
                                 `(lib ,(substring (cadr relto-mp) (cdar m))
                                       ,(substring (cadr relto-mp) 0 (caar m)))
                                 (if (regexp-match? #rx"[.]" (cadr relto-mp))
                                     ;; old style:
                                     `(lib ,(cadr relto-mp) "mzlib")
                                     ;; new style, add "main.rkt":
                                     `(lib "main.rkt" ,(cadr relto-mp)))))
                           ;; already has at least two parts:
                           relto-mp)])
         (let ([path (attach-to-relative-path-string
                      elements (apply string-append
                                      (append
                                       (map (lambda (s)
                                              (string-append s "/"))
                                            (cddr relto-mp))
                                       (list (cadr relto-mp)))))])
           (let ([simpler (simpler-relpath path)])
             (let ([m (regexp-match #rx"^(.*)/([^/]*)$" simpler)])
               (if m
                   (normalize-lib `(lib ,(caddr m) ,(cadr m)))
                   (error 'combine-relative-elements
                          "relative path escapes collection: ~s relative to ~s"
                          elements relto-mp))))))]
      [(eq? (car relto-mp) 'planet)
       (let ([relto-mp
              ;; make sure relto-mp is in long form:
              (if (null? (cddr relto-mp))
                  (normalize-planet relto-mp)
                  relto-mp)])
         (let ([pathstr (simpler-relpath
                         (attach-to-relative-path-string
                          elements 
                          (apply string-append
                                 (append
                                  (map (lambda (s)
                                         (string-append s "/"))
                                       (cdddr relto-mp))
                                  (list (cadr relto-mp))))))])
           (normalize-planet `(planet ,pathstr ,(caddr relto-mp)))))]
      [(eq? (car relto-mp) 'quote)
       (set! relto-mp (build-path (current-directory) "x"))
       (combine-relative-elements elements)]
      [else (error 'combine-relative-elements
                   "don't know how to deal with: ~s for ~s" relto-mp elements)]))
  
  (define (normalize-lib s)
    (if (null? (cddr s))
        ;; single-string version:
        (let ([e (cadr s)])
          (cond
            [(regexp-match? #rx"[.]" e)
             ;; It has a suffix:
             (if (regexp-match? #rx"/" e)
                 ;; It has a path, so it's fine:
                 (let ([e2 (datum-intern-literal (ss->rkt e))])
                   (if (eq? e e2)
                       s
                       `(lib ,e2)))
                 ;; No path, so add "mzlib/":
                 `(lib ,(datum-intern-literal 
                         (string-append "mzlib/" (ss->rkt e)))))]
            [(regexp-match? #rx"/" e)
             ;; It has a separator, so add a suffix:
             `(lib ,(datum-intern-literal (string-append e ".rkt")))]
            [else
             ;; No separator or suffix, so add "/main.rkt":
             `(lib ,(datum-intern-literal (string-append e "/main.rkt")))]))
        ;; multi-string version:
        (if (regexp-match? #rx"[.]" (cadr s))
            ;; there's a suffix, so we can collapse to a single string:
            `(lib ,(datum-intern-literal
                    (string-join (append (cddr s) 
                                         (list (ss->rkt (cadr s))))
                                 "/")))
            ;; No suffix, so we must keep the old style:
            (cons 'lib (map datum-intern-literal (cdr s))))))
  
  (define (normalize-planet s)
    (cond
      [(symbol? (cadr s))
       ;; normalize via string form:
       (normalize-planet `(planet ,(symbol->string (cadr s))))]
      [(null? (cddr s))
       ;; normalize to long form:
       (let* ([strs (regexp-split #rx"/" (cadr s))])
         (let ([owner (car strs)]
               [pkg+vers (regexp-split #rx":" (cadr strs))]
               [path (cddr strs)])
           `(planet ,(if (null? path)
                         "main.rkt"
                         (let ([str (last path)])
                           (if (regexp-match? #rx"[.]" str)
                               (ss->rkt str)
                               (string-append str ".rkt"))))
                    (,owner
                     ,(string-append (car pkg+vers) ".plt")
                     ,@(if (null? (cdr pkg+vers))
                           null
                           `(,(string->number (cadr pkg+vers))
                             . ,(if (null? (cddr pkg+vers))
                                    null
                                    (list
                                     (let ([vers (caddr pkg+vers)])
                                       (cond
                                         [(regexp-match? #rx"<=" vers)
                                          `(- ,(string->number (substring vers 2)))]
                                         [(regexp-match? #rx">=" vers)
                                          (string->number (substring vers 2))]
                                         [(regexp-match? #rx"=" vers)
                                          `(= ,(string->number (substring vers 1)))]
                                         [(regexp-match #rx"(.*)-(.*)" vers)
                                          => (lambda (m)
                                               `(,(string->number (cadr m))
                                                 ,(string->number (caddr m))))]
                                         [(string->number vers)
                                          => (lambda (n) n)]
                                         [else (error 'collapse-module-path
                                                      "confused when normalizing planet path: ~e"
                                                      s)])))))))
                    ,@(if (null? path)
                          null
                          (reverse (cdr (reverse path)))))))]
      [else 
       ;; Long form is the normal form, but see if we need to split up the
       ;; path elements:
       (let ([base (cadr s)]
             [rest (cdddr s)]
             [split? (lambda (s)
                       (regexp-match? #rx"/" s))])
         (if (or (split? base)
                 (ormap split? rest))
             ;; need to split some paths:
             (let ([split (lambda (s)
                            (regexp-split #rx"/" s))])
               (let ([bases (split base)]
                     [rests (map split rest)])
                 (list* (car s)
                        (ss->rkt (last bases))
                        (caddr s)
                        (append
                         (apply append rests)
                         (drop-right bases 1)))))
             ;; already in normal form:
             (let* ([e (cadr s)]
                    [e2 (ss->rkt e)])
               (if (eq? e e2)
                   s
                   (list* (car s) e2 (cddr s))))))]))

  (define (normalize-submod sm)
    ;; Get rid of all ".."s:
    (if (member ".." sm)
        (let ([subpath (let loop ([accum null] [l (cddr sm)])
                         (cond
                          [(null? l) (reverse accum)]
                          [(equal? (car l) "..")
                           (if (null? accum)
                               (error 'collapse-module-path "too many \"..\"s in path: ~e"
                                      sm)
                               (loop (cdr accum) (cdr l)))]
                          [else (loop (cons (car l) accum) (cdr l))]))])
          (if (null? subpath)
              (cadr sm)
              `(submod ,(cadr sm) ,@subpath)))
        (if (null? (cddr sm))
            (cadr sm)
            sm)))

  (let normalize-recur ([s s])
    (cond [(string? s)
           ;; Parse Unix-style relative path string
           (combine-relative-elements (explode-relpath-string s))]
          [(symbol? s)
           ;; Convert to `lib' form:
           (normalize-lib `(lib ,(symbol->string s)))]
          [(and (or (not (pair? s)) (not (list? s))) (not (path? s)))
           #f]
          [(or (path? s) (eq? (car s) 'file))
           (let ([p (if (path? s) s (cadr s))])
             (if (absolute-path? p)
                 (let ([p2 (if (path? p)
                               (path-ss->rkt p)
                               (ss->rkt p))])
                   (cond
                    [(eq? p p2) s]
                    [(path? s) p2]
                    [else `(file ,p2)]))
                 (let loop ([p p] [elements null])
                   (let-values ([(base name dir?) (split-path p)])
                     (cond [(eq? base 'relative)
                            (combine-relative-elements (cons name elements))]
                           [else (loop base (cons name elements))])))))]
          [(eq? (car s) 'lib) (normalize-lib s)]
          [(eq? (car s) 'planet) (normalize-planet s)]
          [(eq? (car s) 'quote) s]
          [(eq? (car s) 'submod) 
           (cond
            [(equal? (cadr s) ".")
             (flatten-relto-mp!)
             (normalize-submod `(submod ,relto-mp ,@relto-submod ,@(cddr s)))]
            [(equal? (cadr s) "..")
             (flatten-relto-mp!)
             (normalize-submod `(submod ,relto-mp ,@relto-submod ,@(cdr s)))]
            [else
             (normalize-submod `(submod ,(normalize-recur (cadr s)) ,@(cddr s)))])]
          [else #f])))

(define collapse-module-path-index
  (case-lambda
    [(mpi)
     (collapse-module-path-index/relative mpi)]
    [(mpi relto-mp)
     (define (force-relto relto-mp)
       (if (procedure? relto-mp) 
           (relto-mp)
           relto-mp))
     (let-values ([(path base) (module-path-index-split mpi)])
       (if path
           (collapse-module-path
            path
            (lambda ()
              (cond
               [(module-path-index? base)
                (collapse-module-path-index base relto-mp)]
               [(resolved-module-path? base)
                (let ([n (resolved-module-path-name base)])
                  (if (pair? n)
                      (if (path? (car n))
                          (cons 'submod n)
                          (list* 'submod `(quote ,(car n)) (cdr n)))
                      (if (path? n)
                          n
                          `(quote ,n))))]
               [else (force-relto relto-mp)])))
           (let ([r (force-relto relto-mp)]
                 [sm (module-path-index-submodule mpi)])
             (if sm
                 (if (and (pair? r)
                          (eq? (car r) 'submod))
                     (append r sm)
                     (list* 'submod r sm))
                 r))))]))

(define (collapse-module-path-index/relative mpi)
  (define relative?
    (let loop ([mpi mpi])
      (define-values (path base) (module-path-index-split mpi))
      (let path-loop ([path path])
        (cond
         [(not path)
          (not base)]
         [(symbol? path)
          #f]
         [(and (pair? path)
               (or (eq? (car path) 'lib)
                   (eq? (car path) 'planet)
                   (eq? (car path) 'quote)))
          #f]
         [(and (pair? path)
               (eq? (car path) 'submod)
               (not (or (equal? (cadr path) ".")
                        (equal? (cadr path) ".."))))
          (path-loop (cadr path))]
         [(and (pair? path)
               (eq? (car path) 'file)
               (complete-path? (cadr path)))
          #f]
         [(and (path? path)
               (complete-path? path))
          #f]
         [else
          (or (not base)
              (and (module-path-index? base)
                   (loop base)))]))))
  
  (if relative?
      (let loop ([mpi mpi])
        (define-values (s base) (if mpi
                                    (module-path-index-split mpi)
                                    (values #f #f)))
        (cond
         [(not s) #f]
         [else
          (define full-prev (loop base))
          (cond
           [(not full-prev)
            s]
           [else
            (define prev (if (and (pair? full-prev)
                                  (eq? 'submod (car full-prev)))
                             (cadr full-prev)
                             full-prev))
            (let s-loop ([s s])
              (cond
               [(string? s)
                ;; Unix-style relative path string
                (cond
                 [(string? prev)
                  (define l (drop-right (explode-relpath-string prev) 1))
                  (if (null? l)
                      s
                      (string-join (append
                                    (for/list ([e (in-list l)])
                                      (case e
                                        [(same) "."]
                                        [(up) ".."]
                                        [else (path-element->string e)]))
                                    (list s))
                                   "/"))]
                 [(path? prev)
                  (define-values (base name dir?) (split-path prev))
                  (apply build-path (if (path? base) base 'same) (explode-relpath-string s))]
                 [else ; `(file ,...)
                  (define-values (base name dir?) (split-path (cadr prev)))
                  (apply build-path (if (path? base) base 'same) (explode-relpath-string s))])]
               [(and (pair? s) (eq? 'file (car s)))
                (build-path
                 (let-values ([(base name dir?)
                               (split-path
                                (cond
                                 [(string? prev) prev]
                                 [(path? prev) prev]
                                 [else ; `(file ,...)
                                  (cadr prev)]))])
                   (if (path? base) base 'same))
                 (cadr s))]
               [(eq? (car s) 'submod) 
                (define (as-submod p sm)
                  (if (and (pair? p) (eq? 'submod (car p)))
                      (append p sm)
                      `(submod ,p ,@sm)))
                (cond
                 [(equal? (cadr s) ".")
                  (as-submod full-prev (cddr s))]
                 [(equal? (cadr s) "..")
                  (as-submod full-prev (cdr s))]
                 [else
                  (as-submod (s-loop (cadr s)) (cddr s))])]))])]))
      (collapse-module-path-index
       mpi
       (lambda ()
         (error 'collapse-module-path-index
                "internal error: should not have needed a base path")))))
           
(provide collapse-module-path
         collapse-module-path-index)
