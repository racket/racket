#lang racket/base

;; Resources are referrable & renderable objects, (most are html pages)

;; (resource path renderer referrer) creates and returns a new "resource"
;; value.  The arguments are:
;; - `path': the path of the output file, relative to the working directory,
;;   indicating where the resource file should be put at, also corresponding to
;;   the URL it will be found at.  It must be a `/'-separated relative string,
;;   no `..', `.', or `//', and it can end in `/' (which will turn to
;;   "index.html").
;; - `renderer': a unary function that renders the resource, receiving the path
;;   for the file to be created as an argument.  This path will be different
;;   than the `path' argument because this function is invoked in the target
;;   directory.
;; - `referrer': a function accepting one or more arguments (and possibly
;;   keywords) that produces a value to be used to refer to this resource
;;   (using `a', `img', etc).  The first value that will be passed to this
;;   function will be the actual URL path, which depends on the currently
;;   rendered page path -- the argument will be relative to it.
;; The resulting resource value is actually a rendering function that is
;; similar to the `referrer', except without the first URL argument -- when it
;; is called, it invokes the `referrer' function with the actual (relativized)
;; URL.  Creating a resource registers the `renderer' to be executed when
;; rendering is initiated.  Note that more resources can be created while
;; rendering; they will also be rendered in turn until no more resources are
;; created.

(require racket/list racket/string scribble/text "xml.rkt")

;; default file, urls to it will point to its directory instead, and a
;; /-suffixed path will render to this file
(define default-file "index.html")

;; the currently rendered directory, as a list
(define rendered-dirpath (make-parameter '()))

;; a mapping from path prefixes to urls (actually, any string) -- when two
;; paths are in the same prefix, links from one to the other are relative, but
;; if they're in different prefixes, the url will be used instead; the roots
;; are expected to be disjoint (= no "/foo" and "/foo/bar" roots)
(provide url-roots)
(define url-roots
  ;; takes in a (listof (list prefix-string url-string)), and produces an alist
  ;; with lists of strings for the keys; the prefix-strings are split on "/"s,
  ;; and the url-strings can be anything at all actually (they are put as-is
  ;; before the path with a "/" between them)
  (make-parameter #f
    (lambda (x)
      (and (list? x) (pair? x)
           (map (lambda (x)
                  (cons (regexp-match* #rx"[^/]+" (car x))
                        (regexp-replace #rx"/$" (cadr x) "")))
                x)))))

;; a utility for relative paths, taking the above `default-file' and
;; `url-roots' into consideration.
(define (relativize file tgtdir curdir)
  (define file* (if (equal? file default-file) "" file))
  (define roots (url-roots))
  (define (make-rooted path)
    (ormap (lambda (root+url)
             (let loop ([r (car root+url)] [p path])
               (if (null? r)
                 `(,(cdr root+url) ,@p ,file*)
                 (and (pair? p) (equal? (car p) (car r))
                      (loop (cdr r) (cdr p))))))
           roots))
  (define result
    (let loop ([t tgtdir] [c curdir] [pfx '()])
      (cond
        ;; find shared prefix
        [(and (pair? t) (pair? c) (equal? (car t) (car c)))
         (loop (cdr t) (cdr c) (cons (car t) pfx))]
        ;; done
        [(or (not roots)                  ; if there are no roots
             (make-rooted (reverse pfx))) ; or if they share a root
         ;; then make them relative
         `(,@(map (lambda (_) "..") c) ,@t ,file*)]
        ;; different roots => use the one for the target
        [(make-rooted t)]
        ;; otherwise throw an error
        [else (error 'relativize "target url is not in any known root: ~a"
                     (string-join `(,@tgtdir ,file*) "/"))])))
  (if (equal? '("") result) "." (string-join result "/")))
#| tests
(require tests/eli-tester)
(define R relativize)
(let ()
  (test do (test (R "bleh.txt"   '()        '()       ) => "bleh.txt"
                 (R "bleh.txt"   '("x")     '()       ) => "x/bleh.txt"
                 (R "bleh.txt"   '("x" "y") '()       ) => "x/y/bleh.txt"
                 (R "bleh.txt"   '()        '("x")    ) => "../bleh.txt"
                 (R "bleh.txt"   '("x")     '("x")    ) => "bleh.txt"
                 (R "bleh.txt"   '("x" "y") '("x")    ) => "y/bleh.txt"
                 (R "bleh.txt"   '()        '("x" "y")) => "../../bleh.txt"
                 (R "bleh.txt"   '("x")     '("x" "y")) => "../bleh.txt"
                 (R "bleh.txt"   '("x" "y") '("x" "y")) => "bleh.txt"
                 (R "bleh.txt"   '("x" "y") '("y" "x")) => "../../x/y/bleh.txt"
                 (R "index.html" '()        '()       ) => "."
                 (R "index.html" '("x")     '()       ) => "x/"
                 (R "index.html" '("x" "y") '()       ) => "x/y/"
                 (R "index.html" '()        '("x")    ) => "../"
                 (R "index.html" '("x")     '("x")    ) => "."
                 (R "index.html" '("x" "y") '("x")    ) => "y/"
                 (R "index.html" '()        '("x" "y")) => "../../"
                 (R "index.html" '("x")     '("x" "y")) => "../"
                 (R "index.html" '("x" "y") '("x" "y")) => "."
                 (R "index.html" '("x" "y") '("y" "x")) => "../../x/y/")
        do (parameterize ([url-roots '(["/x" "/X/"] ["/y" "/Y/"])])
             (test (R "bleh.txt"   '()        '()       ) =error> "not in any"
                   (R "bleh.txt"   '("x")     '()       ) => "/X/bleh.txt"
                   (R "bleh.txt"   '("x" "y") '()       ) => "/X/y/bleh.txt"
                   (R "bleh.txt"   '()        '("x")    ) =error> "not in any"
                   (R "bleh.txt"   '("x")     '("x")    ) => "bleh.txt"
                   (R "bleh.txt"   '("x" "y") '("x")    ) => "y/bleh.txt"
                   (R "bleh.txt"   '()        '("x" "y")) =error> "not in any"
                   (R "bleh.txt"   '("x")     '("x" "y")) => "../bleh.txt"
                   (R "bleh.txt"   '("x" "y") '("x" "y")) => "bleh.txt"
                   (R "bleh.txt"   '("x" "y") '("y" "x")) => "/X/y/bleh.txt"
                   (R "index.html" '()        '()       ) =error> "not in any"
                   (R "index.html" '("x")     '()       ) => "/X/"
                   (R "index.html" '("x" "y") '()       ) => "/X/y/"
                   (R "index.html" '()        '("x")    ) =error> "not in any"
                   (R "index.html" '("x")     '("x")    ) => "."
                   (R "index.html" '("x" "y") '("x")    ) => "y/"
                   (R "index.html" '()        '("x" "y")) =error> "not in any"
                   (R "index.html" '("x")     '("x" "y")) => "../"
                   (R "index.html" '("x" "y") '("x" "y")) => "."
                   (R "index.html" '("x" "y") '("y" "x")) => "/X/y/"))))
|#

;; utility for keeping a list of renderer thunks
(define-values [add-renderer get/reset-renderers]
  (let ([l '()] [s (make-semaphore 1)])
    ;; map paths to #t -- used to avoid overwriting files
    (define t (make-hash))
    (define-syntax-rule (S body) (call-with-semaphore s (lambda () body)))
    (values (lambda (path renderer)
              (S (if (hash-ref t path #f)
                   (error 'resource "path used for two resources: ~e" path)
                   (begin (hash-set! t path #t) (set! l (cons renderer l))))))
            (lambda () (S (begin0 (reverse l) (set! l '())))))))

;; `#:exists' determines what happens when the render destination exists, it
;; can be one of: #f (do nothing), 'delete-file (delete if a file exists, error
;; if exists as a directory)
(provide resource)
(define (resource path renderer referrer #:exists [exists 'delete-file])
  (define (bad reason) (error 'resource "bad path, ~a: ~e" reason path))
  (unless (string? path) (bad "must be a string"))
  (for ([x (in-list '([#rx"^/" "must be relative"]
                      [#rx"//" "must not have empty elements"]
                      [#rx"(?:^|/)[.][.]?(?:/|$)"
                          "must not contain `.' or `..'"]))])
    (when (regexp-match? (car x) path) (bad (cadr x))))
  (let ([path (regexp-replace #rx"(?<=^|/)$" path default-file)])
    (define-values (dirpathlist filename)
      (let-values ([(l r) (split-at-right (regexp-split #rx"/" path) 1)])
        (values l (car r))))
    (define (render)
      (let loop ([ps dirpathlist])
        (if (pair? ps)
          (begin (unless (directory-exists? (car ps))
                   (if (or (file-exists? (car ps)) (link-exists? (car ps)))
                     (bad "exists as a file/link")
                     (make-directory (car ps))))
                 (parameterize ([current-directory (car ps)])
                   (loop (cdr ps))))
          (begin (cond [(not exists)] ; do nothing
                       [(or (file-exists? filename) (link-exists? filename))
                        (delete-file filename)]
                       [(directory-exists? filename)
                        (bad "exists as directory")])
                 (parameterize ([rendered-dirpath dirpathlist])
                   (printf "  ~a\n" path)
                   (renderer filename))))))
    (define (url) (relativize filename dirpathlist (rendered-dirpath)))
    (add-renderer path render)
    (make-keyword-procedure
     (lambda (kws kvs . args) (keyword-apply referrer kws kvs (url) args))
     (lambda args (apply referrer (url) args)))))

;; a convenient utility to create renderers from some output function (like
;; `output-xml' or `display') and some content
(provide file-writer)
(define ((file-writer writer content) file)
  (call-with-output-file file (lambda (o) (writer content o))))

;; runs all renderers, and any renderers that might have been added on the way
(provide render-all)
(define (render-all)
  (printf "Rendering...\n")
  (let loop ()
    (let ([todo (get/reset-renderers)])
      (unless (null? todo)
        (for-each (lambda (r) (r)) todo)
        (loop)))) ; if more were created
  (printf "Rendering done.\n"))
