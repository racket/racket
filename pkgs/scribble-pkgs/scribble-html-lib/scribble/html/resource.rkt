#lang racket/base

;; Resources are renderable & referrable objects, (most are html pages).

;; (resource path renderer) creates and returns a new "resource" value.  The
;; arguments are:
;; - `path': the path of the output file, relative to the working directory,
;;   indicating where the resource file should be put at, also corresponding to
;;   the URL it will be found at.  It must be a `/'-separated relative string,
;;   no `..', `.', or `//', and it can end in `/' (which will turn to
;;   "index.html").
;; - `renderer': a unary function that renders the resource, receiving the path
;;   for the file to be created as an argument.  This path will be different
;;   than the `path' argument because this function is invoked in the target
;;   directory.
;; The resulting resource value is a function that returns the URL for the
;; resource.  The function takes in an optional boolean which defaults to #f,
;; and when #t is given, the result will be an absolute full URL.  Note that
;; the function can be used as a value for output, which will use it as a thunk
;; (that renders as the relative URL for the resource).  The default relative
;; resulting URL is, of course, a value that depends on the currently rendered
;; resource that uses this value.  Creating a resource registers the `renderer'
;; to be executed when rendering is initiated by `render-all'.  Note that more
;; resources can be created while rendering; they will also be rendered in turn
;; until no more new resources are created.

(require scribble/text)

;; default file, urls to it will point to its directory instead, a
;; /-suffixed path will render to this file, and `url-roots' entries
;; with 'index will append this file name to a rewritten path that
;; otherwise ends in /
(define default-file "index.html")

;; the currently rendered directory, as a list
(define rendered-dirpath (make-parameter '()))

;; A mapping from path prefixes to urls (actually, any string) -- when two
;; paths are in the same prefix, links from one to the other are relative
;; (unless absolute links are requested) , but if they're in different
;; prefixes, the url will be used instead; the roots are expected to be
;; disjoint (= no "/foo" and "/foo/bar" roots).  Additionally, optional symbol
;; flags can appear in each entry, currently only 'abs is used below for roots
;; that should always use absolute links (needed for some skeleton pages that
;; are used in nested subdirectories).
(provide url-roots)
(define url-roots (make-parameter #f))

(define cached-roots '(#f . #f))
(define (current-url-roots)
  ;; takes `url-roots', a (listof (list prefix-string url-string . flags)), and
  ;; produces an alist with lists of strings for the keys; the prefix-strings
  ;; are split on "/"s, and the url-strings can be anything at all actually
  ;; (they are put as-is before the path with a "/" between them).
  (define roots (url-roots))
  (unless (eq? roots (car cached-roots))
    (set! cached-roots
          (cons roots
                (and (list? roots) (pair? roots)
                     (map (lambda (root)
                            (list* (regexp-match* #rx"[^/]+" (car root))
                                   (regexp-replace #rx"/$" (cadr root) "")
                                   (cddr root)))
                          roots)))))
  (cdr cached-roots))

;; a utility for relative paths, taking the above `default-file' and
;; `url-roots' into consideration.
(define (relativize file tgtdir curdir)
  (define file* (if (equal? file default-file) "" file))
  (define roots (current-url-roots))
  (define (find-root path mode)
    (ormap (lambda (root+url+flags)
             (let loop ([r (car root+url+flags)] [p path])
               (if (pair? r)
                 (and (pair? p) (equal? (car p) (car r))
                      (loop (cdr r) (cdr p)))
                 (case mode
                   [(get-path) `(,(cadr root+url+flags) 
                                 ,@p 
                                 ,(if (and (equal? file* "")
                                           (memq 'index (cddr root+url+flags)))
                                      default-file
                                      file*))]
                   [(get-abs-or-true)
                    (if (memq 'abs (cddr root+url+flags)) `("" ,@p) #t)]
                   [else (error 'relativize "internal error: ~e" mode)]))))
           roots))
  (define result
    (let loop ([t tgtdir] [c curdir] [pfx '()])
      (cond
        ;; find shared prefix
        [(and (pair? t) (pair? c) (equal? (car t) (car c)))
         (loop (cdr t) (cdr c) (cons (car t) pfx))]
        ;; done with the shared prefix, deal with the root now
        ;; no roots => always use a relative path (useful for debugging)
        [(not roots) `(,@(map (lambda (_) "..") c) ,@t ,file*)]
        ;; share a root => use a relative path unless its an absolute root
        [(find-root (reverse pfx) 'get-abs-or-true)
         => (lambda (abs/true)
              `(;; rel. => as above
                ,@(if (list? abs/true) abs/true (map (lambda (_) "..") c))
                ,@t ,file*))]
        ;; different roots => use the one for the target
        [(find-root tgtdir 'get-path)]
        ;; if there isn't any, throw an error
        [else (error 'relativize "target url is not in any known root: ~a"
                     (string-join `(,@tgtdir ,file*) "/"))])))
  (if (equal? '("") result) "." (string-join result "/")))

#;
(module+ test
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
                     (R "index.html" '("x" "y") '("y" "x")) => "/X/y/"))
          do (parameterize ([url-roots '(["/x" "/X/"] ["/y" "/Y/" abs])])
               (test (R "foo.txt" '("x" "1") '("x" "2")) => "../1/foo.txt"
                     (R "foo.txt" '("y" "1") '("y" "2")) => "/1/foo.txt")))))

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
(provide resource resource?)
;; use a struct to make resources identifiable as such
(struct resource (url) #:constructor-name make-resource
        #:property prop:procedure 0 #:omit-define-syntaxes)
(define (resource path0 renderer #:exists [exists 'delete-file])
  (define (bad reason) (error 'resource "bad path, ~a: ~e" reason path0))
  (unless (string? path0) (bad "must be a string"))
  (for ([x (in-list '([#rx"^/" "must be relative"]
                      [#rx"//" "must not have empty elements"]
                      [#rx"(?:^|/)[.][.]?(?:/|$)"
                          "must not contain `.' or `..'"]))])
    (when (regexp-match? (car x) path0) (bad (cadr x))))
  (define path (regexp-replace #rx"(?<=^|/)$" path0 default-file))
  (define-values [dirpathlist filename]
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
  (define absolute-url
    (lazy (define url (relativize filename dirpathlist '()))
          (if (url-roots)
            url
            ;; we're in local build mode, and insist on an absolute url, so
            ;; construct a `file://' result
            (list* "file://" (current-directory) url))))
  (when renderer
    (add-renderer path render))
  (define (url [absolute? #f])
    ;; be conservative, in case it needs to be extended in the future
    (case absolute?
      [(#f) (relativize filename dirpathlist (rendered-dirpath))]
      [(#t) (force absolute-url)]
      [else (error 'resource "bad absolute flag value: ~e" absolute?)]))
  (make-resource url))

;; a convenient utility to create renderers from some output function (like
;; `output-xml' or `display') and some content
(provide file-writer)
(define ((file-writer writer content) file)
  (call-with-output-file file (lambda (o) (writer content o))))

;; runs all renderers, and any renderers that might have been added on the way
(provide render-all)
(define (render-all)
  (printf "Rendering...\n")
  (define todo (get/reset-renderers))
  (if (null? todo)
    (printf "  Warning: no content to render\n")
    (let loop ([todo todo])
      (unless (null? todo)
        (for-each (lambda (r) (r)) todo)
        (loop (get/reset-renderers))))) ; if more were created
  (printf "Rendering done.\n"))
