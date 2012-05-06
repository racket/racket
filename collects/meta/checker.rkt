;; Shared dependency-checking infrastructure, used by "check-dists.rkt"
;; and by the bundle script

#lang scheme/base

(require scheme/cmdline scheme/runtime-path scheme/match scheme/promise 
         scheme/list ; for use in specs too
         (for-syntax scheme/base) ; for runtime-path
         (except-in scheme/mpair mappend)
         (only-in mzlib/process system)
         "specs.rkt")

(define cd current-directory)

(provide current-verbose-port)
(define current-verbose-port (make-parameter current-output-port))

;;; ===========================================================================
;;; Utilities etc

(define concat string-append)

(define (sort* l)
  (sort l string<?))

(define (dir-list . args)
  (sort* (map path->string (apply directory-list args))))

(define (dprintf fmt . args)
  (let ([p ((current-verbose-port))])
    (apply fprintf p fmt args)
    (flush-output p)))

;;; ===========================================================================
;;; Object properties

(define *properties* (make-weak-hasheq))

(provide get-props
         prop-get
         prop-set!)

(define (get-props obj)
  (hash-ref *properties* obj (lambda ()
                               (let ([props (mlist 'props)])
                                 (hash-set! *properties* obj props)
                                 props))))

(define (prop-get obj prop [default #f])
  (let ([props (get-props obj)])
    (cond [(massq prop (mcdr props)) => mcdr]
          [(procedure? default) (default)]
          [(promise? default) (force default)]
          [else default])))

(define (prop-set! obj prop val)
  (let ([props (get-props obj)])
    (cond [(massq prop (mcdr props)) => (lambda (p) (set-mcdr! p val))]
          [else (set-mcdr! props (mcons (mcons prop val) (mcdr props)))])))

;;; ===========================================================================
;;; Tree utilities

;; A directory tree structure is either a string (for a file), or a pair of a
;; string (for a directory) and its entries.  Directory entries are always
;; sorted.  The strings are all paths beginning where the tree was scanned
;; from, and end with a "/" iff it is a directory.  Sometimes #f is used as an
;; exception "empty tree" value.

(provide get-tree)
;; path -> tree
;; Returns the tree with path (a string with no "/"s) at its root.
(define (get-tree path fake-path)
  (define base (regexp-replace #rx"/$" (path->string (cd)) ""))
  (let loop ([name path] [fake-name fake-path] [path ""] [fake-path ""])
    (cond [(or (file-exists? name) (link-exists? name))
           (let ([path (concat path name)]
                 [fake-path (concat fake-path fake-name)])
             (prop-set! fake-path 'base base)
             (prop-set! fake-path 'name name)
             (prop-set! fake-path 'real path)
             fake-path)]
          [(directory-exists? name)
           (let ([path (concat path name "/")]
                 [fake-path (concat fake-path fake-name "/")])
             (prop-set! fake-path 'base base)
             (prop-set! fake-path 'name name)
             (prop-set! fake-path 'real path)
             (parameterize ([cd name])
               (cons fake-path (map (lambda (name) (loop name name path fake-path))
                                    (dir-list)))))]
          [else (error 'get-tree/base "strange entry: ~a/~a"
                       (path->string (cd)) name)])))

(provide tree-path)
;; tree -> string
;; The path of the tree root -- if a file then identity, otherwise car.
(define (tree-path tree)
  (cond [(string? tree) tree]
        [(pair? tree) (car tree)]
        [else (error 'tree-path "got a bad tree: ~e" tree)]))

(provide tree-flatten)
;; tree [boolean] -> (list-of string)
(define (tree-flatten tree [only-files? #f])
  (let loop ([l '()] [tree (list tree)])
    (if (null? tree)
      (reverse l)
      (let ([1st (car tree)] [rest (cdr tree)])
        (if (pair? 1st)
          (loop (if only-files? l (cons (car 1st) l)) (append (cdr 1st) rest))
          (loop (cons 1st l) rest))))))

;; tree tree -> tree
;; Both trees should have the same root.  This is not a merge -- the trees
;; should not have equal files, directroy strings are taken from the first
;; tree.
(define (tree-add tree1 tree2)
  (cond [(not tree1) tree2]
        [(not tree2) tree1]
        [(not (and (pair? tree1) (pair? tree2)))
         (error 'tree-add "cannot add non-directories -- ~a and ~a"
                (tree-path tree1) (tree-path tree2))]
        [(not (equal? (car tree1) (car tree2)))
         (error 'tree-add "got incompatible entries -- ~a and ~a"
                (tree-path tree1) (tree-path tree2))]
        [else (let loop ([l1 (cdr tree1)]
                         [l2 (cdr tree2)]
                         [r (list (car tree1))])
                (cond [(and (null? l1) (null? l2)) (reverse r)]
                      [(null? l1) (loop l1 (cdr l2) (cons (car l2) r))]
                      [(null? l2) (loop (cdr l1) l2 (cons (car l1) r))]
                      [(string<? (tree-path (car l1)) (tree-path (car l2)))
                       (loop (cdr l1) l2 (cons (car l1) r))]
                      [(string>? (tree-path (car l1)) (tree-path (car l2)))
                       (loop l1 (cdr l2) (cons (car l2) r))]
                      [(and (pair? (car l1)) (pair? (car l2)))
                       (loop (cdr l1) (cdr l2)
                             (cons (tree-add (car l1) (car l2)) r))]
                      [(or (pair? (car l1)) (pair? (car l2)))
                       (error 'tree-add
                              "got incompatible file/dir entries -- ~a"
                              (tree-path (car l1)))]
                      [else
                       (error 'tree-add "a file appears in both trees -- ~a"
                              (tree-path (car l1)))]))]))

(provide add-trees)
;; tree list -> tree list
;; Adds up all input trees, generating a list of trees (in case of different
;; roots).
(define (add-trees trees)
  (let loop ([todo trees] [done '()])
    (cond [(null? todo) (reverse done)]
          [(not (car todo)) (loop (cdr todo) done)]
          [(assoc (caar todo) done) =>
           (lambda (t)
             (loop (cdr todo) (cons (tree-add t (car todo)) (remq t done))))]
          [else (loop (cdr todo) (cons (car todo) done))])))

(provide tree-subtract)
;; tree tree -> tree
;; All file entries that exist in tree2 are removed from tree1.
(define (tree-subtract tree1 tree2)
  (cond
    [(or (not tree1) (not tree2)) tree1]
    [(and (string? tree1) (string? tree2))
     (and (not (equal? tree1 tree2)) tree1)]
    [(and (pair? tree1) (pair? tree2))
     (if (equal? (car tree1) (car tree2))
       (let loop ([l1 (cdr tree1)] [l2 (cdr tree2)] [r '()])
         (cond [(or (null? l1) (null? l2))
                (let ([r (append (reverse r) l1)])
                  (and (pair? r) (cons (car tree1) r)))]
               [(string<? (tree-path (car l1)) (tree-path (car l2)))
                (loop (cdr l1) l2 (cons (car l1) r))]
               [(string>? (tree-path (car l1)) (tree-path (car l2)))
                (loop l1 (cdr l2) r)]
               [else (loop (cdr l1) (cdr l2)
                           (let ([sub (tree-subtract (car l1) (car l2))])
                             (if sub (cons sub r) r)))]))
       tree1)]
    [else (error 'tree-subtract
                 "got incompatible entries -- ~a ~a and ~a ~a"
                 (if (string? tree1) "file" "directory") (tree-path tree1)
                 (if (string? tree2) "file" "directory") (tree-path tree2))]))

;; tree -> tree
;; Removes empty directories and ones that contain only empty directories.
(define (remove-empty-trees tree)
  (if (string? tree)
    tree
    (let ([filtered (filtered-map remove-empty-trees (cdr tree))])
      (and (pair? filtered) (cons (car tree) filtered)))))

(provide tree-filter)
;; (string -> any) tree -> tree
;; If the filter returns '+ or '- this qualifies or disqualifies the
;; current tree immediately, otherwise recurse down directories.  If any other
;; result is returned for directories scanning continues, and for files they
;; are included if the result is not #f.
(define (tree-filter filter tree)
  (let ([filter (if (procedure? filter) filter (spec->filter filter))])
    (let loop ([tree tree])
      (let ([r (filter tree)])
        (case r
          [(+) tree] [(-) #f]
          [else (if (string? tree)
                  (and r tree)
                  (let ([filtered (filtered-map loop (cdr tree))])
                    ;; directories are removed if they're empty and if the
                    ;; predicate resulted in #f which means that we generally
                    ;; don't want the current tree
                    (if (or r (pair? filtered))
                      (cons (car tree) filtered)
                      #f)))])))))

;; return the base path of a tree
(define (tree-base tree)
  (prop-get (tree-path tree) 'base
            (lambda ()
              (error 'tree-base "no `base' property for ~e" tree))))

(provide print-tree)
(define (print-tree tree . mode)
  (let ([full?       (memq 'full mode)]
        ;; only-files is for files and empty dirs (used for untarring)
        [only-files? (memq 'only-files mode)])
    (let loop ([tree tree])
      (when tree
        (unless (and only-files? (pair? tree) (not (null? (cdr tree))))
          (when full? (printf "~a/" (tree-base tree)))
          (printf "~a\n" (tree-path tree)))
        (when (pair? tree) (for-each loop (cdr tree)))))))

;;; ===========================================================================
;;; Spec management

(define *spec-primitives* (make-parameter '()))
(define (register-spec-primitive! sym func)
  (*spec-primitives* (cons (cons sym func) (*spec-primitives*))))
(define (get-spec-primitive spec)
  (cond [(assq spec (*spec-primitives*)) => cdr] [else #f]))

;; Spec primitives

;; These are transformations that will convert a *simplified* expression to a
;; filter function.  Because of this, it is safe to have certain assumptions,
;; like `or' having at least two arguments etc, and it is also fine to not try
;; to do trivial optimizations (there is no need for them).  Also, the input to
;; these functions are functions (there is no point in a function that needs
;; raw arguments, since these can be implemented as macros).

(begin
  ;; `not' negates '+ <-> '- and #f <-> #t
  (register-spec-primitive! 
   '%not
   (lambda (pred)
     (lambda (t)
       (let ([r (pred t)])
         (case r [(+) '-] [(-) '+] [else (not r)])))))

  ;; `or' behaves like max for '- < #f < #t < '+
  (register-spec-primitive! 
   '%or
   (lambda preds
     (lambda (t)
       (let loop ([result '-] [preds preds])
         (if (or (eq? result '+) (null? preds))
             result
             (loop (let ([r ((car preds) t)])
                     (case r
                       [(+) '+] [(-) result]
                       [else (if (eq? result '-) r (or result r))]))
                   (cdr preds)))))))

  ;; `and' behaves like min for '- < #f < #t < '+
  (register-spec-primitive! 
   '%and
   (lambda preds
     (lambda (t)
       (let loop ([result '+] [preds preds])
         (if (or (eq? result '-) (null? preds))
             result
             (loop (let ([r ((car preds) t)])
                     (case r
                       [(-) '-] [(+) result]
                       [else (if (eq? result '+) r (and result r))]))
                   (cdr preds))))))))

;; Spec Macros

;; macros for primitive simplifications
(define (make-and/or-macro op)
  (let ([null-result (if (eq? op '%and) '%all '%none)]
        [best-result (if (eq? op '%and) '%none '%all)])
    ;; can return the same form -- expand-spec will not proceed in that case
    (lambda specs
      (let/ec return
        (let ([specs (mappend
                      (lambda (s)
                        (cond [(and (pair? s) (eq? (car s) op)) (cdr s)]
                              [(eq? s null-result) '()]
                              [(eq? s best-result) (return best-result)]
                              [else (list s)]))
                      specs)])
          (if (null? specs) null-result (cons op specs)))))))

(provide register-macros!)
(define (register-macros!)
  (register-spec! 'or (make-and/or-macro '%or))
  (register-spec! 'and (make-and/or-macro '%and))
  (register-spec! 
   'not
   (lambda specs
     ;; splice results back up, in case of (not) (which can result with a cond)
     (splice (map (lambda (spec)
                    (case spec
                      [(%all) '%none] [(%none) '%all] [else `(%not ,spec)]))
                  specs))))
  (register-spec! 'all '%all)
  (register-spec! 'none '%none)

  (register-spec! '+ 'or) ; `+' is `or'
  (register-spec! 
   '- ; set difference
   (lambda (spec . specs)
     `(and (or ,spec) (not (or ,@specs)))))
  
  (register-spec! 'error (lambda xs (apply error 'spec-error xs))))

;; Turns a string with globbing into a regexp string
(define (glob->regexp glob)
  (define len (string-length glob))
  (define range #f)
  (let loop ([res '()] [i 0])
    (define (next x) (loop (cons x res) (add1 i)))
    (if (= i len)
      (begin
        (when range
          (error 'glob->regexp "unterminated range in glob: ~e" glob))
        (let loop ([left res] [res '()])
          (if (null? left)
            (list->string res)
            (loop (cdr left)
                  ((if (char? (car left)) cons append) (car left) res)))))
      (let ([c (string-ref glob i)])
        (if range
          (begin (set! range
                       (case range
                         [(0) (case c ((#\^) 1) (else 2))]
                         [(1) 2]
                         [else (case c ((#\]) #f) (else 2))]))
                 (next c))
          (case c
            [(#\\) (set! i (add1 i))
                   (if (< i len)
                     (next (list #\\ (string-ref glob i)))
                     (error 'glob->regexp "glob ends in backslash: ~e" glob))]
            [(#\*) (next '(#\[ #\^ #\/ #\] #\*))]
            [(#\?) (next '(#\[ #\^ #\/ #\]))]
            [(#\[) (set! range 0) (next #\[)]
            [(#\. #\+ #\^ #\$ #\( #\) #\]) (next (list #\\ c))]
            ;; translate "{}" to "(?:)", "|" are left as-is "\|"
            [(#\{) (next '(#\( #\? #\:))]
            [(#\}) (next #\))]
            [else (next c)]))))))

(provide regexpify-spec)
;; Turns a string spec into a regexp to be matched against the `path' property.
(define (regexpify-spec str . force-rx?)
  (let* (;; initial "/" goes, so does a pointless initial "/**/"
         [rx (glob->regexp (regexp-replace #rx"^/(\\*\\*/)?" str ""))]
         ;; replace translated "/**/"s (they're never a prefix)
         [rx (regexp-replace* #rx"/\\[\\^/\\]\\*\\[\\^/\\]\\*/"
                              rx "/(?:.*/)?")]
         [rx (regexp (concat (if (regexp-match? #rx"^/" str) "^" "(?:^|/)")
                             rx (if (regexp-match? #rx"/$" str) "$" "/?$")))]
         [anchor (and (not (and (pair? force-rx?) (car force-rx?)))
                      (regexp-match? #rx"^/" str)
                      (regexp-replace #rx"^/([^][{}|*?]*)(.*)?$" str "\\1"))])
    ;; optimize anchored prefix strings
    (if anchor
      (let ([alen (string-length anchor)])
        (lambda (t)
          (let* ([p (tree-path t)] [plen (string-length p)])
            (let loop ([i 0])
              (cond
                [(or (= i alen) (= i plen)) (and (regexp-match? rx p) '+)]
                [(eq? (string-ref anchor i) (string-ref p i)) (loop (add1 i))]
                [else '-])))))
      rx)))

;; Turns a [composite] file spec into a filter function.  Wrap a filter spec
;; function in a cache.  This is not only for optimization, it is responsible
;; for making predicate composition behave like set operations because when a
;; directory's contents is skipped when the filter returns '+ or '-, the
;; contents is still marked.
(define (primitive-spec->filter spec)
  (define (add-query-cache! t r)
    (hash-set! (prop-get (tree-path t) 'queries 
                         (lambda () (let ([ht (make-hash)])
                                      (prop-set! (tree-path t) 'queries ht)
                                      ht)))
               spec
               r))
  (define (make-cached filter)
    (lambda (t)
      (cond [(hash-ref (prop-get (tree-path t) 'queries #hash()) spec #f)]
            [else (let ([r (filter t)])
                    (case r
                      [(+ -) (let loop ([t t])
                               (add-query-cache! t r)
                               (when (pair? t) (for-each loop (cdr t))))]
                      [else (add-query-cache! t r)])
                    r)])))
  (let loop ([spec spec])
    (cond
      [(procedure? spec) (make-cached spec)]
      [(regexp? spec) (loop (lambda (t)
                              (and (regexp-match? spec (tree-path t)) '+)))]
      [(string? spec) (loop (regexpify-spec spec))]
      [(eq? spec '%none) (lambda (t) '-)] ; no need to cache
      [(eq? spec '%all)  (lambda (t) '+)] ; no need to cache
      [(and (pair? spec) (get-spec-primitive (car spec)))
       ;; this is used with simplified expressions, so there is no point in
       ;; passing the raw arguments to the primitive, so just convert them
       ;; first.
       => (lambda (p)
            (make-cached (apply p (map primitive-spec->filter (cdr spec)))))]
      [else (error 'primitive-spec->filter "bad spec: ~.s" spec)])))

;; Toplevel entry point for converting a spec into a tree predicate function.
(define (spec->filter spec)
  (let ([specs (expand-spec spec)])
    (if (= 1 (length specs))
      (primitive-spec->filter (car specs))
      (error 'spec->filter
             "spec `~.s' did not expand to a single expression: ~.s"
             spec specs))))

;;; ===========================================================================
;;; Dependency checks

(define check-version
  (let ([version (version)] [1st? #t])
    (lambda (v file)
      (if 1st?
        (begin
          (unless (equal? version v)
            (eprintf "\nNOTE: bundling a different version from ~a\n\n"
                     "running process"))
          (set! version v)
          (set! 1st? #f))
        (unless (equal? version v)
          (error 'dependencies "bad version in ~s: ~s (expecting ~s)"
                 file v version))))))

(define (add-dependency-contents!)
  (define (racketpath path)
    (bytes->string/utf-8
     (apply bytes-append (cdr (mappend (lambda (p) (list #"/" p))
                                       (list* #"racket" #"collects" path))))))
  (define (read-depfile file)
    (let ([x (with-input-from-file file read)])
      (unless (and (pair? x) (check-version (car x) file))
        (error 'dependencies "bad contents in ~s: ~s" file x))
      (map (lambda (x)
             (match x
               [`(collects ,(and (? bytes?) s) ...) (racketpath s)]
               [`(ext collects ,(and (? bytes?) s) ...) (racketpath s)]
               [_ (error 'dependencies "bad dependency item in ~s: ~s"
                         file x)]))
           (cddr x))))
  (dprintf "Reading dependencies...")
  (let loop ([tree (tree-filter "*.dep" *racket-tree*)])
    (if (pair? tree)
      (for-each loop (cdr tree))
      (parameterize ([cd (prop-get tree 'base)])
        (prop-set! tree 'contents (read-depfile (prop-get tree 'real))))))
  (dprintf " done.\n")
  (set! add-dependency-contents! void))

(define bin-files-lists (delay null))

(provide set-bin-files-delayed-lists!)
(define (set-bin-files-delayed-lists! p)
  (set! bin-files-lists p))

(define (add-alts l)
  (if (null? l) 
      null
      (let ([v (regexp-replace #rx"[.]ss$" (car l) ".rkt")])
        (if (equal? v (car l))
            (cons (car l) (add-alts (cdr l)))
            (list* (car l) v (add-alts (cdr l)))))))

(define (check-dependencies spec distname)
  (add-dependency-contents!)
  (dprintf "Verifying dependencies for ~s..." distname)
  (let* ([all-files
          (sort* (add-alts (tree-flatten (tree-filter spec *racket-tree*))))]
         [deps0 (or (tree-filter `(and ,spec "*.dep") *racket-tree*)
                    (error 'check-dependencies
                           "got no .dep files for ~s" distname))]
         [deps0 (tree-flatten deps0 #t)])
    (let* ([missing (tree-filter 'must-be-empty *racket-tree*)]
           [missing (and (pair? missing) (tree-flatten missing #t))])
      (when (pair? missing)
        (dprintf "files missing from distribution:\n")
        (for ([m missing]) (dprintf "  ~a\n" m))
        (error 'dependencies "got files in must-be-empty (see above)")))
    (let loop ([files all-files]
               [deps (sort* (foldl (lambda (x y)
                                     (append (prop-get x 'contents) y))
                                   '()
                                   deps0))]
               [last-dep #f])
      (cond [(null? deps) #t]
            [(equal? (car deps) last-dep) (loop files (cdr deps) last-dep)]
            [(or (null? files) (string<? (car deps) (car files)))
             ;; Exception: foo.rkt might be satisified by a platform dependent
             ;; compiled/foo_rkt.zo (need to exist in all platform dependent
             ;; trees).  No need to optimize since this happens very
             ;; infrequently.
             (let ([dep (regexp-replace #rx"/([^/]+)\\.([^/]+)$" (car deps)
                                        "/compiled/\\1_\\2.zo")]
                   [alt-dep (and (regexp-match #rx"[.]rkt$" (car deps))
                                 (regexp-replace #rx"/([^/]+)\\.([^/]+)$" (car deps)
                                                 "/compiled/\\1_ss.zo"))])
               (if (andmap (lambda (files) (or (member dep files)
                                               (member alt-dep files)))
                           (force bin-files-lists))
                 (loop files (cdr deps) (car deps))
                 (error 'dependencies "unsatisfied dependency for ~s: ~s ~s"
                        distname (car deps)
                        (cons 'in: (filter (lambda (d)
                                             (member (car deps)
                                                     (prop-get d 'contents)))
                                           deps0)))))]
            [(string<? (car files) (car deps))
             (loop (cdr files) deps last-dep)]
            [else (loop (cdr files) (cdr deps) (car deps))])))
  (dprintf " done.\n"))

;;; ===========================================================================
;;; Start working

(define *platform-tree-lists* null)
(define *racket-tree* #f)

(provide get-racket-tree)
(define (get-racket-tree) *racket-tree*)

(provide verify!)
(define (verify!)
  (define features (filter string? (reverse (*environment*))))
  (tag (cons 'verifying (map string->symbol features))
    (check-dependencies 'distribution
                        (apply concat (cdr (mappend (lambda (x) (list "-" x))
                                                    features)))))
  '())

(provide checker-namespace-anchor)
(define-namespace-anchor checker-namespace-anchor)

(define racket/ #f)
(provide set-racket-tree!)
(define (set-racket-tree! racket/* racket-base/ racket/-name tree-lists)
  (set! racket/ racket/*)
  (set! *platform-tree-lists* tree-lists)
  (dprintf "Scanning main tree...")
  (set! *racket-tree*
        (let loop ([tree  (parameterize ([cd racket-base/])
                            (get-tree racket/-name "racket"))]
                   [trees (apply append *platform-tree-lists*)])
          (if (null? trees)
              (tree-filter '(not junk) tree)
              (loop (tree-subtract tree (car trees)) (cdr trees)))))
  (dprintf " done.\n"))
