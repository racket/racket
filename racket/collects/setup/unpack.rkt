#lang racket/base

(require file/gunzip
         racket/file
         racket/list
         racket/port
         racket/bool
         net/base64
         setup/getinfo
         racket/match
         "dirs.rkt")

(provide unpack
         fold-plt-archive)

;; ----------------------------------------

;; Returns a port and a kill thunk
(define (port64gz->port p64gz)
  ;; Inflate in a thread so the whole input isn't read at once
  (let-values ([(base64-out base64-in) (make-pipe 4096)]
               [(guz-out guz-in) (make-pipe 4096)])
    (let ([64t
           (thread (lambda ()
                     (dynamic-wind
                         void
                         (lambda () (base64-decode-stream p64gz base64-in))
                         (lambda () (close-output-port base64-in)))))]
          [gzt
           (thread (lambda ()
                     (dynamic-wind
                         void
                         (lambda () (gunzip-through-ports base64-out guz-in))
                         (lambda () (close-output-port guz-in)))))])
      (values guz-out (lambda () (kill-thread 64t) (kill-thread gzt))))))

;; ------------------------------------------------------------

;; fold-plt-archive : path[to .plt file] (sexpr A -> A) (sexpr input-port A -> A) (path A -> A) (path input-port A -> A) A -> A
(define (fold-plt-archive filename on-config-fn on-setup-unit on-directory on-file initial-value)
  (let*-values ([(fip) (open-input-file filename)]
                [(ip kill) (port64gz->port fip)])
    (dynamic-wind
        void
        (λ () (fold-plt-archive-port ip on-config-fn on-setup-unit on-directory on-file initial-value))
        (λ ()
           (close-input-port fip)
           (kill)))))

;; fold-plt-archive-port : input-port (sexpr A -> A) (sexpr input-port A -> A) (path A -> A) (path input-port A -> A) A -> A
(define (fold-plt-archive-port p on-config-fn on-setup-unit on-directory on-file initial-value)
  
  ;; skip past the initial #"PLT" and two sexprs
  (unless (and (eq? #\P (read-char p))
               (eq? #\L (read-char p))
               (eq? #\T (read-char p)))
    (error "not an unpackable distribution archive"))
  
  (let* ([config-fn-sexpr (read p)]
         [_  (when (eof-object? config-fn-sexpr) (error "malformed input"))]
         [val (on-config-fn config-fn-sexpr initial-value)]
         
         [setup-unit (read p)]
         [_ (when (eof-object? setup-unit) (error "malformed input"))]
         [val (on-setup-unit setup-unit p val)])
    
    ;; read contents of file directly. [on-setup-unit may have consumed all input,
    ;; but if so this loop will just do nothing.]
    (let loop ([val val])
      (let ([kind (read p)])
        (cond
         [(eof-object? kind) val]
         [else
          (case kind
            [(dir) 
             (let* ([v (read p)]
                    [s (expr->path-descriptor v)])
               (unless (relative-path-descriptor? s)
                 (error "expected a directory name relative path string, got" s))
               (let ([next-val (on-directory s val)])
                 (loop next-val)))]
            [(file file-replace)
             (let* ([v (read p)]
                    [s (expr->path-descriptor v)])
               (unless (relative-path-descriptor? s)
                 (error "expected a file name relative path string, got" s))
               (let ([len (read p)])
                 (unless (and (number? len) (integer? len))
                   (error "expected a file name size, got" len))
                 ;; Find starting *
                 (let loop ()
                   (let ([c (read-char p)])
                     (cond [(char=? c #\*) (void)] ; found it
                           [(char-whitespace? c) (loop)]
                           [(eof-object? c) (void)] ; signal the error below
                           [else (error 
                                  (format "unexpected character setting up ~a, looking for *" s)
                                  c)])))
                 (let-values ([(control fp) (protected-limited-input-port p len)])
                   (let ([next-val (if (procedure-arity-includes? on-file 4)
                                       (on-file s fp kind val)
                                       (on-file s fp val))])
                     (exhaust-port control)
                     (loop next-val)))))]
            [else (error "unknown file tag" kind)])])))))

;; path-descriptor ::= 'same | (list location path)
;; location        ::= symbol in '(same collects doc lib include)

;; expr->path-descriptor : sexpr -> path-descriptor
;; extracts a path-descriptor from an sexpr embedded in a .plt file
;; raises an error if the given sexpr can't be converted to a path descriptor
(define (expr->path-descriptor v)
  (cond
   [(null? v) 'same]
   [(and (pair? v) (symbol? (car v)) (symbol=? (car v) 'same))
    (apply build-path 'same (cdr v))]
   [(and (pair? v) (string? (car v)))
    (let ([location (string->loc (car v))])
      (if (eq? location 'relative)
          (apply build-path v)
          (if (null? (cdr v))
              (list location (build-path/convention-type (system-path-convention-type) 'same))
              (list location (apply build-path (cdr v))))))]
   [else (error "malformed path description: " v)]))

;; string->loc : string -> location
;; converts the string into a corresponding location, or raises an error
;; if that is not possible
(define (string->loc str)
  (let ([loc (string->symbol str)])
    (cond
     [(memq loc '(collects doc lib include same)) loc]
     [else 'relative])))

;; relative-path-descriptor? : path-descriptor -> boolean
;; determines if the given path descriptor names a relative file rather
;; than an absolute one
(define (relative-path-descriptor? s)
  (or (eq? s 'same) 
      (and (path? s) (relative-path? s))
      (relative-path? (cadr s))))

;; protected-limited-output-port input-port n -> (values input-port input-port)
;; returns two input ports. the first reads from the given input port, and the second
;; reads from the first.
;; why would you ever want to do this? So that you can hand out the second, and then
;; regardless of whether the user closes it or not you still have a limited input port
;; you can read to exhaustion.
(define (protected-limited-input-port ip limit)
  (let* ([i2 (make-limited-input-port ip limit #f)]
         [i3 (make-limited-input-port i2 limit #f)])
    (values i2 i3)))

;; exhaust-port : input-port -> void
;; consumes all input on the given port
(define exhaust-port
  (let ([nowhere (open-output-nowhere)])
    (λ (ip) (copy-port ip nowhere))))


;; ------------------------------------------------------------


(define (pretty-name f)
  (with-handlers ([void (lambda (x) f)])
    (let-values ([(base name dir?) (split-path f)])
      (format "~a in ~a"
              (path->string name)
              (if (path? base) (path->string base) base)))))

(define (shuffle-path parent-dir get-dir shuffle? v)
  (if shuffle?
      ;; Re-arrange for "collects', etc.
      (let ([v (remq* '(same) v)])
        (if (null? v)
            (values #f 'same)
            (let ([dir
                   (case (string->symbol (car v))
                     [(collects) (get-dir find-collects-dir find-user-collects-dir)]
                     [(doc)     (get-dir find-doc-dir find-user-doc-dir)]
                     [(lib)     (get-dir find-lib-dir find-user-lib-dir)]
                     [(include) (get-dir find-include-dir find-user-include-dir)]
                     [else #f])])
              (if dir
                  (if (null? (cdr v))
                      (values dir 'same)
                      (values dir (apply build-path (cdr v))))
                  (values parent-dir (apply build-path v))))))
      (values parent-dir (if (null? v) 'same (apply build-path v)))))

(define (unmztar p filter parent-dir get-dir shuffle? print-status)
  (define bufsize 4096)
  (define buffer (make-bytes bufsize))
  (let loop ()
    (let ([kind (read p)])
      (unless (eof-object? kind)
        (case kind
          [(dir) (let-values ([(target-dir s)
                               (shuffle-path parent-dir get-dir shuffle? (read p))])
                   (unless (or (eq? s 'same) (relative-path? s))
                     (error "expected a directory name relative path string, got" s))
                   (when (and target-dir
                              (or (eq? s 'same) (filter 'dir s target-dir)))
                     (let ([d (build-path target-dir s)])
                       (unless (directory-exists? d)
                         (print-status
                          (format "  making directory ~a" (pretty-name d)))
                         (make-directory* d)))))]
          [(file file-replace)
           (let-values ([(target-dir s)
                         (shuffle-path parent-dir get-dir shuffle? (read p))])
             (unless (relative-path? s)
               (error "expected a file name relative path string, got" s))
             (let ([len (read p)])
               (unless (and (number? len) (integer? len))
                 (error "expected a file name size, got" len))
               (let* ([write? (filter kind s target-dir)]
                      [path (build-path target-dir s)])
                 (let ([out (and write?
                                 (if (file-exists? path)
                                     (if (eq? kind 'file)
                                         #f
                                         (open-output-file path #:exists 'truncate))
                                     (open-output-file path)))])
                   (when (and write? (not out))
                     (print-status (format "  skipping ~a; already exists" (pretty-name path))))
                   (when out
                     (print-status (format "  unpacking ~a" (pretty-name path))))
                   ;; Find starting *
                   (let loop ()
                     (let ([c (read-char p)])
                       (cond [(char=? c #\*) (void)] ; found it
                             [(char-whitespace? c) (loop)]
                             [(eof-object? c) (void)] ; signal the error below
                             [else (error 
                                    (format "unexpected character setting up ~a, looking for *"
                                            path)
                                    c)])))
                   ;; Copy file data
                   (let loop ([n len])
                     (unless (zero? n)
                       (let ([l (read-bytes! buffer p 0 (min n bufsize))])
                         (when (eof-object? l)
                           (error (format
                                   "unexpected end-of-file while ~a ~a (at ~a of ~a)"
                                   (if out "unpacking" "skipping")
                                   path
                                   (- len n -1) len)))
                         (when out (write-bytes buffer out 0 l))
                         (loop (- n l)))))
                   (when out (close-output-port out))))))]
          [else (error "unknown file tag" kind)])
        (loop)))))

(define (call-info info flag mk-default test)
  (if info
      (let ([v (info flag mk-default)]) (test v) v)
      (mk-default)))

(define unpack
  (lambda (archive 
           [main-collects-parent-dir (current-directory)]
           [print-status (lambda (x) (printf "~a\n" x))]
           [get-target-directory (lambda () (current-directory))]
           [force? #f]
           [get-target-plt-directory
            (lambda (preferred main-collects-parent-dir options)
              preferred)])
    (let*-values ([(p64gz) (open-input-file archive)]
                  [(p kill) (port64gz->port p64gz)])
      (dynamic-wind
       void
       (lambda ()
         (unless (and (eq? #\P (read-char p))
                      (eq? #\L (read-char p))
                      (eq? #\T (read-char p)))
           (error "not an unpackable distribution archive"))
         (let* ([info (let ([v (read p)])
                        (match v
                          [`(lambda (request failure)
                              (case request
                                [(name) ,name]
                                [(unpacker) 'mzscheme]
                                [(requires) ',requires]
                                [(conflicts) ',conflicts]
                                [(plt-relative?) ,plt-relative?]
                                [(plt-home-relative?) ,plt-home-relative?]
                                . ,(or `([(test-plt-dirs) ,test-dirs] ; #f or `(quote ,dirs)
                                         [else (failure)])
                                       `([,(and 'else test-dirs) (failure)]))))
                           (lambda (request failure)
                             (case request
                               [(name) name]
                               [(unpacker) 'mzscheme]
                               [(requires) requires]
                               [(conflicts) conflicts]
                               [(plt-relative?) plt-relative?]
                               [(plt-home-relative?) plt-home-relative?]
                               [(test-plt-dirs) (and test-dirs
                                                     (not (eq? test-dirs 'else))
                                                     (cadr test-dirs))]
                               [else (failure)]))]
                          [else
                           (error "info-procedure S-expression did not have the expected shape: "
                                  v)]))])
           (unless (and (procedure? info)
                        (procedure-arity-includes? info 2))
             (error "expected a procedure of arity 2, got" info))
           (let ([name (call-info info 'name (lambda () #f)
                                  (lambda (n) 
                                    (unless (string? n)
                                      (if n
                                          (error "couldn't find the package name")
                                          (error "expected a string")))))]
                 [unpacker (call-info info 'unpacker (lambda () #f)
                                      (lambda (n) 
                                        (unless (eq? n 'mzscheme)
                                          (error "unpacker isn't mzscheme:" n))))]
                 [target-dir-info
                  (let ([rel? (call-info info 'plt-relative? (lambda () #f) values)]
                        [not-user-rel? (call-info info 'plt-home-relative? (lambda () #f) values)]
                        [test-dirs (call-info info 'test-plt-dirs (lambda () #f) values)])
                    (if rel?
                        ;; Shuffling...
                        (if (and not-user-rel? 
                                 ;; Check for void because old unpacker didn't use
                                 ;;  the failure thunk.
                                 (not (void? not-user-rel?))
                                 ;; Non-user optional if test-dirs are writable
                                 (or (not test-dirs)
                                     (andmap
                                      (lambda (p)
                                        (and (string? p)
                                             (let ([dir (let-values ([(base dir)
                                                                      (shuffle-path main-collects-parent-dir
                                                                                    (lambda (a b) (a))
                                                                                    #t (list p))])
                                                          (build-path base dir))])
                                               (memq 'write
                                                     (with-handlers ([exn:fail:filesystem? (lambda (x) null)])
                                                       (file-or-directory-permissions dir))))))
                                      test-dirs)))
                            ;; Shuffle to main directory always:
                            (let ([dir (get-target-plt-directory main-collects-parent-dir
                                                                 main-collects-parent-dir
                                                                 (list main-collects-parent-dir))])
                              (list dir (lambda (sys user)
                                          (let ([a (sys)])
                                            (get-target-plt-directory a a (list a))))))
                            ;; Prefer to shuffle to user directory:
                            (let ([addons (find-user-collects-dir)])
                              (let ([dir (get-target-plt-directory
                                          addons
                                          main-collects-parent-dir
                                          (list addons main-collects-parent-dir))])
                                (list dir (lambda (sys user)
                                            (let ([a (sys)]
                                                  [b (user)])
                                              (get-target-plt-directory b a (list b a))))))))
                        ;; No shuffling --- install to target directory:
                        (list (get-target-directory))))])

             ;; Stop if no target directory:
             (if (car target-dir-info)

                 ;; Check declared dependencies (none means v103)
                 (begin
                   (call-info 
                    info 'requires (lambda () null)
                    (lambda (l) 
                      (define (bad)
                        (error "`requires' info is corrupt:" l))
                      (when (void? l)
                        (if force?
                            (print-status "warning: archive is for an older version of Racket")
                            (error "cannot install; archive is for an older version of Racket")))
                      (unless (or (list? l) (and force? (void? l)))
                        (bad))
                      ;; Check each dependency:
                      (when (list? l)
                        (for-each
                         (lambda (d)
                           (unless (and (list? d) (= 2 (length d)))
                             (bad))
                           (let ([coll-path (car d)]
                                 [version (cadr d)])
                             (unless (and (pair? coll-path)
                                          (list? coll-path)
                                          (andmap string? coll-path)
                                          (list? version)
                                          (andmap number? version))
                               (bad))
                             (with-handlers ([exn:fail:filesystem?
                                              (lambda (x)
                                                (if force?
                                                    (print-status 
                                                     (format "warning: missing required collection ~s" coll-path))
                                                    (error "cannot install; missing required collection" coll-path)))])
                               (apply collection-path coll-path))
                             (let ([inst-version 
                                    (with-handlers ([void (lambda (x) 
                                                            (if (exn:break? x)
                                                                (raise x)
                                                                null))])
                                      (let ([info (get-info coll-path)])
                                        (info 'version (lambda () null))))])
                               (let loop ([v version][iv inst-version])
                                 (unless (null? v)
                                   (when (or (null? iv)
                                             (not (= (car v) (car iv))))
                                     (let ([msg (format "version ~a of collection ~s is required, but version ~a is installed"
                                                        version coll-path 
                                                        (if (null? inst-version)
                                                            '<unknown>
                                                            inst-version))])
                                       (if force?
                                           (print-status (format "warning: ~a" msg))
                                           (error (format "cannot install; ~a" msg)))))
                                   (loop (cdr v) (cdr iv)))))))
                         l))))

                   ;; Check for conflicts:
                   (call-info
                    info 'conflicts (lambda () null)
                    (lambda (l) 
                      (define (bad)
                        (error "`conflicts' info is corrupt:" l))
                      (unless (or (list? l) (and force? (void? l)))
                        (bad))
                      (when (list? l)
                        (for-each
                         (lambda (coll-path)
                           (unless (and (pair? coll-path)
                                        (list? coll-path)
                                        (andmap string? coll-path))
                             (bad))
                           (when (with-handlers ([exn:fail? (lambda (x) #f)])
                                   (apply collection-path coll-path))
                             (error "cannot install; conflict with installed collection"
                                    coll-path)))
                         l))))
                   
                   (unless (and name unpacker)
                     (error "bad name or unpacker"))
                   (print-status (format "Unpacking ~a from ~a" name archive))
                   (let ([u (read p)])
                     (match u
                       [`(unit (import ,(? symbol?) mzuntar) 
                               (export)
                               (mzuntar void)
                               (quote ,collections))
                        (make-directory* (car target-dir-info))
                        (unmztar p void
                                 (car target-dir-info) 
                                 (lambda (a b)
                                   ((cadr target-dir-info) a b))
                                 ((length target-dir-info) . > . 1)
                                 print-status)
                        collections]
                       [else
                        (error "expected a `unit' pattern, got" u)])))

                 ;; Cancelled: no collections
                 null))))
       (lambda () (kill) (close-input-port p64gz))))))
