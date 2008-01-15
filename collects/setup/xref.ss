#lang scheme/base

(require scribble/xref
         setup/getinfo
         setup/dirs
         setup/main-collects)

(provide load-collections-xref)

(define cached-xref #f)

(define (get-dests dir)
  (let* ([i (get-info/full dir)]
         [scribblings (i 'scribblings)]
         [categories (i 'doc-categories (lambda ()
                                          (map (lambda (d) 'library) scribblings)))])
  (map (lambda (d cat)
         (and (not (eq? cat 'omit))
              (not (and (pair? cat) (eq? (car cat) 'omit)))
              (pair? d)
              (let* ([flags (if (pair? (cdr d)) (cadr d) null)]
                     [name (if (and (pair? (cdr d)) (pair? (cddr d)) (caddr d))
                               (cadr d)
                               (let-values ([(base name dir?) (split-path (car d))])
                                 (path-replace-suffix name #"")))])
                (build-path
                 (cond
                  [(memq 'main-doc-root flags)
                   (find-doc-dir)]
                  [(memq 'user-doc-root flags)
                   (find-user-doc-dir)]
                  [(memq 'user-doc flags)
                   (build-path (find-user-doc-dir) name)]
                  [(or (memq 'main-doc flags)
                       (pair? (path->main-collects-relative dir)))
                   (build-path (find-doc-dir) name)]
                  [else
                   (build-path dir "compiled" "doc" name)])
                 "out.sxref"))))
       scribblings
       categories)))

(define (load-collections-xref [report-loading void])
  (or cached-xref
      (begin
        (report-loading)
        (let* ([dests (map get-dests (find-relevant-directories '(scribblings)))]
               [dests (filter values (apply append dests))])
          (set! cached-xref
                (load-xref (map (lambda (dest)
                                  (lambda ()
                                    (with-handlers ([exn:fail?
                                                     (lambda (exn)
                                                       (fprintf (current-error-port)
                                                                "WARNING: ~a\n"
                                                                (if (exn? exn)
                                                                  (exn-message exn)
                                                                  (format "~e" exn)))
                                                       #f)])
                                      (let ([r (with-input-from-file dest read)])
                                        (cadr r)))))
                                dests)))
          cached-xref))))
