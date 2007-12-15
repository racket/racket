#lang scheme/base

(require scribble/xref
         setup/getinfo
         setup/dirs)

(provide load-collections-xref)

(define cached-xref #f)

(define (load-collections-xref)
  (or cached-xref
      (let* ([dirs (find-relevant-directories '(scribblings))]
             [infos (map get-info/full dirs)]
             [dests (filter
                     values
                     (apply append
                            (map (lambda (i dir)
                                   (let ([s (i 'scribblings)])
                                     (map (lambda (d)
                                            (if (pair? d)
                                                (let ([flags (if (pair? (cdr d))
                                                                 (cadr d)
                                                                 null)])
                                                  (let ([name (if (and (pair? (cdr d))
                                                                       (pair? (cddr d))
                                                                       (caddr d))
                                                                  (cadr d)
                                                                  (let-values ([(base name dir?) (split-path (car d))])
                                                                    (path-replace-suffix name #"")))])
                                                    (build-path
                                                     (if (memq 'main-doc flags)
                                                         (build-path (find-doc-dir) name)
                                                         (build-path dir "compiled" "doc" name))
                                                     "out.sxref")))
                                                #f))
                                          s)))
                                 infos
                                 dirs)))])
        (set! cached-xref (load-xref dests))
        cached-xref)))
