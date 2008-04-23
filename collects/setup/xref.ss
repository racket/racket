#lang scheme/base

(require scribble/xref
         setup/getinfo
         scheme/fasl
         "private/doc-path.ss")

(provide load-collections-xref)

(define cached-xref #f)

(define (get-dests dir)
  (let* ([i (get-info/full dir)]
         [scribblings (i 'scribblings)])
  (map (lambda (d)
         (and (not (and (list? d)
                        ((length d) . > . 2)
                        (pair? (list-ref d 2))
                        (eq? (car (list-ref d 2)) 'omit)))
              (pair? d)
              (let* ([flags (if (pair? (cdr d)) (cadr d) null)]
                     [name (if (and (pair? (cdr d)) (pair? (cddr d)) 
                                    (pair? (cdddr d)))
                               (cadddr d)
                               (let-values ([(base name dir?) (split-path (car d))])
                                 (path-replace-suffix name #"")))])
                (build-path
                 (doc-path dir name flags)
                 "out.sxref"))))
       scribblings)))

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
                                      (let ([r (call-with-input-file* dest fasl->s-exp)])
                                        (cadr r)))))
                                dests)))
          cached-xref))))
