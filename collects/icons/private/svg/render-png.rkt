#lang racket/base

;; Re-renders every SVG as PNGs of different sizes

(require racket/file xml
         "../utils.rkt")

(define render-icon (make-parameter render/inkscape))

(define (clean-icons dir)
  (printf "Cleaning out icons in ~a~n" dir)
  (printf "----------------------------------------~n")
  (define files (directory-list dir))
  (for ([file  (in-list files)])
    (define full-path (build-path dir file))
    (when (directory-exists? full-path)
      (cond [(exact-integer? (string->number (path->string file)))
             (printf "Deleting directory ~a~n" full-path)
             (delete-directory/files full-path)]
            [else
             (clean-icons full-path)]))))

(define (render-icon/color src-dir src-file color height)
  (cond [color
         (define src-path (build-path src-dir src-file))
         (define dest-dir (build-path src-dir (format "~a/~a" (number->string height) color)))
         (define dest-file (path-replace-suffix src-file ".png"))
         (define doc (call-with-input-file* src-path read-xml))
         (define new-doc (colorize-svg doc
                                       (hash-ref diffuse-gradient-stops color)
                                       (hash-ref undershine-gradient-stops color)))
         (when new-doc
           (define temp-path (make-temporary-file (format "~a-~~a.svg"
                                                          (path-replace-suffix src-file ""))))
           (dynamic-wind
            (λ () (void))
            (λ ()
              (call-with-output-file* temp-path (λ (out) (write-xml new-doc out))
                                      #:exists 'truncate)
              ((render-icon) temp-path dest-dir dest-file height))
            (λ () (delete-file temp-path))))]
        [else
         (define src-path (build-path src-dir src-file))
         (define dest-dir (build-path src-dir (number->string height)))
         (define dest-file (path-replace-suffix src-file ".png"))
         ((render-icon) src-path dest-dir dest-file height)]))

(define (render-icons dir)
  (printf "Rendering icons in ~a~n" dir)
  (printf "----------------------------------------~n")
  (define files (directory-list dir))
  (define-values (base-dir dir-path _) (split-path dir))
  (define heights (icon-category-heights (path->string dir-path)))
  (for ([file  (in-list files)])
    (define file-path (build-path dir file))
    (cond [(directory-exists? file-path)  (render-icons file-path)]
          [(svg-file? file)
           (for* ([height  (in-list heights)]
                  [color   (in-list icon-colors)])
             (render-icon/color dir file color height))])))

(clean-icons svg-icons-base-path)
(render-icons svg-icons-base-path)
