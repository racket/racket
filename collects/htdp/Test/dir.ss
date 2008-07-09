;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname dir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/dir)

(define current (create-dir "."))
(define teachps (create-dir (string-append "/Users/matthias/plt/" "collects/teachpack/htdp")))

(define current-files (map file-name (dir-files current)))
(define teachps-files (map file-name (dir-files teachps)))

(append
 (map (lambda (x) (format "in Teachpacks, not in Test: ~s" x))
      (filter (lambda (x) (not (member x current-files))) teachps-files))
 (map (lambda (x) (format "in Test, not in  Teachpacks: ~s" x))
      (filter (lambda (x) (not (member x teachps-files))) current-files)))

(check-expect (make-file 'a 1 2) (make-file 'a 1 2))

