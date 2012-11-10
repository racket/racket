#lang racket/base

(provide
 ;; map the directory tree at the given path into a data representation according to model 3 of 
 ;; HtDP/1e (part III) and HtDP/2e (Part IV); 
 ;; effects: if a directory d isn't accessible, the function prints (inaccessible d) to stdout
 create-dir ; String -> Dir.v3
 
 ; structure 
 dir? make-dir dir-name dir-dirs dir-files
 
 ; structure 
 file? make-file file-name file-content file-size
 )

;; ---------------------------------------------------------------------------------------------------

(require htdp/error lang/prim (only-in racket/base [file-size s:file-size]))

;; Structures: 
(define-struct dir (name dirs files) #:transparent)
(define-struct file (name size content) #:transparent)

(define-primitive create-dir create-dir/proc)

;; Data:
;; Directory  = (make-dir Symbol (listof Dir) (listof File))
;; File       = (make-file Symbol Number (union '() X))

(define (create-dir/proc a-path)
  (check-arg 'create-dir (string? a-path) "string" "first" a-path)
  (let ([a-path! (string->path a-path)])
    (if (directory-exists? a-path!)
        (car (explore (list a-path!)))
        (error 'create-dir "not a directory: ~e" a-path))))

;; explore : (listof String[directory-names]) -> (listof Directory)
(define (explore dirs)
  (map (lambda (d) 
         (let-values ([(fs ds) (pushd d directory-files&directories)]) 
           (make-dir
            (string->symbol (path->string (my-split-path d)))
            (explore (map (lambda (x) (build-path d x)) ds))
            (map make-file
                 (map (compose string->symbol path->string) fs)
                 (map (lambda (x) (if (file-exists? x) (s:file-size x) 0))
                      (map (lambda (x) (build-path d x)) fs))
                 (map (lambda (x) "") fs)))))
       dirs))

;; String -> String
(define (my-split-path d)
  (let-values ([(base name mbd?) (split-path d)])
    (if (string? base) name d)))

;; pushd : String[directory-name] (-> X) -> X
(define (pushd d f)
  (parameterize ([current-directory d])
    (f)))

;; directory-files&directories : 
;;  (-> (values (listof String[file-names]) (listof String[directory-names])))
(define (directory-files&directories)
  (with-handlers ((exn:fail:filesystem? 
                   (lambda (x)
                     (displayln `(inaccessible ,(current-directory)))
                     (values '() '()))))
    (let ((contents (directory-list)))
      (values
       (filter (lambda (x) (or (file-exists? x) (link-exists? x))) contents)
       (filter (lambda (x) (and (directory-exists? x) (not (link-exists? x))))
               contents)))))

;; get-file-content : file -> (int -> string)
;; to read a file on demand as a string
;; option to expand the library ... 
;; cache it ... 
(define (get-file-content f)
  (read-string (file-size f) (open-input-file (symbol->string (file-name f)))))
