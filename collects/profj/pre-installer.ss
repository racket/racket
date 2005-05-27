(module pre-installer mzscheme
  
  ;copy-jinfos path (list string) -> void
  ;Copies the given jinfos into the compiled directory of the given library
  (define (copy-jinfos path files)
    (let ((compiled-path (build-path path "compiled")))
      (unless (directory-exists? compiled-path) (make-directory compiled-path))
      (for-each (lambda (file)
                  (let* ((f-path (build-path path file))
                         (f-cpath (build-path compiled-path file))
                         (copied? (file-exists? f-cpath)))
                    (cond
                      ((and copied? (file-older-than? f-cpath f-path))
                       (delete-file f-cpath)
                       (copy-file f-path f-cpath))
                      ((not copied?) (copy-file f-path f-cpath)))))
                files)))
  
  ;file-older-than? path path -> bool
  ;Is file-a older than file-b?
  (define (file-older-than? file-a file-b)
    (< (file-or-directory-modify-seconds file-a)
       (file-or-directory-modify-seconds file-b)))
  
  (define (pre-installer plthome)
    (copy-jinfos (collection-path "profj" "libs" "java" "lang") '("Object.jinfo" "String.jinfo" "Throwable.jinfo"
                                                                                 "Comparable.jinfo"))
    (copy-jinfos (collection-path "profj" "libs" "java" "io") '("Serializable.jinfo")))
  (provide pre-installer)
  )