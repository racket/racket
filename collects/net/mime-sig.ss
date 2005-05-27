(module mime-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide net:mime^)
  
  (define-signature net:mime^
    (
     ;; -- exceptions raised --
     (struct mime-error () -setters (- make-mime-error))
     (struct unexpected-termination (msg) -setters (- make-unexpected-termination))
     (struct missing-multipart-boundary-parameter () -setters
             (- make-missing-multipart-boundary-parameter))
     (struct malformed-multipart-entity (msg) -setters (- make-malformed-multipart-entity))
     (struct empty-mechanism () -setters (- make-empty-mechanism))
     (struct empty-type () -setters (- make-empty-type))
     (struct empty-subtype () -setters (- make-empty-subtype))
     (struct empty-disposition-type () -setters (- make-empty-disposition-type))
     
     ;; -- basic mime structures --
     (struct message (version entity fields))
     (struct entity
             (type subtype charset encoding
                   disposition params id
                   description other fields
                   parts body))
     (struct disposition
             (type filename creation
                   modification read
                   size params))
     
     ;; -- mime methods --
     mime-analyze
     )))
