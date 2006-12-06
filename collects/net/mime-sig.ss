(module mime-sig (lib "a-signature.ss")
  ;; -- exceptions raised --
  (struct mime-error () -setters -constructor)
  (struct unexpected-termination (msg) -setters -constructor)
  (struct missing-multipart-boundary-parameter () -setters -constructor)
  (struct malformed-multipart-entity (msg) -setters -constructor)
  (struct empty-mechanism () -setters -constructor)
  (struct empty-type () -setters -constructor)
  (struct empty-subtype () -setters -constructor)
  (struct empty-disposition-type () -setters -constructor)

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
  )
