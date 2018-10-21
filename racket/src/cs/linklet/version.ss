(define version-bytes (string->bytes/utf-8 (version)))
(define vm-bytes (string->bytes/utf-8 (symbol->string (system-type 'vm))))
