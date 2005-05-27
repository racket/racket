; requires empty (from a teaching language)
; requires servlet2.ss and dir.ss teachpacks

(define my-dir (make-dir "a-fake-dir-name" empty empty))

(final-page (dir-name my-dir))