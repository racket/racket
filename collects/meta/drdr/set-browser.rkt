#lang racket

(printf "Setting the default browser to something safe...\n")

; XXX maybe have it call /quit
(put-preferences 
 '(external-browser)
 '(("echo " . "")))
