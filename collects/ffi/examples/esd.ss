#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

(require (lib "esd.ss" "ffi"))

(printf "default = ~s\n" (default-esd))
(printf "latency = ~s\n" (esd-get-latency))
(printf "standby -> ~s\n" (esd-standby))
(sleep 1)
(printf "resume  -> ~s\n" (esd-resume))

(printf "Normal play...\n")
(esd-play-file "esd.ss" "~/stuff/sounds/Eeeooop.wav" #t)
(sleep 1)

(printf "Sample play...\n")
(let ((sample-id (esd-file-cache "foooo" "~/stuff/sounds/Eeeooop.wav")))
  (printf ">>> sample = ~s\n" sample-id)
  (printf ">>> getid -> ~s\n"
          (esd-sample-getid "foooo:/home/eli/stuff/sounds/Eeeooop.wav"))
  (printf "playing...\n")
  (esd-sample-play sample-id)
  (sleep 1)
  (printf "looping...\n")
  (esd-sample-loop sample-id)
  (sleep 3)
  (printf "enough!\n")
  (esd-sample-stop sample-id)
  (sleep 1)
  (printf "bye.\n"))
