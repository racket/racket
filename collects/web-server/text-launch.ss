
(module text-launch mzscheme
  (require "launch.ss")

  (serve)

  (semaphore-wait (make-semaphore)))
