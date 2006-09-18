(module text-launch mzscheme
  (require "private/launch.ss")
  (serve)
  (semaphore-wait (make-semaphore)))