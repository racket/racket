(module text-launch mzscheme
  (require "launch.ss"
           "../web-server.ss")
  (serve)
  (do-not-return))