(module launch-text mzscheme
  (require "launch.ss"
           (only "../web-server.ss" do-not-return))
  (serve)
  (do-not-return))