#lang scheme/base
(require "launch.ss"
         (only-in "../web-server.ss" do-not-return))
(serve)
(do-not-return)