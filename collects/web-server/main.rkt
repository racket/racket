#lang scheme/base
(require "private/launch.ss"
         (only-in "web-server.ss" do-not-return))
(void (serve))
(do-not-return)
