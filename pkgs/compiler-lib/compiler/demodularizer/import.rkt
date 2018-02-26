#lang racket/base

(provide (struct-out import))

(struct import (name shape [pos #:mutable]))
