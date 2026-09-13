#lang racket/base

(provide (struct-out import))

(struct import (name           ; name used in import context
                path/submod+phase
                src-ext-name)) ; linklet exported name
