#lang racket/base

(define config-dir (build-path (find-system-path 'home-dir) ".config/rackit"))
(define config-rkt (build-path config-dir "config.rkt"))
(if (file-exists? config-rkt)
    ((dynamic-require config-rkt 'run) (build-path (current-directory) "rackit.rkt"))
    ((dynamic-require "rackit.rkt" 'run-rackit)))
