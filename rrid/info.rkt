#lang info

(define collection "rrid")
(define version "0.0.1")
(define deps '("base"
               "compatibility-lib"
               "sxml"
               "web-server-lib"))

(define pkg-desc "Reference RRID metadata validator and resolver")

(define racket-launcher-names '("rrid-resolver"))
(define racket-launcher-libraries '("resolver.rkt"))

(define pkg-authors '(tgbugs))
