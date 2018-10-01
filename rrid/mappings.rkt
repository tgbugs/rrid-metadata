#lang racket/base
(require racket/pretty
         (only-in "core.rkt" make-record)
         (only-in srfi/19 string->date date->string)
         "utils.rkt"
         "sources.rkt")

(provide indexes prod-suffix)

;; utility

(define (fix-date str)
  (string->number (date->string (string->date str "~Y~m~dT~H~M~S~z") "~s")))
(module+ test
  (require rackunit)
  (check-= 1536968474 (fix-date "20180914T234114+0000") 0))

;; mappings

(define (ia-es-mapping res) "TODO")

(define (scr-es-mapping res)
  ;(pretty-write res)
  (make-record 'digital
               (hr res '_source 'item 'identifier)
               ; FIXME can't easily pass these in 1 at a time ...
               (car (map (λ (h) (hr h 'identifier)) (hr res '_source 'item 'alternateIdentifiers)))
               #:record
               (scr-rec #:title (hr res '_source 'item 'name)
                        #:submitted (fix-date (car (hr res '_source 'provenance 'creationDate)))  ; FIXME get from the actual db
                        #:updated (fix-date (hr res '_source 'provenance 'ingestTime))    ; FIXME get from the actual db
                        #:resourceType "TODO"  ; Database ... etc.
                        #:resourceTypeGeneral "TODO"  ; Service or Software  (Documentation vs Protocol)
                        #:keywords (map (λ (h) (hr h 'keyword)) (hr res '_source 'item 'keywords))
                        #:url (car (map (λ (c) (hr c 'uri)) (hr res '_source 'distributions 'current)))  ; FIXME
                        #:resource-types (map (λ (t) (hr t 'name)) (hr res '_source 'item 'types))  ; FIXME these need to be used to translate to resource type
                        #:description (hr res '_source 'item 'description)
                        #:synonyms (filter (λ (s) (not (equal? s "")))  ; TODO warn on this
                                           (map (λ (s) (hr s 'name)) (hr res '_source 'item 'synonyms)))
                        )))

(define (abr-es-mapping res) "TODO")

(define (cl-es-mapping res) "TODO")

(define (bs-es-mapping res) "TODO")

;; index

(define prod-suffix "_prod")

(define indexes (hasheq "scr_001421" ia-es-mapping
                        "scr_005400" scr-es-mapping
                        "scr_006397" abr-es-mapping
                        "scr_013869" cl-es-mapping
                        ; doesn't exist yet
                        ;"scr_004854" bs-es-mapping
                        ))

