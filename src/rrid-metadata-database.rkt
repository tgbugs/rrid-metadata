#lang racket/base

(require racket/dict racket/list sxml)

(provide add-rec dump-recs rrid->record rrid->response)

;; database setup
(define (setup-counter)
  (define ic 0)
  (λ () (let ([current-value ic])
          (set! ic (add1 ic)) current-value)))
  
(define (setup-db)

  ; tables, indexes, and sequences
  (define rrid->record-index '((rrid . -1)))
  (define metadata-records '((-1 . record)))
  (define resolve-to-IsDerivedFrom '(-1))  ; This is the subset of the indexes that should not go to the metadata record but to the originating source (eg ZFIN)
  (define counter (setup-counter))

  (define (add-rec record to-resolve #:resolve->source [resolve->source #f])
    (let ([current-index (counter)])
      (when resolve->source (set! resolve-to-IsDerivedFrom (cons current-index resolve-to-IsDerivedFrom)))
      (for ([id to-resolve]) (set! rrid->record-index (cons (cons id current-index) rrid->record-index)))
      (set! metadata-records (cons `(,current-index . ,record) metadata-records))))

  (define (dump-recs) (list metadata-records rrid->record-index))

  (define (rrid->index rrid)
    (dict-ref rrid->record-index (if (symbol? rrid) (symbol->string rrid) rrid)))

  (define (index->record index #:xml [xml #f])
    (let ([sxml (dict-ref metadata-records index)])
      (if xml
          (srl:sxml->xml sxml)
          sxml)))

  (define (rrid->record rrid #:xml [xml #f])
    (index->record (rrid->index rrid) #:xml xml))

  (define (rrid->response rrid #:xml [xml #f])
    (let* ([index (rrid->index rrid)]  ; TODO index->response
           [record (index->record index #:xml xml)])
      (if (member index resolve-to-IsDerivedFrom)  ; TODO failover to SciCrunch resolver for variants
          (303 (car ((txpath "//relatedIdentifier[contains(@relationType, 'IsDerivedFrom')]/text()") record)))  ; vs 302
          `(200 ,record))))

  (list add-rec dump-recs rrid->record rrid->response))

(define database-ops (setup-db))
(define add-rec (first database-ops))
(define dump-recs (second database-ops))
(define rrid->record (third database-ops))
(define rrid->response (fourth database-ops))
