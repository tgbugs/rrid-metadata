#lang racket/base

(require racket/list racket/dict racket/string racket/pretty sxml)

(provide init-db add-rec dump-recs rrid->record rrid->response)

;; temporary serialization impl

(define (double-quote string)
  (string-append "\"" string "\""))

(define (sxml->json sxml)
  (cond
    ((null? sxml) "")
    ((list? sxml)
     (if (not (null? (cdr sxml)))
         (string-append "[" (string-join (list (sxml->json (car sxml))
                                               (sxml->json (cdr sxml)))
                                         ",")
                        "]")
         (sxml->json (car sxml))))
    ((symbol? sxml) (double-quote (symbol->string sxml)))
    ((number? sxml) (number->string sxml))
    ((string? sxml) (double-quote sxml))
    (#t sxml)
    )
  )

;; database setup
(define (setup-counter)
  (define ic 0)
  (λ () (let ([current-value ic])
          (set! ic (add1 ic)) current-value)))
  
(define (setup-db)

  ; tables, indexes, and sequences
  (define rrid->record-index '((rrid . -1)))
  (define metadata-records '((-1 . record)))
  (define resolve->source-sources '())
  (define counter (setup-counter))

  (define (add-resolve->source source) (set! resolve->source-sources (cons source resolve->source-sources)))

  (define (init-db identifier-sources)
      (dict-for-each identifier-sources
                     (λ (type record)
                       (when (dict-ref record 'resolve->source #f)
                         (set! resolve->source-sources
                               (cons (dict-ref record 'source) resolve->source-sources))))))

  (define (add-rec record to-resolve #:resolve->source [resolve->source #f])
    (let ([current-index (counter)])
      ;(when resolve->source (set! resolve-to-IsDerivedFrom (cons current-index resolve-to-IsDerivedFrom)))
      (for ([id to-resolve]) (set! rrid->record-index (cons (cons id current-index) rrid->record-index)))
      (set! metadata-records (cons `(,current-index . ,record) metadata-records))
      (void)))

  (define (dump-recs) (list metadata-records resolve->source-sources rrid->record-index))

  (define (rrid->index rrid)
    (dict-ref rrid->record-index (if (symbol? rrid) (symbol->string rrid) rrid) #f))

  (define (index->record index)
    (dict-ref metadata-records index null))

  (define (format-record sxml format)
    (cond ((equal? format "xml") (srl:sxml->xml sxml))
          ((equal? format "sxml") (pretty-format sxml))
          ((equal? format "html") (srl:sxml->html sxml))
          ((equal? format "json") (sxml->json sxml))
          (#t sxml)))

  (define (rrid->record rrid #:format [format #f])
    (format-record (index->record (rrid->index rrid)) format))

  (define (rrid->response rrid #:format [format #f])
    (let* ([index (rrid->index rrid)]  ; TODO index->response
           [record (if index
                       (index->record index)
                       `(redirect ,(string-append "http://scicrunch.org/resolver/" rrid)))])  ; fail to SciCrunch FIXME extension
      (if index  ; fun here is that zero does not cast to #f :D so we are safe
          (if (member (car ((txpath "//publisher/text()") record)) resolve->source-sources)
              `(redirect ,(car ((txpath "//relatedIdentifier[contains(@relationType, 'IsDerivedFrom')]/text()") record)))
              `(local ,(format-record record format)))
          record)))  ; TODO failover to SciCrunch resolver for variants

  (list init-db add-rec dump-recs rrid->record rrid->response))

(define database-ops (setup-db))
(define init-db (first database-ops))
(define add-rec (second database-ops))
(define dump-recs (third database-ops))
(define rrid->record (fourth database-ops))
(define rrid->response (fifth database-ops))
