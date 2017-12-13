#lang racket/base

(require racket/dict sxml)

(provide add-rec dump-recs rec-by-rrid)

;; database setup
(define (setup-counter)
  (define ic 0)
  (λ () (let ([current-value ic])
          (set! ic (add1 ic)) current-value)))
  
(define (setup-db)
  (define index-rrid->record '((rrid . -1)))
  (define metadata-records '((-1 . record)))
  (define counter (setup-counter))
  (define (add-rec record to-resolve)
    (let ([current-index (counter)])
      (for ([id to-resolve]) (set! index-rrid->record (cons (cons id current-index) index-rrid->record)))
      (set! metadata-records (cons `(,current-index . ,record) metadata-records))))
  (define (dump-recs) (list metadata-records index-rrid->record))
  (define (rec-by-rrid rrid #:xml [xml #f])
    (let ([sxml (dict-ref metadata-records (dict-ref index-rrid->record (if (symbol? rrid)
                                                                            (symbol->string rrid)
                                                                            rrid)))])
      (if xml
          (srl:sxml->xml sxml)
          sxml)))
  (list add-rec dump-recs rec-by-rrid))

(define ar-dr-rr (setup-db))
;(displayln ar-dr-rr)
(define add-rec (car ar-dr-rr))
(define dump-recs (cadr ar-dr-rr))
(define rec-by-rrid (caddr ar-dr-rr))

