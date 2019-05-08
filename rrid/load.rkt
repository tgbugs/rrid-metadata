#lang racket/base
(require racket/file
         racket/list
         racket/path
         racket/pretty
         racket/string
         json
         (prefix-in es/ elasticsearch)
         elasticsearch/utils
         (only-in sxml/sxpath sxpath)
         "core.rkt"
         "utils.rkt"
         "sources.rkt"
         "mappings.rkt"
         "database.rkt"
         "xml-xexpr.rkt")

;; pull data from elasticsearch to create rrid records
;; additional datasources may be needed in the future

(define save-location (build-path "/tmp" "rrid"))

(define (get-es-connection)
  (define ES-USERNAME (getenv "ES_USERNAME"))
  (define ES-PASSWORD (getenv "ES_PASSWORD"))
  (define ES-HOST (if (getenv "ES_HOST") (getenv "ES_HOST") "localhost"))
  (define ES-PORT (if (getenv "ES_PORT") (getenv "ES_PORT") 9200))
  (define es (es/client ES-HOST ES-PORT #t ES-USERNAME ES-PASSWORD))
  es)

(define es (make-parameter #f))

(define (fetch-rrid index)
  (es/hits
   (es/search (es)
              (hash 'query (hash 'match_all
                                 (hash))
                    'size 100
                    )
              #:index index
              #:doctype "")))

(define (search index jsexpr)
  (parameterize ([es (get-es-connection)])
    ;(es/hits
     (es/search (es)
                jsexpr
                #:index index
                #:doctype "rin")))
;)

(module+ test-search
  (define index "RIN_Organism_prod")
  (define query (hash 'query (hash 'match
                                   (hash 'message (hash 'query "C57BL/6J")))
                      'size 100))
  (define query2 (hash 'query (hash 'match_all (hash)) 'size 100))
  (define query3 (hash 'query (hash 'multi_match
                                    (hash 'query "C57BL/6J"
                                          'type "phrase"
                                          'fields '("name")
                                          ))))
  (define query4 (hash 'query (hash 'simple_query_string
                                    (hash 'query "C57BL/6J"
                                          ;'fields '("*")
                                          'fields '("item.name")
                                          'default_operator "and"
                                          ))
                       'size 100))
  (define res (search index query4))
  ;(hrm res hits hits / #:keys)
  ;(hrm res hits hits / _source item name)
  )

(define (get-all index #:doctype [doctype ""])
  (define (scroll-id res)
    (hr res '_scroll_id))
  (define path (build-path-string index doctype "_search?scroll=1m"))  ; scroll is in time ...
  (define (scroll id accum)  ; probably not the best pattern if we want to stream
    (define path (build-path-string "_search" "scroll"))
    (let* ([res (es/do-request (es) 'POST path
                               (hash 'scroll "1m"
                                     'scroll_id id))]
           [hits (es/hits res)])

      (if (null? hits)
          accum
          (scroll (scroll-id res) (append hits accum)))))

  (let ([fres (es/do-request (es) 'POST path
                             (hash 'query (hash 'match_all (hash))
                                   'size 10000))])
    (scroll (scroll-id fres) (es/hits fres))))

(module+ test-get-all
  (init-db identifier-sources)
  (set-add-rec! add-rec)
  (make-gtr! identifier-sources)

  (for-each make-directory*
            (map (λ (ext) (build-path save-location ext)) '("xml" "sxml")))

  (define registry
    (parameterize ([es (get-es-connection)])
      (get-all "scr_005400_prod")))

  ; FIXME this will fail with an extremely cryptic error if the database has not been initialized
  (define r (proc-recs! registry (hash-ref indexes "scr_005400"))))

(define (get-recs id get-func)
  (get-func (string-append id prod-suffix)))

(define (proc-recs! hits es-mapping!)
  (for-each es-mapping! hits)
  (define recs (cdr (hash-values (car (dump-recs)))))
  (for-each write-rec recs))

(define (make-records! [get-func fetch-rrid] #:id [source-id #f])
  (if source-id
      (proc-recs! (get-recs source-id get-func) (hash-ref indexes source-id))
      (hash-for-each indexes (λ (id es-func) (proc-recs! (get-recs id get-func) es-func))))
  (void))

(define (write-rec rec #:location [location save-location])
  (let ([stem (car ((sxpath "/resource/identifier/text()") rec))])
    (save-sxml rec (build-path location "xml" (string-append stem ".xml")))
    (save-sexp rec (build-path location "sxml" (string-append stem ".sxml")))))

(module+ test
  (init-db identifier-sources)
  (set-add-rec! add-rec)
  (make-gtr! identifier-sources)

  (for-each make-directory*
            (map (λ (ext) (build-path save-location ext)) '("xml" "sxml")))

  (parameterize ([es (get-es-connection)])
    (make-records! #:id "scr_005400")
    (make-records!)))

(module+ main-
  (init-db identifier-sources)
  (set-add-rec! add-rec)
  (make-gtr! identifier-sources)

  (for-each make-directory*
            (map (λ (ext) (build-path save-location ext)) '("xml" "sxml")))

  (parameterize ([es (get-es-connection)])
    (make-records! get-all #:id "scr_005400")))
