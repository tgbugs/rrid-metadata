#lang racket

(require sxml ; atm only for the jats bit
 (for-syntax syntax/parse))

(provide make-gtr! set-add-rec! make-record schema-url check-schema)

;; macros
(define-syntax (bind stx)  ; FIXME could be called (node stx) or something? make-sxml-node
  (syntax-parse stx
    [(_bind variable-stx)
     #'(quasiquote (variable-stx ,variable-stx))]))

;; utility

(define (filter-empty lst)
  (filter (λ (l) (not (empty? l))) lst))

(define (make-global namespace fragment)
  (string-append namespace (symbol->string fragment)))


;; record structure

; current schema url version
(define schema-url "http://scicrunch.org/resources/schema/rrid-core-0")

;(define record-structure
; may have more than one optional...
; wildcard element -> * ?
; _ binds to the child-type of the parent
; eq? in child predicate will do an exact test against the listed next
; cars that are lists is where to start... square brackets help clarify elements
; will probably make parsing easier too
; i think we can make type? optional when it is list? and null?
; ie [symbol? range? type?] => type? -> list?
; and [_ range? type?] => type? -> null? since [_ 1] clearly indicates null? in all cases
; do we need null? at the end of [("member" "check" "list") 1]???
; should be able to lift this into a constraint checker...
; TODO is it possible to generate our ingest/conversion code from this? not entirely clear...
;  maybe could generate using an alternate parser from additional annotations to more subtrees?
; the right way to imlement this feels like using a macro to convert [] into consumer functions...

(define schema-structure
#|
structure validation syntax
    element : ssexp symbol
    symbol : SYMBOL
    ssexp : '( symbol-restrict ssexp ') | racket-sexp
    symbol-restrict : '[ u-sym-m int-or-range child-is-pred* (#:warn-missing expected-sub-tree-patterns)* ']
    u-sym-m : _ | symbol | pattern-expression
    int-or-range : INTEGER | range-expression
    range-expression : '( 'range 0-1 int>prev-or-n ')
    0-1 : 0 | 1
    int>prev-or-n : INTEGER | n  ; must test INTEGER > 0-1
    child-is-pred : racket-predicate  ; must test explicitly whether absense is valid in context
|#
'([*TOP* 1]
  ([@ (range 0 1)]
   ([*NAMESPACES* (range 0 1)]
    ([(pattern *) (range 0 n) uri?] [_ 1])))
  ([resource 1]
   ([@ (range 0 1)]
    ([xmlns 1 uri?] [_ 1])  ; FIXME eq? schem-url?
    ([(pattern xmlns:*) (range 0 n) uri?] [_ 1]))
   ([identifier 1 rrid?]
    ([@ 1 equal?] (identifierType "RRID"))
    [_ 1])
   ([properCitation 1 string?]  ; TODO pc validator
    ([@ 1 equal?] (render-as "Proper Citation") (type "Inline Text Citation"))
    [_ 1])
   ([titles 1]
    ([title (range 1 n) string?]
     ([@ (range 0 1)] ([xml:lang (range 0 1) xml-lang?] [_ 1]))
     [_ 1]))
   ([publisher 1 string?] [_ 1])
   ([description (range 0 1) string?]
    ([@ (range 0 1)] ([xml:lang (range 0 1) xml-lang?] [_ 1]))
    [_ 1])
   ([subjects (range 0 1)]
    ([subject (range 0 n) string?]
     ([@ (range 0 1)] ([xml:lang (range 0 1) xml-lang?] [_ 1]))
     [_ 1]))
   ([contributors (range 0 1)]  ; list? vs implicit 'only what we list below is allowed'
    ([contributor (range 0 n)]
     ([@ 1]
      ([contributorType 1 string?] [_ 1]))  ; member?
     ([contributorName 1 string?] [_ 1])))
   ([dates 1]
    ([date (range 1 n) iso8601-tz-string? #:warn-missing ((@ (dateType "Updated"))
                                                       (@ (dateType "Submitted")))]
     ([@ 1] ([dateType 1 member] [("Submitted"
                                   "Updated") 1 null?]))  ; TODO
     [_ 1]))
   ([resourceType 1 string?]
    ([@ 1]
     ([resourceTypeGeneral 1 member] [("Material"
                                       "Software"
                                       "Services") 1 null?]))  ; TODO
    [_ 1])
   ([alternateIdentifiers 1]
    ([alternateIdentifier (range 1 n) (match-@-value alternateIdentifierType)]
     ([@ 1] ([alternateIdentifierType 1 string?] [_ 1]))
     [_ 1]))
   ([relatedIdentifiers 1 #:warn-missing ((@ (relationType "IsCompiledBy"))
                                          (@ (relationType "IsIdenticalTo"))
                                          (@ (relationType "IsDerivedFrom")))]
    ([relatedIdentifier (range 1 n) (match-@-value relatedIdentifierType)]
     ([@ 1]
      ([relatedIdentifierType 1 member] [("URL" "DOI") 1 null?])
      ([relationType 1 member] [,relation-types 1 null?])  ; TODO
      ([resourceTypeGeneral 1 member] [,resource-type-generals 1 null?]))  ; TODO
     [_ 1]))))
)

(define (check-schema sxml)
  (define (check-subtree sub-sxml sub-structure [current null])
    ; repeat (car (cdr (cdr (cdr ...
    ; head
    ; rest
    (displayln `(xhead ,(car sub-sxml)))
    (displayln `(head, (car sub-structure)))
    (let* ([head (car sub-structure)]
           [rest (cdr sub-structure)]
           [node-name (car head)]
           [node-count (cdr head)]
           [expect-type (cond ((not (null? (cddr head))) (cddr head))
                              ((eq? node-name '_) null?)
                              (#t list?))])
      (displayln `(-> ,node-name ,node-count ,expect-type))
      (displayln (eq? node-name (car sub-sxml)))
      ; validate the current level
      (cond ((equal? (car head) '@) (check-subtree (cdr sub-sxml) rest))
            ((list? rest) (check-subtree (cdr sub-sxml) rest))
            (#t null))
      ;(if rest
          ;(check-subtree (cdr sub-sxml) rest)  ; FIXME need to hit caddr cadddr etc...
          ;null)))
      ))
  (check-subtree sxml schema-structure))

(define (record-format
         #:id primary-id
         #:altids altids
         #:relids relids
         #:pc proper-citation
         #:jc jats-citation
         #:submitted submitted
         #:updated updated
         #:titles titles
         #:creators creators
         #:contributors contributors
         #:resourceType resourceType
         #:description description
         #:subjects subjects
         #:publisher publisher)
         ;#:type-specific-record type-specific-record)
  ; marginally conformant to datacite metadata
  ; note that we are closer to a physical sample use case than a publishing use case
  `(*TOP*
    ;(*PI* xml-stylesheet "href=\"#style\" type=\"text/css\"");(@ (href "#style") (type "text/css")))
    (@ (*NAMESPACES* (rridType "http://scicrunch.org/resources/schema/rrid-type-0")))
    ,(filter-empty
      `(resource
        ;(extras (@ (id "style"))  ; this doesn't really work as desired
                ;"resource { display: block; }
                 ;title, publisher { display: block; }
                 ;extras { display: none; }
                ;")
        (@ (xmlns ,schema-url))
        (identifier (@ (identifierType "RRID")) ,(string-append "RRID:" (symbol->string primary-id)))
        (properCitation (@ (render-as "Proper Citation")  ; XXX RRID addition
                           (type "Inline Text Citation"))
                        ,proper-citation)
        ;,creators  ; nameType Organizational for corporate...
        ,titles
        ,(bind publisher)
        ;(publicationYear , year)  ; not clear that this can be manditory nor that it should be
        ,description  ; descriptionType
        ,subjects
        ,contributors  ; used with Distributor for vendors, a bit of overloading
        (dates
         (date (@ (dateType "Submitted")) ,submitted)
         (date (@ (dateType "Updated")) ,updated))  ; TODO created as well
        ;(language "en-US") ; XXX NO
        ,resourceType ;,ResourceType ResourceTypeGeneral  one way to do would be to declare non-equivalence with datacite on these elements let xmlns deal w/ rest probably do need a really clear distinction between "this resource is a real thing" beyond the source iri type...
        ,altids
        ,relids
        ;size XXX NO
        ;formats XXX not applicate
        ;,version  ; not clear that we can really have this for these kinds of resources...
        ;,rights  ; possibly for some software resources... Commercial EULA vs License issue good thing they deal with it
        ;,geoLocations  ; probably optional for this one since our resources are various an muliplicious (woo mice)
        ;,fundingReferences  ; eventually

        ;(JATSCitation (@ (render-as "JATS Citation")  ; TODO not happy with the rendering for this... with the &lt; &gt;
                         ;(type "Typeset SGML Citation"))
                      ;,jats-citation)
        ))))

(define (attrs tags values)
  (cons '|@| (for/list ([tag tags]
                           [value values]
                           #:when value)
               `(,tag ,value))))

(define (title-format title [titleType #f])
  ;(define tags (list '|@| (xml:lang "en-US") ()))

  `(title
    ,(attrs '(xml:lang titleType)
            `("en-US" ,titleType))
    ,title))
  ;(let ([out (cons 'titles (for/list ([t titles]) `(title (@ (xml:lang "en-US")) ,t)))])
    ;(pretty-write titles)
    ;(pretty-write out)
    ;out))

(define (altids-format alternateIdentifierType id) `(alternateIdentifier
                                                     (@ ,(bind alternateIdentifierType))
                                                     ,id))

(define (relids-format id type [relation #f] [rtg #f])
  `(relatedIdentifier ,(attrs '(relatedIdentifierType relationType resourceTypeGeneral)
                              `(,type ,relation ,rtg))
                      ,id))

(define (jats-format prefix rrid)
  ; http://groups.niso.org/apps/group_public/view_comment.php?comment_id=766
  (srl:sxml->xml
   `(research-support-source
                   (@ (support-type "research-materials"))
                   (research-resource-wrap
                    (research-resource ,prefix
                                       (research-resource-id
                                        (@ (resource-id-type "rrid") (vocab "Research Resource Identifier"))
                                        ,rrid))))
   )
  )

(define (subjects-format subjects)
  (let ([out (cons 'subjects (for/list ([s subjects]) `(subject (@ (xml:lang "en-US")) ,s)))])
    ;(pretty-write subjects)
    ;(pretty-write out)
    out))

(define (contrib-format contributorType contributorName)  ; TODO binding macro
  `(contributor (@ ,(bind contributorType)) ,(bind contributorName)))

(define gtr (void))
(define (set-gtr! function)
  (set! gtr function))
(define add-rec (λ (a b) a))
(define (set-add-rec! function)
  (set! add-rec function))
(define (make-record identifier-type primary-id #:record type-record . alternate-ids)

  ; when treated as a qname `@prefix RRID: <http://scicrunch.org/resolver/> .`
  ; FIXME the semantics of the resolver are extremely confusing because we conflate SCR: and RRID: :/ what to do?
  ;     RRID: -> http://scicrunch.org/resolver/RRID:  but could end up with people inserting RRID:RRID: >_<
  ;     RRID: -> http://scicrunch.org/resolver/
  ;      SCR: -> http://uri.scicrunch.org/default/SCR_1234567

  (define (type-dict-or-record key)
    (let ([dict-result (gtr identifier-type key #f)])
      (if dict-result
          dict-result
          (dict-ref type-record key))))

  (define qname (string-append "RRID:" (symbol->string primary-id)))

  ;(set! type-record (cons (cons 'identifier qname) type-record))
  (dict-set! type-record 'identifier qname)
  ;(pretty-write type-record)

  (define to-resolve (map (λ (id) (format "RRID:~a" id))
                          (cons primary-id alternate-ids)))

  (define altids
    (let* ([res (for/list ([id (cdr to-resolve)]) (list "RRID" id))]
           [from-tr (dict-ref type-record 'altids '())]
           [all (append res from-tr)])
      ;[mapl (λ (lst)
      ;(map (λ (l) (apply altids-format l))
      ;(let ([type (car l)][id (cadr l)])
      ;(altids-format type id)))
      ;lst))] )
      (if (empty? all)
          all
          (cons 'alternateIdentifiers (map (λ (l) (apply altids-format l)) all)))))

  (define resourceTypeGeneral (type-dict-or-record 'resourceTypeGeneral))

  (define relids
    (let ([rids (dict-ref type-record 'relids #f)]
          [base `(
                  (,(string-append "http://scicrunch.org/resolver/" qname)
                   "URL" "IsIdenticalTo" ,resourceTypeGeneral)
                  (,(string-append "http://n2t.net/" qname)
                   "URL" "IsIdenticalTo" ,resourceTypeGeneral)
                  (,(string-append "http://identifiers.org/" qname)
                   "URL" "IsIdenticalTo" ,resourceTypeGeneral)
                  (,(make-global (gtr identifier-type 'namespace) primary-id)
                   "URL" "IsDerivedFrom" ,resourceTypeGeneral)
                  (,(gtr identifier-type 'url)
                   "URL" "IsCompiledBy" "Service")
                  ; TODO supports IsCitedBy
                  ; TODO HasMetadata -> our secondary record
                  )])
      ;(displayln (append base rids))
      ;(displayln rids)
      (cons 'relatedIdentifiers (map (λ (l) (apply relids-format l))
                                     (if rids
                                         (append base rids)
                                         base)))))

  (define proper-citation
    (let ([to-format (gtr identifier-type 'format)])
      (apply format (cons (car to-format)
                          (map (λ (f)
                                 (dict-ref type-record f))
                               (cdr to-format))))))

  (define jats-citation (jats-format
                         (car (string-split (string-trim proper-citation "(") ","))
                         qname))

  (define issuing-source (gtr identifier-type 'source))

  ;(define issuing-url (format (dict-ref type-urls identifier-type) primary-id))

  (define titles (cons 'titles
                       (let ([title (dict-ref type-record 'title)])
                         (if (dict-has-key? type-record 'synonyms)
                             (map (λ (v) (title-format v "AlternativeTitle"))
                                  (sort (dict-ref type-record 'synonyms) string<?))
                             `(,(title-format title))))))
  (define creators '())  ; TODO

  (define subjects
    (if (dict-has-key? type-record 'subjects)
        (subjects-format (dict-ref type-record 'subjects))
        '()))
  ;(pretty-write subjects)

  (define contributors
    (let ([contrib-list (let ([v (dict-ref type-record 'contributors #f)])
                          (cond (v v)
                                (#t '())))])
      (if (empty? contrib-list)
          '()
          (cons 'contributors (map (λ (l) (apply contrib-format l)) contrib-list)))))

  (define type-specific-record (lambda () ;do not run right now...
                                 (cons (string->symbol (format "rridType:~s" identifier-type))
                                       (cons `(identifier (@ (type "local")) ,(symbol->string primary-id))  ; (type "fragment")
                                             (append
                                              (for/list ([f (gtr identifier-type 'fields)])
                                                (cons f
                                                      (let ([rec (dict-ref type-record f)])
                                                        (if (list? rec)
                                                            (let ([str (symbol->string f)])
                                                              (if (string-suffix? str "s")
                                                                  (for/list ([r rec])
                                                                    (list (string->symbol (string-trim str "s" #:left? #f))
                                                                          r))
                                                                  (error (format "Your tag ~a is being used for listing things but doesn't end in s!" f))))
                                                            (list rec)))))
                                              ; FIXME provide a link to the formatting rules doc instead
                                              `((citationFormat ,(format "~s" (gtr identifier-type 'format)))))))))
  (let ([record (record-format
                 #:id primary-id
                 #:altids altids
                 #:relids relids
                 #:pc proper-citation
                 #:jc jats-citation
                 #:submitted (dict-ref type-record 'submitted)
                 #:updated (dict-ref type-record 'updated)
                 #:titles titles
                 #:creators creators
                 #:contributors contributors
                 #:resourceType `(resourceType (@ ,(bind resourceTypeGeneral))
                                               ,(type-dict-or-record 'resourceType))  ; this pattern is really easy to lift if we need
                 #:description (let ([out (dict-ref type-record 'description #f)])
                                 (if out `(description (@ (xml:lang "en-US")) ,out) '()))
                 #:subjects subjects
                 #:publisher issuing-source)])
    (add-rec record to-resolve)))

(define (make-gtr! identifier-sources)  ; TODO further decoupling but not now
  (define no-arg (gensym))
  (define (gtr type field [fail no-arg])
    "gtr
     get-type-record
     Pass to make-make-record"
    (let ([source-record (dict-ref identifier-sources type)])
      (dict-ref source-record field (if (eq? fail no-arg)
                                      (λ () (raise-mismatch-error 'dict-ref
                                                                  (format "no value for key: ~e in: " field)
                                                                  source-record))
                                      fail))))
  (set-gtr! gtr)
  (void))

(module+ test
  (require "rrid-metadata-database.rkt")
  (require "sources.rkt")
  (make-gtr! identifier-sources)
  ;(set-add-rec! add-rec)  ; FIXME this is a really really terrible way to handle this :/
  ; but I don't want to have to rewrite the stupid function signature :/
  (check-schema (make-record 'fake
                             'PREFIX_1234567
                             #:record
                             (fake-rec
                              #:title "Fake resource"
                              #:insert_time 1111111111
                              #:curate_time 1222222222
                              #:something "We need this for the proper citation"))))
