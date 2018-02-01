#lang racket

(require sxml ; atm only for the jats bit
 (for-syntax syntax/parse))

(provide make-make-record schema-url)

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

(define (make-make-record identifier-sources add-rec)  ; TODO further decoupling but not now
  (define no-arg (gensym))
  (define (gtr type field [fail no-arg])
    "Pass to make-make-record"
    (let ([source-record (dict-ref identifier-sources type)])
      (dict-ref source-record field (if (eq? fail no-arg)
                                      (λ () (raise-mismatch-error 'dict-ref
                                                                  (format "no value for key: ~e in: " field)
                                                                  source-record))
                                      fail))))

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
                   `((citationFormat ,(format "~s" (gtr identifier-type 'format))))  ; FIXME provide a link to the formatting rules doc instead
                   )))))
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
      (add-rec record to-resolve)
      (void)));record))  ; return void here?
  make-record)

