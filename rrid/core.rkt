#lang racket

(require sxml ; atm only for the jats bit
 (for-syntax syntax/parse))

(provide make-gtr! set-add-rec! make-record schema-url check-schema)

;; macros
(define-syntax (bind stx)  ; FIXME could be called (node stx) or something? make-sxml-node
  (syntax-parse stx
    [(_bind variable-stx)
     #'(quasiquote (variable-stx ,variable-stx))]))

(module syntax-classes racket/base
  (require syntax/parse (for-syntax racket/base syntax/parse))
  (provide (all-defined-out))

  (module derp racket/base
    (require syntax/parse)
    (provide (all-defined-out))
    
    (module dderp racket/base
      (require syntax/parse)
      (provide (all-defined-out))
      (define-syntax-class sc-exact-pat
        ; TODO validating the validation schema means that we should warn if
        ; and exact-cdr-member doesn't match anything
        ; FIXME vs warn-missing on literal leaves? or is that not relevant
        (pattern (name body:expr ...))))
    (require 'dderp (for-syntax 'dderp))
    (provide (all-from-out 'dderp))

    (define-syntax-class blank-any
      #:literals (_)
      (pattern _))

    (define-syntax-class any-number
      #:literals (n)
      (pattern n))

    (define-syntax-class sc-name-pat
      #:literals (pattern)
      (pattern (pattern name-pattern:id))) 

    (define-syntax-class sc-count
      #:literals (range)
      (pattern (range start:exact-nonnegative-integer stop:exact-positive-integer))
      (pattern (range start:exact-nonnegative-integer stop:any-number))) 

    (define-syntax-class sc-restr
      (pattern ([(~optional (~seq #:warn on-count:exact-nonnegative-integer))
                 subtree:sc-exact-pat count-spec:sc-count] ...)))
    
    (define-syntax-class sc-head
      (pattern [(~or* name:id name-pat:sc-name-pat)
                count-spec:sc-count
                (~optional (~seq #:restrictions restriction:sc-restr))]))

    (define-syntax-class sc-pred
      (pattern ([name:id (~or* function:expr function:id)] ...)))

    (define-syntax-class sc-string-pred
      (pattern ([string-value:string predicate:id] ...))))

  (require 'derp (for-syntax 'derp))
  (provide (all-from-out 'derp))
  
  (define-syntax-class sc-terminal
    #:literals (->?)  ; predicate from sibbling path value

    (pattern (~or* exact-value:string exact-value:integer))
    (pattern predicate:id)
    (pattern (->? subtree:sc-exact-pat))  ; make the predicate at compile time?
    )

  (define-syntax-class sc-body
    (pattern (head:sc-head body:sc-body ... terminal:sc-terminal)))


  (define-syntax-class sc-schema
    (pattern (head:sc-head body:sc-body ... (~optional terminal:sc-terminal))))
)

(require 'syntax-classes (for-syntax 'syntax-classes))

(define-syntax (sxml-schema stx)  ; more spec-tree-structure
  (syntax-parse stx
    [(_ (~optional (~seq #:predicates predicate-let:sc-pred))
        (~optional (~seq #:string->predicate string-let:sc-string-pred))
        schema:sc-schema)
    ;[(_ ([node-name this-node-count-spec (~optional predicate)] body:spec-node ...))
     #''(WE MADE IT BOYS!)]))



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
; might be possible to use a reader macro ala #str([*TOP* 1] ...) as a way to define these...

; see https://lexi-lambda.github.io/blog/2017/08/12/user-programmable-infix-operators-in-racket/

(require (for-syntax syntax/parse/class/paren-shape)
         (prefix-in racket/base/ racket/base)
         syntax/parse/define)

(define-syntax (wrapped stx)
  ; this is not working yet... (current does nothing)
  (syntax-parse stx
    [(_nil quote-stx) #'(quote-stx)]))

(define-syntax-parser define-schema-structure
  [[~brackets _ arg ...]
   #'(#%node-spec arg ...)]
  ;[(~parens _ arg ...)
   ;#'(list arg ...)]
  )

;(define-schema-structure [*TOP* 1])
;(define-schema-structure '([*TOP* 1] "yes"))

(define (#%node-spec node-name range-expression [expect-type list?] #:warn-missing expect-patterns)
  (define (checker sexp-tree)
    #t)  ; TODO
  checker)

;(define-syntax-parser #%node-spec
  ;[(_ a b c)])

(define schema-structure ;(define-schema-structure  ; TODO hrm...
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
  (sxml-schema
  #:predicates ([resource-type-general? (λ (value) (member value resource-type-gemerals))]
                [relation-type? (λ (value) (member value relation-types))]
                [xml-lang? (λ (value) (member value xml-langs))])
  #:string->predicate (["DOI" doi?] ["RRID" rrid?] ["URL" url?])
([*TOP* 1]
  ([@ (range 0 1)]
   ([*NAMESPACES* (range 0 1)]
    ([(pattern *) (range 0 n)] uri?)))
  ([resource 1]
   ([@ (range 0 1)]
    ([xmlns 1] rrid?)  ; FIXME eq? schem-url?
    ([(pattern xmlns:*) (range 0 n)] uri?))
   ([identifier 1]
    ;([@ 1 equal?] (identifierType "RRID"))
    ;(@ (identifierType "RRID"))
    ;([@ 1 equal?] ([identifierType 1 equal?] "RRID"))  ; ie (equal? (cdr (validate thing)))
    ([@ 1]  ; this is the most consistent way to do it
     ([identifierType 1 equal?] "RRID"))
    rrid?)
   ([properCitation 1]  ; TODO pc validator
    ;([@ 1 equal?] (render-as "Proper Citation") (type "Inline Text Citation"))
    ;(@ (render-as "Proper Citation") (type "Inline Text Citation"))  ; an exact required non-terminal?
    ; going the other way, more verbose but is more regular and the intent is cleaer
    ;([@ 1 equal?] ([render-as 1 equal?] "Proper Citation") ([type 1 equal?] "Inline Text Citation"))
    ([@ 1] ([render-as 1 equal?] "Proper Citation")  ; this is the most consistent way to do it
           ([type 1 equal?] "Inline Text Citation"))
    string?)
   ([titles 1]
    ([title (range 1 n)]
     ;([@ (range 0 1)] ([xml:lang (range 0 1) xml-lang?] [_ 1]))
     ([@ (range 0 1)] ([xml:lang (range 0 1)]  xml-lang?))  ; all terminals have only 1 instance in sxml
     string?))
   ;([publisher 1 string?] _ )
   ([publisher 1] string?)
   ([description (range 0 1)]
    ([@ (range 0 1)] ([xml:lang (range 0 1)] xml-lang?)) 
    string?)
   ([subjects (range 0 1)]
    ([subject (range 0 n)] ([@ (range 0 1)] ([xml:lang (range 0 1)] xml-lang?)) 
     string?))
   ([contributors (range 0 1)]  ; list? vs implicit 'only what we list below is allowed'
    ([contributor (range 0 n)]
     ([@ 1] ([contributorType 1] string?))  ; member?
     ([contributorName 1] string?)))
   ([dates 1 #:restrictions ([(date (@ (dateType _ ))) 1])]  ; subtree restrictions
   ;([dates 1]
    ([date (range 1 n)
           #:restrictions ([(@ (dateType "Updated")) (range 0 1) #:warn 0]
                           [(@ (dateType "Submitted")) (range 0 1) #:warn 0])]
           ;#:warn-missing ((@ (dateType "Updated"))
                                                          ; (member? warn-on (cdr sublist)) works
                                                          ;(@ (dateType "Submitted")))]
     ;([@ 1] ([dateType 1 member] [("Submitted"
                                   ;"Updated") 1 null?]))  ; TODO
     ; FIXME need to disallow multiple of the same?
     ;([@ 1] ([dateType 1 member] ("Submitted" "Updated")))
     ;([@ 1] ([dateType 1] ,(λ (node) (member node ("Submitted" "Updated")))))
     ;([@ 1] ([dateType 1] (member? "Submitted" "Updated")))
     ([@ 1] ([dateType 1] (~or "Submitted" "Updated")))
     iso8601-tz-string?))
   ([resourceType 1]
    ([@ 1]
     ;([resourceTypeGeneral 1] (member? ,resource-type-generals))  ; TODO
     ([resourceTypeGeneral 1] resource-type-general?))  ; TODO
     ;([resourceTypeGeneral 1 member] [("Material"
                                       ;"Software"
                                       ;"Services") 1 null?]))  ; TODO
    string?)
   ([alternateIdentifiers 1]
    ([alternateIdentifier (range 1 n)]
     ([@ 1] ([alternateIdentifierType 1] string?))
     (->? (@ (alternateIdentifierType _)))))
     ;(value->predicate (@ (alternateIdentifierType _)))))
   ([relatedIdentifiers
     1
     #:restrictions ([(@ (relationType "IsCompiledBy")) (range 0 n) #:warn 0]
                     [(@ (relationType "IsIdenticalTo")) (range 0 n) #:warn 0]
                     [(@ (relationType "IsDerivedFrom")) (range 0 n) #:warn 0])]
    ([relatedIdentifier (range 1 n)]
     ([@ 1]
      ([relatedIdentifierType 1] (member? "URL" "DOI"))
      ;([relationType 1] (member? ,relation-types))  ; TODO
      ([relationType 1] relation-type?)  ; TODO
      ([resourceTypeGeneral 1] resource-type-general?))  ; TODO
     (value->predicate (@ (relatedIdentifierType _)))))))
))

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

;; main record creation code

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
  (require "database.rkt")
  (require "sources.rkt")
  (make-gtr! identifier-sources)
  ;(set-add-rec! add-rec)  ; FIXME this is a really really terrible way to handle this :/
  ; but I don't want to have to rewrite the stupid function signature :/
  (define rec (make-record 'fake
                           'PREFIX_1234567
                           #:record
                           (fake-rec
                            #:title "Fake resource"
                            #:insert_time 1111111111
                            #:curate_time 1222222222
                            #:something "We need this for the proper citation")))
  '(check-schema rec))
