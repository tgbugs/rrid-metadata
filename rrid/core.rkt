#lang racket
(require syntax/parse/debug)
(define (not-null? thing) (not (null? thing)))
(debug-syntax-parse!)
(require net/url-string sxml ; atm only for the jats bit
         syntax/parse
         racket/pretty
         "syntax-classes.rkt"
         (for-syntax syntax/parse
                     racket/syntax
                     racket/string
                     racket/pretty
                     "syntax-classes.rkt")
         ;(only-in racket/list range)
         ;(for-template (only-in racket/list range))
         ;(for-syntax (only-in racket/list range))
 )

(provide make-gtr! set-add-rec! make-record schema-url check-schema)

;; macros
(define-syntax (bind stx)  ; FIXME could be called (node stx) or something? make-sxml-node
  (syntax-parse stx
    [(_bind variable-stx)
     #'(quasiquote (variable-stx ,variable-stx))]))

;(require (for-meta -2 (only-in racket/list range)))
;(require syntax/parse
             ;(only-in racket/list range)
             ;(for-syntax (only-in racket/list range))
             ;(for-template (only-in racket/list range))
             ;(for-meta -2 (only-in racket/list range))
             ;)


;(require (for-template (only-in racket/list range))
;(for-syntax (only-in racket/list range)))

;(pred-let->functions )

(define (->? stx case-statements)
  (syntax-parse stx
    [(_ compile-time-subtree-pattern)
    ;subtree->predicate
    #'(λ (run-time-subtree-path)
        ;(let ([-string (compile-time-subtree-pattern.match run-time-subtree-path)])
        (let ([-string (match run-time-subtree-path)])
          (case (match run-time-subtree-path compile-time-subtree-pattern)
            case-statements)))]))
            ;string-let.case-statements)))]))

(define-syntax stx-sxml (λ () "Can't use stx-sxml outside an sxml-schema declaration"))  ; TODO proper error
(define-syntax (sxml-schema stx)  ; more spec-tree-structure
  (syntax-parse stx
    #:literals (stx-sxml)
    [(_ (~optional (~seq #:name syntax-name:id))
        (~optional (~seq #:predicates predicate-let:sc-pred))
        (~optional (~seq #:string->predicate string-let:sc-string-pred))
        schema:sc-body)
     ;(pretty-write `(ct-body-sc: ,(syntax->datum #'schema.syntax-classes)))
     (define BODY
       ; FIXME with-syntax* fails completely silently when not imported wtf
       (with-syntax* ([shead (attribute schema.head)]
                      ;[sc-pat (attribute schema.sc-pat)]
                      ;[sc-pat (attribute sbody.-sc-pat)]
                      ;[sbody (attribute schema.body)]
                      [(syntax-class ...) #'schema.syntax-classes]
                      [checker #'(let ([stx-sxml (datum->syntax #f sxml)])  ; vs with-syntax?
                                   ;schema.abody ...  annoying that this does not work
                                   ;schema.body ...
                                   ;sbody
                                  ;sbody.sc-pat ...
                                  ;schema.-sc-pat ... ; FIXME annoying that the way this is done can't use ...
                                  (syntax-parse stx-sxml
                                    ;#:literals schema.literals  ; we don't actually want this fix the pattern issue
                                    #:local-conventions schema.local-conventions
                                    [schema.racket  ; just validate, there may be better ways
                                     stx-sxml]))]
                      )
           (if (list? (car (syntax->datum #'schema.syntax-classes)))
               (if (attribute syntax-name)
                   #'(define (syntax-name sxml)
                       syntax-class ...
                       checker
                       sxml)
                   #'(λ (sxml)
                       syntax-class ...
                       checker
                       sxml))
               (if (attribute syntax-name)
                   #'(define (syntax-name sxml)
                       schema.syntax-classes
                       checker
                       sxml)
                   #'(λ (sxml)
                       schema.syntax-classes
                       checker
                       sxml)))
       ))
     ;(pretty-print `(ct-BODY: ,(syntax->datum BODY)))

     (define (normalize-sc sc)
       (if (eq? (car sc) 'define-syntax-class)
           (list 'begin sc)
           (cons 'begin sc)))

     #;(define S-BODY
       ;(println `(ct-classes: ,(syntax->datum #'schema.syntax-classes)))
       (if (list? (car (syntax->datum #'schema.syntax-classes)))
           (with-syntax ([(syntax-class ...) #'schema.syntax-classes])
             #`(begin syntax-class ... #,BODY) )
           #`(schema.syntax-classes #,BODY))  ; lol so much cleaner...
         )

     (define S-BODY BODY)

     ; FIXME gonna be a bit different using syntax, would have to use with-syntax
     ; or something like that
     (define P-BODY (if (attribute predicate-let)
                        #`(let predicate-let
                              ; TODO string-let
                              (pretty-print (list 'predicates: predicate-let.name ...))
                            #,S-BODY)
                        S-BODY))

     ;(pretty-write `(ct-S-BODY: ,(syntax->datum S-BODY)))
     (pretty-write `(ct-P-BODY: ,(syntax->datum P-BODY)))
     ;(pretty-write `(ct-MORE: ,(syntax->datum #'schema.stx-tests)))  ; NOTE this is no longer needed
     ;(pretty-write `(ct-MORE: ,(attribute schema.test-name)))
     P-BODY
     ]))

; [tag 1] -> (tag body:expr)
; [tag n] -> (_ (tag body:expr) (tag body:expr) ...)
; [tag (range 0 1)] -> (_ (~optional (tag body:expr)))
; [tag (range 0 n)] -> (_ (~optional (tag body:expr)) ...)
; (head body ... terminal)
;([@ (range 0 1)]
    ;([*NAMESPACES* (range 0 1)]
     ;([(pattern *) (range 0 n)] uri?)))
(define asdfasdf '(hello))
#;(module+ test  ; this is garbage now...

  (define (wat sxml)
    (let ((stx-sxml (datum->syntax #f sxml)))
      (syntax-parse stx-sxml
        ;#:literals (pattern a:* pattern b:*)  ; FIXME need to unique it as well and get rid of patterns
        [(a (b "c") ...)
         stx-sxml]))
    ; maybe return #t if all good?
    ; we error on not good so that help
    sxml)

  (wat '(a (b "c") (b "c")))
  ; (wat '(a (b "e"))) fails as expected
  (define (thing2 sxml)
    (let ((stx-sxml (datum->syntax #f sxml)))
      (syntax-parse stx-sxml
        ((TOP (~optional (~seq (asdf) ...))) stx-sxml)))
    sxml)
  (thing2 '(TOP))
  (thing2 '(TOP (asdf) (asdf)))
  (define (aaa sxml)
    (let ((stx-sxml (datum->syntax #f sxml)))
      (syntax-parse stx-sxml ((top (~optional (~seq (yeee) ...))) stx-sxml)))
    sxml)
  (aaa '(top))
  (aaa '(top (yeee)))
  )

(module+ test
  (sxml-schema #:name deep-nesting ([a 1] ([b 1] ([c 1] ([d 1] ([e 1] ([f 1] "g")
                                                                      ([x 1] string?)
                                                                      ([h 1] "i"))))))))
#;(module+ test 

  ;(sxml-schema ())  ; fails as expected
  ;(sxml-schema ([]))  ; fails as expected
  ;(sxml-schema ([tag]))  ; fails as expected
  ;(sxml-schema ([tag 0]))  ; fails as expected
  ((sxml-schema ([tag 1])) '(tag))
  ((sxml-schema ([tag 1])) '(not-tag))  ; FIXME this should fail!
  ((sxml-schema ([tag 1] 0)) '(tag 0))
  ;((sxml-schema ([tag 1] 0)) '(tag))  ; fails as expected
  ((sxml-schema ([tag 1] "")) '(tag ""))
  (sxml-schema ([tag 1] ([tag2 1] 0) 0))
  ;(sxml-schema ([tag 1] ([tag2 (range 0 1)] 0) 0))  ; TODO FIXME ~optional complaint!?

  #;(
  ;(procedure-arity (car (sxml-schema ([tag 1] 0))))
  ;((car (sxml-schema ([tag 1] 0))) '(tag))
  ;((car (sxml-schema ([tag 1] 0))) '(0))
  (sxml-schema ([tag 1] ([tag2 1 #:restrictions ([(tag3 _) 1])] ([tag3 (range 0 n)] string?))))  ; FIXME what was the _ supposed to be?
  ;(sxml-schema ([tag 1] 0 ""))  ; fails as expected
  ;(sxml-schema ([tag 1] "" 0))  ; fails as expected
  (sxml-schema ([tag 1] ([tag2 (range 0 1)] pred2?) pred1?))
  (sxml-schema ([tag 1] ([tag2 (range 0 1)] ([tag3 1] pred3?) "ok") pred1?))
  (sxml-schema ([tag 1] ([tag2 (range 0 1)] ([tag3 1] pred3?)) pred1?))
  ;(sxml-schema ([tag 1] '0))  ; TODO do we want to allow this?
  ;(sxml-schema ([(pattern1 prefix:*) 1]))  ; fails as expected
  (sxml-schema ([(pattern prefix:*) 1]))
  (sxml-schema ([*TOP* 1]
                ([@ (range 0 1)]
                 ([*NAMESPACES* (range 0 1)] 10))))
  (sxml-schema
   #:predicates ([predicate? (λ (value) #t)]
                 [my-string? string?])
   #:string->predicate (["myType" my-type?])
   ([tag 1]))
  (sxml-schema ([tag 1] "value"))
  (sxml-schema ([tag 1] ([tag2 (range 0 1)] "value2") "value"))
  (sxml-schema ([tag 1]
                ([tag2 (range 0 1)] "value2")
                ([tag3 (range 0 n)] predicate?)
                "value"))
   )

  ;(sixth (schema-structure))
  (sxml-schema #:name test-pred ([top 1] ([head (range 1 n)] string?)))
  (test-pred '(top (head "anything") (head "anything2")))
  ;(test-pred '(top (head "anything") (head 'not-a-string)))  ; fails as expected
  (sxml-schema #:name test1 ([top 1] ([yeee (range 0 n)] "wat")))
  ;(test1 '(top (yeee) (yeee)))  ; fails as expected
  (test1 '(top (yeee "wat") (yeee "wat")))
  (sxml-schema #:name test2 ([(pattern a:*) 1] ([(pattern b:*) 1] "wat")))
  ;(test2 '(a:* (b:*))) ; fails as expected
  ;(test2 '(a:* (b:* "not wat")))  ; fails as expected
  ;(test2 '(should (fail "wat")))  ; fails as expected
  ;(test2 '(should (fail "and does")))
  (test2 '(a:* (b:* "wat")))
  ;(test2 '(a:* (b:* "wat") (b:* "wat")))  ; fails as expected
  (sxml-schema #:name test3 ([(pattern a:*) 1] ([(pattern b:*) (range 0 n)] "wat")))  ; test for duplicate pattern defs
  (test3 '(a:* (b:* "wat") (b:* "wat")))
  (test3 '(a:hello (b:there "wat")))
  ;(test3 '(b:hello (a:there "wat"))) ; failes as expected

  (sxml-schema #:name thing ([TOP 1] ([asdf (range 0 n)] "hello there")))
  ; FIXME I think we want this to translate into (~or* (~optional 1) (~optional 2)) ...
  (sxml-schema #:name multi-body-test ([TOP 1] ([(pattern *body1) (range 0 n)] "hello there")
                                               ([(pattern *body2) (range 0 n)] "general nobody")
                                               ;null)) ; FIXME this should fail? null isn't a predicate
                                               null?))
  (multi-body-test '(TOP (a-body1 "hello there") (b-body1 "hello there")
                         (body2 "general nobody")))

)

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

(require #;(for-syntax syntax/parse/class/paren-shape)  ; leave this out for now
         (prefix-in racket/base/ racket/base)
         syntax/parse/define)

(define-syntax (wrapped stx)
  ; this is not working yet... (current does nothing)
  (syntax-parse stx
    [(_nil quote-stx) #'(quote-stx)]))

#;(define-syntax-parser define-schema-structure
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

(define (schema-structure) ;(define-schema-structure  ; TODO hrm...
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
  (define-values (resource-type-generals relation-types xml-langs)
    (values  ; TODO pull these out
     '("Material" "Software" "Service")
     '("IsCompiledBy" "IsIdenticalTo" "IsDerivedFrom")
     '("en-US")))
  '(sxml-schema
  #:predicates ([related-identifier-type? (λ (value) (member value '("URL" "DOI")))]
                [resource-type-general? (λ (value) (member value resource-type-generals))]
                [relation-type? (λ (value) (member value relation-types))]
                [date-type? (λ (value) (member value '("Submitted" "Updated")))]
                [xml-lang? (λ (value) (member value xml-langs))]
                [rrid? (λ (value) (string-prefix? value "RRID:"))]
                [uri? (λ (value) (regexp-match url-regexp value))]
                )
  #:string->predicate (["DOI" doi?] ["RRID" rrid?] ["URL" uri?])
  ([*TOP* 1]
   ([@ (range 0 1)]
    ([*NAMESPACES* (range 0 1)]
     ([(pattern *) (range 0 n)] uri?)))
   ([resource 1]
    ([@ (range 0 1)]
     ([xmlns 1] rrid?)  ; FIXME eq? schem-url?
     ([(pattern xmlns:*) (range 0 n)] uri?))
    ([identifier 1]
     ([@ 1]  ; this is the most consistent way to do it
      ([identifierType 1] "RRID"))
     rrid?)
    ([properCitation 1]  ; TODO pc validator
     ([@ 1] ([render-as 1] "Proper Citation")  ; this is the most consistent way to do it
            ([type 1] "Inline Text Citation"))
     string?)
    ([titles 1]
     ([title (range 1 n)]
      ([@ (range 0 1)] ([xml:lang (range 0 1)]  xml-lang?))  ; all terminals have only 1 instance in sxml
      string?))
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
    ([dates 1 #:restrictions ([(date (@ (dateType _ ))) 1])]  ; subtree restrictions for at most one
     ;([dates 1]
     ([date (range 1 n)
            #:restrictions ([(@ (dateType "Updated")) (range 0 1) #:warn 0]
                            [(@ (dateType "Submitted")) (range 0 1) #:warn 0])]
      ([@ 1] ([dateType 1] date-type?))
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
       ([relatedIdentifierType 1] related-identifier-type?)  ; FIXME inconsistent
       ;([relationType 1] (member? ,relation-types))  ; TODO
       ([relationType 1] relation-type?)  ; TODO
       ([resourceTypeGeneral 1] resource-type-general?))  ; TODO
      (->? (@ (relatedIdentifierType _)))))))
      ;(value->predicate (@ (relatedIdentifierType _)))))))
))

;(define ss (schema-structure))

(define (check-schema schema sxml)
  (define (check-subtree sub-structure sub-sxml [current null])
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
  (check-subtree schema sxml))

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
  #;(check-schema (schema-structure) rec))
