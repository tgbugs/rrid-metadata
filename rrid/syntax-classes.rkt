#lang racket/base
(module derp racket/base
  (module double-derp racket/base
    #;
    (module blank racket/base
      (provide (all-defined-out))
      (define _ #'please-only-use-me-in-templates-thank-you!))
    (require syntax/parse
             (only-in racket/string string-prefix? string-suffix?)
             (for-syntax (only-in racket/string string-prefix? string-suffix?))
             (for-template (only-in racket/base _))
             #;
             (for-template 'blank))
    (provide (all-defined-out)
             #;
             (for-template (all-from-out 'blank)))

    (define (syntax->string-prefix? stx-str pref-str)
      (string-prefix? (symbol->string (syntax->datum stx-str)) pref-str))
    (define (syntax->string-suffix? stx-str suff-str)
      (string-suffix? (symbol->string (syntax->datum stx-str)) suff-str))

    (define-syntax-class blank-any
      #:literals (_)
      (pattern _))
    (define-syntax-class sc-exact-pat
      ; TODO validating the validation schema means that we should warn if
      ; and exact-cdr-member doesn't match anything
      ; FIXME vs warn-missing on literal leaves? or is that not relevant
      ; there is only one subtree, and it must be direct
      (pattern (name:id (~or* body:sc-exact-pat -match:blank-any))
               ;#:attr cars #'(name body.name) ; body.body.name ; HRM
               #:attr to-match (if (attribute -match)
                                   #'(name to-match)
                                   #'(name body))
               ;#:attr match #'(λ (value) (match value [this-syntax (attribute to-match)]))
               #:attr match #'(λ (value) (match value [this-syntax to-match]))
               )

      ))
  (module symbols racket/base
    (provide (all-defined-out))
    ; big enough for any rrid metadata until the AI's want all their
    ; constitutent identifiers listed on their collaborative papers...
    (define n 99999)
    #;(define ->? (λ () "can't use this outside of special syntax"))
    )

  (require syntax/parse
           racket/syntax
           racket/string
           'double-derp
           (for-syntax racket/base
                       'double-derp
                       )
           (for-template (except-in racket/base _)
                         ;(only-in racket/list range)
                         (only-in racket/string string-split)
                         (only-in syntax/parse pattern define-syntax-class id)
                         (only-in 'double-derp syntax->string-prefix? syntax->string-suffix?)
                         'symbols
                         )  ; fixes range unbound in phase 0 -1 relative
           )
  (provide (all-defined-out)
           (all-from-out 'double-derp)
           (for-template (all-from-out 'symbols)))

  (define-syntax-class sc-name-pat
    #:literals (pattern)
    (pattern (pattern name-pattern:id))) 

  (define-syntax-class sc-count
    #:datum-literals (range n)
    (pattern number:exact-positive-integer
             ; this is basically 1 or n but we use (range 1 n)
             #:attr start #'number
             #:attr stop #'number
             #:attr range #f)
    (pattern (range 0 n)
             #:attr start #f
             #:attr stop #f
             #:attr range #t)
    (pattern (range 0 stop:exact-positive-integer)
             #:attr start #f
             #:attr range #t)
    (pattern (range start:exact-positive-integer n)
             #:attr stop #f
             #:attr range #t)
    (pattern (range start:exact-positive-integer stop:exact-positive-integer)
             #:fail-unless (<= (syntax-e #'start) (syntax-e #'stop)) "stop must be greater than start!"
             #:attr range #t)) 

  (define-syntax-class sc-restr
    (pattern ([subtree:sc-exact-pat 
               count-spec:sc-count
               (~optional (~seq #:warn on-count:exact-nonnegative-integer))] ...)))

  (define count 0)
  (define (next-name)
    (let-values ([(n m) (quotient/remainder count 26)]  ; FIXME why doesn't the error here give a line number :/
           )
      (set! count (add1 count))
      ; FIXME there has to be a better way...
      (list->string (build-list (add1 n) (λ (blank) (integer->char (+ 97 m)))))))
  (define (nsuf this-syntax stx)
    (datum->syntax this-syntax (string->symbol (string-append (symbol->string (syntax->datum stx)) "-" (next-name)))))

  ;(define-for-syntax (syntax->string-prefix? stx-str pref-str)
    ;(string-prefix? (symbol->string (syntax->datum stx-str)) pref-str))
  ;(define-for-syntax (syntax->string-suffix? stx-str suff-str)
    ;(string-suffix? (symbol->string (syntax->datum stx-str)) suff-str))

  (define-syntax-class sc-head
    #:disable-colon-notation
    #:local-conventions ([runtime-name id]
                         [-name id]
                         [name-pat sc-name-pat]
                         [count-spec sc-count]
                         ;[restriction sc-restr]  ; FIXME TODO this is broken...
                         [restriction expr]
                         )
    (pattern [(~or* -name name-pat)
              count-spec
              (~optional (~seq #:restrictions restriction))]
             #:with internal-name (generate-temporary 'internal-name)  ; we need this so we can still use colon notation
             #:attr name (if (attribute -name)
                             (nsuf this-syntax #'-name)
                             ;(generate-temporary 'internal-name-pattern)
                             (nsuf this-syntax #'name-pat.name-pattern))
             #:attr match (if (attribute -name)
                              ;#'-name
                              ;(nsuf this-syntax #'-name)
                              #'-name
                              #'internal-name  ; no colons in the internal names
                              ;#'name-pat.name-pattern
                              )
             #:attr sc-pat (if (attribute -name)
                               #f
                               #;
                               #'(define-syntax-class name
                                   #:disable-colon-notation
                                   (pattern runtime-name
                                            #:do [(unless (eq? '-name (syntax->datum #'runtime-name))
                                                    (raise-syntax-error 'bad-structure
                                                                        (format "expected ~a got ~a"
                                                                                'match
                                                                                (syntax->datum #'runtime-name))))]))
                               #`(define-syntax-class name
                                   #:disable-colon-notation
                                   (pattern runtime-name
                                            #:do [(let* ([p-s (string-split (symbol->string 'name-pat.name-pattern) "*" #:trim? #f)]
                                                         [p (car p-s)]
                                                         [s (cadr p-s)])
                                                    (unless (and (syntax->string-prefix? #'runtime-name p)
                                                                 (syntax->string-suffix? #'runtime-name s))
                                                      (println (list 'failtime: p-s p s #'runtime-name
                                                                     (syntax->string-prefix? #'runtime-name p)
                                                                     (syntax->string-suffix? #'runtime-name s)))
                                                      (raise-syntax-error 'bad-structure
                                                                          (format "expected ~a got ~a"
                                                                                  'name-pat.name-pattern
                                                                                  (syntax->datum #'runtime-name))
                                                                          #;#,this-syntax)))])))
             #:attr range (attribute count-spec.range)
             #:attr start (attribute count-spec.start)
             #:attr stop (attribute count-spec.stop)
             ; we can't actually do this inside of there becuase ~optional needs to wrap it
             ;#:attr racket (cond [(attribute count-spec.number) ; TODO name-pat
             ;#'name]
             ;[#t (cond [(eq? (attribute count-spec.start) 1) #'name]
             ;[(eq? (attribute count-spec.start) 0) #'(~optional name body)])])
             ))

  (define-syntax-class sc-pred
    (pattern ([name:id (~or* function:expr function:id)] ...)))

  (define-syntax-class sc-string-pred
    (pattern ([string-value:string predicate:id] ...)  ; FIXME confusing because backward from usual let
             #:attr lambda-let #'([predicate (λ (value) (equal? string-value value))] ...)
             )))

(require syntax/parse
         racket/syntax
         racket/pretty
         'derp
         (only-in racket/list flatten)
         (for-template racket/base racket/syntax syntax/parse) 
         (for-syntax racket/base syntax/parse 'derp))
(provide (all-defined-out)
         (all-from-out 'derp))

(define-syntax-class sc-terminal
  #:datum-literals (->?)  ; predicate from sibbling path value
  ;#:local-conventions
  #;([predicate id]
     [exact-value string]
     [exact-value integer]
     [subtree sc-exact-pat])
  (pattern (~or* predicate:id
                 exact-value:string 
                 exact-value:integer  ; FIXME when does this happen?!
                 (->? subtree:sc-exact-pat))
           #:with termsc (nsuf this-syntax #'termsc)
           ;#:attr predicate-name (if (attribute predicate) () #f)
           #:attr name (cond [(attribute predicate) (generate-temporary #'predicate)]
                             [(attribute exact-value) #'exact-value]
                             [(attribute subtree) #'"TODO retrive the value at that subtree and make sure it matches"])
           #:attr sc-pat (cond [(attribute predicate)
                                #'(define-syntax-class termsc
                                    (pattern runtime-value:expr
                                             #:do [(unless (predicate (syntax->datum #'runtime-value))
                                                     (raise-syntax-error 'bad-structure
                                                                         (format "TODO ~a not a ~a"
                                                                                 #'runtime-value
                                                                                 (symbol->string 'predicate))
                                                                         this-syntax))]))]
                               [else #f #;#'(i have no idea what is going on here)])
           ))

(define (not-null? thing) (not (null? thing)))

(define (filter-dots stx this-syntax)
  (datum->syntax this-syntax (filter not-null? (syntax->datum stx))))

(define (flatten-dots stx this-syntax)
  (datum->syntax this-syntax (map flatten (filter not-null? (map flatten (syntax->datum stx))))))

(define (join-parts this-syntax stx)
  (let ([dat (syntax->datum stx)])
    (datum->syntax this-syntax (apply append dat))))

(define-syntax-class sc-body
  #:disable-colon-notation
  #:local-conventions ([head sc-head]
                       [body sc-body]
                       [terminal sc-terminal])
  (pattern (head body ... (~optional terminal))

           #:do [(define len-body (length (syntax->datum #'(body.racket ...))))
                 ;(define has-body (not (= len-body) 0))  ; preserving for a bug report
                 (define has-body (not (= len-body 0)))  ; having (not (= len-body) 0) causes an _insane_ error
                 (define has-multi-body (<= 2 len-body))
                 #;
                 (pretty-write len-body)]
           #:with :... (datum->syntax this-syntax '...)  ; FIXME this-syntax may not be appropriate here
           #:with :...+ (datum->syntax this-syntax '...+)
           #:with :~do (datum->syntax this-syntax '~do)
           #:with :+inf.0 (datum->syntax this-syntax +inf.0)
           #:with :0 (datum->syntax this-syntax 0)
           #:with :1 (datum->syntax this-syntax 1)

           #:attr :~alt (if has-multi-body (datum->syntax this-syntax '~alt) #f)
           #:attr :~between (if has-multi-body (datum->syntax this-syntax '~between) #f)
           #:attr :~optional (if (and has-body (attribute head.start))  ; FIXME this needs to support ...
                                    (datum->syntax this-syntax '~optional)
                                    #f)

           #:attr name #'head.match
           #:attr range (attribute head.range)
           #:attr head-stop (attribute head.stop)  ; TODO make sure this is simply false if n is specified
           #:attr head-start (attribute head.start)
           ;#:do [(pretty-write (list 'range?: (attribute range) (attribute head-start) (attribute head-stop)))]

           #:with (syntax-classes ...) (syntax/loc this-syntax
                                         ((~? head.sc-pat)
                                          body.syntax-classes ... ...
                                          (~? terminal.sc-pat)))
           ;#:do [(pretty-write (syntax->datum #'(syntax-classes ...)))]
           #:with (literals ...) #'((~? name) body.literals ... ...)
           ;#:attr literals #'((~? name) (~? body.literals) ...)
           #:attr h-name (if (attribute head.-name) #f #'name) 
           #:attr t-predicate (if (attribute terminal.predicate) #'terminal.name #f) 

           #:with (local-conventions ...) #'((~? [h-name head.name])
                                             body.local-conventions ... ...
                                             (~? [t-predicate terminal.termsc]))
           ;#:do [(pretty-write (syntax->datum #'(local-conventions ...)))]

           ;#:attr body-between/optional (if (= (syntax-e #'head.start) 0) #':~optional #f)
           #:attr elip-type (if (attribute head.stop)
                                #f
                                (if (attribute head.start)
                                    #':...+
                                    #':...))
           #:attr racket #'(name
                            ; FIXME malts and maybe elips go after all the body stuff?
                            (~? (:~alt (:~between body.racket body.head-start body.head-stop) ...)
                                (~? (:~optional (~@ body.racket (~? body.elip-type)) ...)
                                    (~@ (~@ body.racket (~? body.elip-type)) ...)))
                            (~? terminal.name))
           ;#:do [(pretty-write (syntax->datum #'racket))]
           )
  )

#;
(define-syntax-class -sc-body
  (pattern (head:sc-head body:sc-body ... (~optional terminal:sc-terminal))
           #:attr name #'head.match
           #:attr range (attribute head.range)
           #:attr syntax-classes (if (not (null? (syntax->datum #'(body.syntax-classes ...))))
                                     (if (attribute head.name-pat)
                                         (if (and (attribute terminal) (attribute terminal.predicate))
                                             (join-parts this-syntax #'([head.sc-pat] body.syntax-classes ... [terminal.sc-pat]))
                                             (join-parts this-syntax #'([head.sc-pat] body.syntax-classes ...)))
                                         (if (and (attribute terminal) (attribute terminal.predicate))
                                             (join-parts this-syntax #'(body.syntax-classes ... [terminal.sc-pat]))
                                             (join-parts this-syntax #'(body.syntax-classes ...))
                                             ))
                                     (if (attribute head.name-pat)
                                         (if (and (attribute terminal) (attribute terminal.predicate))
                                             #'(head.sc-pat terminal.sc-pat)
                                             #'[head.sc-pat])
                                         (if (and (attribute terminal) (attribute terminal.predicate))
                                             #'[terminal.sc-pat]
                                             #'())))
           #:attr head-convention (if (attribute head.-name) #'() #'([head.match head.name]))
           #:attr term-convention (if (and (attribute terminal)
                                            (attribute terminal.predicate))
                                      #'([terminal.name terminal.termsc])
                                       #'())
           #:attr local-conventions (let ([body-conv
                                           (let ([body-datum
                                                  (apply append
                                                       (filter (λ (thing) (not (null? thing)))
                                                               (syntax->datum
                                                                #'(head-convention body.local-conventions ... term-convention))))])
                                             (if (null? body-datum)
                                                 #'()
                                                 (datum->syntax this-syntax body-datum)))])
                                      body-conv)
           #:attr -literals (if (attribute head.-name)
                                #'(head.-name body.-literals ...)
                                #'(body.-literals ...))
           #:attr literals (datum->syntax this-syntax
                                          (let ([lits (attribute -literals)])
                                            ;(println lits)
                                            (flatten (map syntax->datum (flatten lits)))))
           #:attr start (syntax-e #'head.start)
           #:attr stop (syntax-e #'head.stop)

           #:with alt (datum->syntax this-syntax '~alt)
           #:with seq (datum->syntax this-syntax '~seq)
           #:with opt (datum->syntax this-syntax '~optional)
           #:with between (datum->syntax this-syntax '~between)
           #:with elip (datum->syntax this-syntax '...)
           #:with elip+ (datum->syntax this-syntax '...+)
           #:with tdo (datum->syntax this-syntax '~do)  ; if this is noop I will be super pissed
           #:with inf (datum->syntax this-syntax +inf.0)
           #:with zero (datum->syntax this-syntax 0)
           #:with one (datum->syntax this-syntax 1)
           #:attr alts (if (not-null? (syntax->datum #'(body ...)))  ; FIXME vs #'(body.head-racket ...)
                           #'(seq (alt body.head-racket ...) elip)
                           #f)
           #:attr head-start #'head.start
           #:attr head-stop #'head.stop  ; TODO make sure this gets converted to 1 or +inf.0
           #:attr elip-type (if (= 1 (attribute stop)) #'(seq) (if (= 0 (attribute start)) #'elip #'elip+))  ; FIXME...
           #:attr head-racket (let* ([-start (attribute start)]
                                     [-stop (attribute stop)]
                                     [start (cond [(= -start 0) #f]
                                                  [(= -start 1) #t]
                                                  [#t (raise-syntax-error 'hrm "HRM allow min 2?")])]
                                     [stop (cond [(= -stop +inf.0) #f]
                                                 [(= -stop 1) #t]
                                                 [#t (raise-syntax-error 'hrm "HRM allow not 1 or n?")])]
                                     [altsp (attribute alts)]
                                     [bodyp (attribute alts)]
                                     [malts (< 1 (length (syntax->datum #'(body.head-racket ...))))]
                                     [termp (attribute terminal)]
                                     ; TODO !!! when there is more than one body what do we do?
                                     ; also do we allow restrictions on the order? this is technically
                                     ; originally from xml where the order of nodes doesn't matter and
                                     ; so... probably don't want to try that here... same issue with
                                     ; defining order by allowing recursive structures...
                                     [TODO (length (syntax->datum #'(body ...)))])
                                (if malts
                                    (if termp
                                        #'(name (alt (between body.head-racket body.head-start body.head-stop) ...) elip terminal.name)
                                        #'(name (alt (between body.head-racket body.head-start body.head-stop) ...) elip))
                                    (if bodyp
                                        (if (= 1 (car (syntax->datum #'(body.head-stop ...))))
                                            (if (= 1 (car (syntax->datum #'(body.head-start ...))))
                                                (if termp
                                                    ;(range 1 1)
                                                    #'(name body.head-racket ... terminal.name)
                                                    #'(name body.head-racket ...))
                                                (if termp
                                                    ; (range 0 1)
                                                    #'(name (opt body.head-racket ...) terminal.name)
                                                    #'(name (opt body.head-racket ...))))
                                            (if termp
                                                ; (range 1 n)
                                                #'(name body.head-racket ... body.elip-type ... terminal.name)
                                                #'(name body.head-racket ... body.elip-type ... )))
                                        (if termp
                                            #'(name terminal.name)
                                            #'(name)))))
           #:attr racket (attribute head-racket); #`(#,(attribute name) body.head-racket ...)
           #:attr -racket '(let ([start (attribute body.start)]
                                 [stop (attribute body.stop)])
                             (if start
                                 (if stop
                                     #'(head.name body.head-racket ...) ; no ...
                                     ;#'(head.name (body.name body.body) ... ...) ; no ...
                                     #'(head.name body.head-racket ...) ; ...
                                     ;#'(head.name (body.name body.body) ... ...) ; ...
                                     )
                                 (if stop
                                     #'(head.name body.head-racket ...)
                                     ;#'(head.name (~optional (body.name body.body)) ... ...) ; opt no ...
                                     ;#'(head.name (~optional (body.name body.body)) ... ...) ; opt ...
                                     )))
           #:attr names (if (attribute body)
                            (cons (attribute head.name) (attribute body.names))
                            (list (attribute head.name)))
           #:attr stx-names (datum->syntax this-syntax (attribute names))
           )
  )
