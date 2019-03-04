#lang racket/base
(require
 rackunit
 racket/function
 syntax/macro-testing
 (for-syntax racket/base syntax/parse))
(provide hr check-syntax-exn define-string-predicate)

(define-syntax (check-syntax-exn stx)
  (syntax-parse stx
    [(_ body:expr)
     (syntax/loc #'body (check-exn exn:fail:syntax? (thunk (convert-syntax-error body))))]))

(define-syntax (define-string-predicate stx)
    (syntax-parse stx
      [(_ predicate:id string-to-match:string ...+)
       #'(define (predicate value) (member value (list string-to-match ...)))]))

(define (hr nested-hashes #:keys [keys-please #f] . keys)
  ; TODO just allow user to set #:keys and have that become true?
  ; #:keys #f does make toggeling eaiser...
  ; TODO allow user to spec that there is a list of hashes
  (define rkeys (reverse keys))
  (let ([end (if (null? (cdr rkeys))
                 (hash-ref nested-hashes (car rkeys))
                 (hash-ref (apply hr nested-hashes (reverse (cdr rkeys))) (car rkeys)))])
    (if keys-please
        (hash-keys end)
        end)))

(module+ test
  (define res #hasheq((_source . #hasheq((synonyms . #hasheq((a . 1)
                                                             (b . 2)
                                                             (c . 3)))))))
  (hr res '_source #:keys #t)
  (hr res '_source 'synonyms #:keys #f))
