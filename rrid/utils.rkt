#lang racket/base
(provide hr)

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
