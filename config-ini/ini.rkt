#lang racket

(require racket/file)
(require racket/help)

(provide ini-new)
(provide ini-read)
(provide ini-write)

(define (ini? a)
  (hash? a))

;;(provide (contract-out
;;        [ini-new (-> void ini?)]
;;      [ini-read (-> (-> ini? string?) boolean?)]
;;    [ini-write (-> (-> ini? string?) boolean?)]))

(define (ini-new)
  (make-hash))

(define (append-ini ini entry x)
  (let ([matchs (regexp-match #px"^\\s*(.+)\\s*=\\s*(.+)\\s*$" x)])
    (if (= (length matchs) 3)
        (let ([token (string-append entry ":" (list-ref matchs 1))]
              [value (list-ref matchs 2)])
          (hash-set! ini token value))
        (error (string-append "line error:" x)))))

(define (ini-read-line input)
  (let ([line (read-line input)])
    (when (string? line)
      (if (or (regexp-match #px"^\\s#" line)
              (regexp-match #px"^\\s*$" line))
          (ini-read-line input)
          (string-copy line)))))

(define (input-ini input ini entry)
  (define x (ini-read-line input))
  (when (string? x)
    (let* ([match (regexp-match #px"^\\s*\\[(.+)\\]\\s*$" x)])
      (if (not match)
          (append-ini ini entry x)
          (set! entry (list-ref match 1))))
    (input-ini input ini entry)))

(define (output-ini output ini)
  (letrec ([entry ""])
    (for ([i (sort (hash-keys ini) string<?)])
      (let* ([nlist (regexp-split  #rx":" i)]
             [tentry (list-ref nlist 0)]
             [ttoken (list-ref nlist 1)]
             [value (hash-ref ini i)])
        (if (string=? entry tentry)
            (displayln (string-append ttoken " = " value) output)
            (begin
              (displayln (string-append "[" tentry "]") output)
              (displayln (string-append ttoken " = " value) output)
              (set! entry tentry)))))))

(define (ini-read ini path)
  (call-with-input-file path
    (lambda (input) (input-ini input ini #f))
    #:mode 'text))

(define (ini-write ini path)
  (call-with-output-file path
    (lambda (output) (output-ini output ini))
    #:mode 'text
    #:exists 'truncate))
