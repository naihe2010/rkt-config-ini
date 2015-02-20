#lang racket

(require racket/file)

(provide ini-new)
(provide ini-read)
(provide ini-write)
(provide ini-sections)
(provide ini-has-key?)
(provide ini-get-key-string)
(provide ini-get-key-boolean)
(provide ini-get-key-number)
(provide ini-set-key)

(define (ini? a)
  (hash? a))

;;(provide (contract-out
;;        [ini-new (-> void ini?)]
;;      [ini-read (-> (-> ini? string?) boolean?)]
;;    [ini-write (-> (-> ini? string?) boolean?)]))

(define (ini-new)
  (make-hash))

(define (append-ini ini entry x)
  (let ([matchs (string-split x "=" #:repeat? #t)])
    (if (= (length matchs) 2)
        (let ([token (string-append entry ":" (string-trim (list-ref matchs 0)))]
              [value (string-trim (list-ref matchs 1))])
          (hash-set! ini token value))
        (error (string-append "line error:" x)))))

(define (ini-read-line input)
  (let ([line (read-line input)])
    (when (string? line)
      (if (or (regexp-match #px"^\\s#" line)
              (regexp-match #px"^\\s*$" line))
          (ini-read-line input)
          (string-trim line)))))

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
      (let* ([nlist (string-split i ":")]
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

(define (ini-sections ini)
  (let ([tokens (make-hash)])
    (hash-for-each ini
                   (lambda (k v)
                     (hash-set! tokens
                                (list-ref (string-split k ":") 0)
                                1)))
    (hash-keys tokens)))

(define (ini-has-key? ini entry key)
  (hash-has-key? ini (string-append entry ":" key)))

(define (ini-get-key-string ini entry key)
  (hash-ref ini(string-append entry ":" key)))

(define (ini-get-key-boolean ini entry key)
  (let* ([str (ini-get-key-string ini entry key)]
         [fch (substring str 0 1)])
    (if (or (string=? fch "y")
            (string=? fch "Y")
            (string=? fch "t")
            (string=? fch "T")
            (string=? fch "1")) #t #f)))

(define (ini-get-key-number ini entry key)
  (let* ([str (ini-get-key-string ini entry key)])
    (string->number str)))

(define (ini-set-key ini entry key value)
  (hash-set! ini (string-append entry ":" key) value))
