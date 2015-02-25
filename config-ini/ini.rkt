#lang racket

(require racket/file)

(define (ini-new)
  (make-hash))

(define (append-ini inientry x)
  (let ([matchs (string-split x "=" #:trim? #f)])
    (if (= (length matchs) 2)
        (let ([token (string-trim (list-ref matchs 0))]
              [value (string-trim (list-ref matchs 1))])
          (hash-set! inientry token value))
        (error (string-append "line error:" x)))))

(define (ini-read-line input)
  (let ([line (read-line input)])
    (when (string? line)
      (if (or (regexp-match #px"^\\s#" line)
              (regexp-match #px"^\\s*$" line))
          (ini-read-line input)
          (string-trim line)))))

(define (input-ini input ini inientry)
  (define x (ini-read-line input))
  (when (string? x)
    (let* ([match (regexp-match #px"^\\s*\\[(.+)\\]\\s*$" x)])
      (if (not match)
          (append-ini inientry x)
          (begin
            (set! inientry (make-hash))
            (hash-set! ini (list-ref match 1) inientry)))
      (input-ini input ini inientry))))

(define (output-ini output ini)
  (hash-for-each ini
                 (lambda (entry entryini)
                   (displayln (string-append "[" entry "]") output)
                   (hash-for-each entryini
                                  (lambda (token value)
                                    (displayln (string-append token " = " value) output)))
                   (displayln "" output))))

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
  (hash-keys ini))

(define (ini-has-key? ini entry key)
  (if (hash-has-key? ini entry)
      (hash-has-key? (hash-ref ini entry) key)
      #f))

(define (ini-has-section? ini entry)
  (hash-has-key? ini entry))

(define (ini-get-key-string ini entry key)
  (hash-ref (hash-ref ini entry) key))

(define (ini-get-key-boolean ini entry key)
  (let* ([str (ini-get-key-string ini entry key)]
         [fch (substring str 0 1)])
    (for/or ([ch (list "y" "Y" "t" "T" "1")]) (string=? ch fch))))

(define (ini-get-key-number ini entry key)
  (let* ([str (ini-get-key-string ini entry key)])
    (string->number str)))

(define (ini-set-key ini entry key value)
  (unless (hash-has-key? ini entry)
    (hash-set! ini entry (make-hash)))
  (let ([inientry (hash-ref ini entry)])
    (cond
      [(string? value)
       (hash-set! inientry key value)]
      [(number? value)
       (hash-set! inientry key (number->string value))]
      [(boolean? value)
       (hash-set! inientry key (if value "1" "0"))]
      [else
       (error (string-append "Not supported data type of:" value))])))

(define (ini-remove-key ini entry key)
  (hash-remove! (hash-ref ini entry) key))

(define (ini-remove-section ini entry)
  (hash-remove! ini entry))

;; Provides
(provide
 ini-new
 ini-read
 ini-write
 ini-sections
 ini-has-key?
 ini-has-section?
 ini-get-key-string
 ini-get-key-boolean
 ini-get-key-number
 ini-set-key
 ini-remove-key
 ini-remove-section)
