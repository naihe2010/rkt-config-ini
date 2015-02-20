# rkt-config-ini
Config INI reader/writer by Racket language

## require
(require rkt-config-ini)

## new ini struct
> (define my-ini (ini-new))

*The my-ini variable will be used by ini-read/ini-write, and it
is a mutable hash table.*

## read ini from path
> (ini-read my-ini "/tmp/test.ini")

## write ini to path
> (ini-write my-ini "/tmp/test2.ini")

