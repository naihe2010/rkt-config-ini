# rkt-config-ini
Config INI reader/writer by Racket language

# require
(require config-ini)

# function
## new ini struct
*The my-ini variable will be used by ini-read/ini-write, and it
is a mutable hash table.*
> (define my-ini (ini-new))

## read ini from path
> (ini-read my-ini "/tmp/test.ini")

## write ini to path
> (ini-write my-ini "/tmp/test2.ini")

## get section list
> (ini-get-sections my-ini)

## test if token is in ini
> (ini-has-key? my-ini "global" "ip")

## test if entry is in ini
> (ini-has-section? my-ini "global")

## get string value from ini
> (ini-get-key-string my-ini "global" "ip")

## get number value from ini
> (ini-get-key-number my-ini "global" "port")

## get boolean value from ini
> (ini-get-key-number my ini "global" "reuse")

## set value to ini
*value must be string*
> (ini-set-key my-ini "global" "ip" "0.0.0.0")

> (ini-set-key my-ini "global" "port" (number->string 8080))

> (ini-set-key my-ini "global" "reuse" "yes")
