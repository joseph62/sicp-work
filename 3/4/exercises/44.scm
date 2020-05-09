(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

; This transfer procedure does not require the same serialization as the
; previously discussed exchange procedure. The fundamental difference between the
; two operations is that the exchange operations was dependent on the current balance
; of both accounts. The transfer procedure simply takes the amount given and 
; invokes the withdraw and deposit procedures which are serialized for individual account safety.
