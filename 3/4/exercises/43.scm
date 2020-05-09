; Exchanges between three accounts using the serialized method will
; always result in the three balances existing in each of the three accounts
; because the serialized account swapping version ensures that the two accounts
; being operated upon cannot have other procedures running that change their balances
; during the course of the swap procedure.

; Account 1: 10
; Account 2: 20
; Account 3: 30

; P1: exchange 1 and 2
; P2: exchange 1 and 3

; P1: read account 1: 10
; P1: read account 2: 20
; P2: read account 1: 10
; P1: diff 10
; P2: read account 3: 30
; P2: diff 20
; P2: deposit 20 account 1: 30
; P2: withdraw 20 account 3: 10
; P1: deposit 10 account 1: 40
; P1: withdraw 10 account 2: 10

; Account 1: 40
; Account 2: 10
; Account 3: 10

; With the serialization built around the changing procedures
; for individual accounts the sum of the three balances
; after some number of concurrent exchanges would still add
; up to 60. Without the serialization the values of individual
; transactions would overwrite other in progress transactions
; and break this property as well.

; Account 1: 10
; Account 2: 20
; Account 3: 30

; P1: exchange 1 and 2
; P2: exchange 1 and 3

; P1: read account 1: 10
; P1: read account 2: 20
; P2: read account 1: 10
; P1: diff 10
; P2: read account 3: 30
; P2: diff 20
; P2: deposit 20 account 1
; P2: deposit calculate difference: 30
; P2: withdraw 20 account 3: 10
; P1: deposit 10 account 1
; P1: deposit calculate difference: 20
; P2: deposit set balance account 1: 30
; P1: deposit set balance account 1: 20
; P1: withdraw 10 account 2: 10

; Account 1: 20
; Account 2: 10
; Account 3: 10
