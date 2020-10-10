#lang sicp
(car ''abracadabra)

;is equivalent to:
(car (quote (quote abracadabra)))

;is equivalent to:
(car '(quote abracadabra))

;eg: abracadabra
(cadr '(quote abracadabra))
