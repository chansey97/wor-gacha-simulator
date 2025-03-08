#lang racket
(provide (all-defined-out))

(struct rarity (name probability stars is-lord heroes) #:transparent)

(struct card (hero rarity) #:transparent)
