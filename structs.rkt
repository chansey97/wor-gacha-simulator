#lang racket
(provide (all-defined-out))

(struct rarity (name probability star is-lord heroes) #:transparent)

(struct card (hero rarity) #:transparent)
