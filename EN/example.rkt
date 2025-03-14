;; Creating the example.rkt in EN folder.
#lang racket
(require threading)
(require racket/class)
(require "../structs.rkt")
(require "../utils.rkt")
(require "../pity-system.rkt")
(require "../spirits-pool.rkt")
(require "../limited-spirits-pool.rkt")
(require "./rarities.rkt")

;; Spirits shared pity system
(define spirits-pity-system
  (new pity-system%
       [hard-pity-threshold 199]
       [soft-pity-threshold 180]
       [soft-pity-boost #e0.05]
       [is-soft-pity-on #t] ; Enable soft pity
       ))

;; Limited Invocation of Spirits
(define limited-spirits-up-hero-5-stars "Diaochan")
(define limited-spirits-rarities
  (add-hero spirits-rarities limited-spirits-up-hero-5-stars 5 #f 15))
(define limited-spirits-pool-name "Limited Invocation of Spirits")
(define limited-spirits-pool
  (new limited-spirits-pool%
       [name limited-spirits-pool-name]
       [base-rarities limited-spirits-rarities]
       [pity-system spirits-pity-system]
       [up-hero limited-spirits-up-hero-5-stars]))

;; Special Invocation of Spirits
(define special-spirits-up-heroes-5-stars-lord '())
(define special-spirits-up-heroes-5-stars '("Constance" "Calypso"))
(define special-spirits-up-heroes-4-stars '("Esme" "Cyrene" "Osiren"))
(define special-spirits-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) spirits-rarities special-spirits-up-heroes-5-stars-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _ special-spirits-up-heroes-5-stars)
      (foldl (λ (hero rs) (add-hero rs hero 4 #f 14)) _ special-spirits-up-heroes-4-stars)))
(define special-spirits-pool-name "Special Invocation of Spirits")
(define special-spirits-pool
  (new spirits-pool%
       [name special-spirits-pool-name]
       [base-rarities special-spirits-rarities]
       [pity-system spirits-pity-system]))

;; Perform a 10-pulls from the Limited Invocation of Spirits.
(for ([i (in-range 10)])
  (let* ((cards (send limited-spirits-pool pull))
         (card (first cards))
         (hero (card-hero card)))
    (printf "~a\n" hero)))

