#lang racket
(require racket/class)
(require threading)
(require "../utils.rkt")
(require "../pity-system.rkt")
(require "../spirits-pool.rkt")
(require "../limited-spirits-pool.rkt")
(require "../surprise-spirits-pool.rkt")
(require "../ancient-pool.rkt")
(require "../divine-pool.rkt")
(require "./rarities.rkt")
(provide (all-defined-out))

;; International version

(define limited-pool-own-pity-threshold 199)
;; (define limited-pool-own-pity-threshold 249) ; old version

;; Spirits shared pity system
(define spirits-pity-system
  (new pity-system%
       [hard-pity-threshold 199]
       [soft-pity-threshold 180]
       [soft-pity-boost #e0.05]
       [is-soft-pity-on #t]))

;; Ancient shared pity system
(define ancient-pity-system
  (new pity-system%
       [hard-pity-threshold 199]
       [soft-pity-threshold 185]
       [soft-pity-boost #e0.08]
       [is-soft-pity-on #t]))

;; Divine shared pity system
(define divine-pity-system
  (new pity-system%
       [hard-pity-threshold 19]
       [soft-pity-threshold 12]
       [soft-pity-boost #e0.05]
       [is-soft-pity-on #t])) 

;; Normal Invocation of Spirits
(define normal-spirits-rarities spirits-rarities)
(define normal-spirits-pool-name "Invocation of Spirits")
(define normal-spirits-pool
  (new spirits-pool%
       [name normal-spirits-pool-name]
       [base-rarities normal-spirits-rarities]
       [pity-system spirits-pity-system]))

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
       [up-hero limited-spirits-up-hero-5-stars]
       [own-pity-threshold limited-pool-own-pity-threshold]))

;; Crazy Invocation of Spirits
(define crazy-spirits-rarities
  (make-spirits-crazy spirits-rarities))
(define crazy-spirits-pool-name "Crazy Invocation of Spirits")
(define crazy-spirits-pool
  (new spirits-pool%
       [name crazy-spirits-pool-name]
       [base-rarities crazy-spirits-rarities]
       [pity-system spirits-pity-system]))

;; Surprise Invocation of Spirits
(define surprise-spirits-rarities spirits-rarities)
(define surprise-spirits-pool-name "Surprise Invocation of Spirits")
(define surprise-spirits-pool
  (new surprise-spirits-pool%
       [name surprise-spirits-pool-name]
       [base-rarities surprise-spirits-rarities]
       [pity-system spirits-pity-system]))

;; Normal Ancient Summoning
(define normal-ancient-rarities ancient-rarities)
(define normal-ancient-pool-name "Normal Ancient Summoning")
(define normal-ancient-pool
  (new ancient-pool%
       [name normal-ancient-pool-name]
       [base-rarities normal-ancient-rarities]
       [pity-system ancient-pity-system]))

;; Special Ancient Summoning
(define special-ancient-up-heroes-5-stars-lord '("Cyrus"))
(define special-ancient-up-heroes-5-stars '("Lu Bu"))
(define special-ancient-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) ancient-rarities special-ancient-up-heroes-5-stars-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _                special-ancient-up-heroes-5-stars)))
(define special-ancient-pool-name "Special Ancient Summoning")
(define special-ancient-pool
  (new ancient-pool%
       [name special-ancient-pool-name]
       [base-rarities special-ancient-rarities]
       [pity-system ancient-pity-system]))

;; Normal Divine Summoning
(define normal-divine-rarities divine-rarities)
(define normal-divine-pool-name "Normal Divine Summoning")
(define normal-divine-pool
  (new divine-pool%
       [name normal-divine-pool-name]
       [base-rarities normal-divine-rarities]
       [pity-system divine-pity-system]))

;; Special Divine Summoning
(define special-divine-up-heroes-5-stars-lord '())
(define special-divine-up-heroes-5-stars '("Constance" "Calypso"))
(define special-divine-up-heroes-4-stars '("Esme" "Cyrene" "Osiren"))
(define special-divine-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) divine-rarities special-divine-up-heroes-5-stars-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _ special-divine-up-heroes-5-stars)
      (foldl (λ (hero rs) (add-hero rs hero 4 #f 14)) _ special-divine-up-heroes-4-stars)))
(define special-divine-pool-name "Special Divine Summoning")
(define special-divine-pool
  (new divine-pool%
       [name special-divine-pool-name]
       [base-rarities special-divine-rarities]
       [pity-system divine-pity-system]))

;; Crazy Divine Summoning
(define crazy-divine-rarities
  (make-divine-crazy divine-rarities))
(define crazy-divine-pool-name "Crazy Divine Summoning")
(define crazy-divine-pool
  (new divine-pool%
       [name crazy-divine-pool-name]
       [base-rarities crazy-divine-rarities]
       [pity-system divine-pity-system]))

(define (split-into-4-segments n)
  (for/list ((i (in-range 5))) (* i (/ n 4))))




