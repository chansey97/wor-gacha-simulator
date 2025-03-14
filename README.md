# wor-gacha-simulator
Gacha Simulator of [Watcher of Realms](https://www.watcherofrealms.com/) [潮汐守望者](http://cxswz.moonton.com/)抽卡模拟器

The game’s pools can share a pity system,which allows for varied pulling strategies. This project aims to evaluate these pools to find the best strategy.

Conclusion: See [CN server (国服)](./CN/conclusion.md) and [International Server (国际服)](./EN/conclusion.md)

## Basic Usage

1. Install [Racket](https://racket-lang.org/)
2. Double click main.rkt to launch DrRacket.
   - Use CN/main.rkt for the CN Server
   - Use EN/main.rkt for the International Server
3. Click "Run" to launch the simulator.

The difference between CN and International Server is that

1. The International Server has a soft pity system, i.e. **The Pity Timer will activate if no Legendary hero is summoned after 180 consecutive Invocation of Spirits summons. When the Pity Timer is active and no Legendary hero is summoned, the drop rate for a Legendary hero will increase by 5% in the next summon. A Legendary hero is guaranteed within 200 summons**, while CN server only guarantees a Legendary hero with 200 summons
2. CN and International Server have different drop lists, e.g. 柯罗莎 is exclusive to CN Server.
3. CN Server cannot switch languages, so heroes only have Chinese names.

## Advanced Usage

Write Racket code to test your own pulling strategy.

For example, in the International Server, create a Limited Invocation of Spirits with Diaochan on rate-up and a Special Invocation of Spirits with Constance, Calypso, Esme, Cyrene and Osiren on rate-up. They share the same pity system with an initial pity counter of 50. Perform a 10-pull from the Limited Invocation of Spirits.

```racket
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
```



