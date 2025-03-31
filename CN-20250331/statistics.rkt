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

;; CN version

;; 英灵系共享保底
(define spirits-pity-system
  (new pity-system%
       [hard-pity-threshold 199]
       [soft-pity-threshold 180]
       [soft-pity-boost #e0.05]
       [is-soft-pity-on #f]))

;; 远古系共享保底
(define ancient-pity-system
  (new pity-system%
       [hard-pity-threshold 199]
       [soft-pity-threshold 185]
       [soft-pity-boost #e0.08]
       [is-soft-pity-on #f]))

;; 神圣系共享保底
(define divine-pity-system
  (new pity-system%
       [hard-pity-threshold 19]
       [soft-pity-threshold 12]
       [soft-pity-boost #e0.05]
       [is-soft-pity-on #f])) 

;; 狂欢英灵召唤
(define crazy-spirits-rarities
  (make-spirits-crazy spirits-rarities))
(define crazy-spirits-pool-name "狂欢英灵召唤")
(define crazy-spirits-pool
  (new spirits-pool%
       [name crazy-spirits-pool-name]
       [base-rarities crazy-spirits-rarities]
       [pity-system spirits-pity-system]))

;; 特定远古召唤
(define special-ancient-up-heroes-5-star-lord '("普雷特斯"))
(define special-ancient-up-heroes-5-star '("杰拉"))
(define special-ancient-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) ancient-rarities special-ancient-up-heroes-5-star-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _                special-ancient-up-heroes-5-star)))
(define special-ancient-pool-name "特定远古召唤")
(define special-ancient-pool
  (new ancient-pool%
       [name special-ancient-pool-name]
       [base-rarities special-ancient-rarities]
       [pity-system ancient-pity-system]))

;; 狂欢神圣召唤
(define crazy-divine-rarities
  (make-divine-crazy divine-rarities))
(define crazy-divine-pool-name "狂欢神圣召唤")
(define crazy-divine-pool
  (new divine-pool%
       [name crazy-divine-pool-name]
       [base-rarities crazy-divine-rarities]
       [pity-system divine-pity-system]))

(define (split-into-4-segments n)
  (for/list ((i (in-range 5))) (* i (/ n 4))))

