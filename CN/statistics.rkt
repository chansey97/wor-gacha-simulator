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

;; 普通英灵召唤
(define normal-spirits-rarities spirits-rarities)
(define normal-spirits-pool-name "普通英灵召唤")
(define normal-spirits-pool
  (new spirits-pool%
       [name normal-spirits-pool-name]
       [base-rarities normal-spirits-rarities]
       [pity-system spirits-pity-system]))

;; 特定英灵召唤
(define special-spirits-up-heroes-5-star-lord '())
(define special-spirits-up-heroes-5-star '("康斯坦丝" "卡利普索"))
(define special-spirits-up-heroes-4-star '("艾斯米" "塞蕾妮" "奥西伦"))
(define special-spirits-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) spirits-rarities special-spirits-up-heroes-5-star-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _ special-spirits-up-heroes-5-star)
      (foldl (λ (hero rs) (add-hero rs hero 4 #f 14)) _ special-spirits-up-heroes-4-star)))
(define special-spirits-pool-name "特定英灵召唤")
(define special-spirits-pool
  (new spirits-pool%
       [name special-spirits-pool-name]
       [base-rarities special-spirits-rarities]
       [pity-system spirits-pity-system]))

;; 限定英灵召唤
(define limited-spirits-up-hero-5-star "貂蝉")
(define limited-spirits-rarities
  (add-hero spirits-rarities limited-spirits-up-hero-5-star 5 #f 15))
(define limited-spirits-pool-name "限定英灵召唤")
(define limited-spirits-pool
  (new limited-spirits-pool%
       [name limited-spirits-pool-name]
       [base-rarities limited-spirits-rarities]
       [pity-system spirits-pity-system]
       [up-hero limited-spirits-up-hero-5-star]))

;; 狂欢英灵召唤
(define crazy-spirits-rarities
  (make-spirits-crazy spirits-rarities))
(define crazy-spirits-pool-name "狂欢英灵召唤")
(define crazy-spirits-pool
  (new spirits-pool%
       [name crazy-spirits-pool-name]
       [base-rarities crazy-spirits-rarities]
       [pity-system spirits-pity-system]))

;; 惊喜英灵召唤
(define surprise-spirits-rarities spirits-rarities)
(define surprise-spirits-pool-name "惊喜英灵召唤")
(define surprise-spirits-pool
  (new surprise-spirits-pool%
       [name surprise-spirits-pool-name]
       [base-rarities surprise-spirits-rarities]
       [pity-system spirits-pity-system]))

;; 普通远古召唤
(define normal-ancient-rarities ancient-rarities)
(define normal-ancient-pool-name "普通远古召唤")
(define normal-ancient-pool
  (new ancient-pool%
       [name normal-ancient-pool-name]
       [base-rarities normal-ancient-rarities]
       [pity-system ancient-pity-system]))

;; 特定远古召唤
(define special-ancient-up-heroes-5-star-lord '("居鲁士"))
(define special-ancient-up-heroes-5-star '("吕布"))
(define special-ancient-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) ancient-rarities special-ancient-up-heroes-5-star-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _                special-ancient-up-heroes-5-star)))
(define special-ancient-pool-name "特定远古召唤")
(define special-ancient-pool
  (new ancient-pool%
       [name special-ancient-pool-name]
       [base-rarities special-ancient-rarities]
       [pity-system ancient-pity-system]))

;; 普通神圣召唤
(define normal-divine-rarities divine-rarities)
(define normal-divine-pool-name "普通神圣召唤")
(define normal-divine-pool
  (new divine-pool%
       [name normal-divine-pool-name]
       [base-rarities normal-divine-rarities]
       [pity-system divine-pity-system]))

;; 特定神圣召唤
(define special-divine-up-heroes-5-star-lord '())
(define special-divine-up-heroes-5-star '("康斯坦丝" "卡利普索"))
(define special-divine-up-heroes-4-star '("艾斯米" "塞蕾妮" "奥西伦"))
(define special-divine-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) divine-rarities special-divine-up-heroes-5-star-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _ special-divine-up-heroes-5-star)
      (foldl (λ (hero rs) (add-hero rs hero 4 #f 14)) _ special-divine-up-heroes-4-star)))
(define special-divine-pool-name "特定神圣召唤")
(define special-divine-pool
  (new divine-pool%
       [name special-divine-pool-name]
       [base-rarities special-divine-rarities]
       [pity-system divine-pity-system]))

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

