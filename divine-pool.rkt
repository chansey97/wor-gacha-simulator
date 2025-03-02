#lang racket
(require "./structs.rkt")
(require "./spirits-pool.rkt")
(provide (all-defined-out))

(define divine-pool%-hard-pity-threshold 19)

(define divine-pool%-soft-pity-threshold 12)

(define divine-pool%-soft-pity-boost spirits-pool%-soft-pity-boost)

;; 神圣系卡池
;; 抽卡逻辑和 spirits-pool.rkt 相同，只是保底不同（基本照搬 spirits-pool.rkt 代码）
(define divine-pool%
  (class spirits-pool%
    (super-new)
    
    (inherit-field rarities)
    (inherit-field shared-pity)
    (inherit-field is-soft-pity-on)
    
    (inherit select-rarity)
    (inherit select-hero)
    
    (inherit reset-rarities)
    (inherit update-rarities-for-hard-pity)
    (inherit update-rarities-for-soft-pity)
    
    (define/override (pull)
      (define current-pity (unbox shared-pity))
      (cond
        [(>= current-pity divine-pool%-hard-pity-threshold)
         ;; 硬保底：必定获得5星英雄
         (update-rarities-for-hard-pity)
         (define rarity (select-rarity))
         (reset-rarities)
         (set-box! shared-pity 0)
         (define hero (select-hero rarity))
         (list hero)]
        [(and (>= current-pity divine-pool%-soft-pity-threshold)
              is-soft-pity-on)
         ;; 软保底：每次5星英雄总概率提升5%，非5星总概率减少5%
         (update-rarities-for-soft-pity (+ (- current-pity divine-pool%-soft-pity-threshold) 1))
         (define rarity (select-rarity))
         (if (= (rarity-stars rarity) 5)
             (begin (reset-rarities)
                    (set-box! shared-pity 0))
             (set-box! shared-pity (+ current-pity 1)))
         (define hero (select-hero rarity))
         (list hero)]
        [else
         ;; 正常抽卡
         (define rarity (select-rarity))
         (if (= (rarity-stars rarity) 5)
             (set-box! shared-pity 0)
             (set-box! shared-pity (+ current-pity 1)))
         (define hero (select-hero rarity))    
         (list hero)]))
    
    (define/override (reset)
      (super reset)
      (reset-rarities))
    ))
