#lang racket
(require "./structs.rkt")
(require "./pool.rkt")
(provide (all-defined-out))

;; 英灵系卡池（普通英灵召唤、特定英灵召唤）
(define spirits-pool%
  (class pool%
    (super-new)
    
    (inherit-field rarities)
    (inherit-field shared-pity)
    (init-field [is-soft-pity-on #f])
    
    (inherit select-rarity)
    (inherit select-hero)

    (define base-rarities rarities)

    (define/public (sum-prob rs)
      (foldl + 0 (map rarity-probability rs)))

    (define/public (reset-rarities)
      (set! rarities base-rarities))

    (define/public (update-rarities-for-hard-pity)
      (let* ([five-stars (filter (λ (r) (= (rarity-stars r) 5)) base-rarities)]
             [five-stars-prob (sum-prob five-stars)])
        (set! rarities
              (for/list ([r base-rarities])
                (if (= (rarity-stars r) 5)
                    (struct-copy rarity r [probability (/ (rarity-probability r) five-stars-prob)])
                    (struct-copy rarity r [probability 0]))))))

    ;; 更新稀有度列表为软保底（5星概率提升5%）
    (define/public (update-rarities-for-soft-pity n)
      (let* ([five-stars (filter (λ (r) (= (rarity-stars r) 5)) base-rarities)]
             [five-stars-prob (sum-prob five-stars)]
             [others (filter (λ (r) (not (= (rarity-stars r) 5))) base-rarities)]
             [others-prob (sum-prob others)]
             [boost (* #e0.05 n)] ; 5%提升
             [scaled-rarities
              (for/list ([r base-rarities])
                (cond
                  [(= (rarity-stars r) 5)
                   ;; 5星英雄概率按比例增加
                   ;; 新概率 = 原概率 + 提升量 * (原概率 / 原5星总概率)
                   (let ([scale-factor (/ (rarity-probability r) five-stars-prob)])
                     (struct-copy rarity r
                                  [probability (+ (rarity-probability r)
                                                  (* boost scale-factor))]))
                   ]
                  [else
                   ;; 非5星英雄概率按比例减少
                   ;; 新概率 = 原概率 - 提升量 * (原概率 / 原非5星总概率)
                   (let ([scale-factor (/ (rarity-probability r) others-prob)])
                     (struct-copy rarity r
                                  [probability (max 0 (- (rarity-probability r)
                                                         (* boost scale-factor)))]))
                   ]))])
        (set! rarities scaled-rarities)))
    
    ;; 英灵召唤的默认抽卡逻辑
    (define/override (pull)
      (define current-pity (unbox shared-pity))
      (cond
        [(>= current-pity 199)
         ;; 硬保底：必定获得5星英雄
         (update-rarities-for-hard-pity)
         (define rarity (select-rarity))
         (reset-rarities)
         (set-box! shared-pity 0)
         (define hero (select-hero rarity))
         (list hero)]
        [(and (>= current-pity 180)
              is-soft-pity-on)
         ;; 软保底：每次5星英雄总概率提升5%，非5星总概率减少5%
         (update-rarities-for-soft-pity (+ (- current-pity 180) 1))
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
         (list hero)])
      )

    (define/override (reset)
      (super reset)
      (reset-rarities))
    ))
