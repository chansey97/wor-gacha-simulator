#lang racket
(require "./structs.rkt")
(require "./pool.rkt")
(provide (all-defined-out))

;; 英灵系卡池（普通英灵召唤、特定英灵召唤）
(define spirits-pool%
  (class pool%
    (super-new)
    
    (inherit-field base-rarities)
    (inherit-field pity-system)

    (inherit select-rarity)
    (inherit select-hero)

    (define/public (sum-prob rs)
      (foldl + 0 (map rarity-probability rs)))

    ;; 基于保底，计算稀有度列表
    (define/public (calc-rarities)
      (let ((hard-pity-threshold (get-field hard-pity-threshold pity-system))
            (soft-pity-threshold (get-field soft-pity-threshold pity-system))
            (is-soft-pity-on (get-field is-soft-pity-on pity-system))
            (current-pity (get-field current-pity pity-system)))
        (cond
          [(>= current-pity hard-pity-threshold)
           ;; 硬保底：获得 5 星英雄的概率提升至 100%
           (let* ([five-stars (filter (λ (r) (= (rarity-star r) 5)) base-rarities)]
                  [five-stars-prob (sum-prob five-stars)])
             (for/list ([r base-rarities])
               (if (= (rarity-star r) 5)
                   (struct-copy rarity r [probability (/ (rarity-probability r) five-stars-prob)])
                   (struct-copy rarity r [probability 0]))))]
          [(and is-soft-pity-on (>= current-pity soft-pity-threshold))
           ;; 软保底：每次 5 星英雄总概率提升5%，非 5 星总概率减少5%
           (let* ((soft-pity-threshold (get-field soft-pity-threshold pity-system))
                  (soft-pity-boost (get-field soft-pity-boost pity-system))
                  (current-pity (get-field current-pity pity-system))
                  (five-stars (filter (λ (r) (= (rarity-star r) 5)) base-rarities))
                  (five-stars-prob (sum-prob five-stars))
                  (others (filter (λ (r) (not (= (rarity-star r) 5))) base-rarities))
                  (others-prob (sum-prob others))
                  (boost (* soft-pity-boost (+ (- current-pity soft-pity-threshold) 1))) ; 5%提升
                  (scaled-rarities
                   (for/list ([r base-rarities])
                     (cond
                       [(= (rarity-star r) 5)
                        ;; 5星英雄概率按比例增加
                        ;; 新概率 = 原概率 + 提升量 * (原概率 / 原5星总概率)
                        (let ([scale-factor (/ (rarity-probability r) five-stars-prob)])
                          (struct-copy rarity r
                                       [probability (+ (rarity-probability r)
                                                       (* boost scale-factor))]))]
                       [else
                        ;; 非5星英雄概率按比例减少
                        ;; 新概率 = 原概率 - 提升量 * (原概率 / 原非5星总概率)
                        (let ([scale-factor (/ (rarity-probability r) others-prob)])
                          (struct-copy rarity r
                                       [probability (max 0 (- (rarity-probability r)
                                                              (* boost scale-factor)))]))]
                       ))))
             scaled-rarities)]
          [else
           ;; 无保底：使用基础稀有度列表
           base-rarities]
          )))
    
    (define/override (pull)
      (let* ((rarities (calc-rarities))
             (rarity (select-rarity rarities))
             (hero (select-hero rarity))
             (current-pity (get-field current-pity pity-system)))
        (if (= (rarity-star rarity) 5)
            (set-field! current-pity pity-system 0)
            (set-field! current-pity pity-system (+ current-pity 1)))
        (list (card hero rarity))))
    ))
