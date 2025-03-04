#lang racket
(require "./structs.rkt")
(require "./pool.rkt")
(require "./pity-system.rkt")
(provide (all-defined-out))

;; 远古系卡池（保底仅针对领主）
(define ancient-pool%
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
           ;; 硬保底：获得 5 星领主概率提升至 100%
           (for/list ([r base-rarities])
             (if (and (= (rarity-stars r) 5) (rarity-is-lord r))
                 (struct-copy rarity r [probability 1])
                 (struct-copy rarity r [probability 0])))]
          [(and is-soft-pity-on (>= current-pity soft-pity-threshold))
           ;; 软保底：每次 5 星领主概率提升8%，非5星总概率减少8%
           (let* ((soft-pity-threshold (get-field soft-pity-threshold pity-system))
                  (soft-pity-boost (get-field soft-pity-boost pity-system))
                  (current-pity (get-field current-pity pity-system))
                  (others (filter (λ (r) (not (and (= (rarity-stars r) 5) (rarity-is-lord r)))) base-rarities))
                  (others-prob (sum-prob others))
                  (boost (* soft-pity-boost (+ (- current-pity soft-pity-threshold) 1)))
                  (scaled-rarities
                   (for/list ([r base-rarities])
                     (cond
                       [(and (= (rarity-stars r) 5) (rarity-is-lord r))
                        ;; 5星领主英雄概率按比例增加
                        ;; 新概率 = 原概率 + 提升量
                        (struct-copy rarity r
                                     [probability (+ (rarity-probability r) boost)])]
                       [else
                        ;; 非5星领主英雄概率按比例减少
                        ;; 新概率 = 原概率 - 提升量 * (原概率 / 原非5星领主总概率)
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
        (if (and (= (rarity-stars rarity) 5) (rarity-is-lord rarity))
            (set-field! current-pity pity-system 0)
            (set-field! current-pity pity-system (+ current-pity 1)))
        (list hero)))
    ))
