#lang racket
(require "./structs.rkt")
(require "./pool.rkt")
(provide (all-defined-out))

(define ancient-pool%-hard-pity-threshold 199)

(define ancient-pool%-soft-pity-threshold 185)

(define ancient-pool%-soft-pity-boost #e0.08)

;; 远古系卡池（保底仅针对领主）
(define ancient-pool%
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
      (set! rarities
            (for/list ([r base-rarities])
              (if (and (= (rarity-stars r) 5) (rarity-is-lord r))
                  (struct-copy rarity r [probability 1])
                  (struct-copy rarity r [probability 0])))))

    ;; 更新稀有度列表为软保底（5星领主概率提升8%）
    (define/public (update-rarities-for-soft-pity n)
      (let* ([others (filter (λ (r) (not (and (= (rarity-stars r) 5)
                                              (rarity-is-lord r))))
                             base-rarities)]
             [others-prob (sum-prob others)]
             [boost (* ancient-pool%-soft-pity-boost n)] ; 8%提升
             [scaled-rarities
              (for/list ([r base-rarities])
                (cond
                  [(and (= (rarity-stars r) 5)
                        (rarity-is-lord r))
                   ;; 5星领主英雄概率按比例增加
                   ;; 新概率 = 原概率 + 提升量
                   (struct-copy rarity r
                                [probability (+ (rarity-probability r) boost)])
                   ]
                  [else
                   ;; 非5星领主英雄概率按比例减少
                   ;; 新概率 = 原概率 - 提升量 * (原概率 / 原非5星领主总概率)
                   (let ([scale-factor (/ (rarity-probability r) others-prob)])
                     (struct-copy rarity r
                                  [probability (max 0 (- (rarity-probability r)
                                                         (* boost scale-factor)) )]))
                   ]))])
        (set! rarities scaled-rarities)))

    
    (define/override (pull)
      (define current-pity (unbox shared-pity))
      (cond
        [(>= current-pity ancient-pool%-hard-pity-threshold)
         ;; 硬保底：必定获得5星领主英雄
         (update-rarities-for-hard-pity)
         (define rarity (select-rarity))
         (reset-rarities)
         (set-box! shared-pity 0)
         (define hero (select-hero rarity))
         (list hero)]
        [(and (>= current-pity ancient-pool%-soft-pity-threshold)
              is-soft-pity-on)
         ;; 软保底：每次5星领主概率提升8%，非5星总概率减少8%
         (update-rarities-for-soft-pity (+ (- current-pity ancient-pool%-soft-pity-threshold) 1))
         (define rarity (select-rarity))
         (if (and (= (rarity-stars rarity) 5)
                  (rarity-is-lord rarity))
             (begin
               (reset-rarities)
               (set-box! shared-pity 0))
             (set-box! shared-pity (+ current-pity 1)))
         (define hero (select-hero rarity))
         (list hero)]
        [else
         ;; 正常抽卡
         (define rarity (select-rarity))
         (if (and (= (rarity-stars rarity) 5)
                  (rarity-is-lord rarity))
             (set-box! shared-pity 0)
             (set-box! shared-pity (+ current-pity 1)))
         (define hero (select-hero rarity))    
         (list hero)]
        ))

    (define/override (reset)
      (super reset)
      (reset-rarities))
    ))
