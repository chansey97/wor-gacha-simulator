#lang racket
(require "./structs.rkt")
(require "./spirits-pool.rkt")
(provide (all-defined-out))

;; 限定英灵召唤（带独立保底）
(define limited-spirits-pool%
  (class spirits-pool%
    (super-new)
    
    (inherit-field base-rarities)
    
    (init-field up-hero)
    (init-field [own-pity-threshold 199])
    (init-field [own-pity 0])

    (define up-hero-rarity (find-rarity up-hero))
    
    (define/private (find-rarity hero)
      (findf (λ (r) (member hero (rarity-heroes r))) base-rarities))
    
    (define/override (pull)
      (cond
        ;; 限定保底关闭前
        [(>= own-pity 0)
         (cond
           ;; 200 抽必出限定, 关闭限定保底
           [(>= own-pity own-pity-threshold)
            (super reset)
            (set! own-pity -1)
            (list (card up-hero up-hero-rarity))]
           [else
            ;; 200 抽之前的状态
            (match-let ([(list (card hero rarity)) (super pull)])
              (cond
                ;; 提前抽出限定, 关闭限定保底
                [(string=? hero up-hero)
                 (set! own-pity -1)  
                 (list (card hero rarity))]
                ;; 没有抽出限定
                [else
                 (set! own-pity (+ own-pity 1))
                 (list (card hero rarity))]))])]
        ;; 限定保底关闭后
        [else
         (super pull)]))

    (define/override (reset)
      (super reset)
      (set! own-pity 0))
    ))
