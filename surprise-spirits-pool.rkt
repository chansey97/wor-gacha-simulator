#lang racket
(require "./structs.rkt")
(require "./spirits-pool.rkt")
(provide (all-defined-out))

;; 惊喜英灵召唤 (1+1)
(define surprise-spirits-pool%
  (class spirits-pool%
    (super-new)
    
    ;; 额外奖励已被激活?
    (init-field [bonus-actived #f])
    
    (inherit-field base-rarities)
    (inherit-field pity-system)
    
    (define/override (pull)
      (let ((hard-pity-threshold (get-field hard-pity-threshold pity-system)))
        (match-let ([(list card) (super pull)])
          (cond
            [(and (not bonus-actived)
                  (= (rarity-star (card-rarity card)) 5))
             (set! bonus-actived #t)
             (set-field! current-pity pity-system hard-pity-threshold)
             (match-let ([(list bonus-card) (super pull)])
               (list card bonus-card))]
            [else
             (list card)])
          )))
    
    (define/override (reset)
      (super reset)
      (set! bonus-actived #f))
    ))
