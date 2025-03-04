#lang racket
(require "./structs.rkt")
(require "./utils.rkt")
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
        (match-let ([(list hero) (super pull)])
          (let ((rarity (find-rarity base-rarities hero)))
            (cond
              [(and (not bonus-actived) (= (rarity-stars rarity) 5))
               (set! bonus-actived #t)
               (set-field! current-pity pity-system hard-pity-threshold)
               (match-let ([(list bonus-hero) (super pull)])
                 (append (list hero)
                         (list bonus-hero)))]
              [else
               (list hero)]))
          )))
    
    (define/override (reset)
      (super reset)
      (set! bonus-actived #f))
    ))
