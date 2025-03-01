#lang racket
(require "./structs.rkt")
(require "./utils.rkt")
(require "./spirits-pool.rkt")
(provide (all-defined-out))

;; 惊喜英灵召唤 (1+1)
(define surprise-spirits-pool%
  (class spirits-pool%
    (super-new)
    (init-field [bonus-actived #f])  ; 额外奖励已被激活?
    (inherit-field rarities)
    (inherit-field shared-pity)
    
    (define/override (pull)
      (match-let ([(list hero) (super pull)])
        (define rarity (find-rarity rarities hero))
        (cond
          [(and (not bonus-actived) (= (rarity-stars rarity) 5))
           (set! bonus-actived #t)
           (set-box! shared-pity 199)
           (match-let ([(list bonus-hero) (super pull)])
             (append (list hero)
                     (list bonus-hero)))]
          [else
           (list hero)])
        ))
    
    (define/override (reset)
      (super reset)
      (set! bonus-actived #f))
    ))
