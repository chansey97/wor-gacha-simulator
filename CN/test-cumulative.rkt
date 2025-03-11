#lang racket
(require racket/pretty)
(require racket/class)
(require "../structs.rkt")
(require "../pity-system.rkt")
(require "../spirits-pool.rkt")
(require "./rarities.rkt")

(define is-soft-pity-on #f)

(define (test-cumulative)
  ;; 英灵系共享保底
  (define spirits-pity-system
    (new pity-system%
         [hard-pity-threshold 199]
         [soft-pity-threshold 180]
         [soft-pity-boost #e0.05]
         [is-soft-pity-on is-soft-pity-on]))
  
  ;; 普通英灵召唤
  (define normal-spirits-rarities spirits-rarities)
  (define normal-spirits-pool-name "普通英灵召唤")
  (define normal-spirits-pool
    (new spirits-pool%
         [name normal-spirits-pool-name]
         [base-rarities normal-spirits-rarities]
         [pity-system spirits-pity-system]))

  (let* ((pool normal-spirits-pool)
         (rarities (get-field base-rarities pool))
         (cumulative (send pool build-cumulative rarities)))
    (let ((rarities-cleanup (for/list ([r rarities])
                              (struct-copy rarity r
                                           [probability (exact->inexact (rarity-probability r))]
                                           [heroes '()])))
          (cumulative-cleanup (map exact->inexact cumulative)))
      (printf "rarities-cleanup\n")
      (pretty-print rarities-cleanup)
      (printf "cumulative-cleanup\n")
      (pretty-print cumulative-cleanup))
    ))

;; (test-cumulative)
;; rarities-cleanup
;; (list
;;  (rarity "5星领主英雄" 0.0004 5 #t '())
;;  (rarity "5星普通英雄" 0.0046 5 #f '())
;;  (rarity "4星领主英雄" 0.0012 4 #t '())
;;  (rarity "4星普通英雄" 0.0788 4 #f '())
;;  (rarity "3星领主英雄" 0.0399 3 #t '())
;;  (rarity "3星普通英雄" 0.3751 3 #f '())
;;  (rarity "2星普通英雄" 0.5 2 #f '()))
;; cumulative-cleanup
;; '(0.0004 0.005 0.0062 0.085 0.1249 0.5 1.0)
