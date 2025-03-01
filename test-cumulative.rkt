#lang racket
(require threading)
(require racket/pretty)
(require racket/class)
(require "./structs.rkt")
(require "./utils.rkt")
(require "./spirits-pool.rkt")
(require "./limited-spirits-pool.rkt")
(require "./surprise-spirits-pool.rkt")
(require "./ancient-pool.rkt")
(require "./divine-pool.rkt")
(require "./rarities.rkt")

(define (test-cumulative)
  ;; 英灵系共享保底
  (define spirits-shared-pity (box 0))
  
  ;; 普通英灵召唤
  (define normal-spirits-rarities spirits-rarities)
  (define normal-spirits-pool-name "普通英灵召唤")
  (define normal-spirits-pool
    (new spirits-pool%
         [name normal-spirits-pool-name]
         [shared-pity spirits-shared-pity]
         [rarities normal-spirits-rarities]
         [is-soft-pity-on #t]))

  (define pool normal-spirits-pool)

  (let* ((rarities (get-field rarities pool))
         (cumulative (send pool build-cumulative rarities)))

    (let ((rarities-cleanup (for/list ([r rarities])
                              (struct-copy rarity r
                                           [probability (exact->inexact (rarity-probability r))]
                                           [heroes '()])))
          (cumulative-cleanup (map exact->inexact cumulative)))
      (printf "rarities\n")
      (pretty-print rarities-cleanup)
      (printf "cumulative\n")
      (pretty-print cumulative-cleanup))
    ))


