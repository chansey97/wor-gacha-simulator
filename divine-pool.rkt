#lang racket
(require "./spirits-pool.rkt")
(provide (all-defined-out))

;; 神圣系卡池
;; 抽卡逻辑和 spirits-pool.rkt 相同，只是构造参数 pity-system 不同
(define divine-pool%
  (class spirits-pool%
    (super-new)
    ))
