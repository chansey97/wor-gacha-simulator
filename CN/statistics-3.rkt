#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; CN version

;; run-statistics-3: 统计 普通英灵池、狂欢英灵池、普通神圣池、狂欢神圣池 首次获取一个5星领主的平均抽数
(define (run-statistics-3 sample-size)
  (printf "run-statistics-3:\n")
  (define pools (list normal-spirits-pool
                      crazy-spirits-pool
                      normal-divine-pool
                      crazy-divine-pool))
  
  (define (pull-until-get-5-stars-lord pool)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (and (= (rarity-stars rarity) 5)
                 (rarity-is-lord rarity))
            (add1 count)
            (loop (add1 count))))))
  
  (for ([pool pools])
    (send pool reset)
    (let* ((pity-system (get-field pity-system pool))
           (pool-name (get-field name pool))
           (hard-pity-threshold (get-field hard-pity-threshold pity-system))
           (samples (for/list ([i (in-range sample-size)])
                      (pull-until-get-5-stars-lord pool)))
           (average (exact->inexact (mean samples)))
           (median (exact->inexact (median < samples)))
           (max (exact->inexact (apply max samples)))
           (min (exact->inexact (apply min samples)))
           (stddev (exact->inexact (stddev samples)))
           (bins (bin-samples (split-into-4-segments (add1 hard-pity-threshold)) <= samples))
           (percentages (for/list ([bin bins])
                          (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
           (hard-pity-percentage (exact->inexact (* (/ (count (λ (x) (= x (add1 hard-pity-threshold))) samples)
                                                       (length samples)) 100))))
      (printf "==== 统计 ~a 首次获取一个~a的期望抽数 （样本量：~a） ====\n"
              pool-name "5星领主" sample-size)
      (printf "平均抽数: ~a\n" average)
      (printf "中位数: ~a\n" median)
      (printf "最大值: ~a\n" max)
      (printf "最小值: ~a\n" min)
      (printf "标准差: ~a\n" stddev)
      (for ([bin bins]
            [percentage percentages])
        (printf "~a% 的玩家在 [~a, ~a] 抽内首次获取一个~a\n"
                percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5星领主"))
      (printf "\n")
      )))

(run-statistics-3 10000)

;; run-statistics-3:
;; ==== 统计 普通英灵召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 1580.4221
;; 中位数: 1110.0
;; 最大值: 13300.0
;; 最小值: 1.0
;; 标准差: 1527.4444958595352
;; 2.25% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 2.05% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 1.83% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 4.8% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 89.07% 的玩家在 [201, 13300] 抽内首次获取一个5星领主

;; ==== 统计 狂欢英灵召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 1080.4828
;; 中位数: 767.0
;; 最大值: 9628.0
;; 最小值: 1.0
;; 标准差: 1045.9451668726042
;; 3.82% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 3.84% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 3.51% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 4.43% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 84.4% 的玩家在 [201, 9628] 抽内首次获取一个5星领主

;; ==== 统计 普通神圣召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 175.6339
;; 中位数: 122.0
;; 最大值: 1592.0
;; 最小值: 1.0
;; 标准差: 170.91714446125644
;; 2.08% 的玩家在 [1, 5] 抽内首次获取一个5星领主
;; 1.94% 的玩家在 [6, 10] 抽内首次获取一个5星领主
;; 1.87% 的玩家在 [11, 15] 抽内首次获取一个5星领主
;; 4.0% 的玩家在 [16, 20] 抽内首次获取一个5星领主
;; 90.11% 的玩家在 [21, 1592] 抽内首次获取一个5星领主

;; ==== 统计 狂欢神圣召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 116.3015
;; 中位数: 81.0
;; 最大值: 935.0
;; 最小值: 1.0
;; 标准差: 114.39920715525086
;; 3.86% 的玩家在 [1, 5] 抽内首次获取一个5星领主
;; 3.99% 的玩家在 [6, 10] 抽内首次获取一个5星领主
;; 3.49% 的玩家在 [11, 15] 抽内首次获取一个5星领主
;; 3.9% 的玩家在 [16, 20] 抽内首次获取一个5星领主
;; 84.76% 的玩家在 [21, 935] 抽内首次获取一个5星领主
