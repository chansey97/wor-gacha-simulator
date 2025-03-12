#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; CN version

;; run-statistics-1: 统计 普通英灵池、狂欢英灵池、普通神圣池、狂欢神圣池 首次获取一个5星英雄的平均抽数
(define (run-statistics-1 sample-size)
  (printf "run-statistics-1:\n")
  (define pools (list normal-spirits-pool
                      crazy-spirits-pool
                      normal-divine-pool
                      crazy-divine-pool))
  
  (define (pull-until-get-5-stars pool)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (= (rarity-stars rarity) 5)
            (add1 count)
            (loop (add1 count))))))

  (for ([pool pools])
    (send pool reset)
    (let* ((pity-system (get-field pity-system pool))
           (pool-name (get-field name pool))
           (hard-pity-threshold (get-field hard-pity-threshold pity-system))
           (samples (for/list ([i (in-range sample-size)])
                      (pull-until-get-5-stars pool)))
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
      (printf "==== 统计 ~a 首次获取一个~a的期望抽数 （样本量：~a） ====\n" pool-name "5星英雄" sample-size)
      (printf "平均抽数: ~a\n" average)
      (printf "中位数: ~a\n" median)
      (printf "最大值: ~a\n" max)
      (printf "最小值: ~a\n" min)
      (printf "标准差: ~a\n" stddev)
      (for ([bin bins]
            [percentage percentages])
        (printf "~a% 的玩家在 [~a, ~a] 抽内首次获取一个~a\n"
                percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5星英雄"))
      (printf "~a% 的玩家通过硬保底 ~a 抽首次获取一个~a\n"
              hard-pity-percentage (add1 hard-pity-threshold) "5星英雄")
      (printf "\n")
      )))

(run-statistics-1 10000)

;; run-statistics-1:
;; ==== 统计 普通英灵召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 127.6144
;; 中位数: 141.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 71.1063183735454
;; 21.81% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 16.71% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 14.05% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 47.43% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 37.03% 的玩家通过硬保底 200 抽首次获取一个5星英雄

;; ==== 统计 狂欢英灵召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 87.6173
;; 中位数: 70.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 66.6082055058534
;; 38.87% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 24.12% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 14.2% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 22.81% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 14.31% 的玩家通过硬保底 200 抽首次获取一个5星英雄

;; ==== 统计 普通神圣召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 11.7618
;; 中位数: 12.0
;; 最大值: 20.0
;; 最小值: 1.0
;; 标准差: 7.013591145768336
;; 26.96% 的玩家在 [1, 5] 抽内首次获取一个5星英雄
;; 19.4% 的玩家在 [6, 10] 抽内首次获取一个5星英雄
;; 14.67% 的玩家在 [11, 15] 抽内首次获取一个5星英雄
;; 38.97% 的玩家在 [16, 20] 抽内首次获取一个5星英雄
;; 30.71% 的玩家通过硬保底 20 抽首次获取一个5星英雄

;; ==== 统计 狂欢神圣召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 7.66
;; 中位数: 6.0
;; 最大值: 20.0
;; 最小值: 1.0
;; 标准差: 5.9675120443950505
;; 46.93% 的玩家在 [1, 5] 抽内首次获取一个5星英雄
;; 25.69% 的玩家在 [6, 10] 抽内首次获取一个5星英雄
;; 12.63% 的玩家在 [11, 15] 抽内首次获取一个5星英雄
;; 14.75% 的玩家在 [16, 20] 抽内首次获取一个5星英雄
;; 8.94% 的玩家通过硬保底 20 抽首次获取一个5星英雄
