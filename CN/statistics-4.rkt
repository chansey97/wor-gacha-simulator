#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; CN version

;; run-statistics-4: 统计 普通远古池 获取一个5星英雄的平均抽数
;; 注：由于远古池当抽到5星普通英雄后，不会重置保底，这里的样本未必是首次抽到5星英雄的抽数，
;; 参数 first-only 强制统计首次。
(define (run-statistics-4 sample-size [first-only #f])
  (printf "run-statistics-4:\n")
  (define pool normal-ancient-pool)
  (define pool-name (get-field name pool))
  (define pity-system (get-field pity-system pool))
  (define hard-pity-threshold (get-field hard-pity-threshold pity-system))

  (define (pull-until-get-5-star)
    (when first-only
      (send pool reset))
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (= (rarity-star rarity) 5)
            (add1 count)
            (loop (add1 count))))))

  (send pool reset)
  (let* ((samples (for/list ([i (in-range sample-size)])
                    (pull-until-get-5-star)))
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
    (if first-only
        (printf "==== 统计 ~a 首次获取一个~a的期望抽数 （样本量：~a） ====\n"
                pool-name "5星英雄" sample-size)
        (printf "==== 统计 ~a 获取一个~a的期望抽数 （样本量：~a） ====\n"
                pool-name "5星英雄" sample-size))
    (printf "平均抽数: ~a\n" average)
    (printf "中位数: ~a\n" median)
    (printf "最大值: ~a\n" max)
    (printf "最小值: ~a\n" min)
    (printf "标准差: ~a\n" stddev)
    (for ([bin bins]
          [percentage percentages])
      (if first-only
          (printf "~a% 的玩家在 [~a, ~a] 抽内首次获取一个~a\n"
                  percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5星英雄")
          (printf "~a% 的玩家在 [~a, ~a] 抽内获取一个~a\n"
                  percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5星英雄")))
    (if first-only
        (printf "~a% 的玩家通过硬保底 ~a 抽首次获取一个~a\n"
                hard-pity-percentage (add1 hard-pity-threshold) "5星英雄")
        (printf "~a% 的玩家通过硬保底 ~a 抽获取一个~a\n"
                hard-pity-percentage (add1 hard-pity-threshold) "5星英雄"))
    (printf "\n")
    ))

(run-statistics-4 10000)

(run-statistics-4 10000 #t)

;; run-statistics-4:
;; ==== 统计 普通远古召唤 获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 49.8704
;; 中位数: 37.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 45.001715565520385
;; 61.52% 的玩家在 [1, 50] 抽内获取一个5星英雄
;; 24.91% 的玩家在 [51, 100] 抽内获取一个5星英雄
;; 9.19% 的玩家在 [101, 150] 抽内获取一个5星英雄
;; 4.38% 的玩家在 [151, 200] 抽内获取一个5星英雄
;; 1.25% 的玩家通过硬保底 200 抽获取一个5星英雄

;; run-statistics-4:
;; ==== 统计 普通远古召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 54.1242
;; 中位数: 39.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 49.2330638327537
;; 59.33% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 24.69% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 9.63% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 6.35% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 2.91% 的玩家通过硬保底 200 抽首次获取一个5星英雄
