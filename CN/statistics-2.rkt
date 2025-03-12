#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; CN version

;; run-statistics-2: 统计 普通远古池 首次获取一个5星领主的平均抽数
(define (run-statistics-2 sample-size)
  (printf "run-statistics-2:\n")
  (define pool normal-ancient-pool)
  (define pool-name (get-field name pool))
  (define rarities (get-field base-rarities pool))
  (define pity-system (get-field pity-system pool))
  (define hard-pity-threshold (get-field hard-pity-threshold pity-system))
  
  (define (pull-until-get-5-stars-lord)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (and (= (rarity-stars rarity) 5)
                 (rarity-is-lord rarity))
            (add1 count)
            (loop (add1 count))))))

  (send pool reset)
  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-5-stars-lord)))
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
    (printf "~a% 的玩家通过硬保底 ~a 抽首次获取一个~a\n"
            hard-pity-percentage (add1 hard-pity-threshold) "5星领主")
    (printf "\n")
    ))

(run-statistics-2 10000)

;; run-statistics-2:
;; ==== 统计 普通远古召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 100.8628
;; 中位数: 88.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 70.0846400872545
;; 32.83% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 21.59% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 14.98% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 30.6% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 20.43% 的玩家通过硬保底 200 抽首次获取一个5星领主

