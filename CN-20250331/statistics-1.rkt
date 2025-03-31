#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; CN version

;; run-statistics-1: 统计 首次抽到 一个特定英雄（如：普雷特斯） 的希望抽数

(define (run-statistics target-hero sample-size)
  (printf "run-statistics: target-hero=~a sample-size=~a\n" target-hero sample-size)
  (define pool special-ancient-pool)
  (define pool-name (get-field name pool))
  (define rarities (get-field base-rarities pool))
  (define pity-system (get-field pity-system pool))
  (define hard-pity-threshold (get-field hard-pity-threshold pity-system))
  
  (define (pull-until-get-hero target-hero)
    (send pool reset)
    (let loop ((pull-count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (hero (card-hero card)))
        (if (string=? hero target-hero)
            (add1 pull-count)
            (loop (add1 pull-count))))))
  
  (send pool reset)
  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-hero target-hero)))
         (average (exact->inexact (mean samples)))
         (median (exact->inexact (median < samples)))
         (max (exact->inexact (apply max samples)))
         (min (exact->inexact (apply min samples)))
         (stddev (exact->inexact (stddev samples)))
         (bins (bin-samples (split-into-4-segments (add1 hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100)))))
    (printf "==== 统计 ~a 首次获取一个~a的期望抽数 （样本量：~a） ====\n"
            pool-name target-hero sample-size)
    (printf "平均抽数: ~a\n" average)
    (printf "中位数: ~a\n" median)
    (printf "最大值: ~a\n" max)
    (printf "最小值: ~a\n" min)
    (printf "标准差: ~a\n" stddev)
    (for ([bin bins]
          [percentage percentages])
      (printf "~a% 的玩家在 [~a, ~a] 抽内首次获取一个~a\n"
              percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) target-hero))
    (printf "\n")
    )
  )

(run-statistics "普雷特斯" 100000)

;; run-statistics: target-hero=普雷特斯 sample-size=100000
;; ==== 统计 特定远古召唤 首次获取一个普雷特斯的期望抽数 （样本量：100000） ====
;; 平均抽数: 206.05417
;; 中位数: 178.0
;; 最大值: 2503.0
;; 最小值: 1.0
;; 标准差: 178.86259048669484
;; 17.744% 的玩家在 [1, 50] 抽内首次获取一个普雷特斯
;; 14.51% 的玩家在 [51, 100] 抽内首次获取一个普雷特斯
;; 11.978% 的玩家在 [101, 150] 抽内首次获取一个普雷特斯
;; 19.567% 的玩家在 [151, 200] 抽内首次获取一个普雷特斯
;; 36.201% 的玩家在 [201, 2503] 抽内首次获取一个普雷特斯


;; (run-statistics "哈尔兹王" 10000)
;; run-statistics: target-hero=哈尔兹王 sample-size=10000
;; ==== 统计 特定远古召唤 首次获取一个哈尔兹王的期望抽数 （样本量：10000） ====
;; 平均抽数: 3111.9622
;; 中位数: 2168.0
;; 最大值: 27612.0
;; 最小值: 1.0
;; 标准差: 3096.437406209136
;; 1.44% 的玩家在 [1, 50] 抽内首次获取一个哈尔兹王
;; 1.2% 的玩家在 [51, 100] 抽内首次获取一个哈尔兹王
;; 1.2% 的玩家在 [101, 150] 抽内首次获取一个哈尔兹王
;; 1.71% 的玩家在 [151, 200] 抽内首次获取一个哈尔兹王
;; 94.45% 的玩家在 [201, 27612] 抽内首次获取一个哈尔兹王
