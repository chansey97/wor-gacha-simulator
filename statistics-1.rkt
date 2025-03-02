#lang racket
(require racket/class)
(require threading)
(require math/statistics)
(require "./structs.rkt")
(require "./utils.rkt")
(require "./spirits-pool.rkt")
(require "./limited-spirits-pool.rkt")
(require "./surprise-spirits-pool.rkt")
(require "./ancient-pool.rkt")
(require "./divine-pool.rkt")
(require "./rarities.rkt")

(define (take-sample pool predicate)
  (let loop ([count 1])
    (define heroes (send pool pull))
    (define hero (first heroes))
    (if (predicate hero)
        count
        (loop (add1 count)))))

(define (run-statistics/normal-spirits sample-size check-lord is-soft-pity-on)
  (let* ((shared-pity (box 0))
         (rarities spirits-rarities)
         (pool-name "普通英灵召唤")
         (pool (new spirits-pool%
                    [name pool-name]
                    [shared-pity shared-pity]
                    [rarities rarities]
                    [is-soft-pity-on is-soft-pity-on]))
         (pred (if check-lord
                   (λ (hero)
                     (let ((r (find-rarity rarities hero)))
                       (and (= (rarity-stars r) 5)
                            (rarity-is-lord r))))
                   (λ (hero)
                     (= (rarity-stars (find-rarity rarities hero)) 5))))
         (samples (for/list ([i (in-range sample-size)])
                    (take-sample pool pred)))
         (bins (bin-samples `(0 50 100 150 ,(add1 spirits-pool%-hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
         (hard-pity-percentage (unless check-lord
                                 (exact->inexact (* (/ (count (λ (x) (= x (add1 spirits-pool%-hard-pity-threshold))) samples)
                                                       (length samples)) 100))))
         (average (mean samples))
         (median (median < samples))
         (max-pulls (apply max samples))
         (min-pulls (apply min samples))
         (std-dev (stddev samples)))
    (printf "==== ~a ~a 获取~a的期望 （样本量：~a） ====\n"
            (if is-soft-pity-on "国际服（软保底）" "国服")
            pool-name
            (if check-lord "5星领主" "5星英雄")
            sample-size)
    (printf "平均抽数: ~a\n" (exact-round average))
    (printf "中位数: ~a\n" median)
    (printf "最大值: ~a\n" max-pulls)
    (printf "最小值: ~a\n" min-pulls)
    (printf "标准差: ~a\n" (real->decimal-string std-dev 1))
    (for ([bin bins]
          [percentage percentages])
      (printf "~a% 的玩家在 [~a, ~a] 抽内获取一个 ~a\n" percentage
              (add1 (sample-bin-min bin))
              (sample-bin-max bin)
              (if check-lord "5星领主" "5星英雄")))
    (unless check-lord
      (printf "~a% 的玩家通过硬保底 ~a 抽获取一个 ~a\n" hard-pity-percentage (add1 spirits-pool%-hard-pity-threshold) "5星英雄"))
    ))

(define (run-statistics/crazy-spirits sample-size check-lord is-soft-pity-on)
  (let* ((shared-pity (box 0))
         (rarities (make-spirits-crazy spirits-rarities))
         (pool-name "狂欢英灵召唤")
         (pool (new spirits-pool%
                    [name pool-name]
                    [shared-pity shared-pity]
                    [rarities rarities]
                    [is-soft-pity-on is-soft-pity-on]))
         (pred (if check-lord
                   (λ (hero)
                     (let ((r (find-rarity rarities hero)))
                       (and (= (rarity-stars r) 5)
                            (rarity-is-lord r))))
                   (λ (hero)
                     (= (rarity-stars (find-rarity rarities hero)) 5))))
         (samples (for/list ([i (in-range sample-size)])
                    (take-sample pool pred)))
         (average (mean samples))
         (samples (for/list ([i (in-range sample-size)])
                    (take-sample pool pred)))
         (bins (bin-samples `(0 50 100 150 ,(add1 spirits-pool%-hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
         (hard-pity-percentage (unless check-lord
                                 (exact->inexact (* (/ (count (λ (x) (= x (add1 spirits-pool%-hard-pity-threshold))) samples)
                                                       (length samples)) 100))))
         (median (median < samples))
         (max-pulls (apply max samples))
         (min-pulls (apply min samples))
         (std-dev (stddev samples)))
    (printf "==== ~a ~a 获取~a的期望 （样本量：~a） ====\n"
            (if is-soft-pity-on "国际服（软保底）" "国服")
            pool-name
            (if check-lord "5星领主" "5星英雄")
            sample-size)
    (printf "平均抽数: ~a\n" (exact-round average))
    (printf "中位数: ~a\n" median)
    (printf "最大值: ~a\n" max-pulls)
    (printf "最小值: ~a\n" min-pulls)
    (printf "标准差: ~a\n" (real->decimal-string std-dev 1))
    (for ([bin bins]
          [percentage percentages])
      (printf "~a% 的玩家在 [~a, ~a] 抽内获取一个 ~a\n" percentage
              (add1 (sample-bin-min bin))
              (sample-bin-max bin)
              (if check-lord "5星领主" "5星英雄")))
    (unless check-lord
      (printf "~a% 的玩家通过硬保底 ~a 抽获取一个 ~a\n" hard-pity-percentage (add1 spirits-pool%-hard-pity-threshold) "5星英雄"))
    ))

(define (run-statistics/normal-ancient sample-size check-lord is-soft-pity-on)
  (let* ((shared-pity (box 0))
         (rarities ancient-rarities)
         (pool-name "普通远古召唤")
         (pool (new ancient-pool%
                    [name pool-name]
                    [shared-pity shared-pity]
                    [rarities rarities]
                    [is-soft-pity-on is-soft-pity-on]))
         (pred (if check-lord
                   (λ (hero)
                     (let ((r (find-rarity rarities hero)))
                       (and (= (rarity-stars r) 5)
                            (rarity-is-lord r))))
                   (λ (hero)
                     (= (rarity-stars (find-rarity rarities hero)) 5))))
         (samples (for/list ([i (in-range sample-size)])
                    (take-sample pool pred)))
         (average (mean samples))
         (bins (bin-samples `(0 50 100 150 ,(add1 ancient-pool%-hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
         (hard-pity-percentage (exact->inexact (* (/ (count (λ (x) (= x (add1 ancient-pool%-hard-pity-threshold))) samples)
                                                     (length samples)) 100)))
         (median (median < samples))
         (max-pulls (apply max samples))
         (min-pulls (apply min samples))
         (std-dev (stddev samples)))
    (printf "==== ~a ~a 获取~a的期望 （样本量：~a） ====\n"
            (if is-soft-pity-on "国际服（软保底）" "国服")
            pool-name
            (if check-lord "5星领主" "5星英雄")
            sample-size)
    (printf "平均抽数: ~a\n" (exact-round average))
    (printf "中位数: ~a\n" median)
    (printf "最大值: ~a\n" max-pulls)
    (printf "最小值: ~a\n" min-pulls)
    (printf "标准差: ~a\n" (real->decimal-string std-dev 1))
    (for ([bin bins]
          [percentage percentages])
      (printf "~a% 的玩家在 [~a, ~a] 抽内获取一个 ~a\n" percentage
              (add1 (sample-bin-min bin))
              (sample-bin-max bin)
              (if check-lord "5星领主" "5星英雄")))
    (printf "~a% 的玩家通过硬保底 ~a 抽获取一个 ~a\n"
            hard-pity-percentage
            (add1 ancient-pool%-hard-pity-threshold)
            (if check-lord "5星领主" "5星英雄"))
    ))

(define (run-statistics/normal-divine sample-size check-lord is-soft-pity-on)
  (let* ((shared-pity (box 0))
         (rarities divine-rarities)
         (pool-name "普通神圣召唤")
         (pool (new divine-pool%
                    [name pool-name]
                    [shared-pity shared-pity]
                    [rarities rarities]
                    [is-soft-pity-on is-soft-pity-on]))
         (pred (if check-lord
                   (λ (hero)
                     (let ((r (find-rarity rarities hero)))
                       (and (= (rarity-stars r) 5)
                            (rarity-is-lord r))))
                   (λ (hero)
                     (= (rarity-stars (find-rarity rarities hero)) 5))))
         (samples (for/list ([i (in-range sample-size)])
                    (take-sample pool pred)))
         (average (mean samples))
         (bins (bin-samples `(0 5 10 15 ,(add1 divine-pool%-hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
         (hard-pity-percentage (unless check-lord
                                 (exact->inexact (* (/ (count (λ (x) (= x (add1 divine-pool%-hard-pity-threshold))) samples)
                                                       (length samples)) 100))))
         (median (median < samples))
         (max-pulls (apply max samples))
         (min-pulls (apply min samples))
         (std-dev (stddev samples)))
    (printf "==== ~a ~a 获取~a的期望 （样本量：~a） ====\n"
            (if is-soft-pity-on "国际服（软保底）" "国服")
            pool-name
            (if check-lord "5星领主" "5星英雄")
            sample-size)
    (printf "平均抽数: ~a\n" (exact-round average))
    (printf "中位数: ~a\n" median)
    (printf "最大值: ~a\n" max-pulls)
    (printf "最小值: ~a\n" min-pulls)
    (printf "标准差: ~a\n" (real->decimal-string std-dev 1))
    (for ([bin bins]
          [percentage percentages])
      (printf "~a% 的玩家在 [~a, ~a] 抽内获取一个 ~a\n" percentage
              (add1 (sample-bin-min bin))
              (sample-bin-max bin)
              (if check-lord "5星领主" "5星英雄")))
    (unless check-lord
      (printf "~a% 的玩家通过硬保底 ~a 抽获取一个 ~a\n" hard-pity-percentage (add1 divine-pool%-hard-pity-threshold) "5星英雄"))
    ))

(define (run-statistics/crazy-divine sample-size check-lord is-soft-pity-on)
  (let* ((shared-pity (box 0))
         (rarities (make-divine-crazy divine-rarities))
         (pool-name "狂欢神圣召唤")
         (pool (new divine-pool%
                    [name pool-name]
                    [shared-pity shared-pity]
                    [rarities rarities]
                    [is-soft-pity-on is-soft-pity-on]))
         (pred (if check-lord
                   (λ (hero)
                     (let ((r (find-rarity rarities hero)))
                       (and (= (rarity-stars r) 5)
                            (rarity-is-lord r))))
                   (λ (hero)
                     (= (rarity-stars (find-rarity rarities hero)) 5))))
         (samples (for/list ([i (in-range sample-size)])
                    (take-sample pool pred)))
         (average (mean samples))
         (bins (bin-samples `(0 5 10 15 ,(add1 divine-pool%-hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
         (hard-pity-percentage (unless check-lord
                                 (exact->inexact (* (/ (count (λ (x) (= x (add1 divine-pool%-hard-pity-threshold))) samples)
                                                       (length samples)) 100))))         
         (median (median < samples))
         (max-pulls (apply max samples))
         (min-pulls (apply min samples))
         (std-dev (stddev samples)))
    (printf "==== ~a ~a 获取~a的期望（样本量：~a） ====\n"
            (if is-soft-pity-on "国际服（软保底）" "国服")
            pool-name
            (if check-lord "5星领主" "5星英雄")
            sample-size)
    (printf "平均抽数: ~a\n" (exact-round average))
    (printf "中位数: ~a\n" median)
    (printf "最大值: ~a\n" max-pulls)
    (printf "最小值: ~a\n" min-pulls)
    (printf "标准差: ~a\n" (real->decimal-string std-dev 1))
    (for ([bin bins]
          [percentage percentages])
      (printf "~a% 的玩家在 [~a, ~a] 抽内获取一个 ~a\n" percentage
              (add1 (sample-bin-min bin))
              (sample-bin-max bin)
              (if check-lord "5星领主" "5星英雄")))
    (unless check-lord
      (printf "~a% 的玩家通过硬保底 ~a 抽获取一个 ~a\n" hard-pity-percentage (add1 divine-pool%-hard-pity-threshold) "5星英雄"))
    ))

;;; 国服

;; 注：惊喜英灵召唤本质和普通英灵召唤没有区别，只是当第一次出 5 星英雄时会额外送一个 5 星英雄（且只触发一次）。
;; 因此，可以认为，在惊喜英灵召唤中，抽到一个 5 星英雄的期望抽数是普通英灵召唤的 1/2 。
;; (run-statistics/normal-spirits 10000 #f #f)
;; ==== 国服 普通英灵召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 125
;; 中位数: 135
;; 最大值: 200
;; 最小值: 1
;; 标准差: 71.6
;; 22.37% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 17.92% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 13.36% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 46.35% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 36.24% 的玩家通过硬保底 200 抽获取一个 5星英雄

;; (run-statistics/normal-spirits 10000 #t #f)
;; ==== 国服 普通英灵召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 1586
;; 中位数: 1142
;; 最大值: 13329
;; 最小值: 1
;; 标准差: 1518.4
;; 2.16% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 1.86% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 1.86% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 4.64% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 89.48% 的玩家在 [201, 13329] 抽内获取一个 5星领主

;; (run-statistics/crazy-spirits 10000 #f #f)
;; ==== 国服 狂欢英灵召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 86
;; 中位数: 69
;; 最大值: 200
;; 最小值: 1
;; 标准差: 65.9
;; 39.63% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 23.79% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 14.89% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 21.69% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 13.39% 的玩家通过硬保底 200 抽获取一个 5星英雄

;; (run-statistics/crazy-spirits 10000 #t #f)
;; ==== 国服 狂欢英灵召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 1089
;; 中位数: 769
;; 最大值: 10879
;; 最小值: 1
;; 标准差: 1070.6
;; 4.08% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 3.74% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 3.57% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 4.3% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 84.31% 的玩家在 [201, 10879] 抽内获取一个 5星领主

;; 注：由于远古池当抽到5星普通英雄后，不会重置保底，这里的样本未必是首次抽到5星英雄的抽数。
;; (run-statistics/normal-ancient 10000 #f #f)
;; ==== 国服 普通远古召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 50
;; 中位数: 37
;; 最大值: 200
;; 最小值: 1
;; 标准差: 45.1
;; 62.54% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 23.97% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 8.91% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 4.58% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 1.43% 的玩家通过硬保底 200 抽获取一个 5星英雄

;; (run-statistics/normal-ancient 10000 #t #f)
;; ==== 国服 普通远古召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 100
;; 中位数: 86
;; 最大值: 200
;; 最小值: 1
;; 标准差: 69.6
;; 32.71% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 22.61% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 14.88% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 29.8% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 20.21% 的玩家通过硬保底 200 抽获取一个 5星领主

;; (run-statistics/normal-divine 10000 #f #f)
;; ==== 国服 普通神圣召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 12
;; 中位数: 11
;; 最大值: 20
;; 最小值: 1
;; 标准差: 7.0
;; 26.48% 的玩家在 [1, 5] 抽内获取一个 5星英雄
;; 20.37% 的玩家在 [6, 10] 抽内获取一个 5星英雄
;; 14.57% 的玩家在 [11, 15] 抽内获取一个 5星英雄
;; 38.58% 的玩家在 [16, 20] 抽内获取一个 5星英雄
;; 30.07% 的玩家通过硬保底 20 抽获取一个 5星英雄

;; (run-statistics/normal-divine 10000 #t #f)
;; ==== 国服 普通神圣召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 180
;; 中位数: 124
;; 最大值: 1769
;; 最小值: 1
;; 标准差: 176.0
;; 2.01% 的玩家在 [1, 5] 抽内获取一个 5星领主
;; 2.04% 的玩家在 [6, 10] 抽内获取一个 5星领主
;; 1.93% 的玩家在 [11, 15] 抽内获取一个 5星领主
;; 3.69% 的玩家在 [16, 20] 抽内获取一个 5星领主
;; 90.33% 的玩家在 [21, 1769] 抽内获取一个 5星领主

;; (run-statistics/crazy-divine 10000 #f #f)
;; ==== 国服 狂欢神圣召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 8
;; 中位数: 6
;; 最大值: 20
;; 最小值: 1
;; 标准差: 5.9
;; 46.98% 的玩家在 [1, 5] 抽内获取一个 5星英雄
;; 25.05% 的玩家在 [6, 10] 抽内获取一个 5星英雄
;; 13.06% 的玩家在 [11, 15] 抽内获取一个 5星英雄
;; 14.91% 的玩家在 [16, 20] 抽内获取一个 5星英雄
;; 8.64% 的玩家通过硬保底 20 抽获取一个 5星英雄

;; (run-statistics/crazy-divine 10000 #t #f)
;; ==== 国服 狂欢神圣召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 116
;; 中位数: 81
;; 最大值: 1101
;; 最小值: 1
;; 标准差: 114.3
;; 3.7% 的玩家在 [1, 5] 抽内获取一个 5星领主
;; 3.71% 的玩家在 [6, 10] 抽内获取一个 5星领主
;; 3.48% 的玩家在 [11, 15] 抽内获取一个 5星领主
;; 3.92% 的玩家在 [16, 20] 抽内获取一个 5星领主
;; 85.19% 的玩家在 [21, 1101] 抽内获取一个 5星领主


;;; 国际服（含软保底）

;; (run-statistics/normal-spirits 10000 #f #t)
;; ==== 国际服（软保底） 普通英灵召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 123
;; 中位数: 144
;; 最大值: 195
;; 最小值: 1
;; 标准差: 65.7
;; 21.59% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 16.73% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 13.26% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 48.42% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 0.0% 的玩家通过硬保底 200 抽获取一个 5星英雄

;; (run-statistics/normal-spirits 10000 #t #t)
;; ==== 国际服（软保底） 普通英灵召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 1503
;; 中位数: 1074
;; 最大值: 14089
;; 最小值: 1
;; 标准差: 1445.8
;; 2.09% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 1.95% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 2.0% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 5.22% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 88.74% 的玩家在 [201, 14089] 抽内获取一个 5星领主

;; (run-statistics/crazy-spirits 10000 #f #t)
;; ==== 国际服（软保底） 狂欢英灵召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 86
;; 中位数: 70
;; 最大值: 195
;; 最小值: 1
;; 标准差: 62.8
;; 38.7% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 24.16% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 14.58% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 22.56% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 0.0% 的玩家通过硬保底 200 抽获取一个 5星英雄

;; (run-statistics/crazy-spirits 10000 #t #t)
;; ==== 国际服（软保底） 狂欢英灵召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 1047
;; 中位数: 739
;; 最大值: 9660
;; 最小值: 1
;; 标准差: 1006.5
;; 3.98% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 3.92% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 3.98% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 5.23% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 82.89% 的玩家在 [201, 9660] 抽内获取一个 5星领主

;; (run-statistics/normal-ancient 10000 #f #t)
;; ==== 国际服（软保底） 普通远古召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 50
;; 中位数: 36
;; 最大值: 194
;; 最小值: 1
;; 标准差: 44.3
;; 61.98% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 24.36% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 9.3% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 4.36% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 0.0% 的玩家通过硬保底 200 抽获取一个 5星英雄

;; (run-statistics/normal-ancient 10000 #t #t)
;; ==== 国际服（软保底） 普通远古召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 97
;; 中位数: 85
;; 最大值: 196
;; 最小值: 1
;; 标准差: 66.9
;; 33.91% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 21.91% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 13.97% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 30.21% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 0.0% 的玩家通过硬保底 200 抽获取一个 5星领主

;; (run-statistics/normal-divine 10000 #f #t)
;; ==== 国际服（软保底） 普通神圣召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 11
;; 中位数: 12
;; 最大值: 20
;; 最小值: 1
;; 标准差: 6.0
;; 26.63% 的玩家在 [1, 5] 抽内获取一个 5星英雄
;; 19.53% 的玩家在 [6, 10] 抽内获取一个 5星英雄
;; 26.06% 的玩家在 [11, 15] 抽内获取一个 5星英雄
;; 27.78% 的玩家在 [16, 20] 抽内获取一个 5星英雄
;; 5.19% 的玩家通过硬保底 20 抽获取一个 5星英雄

;; (run-statistics/normal-divine 10000 #t #t)
;; ==== 国际服（软保底） 普通神圣召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 161
;; 中位数: 112
;; 最大值: 1587
;; 最小值: 1
;; 标准差: 158.4
;; 1.96% 的玩家在 [1, 5] 抽内获取一个 5星领主
;; 1.77% 的玩家在 [6, 10] 抽内获取一个 5星领主
;; 2.71% 的玩家在 [11, 15] 抽内获取一个 5星领主
;; 4.16% 的玩家在 [16, 20] 抽内获取一个 5星领主
;; 89.4% 的玩家在 [21, 1587] 抽内获取一个 5星领主

;; (run-statistics/crazy-divine 10000 #f #t)
;; ==== 国际服（软保底） 狂欢神圣召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 7
;; 中位数: 6
;; 最大值: 20
;; 最小值: 1
;; 标准差: 5.3
;; 47.07% 的玩家在 [1, 5] 抽内获取一个 5星英雄
;; 24.44% 的玩家在 [6, 10] 抽内获取一个 5星英雄
;; 18.02% 的玩家在 [11, 15] 抽内获取一个 5星英雄
;; 10.47% 的玩家在 [16, 20] 抽内获取一个 5星英雄
;; 1.19% 的玩家通过硬保底 20 抽获取一个 5星英雄

;; (run-statistics/crazy-divine 10000 #t #t)
;; ==== 国际服（软保底） 狂欢神圣召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 110
;; 中位数: 76
;; 最大值: 1108
;; 最小值: 1
;; 标准差: 108.8
;; 3.7% 的玩家在 [1, 5] 抽内获取一个 5星领主
;; 3.9% 的玩家在 [6, 10] 抽内获取一个 5星领主
;; 4.1% 的玩家在 [11, 15] 抽内获取一个 5星领主
;; 4.18% 的玩家在 [16, 20] 抽内获取一个 5星领主
;; 84.12% 的玩家在 [21, 1108] 抽内获取一个 5星领主

