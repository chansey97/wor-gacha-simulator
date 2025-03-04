#lang racket
(require racket/class)
(require threading)
(require math/statistics)
(require "./structs.rkt")
(require "./utils.rkt")
(require "./pity-system.rkt")
(require "./spirits-pool.rkt")
(require "./limited-spirits-pool.rkt")
(require "./surprise-spirits-pool.rkt")
(require "./ancient-pool.rkt")
(require "./divine-pool.rkt")
(require "./rarities.rkt")

;; TODO: 不应该基于池来计算平均抽数，而应该基于策略
;; 因此，每次伯努利实验，应该返回 抽数/抽到的金卡

;; TODO: 统一这些不同的测试例

(define (take-sample pool predicate)
  (let loop ([count 1])
    (define heroes (send pool pull))
    (define hero (first heroes))
    (if (predicate hero)
        count
        (loop (add1 count)))))

(define (run-statistics/normal-spirits sample-size check-lord is-soft-pity-on)
  (let* ((hard-pity-threshold 199)
         (pity-system (new pity-system%
                           [hard-pity-threshold hard-pity-threshold]
                           [soft-pity-threshold 180]
                           [soft-pity-boost #e0.05]
                           [is-soft-pity-on is-soft-pity-on]))
         (rarities spirits-rarities)
         (pool-name "普通英灵召唤")
         (pool (new spirits-pool%
                    [name pool-name]
                    [base-rarities rarities]
                    [pity-system pity-system]))
         (pred (if check-lord
                   (λ (hero)
                     (let ((r (find-rarity rarities hero)))
                       (and (= (rarity-stars r) 5)
                            (rarity-is-lord r))))
                   (λ (hero)
                     (= (rarity-stars (find-rarity rarities hero)) 5))))
         (samples (for/list ([i (in-range sample-size)])
                    (take-sample pool pred)))
         (bins (bin-samples `(0 50 100 150 ,(add1 hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
         (hard-pity-percentage (unless check-lord
                                 (exact->inexact (* (/ (count (λ (x) (= x (add1 hard-pity-threshold))) samples)
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
      (printf "~a% 的玩家通过硬保底 ~a 抽获取一个 ~a\n" hard-pity-percentage (add1 hard-pity-threshold) "5星英雄"))
    ))

(define (run-statistics/crazy-spirits sample-size check-lord is-soft-pity-on)
  (let* ((hard-pity-threshold 199)
         (pity-system (new pity-system%
                           [hard-pity-threshold hard-pity-threshold]
                           [soft-pity-threshold 180]
                           [soft-pity-boost #e0.05]
                           [is-soft-pity-on is-soft-pity-on]))
         (rarities (make-spirits-crazy spirits-rarities))
         (pool-name "狂欢英灵召唤")
         (pool (new spirits-pool%
                    [name pool-name]
                    [base-rarities rarities]
                    [pity-system pity-system]))
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
         (bins (bin-samples `(0 50 100 150 ,(add1 hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
         (hard-pity-percentage (unless check-lord
                                 (exact->inexact (* (/ (count (λ (x) (= x (add1 hard-pity-threshold))) samples)
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
      (printf "~a% 的玩家通过硬保底 ~a 抽获取一个 ~a\n" hard-pity-percentage (add1 hard-pity-threshold) "5星英雄"))
    ))

(define (run-statistics/normal-ancient sample-size check-lord is-soft-pity-on)
  (let* ((hard-pity-threshold 199)
         (pity-system (new pity-system%
                           [hard-pity-threshold hard-pity-threshold]
                           [soft-pity-threshold 185]
                           [soft-pity-boost #e0.08]
                           [is-soft-pity-on is-soft-pity-on]))
         (rarities ancient-rarities)
         (pool-name "普通远古召唤")
         (pool (new ancient-pool%
                    [name pool-name]
                    [base-rarities rarities]
                    [pity-system pity-system]))
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
         (bins (bin-samples `(0 50 100 150 ,(add1 hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
         (hard-pity-percentage (exact->inexact (* (/ (count (λ (x) (= x (add1 hard-pity-threshold))) samples)
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
            (add1 hard-pity-threshold)
            (if check-lord "5星领主" "5星英雄"))
    ))

(define (run-statistics/normal-divine sample-size check-lord is-soft-pity-on)
  (let* ((hard-pity-threshold 19)
         (pity-system (new pity-system%
                           [hard-pity-threshold hard-pity-threshold]
                           [soft-pity-threshold 12]
                           [soft-pity-boost #e0.05]
                           [is-soft-pity-on is-soft-pity-on]))
         (rarities divine-rarities)
         (pool-name "普通神圣召唤")
         (pool (new divine-pool%
                    [name pool-name]
                    [base-rarities rarities]
                    [pity-system pity-system]))
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
         (bins (bin-samples `(0 5 10 15 ,(add1 hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
         (hard-pity-percentage (unless check-lord
                                 (exact->inexact (* (/ (count (λ (x) (= x (add1 hard-pity-threshold))) samples)
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
      (printf "~a% 的玩家通过硬保底 ~a 抽获取一个 ~a\n" hard-pity-percentage (add1 hard-pity-threshold) "5星英雄"))
    ))

(define (run-statistics/crazy-divine sample-size check-lord is-soft-pity-on)
  (let* ((hard-pity-threshold 19)
         (pity-system (new pity-system%
                           [hard-pity-threshold hard-pity-threshold]
                           [soft-pity-threshold 12]
                           [soft-pity-boost #e0.05]
                           [is-soft-pity-on is-soft-pity-on]))
         (rarities (make-divine-crazy divine-rarities))
         (pool-name "狂欢神圣召唤")
         (pool (new divine-pool%
                    [name pool-name]
                    [base-rarities rarities]
                    [pity-system pity-system]))
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
         (bins (bin-samples `(0 5 10 15 ,(add1 hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
         (hard-pity-percentage (unless check-lord
                                 (exact->inexact (* (/ (count (λ (x) (= x (add1 hard-pity-threshold))) samples)
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
      (printf "~a% 的玩家通过硬保底 ~a 抽获取一个 ~a\n" hard-pity-percentage (add1 hard-pity-threshold) "5星英雄"))
    ))

;;; 国服（无软保底）

;; 注：惊喜英灵召唤本质和普通英灵召唤没有区别，只是当第一次出 5 星英雄时会额外送一个 5 星英雄（且只触发一次）。
;; 因此，可以认为，在惊喜英灵召唤中，抽到一个 5 星英雄的期望抽数是普通英灵召唤的 1/2 。
(run-statistics/normal-spirits 10000 #f #f)
;; ==== 国服 普通英灵召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 128
;; 中位数: 142
;; 最大值: 200
;; 最小值: 1
;; 标准差: 71.4
;; 21.65% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 17.32% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 13.34% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 47.69% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 37.85% 的玩家通过硬保底 200 抽获取一个 5星英雄

(run-statistics/normal-spirits 10000 #t #f)
;; ==== 国服 普通英灵召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 1601
;; 中位数: 1123
;; 最大值: 13186
;; 最小值: 1
;; 标准差: 1564.6
;; 1.81% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 2.01% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 1.91% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 5.07% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 89.2% 的玩家在 [201, 13186] 抽内获取一个 5星领主

(run-statistics/crazy-spirits 10000 #f #f)
;; ==== 国服 狂欢英灵召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 87
;; 中位数: 70
;; 最大值: 200
;; 最小值: 1
;; 标准差: 66.2
;; 39.54% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 23.67% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 14.44% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 22.35% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 13.58% 的玩家通过硬保底 200 抽获取一个 5星英雄

(run-statistics/crazy-spirits 10000 #t #f)
;; === 国服 狂欢英灵召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 1102
;; 中位数: 759
;; 最大值: 10864
;; 最小值: 1
;; 标准差: 1068.0
;; 3.77% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 4.2% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 3.63% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 4.56% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 83.84% 的玩家在 [201, 10864] 抽内获取一个 5星领主

;; 注：由于远古池当抽到5星普通英雄后，不会重置保底，这里的样本未必是首次抽到5星英雄的抽数。
(run-statistics/normal-ancient 10000 #f #f)
;; ==== 国服 普通远古召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 50
;; 中位数: 36
;; 最大值: 200
;; 最小值: 1
;; 标准差: 45.7
;; 62.56% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 23.3% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 9.49% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 4.65% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 1.32% 的玩家通过硬保底 200 抽获取一个 5星英雄

(run-statistics/normal-ancient 10000 #t #f)
;; ==== 国服 普通远古召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 99
;; 中位数: 86
;; 最大值: 200
;; 最小值: 1
;; 标准差: 69.7
;; 33.58% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 22.22% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 14.58% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 29.62% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 19.92% 的玩家通过硬保底 200 抽获取一个 5星领主

(run-statistics/normal-divine 10000 #f #f)
;; ==== 国服 普通神圣召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 12
;; 中位数: 12
;; 最大值: 20
;; 最小值: 1
;; 标准差: 7.0
;; 26.37% 的玩家在 [1, 5] 抽内获取一个 5星英雄
;; 18.87% 的玩家在 [6, 10] 抽内获取一个 5星英雄
;; 14.16% 的玩家在 [11, 15] 抽内获取一个 5星英雄
;; 40.6% 的玩家在 [16, 20] 抽内获取一个 5星英雄
;; 31.52% 的玩家通过硬保底 20 抽获取一个 5星英雄

(run-statistics/normal-divine 10000 #t #f)
;; ==== 国服 普通神圣召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 178
;; 中位数: 125
;; 最大值: 1552
;; 最小值: 1
;; 标准差: 173.6
;; 2.05% 的玩家在 [1, 5] 抽内获取一个 5星领主
;; 1.98% 的玩家在 [6, 10] 抽内获取一个 5星领主
;; 1.96% 的玩家在 [11, 15] 抽内获取一个 5星领主
;; 3.95% 的玩家在 [16, 20] 抽内获取一个 5星领主
;; 90.06% 的玩家在 [21, 1552] 抽内获取一个 5星领主

(run-statistics/crazy-divine 10000 #f #f)
;; ==== 国服 狂欢神圣召唤 获取5星英雄的期望（样本量：10000） ====
;; 平均抽数: 8
;; 中位数: 6
;; 最大值: 20
;; 最小值: 1
;; 标准差: 6.0
;; 47.7% 的玩家在 [1, 5] 抽内获取一个 5星英雄
;; 24.58% 的玩家在 [6, 10] 抽内获取一个 5星英雄
;; 12.96% 的玩家在 [11, 15] 抽内获取一个 5星英雄
;; 14.76% 的玩家在 [16, 20] 抽内获取一个 5星英雄
;; 9.04% 的玩家通过硬保底 20 抽获取一个 5星英雄

(run-statistics/crazy-divine 10000 #t #f)
;; ==== 国服 狂欢神圣召唤 获取5星领主的期望（样本量：10000） ====
;; 平均抽数: 115
;; 中位数: 79
;; 最大值: 1191
;; 最小值: 1
;; 标准差: 114.4
;; 4.07% 的玩家在 [1, 5] 抽内获取一个 5星领主
;; 4.11% 的玩家在 [6, 10] 抽内获取一个 5星领主
;; 3.51% 的玩家在 [11, 15] 抽内获取一个 5星领主
;; 3.88% 的玩家在 [16, 20] 抽内获取一个 5星领主
;; 84.43% 的玩家在 [21, 1191] 抽内获取一个 5星领主


;;; 国际服（含软保底）

(run-statistics/normal-spirits 10000 #f #t)
;; ==== 国际服（软保底） 普通英灵召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 122
;; 中位数: 142
;; 最大值: 194
;; 最小值: 1
;; 标准差: 65.9
;; 21.97% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 16.17% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 13.95% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 47.91% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 0.0% 的玩家通过硬保底 200 抽获取一个 5星英雄

(run-statistics/normal-spirits 10000 #t #t)
;; ==== 国际服（软保底） 普通英灵召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 1514
;; 中位数: 1075
;; 最大值: 15974
;; 最小值: 1
;; 标准差: 1449.2
;; 2.01% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 1.67% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 2.27% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 5.15% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 88.9% 的玩家在 [201, 15974] 抽内获取一个 5星领主

(run-statistics/crazy-spirits 10000 #f #t)
;; ==== 国际服（软保底） 狂欢英灵召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 85
;; 中位数: 70
;; 最大值: 193
;; 最小值: 1
;; 标准差: 62.9
;; 39.19% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 23.55% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 14.51% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 22.75% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 0.0% 的玩家通过硬保底 200 抽获取一个 5星英雄

(run-statistics/crazy-spirits 10000 #t #t)
;; ==== 国际服（软保底） 狂欢英灵召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 1061
;; 中位数: 729
;; 最大值: 9037
;; 最小值: 1
;; 标准差: 1034.3
;; 3.93% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 3.43% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 3.87% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 5.21% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 83.56% 的玩家在 [201, 9037] 抽内获取一个 5星领主

(run-statistics/normal-ancient 10000 #f #t)
;; ==== 国际服（软保底） 普通远古召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 50
;; 中位数: 36
;; 最大值: 198
;; 最小值: 1
;; 标准差: 45.1
;; 62.61% 的玩家在 [1, 50] 抽内获取一个 5星英雄
;; 23.62% 的玩家在 [51, 100] 抽内获取一个 5星英雄
;; 8.98% 的玩家在 [101, 150] 抽内获取一个 5星英雄
;; 4.79% 的玩家在 [151, 200] 抽内获取一个 5星英雄
;; 0.0% 的玩家通过硬保底 200 抽获取一个 5星英雄

(run-statistics/normal-ancient 10000 #t #t)
;; ==== 国际服（软保底） 普通远古召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 98
;; 中位数: 88
;; 最大值: 197
;; 最小值: 1
;; 标准差: 66.3
;; 32.12% 的玩家在 [1, 50] 抽内获取一个 5星领主
;; 22.98% 的玩家在 [51, 100] 抽内获取一个 5星领主
;; 14.61% 的玩家在 [101, 150] 抽内获取一个 5星领主
;; 30.29% 的玩家在 [151, 200] 抽内获取一个 5星领主
;; 0.0% 的玩家通过硬保底 200 抽获取一个 5星领主

(run-statistics/normal-divine 10000 #f #t)
;; ==== 国际服（软保底） 普通神圣召唤 获取5星英雄的期望 （样本量：10000） ====
;; 平均抽数: 11
;; 中位数: 12
;; 最大值: 20
;; 最小值: 1
;; 标准差: 6.0
;; 26.99% 的玩家在 [1, 5] 抽内获取一个 5星英雄
;; 19.45% 的玩家在 [6, 10] 抽内获取一个 5星英雄
;; 25.18% 的玩家在 [11, 15] 抽内获取一个 5星英雄
;; 28.38% 的玩家在 [16, 20] 抽内获取一个 5星英雄
;; 5.25% 的玩家通过硬保底 20 抽获取一个 5星英雄

(run-statistics/normal-divine 10000 #t #t)
;; ==== 国际服（软保底） 普通神圣召唤 获取5星领主的期望 （样本量：10000） ====
;; 平均抽数: 164
;; 中位数: 115
;; 最大值: 1391
;; 最小值: 1
;; 标准差: 158.8
;; 2.0% 的玩家在 [1, 5] 抽内获取一个 5星领主
;; 1.81% 的玩家在 [6, 10] 抽内获取一个 5星领主
;; 2.49% 的玩家在 [11, 15] 抽内获取一个 5星领主
;; 4.14% 的玩家在 [16, 20] 抽内获取一个 5星领主
;; 89.56% 的玩家在 [21, 1391] 抽内获取一个 5星领主

(run-statistics/crazy-divine 10000 #f #t)
;; ==== 国际服（软保底） 狂欢神圣召唤 获取5星英雄的期望（样本量：10000） ====
;; 平均抽数: 7
;; 中位数: 6
;; 最大值: 20
;; 最小值: 1
;; 标准差: 5.3
;; 47.45% 的玩家在 [1, 5] 抽内获取一个 5星英雄
;; 25.15% 的玩家在 [6, 10] 抽内获取一个 5星英雄
;; 17.67% 的玩家在 [11, 15] 抽内获取一个 5星英雄
;; 9.73% 的玩家在 [16, 20] 抽内获取一个 5星英雄
;; 1.52% 的玩家通过硬保底 20 抽获取一个 5星英雄

(run-statistics/crazy-divine 10000 #t #t)
;; ==== 国际服（软保底） 狂欢神圣召唤 获取5星领主的期望（样本量：10000） ====
;; 平均抽数: 111
;; 中位数: 78
;; 最大值: 1500
;; 最小值: 1
;; 标准差: 110.9
;; 3.9% 的玩家在 [1, 5] 抽内获取一个 5星领主
;; 3.93% 的玩家在 [6, 10] 抽内获取一个 5星领主
;; 4.13% 的玩家在 [11, 15] 抽内获取一个 5星领主
;; 3.92% 的玩家在 [16, 20] 抽内获取一个 5星领主
;; 84.12% 的玩家在 [21, 1500] 抽内获取一个 5星领主
