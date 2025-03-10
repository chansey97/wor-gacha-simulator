#lang racket
(require "./statistics.rkt")

;; International version

(set-field! is-soft-pity-on spirits-pity-system #t)
(set-field! is-soft-pity-on ancient-pity-system #t)
(set-field! is-soft-pity-on divine-pity-system #t)

;; (run-statistics-1 10000)

;; (run-statistics-2 10000)

;; (run-statistics-3 10000)

;; (run-statistics-4 10000)

;; (run-statistics-4 10000 #t)

;; (run-statistics-5 10000)

;; (run-statistics-6 10000)

;; run-statistics-1:
;; ==== 统计 普通英灵召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 120.7561
;; 中位数: 139.0
;; 最大值: 196.0
;; 最小值: 1.0
;; 标准差: 65.8794733797258
;; 21.89% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 17.8% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 13.61% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 46.7% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 0.0% 的玩家通过硬保底 200 抽首次获取一个5星英雄

;; ==== 统计 狂欢英灵召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 83.4917
;; 中位数: 68.0
;; 最大值: 194.0
;; 最小值: 1.0
;; 标准差: 62.32620260460283
;; 39.81% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 24.02% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 14.63% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 21.54% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 0.0% 的玩家通过硬保底 200 抽首次获取一个5星英雄

;; ==== 统计 普通神圣召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 10.7518
;; 中位数: 12.0
;; 最大值: 20.0
;; 最小值: 1.0
;; 标准差: 5.9998830621937955
;; 27.02% 的玩家在 [1, 5] 抽内首次获取一个5星英雄
;; 19.48% 的玩家在 [6, 10] 抽内首次获取一个5星英雄
;; 25.21% 的玩家在 [11, 15] 抽内首次获取一个5星英雄
;; 28.29% 的玩家在 [16, 20] 抽内首次获取一个5星英雄
;; 5.72% 的玩家通过硬保底 20 抽首次获取一个5星英雄

;; ==== 统计 狂欢神圣召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 7.3659
;; 中位数: 6.0
;; 最大值: 20.0
;; 最小值: 1.0
;; 标准差: 5.336067577345699
;; 46.91% 的玩家在 [1, 5] 抽内首次获取一个5星英雄
;; 25.07% 的玩家在 [6, 10] 抽内首次获取一个5星英雄
;; 17.91% 的玩家在 [11, 15] 抽内首次获取一个5星英雄
;; 10.11% 的玩家在 [16, 20] 抽内首次获取一个5星英雄
;; 1.54% 的玩家通过硬保底 20 抽首次获取一个5星英雄

;; run-statistics-2:
;; ==== 统计 普通远古召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 97.9474
;; 中位数: 87.0
;; 最大值: 197.0
;; 最小值: 1.0
;; 标准差: 66.58704553620021
;; 32.93% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 22.28% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 14.71% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 30.08% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 0.0% 的玩家通过硬保底 200 抽首次获取一个5星领主

;; run-statistics-3:
;; ==== 统计 普通英灵召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 1519.8434
;; 中位数: 1059.0
;; 最大值: 13646.0
;; 最小值: 1.0
;; 标准差: 1482.019068998925
;; 1.96% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 2.02% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 1.7% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 5.25% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 89.07% 的玩家在 [201, 13646] 抽内首次获取一个5星领主

;; ==== 统计 狂欢英灵召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 1061.5906
;; 中位数: 741.0
;; 最大值: 12157.0
;; 最小值: 1.0
;; 标准差: 1051.3661329868107
;; 3.8% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 3.57% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 3.53% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 4.95% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 84.15% 的玩家在 [201, 12157] 抽内首次获取一个5星领主

;; ==== 统计 普通神圣召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 162.483
;; 中位数: 114.0
;; 最大值: 1585.0
;; 最小值: 1.0
;; 标准差: 159.8638005021775
;; 1.83% 的玩家在 [1, 5] 抽内首次获取一个5星领主
;; 2.19% 的玩家在 [6, 10] 抽内首次获取一个5星领主
;; 2.82% 的玩家在 [11, 15] 抽内首次获取一个5星领主
;; 3.76% 的玩家在 [16, 20] 抽内首次获取一个5星领主
;; 89.4% 的玩家在 [21, 1585] 抽内首次获取一个5星领主

;; ==== 统计 狂欢神圣召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 108.8731
;; 中位数: 77.0
;; 最大值: 834.0
;; 最小值: 1.0
;; 标准差: 105.40255592911397
;; 4.1% 的玩家在 [1, 5] 抽内首次获取一个5星领主
;; 3.75% 的玩家在 [6, 10] 抽内首次获取一个5星领主
;; 3.99% 的玩家在 [11, 15] 抽内首次获取一个5星领主
;; 4.18% 的玩家在 [16, 20] 抽内首次获取一个5星领主
;; 83.98% 的玩家在 [21, 834] 抽内首次获取一个5星领主

;; run-statistics-4:
;; ==== 统计 普通远古召唤 获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 49.4019
;; 中位数: 37.0
;; 最大值: 195.0
;; 最小值: 1.0
;; 标准差: 44.05435933469013
;; 62.03% 的玩家在 [1, 50] 抽内获取一个5星英雄
;; 24.58% 的玩家在 [51, 100] 抽内获取一个5星英雄
;; 9.33% 的玩家在 [101, 150] 抽内获取一个5星英雄
;; 4.06% 的玩家在 [151, 200] 抽内获取一个5星英雄
;; 0.0% 的玩家通过硬保底 200 抽获取一个5星英雄

;; run-statistics-4:
;; ==== 统计 普通远古召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 53.0974
;; 中位数: 38.0
;; 最大值: 195.0
;; 最小值: 1.0
;; 标准差: 48.13446076606655
;; 60.6% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 23.32% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 9.7% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 6.38% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 0.0% 的玩家通过硬保底 200 抽首次获取一个5星英雄

;; run-statistics-5:
;; ==== 统计 惊喜英灵召唤 当获取惊喜奖励时，每5星英雄抽数 （样本量：10000） ====
;; 平均抽数（宏观指标）: 60.0701
;; 平均抽数: 60.0701
;; 中位数: 68.5
;; 最大值: 97.0
;; 最小值: 0.5
;; 标准差: 33.00585381398275

;; run-statistics-6:
;; ==== 统计 限定英灵召唤 当获取惊喜奖励时，每5星英雄抽数 （样本量：10000） ====
;; 平均抽数（宏观指标）: 88.00980537078098
;; 平均抽数: 95.51752261904763
;; 中位数: 100.0
;; 最大值: 194.0
;; 最小值: 1.0
;; 标准差: 38.16833928137392

;; (for ([i (in-range 10 200 10)])
;;   (run-statistics-7 10000 0 i))

;; (for ([i (in-range 10 200 10)])
;;   (run-statistics-7 10000 0 i #t))

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 10 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 10 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 98.58383721720226
;; 平均抽数: 101.83150476190477
;; 中位数: 100.5
;; 最大值: 190.5
;; 最小值: 1.0
;; 标准差: 34.74188625947182

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 20 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 20 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 107.58198225757113
;; 平均抽数: 113.91707166666667
;; 中位数: 114.0
;; 最大值: 187.5
;; 最小值: 1.0
;; 标准差: 45.137793379694756

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 30 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 30 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 106.64571339660806
;; 平均抽数: 112.46066702380952
;; 中位数: 113.5
;; 最大值: 182.0
;; 最小值: 1.0
;; 标准差: 44.3760189183829

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 40 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 40 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 104.14377310990481
;; 平均抽数: 109.48884642857143
;; 中位数: 110.66666666666667
;; 最大值: 177.0
;; 最小值: 1.0
;; 标准差: 43.268525147720894

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 50 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 50 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 102.61190800544915
;; 平均抽数: 107.14716261904762
;; 中位数: 108.0
;; 最大值: 172.5
;; 最小值: 1.0
;; 标准差: 42.31139893389826

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 60 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 60 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 100.58687713033598
;; 平均抽数: 104.63918892857143
;; 中位数: 105.5
;; 最大值: 167.5
;; 最小值: 1.0
;; 标准差: 40.8897485200856

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 70 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 70 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 100.49417239817141
;; 平均抽数: 104.46269261904762
;; 中位数: 104.0
;; 最大值: 162.0
;; 最小值: 1.0
;; 标准差: 39.67244455119737

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 80 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 80 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 98.63106511355066
;; 平均抽数: 102.10091892857143
;; 中位数: 101.0
;; 最大值: 157.5
;; 最小值: 1.0
;; 标准差: 38.59308044257245

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 90 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 90 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 97.5491747490216
;; 平均抽数: 100.74996666666667
;; 中位数: 100.0
;; 最大值: 152.0
;; 最小值: 1.0
;; 标准差: 36.831640811777284

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 100 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 100 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 95.92484702232181
;; 平均抽数: 99.08115404761905
;; 中位数: 100.0
;; 最大值: 147.0
;; 最小值: 1.0
;; 标准差: 34.898230488631874

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 110 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 110 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 94.0845193019106
;; 平均抽数: 97.18212428571428
;; 中位数: 100.0
;; 最大值: 142.5
;; 最小值: 1.0
;; 标准差: 33.876998033898616

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 120 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 120 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 92.71912779707215
;; 平均抽数: 95.76353714285715
;; 中位数: 100.0
;; 最大值: 137.5
;; 最小值: 1.0
;; 标准差: 32.04287819881204

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 130 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 130 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 92.09253304751697
;; 平均抽数: 94.960925
;; 中位数: 100.0
;; 最大值: 132.5
;; 最小值: 1.0
;; 标准差: 30.36251868221259

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 140 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 140 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 90.53135731068137
;; 平均抽数: 93.25884833333333
;; 中位数: 100.0
;; 最大值: 140.0
;; 最小值: 1.0
;; 标准差: 29.256527118107385

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 150 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 150 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 88.93111595783351
;; 平均抽数: 91.75797714285714
;; 中位数: 100.0
;; 最大值: 150.0
;; 最小值: 1.0
;; 标准差: 28.143818590580825

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 160 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 160 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 87.70332810915461
;; 平均抽数: 90.475645
;; 中位数: 100.0
;; 最大值: 160.0
;; 最小值: 1.0
;; 标准差: 27.453175743751547

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 170 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 170 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 86.65666651186551
;; 平均抽数: 89.64738214285714
;; 中位数: 100.0
;; 最大值: 170.0
;; 最小值: 1.0
;; 标准差: 26.630033634810808

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 180 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 180 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 85.47358774320895
;; 平均抽数: 88.29951333333334
;; 中位数: 100.0
;; 最大值: 180.0
;; 最小值: 1.0
;; 标准差: 26.077028589180426

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 190 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 190 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 88.50081421169504
;; 平均抽数: 95.2406938095238
;; 中位数: 100.0
;; 最大值: 190.0
;; 最小值: 1.0
;; 标准差: 37.01576025605302

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 10 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 10 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 87.8915230128357
;; 平均抽数: 90.69373976190477
;; 中位数: 86.66666666666667
;; 最大值: 192.5
;; 最小值: 1.0
;; 标准差: 32.23018211397874

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 20 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 20 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 96.09166824824445
;; 平均抽数: 101.95645333333333
;; 中位数: 101.0
;; 最大值: 187.0
;; 最小值: 1.0
;; 标准差: 41.40012172435

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 30 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 30 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 96.34450723852288
;; 平均抽数: 101.5780630952381
;; 中位数: 100.66666666666667
;; 最大值: 182.0
;; 最小值: 1.0
;; 标准差: 40.6881110795216

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 40 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 40 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 96.1001148924369
;; 平均抽数: 100.6478338095238
;; 中位数: 100.5
;; 最大值: 177.0
;; 最小值: 1.0
;; 标准差: 40.232184422192574

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 50 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 50 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 95.66582784102256
;; 平均抽数: 100.03550071428572
;; 中位数: 100.0
;; 最大值: 171.5
;; 最小值: 1.0
;; 标准差: 39.10805002238789

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 60 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 60 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 94.72615837215004
;; 平均抽数: 98.49901523809524
;; 中位数: 100.0
;; 最大值: 168.0
;; 最小值: 1.0
;; 标准差: 38.354770004531886

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 70 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 70 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 94.4360530362858
;; 平均抽数: 97.71397595238095
;; 中位数: 100.0
;; 最大值: 162.5
;; 最小值: 1.0
;; 标准差: 37.6128227479541

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 80 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 80 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 93.81957507557944
;; 平均抽数: 97.13046023809524
;; 中位数: 100.0
;; 最大值: 157.0
;; 最小值: 1.0
;; 标准差: 35.70086093901248

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 90 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 90 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 93.28245800552838
;; 平均抽数: 96.45942023809523
;; 中位数: 100.0
;; 最大值: 153.0
;; 最小值: 1.0
;; 标准差: 34.89963326014742

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 100 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 100 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 92.38571612736882
;; 平均抽数: 95.61324976190477
;; 中位数: 100.0
;; 最大值: 147.5
;; 最小值: 1.0
;; 标准差: 33.79887887208888

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 110 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 110 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 91.90373958196972
;; 平均抽数: 94.75606976190477
;; 中位数: 100.0
;; 最大值: 143.5
;; 最小值: 1.0
;; 标准差: 32.54964840205394

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 120 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 120 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 90.67962865650728
;; 平均抽数: 93.57725547619047
;; 中位数: 100.0
;; 最大值: 137.5
;; 最小值: 1.0
;; 标准差: 31.29853183195227

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 130 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 130 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 90.23353854389721
;; 平均抽数: 93.03326595238096
;; 中位数: 100.0
;; 最大值: 132.5
;; 最小值: 1.0
;; 标准差: 29.81335897920613

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 140 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 140 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 89.5151913658625
;; 平均抽数: 92.03171047619048
;; 中位数: 100.0
;; 最大值: 140.0
;; 最小值: 1.0
;; 标准差: 28.788093670855243

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 150 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 150 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 88.43569541852628
;; 平均抽数: 91.20381380952381
;; 中位数: 100.0
;; 最大值: 150.0
;; 最小值: 1.0
;; 标准差: 27.802250675793573

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 160 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 160 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 87.86005359452966
;; 平均抽数: 90.53823833333334
;; 中位数: 100.0
;; 最大值: 160.0
;; 最小值: 1.0
;; 标准差: 27.14460367533435

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 170 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 170 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 86.12340504796498
;; 平均抽数: 88.97736476190477
;; 中位数: 100.0
;; 最大值: 170.0
;; 最小值: 1.0
;; 标准差: 26.907037413565643

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 180 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 180 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 85.96926413048602
;; 平均抽数: 88.5776380952381
;; 中位数: 100.0
;; 最大值: 180.0
;; 最小值: 1.0
;; 标准差: 25.668902158824785

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 190 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 190 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 88.24889987639061
;; 平均抽数: 95.15959214285714
;; 中位数: 100.0
;; 最大值: 190.0
;; 最小值: 1.0
;; 标准差: 37.4968739674438
