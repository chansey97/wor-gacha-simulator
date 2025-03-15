#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; CN version

;; run-statistics-5: 统计 惊喜池 当获取惊喜奖励时，每5星英雄抽数
(define (run-statistics-5 sample-size)
  (printf "run-statistics-5:\n")
  (define pool surprise-spirits-pool)
  (define pool-name (get-field name pool))
  
  (define (pull-until-get-surprise-bouns)
    (send pool reset)
    (let loop ((pull-count 0))
      (let* ((heroes (send pool pull)))
        (let ((heroes-count (length heroes)))
          (if (= heroes-count 2) ; in surprise pool, when return 2 heroes, MUST BE 5-star
              (list (add1 pull-count) heroes-count)
              (loop (add1 pull-count)))))))
  
  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-surprise-bouns)))
         (lst-of-pull-count (map first samples))
         (lst-of-5-star-count (map second samples))
         (global-pulls-per-5-star (exact->inexact (/ (apply + lst-of-pull-count) (apply + lst-of-5-star-count))))
         (individual-pulls-per-5-star-samples (map / lst-of-pull-count lst-of-5-star-count))
         (individual-pulls-per-5-star-average (exact->inexact (mean individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-median (exact->inexact (median < individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-max (exact->inexact (apply max individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-min (exact->inexact (apply min individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-stddev (exact->inexact (stddev individual-pulls-per-5-star-samples))))
    (printf "==== 统计 ~a 当获取惊喜奖励时，每~a抽数 （样本量：~a） ====\n"
            pool-name "5星英雄" sample-size)
    (printf "平均抽数（宏观指标）: ~a\n" global-pulls-per-5-star)
    (printf "平均抽数: ~a\n" individual-pulls-per-5-star-average)
    (printf "中位数: ~a\n" individual-pulls-per-5-star-median)
    (printf "最大值: ~a\n" individual-pulls-per-5-star-max)
    (printf "最小值: ~a\n" individual-pulls-per-5-star-min)
    (printf "标准差: ~a\n" individual-pulls-per-5-star-stddev)
    (printf "\n")))

(run-statistics-5 10000)

;; run-statistics-5:
;; ==== 统计 惊喜英灵召唤 当获取惊喜奖励时，每5星英雄抽数 （样本量：10000） ====
;; 平均抽数（宏观指标）: 63.2471
;; 平均抽数: 63.2471
;; 中位数: 69.0
;; 最大值: 100.0
;; 最小值: 0.5
;; 标准差: 35.730230080283555
