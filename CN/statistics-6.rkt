#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; CN version

;; run-statistics-6: 统计 限定英灵池，当抽出限定时，每5星英雄抽数
(define (run-statistics-6 sample-size)
  (printf "run-statistics-6:\n")
  (define pool limited-spirits-pool)
  (define pool-name (get-field name pool))
  (define up-hero (get-field up-hero pool))
  (define rarities (get-field base-rarities pool))
  
  (define (pull-until-get-limited)
    (send pool reset)
    (let loop ((pull-count 0)
               (5-star-count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (hero (card-hero card))
             (rarity (card-rarity card)))
        (if (= (rarity-star rarity) 5)
            (if (string=? hero up-hero)
                (list (add1 pull-count) (add1 5-star-count))
                (loop (add1 pull-count) (add1 5-star-count)))
            (loop (add1 pull-count) 5-star-count)))))

  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-limited)))
         (lst-of-pull-count (map first samples))
         (lst-of-5-star-count (map second samples))
         (global-pulls-per-5-star (exact->inexact (/ (apply + lst-of-pull-count) (apply + lst-of-5-star-count))))
         (individual-pulls-per-5-star-samples (map / lst-of-pull-count lst-of-5-star-count))
         (individual-pulls-per-5-star-average (exact->inexact (mean individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-median (exact->inexact (median < individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-max (exact->inexact (apply max individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-min (exact->inexact (apply min individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-stddev (exact->inexact (stddev individual-pulls-per-5-star-samples))))
    (printf "==== 统计 ~a 当抽出限定时，每~a抽数 （样本量：~a） ====\n"
            pool-name "5星英雄" sample-size)
    (printf "平均抽数（宏观指标）: ~a\n" global-pulls-per-5-star)
    (printf "平均抽数: ~a\n" individual-pulls-per-5-star-average)
    (printf "中位数: ~a\n" individual-pulls-per-5-star-median)
    (printf "最大值: ~a\n" individual-pulls-per-5-star-max)
    (printf "最小值: ~a\n" individual-pulls-per-5-star-min)
    (printf "标准差: ~a\n" individual-pulls-per-5-star-stddev)
    ;; (printf "lst-of-pull-count: ~a\n" (remove-duplicates lst-of-pull-count))
    ;; (printf "lst-of-5-star-count ~a\n" (remove-duplicates lst-of-5-star-count))
    (printf "\n")
    ))

(run-statistics-6 10000)

;; run-statistics-6:
;; ==== 统计 限定英灵召唤 当抽出限定时，每5星英雄抽数 （样本量：10000） ====
;; 平均抽数（宏观指标）: 105.11115654205608
;; 平均抽数: 126.1671280952381
;; 中位数: 100.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 61.64051419266993
