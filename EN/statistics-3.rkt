#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; International version

;; run-statistics-3: Count the expected number of pulls to obtain a 5-Star Lord Hero for the first time in
;; Normal Invocation of Spirits, Crazy Invocation of Spirits, Normal Divine Summoning and Crazy Divine Summoning.
(define (run-statistics-3 sample-size)
  (printf "run-statistics-3:\n")
  (define pools (list normal-spirits-pool
                      crazy-spirits-pool
                      normal-divine-pool
                      crazy-divine-pool))
  
  (define (pull-until-get-5-star-lord pool)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (and (= (rarity-star rarity) 5)
                 (rarity-is-lord rarity))
            (add1 count)
            (loop (add1 count))))))
  
  (for ([pool pools])
    (send pool reset)
    (let* ((pity-system (get-field pity-system pool))
           (pool-name (get-field name pool))
           (hard-pity-threshold (get-field hard-pity-threshold pity-system))
           (samples (for/list ([i (in-range sample-size)])
                      (pull-until-get-5-star-lord pool)))
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
      (printf "==== Count ~a, the expected number of pulls to obtain a ~a for the first time (sample-size: ~a). ====\n"
              pool-name "5-Star Lord Hero" sample-size)
      (printf "average: ~a\n" average)
      (printf "median: ~a\n" median)
      (printf "max: ~a\n" max)
      (printf "min: ~a\n" min)
      (printf "stddev: ~a\n" stddev)
      (for ([bin bins]
            [percentage percentages])
        (printf "~a% of players within [~a, ~a] pulls obtain a ~a for the first time.\n"
                percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5-Star Lord Hero"))
      (printf "\n")
      )))

(run-statistics-3 10000)

;; run-statistics-3:
;; ==== Count Invocation of Spirits, the expected number of pulls to obtain a 5-Star Lord Hero for the first time (sample-size: 10000). ====
;; average: 1510.3747
;; median: 1062.0
;; max: 13900.0
;; min: 1.0
;; stddev: 1450.0850283689954
;; 1.99% of players within [1, 50] pulls obtain a 5-Star Lord Hero for the first time.
;; 1.98% of players within [51, 100] pulls obtain a 5-Star Lord Hero for the first time.
;; 1.84% of players within [101, 150] pulls obtain a 5-Star Lord Hero for the first time.
;; 5.18% of players within [151, 200] pulls obtain a 5-Star Lord Hero for the first time.
;; 89.01% of players within [201, 13900] pulls obtain a 5-Star Lord Hero for the first time.

;; ==== Count Crazy Invocation of Spirits, the expected number of pulls to obtain a 5-Star Lord Hero for the first time (sample-size: 10000). ====
;; average: 1034.5453
;; median: 733.0
;; max: 11208.0
;; min: 1.0
;; stddev: 1007.3310728593207
;; 3.9% of players within [1, 50] pulls obtain a 5-Star Lord Hero for the first time.
;; 3.71% of players within [51, 100] pulls obtain a 5-Star Lord Hero for the first time.
;; 3.58% of players within [101, 150] pulls obtain a 5-Star Lord Hero for the first time.
;; 4.95% of players within [151, 200] pulls obtain a 5-Star Lord Hero for the first time.
;; 83.86% of players within [201, 11208] pulls obtain a 5-Star Lord Hero for the first time.

;; ==== Count Normal Divine Summoning, the expected number of pulls to obtain a 5-Star Lord Hero for the first time (sample-size: 10000). ====
;; average: 160.8963
;; median: 114.0
;; max: 1533.0
;; min: 1.0
;; stddev: 154.80049271985538
;; 1.77% of players within [1, 5] pulls obtain a 5-Star Lord Hero for the first time.
;; 1.97% of players within [6, 10] pulls obtain a 5-Star Lord Hero for the first time.
;; 2.92% of players within [11, 15] pulls obtain a 5-Star Lord Hero for the first time.
;; 3.32% of players within [16, 20] pulls obtain a 5-Star Lord Hero for the first time.
;; 90.02% of players within [21, 1533] pulls obtain a 5-Star Lord Hero for the first time.

;; ==== Count Crazy Divine Summoning, the expected number of pulls to obtain a 5-Star Lord Hero for the first time (sample-size: 10000). ====
;; average: 109.7017
;; median: 78.0
;; max: 919.0
;; min: 1.0
;; stddev: 106.45900486623948
;; 3.63% of players within [1, 5] pulls obtain a 5-Star Lord Hero for the first time.
;; 3.66% of players within [6, 10] pulls obtain a 5-Star Lord Hero for the first time.
;; 3.95% of players within [11, 15] pulls obtain a 5-Star Lord Hero for the first time.
;; 4.32% of players within [16, 20] pulls obtain a 5-Star Lord Hero for the first time.
;; 84.44% of players within [21, 919] pulls obtain a 5-Star Lord Hero for the first time.
