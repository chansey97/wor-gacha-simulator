#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; International version

;; run-statistics-1: Count the expected number of pulls to obtain a 5-Star Hero for the first time in
;; Normal Invocation of Spirits, Crazy Invocation of Spirits, Normal Divine Summoning and Crazy Divine Summoning.
(define (run-statistics-1 sample-size)
  (printf "run-statistics-1:\n")
  (define pools (list normal-spirits-pool
                      crazy-spirits-pool
                      normal-divine-pool
                      crazy-divine-pool))
  
  (define (pull-until-get-5-star pool)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (= (rarity-star rarity) 5)
            (add1 count)
            (loop (add1 count))))))

  (for ([pool pools])
    (send pool reset)
    (let* ((pity-system (get-field pity-system pool))
           (pool-name (get-field name pool))
           (hard-pity-threshold (get-field hard-pity-threshold pity-system))
           (samples (for/list ([i (in-range sample-size)])
                      (pull-until-get-5-star pool)))
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
              pool-name "5-Star Hero" sample-size)
      (printf "average: ~a\n" average)
      (printf "median: ~a\n" median)
      (printf "max: ~a\n" max)
      (printf "min: ~a\n" min)
      (printf "stddev: ~a\n" stddev)
      (for ([bin bins]
            [percentage percentages])
        (printf "~a% of players within [~a, ~a] pulls obtain a ~a for the first time.\n"
                percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5-Star Hero"))
      (printf "~a% of players through the hard pity of ~a pulls obtain a ~a for the first time.\n"
              hard-pity-percentage (add1 hard-pity-threshold) "5-Star Hero")
      (printf "\n")
      )))

(run-statistics-1 10000)

;; run-statistics-1:
;; ==== Count Invocation of Spirits, the expected number of pulls to obtain a 5-Star Hero for the first time (sample-size: 10000). ====
;; average: 121.0329
;; median: 139.0
;; max: 195.0
;; min: 1.0
;; stddev: 65.98001680501453
;; 21.87% of players within [1, 50] pulls obtain a 5-Star Hero for the first time.
;; 17.6% of players within [51, 100] pulls obtain a 5-Star Hero for the first time.
;; 13.39% of players within [101, 150] pulls obtain a 5-Star Hero for the first time.
;; 47.14% of players within [151, 200] pulls obtain a 5-Star Hero for the first time.
;; 0.0% of players through the hard pity of 200 pulls obtain a 5-Star Hero for the first time.

;; ==== Count Crazy Invocation of Spirits, the expected number of pulls to obtain a 5-Star Hero for the first time (sample-size: 10000). ====
;; average: 85.5268
;; median: 70.0
;; max: 195.0
;; min: 1.0
;; stddev: 62.89811349921395
;; 39.15% of players within [1, 50] pulls obtain a 5-Star Hero for the first time.
;; 23.19% of players within [51, 100] pulls obtain a 5-Star Hero for the first time.
;; 14.98% of players within [101, 150] pulls obtain a 5-Star Hero for the first time.
;; 22.68% of players within [151, 200] pulls obtain a 5-Star Hero for the first time.
;; 0.0% of players through the hard pity of 200 pulls obtain a 5-Star Hero for the first time.

;; ==== Count Normal Divine Summoning, the expected number of pulls to obtain a 5-Star Hero for the first time (sample-size: 10000). ====
;; average: 10.8303
;; median: 12.0
;; max: 20.0
;; min: 1.0
;; stddev: 6.013277135639102
;; 26.16% of players within [1, 5] pulls obtain a 5-Star Hero for the first time.
;; 20.06% of players within [6, 10] pulls obtain a 5-Star Hero for the first time.
;; 24.67% of players within [11, 15] pulls obtain a 5-Star Hero for the first time.
;; 29.11% of players within [16, 20] pulls obtain a 5-Star Hero for the first time.
;; 5.65% of players through the hard pity of 20 pulls obtain a 5-Star Hero for the first time.

;; ==== Count Crazy Divine Summoning, the expected number of pulls to obtain a 5-Star Hero for the first time (sample-size: 10000). ====
;; average: 7.3344
;; median: 6.0
;; max: 20.0
;; min: 1.0
;; stddev: 5.309348042839158
;; 47.21% of players within [1, 5] pulls obtain a 5-Star Hero for the first time.
;; 25.0% of players within [6, 10] pulls obtain a 5-Star Hero for the first time.
;; 17.71% of players within [11, 15] pulls obtain a 5-Star Hero for the first time.
;; 10.08% of players within [16, 20] pulls obtain a 5-Star Hero for the first time.
;; 1.44% of players through the hard pity of 20 pulls obtain a 5-Star Hero for the first time.

