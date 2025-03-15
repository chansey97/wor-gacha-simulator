#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; International version

;; run-statistics-2: Count the expected number of pulls to obtain a 5-Star Lord Hero for the first time in
;; Normal Ancient Summoning.
(define (run-statistics-2 sample-size)
  (printf "run-statistics-2:\n")
  (define pool normal-ancient-pool)
  (define pool-name (get-field name pool))
  (define rarities (get-field base-rarities pool))
  (define pity-system (get-field pity-system pool))
  (define hard-pity-threshold (get-field hard-pity-threshold pity-system))
  
  (define (pull-until-get-5-star-lord)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (and (= (rarity-star rarity) 5)
                 (rarity-is-lord rarity))
            (add1 count)
            (loop (add1 count))))))

  (send pool reset)
  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-5-star-lord)))
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
    (printf "~a% of players through the hard pity of ~a pulls obtain a ~a for the first time.\n"
            hard-pity-percentage (add1 hard-pity-threshold) "5-Star Lord Hero")
    (printf "\n")
    ))

(run-statistics-2 10000)

;; run-statistics-2:
;; ==== Count Normal Ancient Summoning, the expected number of pulls to obtain a 5-Star Lord Hero for the first time (sample-size: 10000). ====
;; average: 97.8617
;; median: 87.0
;; max: 196.0
;; min: 1.0
;; stddev: 66.60414681617054
;; 33.22% of players within [1, 50] pulls obtain a 5-Star Lord Hero for the first time.
;; 21.55% of players within [51, 100] pulls obtain a 5-Star Lord Hero for the first time.
;; 15.18% of players within [101, 150] pulls obtain a 5-Star Lord Hero for the first time.
;; 30.05% of players within [151, 200] pulls obtain a 5-Star Lord Hero for the first time.
;; 0.0% of players through the hard pity of 200 pulls obtain a 5-Star Lord Hero for the first time.
