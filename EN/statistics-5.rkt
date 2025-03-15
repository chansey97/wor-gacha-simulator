#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; International version

;; run-statistics-5: Count the number of pulls per 5-Star Hero when obtaining a surprise reward in
;; Surprise Invocation of Spirits.
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
    (printf "==== Count ~a, the number of pulls per ~a when a surprise reward is obtained (sample size: ~a). ====\n"
            pool-name "5-Star Hero" sample-size)
    (printf "average(macro indicator): ~a\n" global-pulls-per-5-star)
    (printf "average: ~a\n" individual-pulls-per-5-star-average)
    (printf "median: ~a\n" individual-pulls-per-5-star-median)
    (printf "max: ~a\n" individual-pulls-per-5-star-max)
    (printf "min: ~a\n" individual-pulls-per-5-star-min)
    (printf "stddev: ~a\n" individual-pulls-per-5-star-stddev)
    (printf "\n")))

(run-statistics-5 10000)

;; run-statistics-5:
;; ==== Count Surprise Invocation of Spirits, the number of pulls per 5-Star Hero when a surprise reward is obtained (sample size: 10000). ====
;; average(macro indicator): 60.4087
;; average: 60.4087
;; median: 69.5
;; max: 97.5
;; min: 0.5
;; stddev: 33.169594726345395
