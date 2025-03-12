#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; International version

;; run-statistics-7: Count the number of pulls per 5-Stars Hero when using the dual-lego strategy in
;; Limited Invocation of Spirits and Normal Invocation of Spirits
(define (run-statistics-7 sample-size init-shared-pity start-in-limited-pulls [adjoint-is-crazy #f])
  (printf "run-statistics-7:\n")
  (define limited-pool limited-spirits-pool)
  (define limited-pool-name (get-field name limited-pool))
  (define limited-pool-up-hero (get-field up-hero limited-pool))
  (define adjoint-pool (if adjoint-is-crazy crazy-spirits-pool normal-spirits-pool))
  (define adjoint-pool-name (get-field name adjoint-pool))
  (define shared-pity-system (get-field pity-system limited-pool))

  (define (dual-lego-optimization-strategy)
    (send limited-pool reset)
    (send adjoint-pool reset)
    (set-field! current-pity shared-pity-system init-shared-pity)
    
    (let loop ((pull-count 0)
               (5-stars-count 0)
               (status 'start-in-limited)
               (status-chains '((start-in-limited 0))))
      (match status
        ['start-in-limited
         (if (< pull-count start-in-limited-pulls)
             (let* ((cards (send limited-pool pull))
                    (card (first cards))
                    (hero (card-hero card))
                    (rarity (card-rarity card)))
               (if (= (rarity-stars rarity) 5)
                   (if (string=? hero limited-pool-up-hero)
                       (list (add1 pull-count) (add1 5-stars-count)
                             (append status-chains (list (list status (add1 pull-count)))))
                       (loop (add1 pull-count) (add1 5-stars-count) 'continue-in-limited
                             (append status-chains (list (list 'continue-in-limited (add1 pull-count))))))
                   (loop (add1 pull-count) 5-stars-count status status-chains)))
             (loop pull-count 5-stars-count 'pull-in-adjoint
                   (append status-chains (list (list 'pull-in-adjoint pull-count)))))]
        ['continue-in-limited
         (let* ((cards (send limited-pool pull))
                (card (first cards))
                (hero (card-hero card))
                (rarity (card-rarity card)))
           (if (= (rarity-stars rarity) 5)
               (if (string=? hero limited-pool-up-hero)
                   (list (add1 pull-count) (add1 5-stars-count)
                         (append status-chains (list (list status (add1 pull-count)))))
                   (loop (add1 pull-count) (add1 5-stars-count) status status-chains))
               (loop (add1 pull-count) 5-stars-count status status-chains)))]
        ['pull-in-adjoint
         (let* ((cards (send adjoint-pool pull))
                (card (first cards))
                (rarity (card-rarity card)))
           (if (= (rarity-stars rarity) 5)
               (loop (add1 pull-count) (add1 5-stars-count) 'continue-in-limited
                     (append status-chains (list (list 'continue-in-limited (add1 pull-count)))))
               (loop (add1 pull-count) 5-stars-count status status-chains)))]
        )))
  (let* ((samples (for/list ([i (in-range sample-size)]) (dual-lego-optimization-strategy)))
         (lst-of-pull-count (map first samples))
         (lst-of-5-stars-count (map second samples))
         (lst-of-status-chains (map third samples))
         (global-pulls-per-5-stars (exact->inexact (/ (apply + lst-of-pull-count) (apply + lst-of-5-stars-count))))
         (individual-pulls-per-5-stars-samples (map / lst-of-pull-count lst-of-5-stars-count))
         (individual-pulls-per-5-stars-average (exact->inexact (mean individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-median (exact->inexact (median < individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-max (exact->inexact (apply max individual-pulls-per-5-stars-samples)))
         ;; (individual-pulls-per-5-stars-max-detail
         ;;  (let ((max-index (index-of individual-pulls-per-5-stars-samples (exact-round individual-pulls-per-5-stars-max))))
         ;;    (list (list-ref lst-of-pull-count max-index)
         ;;          (list-ref lst-of-5-stars-count max-index)
         ;;          (list-ref lst-of-status-chains max-index))))
         (individual-pulls-per-5-stars-min (exact->inexact (apply min individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-stddev (exact->inexact (stddev individual-pulls-per-5-stars-samples)))
         ;; (debug-detail
         ;;  (let ((median-index (index-of individual-pulls-per-5-stars-samples 100)))
         ;;    (if median-index
         ;;        (list (list-ref lst-of-pull-count median-index)
         ;;              (list-ref lst-of-5-stars-count median-index)
         ;;              (list-ref lst-of-status-chains median-index))
         ;;        #f)))
         )

    (printf "==== Count ~a + ~a the number of pulls per ~a when using the dual-lego strategy (sample-size: ~a) ====\n"
            limited-pool-name adjoint-pool-name "5-Stars Hero" sample-size)
    (printf "-- Firstly, pulling in ~a (init-shared-pity:~a)\n"
            limited-pool-name init-shared-pity)
    (printf "If ~a pulls do not yield a 5-Stars Hero, switch to ~a until a 5-Stars Hero is pulled, then return to ~a and continue pulling until a Limited 5-Stars Hero is obtained.\n"
            start-in-limited-pulls adjoint-pool-name limited-pool-name)
    (printf "If ~a pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.\n"
            start-in-limited-pulls)
    (printf "average(macro indicator): ~a\n" global-pulls-per-5-stars)
    (printf "average: ~a\n" individual-pulls-per-5-stars-average)
    (printf "median: ~a\n" individual-pulls-per-5-stars-median)
    (printf "max: ~a\n" individual-pulls-per-5-stars-max)
    ;; (printf "max detail: ~a\n" individual-pulls-per-5-stars-max-detail)
    (printf "min: ~a\n" individual-pulls-per-5-stars-min)
    (printf "stddev: ~a\n" individual-pulls-per-5-stars-stddev)
    ;; (printf "debug-detail: ~a\n" debug-detail)
    (printf "\n")
    ))

;; Conclusion:
;;
;; Risk Averse (avoid worst case): start-in-limited-pulls = 133
;; Risk Neutral (only pursue expected value maximization): start-in-limited-pulls = 180
;;
;; P.S. 133 can also be computed by math:
;; Assuming x is the start-in-limited-pulls, solve the maximum of f(x) = (x + (200 - x) + (200 -x)) / 2,
;; where x > 100 and (x + (200 - x) + (200 -x)) / 2 >= x
;; Thus 100 < x <= 400/3 = 133.3333
;; Since f(x) is monotonic decreasing, so the maximum of f(x) = f(133.3333) = 133.33333

;; (printf "#### adjoint pool is normal, start-in-limited-pulls=[10, 190] ####\n\n")
;; (for ([i (in-range 10 200 10)])
;;   (run-statistics-7 10000 0 i))

;; #### adjoint pool is normal, start-in-limited-pulls=[10, 190] ####\n

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 10 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 10 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 97.86320943245404
;; average: 101.39227607142857
;; median: 99.0
;; max: 190.0
;; min: 1.0
;; stddev: 34.02685543063373

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 20 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 20 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 106.90013916575771
;; average: 113.9983411904762
;; median: 113.5
;; max: 187.0
;; min: 1.0
;; stddev: 44.36066102256475

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 30 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 30 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 105.16164185905225
;; average: 111.67010142857143
;; median: 112.0
;; max: 183.0
;; min: 1.0
;; stddev: 43.37476107308091

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 40 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 40 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 103.71404714746204
;; average: 109.53626357142858
;; median: 109.5
;; max: 177.0
;; min: 1.0
;; stddev: 42.362445842057376

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 50 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 50 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 102.48265507704464
;; average: 107.78204571428572
;; median: 108.5
;; max: 172.5
;; min: 1.0
;; stddev: 41.79219623418624

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 60 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 60 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 100.95481241759559
;; average: 105.83294166666667
;; median: 106.5
;; max: 167.0
;; min: 1.0
;; stddev: 40.62704864970487

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 70 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 70 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 99.27741179323445
;; average: 104.1861719047619
;; median: 104.0
;; max: 162.5
;; min: 1.0
;; stddev: 39.27123933841328

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 80 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 80 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 98.19247959978676
;; average: 102.58887642857142
;; median: 101.0
;; max: 158.0
;; min: 1.0
;; stddev: 37.839990029060246

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 90 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 90 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 96.39396220103696
;; average: 100.24464857142857
;; median: 100.0
;; max: 153.0
;; min: 1.0
;; stddev: 36.33543512747616

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 100 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 100 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 94.54724674232871
;; average: 98.22830428571429
;; median: 100.0
;; max: 147.5
;; min: 1.0
;; stddev: 34.72932609262863

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 110 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 110 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 93.83403217525243
;; average: 97.61052547619047
;; median: 100.0
;; max: 142.5
;; min: 1.0
;; stddev: 33.24076872855126

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 120 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 120 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 92.46743228602384
;; average: 95.96897142857142
;; median: 100.0
;; max: 137.0
;; min: 1.0
;; stddev: 31.49210727955329

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 130 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 130 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 91.01878669275929
;; average: 94.79100047619048
;; median: 100.0
;; max: 132.0
;; min: 1.0
;; stddev: 29.982177849484856

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 140 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 140 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 89.95900296630805
;; average: 93.376305
;; median: 100.0
;; max: 140.0
;; min: 1.0
;; stddev: 28.693479220843454

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 150 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 150 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 88.50084919996425
;; average: 92.04040833333333
;; median: 100.0
;; max: 150.0
;; min: 1.0
;; stddev: 27.46185050969066

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 160 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 160 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.68505611875453
;; average: 90.70334166666666
;; median: 100.0
;; max: 159.0
;; min: 1.0
;; stddev: 26.336128653850263

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 170 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 170 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.51564215148188
;; average: 89.66478595238095
;; median: 100.0
;; max: 170.0
;; min: 1.0
;; stddev: 25.335639812828596

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 180 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 180 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.10338631651693
;; average: 88.36423214285715
;; median: 100.0
;; max: 180.0
;; min: 1.0
;; stddev: 25.288511641447922

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 190 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 190 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.19630340854536
;; average: 93.72131880952381
;; median: 100.0
;; max: 190.0
;; min: 1.0
;; stddev: 34.945654900875795

;; (printf "#### adjoint pool is normal, start-in-limited-pulls=[160, 199] ####\n\n")
;; (for ([i (in-range 160 200)])
;;   (run-statistics-7 10000 0 i))

;; #### adjoint pool is normal, start-in-limited-pulls=[160, 199] ####

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 160 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 160 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.70596513528237
;; average: 90.79701666666666
;; median: 100.0
;; max: 160.0
;; min: 1.0
;; stddev: 26.641943115857956

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 161 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 161 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.30413998376477
;; average: 90.7166938095238
;; median: 100.0
;; max: 161.0
;; min: 1.0
;; stddev: 26.624041081948413

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 162 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 162 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.42175486205338
;; average: 90.72969833333333
;; median: 100.0
;; max: 162.0
;; min: 1.0
;; stddev: 26.109448859732698

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 163 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 163 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.32054595746611
;; average: 90.80245547619047
;; median: 100.0
;; max: 163.0
;; min: 1.0
;; stddev: 26.06142077877986

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 164 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 164 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.691647753064
;; average: 89.99899
;; median: 100.0
;; max: 164.0
;; min: 1.0
;; stddev: 26.537091600592216

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 165 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 165 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.02032575654462
;; average: 90.36836714285714
;; median: 100.0
;; max: 165.0
;; min: 1.0
;; stddev: 25.91403145290972

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 166 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 166 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.7410653467145
;; average: 90.0825530952381
;; median: 100.0
;; max: 166.0
;; min: 1.0
;; stddev: 25.676660975415864

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 167 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 167 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.61330731175963
;; average: 90.04538547619048
;; median: 100.0
;; max: 167.0
;; min: 1.0
;; stddev: 25.967983054193525

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 168 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 168 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.56214740673339
;; average: 89.94930214285715
;; median: 100.0
;; max: 168.0
;; min: 1.0
;; stddev: 25.817889861800413

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 169 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 169 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.44718342006756
;; average: 89.73016714285714
;; median: 100.0
;; max: 169.0
;; min: 1.0
;; stddev: 25.77477646958108

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 170 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 170 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.98234357224118
;; average: 89.20514714285714
;; median: 100.0
;; max: 170.0
;; min: 1.0
;; stddev: 25.8266487972499

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 171 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 171 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.66590178981184
;; average: 89.76702
;; median: 100.0
;; max: 171.0
;; min: 1.0
;; stddev: 25.543215505122724

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 172 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 172 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.08180239466228
;; average: 89.3115638095238
;; median: 100.0
;; max: 172.0
;; min: 1.0
;; stddev: 25.39891110298008

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 173 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 173 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.4724470663557
;; average: 89.00288809523809
;; median: 100.0
;; max: 173.0
;; min: 1.0
;; stddev: 25.995093409871235

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 174 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 174 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.76059050064185
;; average: 88.97622785714286
;; median: 100.0
;; max: 174.0
;; min: 1.0
;; stddev: 25.469794229503975

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 175 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 175 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.28738759511182
;; average: 89.39884166666667
;; median: 100.0
;; max: 175.0
;; min: 1.0
;; stddev: 25.438027039966006

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 176 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 176 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.60389520696165
;; average: 88.93700476190476
;; median: 100.0
;; max: 176.0
;; min: 1.0
;; stddev: 25.688523816766075

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 177 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 177 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.54376840942562
;; average: 88.61581761904762
;; median: 100.0
;; max: 177.0
;; min: 1.0
;; stddev: 25.252720206883563

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 178 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 178 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.33935614051178
;; average: 88.54563333333333
;; median: 100.0
;; max: 178.0
;; min: 1.0
;; stddev: 25.03321392756698

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 179 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 179 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.44708096199743
;; average: 88.73458547619047
;; median: 100.0
;; max: 179.0
;; min: 1.0
;; stddev: 24.825670187765475

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 180 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 180 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.31213739755042
;; average: 88.65885214285714
;; median: 100.0
;; max: 180.0
;; min: 1.0
;; stddev: 25.394426852551963

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 181 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 181 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 84.98086610897998
;; average: 88.32146666666667
;; median: 100.0
;; max: 181.0
;; min: 1.0
;; stddev: 26.09913205589625

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 182 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 182 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.72779769943651
;; average: 89.599265
;; median: 100.0
;; max: 182.0
;; min: 1.0
;; stddev: 27.177695352148966

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 183 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 183 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.70686109553188
;; average: 89.98901928571429
;; median: 100.0
;; max: 183.0
;; min: 1.0
;; stddev: 28.558448549447935

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 184 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 184 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.80201483139778
;; average: 90.77893166666667
;; median: 100.0
;; max: 184.0
;; min: 1.0
;; stddev: 29.175868806216428

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 185 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 185 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.62251593108331
;; average: 92.01645261904761
;; median: 100.0
;; max: 185.0
;; min: 1.0
;; stddev: 30.988016820420874

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 186 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 186 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.76758163748225
;; average: 92.97580476190477
;; median: 100.0
;; max: 186.0
;; min: 1.0
;; stddev: 32.6682699649372

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 187 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 187 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.92807402461128
;; average: 93.11704428571429
;; median: 100.0
;; max: 187.0
;; min: 1.0
;; stddev: 33.38612879822667

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 188 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 188 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.31517827151015
;; average: 93.80069261904762
;; median: 100.0
;; max: 188.0
;; min: 1.0
;; stddev: 34.32955175806603

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 189 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 189 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.04073154426511
;; average: 93.78575380952381
;; median: 100.0
;; max: 189.0
;; min: 1.0
;; stddev: 34.687044982067974

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 190 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 190 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.38023923444976
;; average: 94.16784928571428
;; median: 100.0
;; max: 190.0
;; min: 1.0
;; stddev: 34.63900169948464

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 191 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 191 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.5307917888563
;; average: 94.54025476190476
;; median: 100.0
;; max: 191.0
;; min: 1.0
;; stddev: 35.664624890343134

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 192 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 192 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.39970203767781
;; average: 94.22792261904762
;; median: 100.0
;; max: 192.0
;; min: 1.0
;; stddev: 35.63795155853139

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 193 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 193 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.43604117334364
;; average: 94.01536547619048
;; median: 100.0
;; max: 193.0
;; min: 1.0
;; stddev: 35.32604545835604

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 194 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 194 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.0753706360593
;; average: 93.8654688095238
;; median: 100.0
;; max: 193.0
;; min: 1.0
;; stddev: 34.737174868563386

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 195 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 195 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.4577762816197
;; average: 94.45578547619047
;; median: 100.0
;; max: 195.0
;; min: 1.0
;; stddev: 35.060795335959746

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 196 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 196 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.11985198712095
;; average: 93.88106714285715
;; median: 100.0
;; max: 194.0
;; min: 1.0
;; stddev: 35.17453925852033

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 197 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 197 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.40247067871563
;; average: 94.26570380952381
;; median: 100.0
;; max: 195.0
;; min: 1.0
;; stddev: 35.55341659474235

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 198 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 198 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.56086056697309
;; average: 94.33864166666666
;; median: 100.0
;; max: 194.0
;; min: 1.0
;; stddev: 35.14328746107281

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 199 pulls do not yield a 5-Stars Hero, switch to Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 199 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.19986566877759
;; average: 94.25051309523809
;; median: 100.0
;; max: 193.0
;; min: 1.0
;; stddev: 36.05627315883669



;; (printf "#### adjoint pool is crazy, start-in-limited-pulls=[10, 190] ####\n\n")
;; (for ([i (in-range 10 200 10)])
;;   (run-statistics-7 10000 0 i #t))

;; #### adjoint pool is crazy, start-in-limited-pulls=[10, 200] ####

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 10 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 10 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.4968388127246
;; average: 90.36833214285714
;; median: 86.0
;; max: 190.5
;; min: 1.0
;; stddev: 31.234011558042543

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 20 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 20 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 95.61828117981734
;; average: 101.78499154761904
;; median: 100.5
;; max: 187.0
;; min: 1.0
;; stddev: 40.651109391149376

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 30 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 30 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 95.68349425986467
;; average: 101.54755761904762
;; median: 101.0
;; max: 182.0
;; min: 1.0
;; stddev: 40.24656997947783

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 40 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 40 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 95.16817212638108
;; average: 100.44516119047618
;; median: 100.0
;; max: 178.0
;; min: 1.0
;; stddev: 39.821970893855436

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 50 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 50 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 95.4340928870955
;; average: 100.1403023015873
;; median: 100.0
;; max: 171.5
;; min: 1.0
;; stddev: 38.849919504178686

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 60 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 60 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 94.64265198659646
;; average: 99.28921214285714
;; median: 100.0
;; max: 167.0
;; min: 1.0
;; stddev: 37.68182275769923

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 70 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 70 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 93.51588496115201
;; average: 97.50629142857143
;; median: 100.0
;; max: 162.5
;; min: 1.0
;; stddev: 36.94174243760153

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 80 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 80 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 93.603772809469
;; average: 97.78619380952381
;; median: 100.0
;; max: 157.0
;; min: 1.0
;; stddev: 35.39353127902482

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 90 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 90 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 92.55820436012648
;; average: 96.38700095238096
;; median: 100.0
;; max: 152.0
;; min: 1.0
;; stddev: 34.29600368589534

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 100 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 100 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 91.75797444792283
;; average: 95.3564376984127
;; median: 100.0
;; max: 147.0
;; min: 1.0
;; stddev: 33.11700983346881

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 110 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 110 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 91.18172523961661
;; average: 95.03448166666666
;; median: 100.0
;; max: 142.5
;; min: 1.0
;; stddev: 31.606541312030778

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 120 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 120 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 90.57092736171872
;; average: 94.02994261904762
;; median: 100.0
;; max: 137.0
;; min: 1.0
;; stddev: 30.894520835745134

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 130 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 130 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 89.83596889743141
;; average: 93.36779547619048
;; median: 100.0
;; max: 132.5
;; min: 1.0
;; stddev: 29.145592760739692

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 140 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 140 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 88.62122954444936
;; average: 91.99258595238095
;; median: 100.0
;; max: 140.0
;; min: 1.0
;; stddev: 28.58305103679709

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 150 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 150 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.93243001520436
;; average: 91.12761595238095
;; median: 100.0
;; max: 150.0
;; min: 1.0
;; stddev: 27.486116232711993

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 160 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 160 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 87.54075926346897
;; average: 90.76420047619048
;; median: 100.0
;; max: 160.0
;; min: 1.0
;; stddev: 26.348128539146956

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 170 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 170 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.7460889395667
;; average: 88.98426428571429
;; median: 100.0
;; max: 170.0
;; min: 1.0
;; stddev: 26.120004993036957

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 180 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 180 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 85.45244370462848
;; average: 88.58090095238096
;; median: 100.0
;; max: 180.0
;; min: 1.0
;; stddev: 25.477265238804094

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Stars Hero when using the dual-lego strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 190 pulls do not yield a 5-Stars Hero, switch to Crazy Invocation of Spirits until a 5-Stars Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Stars Hero is obtained.
;; If 190 pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.
;; average(macro indicator): 86.98136824001529
;; average: 93.57331833333333
;; median: 100.0
;; max: 190.0
;; min: 1.0
;; stddev: 34.94353822962596

