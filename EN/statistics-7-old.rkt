#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; International version

;; run-statistics-7: Count the number of pulls per 5-Star Hero when using the dual-pool strategy in
;; Limited Invocation of Spirits and Normal Invocation of Spirits
(define (run-statistics-7 sample-size init-shared-pity start-in-limited-pulls [adjoint-is-crazy #f])
  (printf "run-statistics-7:\n")
  (define limited-pool limited-spirits-pool)
  (define limited-pool-name (get-field name limited-pool))
  (define limited-pool-up-hero (get-field up-hero limited-pool))
  (define adjoint-pool (if adjoint-is-crazy crazy-spirits-pool normal-spirits-pool))
  (define adjoint-pool-name (get-field name adjoint-pool))
  (define shared-pity-system (get-field pity-system limited-pool))

  (define (dual-pool-optimization-strategy)
    (send limited-pool reset)
    (send adjoint-pool reset)
    (set-field! current-pity shared-pity-system init-shared-pity)
    
    (let loop ((pull-count 0)
               (5-star-count 0)
               (status 'start-in-limited)
               (status-chains '((start-in-limited 0))))
      (match status
        ['start-in-limited
         (if (< pull-count start-in-limited-pulls)
             (let* ((cards (send limited-pool pull))
                    (card (first cards))
                    (hero (card-hero card))
                    (rarity (card-rarity card)))
               (if (= (rarity-star rarity) 5)
                   (if (string=? hero limited-pool-up-hero)
                       (list (add1 pull-count) (add1 5-star-count)
                             (append status-chains (list (list status (add1 pull-count)))))
                       (loop (add1 pull-count) (add1 5-star-count) 'continue-in-limited
                             (append status-chains (list (list 'continue-in-limited (add1 pull-count))))))
                   (loop (add1 pull-count) 5-star-count status status-chains)))
             (loop pull-count 5-star-count 'pull-in-adjoint
                   (append status-chains (list (list 'pull-in-adjoint pull-count)))))]
        ['continue-in-limited
         (let* ((cards (send limited-pool pull))
                (card (first cards))
                (hero (card-hero card))
                (rarity (card-rarity card)))
           (if (= (rarity-star rarity) 5)
               (if (string=? hero limited-pool-up-hero)
                   (list (add1 pull-count) (add1 5-star-count)
                         (append status-chains (list (list status (add1 pull-count)))))
                   (loop (add1 pull-count) (add1 5-star-count) status status-chains))
               (loop (add1 pull-count) 5-star-count status status-chains)))]
        ['pull-in-adjoint
         (let* ((cards (send adjoint-pool pull))
                (card (first cards))
                (rarity (card-rarity card)))
           (if (= (rarity-star rarity) 5)
               (loop (add1 pull-count) (add1 5-star-count) 'continue-in-limited
                     (append status-chains (list (list 'continue-in-limited (add1 pull-count)))))
               (loop (add1 pull-count) 5-star-count status status-chains)))]
        )))
  (let* ((samples (for/list ([i (in-range sample-size)]) (dual-pool-optimization-strategy)))
         (lst-of-pull-count (map first samples))
         (lst-of-5-star-count (map second samples))
         (lst-of-status-chains (map third samples))
         (global-pulls-per-5-star (exact->inexact (/ (apply + lst-of-pull-count) (apply + lst-of-5-star-count))))
         (individual-pulls-per-5-star-samples (map / lst-of-pull-count lst-of-5-star-count))
         (individual-pulls-per-5-star-average (exact->inexact (mean individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-median (exact->inexact (median < individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-max (exact->inexact (apply max individual-pulls-per-5-star-samples)))
         ;; (individual-pulls-per-5-star-max-detail
         ;;  (let ((max-index (index-of individual-pulls-per-5-star-samples (exact-round individual-pulls-per-5-star-max))))
         ;;    (list (list-ref lst-of-pull-count max-index)
         ;;          (list-ref lst-of-5-star-count max-index)
         ;;          (list-ref lst-of-status-chains max-index))))
         (individual-pulls-per-5-star-min (exact->inexact (apply min individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-stddev (exact->inexact (stddev individual-pulls-per-5-star-samples)))
         ;; (debug-detail
         ;;  (let ((median-index (index-of individual-pulls-per-5-star-samples 100)))
         ;;    (if median-index
         ;;        (list (list-ref lst-of-pull-count median-index)
         ;;              (list-ref lst-of-5-star-count median-index)
         ;;              (list-ref lst-of-status-chains median-index))
         ;;        #f)))
         )

    (printf "==== Count ~a + ~a the number of pulls per ~a when using the dual-pool strategy (sample-size: ~a) ====\n"
            limited-pool-name adjoint-pool-name "5-Star Hero" sample-size)
    (printf "-- Firstly, pulling in ~a (init-shared-pity:~a)\n"
            limited-pool-name init-shared-pity)
    (printf "If ~a pulls do not yield a 5-Star Hero, switch to ~a until a 5-Star Hero is pulled, then return to ~a and continue pulling until a Limited 5-Star Hero is obtained.\n"
            start-in-limited-pulls adjoint-pool-name limited-pool-name)
    (printf "If ~a pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.\n"
            start-in-limited-pulls)
    (printf "average(macro indicator): ~a\n" global-pulls-per-5-star)
    (printf "average: ~a\n" individual-pulls-per-5-star-average)
    (printf "median: ~a\n" individual-pulls-per-5-star-median)
    (printf "max: ~a\n" individual-pulls-per-5-star-max)
    ;; (printf "max detail: ~a\n" individual-pulls-per-5-star-max-detail)
    (printf "min: ~a\n" individual-pulls-per-5-star-min)
    (printf "stddev: ~a\n" individual-pulls-per-5-star-stddev)
    ;; (printf "debug-detail: ~a\n" debug-detail)
    (printf "\n")
    ))

;; In old international version, Limited Pool's own-pity-threshold is 249 instead of 199
(define backup-own-pity-threshold (get-field own-pity-threshold limited-spirits-pool))
(set-field! own-pity-threshold limited-spirits-pool 249)

;; (printf "#### adjoint pool is normal, start-in-limited-pulls=[10, 190] ####\n\n")
;; (for ([i (in-range 10 200 10)])
;;     (run-statistics-7 10000 0 i))

;; #### adjoint pool is normal, start-in-limited-pulls=[10, 190] ####

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 10 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 10 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 101.6132917451592
;; average: 105.26688773809524
;; median: 105.5
;; max: 190.5
;; min: 1.0
;; stddev: 34.144513395925316

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 20 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 20 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 101.93289961656924
;; average: 105.11895345238095
;; median: 103.75
;; max: 192.0
;; min: 1.0
;; stddev: 33.78178422573721

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 30 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 30 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 100.94885594585885
;; average: 103.30044595238095
;; median: 101.5
;; max: 190.5
;; min: 1.0
;; stddev: 34.872359745442026

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 40 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 40 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 100.0698259250792
;; average: 101.99224825396826
;; median: 99.25
;; max: 190.0
;; min: 1.0
;; stddev: 34.80189697060109

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 50 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 50 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 99.07033345475016
;; average: 100.89351023809523
;; median: 97.33333333333333
;; max: 191.5
;; min: 1.0
;; stddev: 34.465808600785884

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 60 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 60 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 98.48521131537888
;; average: 100.02176753968254
;; median: 97.5
;; max: 190.5
;; min: 1.0
;; stddev: 34.47724997990046

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 70 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 70 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 105.43070022611855
;; average: 111.4199411904762
;; median: 106.0
;; max: 187.5
;; min: 1.0
;; stddev: 45.40349015733706

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 80 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 80 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 105.14080589385055
;; average: 111.0313486904762
;; median: 110.0
;; max: 182.5
;; min: 1.0
;; stddev: 44.17687001222707

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 90 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 90 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 104.10821367030442
;; average: 109.56794238095237
;; median: 108.66666666666667
;; max: 176.5
;; min: 1.0
;; stddev: 42.94384668884369

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 100 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 100 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 101.44126558005753
;; average: 106.7454319047619
;; median: 106.0
;; max: 172.5
;; min: 1.0
;; stddev: 41.26813059394227

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 110 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 110 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 100.88125559839546
;; average: 105.96342214285714
;; median: 107.0
;; max: 168.0
;; min: 1.0
;; stddev: 40.00227189677278

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 120 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 120 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 99.5433507935267
;; average: 104.82704523809524
;; median: 104.33333333333333
;; max: 162.5
;; min: 1.0
;; stddev: 38.70986910403722

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 130 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 130 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 99.06428400095946
;; average: 103.95525634920635
;; median: 101.33333333333333
;; max: 157.5
;; min: 1.0
;; stddev: 37.2463411823237

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 140 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 140 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 97.91524879614768
;; average: 102.91962023809523
;; median: 98.33333333333333
;; max: 152.5
;; min: 1.0
;; stddev: 35.68588606666717

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 150 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 150 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 96.51387086307592
;; average: 101.45497333333333
;; median: 95.33333333333333
;; max: 150.0
;; min: 1.0
;; stddev: 34.635226684979614

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 160 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 160 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 96.1170344146221
;; average: 100.75296642857143
;; median: 93.0
;; max: 160.0
;; min: 1.0
;; stddev: 33.47239875383581

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 170 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 170 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 95.49149582068449
;; average: 100.2222938095238
;; median: 97.5
;; max: 170.0
;; min: 1.0
;; stddev: 32.504321169031144

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 180 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 180 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 94.32751767708464
;; average: 99.10521976190476
;; median: 104.0
;; max: 180.0
;; min: 1.0
;; stddev: 31.875943187301132

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 190 pulls do not yield a 5-Star Hero, switch to Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 190 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 95.52106391355703
;; average: 103.344205
;; median: 112.5
;; max: 190.0
;; min: 1.0
;; stddev: 37.8971847119098








;; (printf "#### adjoint pool is crazy, start-in-limited-pulls=[10, 190] ####\n\n")
;; (for ([i (in-range 10 200 10)])
;;   (run-statistics-7 10000 0 i #t))

;; #### adjoint pool is crazy, start-in-limited-pulls=[10, 190] ####

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 10 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 10 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 92.08376833976834
;; average: 95.17202916666666
;; median: 93.33333333333333
;; max: 189.0
;; min: 1.0
;; stddev: 31.440106407617435

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 20 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 20 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 92.38297872340425
;; average: 95.04449630952381
;; median: 93.33333333333333
;; max: 190.0
;; min: 1.0
;; stddev: 31.764278975227956

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 30 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 30 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 92.909975198892
;; average: 94.92222369047619
;; median: 93.33333333333333
;; max: 192.0
;; min: 1.0
;; stddev: 32.068697476283795

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 40 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 40 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 92.53916240929355
;; average: 94.46348952380953
;; median: 93.33333333333333
;; max: 189.5
;; min: 1.0
;; stddev: 32.157339928085385

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 50 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 50 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 93.1022362100381
;; average: 94.62364146825396
;; median: 93.33333333333333
;; max: 190.0
;; min: 1.0
;; stddev: 31.3769941070764

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 60 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 60 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 92.78497418070134
;; average: 94.37993261904762
;; median: 92.66666666666667
;; max: 191.5
;; min: 1.0
;; stddev: 32.287094147118665

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 70 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 70 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 100.42914334433036
;; average: 106.00306523809523
;; median: 97.0
;; max: 187.0
;; min: 1.0
;; stddev: 43.21811817362476

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 80 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 80 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 100.64259084918095
;; average: 106.10345142857143
;; median: 99.0
;; max: 182.0
;; min: 1.0
;; stddev: 42.01938427193069

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 90 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 90 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 99.70802670510584
;; average: 104.87173714285714
;; median: 98.33333333333333
;; max: 177.0
;; min: 1.0
;; stddev: 40.57546310650413

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 100 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 100 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 99.44603229929128
;; average: 104.37291976190477
;; median: 99.5
;; max: 172.0
;; min: 1.0
;; stddev: 39.94097625201458

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 110 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 110 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 98.87852975495916
;; average: 103.94607928571429
;; median: 100.5
;; max: 167.5
;; min: 1.0
;; stddev: 38.516873561017576

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 120 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 120 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 98.1156117146906
;; average: 102.87829797619048
;; median: 101.0
;; max: 162.0
;; min: 1.0
;; stddev: 37.7984446364755

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 130 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 130 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 98.10137587238285
;; average: 102.96751420634921
;; median: 101.0
;; max: 157.5
;; min: 1.0
;; stddev: 36.62671343254018

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 140 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 140 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 96.69206259578931
;; average: 101.30580523809523
;; median: 98.0
;; max: 152.0
;; min: 1.0
;; stddev: 35.802592115093496

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 150 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 150 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 96.19097292689857
;; average: 101.04002523809524
;; median: 95.33333333333333
;; max: 150.0
;; min: 1.0
;; stddev: 34.46540917526785

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 160 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 160 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 95.53494612726163
;; average: 100.72070904761905
;; median: 92.66666666666667
;; max: 159.0
;; min: 1.0
;; stddev: 32.740611393426846

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 170 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 170 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 94.93798577807178
;; average: 99.75423333333333
;; median: 95.5
;; max: 170.0
;; min: 1.0
;; stddev: 32.47823480082164

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 180 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 180 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 94.27588946026172
;; average: 98.8344
;; median: 100.5
;; max: 180.0
;; min: 1.0
;; stddev: 31.761940392631548

;; run-statistics-7:
;; ==== Count Limited Invocation of Spirits + Crazy Invocation of Spirits the number of pulls per 5-Star Hero when using the dual-pool strategy (sample-size: 10000) ====
;; -- Firstly, pulling in Limited Invocation of Spirits (init-shared-pity:0)
;; If 190 pulls do not yield a 5-Star Hero, switch to Crazy Invocation of Spirits until a 5-Star Hero is pulled, then return to Limited Invocation of Spirits and continue pulling until a Limited 5-Star Hero is obtained.
;; If 190 pulls yield a 5-Star Hero, if it is a Limited 5-Star Hero, then stop; otherwise, continue pulling until a Limited 5-Star Hero is obtained.
;; average(macro indicator): 94.92848753751794
;; average: 103.2927630952381
;; median: 111.0
;; max: 190.0
;; min: 1.0
;; stddev: 38.19750314495345













(set-field! own-pity-threshold limited-spirits-pool backup-own-pity-threshold)

