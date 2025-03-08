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

;; 英灵系共享保底
(define spirits-pity-system
  (new pity-system%
       [hard-pity-threshold 199]
       [soft-pity-threshold 180]
       [soft-pity-boost #e0.05]
       [is-soft-pity-on #f]))

;; 远古系共享保底
(define ancient-pity-system
  (new pity-system%
       [hard-pity-threshold 199]
       [soft-pity-threshold 185]
       [soft-pity-boost #e0.08]
       [is-soft-pity-on #f]))

;; 神圣系共享保底
(define divine-pity-system
  (new pity-system%
       [hard-pity-threshold 19]
       [soft-pity-threshold 12]
       [soft-pity-boost #e0.05]
       [is-soft-pity-on #f])) 

;; 普通英灵召唤
(define normal-spirits-rarities spirits-rarities)
(define normal-spirits-pool-name "普通英灵召唤")
(define normal-spirits-pool
  (new spirits-pool%
       [name normal-spirits-pool-name]
       [base-rarities normal-spirits-rarities]
       [pity-system spirits-pity-system]))

;; 特定英灵召唤
(define special-spirits-up-heroes-5-stars-lord '())
(define special-spirits-up-heroes-5-stars '("康斯坦丝" "卡利普索"))
(define special-spirits-up-heroes-4-stars '("艾斯米" "塞蕾妮" "奥西伦"))
(define special-spirits-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) spirits-rarities special-spirits-up-heroes-5-stars-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _ special-spirits-up-heroes-5-stars)
      (foldl (λ (hero rs) (add-hero rs hero 4 #f 14)) _ special-spirits-up-heroes-4-stars)))
(define special-spirits-pool-name "特定英灵召唤")
(define special-spirits-pool
  (new spirits-pool%
       [name special-spirits-pool-name]
       [base-rarities special-spirits-rarities]
       [pity-system spirits-pity-system]))

;; 限定英灵召唤
(define limited-spirits-up-hero-5-stars "貂蝉")
(define limited-spirits-rarities
  (add-hero spirits-rarities limited-spirits-up-hero-5-stars 5 #f 15))
(define limited-spirits-pool-name "限定英灵召唤")
(define limited-spirits-pool
  (new limited-spirits-pool%
       [name limited-spirits-pool-name]
       [base-rarities limited-spirits-rarities]
       [pity-system spirits-pity-system]
       [up-hero limited-spirits-up-hero-5-stars]))

;; 狂欢英灵召唤
(define crazy-spirits-rarities
  (make-spirits-crazy spirits-rarities))
(define crazy-spirits-pool-name "狂欢英灵召唤")
(define crazy-spirits-pool
  (new spirits-pool%
       [name crazy-spirits-pool-name]
       [base-rarities crazy-spirits-rarities]
       [pity-system spirits-pity-system]))

;; 惊喜英灵召唤
(define surprise-spirits-rarities spirits-rarities)
(define surprise-spirits-pool-name "惊喜英灵召唤")
(define surprise-spirits-pool
  (new surprise-spirits-pool%
       [name surprise-spirits-pool-name]
       [base-rarities surprise-spirits-rarities]
       [pity-system spirits-pity-system]))

;; 普通远古召唤
(define normal-ancient-rarities ancient-rarities)
(define normal-ancient-pool-name "普通远古召唤")
(define normal-ancient-pool
  (new ancient-pool%
       [name normal-ancient-pool-name]
       [base-rarities normal-ancient-rarities]
       [pity-system ancient-pity-system]))

;; 特定远古召唤
(define special-ancient-up-heroes-5-stars-lord '("居鲁士"))
(define special-ancient-up-heroes-5-stars '("吕布"))
(define special-ancient-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) ancient-rarities special-ancient-up-heroes-5-stars-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _                special-ancient-up-heroes-5-stars)))
(define special-ancient-pool-name "特定远古召唤")
(define special-ancient-pool
  (new ancient-pool%
       [name special-ancient-pool-name]
       [base-rarities special-ancient-rarities]
       [pity-system ancient-pity-system]))

;; 普通神圣召唤
(define normal-divine-rarities divine-rarities)
(define normal-divine-pool-name "普通神圣召唤")
(define normal-divine-pool
  (new divine-pool%
       [name normal-divine-pool-name]
       [base-rarities normal-divine-rarities]
       [pity-system divine-pity-system]))

;; 特定神圣召唤
(define special-divine-up-heroes-5-stars-lord '())
(define special-divine-up-heroes-5-stars '("康斯坦丝" "卡利普索"))
(define special-divine-up-heroes-4-stars '("艾斯米" "塞蕾妮" "奥西伦"))
(define special-divine-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) divine-rarities special-divine-up-heroes-5-stars-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _ special-divine-up-heroes-5-stars)
      (foldl (λ (hero rs) (add-hero rs hero 4 #f 14)) _ special-divine-up-heroes-4-stars)))
(define special-divine-pool-name "特定神圣召唤")
(define special-divine-pool
  (new divine-pool%
       [name special-divine-pool-name]
       [base-rarities special-divine-rarities]
       [pity-system divine-pity-system]))

;; 狂欢神圣召唤
(define crazy-divine-rarities
  (make-divine-crazy divine-rarities))
(define crazy-divine-pool-name "狂欢神圣召唤")
(define crazy-divine-pool
  (new divine-pool%
       [name crazy-divine-pool-name]
       [base-rarities crazy-divine-rarities]
       [pity-system divine-pity-system]))

(define (split-into-4-segments n)
  (for/list ((i (in-range 5))) (* i (/ n 4))))

;; run-statistics-1: 统计 普通英灵池、狂欢英灵池、普通神圣池、狂欢神圣池 首次获取一个5星英雄的平均抽数
(define (run-statistics-1 sample-size)
  (printf "run-statistics-1:\n")
  (define pools (list normal-spirits-pool
                      crazy-spirits-pool
                      normal-divine-pool
                      crazy-divine-pool))
  
  (define (pull-until-get-5-stars pool)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (= (rarity-stars rarity) 5)
            (add1 count)
            (loop (add1 count))))))

  (for ([pool pools])
    (send pool reset)
    (let* ((pity-system (get-field pity-system pool))
           (pool-name (get-field name pool))
           (hard-pity-threshold (get-field hard-pity-threshold pity-system))
           (samples (for/list ([i (in-range sample-size)])
                      (pull-until-get-5-stars pool)))
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
      (printf "==== 统计 ~a 首次获取一个~a的期望抽数 （样本量：~a） ====\n" pool-name "5星英雄" sample-size)
      (printf "平均抽数: ~a\n" average)
      (printf "中位数: ~a\n" median)
      (printf "最大值: ~a\n" max)
      (printf "最小值: ~a\n" min)
      (printf "标准差: ~a\n" stddev)
      (for ([bin bins]
            [percentage percentages])
        (printf "~a% 的玩家在 [~a, ~a] 抽内首次获取一个~a\n" percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5星英雄"))
      (printf "~a% 的玩家通过硬保底 ~a 抽首次获取一个~a\n" hard-pity-percentage (add1 hard-pity-threshold) "5星英雄")
      (printf "\n")
      )))

;; run-statistics-2: 统计 普通远古池 首次获取一个5星领主的平均抽数
(define (run-statistics-2 sample-size)
  (printf "run-statistics-2:\n")
  (define pool normal-ancient-pool)
  (define pool-name (get-field name pool))
  (define rarities (get-field base-rarities pool))
  (define pity-system (get-field pity-system pool))
  (define hard-pity-threshold (get-field hard-pity-threshold pity-system))
  
  (define (pull-until-get-5-stars-lord)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (and (= (rarity-stars rarity) 5)
                 (rarity-is-lord rarity))
            (add1 count)
            (loop (add1 count))))))

  (send pool reset)
  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-5-stars-lord)))
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
    (printf "==== 统计 ~a 首次获取一个~a的期望抽数 （样本量：~a） ====\n" pool-name "5星领主" sample-size)
    (printf "平均抽数: ~a\n" average)
    (printf "中位数: ~a\n" median)
    (printf "最大值: ~a\n" max)
    (printf "最小值: ~a\n" min)
    (printf "标准差: ~a\n" stddev)
    (for ([bin bins]
          [percentage percentages])
      (printf "~a% 的玩家在 [~a, ~a] 抽内首次获取一个~a\n" percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5星领主"))
    (printf "~a% 的玩家通过硬保底 ~a 抽首次获取一个~a\n" hard-pity-percentage (add1 hard-pity-threshold) "5星领主")
    (printf "\n")
    ))

;; run-statistics-3: 统计 普通英灵池、狂欢英灵池、普通神圣池、狂欢神圣池 首次获取一个5星领主的平均抽数
(define (run-statistics-3 sample-size)
  (printf "run-statistics-3:\n")
  (define pools (list normal-spirits-pool
                      crazy-spirits-pool
                      normal-divine-pool
                      crazy-divine-pool))
  
  (define (pull-until-get-5-stars-lord pool)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (and (= (rarity-stars rarity) 5)
                 (rarity-is-lord rarity))
            (add1 count)
            (loop (add1 count))))))
  
  (for ([pool pools])
    (send pool reset)
    (let* ((pity-system (get-field pity-system pool))
           (pool-name (get-field name pool))
           (hard-pity-threshold (get-field hard-pity-threshold pity-system))
           (samples (for/list ([i (in-range sample-size)])
                      (pull-until-get-5-stars-lord pool)))
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
      (printf "==== 统计 ~a 首次获取一个~a的期望抽数 （样本量：~a） ====\n" pool-name "5星领主" sample-size)
      (printf "平均抽数: ~a\n" average)
      (printf "中位数: ~a\n" median)
      (printf "最大值: ~a\n" max)
      (printf "最小值: ~a\n" min)
      (printf "标准差: ~a\n" stddev)
      (for ([bin bins]
            [percentage percentages])
        (printf "~a% 的玩家在 [~a, ~a] 抽内首次获取一个~a\n" percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5星领主"))
      (printf "\n")
      )))

;; run-statistics-4: 统计 普通远古池 获取一个5星英雄的平均抽数
;; 注：由于远古池当抽到5星普通英雄后，不会重置保底，这里的样本未必是首次抽到5星英雄的抽数，
;; 参数 first-only 强制统计首次。
(define (run-statistics-4 sample-size [first-only #f])
  (printf "run-statistics-4:\n")
  (define pool normal-ancient-pool)
  (define pool-name (get-field name pool))
  (define rarities (get-field base-rarities pool))
  (define pity-system (get-field pity-system pool))
  (define hard-pity-threshold (get-field hard-pity-threshold pity-system))

  (define (pull-until-get-5-stars)
    (when first-only
      (send pool reset))
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (= (rarity-stars rarity) 5)
            (add1 count)
            (loop (add1 count))))))

  (send pool reset)
  (let* ((samples (for/list ([i (in-range sample-size)])
                    (pull-until-get-5-stars)))
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
    (if first-only
        (printf "==== 统计 ~a 首次获取一个~a的期望抽数 （样本量：~a） ====\n" pool-name "5星英雄" sample-size)
        (printf "==== 统计 ~a 获取一个~a的期望抽数 （样本量：~a） ====\n" pool-name "5星英雄" sample-size))
    (printf "平均抽数: ~a\n" average)
    (printf "中位数: ~a\n" median)
    (printf "最大值: ~a\n" max)
    (printf "最小值: ~a\n" min)
    (printf "标准差: ~a\n" stddev)
    (for ([bin bins]
          [percentage percentages])
      (if first-only
          (printf "~a% 的玩家在 [~a, ~a] 抽内首次获取一个~a\n" percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5星英雄")
          (printf "~a% 的玩家在 [~a, ~a] 抽内获取一个~a\n" percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5星英雄")))
    (if first-only
        (printf "~a% 的玩家通过硬保底 ~a 抽首次获取一个~a\n" hard-pity-percentage (add1 hard-pity-threshold) "5星英雄")
        (printf "~a% 的玩家通过硬保底 ~a 抽获取一个~a\n" hard-pity-percentage (add1 hard-pity-threshold) "5星英雄"))
    (printf "\n")
    ))

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
          (if (= heroes-count 2) ; in surprise pool, when return 2 heroes, MUST BE 5-stars
              (list (add1 pull-count) heroes-count)
              (loop (add1 pull-count)))))))
  
  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-surprise-bouns)))
         (lst-of-pull-count (map first samples))
         (lst-of-5-stars-count (map second samples))
         (global-pulls-per-5-stars (exact->inexact (/ (apply + lst-of-pull-count) (apply + lst-of-5-stars-count))))
         (individual-pulls-per-5-stars-samples (map / lst-of-pull-count lst-of-5-stars-count))
         (individual-pulls-per-5-stars-average (exact->inexact (mean individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-median (exact->inexact (median < individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-max (exact->inexact (apply max individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-min (exact->inexact (apply min individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-stddev (exact->inexact (stddev individual-pulls-per-5-stars-samples))))
    (printf "==== 统计 ~a 当获取惊喜奖励时，每~a抽数 （样本量：~a） ====\n" pool-name "5星英雄" sample-size)
    (printf "平均抽数（宏观指标）: ~a\n" global-pulls-per-5-stars)
    (printf "平均抽数: ~a\n" individual-pulls-per-5-stars-average)
    (printf "中位数: ~a\n" individual-pulls-per-5-stars-median)
    (printf "最大值: ~a\n" individual-pulls-per-5-stars-max)
    (printf "最小值: ~a\n" individual-pulls-per-5-stars-min)
    (printf "标准差: ~a\n" individual-pulls-per-5-stars-stddev)
    (printf "\n")))

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
               (5-stars-count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (hero (card-hero card))
             (rarity (card-rarity card)))
        (if (= (rarity-stars rarity) 5)
            (if (string=? hero up-hero)
                (list (add1 pull-count) (add1 5-stars-count))
                (loop (add1 pull-count) (add1 5-stars-count)))
            (loop (add1 pull-count) 5-stars-count)))))

  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-limited)))
         (lst-of-pull-count (map first samples))
         (lst-of-5-stars-count (map second samples))
         (global-pulls-per-5-stars (exact->inexact (/ (apply + lst-of-pull-count) (apply + lst-of-5-stars-count))))
         (individual-pulls-per-5-stars-samples (map / lst-of-pull-count lst-of-5-stars-count))
         (individual-pulls-per-5-stars-average (exact->inexact (mean individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-median (exact->inexact (median < individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-max (exact->inexact (apply max individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-min (exact->inexact (apply min individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-stddev (exact->inexact (stddev individual-pulls-per-5-stars-samples))))
    (printf "==== 统计 ~a 当获取惊喜奖励时，每~a抽数 （样本量：~a） ====\n" pool-name "5星英雄" sample-size)
    (printf "平均抽数（宏观指标）: ~a\n" global-pulls-per-5-stars)
    (printf "平均抽数: ~a\n" individual-pulls-per-5-stars-average)
    (printf "中位数: ~a\n" individual-pulls-per-5-stars-median)
    (printf "最大值: ~a\n" individual-pulls-per-5-stars-max)
    (printf "最小值: ~a\n" individual-pulls-per-5-stars-min)
    (printf "标准差: ~a\n" individual-pulls-per-5-stars-stddev)
    ;; (printf "lst-of-pull-count: ~a\n" (remove-duplicates lst-of-pull-count))
    ;; (printf "lst-of-5-stars-count ~a\n" (remove-duplicates lst-of-5-stars-count))
    (printf "\n")
    ))

;; run-statistics-7: 统计 限定英灵池+普通（狂欢）英灵池，采用卡双金策略时，每5星英雄抽数
(define (run-statistics-7 sample-size init-shared-pity start-in-limited-pulls [adjoint-is-crazy #f])
  (printf "run-statistics-7:\n")
  (define limited-pool limited-spirits-pool)
  (define limited-pool-name (get-field name limited-pool))
  (define limited-pool-rarities (get-field base-rarities limited-pool))
  (define limited-pool-up-hero (get-field up-hero limited-pool))
  (define adjoint-pool (if adjoint-is-crazy crazy-spirits-pool normal-spirits-pool))
  (define adjoint-pool-name (get-field name adjoint-pool))
  (define adjoint-pool-rarities (get-field base-rarities adjoint-pool))
  (define shared-pity-system (get-field pity-system limited-pool))

  (define (dual-lego-optimization-strategy)
    (send limited-pool reset)
    (send adjoint-pool reset)
    (set-field! current-pity shared-pity-system init-shared-pity)
    
    (let loop ((pull-count 0)
               (5-stars-count 0)
               (status 'start-in-limited))
      (match status
        ['start-in-limited
         (if (<= pull-count start-in-limited-pulls)
             (let* ((cards (send limited-pool pull))
                    (card (first cards))
                    (hero (card-hero card))
                    (rarity (card-rarity card)))
               (if (= (rarity-stars rarity) 5)
                   (if (string=? hero limited-pool-up-hero)
                       (list (add1 pull-count) (add1 5-stars-count))
                       (loop (add1 pull-count) (add1 5-stars-count) 'continue-in-limited))
                   (loop (add1 pull-count) 5-stars-count 'start-in-limited)))
             (loop pull-count 5-stars-count 'pull-in-adjoint))]
        ['continue-in-limited
         (let* ((cards (send limited-pool pull))
                (card (first cards))
                (hero (card-hero card))
                (rarity (card-rarity card)))
           (if (= (rarity-stars rarity) 5)
               (if (string=? hero limited-pool-up-hero)
                   (list (add1 pull-count) (add1 5-stars-count))
                   (loop (add1 pull-count) (add1 5-stars-count) 'continue-in-limited))
               (loop (add1 pull-count) 5-stars-count 'continue-in-limited)))]
        ['pull-in-adjoint
         (let* ((cards (send adjoint-pool pull))
                (card (first cards))
                (rarity (card-rarity card)))
           (if (= (rarity-stars rarity) 5)
               (loop (add1 pull-count) (add1 5-stars-count) 'continue-in-limited)
               (loop (add1 pull-count) 5-stars-count 'pull-in-adjoint)))]
        )))
  (let* ((samples (for/list ([i (in-range sample-size)]) (dual-lego-optimization-strategy)))
         (lst-of-pull-count (map first samples))
         (lst-of-5-stars-count (map second samples))
         (global-pulls-per-5-stars (exact->inexact (/ (apply + lst-of-pull-count) (apply + lst-of-5-stars-count))))
         (individual-pulls-per-5-stars-samples (map / lst-of-pull-count lst-of-5-stars-count))
         (individual-pulls-per-5-stars-average (exact->inexact (mean individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-median (exact->inexact (median < individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-max (exact->inexact (apply max individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-min (exact->inexact (apply min individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-stddev (exact->inexact (stddev individual-pulls-per-5-stars-samples))))
    (printf "==== 统计 ~a + ~a 采用卡双金策略时，每~a抽数 （样本量：~a） ====\n"
            limited-pool-name
            adjoint-pool-name
            "5星英雄"
            sample-size)
    (printf "-- 先在 ~a 里抽 （初始共享保底：~a）\n"
            limited-pool-name
            init-shared-pity)
    (printf "-- 如果 ~a 抽没有获得5星英雄，换 ~a 直到抽出5星英雄，再返回 ~a 继续抽，直到抽出限定5星英雄\n"
            start-in-limited-pulls
            adjoint-pool-name
            limited-pool-name)
    (printf "-- 如果 ~a 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄\n"
            start-in-limited-pulls)
    (printf "平均抽数（宏观指标）: ~a\n" global-pulls-per-5-stars)
    (printf "平均抽数: ~a\n" individual-pulls-per-5-stars-average)
    (printf "中位数: ~a\n" individual-pulls-per-5-stars-median)
    (printf "最大值: ~a\n" individual-pulls-per-5-stars-max)
    (printf "最小值: ~a\n" individual-pulls-per-5-stars-min)
    (printf "标准差: ~a\n" individual-pulls-per-5-stars-stddev)
    (printf "\n")
    ))

;; (run-statistics-1 10000)

;; (run-statistics-2 10000)

;; (run-statistics-3 10000)

;; (run-statistics-4 10000)

;; (run-statistics-4 10000 #t)

;; (run-statistics-5 10000)

;; (run-statistics-6 10000)

;; (for ([i (in-range 10 200 10)])
;;   (run-statistics-7 10000 0 i))

;; (for ([i (in-range 10 200 10)])
;;   (run-statistics-7 10000 0 i #t))


;; run-statistics-1:
;; ==== 统计 普通英灵召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 127.6144
;; 中位数: 141.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 71.1063183735454
;; 21.81% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 16.71% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 14.05% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 47.43% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 37.03% 的玩家通过硬保底 200 抽首次获取一个5星英雄

;; ==== 统计 狂欢英灵召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 87.6173
;; 中位数: 70.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 66.6082055058534
;; 38.87% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 24.12% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 14.2% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 22.81% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 14.31% 的玩家通过硬保底 200 抽首次获取一个5星英雄

;; ==== 统计 普通神圣召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 11.7618
;; 中位数: 12.0
;; 最大值: 20.0
;; 最小值: 1.0
;; 标准差: 7.013591145768336
;; 26.96% 的玩家在 [1, 5] 抽内首次获取一个5星英雄
;; 19.4% 的玩家在 [6, 10] 抽内首次获取一个5星英雄
;; 14.67% 的玩家在 [11, 15] 抽内首次获取一个5星英雄
;; 38.97% 的玩家在 [16, 20] 抽内首次获取一个5星英雄
;; 30.71% 的玩家通过硬保底 20 抽首次获取一个5星英雄

;; ==== 统计 狂欢神圣召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 7.66
;; 中位数: 6.0
;; 最大值: 20.0
;; 最小值: 1.0
;; 标准差: 5.9675120443950505
;; 46.93% 的玩家在 [1, 5] 抽内首次获取一个5星英雄
;; 25.69% 的玩家在 [6, 10] 抽内首次获取一个5星英雄
;; 12.63% 的玩家在 [11, 15] 抽内首次获取一个5星英雄
;; 14.75% 的玩家在 [16, 20] 抽内首次获取一个5星英雄
;; 8.94% 的玩家通过硬保底 20 抽首次获取一个5星英雄

;; run-statistics-2:
;; ==== 统计 普通远古召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 100.8628
;; 中位数: 88.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 70.0846400872545
;; 32.83% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 21.59% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 14.98% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 30.6% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 20.43% 的玩家通过硬保底 200 抽首次获取一个5星领主

;; run-statistics-3:
;; ==== 统计 普通英灵召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 1580.4221
;; 中位数: 1110.0
;; 最大值: 13300.0
;; 最小值: 1.0
;; 标准差: 1527.4444958595352
;; 2.25% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 2.05% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 1.83% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 4.8% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 89.07% 的玩家在 [201, 13300] 抽内首次获取一个5星领主

;; ==== 统计 狂欢英灵召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 1080.4828
;; 中位数: 767.0
;; 最大值: 9628.0
;; 最小值: 1.0
;; 标准差: 1045.9451668726042
;; 3.82% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 3.84% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 3.51% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 4.43% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 84.4% 的玩家在 [201, 9628] 抽内首次获取一个5星领主

;; ==== 统计 普通神圣召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 175.6339
;; 中位数: 122.0
;; 最大值: 1592.0
;; 最小值: 1.0
;; 标准差: 170.91714446125644
;; 2.08% 的玩家在 [1, 5] 抽内首次获取一个5星领主
;; 1.94% 的玩家在 [6, 10] 抽内首次获取一个5星领主
;; 1.87% 的玩家在 [11, 15] 抽内首次获取一个5星领主
;; 4.0% 的玩家在 [16, 20] 抽内首次获取一个5星领主
;; 90.11% 的玩家在 [21, 1592] 抽内首次获取一个5星领主

;; ==== 统计 狂欢神圣召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 116.3015
;; 中位数: 81.0
;; 最大值: 935.0
;; 最小值: 1.0
;; 标准差: 114.39920715525086
;; 3.86% 的玩家在 [1, 5] 抽内首次获取一个5星领主
;; 3.99% 的玩家在 [6, 10] 抽内首次获取一个5星领主
;; 3.49% 的玩家在 [11, 15] 抽内首次获取一个5星领主
;; 3.9% 的玩家在 [16, 20] 抽内首次获取一个5星领主
;; 84.76% 的玩家在 [21, 935] 抽内首次获取一个5星领主

;; run-statistics-4:
;; ==== 统计 普通远古召唤 获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 49.8704
;; 中位数: 37.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 45.001715565520385
;; 61.52% 的玩家在 [1, 50] 抽内获取一个5星英雄
;; 24.91% 的玩家在 [51, 100] 抽内获取一个5星英雄
;; 9.19% 的玩家在 [101, 150] 抽内获取一个5星英雄
;; 4.38% 的玩家在 [151, 200] 抽内获取一个5星英雄
;; 1.25% 的玩家通过硬保底 200 抽获取一个5星英雄

;; run-statistics-4:
;; ==== 统计 普通远古召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 54.1242
;; 中位数: 39.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 49.2330638327537
;; 59.33% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 24.69% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 9.63% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 6.35% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 2.91% 的玩家通过硬保底 200 抽首次获取一个5星英雄

;; run-statistics-5:
;; ==== 统计 惊喜英灵召唤 当获取惊喜奖励时，每5星英雄抽数 （样本量：10000） ====
;; 平均抽数（宏观指标）: 63.2471
;; 平均抽数: 63.2471
;; 中位数: 69.0
;; 最大值: 100.0
;; 最小值: 0.5
;; 标准差: 35.730230080283555

;; run-statistics-6:
;; ==== 统计 限定英灵召唤 当获取惊喜奖励时，每5星英雄抽数 （样本量：10000） ====
;; 平均抽数（宏观指标）: 105.11115654205608
;; 平均抽数: 126.1671280952381
;; 中位数: 100.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 61.64051419266993

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 10 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 10 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 111.94845243943918
;; 平均抽数: 119.4401375
;; 中位数: 117.5
;; 最大值: 194.5
;; 最小值: 1.0
;; 标准差: 46.62323151661123

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 20 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 20 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 110.29008870034708
;; 平均抽数: 116.83088
;; 中位数: 115.0
;; 最大值: 189.5
;; 最小值: 1.0
;; 标准差: 46.40815063854684

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 30 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 30 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 108.59830693610049
;; 平均抽数: 114.516585
;; 中位数: 112.0
;; 最大值: 184.5
;; 最小值: 1.0
;; 标准差: 45.46940601474835

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 40 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 40 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 106.42274144531713
;; 平均抽数: 111.65015095238095
;; 中位数: 110.5
;; 最大值: 179.5
;; 最小值: 1.0
;; 标准差: 44.82922421037134

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 50 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 50 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 105.15055799524596
;; 平均抽数: 109.59899464285714
;; 中位数: 108.66666666666667
;; 最大值: 174.5
;; 最小值: 1.0
;; 标准差: 44.14887243450579

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 60 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 60 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 103.7890742931745
;; 平均抽数: 108.02152833333334
;; 中位数: 107.66666666666667
;; 最大值: 169.5
;; 最小值: 1.0
;; 标准差: 42.751525836685225

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 70 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 70 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 102.94018238099223
;; 平均抽数: 106.33745809523809
;; 中位数: 105.66666666666667
;; 最大值: 164.5
;; 最小值: 1.0
;; 标准差: 41.419077960958994

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 80 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 80 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 101.81736236889769
;; 平均抽数: 105.37316464285715
;; 中位数: 104.0
;; 最大值: 159.5
;; 最小值: 1.0
;; 标准差: 39.70146682818315

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 90 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 90 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 99.67939223697651
;; 平均抽数: 103.02050761904762
;; 中位数: 101.5
;; 最大值: 154.5
;; 最小值: 1.0
;; 标准差: 38.42172172667561

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 100 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 100 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 98.85955690703736
;; 平均抽数: 101.83567404761905
;; 中位数: 100.0
;; 最大值: 149.5
;; 最小值: 1.0
;; 标准差: 37.123084586716494

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 110 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 110 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 96.90669006361044
;; 平均抽数: 99.80339880952381
;; 中位数: 100.0
;; 最大值: 144.5
;; 最小值: 1.0
;; 标准差: 35.46542168903838

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 120 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 120 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 95.71025424856902
;; 平均抽数: 98.67105642857143
;; 中位数: 100.0
;; 最大值: 139.5
;; 最小值: 1.0
;; 标准差: 34.026305630610935

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 130 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 130 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 94.88007771903665
;; 平均抽数: 97.4406338095238
;; 中位数: 100.0
;; 最大值: 134.5
;; 最小值: 1.0
;; 标准差: 32.570914563562724

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 140 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 140 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 93.18251331300351
;; 平均抽数: 95.78927
;; 中位数: 100.0
;; 最大值: 141.0
;; 最小值: 1.0
;; 标准差: 31.315338637450676

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 150 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 150 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 92.7853708156094
;; 平均抽数: 95.59888095238095
;; 中位数: 100.0
;; 最大值: 151.0
;; 最小值: 1.0
;; 标准差: 29.833870283114926

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 160 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 160 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 90.86025735974819
;; 平均抽数: 93.70464261904762
;; 中位数: 100.0
;; 最大值: 161.0
;; 最小值: 1.0
;; 标准差: 28.855135990426735

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 170 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 170 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 90.32783563451298
;; 平均抽数: 93.0236876984127
;; 中位数: 100.0
;; 最大值: 171.0
;; 最小值: 1.0
;; 标准差: 27.89293901283906

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 180 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 180 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 89.0061695920417
;; 平均抽数: 91.61365880952381
;; 中位数: 100.0
;; 最大值: 181.0
;; 最小值: 1.0
;; 标准差: 27.292285740392316

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 190 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 190 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 87.9123745819398
;; 平均抽数: 90.81024047619047
;; 中位数: 100.0
;; 最大值: 191.0
;; 最小值: 1.0
;; 标准差: 26.691315236078953

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 10 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 10 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 97.87902255639098
;; 平均抽数: 104.47554904761905
;; 中位数: 102.33333333333333
;; 最大值: 194.5
;; 最小值: 1.0
;; 标准差: 42.26591892215998

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 20 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 20 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 98.28255688806888
;; 平均抽数: 103.99965142857143
;; 中位数: 102.0
;; 最大值: 189.5
;; 最小值: 1.0
;; 标准差: 42.48051620966298

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 30 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 30 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 98.26732905648568
;; 平均抽数: 103.34454964285715
;; 中位数: 100.5
;; 最大值: 184.5
;; 最小值: 1.0
;; 标准差: 42.29135615156388

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 40 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 40 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 96.95911016110517
;; 平均抽数: 101.79149023809524
;; 中位数: 100.0
;; 最大值: 179.5
;; 最小值: 1.0
;; 标准差: 40.84577032261461

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 50 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 50 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 97.19523443504997
;; 平均抽数: 101.36213023809523
;; 中位数: 100.0
;; 最大值: 174.5
;; 最小值: 1.0
;; 标准差: 40.30362711274166

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 60 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 60 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 96.5105314719569
;; 平均抽数: 100.33003690476191
;; 中位数: 100.0
;; 最大值: 169.5
;; 最小值: 1.0
;; 标准差: 39.1594376433935

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 70 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 70 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 96.09630313457262
;; 平均抽数: 99.08866119047619
;; 中位数: 100.0
;; 最大值: 164.5
;; 最小值: 1.0
;; 标准差: 38.358976024652385

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 80 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 80 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 96.45622938340523
;; 平均抽数: 99.77373333333334
;; 中位数: 100.0
;; 最大值: 159.5
;; 最小值: 1.0
;; 标准差: 37.51226367848846

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 90 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 90 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 95.4227492615266
;; 平均抽数: 98.39447547619048
;; 中位数: 100.0
;; 最大值: 154.5
;; 最小值: 1.0
;; 标准差: 35.989010315674776

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 100 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 100 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 94.46778540996435
;; 平均抽数: 97.29312333333333
;; 中位数: 100.0
;; 最大值: 149.5
;; 最小值: 1.0
;; 标准差: 35.13097693695393

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 110 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 110 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 93.98145771562596
;; 平均抽数: 96.51365928571428
;; 中位数: 100.0
;; 最大值: 144.5
;; 最小值: 1.0
;; 标准差: 33.81343817106285

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 120 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 120 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 93.24279908900102
;; 平均抽数: 95.70654
;; 中位数: 100.0
;; 最大值: 139.5
;; 最小值: 1.0
;; 标准差: 32.67831687586604

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 130 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 130 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 92.57225277201839
;; 平均抽数: 95.04530047619048
;; 中位数: 100.0
;; 最大值: 134.5
;; 最小值: 1.0
;; 标准差: 31.434957282005417

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 140 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 140 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 91.70209587614866
;; 平均抽数: 94.52868714285714
;; 中位数: 100.0
;; 最大值: 141.0
;; 最小值: 1.0
;; 标准差: 30.091233976041856

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 150 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 150 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 91.34878363435311
;; 平均抽数: 93.86845166666667
;; 中位数: 100.0
;; 最大值: 151.0
;; 最小值: 1.0
;; 标准差: 29.23367734798339

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 160 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 160 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 90.72998693786154
;; 平均抽数: 93.42860357142857
;; 中位数: 100.0
;; 最大值: 161.0
;; 最小值: 1.0
;; 标准差: 28.384975380170175

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 170 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 170 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 89.4827943810691
;; 平均抽数: 91.987425
;; 中位数: 100.0
;; 最大值: 171.0
;; 最小值: 1.0
;; 标准差: 27.96423458947036

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 180 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 180 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 88.25968790020396
;; 平均抽数: 91.05591
;; 中位数: 100.0
;; 最大值: 181.0
;; 最小值: 1.0
;; 标准差: 27.802179797928513

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 190 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 190 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 88.15732560946174
;; 平均抽数: 90.81255595238095
;; 中位数: 100.0
;; 最大值: 191.0
;; 最小值: 1.0
;; 标准差: 26.994156420245663
