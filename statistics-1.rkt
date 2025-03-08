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
    (define rarities (get-field base-rarities pool))
    (let loop ((count 0))
      (let* ((heroes (send pool pull))
             (hero (first heroes)))
        (if (= (rarity-stars (find-rarity rarities hero)) 5)
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
      (let* ((heroes (send pool pull))
             (hero (first heroes))
             (rarity (find-rarity rarities hero)))
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
    (define rarities (get-field base-rarities pool))
    (let loop ((count 0))
      (let* ((heroes (send pool pull))
             (hero (first heroes))
             (rarity (find-rarity rarities hero)))
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
      (let* ((heroes (send pool pull))
             (hero (first heroes))
             (rarity (find-rarity rarities hero)))
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
      (let* ((heroes (send pool pull))
             (hero (first heroes))
             (rarity (find-rarity rarities hero)))
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
             (let* ((heroes (send limited-pool pull))
                    (hero (first heroes))
                    (rarity (find-rarity limited-pool-rarities hero)))
               (if (= (rarity-stars rarity) 5)
                   (if (string=? hero limited-pool-up-hero)
                       (list (add1 pull-count) (add1 5-stars-count))
                       (loop (add1 pull-count) (add1 5-stars-count) 'continue-in-limited))
                   (loop (add1 pull-count) 5-stars-count 'start-in-limited)))
             (loop pull-count 5-stars-count 'pull-in-adjoint))]
        ['continue-in-limited
         (let* ((heroes (send limited-pool pull))
                (hero (first heroes))
                (rarity (find-rarity limited-pool-rarities hero)))
           (if (= (rarity-stars rarity) 5)
               (if (string=? hero limited-pool-up-hero)
                   (list (add1 pull-count) (add1 5-stars-count))
                   (loop (add1 pull-count) (add1 5-stars-count) 'continue-in-limited))
               (loop (add1 pull-count) 5-stars-count 'continue-in-limited)))]
        ['pull-in-adjoint
         (let* ((heroes (send adjoint-pool pull))
                (hero (first heroes))
                (rarity (find-rarity adjoint-pool-rarities hero)))
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
;; 平均抽数: 127.8098
;; 中位数: 141.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 71.52571302657527
;; 21.92% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 16.63% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 13.6% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 47.85% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 37.72% 的玩家通过硬保底 200 抽首次获取一个5星英雄

;; ==== 统计 狂欢英灵召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 85.9191
;; 中位数: 69.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 65.52510629667074
;; 39.62% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 24.39% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 14.35% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 21.64% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 12.94% 的玩家通过硬保底 200 抽首次获取一个5星英雄

;; ==== 统计 普通神圣召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 11.7461
;; 中位数: 12.0
;; 最大值: 20.0
;; 最小值: 1.0
;; 标准差: 7.039974061741989
;; 27.54% 的玩家在 [1, 5] 抽内首次获取一个5星英雄
;; 18.73% 的玩家在 [6, 10] 抽内首次获取一个5星英雄
;; 14.58% 的玩家在 [11, 15] 抽内首次获取一个5星英雄
;; 39.15% 的玩家在 [16, 20] 抽内首次获取一个5星英雄
;; 30.67% 的玩家通过硬保底 20 抽首次获取一个5星英雄

;; ==== 统计 狂欢神圣召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 7.6499
;; 中位数: 6.0
;; 最大值: 20.0
;; 最小值: 1.0
;; 标准差: 5.9373335757728825
;; 47.41% 的玩家在 [1, 5] 抽内首次获取一个5星英雄
;; 25.04% 的玩家在 [6, 10] 抽内首次获取一个5星英雄
;; 12.9% 的玩家在 [11, 15] 抽内首次获取一个5星英雄
;; 14.65% 的玩家在 [16, 20] 抽内首次获取一个5星英雄
;; 8.35% 的玩家通过硬保底 20 抽首次获取一个5星英雄

;; run-statistics-2:
;; ==== 统计 普通远古召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 100.1409
;; 中位数: 87.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 69.91314216361613
;; 33.29% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 21.69% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 14.8% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 30.22% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 20.29% 的玩家通过硬保底 200 抽首次获取一个5星领主

;; run-statistics-3:
;; ==== 统计 普通英灵召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 1583.1028
;; 中位数: 1097.0
;; 最大值: 15599.0
;; 最小值: 1.0
;; 标准差: 1541.9417324374356
;; 1.85% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 1.98% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 2.09% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 5.21% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 88.87% 的玩家在 [201, 15599] 抽内首次获取一个5星领主

;; ==== 统计 狂欢英灵召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 1054.4495
;; 中位数: 746.0
;; 最大值: 9728.0
;; 最小值: 1.0
;; 标准差: 1042.5616852012881
;; 3.99% 的玩家在 [1, 50] 抽内首次获取一个5星领主
;; 4.24% 的玩家在 [51, 100] 抽内首次获取一个5星领主
;; 4.04% 的玩家在 [101, 150] 抽内首次获取一个5星领主
;; 4.44% 的玩家在 [151, 200] 抽内首次获取一个5星领主
;; 83.29% 的玩家在 [201, 9728] 抽内首次获取一个5星领主

;; ==== 统计 普通神圣召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 177.8458
;; 中位数: 126.0
;; 最大值: 1570.0
;; 最小值: 1.0
;; 标准差: 171.4315140875796
;; 1.9% 的玩家在 [1, 5] 抽内首次获取一个5星领主
;; 1.93% 的玩家在 [6, 10] 抽内首次获取一个5星领主
;; 2.17% 的玩家在 [11, 15] 抽内首次获取一个5星领主
;; 3.83% 的玩家在 [16, 20] 抽内首次获取一个5星领主
;; 90.17% 的玩家在 [21, 1570] 抽内首次获取一个5星领主

;; ==== 统计 狂欢神圣召唤 首次获取一个5星领主的期望抽数 （样本量：10000） ====
;; 平均抽数: 115.5187
;; 中位数: 81.0
;; 最大值: 1123.0
;; 最小值: 1.0
;; 标准差: 114.92439623643885
;; 4.03% 的玩家在 [1, 5] 抽内首次获取一个5星领主
;; 4.02% 的玩家在 [6, 10] 抽内首次获取一个5星领主
;; 3.48% 的玩家在 [11, 15] 抽内首次获取一个5星领主
;; 3.8% 的玩家在 [16, 20] 抽内首次获取一个5星领主
;; 84.67% 的玩家在 [21, 1123] 抽内首次获取一个5星领主

;; run-statistics-4:
;; ==== 统计 普通远古召唤 获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 50.0192
;; 中位数: 36.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 45.53097880959732
;; 62.48% 的玩家在 [1, 50] 抽内获取一个5星英雄
;; 23.7% 的玩家在 [51, 100] 抽内获取一个5星英雄
;; 9.08% 的玩家在 [101, 150] 抽内获取一个5星英雄
;; 4.74% 的玩家在 [151, 200] 抽内获取一个5星英雄
;; 1.31% 的玩家通过硬保底 200 抽获取一个5星英雄

;; run-statistics-4:
;; ==== 统计 普通远古召唤 首次获取一个5星英雄的期望抽数 （样本量：10000） ====
;; 平均抽数: 55.0489
;; 中位数: 39.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 50.42905024675757
;; 59.24% 的玩家在 [1, 50] 抽内首次获取一个5星英雄
;; 23.99% 的玩家在 [51, 100] 抽内首次获取一个5星英雄
;; 9.51% 的玩家在 [101, 150] 抽内首次获取一个5星英雄
;; 7.26% 的玩家在 [151, 200] 抽内首次获取一个5星英雄
;; 3.16% 的玩家通过硬保底 200 抽首次获取一个5星英雄

;; run-statistics-5:
;; ==== 统计 惊喜英灵召唤 当获取惊喜奖励时，每5星英雄抽数 （样本量：10000） ====
;; 平均抽数（宏观指标）: 62.67075
;; 平均抽数: 62.67075
;; 中位数: 68.0
;; 最大值: 100.0
;; 最小值: 0.5
;; 标准差: 35.86023172593144

;; run-statistics-6:
;; ==== 统计 限定英灵召唤 当获取惊喜奖励时，每5星英雄抽数 （样本量：10000） ====
;; 平均抽数（宏观指标）: 105.40784543325528
;; 平均抽数: 126.44249166666667
;; 中位数: 100.0
;; 最大值: 200.0
;; 最小值: 1.0
;; 标准差: 61.65039891271712

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 10 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 10 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 111.75701780863265
;; 平均抽数: 119.08637523809524
;; 中位数: 117.5
;; 最大值: 194.5
;; 最小值: 1.0
;; 标准差: 46.053768796461455

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 20 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 20 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 109.75279211276285
;; 平均抽数: 116.08293392857144
;; 中位数: 114.0
;; 最大值: 189.5
;; 最小值: 1.0
;; 标准差: 46.44953966990846

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 30 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 30 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 108.51250638080653
;; 平均抽数: 113.87970357142858
;; 中位数: 112.5
;; 最大值: 184.5
;; 最小值: 1.0
;; 标准差: 45.92516650572135

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 40 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 40 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 106.6100790513834
;; 平均抽数: 111.76338369047619
;; 中位数: 111.0
;; 最大值: 179.5
;; 最小值: 1.0
;; 标准差: 44.959854091760214

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 50 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 50 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 104.97058468472093
;; 平均抽数: 109.62204666666666
;; 中位数: 108.5
;; 最大值: 174.5
;; 最小值: 1.0
;; 标准差: 43.979830155399384

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 60 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 60 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 104.70603696098563
;; 平均抽数: 108.74313738095238
;; 中位数: 108.0
;; 最大值: 169.5
;; 最小值: 1.0
;; 标准差: 42.38355055864458

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 70 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 70 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 102.39523232997955
;; 平均抽数: 106.0236819047619
;; 中位数: 105.0
;; 最大值: 164.5
;; 最小值: 1.0
;; 标准差: 41.8830689657995

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 80 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 80 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 101.35231361923712
;; 平均抽数: 104.69070476190477
;; 中位数: 104.0
;; 最大值: 159.5
;; 最小值: 1.0
;; 标准差: 40.245518920653176

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 90 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 90 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 100.20794844253491
;; 平均抽数: 103.08030380952381
;; 中位数: 101.5
;; 最大值: 154.5
;; 最小值: 1.0
;; 标准差: 38.730982513578326

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 100 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 100 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 98.36104975964662
;; 平均抽数: 101.28104214285715
;; 中位数: 100.0
;; 最大值: 149.5
;; 最小值: 1.0
;; 标准差: 37.185717167331134

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 110 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 110 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 97.0336856010568
;; 平均抽数: 100.00999833333333
;; 中位数: 100.0
;; 最大值: 144.5
;; 最小值: 1.0
;; 标准差: 35.519302476963105

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 120 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 120 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 95.85288226129535
;; 平均抽数: 98.46694595238095
;; 中位数: 100.0
;; 最大值: 139.5
;; 最小值: 1.0
;; 标准差: 33.98104979193917

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 130 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 130 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 94.42268688862963
;; 平均抽数: 97.11551833333333
;; 中位数: 100.0
;; 最大值: 134.5
;; 最小值: 1.0
;; 标准差: 32.831144059637396

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 140 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 140 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 94.08212119830377
;; 平均抽数: 96.88571166666667
;; 中位数: 100.0
;; 最大值: 141.0
;; 最小值: 1.0
;; 标准差: 30.755312249072922

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 150 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 150 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 92.00510368292795
;; 平均抽数: 94.73939261904762
;; 中位数: 100.0
;; 最大值: 151.0
;; 最小值: 1.0
;; 标准差: 30.019520130095003

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 160 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 160 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 90.82597366339398
;; 平均抽数: 93.52535714285715
;; 中位数: 100.0
;; 最大值: 161.0
;; 最小值: 1.0
;; 标准差: 29.28940116243556

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 170 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 170 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 90.62900037650603
;; 平均抽数: 93.25697603174604
;; 中位数: 100.0
;; 最大值: 171.0
;; 最小值: 1.0
;; 标准差: 27.525972012408594

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 180 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 180 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 89.27461633781337
;; 平均抽数: 91.85323642857144
;; 中位数: 100.0
;; 最大值: 181.0
;; 最小值: 1.0
;; 标准差: 27.215270245367115

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 普通英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 190 抽没有获得5星英雄，换 普通英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 190 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 87.59085473764767
;; 平均抽数: 90.45271650793651
;; 中位数: 100.0
;; 最大值: 191.0
;; 最小值: 1.0
;; 标准差: 27.061527555458134

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 10 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 10 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 98.0532083145052
;; 平均抽数: 104.30234166666666
;; 中位数: 102.0
;; 最大值: 194.5
;; 最小值: 1.0
;; 标准差: 42.211650114111464

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 20 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 20 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 98.00061190148386
;; 平均抽数: 103.94417666666666
;; 中位数: 101.33333333333333
;; 最大值: 189.5
;; 最小值: 1.0
;; 标准差: 42.3119940249401

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 30 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 30 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 97.63003791876784
;; 平均抽数: 102.68476321428571
;; 中位数: 100.66666666666667
;; 最大值: 184.5
;; 最小值: 1.0
;; 标准差: 41.229561573800744

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 40 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 40 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 97.3902053087543
;; 平均抽数: 102.25166607142857
;; 中位数: 100.5
;; 最大值: 179.5
;; 最小值: 1.0
;; 标准差: 41.25523136313529

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 50 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 50 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 97.38245883733161
;; 平均抽数: 101.48557404761905
;; 中位数: 100.5
;; 最大值: 174.5
;; 最小值: 1.0
;; 标准差: 40.045757409495636

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 60 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 60 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 95.83589220089833
;; 平均抽数: 99.71268357142857
;; 中位数: 100.0
;; 最大值: 169.5
;; 最小值: 1.0
;; 标准差: 39.1827704509165

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 70 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 70 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 95.9010505335714
;; 平均抽数: 99.37888150793651
;; 中位数: 100.0
;; 最大值: 164.5
;; 最小值: 1.0
;; 标准差: 38.38352272078739

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 80 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 80 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 95.57052640501823
;; 平均抽数: 98.55626976190476
;; 中位数: 100.0
;; 最大值: 159.5
;; 最小值: 1.0
;; 标准差: 37.426838797624846

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 90 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 90 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 95.08116772097794
;; 平均抽数: 98.11113928571429
;; 中位数: 100.0
;; 最大值: 154.5
;; 最小值: 1.0
;; 标准差: 36.54396476397777

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 100 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 100 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 94.39042285317358
;; 平均抽数: 97.20461214285714
;; 中位数: 100.0
;; 最大值: 149.5
;; 最小值: 1.0
;; 标准差: 35.02353862053655

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 110 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 110 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 93.45775081332981
;; 平均抽数: 96.22486833333333
;; 中位数: 100.0
;; 最大值: 144.5
;; 最小值: 1.0
;; 标准差: 34.11251264051551

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 120 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 120 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 93.19076198097271
;; 平均抽数: 95.98489880952381
;; 中位数: 100.0
;; 最大值: 139.5
;; 最小值: 1.0
;; 标准差: 32.47236162810327

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 130 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 130 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 92.79228313171741
;; 平均抽数: 95.57048714285715
;; 中位数: 100.0
;; 最大值: 134.5
;; 最小值: 1.0
;; 标准差: 31.21131258208641

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 140 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 140 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 92.2318695335277
;; 平均抽数: 94.94653928571428
;; 中位数: 100.0
;; 最大值: 141.0
;; 最小值: 1.0
;; 标准差: 30.08404893424407

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 150 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 150 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 91.75523148148149
;; 平均抽数: 94.24349428571429
;; 中位数: 100.0
;; 最大值: 151.0
;; 最小值: 1.0
;; 标准差: 29.43515753901928

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 160 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 160 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 90.40805587892899
;; 平均抽数: 93.134785
;; 中位数: 100.0
;; 最大值: 161.0
;; 最小值: 1.0
;; 标准差: 28.300381402581003

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 170 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 170 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 89.3443380807845
;; 平均抽数: 92.30565714285714
;; 中位数: 100.0
;; 最大值: 171.0
;; 最小值: 1.0
;; 标准差: 27.421735893478747

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 180 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 180 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 88.62116397097188
;; 平均抽数: 91.37298428571428
;; 中位数: 100.0
;; 最大值: 181.0
;; 最小值: 1.0
;; 标准差: 27.5193759836362

;; run-statistics-7:
;; ==== 统计 限定英灵召唤 + 狂欢英灵召唤 采用卡双金策略时，每5星英雄抽数 （样本量：10000） ====
;; -- 先在 限定英灵召唤 里抽 （初始共享保底：0）
;; -- 如果 190 抽没有获得5星英雄，换 狂欢英灵召唤 直到抽出5星英雄，再返回 限定英灵召唤 继续抽，直到抽出限定5星英雄
;; -- 如果 190 抽获得了5星英雄，如果是限定5星英雄，则结束，否则继续抽，直到抽出限定5星英雄
;; 平均抽数（宏观指标）: 87.8587451469108
;; 平均抽数: 90.55405833333333
;; 中位数: 100.0
;; 最大值: 191.0
;; 最小值: 1.0
;; 标准差: 26.693356550771153

