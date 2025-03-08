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
(provide (all-defined-out))

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
    (printf "==== 统计 ~a 当抽出限定时，每~a抽数 （样本量：~a） ====\n" pool-name "5星英雄" sample-size)
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
    ;; (printf "最大值的策略细节：~a\n" individual-pulls-per-5-stars-max-detail)
    (printf "最小值: ~a\n" individual-pulls-per-5-stars-min)
    (printf "标准差: ~a\n" individual-pulls-per-5-stars-stddev)
    ;; (printf "debug-detail：~a\n" debug-detail)
    (printf "\n")
    ))


;; (for ([i (in-range 10 200 10)])
;;   (run-statistics-7 10000 0 i))

;; (run-statistics-7 10000 0 190)

;; (run-statistics-7 10000 0 100)
