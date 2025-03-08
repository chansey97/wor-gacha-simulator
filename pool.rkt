#lang racket
(require racket/class)
(require "./structs.rkt")
(require "./utils.rkt")
(provide (all-defined-out))

;; 卡池的抽象基类
(define pool%
  (class object%
    (super-new)
    
    (init-field name)
    (init-field base-rarities)
    (init-field pity-system)

    ;; 从 rarities 表, 构建累积概率分布
    (define/public (build-cumulative rarities)
      (scanl1 + (map rarity-probability rarities)))
    
    ;; 第一次转盘, 决定你抽到的英雄品质
    (define/public (select-rarity rarities)
      ;; 这里使用 (* (inexact->exact (random)) (last cumulative)) 返回 [0, (last cumulative))
      ;; 是因为可能出现软保底先于硬保底达到 100%
      ;; 比如：远古池是从 185 抽开始，每抽5星领主概率提升8%
      ;; 从 186 到 200 共有 15 次机会，每次增加 8%。
      ;; 在第 198 抽时（触发第 13 次软保底时），5星领主英雄的概率已经超过 100%，其他品质英雄的概率都为 0
      ;; （此时，5星领主英雄的概率 1.048=(13 * 0.08)+0.008），因此我们有理由在 [0, 1.048) 中选取，而非 [0, 1)。
      ;; 更极端的情况，比如：神圣池（已普通神圣池为例），是从 12 抽开始，每抽5星英雄概率提升5%
      ;; 从 13 到 20 共有 8 次机会，每次增加 5%。
      ;; 显然，第 20 抽时，还不足以让 5星英雄的概率达到 100%，但为了更好的容错，我们处理这种情况。
      ;; 假设没有硬保底，在第 31 抽时（触发第 19 次软保底时），5星英雄的概率超过了 100%，其他品质英雄的概率都为 0
      ;; （此时，5星英雄的概率 1.01=(19 * 0.05)+0.004+0.056），但是5星领主和5星普通英雄的概率仍然要维持原来的比例
      ;; （事实上，此时 5星领主的概率是 0.06733333333333333 5星普通的概率是 0.9426666666666667）。
      ;; 注：神圣池的例子是不会发生的，因为硬保底保证了第 20 抽必出5星英雄，这里只是解释动机。
      (let* ((cumulative (build-cumulative rarities))
             ;; TODO: Do we really need (inexact->exact (random))?
             (rand (* (inexact->exact (random)) (last cumulative))))
        (for/first ([prob cumulative]
                    [r rarities]
                    #:when (< rand prob))
          r)))

    ;; 第二次转盘, 决定你获得该品质下的具体英雄
    (define/public (select-hero rarity)
      (list-ref (rarity-heroes rarity) (random (length (rarity-heroes rarity)))))
    
    (define/public (pull)
      (let* ((current-pity (get-field current-pity pity-system))
             (rarity (select-rarity base-rarities))
             (hero (select-hero rarity)))
        (set-field! current-pity pity-system (+ current-pity 1))
        (list (card hero rarity))))

    (define/public (reset)
      (set-field! current-pity pity-system 0)
      )))


