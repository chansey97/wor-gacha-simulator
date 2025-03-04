#lang racket
(require "./structs.rkt")
(provide (all-defined-out))

(define (scanl f q lst)
  (cons q 
        (if (null? lst)
            '()
            (scanl f (f q (car lst)) (cdr lst)))))

(define (scanl1 f lst)
  (if (null? lst)
      '()
      (scanl f (car lst) (cdr lst))))


;; TODO:
;; find-rarity - 从 hero 反查 rarity，性能不是很好
(define (find-rarity rarities hero)
  (findf (λ (r) (member hero (rarity-heroes r))) rarities))

;; TODO: 做了 cache 后，速度更慢了
;; 其实并不需要这个功能，我们只需要返回 hero 的品质，而不需要其中的概率。
;; 因此可以做一张全局表，但是其他地方需要改动。
;; (define find-rarity
;;   (let ((memo (make-hash)))
;;     (λ (rarities hero)
;;       (cond
;;         [(hash-has-key? memo rarities)
;;          (let ((cache (hash-ref memo rarities)))
;;            (hash-ref cache hero))]
;;         [else
;;          (let ((cache (make-hash)))
;;            (for ([r rarities])
;;              (for ([h (rarity-heroes r)])
;;                (hash-set! cache h r)))
;;            (hash-set! memo rarities cache)
;;            (hash-ref cache hero))]))))

;; 增加限定英雄 n=15 或 UP英雄 n=14
(define (add-hero rarities hero stars is-lord n)
  (for/list ([r rarities])
    (if (and (= (rarity-stars r) stars)
             (equal? (rarity-is-lord r) is-lord))
        (struct-copy rarity r
                     [heroes (append
                              (rarity-heroes r)
                              (make-list n hero))])
        r)
    ))

;; 英灵召唤中的5星英雄的获取概率翻倍
;; 注：5星英雄概率增加的部分将从2星英雄那里扣除
(define (make-spirits-crazy rarities)
  (let* ([five-stars (filter (λ (r) (= (rarity-stars r) 5)) rarities)]
         [p5-total (apply + (map rarity-probability five-stars))])
    (map
     (λ (r)
       (cond
         [(= (rarity-stars r) 5)
          (struct-copy rarity r [probability (* (rarity-probability r) #e2)])]
         [(= (rarity-stars r) 2)
          (struct-copy rarity r [probability (- (rarity-probability r) p5-total)])]
         [else r]))
     rarities)))

;; 神圣召唤中的5星英雄的获取概率翻倍
;; 注：5星英雄概率增加的部分将从4星英雄那里扣除（扣除规则匪夷所思，感觉是 HARD CODE 写死）
(define (make-divine-crazy rarities)
  (let* ([five-stars (filter (λ (r) (= (rarity-stars r) 5)) rarities)]
         [p5 (apply + (map rarity-probability five-stars))])
    (when (not (= p5 (+ #e0.0006 #e0.0594)))
      (error "make-divine-crazy: 输入 rarities 中没有5星英雄"))
    (map
     (λ (r)
       (cond
         [(= (rarity-stars r) 5)
          (struct-copy rarity r [probability (* (rarity-probability r) #e2)])]
         [(= (rarity-stars r) 4)
          (cond
            [(rarity-is-lord r)
             (struct-copy rarity r [probability (- (rarity-probability r) #e0.0006)])]
            [else
             (struct-copy rarity r [probability (- (rarity-probability r) #e0.0594)])])]
         [else (error "神圣召唤没有 < 4 星的英雄")]))
     rarities)))
