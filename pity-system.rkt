#lang racket
(require racket/class)
(provide (all-defined-out))

(define pity-system%
  (class object%
    (super-new)
    (init-field [hard-pity-threshold 199])
    (init-field [soft-pity-threshold 180])
    (init-field [soft-pity-boost #e0.05])
    (init-field [is-soft-pity-on #f])
    (init-field [current-pity 0])
    ))
