#lang racket/base

(define (map-from-to x inp-start inp-end out-start out-end)
  (* (/ (- x inp-start) (- inp-end inp-start))
     (+ (- out-end out-start) out-start)))

(provide (all-defined-out))
