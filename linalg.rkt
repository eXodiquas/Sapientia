#lang racket/base

(require racket/match)

(struct vec2d (x y) #:transparent)
(struct mat2x2 (a b c d) #:transparent)

(define (vec2d-random)
  (norm (vec2d (sub1 (* 2 (random))) (sub1 (* 2 (random))))))

(define (v+ v1 v2)
  (match-let ([(vec2d x1 y1) v1]
              [(vec2d x2 y2) v2])
    (vec2d (+ x1 x2) (+ y1 y2))))

(define (v* v s)
  (match-let ([(vec2d x y) v])
    (vec2d (* x s) (* y s))))

(define (λv fn v)
  (match-let ([(vec2d x y) v])
    (vec2d (fn x) (fn y))))

(define (λv-x fn v)
  (match-let ([(vec2d x y) v])
    (vec2d (fn x) y)))

(define (λv-y fn v)
  (match-let ([(vec2d x y) v])
    (vec2d x (fn y))))

(define (mag v)
  (match-let ([(vec2d x y) v])
    (sqrt (+ (* x x) (* y y)))))

(define (norm v)
  (match-let ([(vec2d x y) v])
    (let ([m (mag v)])
      (vec2d (/ x m) (/ y m)))))

(define (matvec* m v)
  (match-let ([(mat2x2 a b c d) m]
              [(vec2d x y) v])
    (vec2d (+ (* a x) (* b y))
           (+ (* c x) (* d y)))))

(define (mat-compose left right)
  (match-let ([(mat2x2 a1 b1 c1 d1) left]
              [(mat2x2 a2 b2 c2 d2) right])
    (mat2x2 (+ (* a1 a2) (* b1 c2))
            (+ (* a1 b2) (* b1 d2))
            (+ (* a2 c1) (* c2 d1))
            (+ (* b2 c1) (* d1 d2)))))

(provide (all-defined-out))
