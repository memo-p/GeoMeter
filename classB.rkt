#lang racket/gui
(provide (all-defined-out))
(require "interface.rkt")

(define POINT-PEN (make-object pen% (make-object color% 0 0 255) 1 'solid))
(define POINTI2D-PEN (make-object pen% (make-object color% 255 0 0) 2 'solid))
(define SEG-PEN (make-object pen% (make-object color% 0 0 0 ) 1 'solid))
(define FOR-PEN (make-object pen% (make-object color% 0 255 0) 4 'dot))
(define VEC-PEN (make-object pen% (make-object color% 250 128 128) 1 'solid))

(define pt%
  (class* object% (point<%>)
    (init-field (x 0) (y 0) (visible #t));champ initialisable lors de la creation de l'objet
    (define/public (get-x)
      x)
    (define/public (set-x! nx);les set et getteur
      (set! x nx))
    (define/public (get-y)
      y)
    (define/public (set-y! ny)
      (set! y ny))
    (define/public (get-visible)
      visible)
    (define/public (set-visible! nvisible)
      (set! visible nvisible))
    (define/public (egal? pt)
      (and (= (send pt get-x) x) (= (send pt get-y) y)))
    (define/public (affiche dc);la fonction affiche commune a tout les objets qui seront créé qui affiche uniquement si visible!
      (when visible
        (send dc set-pen POINT-PEN)
        (send dc draw-line (- x 2) (- y 2) (+ x 2) (+ y 2))
        (send dc draw-line (- x 2) (+ y 2) (+ x 2) (- y 2))
        (send dc draw-point x y)))
    (super-new)))


(define vecteur%;petite classe pour representer les vecteurs 2d
  (class* object% (vecteur<%>)
    (init-field (dx 0)(dy 0))
    (define/public (get-dx)
      dx)
    (define/public (set-dx! ndx)
      (set! dx ndx))
    (define/public (get-dy)
      dy)
    (define/public (set-dy! ndy)
      (set! dy ndy))
    (super-new)))

(define vecteur2P%;petite classe pour representer les vecteurs 2d
  (class* object% (vecteur<%>)
    (init-field (origine (new pt%)) (pt1 (new pt%)) (pt2 (new pt%))(visible #t))
    (define/public (get-dx)
      (- (send pt2 get-x) (send pt1 get-x)))
    (define/public (get-dy)
      (- (send pt2 get-y) (send pt1 get-y)))
    (define/public (get-pt1)
      pt1)
    (define/public (get-pt2)
      pt2)
    (define/public (set-origine! pt)
      (send origine set-x! (send pt get-x))
      (send origine set-y! (send pt get-y)))
    (define/public (set-visible! b)
      (set! visible b))
    (define/public (get-visible)
      visible)
    (define/public (affiche dc)
      (when visible
        (let* ((xo (send origine get-x)) (yo (send origine get-y)) (x (+ xo (get-dx))) (y (+ yo (get-dy))))
          (send dc set-pen VEC-PEN)
          (send dc draw-line xo yo x y)
          (send dc draw-line (- x 2) (- y 2) (+ x 2) (+ y 2))
          (send dc draw-line (- x 2) (+ y 2) (+ x 2) (- y 2)))))
    (define/public (force dc)
        (let* ((xo (send origine get-x)) (yo (send origine get-y)) (x (+ xo (get-dx))) (y (+ yo (get-dy))))
          (send dc set-pen FOR-PEN)
          (send dc draw-line xo yo x y)
          (send dc draw-line (- x 2) (- y 2) (+ x 2) (+ y 2))
          (send dc draw-line (- x 2) (+ y 2) (+ x 2) (- y 2))))
    (super-new)))

(define poly4%
  (class object%
    (init-field (p1 (new pt%)) (p2 (new pt%)) (p3 (new pt%)) (p4 (new pt%)) (visible #t))
    (define/public (set-visible! b)
      (set! visible b))
    (define/public (get-visible)
      visible)
    (define/public (affiche dc)
      (when visible
        (send dc set-pen SEG-PEN)
        (send dc draw-line (send p1 get-x) (send p1 get-y) (send p2 get-x) (send p2 get-y))
        (send dc draw-line (send p2 get-x) (send p2 get-y) (send p3 get-x) (send p3 get-y))
        (send dc draw-line (send p3 get-x) (send p3 get-y) (send p4 get-x) (send p4 get-y))
        (send dc draw-line (send p4 get-x) (send p4 get-y) (send p1 get-x) (send p1 get-y))))
    (super-new)))

