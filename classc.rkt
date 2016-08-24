#lang racket/gui
(provide (all-defined-out))
(require "interface.rkt")
(require "classB.rkt");recuperation des classe des pt% et poly4%


;crayon et brush pour les cercle
(define CERCLE-PEN (make-object pen% (make-object color% 255 0 0) 1 'solid))
(define CERCLE-BRUSH (make-object brush% (make-object color%) 'transparent))

;http://fr.wikipedia.org/wiki/Distance_entre_deux_points_sur_le_plan_cart%C3%A9sien
(define cercle%;cercle definie par 2 point distinct
  (class* object% (cercle<%>)
    (init-field (centre (new pt%)) (dessus (new pt%)) (visible #t))
    (define/public (get-centre)
      centre)
    (define/public (set-centre! pt)
      (set! centre pt))
    (define/public (get-dessus)
      dessus)
    (define/public (get-rayon) 
      (sqrt (+ (sqr (- (send centre get-x) (send dessus get-x))) (sqr (- (send centre get-y) (send dessus get-y))))))
    (define/public (set-dessus! pt)
      (set! dessus pt))
    (define/public (set-visible! b)
      (set! visible b))
    (define/public (get-visible)
      visible)
    (define/public (affiche dc)
      (when visible
        (send dc set-pen CERCLE-PEN)
        (send dc set-brush CERCLE-BRUSH)
        (let* ((dist (sqrt (+ (sqr (- (send centre get-x) (send dessus get-x))) (sqr (- (send centre get-y) (send dessus get-y)))))) (dist2 (* dist 2)))
          (send dc draw-ellipse (- (send centre get-x) dist) (- (send centre get-y) dist) dist2 dist2))))
    (super-new)))


(define cerclePR% ;cette classe defini les cercle donné par un point et un rayon
  (class* object% (cercle<%>)
    (init-field (centre (new pt%)) (rayon 10)(visible #t))
    (define/public (get-centre)
      centre)
    (define/public (get-rayon)
      rayon)
    (define/public (set-rayon! newRayon)
      (set! rayon newRayon))
    (define/public (get-dessus);generation du point sur le cercle le plus facile
      (new pt% (x (send centre get-x)) (y (+ rayon (send centre get-y)))))
    (define/public (set-visible! b)
      (set! visible b))
    (define/public (get-visible)
      visible)
    (define/public (affiche dc)
      (when visible
        (let* ((dist2 (* rayon 2)))
          (send dc set-pen CERCLE-PEN)
          (send dc set-brush CERCLE-BRUSH)
          (send dc draw-ellipse (- (send centre get-x) rayon) (- (send centre get-y) rayon) dist2 dist2))))
    (super-new)))

;http://math.15873.pagesperso-orange.fr/Cercl3p.html
(define cercle3P%;class des cercle defini par 3pt%
  (class* object% (cercle<%>)
    (init-field (a (new pt%)) (b (new pt%)) (c (new pt%)) (visible #t))
    (define xc 0);coordonnée du centre du cercle
    (define yc 0);coordonné du centre du cercle
    (define/public (get-a)
      a)
    (define/public (set-a! pt)
      (set! a pt))
    (define/public (get-b)
      b)
    (define/public (set-b! pt)
      (set! b pt))
    (define/public (get-c)
      c)
    (define/public (set-c! pt)
      (set! c pt))
    (define/public (set-visible! b)
      (set! visible b))
    (define/public (get-visible)
      visible)
    (define/public (get-centre);meme si la fonction na pas de centre lors de sa creation l'utilisateur ne vera le cercle qu'un fois son affichage complet donc on peu lui renvoyer xc yc
      (new ptC3P% (a (get-a)) (b (get-b)) (c (get-c))))
    (define/public (get-rayon)
      (let ((A (get-centre))(B a))
        (sqrt (+ (sqr (- (send A get-x) (send B get-x))) (sqr (- (send A get-y) (send B get-y)))))))
    (define/public (get-dessus);on aurai autant pu metre b ou c 
      a)
    (define/public (affiche dc)
      (when visible
        (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (e) 
                                                             (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (e) 
                                                                                                                  (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (e) void)));donc les points son alignées
                                                                                                                    (let ((yc-yb (- (send b get-y) (send a get-y)));troisieme essais
                                                                                                                          (ya-yb (- (send c get-y) (send a get-y)));on decale d'un cran encore a->c b->a c->b
                                                                                                                          (xc-xb (- (send b get-x) (send a get-x)))
                                                                                                                          (xc-xbC (- (sqr (send b get-x)) (sqr (send a get-x))))
                                                                                                                          (xa-xb (- (send c get-x) (send a get-x)))
                                                                                                                          (xa-xbC (- (sqr (send c get-x)) (sqr (send a get-x))))
                                                                                                                          (yc-ya (- (send b get-y) (send c get-y)))
                                                                                                                          (yc+yb (+ (send b get-y) (send a get-y))))
                                                                                                                      (set! xc (/ 
                                                                                                                                (+ (- (/ xc-xbC  yc-yb) (/ xa-xbC  ya-yb)) yc-ya)
                                                                                                                                (- (* 2 (/ xc-xb yc-yb))
                                                                                                                                   (* 2 (/ xa-xb ya-yb)))))
                                                                                                                      (set! yc  (/ (+ (* -2 xc 
                                                                                                                                         (/ xc-xb yc-yb))
                                                                                                                                      (/ xc-xbC yc-yb) yc+yb) 2))
                                                                                                                      ;          (printf "xc = ~a et yc = ~a" xc yc)
                                                                                                                      (let* ((dist (sqrt (+ (sqr (- xc (send a get-x))) (sqr (- yc (send a get-y)))))) (dist2 (* dist 2)))
                                                                                                                        (send dc set-pen CERCLE-PEN)
                                                                                                                        (send dc set-brush CERCLE-BRUSH)
                                                                                                                        (send dc draw-ellipse (- xc dist) (- yc dist) dist2 dist2)))))))
                                                               (let ((yc-yb (- (send a get-y) (send c get-y)));deuxieme essais on decale d'un cran a->b b->c c->a
                                                                     (ya-yb (- (send b get-y) (send c get-y)))
                                                                     (xc-xb (- (send a get-x) (send c get-x)))
                                                                     (xc-xbC (- (sqr (send a get-x)) (sqr (send c get-x))))
                                                                     (xa-xb (- (send b get-x) (send c get-x)))
                                                                     (xa-xbC (- (sqr (send b get-x)) (sqr (send c get-x))))
                                                                     (yc-ya (- (send a get-y) (send b get-y)))
                                                                     (yc+yb (+ (send a get-y) (send c get-y))))
                                                                 (set! xc (/ 
                                                                           (+ (- (/ xc-xbC  yc-yb) (/ xa-xbC  ya-yb)) yc-ya)
                                                                           (- (* 2 (/ xc-xb yc-yb))
                                                                              (* 2 (/ xa-xb ya-yb)))))
                                                                 (set! yc  (/ (+ (* -2 xc 
                                                                                    (/ xc-xb yc-yb))
                                                                                 (/ xc-xbC yc-yb) yc+yb) 2))
                                                                 ;          (printf "xc = ~a et yc = ~a" xc yc)
                                                                 (let* ((dist (sqrt (+ (sqr (- xc (send c get-x))) (sqr (- yc (send c get-y)))))) (dist2 (* dist 2)))
                                                                   (send dc set-pen CERCLE-PEN)
                                                                   (send dc set-brush CERCLE-BRUSH)
                                                                   (send dc draw-ellipse (- xc dist) (- yc dist) dist2 dist2))))))) 
          (let ((yc-yb (- (send c get-y) (send b get-y)));premier essais 
                (ya-yb (- (send a get-y) (send b get-y)))
                (xc-xb (- (send c get-x) (send b get-x)))
                (xc-xbC (- (sqr (send c get-x)) (sqr (send b get-x))))
                (xa-xb (- (send a get-x) (send b get-x)))
                (xa-xbC (- (sqr (send a get-x)) (sqr (send b get-x))))
                (yc-ya (- (send c get-y) (send a get-y)))
                (yc+yb (+ (send c get-y) (send b get-y))))
            (set! xc (/ 
                      (+ (- (/ xc-xbC  yc-yb) (/ xa-xbC  ya-yb)) yc-ya)
                      (- (* 2 (/ xc-xb yc-yb))
                         (* 2 (/ xa-xb ya-yb)))))
            (set! yc  (/ (+ (* -2 xc 
                               (/ xc-xb yc-yb))
                            (/ xc-xbC yc-yb) yc+yb) 2))
            ;          (printf "xc = ~a et yc = ~a" xc yc)
            (let* ((dist (sqrt (+ (sqr (- xc (send b get-x))) (sqr (- yc (send b get-y)))))) (dist2 (* dist 2)))
              (send dc set-pen CERCLE-PEN)
              (send dc set-brush CERCLE-BRUSH)
              (send dc draw-ellipse (- xc dist) (- yc dist) dist2 dist2))))))
    (super-new)))

(define ptC3P%;classe des point centre d'un cercle defini par 3 points
  (class* object% (point<%>)
    (init-field (a (new pt%)) (b (new pt%)) (c (new pt%)) (visible #t))
    (define/public (get-visible)
      visible)
    (define/public (get-x)
      (let* ((xa (send a get-x))
             (ya (send a get-y))
             (xb (send b get-x))
             (yb (send b get-y))
             (xc (send c get-x))
             (yc (send c get-y))
             (yc-yb (- yc yb))
             (ya-yb (- ya yb))
             (xc-xb (- xc xb))
             (xc-xbC (- (sqr xc) (sqr xb)))
             (xa-xb (- xa xb))
             (xa-xbC (- (sqr xa) (sqr xb)))
             (yc-ya (- yc ya))
             (yc+yb (+ yc yb)))
        (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (e) 8000))) 
          (/ 
           (+ (- (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (e) 8000))) (/ xc-xbC  yc-yb));si on a une division par zero on simule pour la fluidité !
                 (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (e) 8000))) (/ xa-xbC  ya-yb))) yc-ya)
           (- (* 2 (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (e) 8000))) (/ xc-xb yc-yb)))
              (* 2 (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (e) 8000))) (/ xa-xb ya-yb))))))))
    (define/public (get-y)
      (let ((xc (get-x))
            (yc-yb (- (send c get-y) (send b get-y)))
            (xc-xb (- (send c get-x) (send b get-x)))
            (xc-xbC (- (sqr (send c get-x)) (sqr (send b get-x))))
            (yc+yb (+ (send c get-y) (send b get-y))))
        (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (e) 8000))) 
          (/ (+ (* -2 xc (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (e) 8000))) (/ xc-xb yc-yb))) 
                (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (e) 8000))) (/ xc-xbC yc-yb)) yc+yb) 2))))
    (define/public (egal? pt)
      (and (= (send pt get-x) (get-x)) (= (send pt get-y) (get-y))))
    (define/public (set-visible! b)
      (set! visible b))
    (define/public (affiche dc)
      (when visible
        (let ((x (get-x))
              (y (get-y)))
          (send dc set-pen POINT-PEN)
          (send dc draw-line (- x 2) (- y 2) (+ x 2) (+ y 2))
          (send dc draw-line (- x 2) (+ y 2) (+ x 2) (- y 2))
          (send dc draw-point x y))))
    (super-new)))



