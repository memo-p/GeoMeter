#lang racket
(provide (all-defined-out))

(define point<%>;interface minimum des points
  (interface ()
    get-x;fonction renvoyant la coordonnée x du point
    get-y;fonction renvoyant la coordonnée y du point
    egal?));fonction renvoyant si on a egaliter entre le point et un point donnée

(define segment<%>;interface minimum des segments
  (interface ()
    get-a ;fonction renvoyant le premier point du segment
    get-b ;fonction renvoyant le second point du segment
    get-vct)) ;fonction renvoyant un vecteur du segment

(define droite<%>;interface minimum des droites
  (interface ()
    get-pt1 ;fonction renvoyant un point connu de la droite
    get-vct ;fonction renvoyant un vcteur de la droite
    get-eq)) ;fonction renvoyant un posn de l'equation cartesienne de la droite

(define vecteur<%>;interface minimum des vecteurs
  (interface ()
    get-dx ;fonction renvoyant le dx du vecteur
    get-dy)) ;fonction renvoyant le dy du vecteur

(define cercle<%>;interface minimum des cercles
  (interface ()
    get-centre ;fonction renvoyant le centre du cercle
    get-dessus ;fonction renvoyant un point sur le cercle
    ;get-rayon ;fonction retournant le rayon
    ))
