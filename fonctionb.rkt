#lang racket/gui
(provide (all-defined-out))
(require "classB.rkt");recuperation des classe des pt% et poly4%
(require "classC.rkt");recuperation des classe des cercle% et cercle3P%
(require "classA.rkt");recuperation des classe des segment% et droite%
(require "interface.rkt")

(require lang/posn)
(define POINTS '())
(define DROITE '())
(define CERCLE '());les variables globale qui seront modifier uniquement par les fonctions ci-dessous
(define SEGMENT '())
(define POLY4 '())
(define VECTEUR '())
(define EDITEUR (new text%)) ;editeur de l'historique

(define nombrePoint 0) ; constante qui vont servir pour nommer les figures dans l'historique
(define nombreDroite 0)
(define nombreMediatrice 0)
(define nombrePara 0)
(define nombrePerpen 0)
(define nombreSegment 0)
(define nombreCercle 0)
(define nombrePoly 0)
(define nombreInter 0)
(define nombreVecteur 0)


;chacune des fonction ci-dessous vont en plus de renvoyer l'objet demander, integrer l'objet dans la liste correspondante!


(define (point);retourne un point au hasard dans la fenetre
  (let ((x1 (random 600))(y1 (random 600)))
    (set! POINTS (cons (new pt% (x x1) (y y1)) POINTS))
    (send tmp save (format "(pointXY ~a ~a)" x1 y1));on savegarde l'origine qui sera recréé si on recharge
    (car POINTS)))

(define (pointXY X Y);retourne un pt% au coordonnée indiquer
  (set! POINTS (cons (new pt% (x X) (y Y)) POINTS))
  (send tmp save (format "(pointXY ~a ~a)" X Y))
  (car POINTS))

(define (milieu Segment);renvoi le pt% au milieu du segment S
  (set! POINTS (cons (new ptMS% (S Segment)) POINTS))
  (let ((ns (numeroS Segment)))
    (send tmp save (format "(milieu (recupS ~a))" ns)))
  (car POINTS))

(define (segmentA);retourne un segment% aleatoire
  (set! SEGMENT (cons (segment (point) (point)) SEGMENT))
  (send tmp save "(segment (car POINTS) (cadr POINTS))")
  (car SEGMENT))

(define (segment A B);renvoi un segment dont les extremiter son A et B
  (set! SEGMENT (cons (new segment% (a A) (b B)) SEGMENT))
  (let ((nA (numeroP A))(nB (numeroP B)))
    (send tmp save (format "(segment (recupP ~a) (recupP ~a))" nA nB)))
  (car SEGMENT))


(define (cercle a rAyon);créé un cercle% de centre a et de rayon rayon
  (set! CERCLE (cons (new cerclePR% (centre a) (rayon rAyon)) CERCLE))
  (let ((na (numeroP a)))
    (send tmp save (format "(cercle (recupP ~a) ~a)" na rAyon)))
  (car CERCLE))

(define (cercle2P P1 P2) ;créé un cercle de centre P1 avec P2 dessus
  (set! CERCLE (cons (new cercle% (centre P1) (dessus P2)) CERCLE))
  (let ((nA (numeroP P1))(nB (numeroP P2)))
    (send tmp save (format "(cercle2P (recupP ~a) (recupP ~a))" nA nB)))
  (car CERCLE))

(define (cercle3P A B C);créé un cercle3P% passant par A B et C
  (set! CERCLE (cons (new cercle3P% (a A) (b B) (c C)) CERCLE))
  (let ((nA (numeroP A))(nB (numeroP B))(nC (numeroP C)))
    (send tmp save (format "(cercle3P (recupP ~a) (recupP ~a) (recupP ~a))" nA nB nC)))
  (car CERCLE))

(define (droite);retourne une droite aléatoire
  (set! DROITE (cons (new droitePV% (pt1 (point)) (vct (vecteurA))) DROITE))
  (let ((nA (numeroP (car POINTS)))(nB (numeroV (car VECTEUR))))
    (send tmp save (format "(droitePV (recupP ~a) (recupv ~a))" nA nB)))
  (car DROITE))

(define (droite2P A B);renvoie une droite2P% passant par lesz point A et B
  (set! DROITE (cons (new droite2P% (pt1 A) (pt2 B)) DROITE))
  (let ((nA (numeroP A))(nB (numeroP B)))
    (send tmp save (format "(droite2P (recupP ~a) (recupP ~a))" nA nB)))
  (car DROITE))

(define (droitePV P V);retourne une droitePV% passant par P de vecteur V
  (set! DROITE (cons (new droitePV% (pt1 P) (vct V)) DROITE))
  (let ((nA (numeroP P))(nB (numeroV V)))
    (send tmp save (format "(droitePV (recupP ~a) (recupv ~a))" nA nB)))
  (car DROITE))

(define (parallele Dr A); renvoie une droitePV% parallele a la droite D passant par A
  (set! DROITE (cons (new droitePAR% (pt1 A) (D Dr)) DROITE))
  (let ((nA (numeroP A))(nB (numeroD Dr)))
    (send tmp save (format "(parallele (recupD ~a) (recupP ~a))" nB nA)))
  (car DROITE))

(define (perpendiculaire Dr A);retourne une droite perpendiculaire a D passant par A
  (set! DROITE (cons (new droitePER% (pt1 A) (D Dr)) DROITE))
  (let ((nA (numeroP A))(nB (numeroD Dr)))
    (send tmp save (format "(perpendiculaire (recupD ~a) (recupP ~a))" nB nA)))
  (car DROITE))

(define (mediatrice Seg);renvoi une droitePV% mediatrice du segment S
  (set! DROITE (cons (new droiteM% (S Seg)) DROITE))
  (let ((nA (numeroS Seg)))
    (send tmp save (format "(mediatrice (recupS ~a))"nA)))
  (car DROITE))

(define (intersection2D D1 D2);cette fonction renvoi un ptI2D% de l'intersection de deux droites
  (set! POINTS (cons (new ptI2D% (d1 D1) (d2 D2)) POINTS))
  (let ((nA (numeroD D1))(nB (numeroD D2)))
    (send tmp save (format "(intersection2D (recupD ~a) (recupD ~a))" nB nA)))
  (car POINTS))

(define (distance A B);cette fonction renvoi la distance entre 2 pt%
  (sqrt (+ (sqr (- (send A get-x) (send B get-x))) (sqr (- (send A get-y) (send B get-y))))))

(define (distanceS S);renvoi la longueur d'un segment
  (distance (send S get-a) (send S get-b)))

(define (vecteurA);retourne un vecteur aleatoire
  (vecteur (- (random 60) 30) (- (random 60) 30)))

(define (vecteurA2P);cette fonction renvoi un vecteur pointant sur deux point aleatoire
  (vecteur2P (point)(point))
  )

(define (vecteur2P a b);cette fonction renvoi un vecteur pointant sur les poitna et b
  (set! VECTEUR (cons (new vecteur2P% (origine (pointXY (send a get-x) (send a get-y))) (pt1 a)(pt2 b)) VECTEUR))
  (car VECTEUR))

(define (vecteur dX dY);renvoi un vecteur% 
  (set! VECTEUR (cons (new vecteur% (dx dX) (dy dY)) VECTEUR))
  )

(define (poly4 P1 P2 P3 P4) ;renvoi le polygone a 4 sommets definit par les 4 point donnée (pt%)
  (set! POLY4 (cons (new poly4% (p1 P1)(p2 P2) (p3 P3) (p4 P4)) POLY4))
  (let ((nA (numeroP P1))(nB (numeroP P2))(nC (numeroP P3))(nD (numeroP P4)))
    (send tmp save (format "(poly4 (recupP ~a) (recupP ~a) (recupP ~a) (recupP ~a))" nA nB nC nD)))
  (car POLY4))

(define (ajoute figure);cette fonction ajoute la figure f dans la bonne liste
  (cond ((is-a? figure point<%>) (set! POINTS (cons figure POINTS)))
        (else void)))

(define (init);cette fonction reinitialise toutes les listes
  (set! POINTS '())
  (set! DROITE '())
  (set! CERCLE '());reinitialisation de tout !
  (set! SEGMENT '())
  (set! POLY4 '())
  (set! VECTEUR '())
  (set! nombrePoint 0) 
  (set! nombreDroite 0)
  (set! nombreMediatrice 0)
  (set! nombrePara 0)
  (set! nombrePerpen 0)
  (set! nombreSegment 0)
  (set! nombreCercle 0)
  (set! nombrePoly 0)
  (set! nombreInter 0)
  (set! nombreVecteur 0)
  (send EDITEUR lock #f)
  (send EDITEUR erase)
  (send EDITEUR lock #t)
  (send tmp init))

(define ajoutePointSouris;fonction pour ajouter des point sur le canvas a la souris
  (lambda (evt)
    (when (equal? (send evt get-event-type) 'left-down);si on clique gauche (on ajoute le point)
      (pointXY (send evt get-x) (send evt get-y))
      (set! nombrePoint (+ nombrePoint 1))
      (send EDITEUR lock #f)       ;on ajoute sa marque dans l'historique
      (send EDITEUR insert 
            (format "Point P~a ajouté (~a;~a)\n" nombrePoint (send evt get-x) (send evt get-y)))
      (send EDITEUR lock #t)
      )))

(define ajouteSegmentSouris;fonction pour ajouter des segments sur le canvas
  (let ((etat 0)(pt1 #f))
    (lambda (evt)
      (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
        (if (= etat 0)
            (begin ;premiere etape on recupere le premier point
              (set! pt1 (le-bon-point (send  evt get-x) (send evt get-y)))
              (set! etat 1))
            (begin;deuxieme etape on recupere le second point et on trace
              (segment pt1 (le-bon-point (send  evt get-x) (send evt get-y)))
              (set! etat 0)
              (set! nombreSegment (+ nombreSegment 1))
              (send EDITEUR lock #f) ;on ajoute sa marque dans l'historique
              (send EDITEUR insert 
                    (format "Segment S~a ajouté\n" nombreSegment))
              (send EDITEUR lock #t)))))))

(define ajouteVecteurSouris;fonction pour ajouter des vecteur sur le canvas
  (let ((etat 0)(pt1 #f))
    (lambda (evt)
      (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
        (if (= etat 0)
            (begin ;premiere etape on recupere le premier point
              (set! pt1 (le-bon-point (send  evt get-x) (send evt get-y)))
              (set! etat 1))
            (begin;deuxieme etape on recupere le second point et on trace
              (vecteur2P pt1 (le-bon-point (send  evt get-x) (send evt get-y)))
              (set! etat 0)
              (set! nombreVecteur (+ nombreVecteur 1))
              (send EDITEUR lock #f) ;on ajoute sa marque dans l'historique
              (send EDITEUR insert 
                    (format "Vecteur V~a ajouté\n" nombreVecteur))
              (send EDITEUR lock #t)))))))

(define ajouteDroiteSouris;fonction pour ajouter des droite sur le canvas
  (let ((etat 0)(pt1 #f))
    (lambda (evt)
      (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
        (if (= etat 0)
            (begin
              (set! pt1 (le-bon-point (send  evt get-x) (send evt get-y)))
              (set! etat 1))
            (begin;deuxieme etape on recupere le second point et on trace
              (droite2P pt1 (le-bon-point (send  evt get-x) (send evt get-y)))
              (set! etat 0)
              (set! nombreDroite (+ nombreDroite 1))
              (send EDITEUR lock #f) ;on ajoute sa marque dans l'historique.
              (send EDITEUR insert 
                    (format "Droite D~a ajouté\n" nombreDroite))
              (send EDITEUR lock #t)))))))

(define ajouteCercleSouris;fonction pour ajouter des cercle avec 2 points
  (let ((etat 0)(pt1 #f)(pt2 #f)(bon-pt #f)(cercle #f))
    (lambda (evt)
      (if (equal? (send evt get-event-type) 'left-down);si on clique gauche
          (if (= etat 0)
              (begin 
                (set! pt1 (le-bon-point (send  evt get-x) (send evt get-y)));on recherche le centre
                (set! pt2 (pointXY (send evt get-x) (+ 1 (send evt get-y))));et on ajoute le point sur le cercle
                (set! cercle (cercle2P pt1 pt2))
                (set! etat 1))
              (begin
                (set! etat 0)
                (set! nombreCercle (+ nombreCercle 1))
                (send EDITEUR lock #f) ;on ajoute sa marque dans l'historique
                (send EDITEUR insert 
                      (format "Cercle C~a ajouté\n" nombreCercle))
                (send EDITEUR lock #t)
                (set! bon-pt (pt-proche (send evt get-x) (send evt get-y) (cdr POINTS)))
                (when bon-pt
                  (print (send bon-pt get-x))
                  (send cercle set-dessus! bon-pt)
                  (send pt2 set-visible! #f);on retire le point2 (nous l'avons créé a l'instant il nous apartient !)
                  (send pt2 set-x! -20)
                  )
                (let ((nA (numeroC cercle))(nb (numeroP (send cercle get-dessus))))
                  (send tmp save (format "(send (recupP ~a) set-x! ~a)" nb (send (send cercle get-dessus) get-x)))
                  (send tmp save (format "(send (recupP ~a) set-y! ~a)" nb (send (send cercle get-dessus) get-y)))
                  (send tmp save (format "(send (recupC ~a) set-dessus! (recupP ~a))" nA nb)))
                ))
          (when (= etat 1)
            (set! bon-pt (pt-proche (send evt get-x) (send evt get-y) POINTS))
            (if bon-pt
                (begin
                  (send pt2 set-x! (send bon-pt get-x));on fai évoluer le cercle avec la souris
                  (send pt2 set-y! (send bon-pt get-y)))
                (begin
                  (send pt2 set-x! (send evt get-x));on fai évoluer le cercle avec la souris
                  (send pt2 set-y! (send evt get-y)))))))))

(define ajouteCerclePRSouris;fonction pour ajouter des cercle avec 1point et le rayon
  (let ((etat 0)(pt1 #f)(rayon #f)(bon-pt #f)(cercl #f))
    (lambda (evt)
      (if (equal? (send evt get-event-type) 'left-down);si on clique gauche
          (if (= etat 0)  
              (begin 
                (set! pt1 (le-bon-point (send  evt get-x) (send evt get-y)));on recherche le centre
                (set! rayon (distance pt1 evt));on commence le rayon a 0 donc
                (set! cercl (cercle pt1 rayon))
                (set! etat 1))
              (begin
                (set! etat 0)
                (set! nombreCercle (+ nombreCercle 1))
                (let ((nA (numeroC cercl)))
                  (send tmp save (format "(send (recupC ~a) set-rayon! ~a)" nA (send cercl get-rayon))))
                (send EDITEUR lock #f) ;on ajoute sa marque dans l'historique
                (send EDITEUR insert 
                      (format "Cercle C~a ajouté\n" nombreCercle))
                (send EDITEUR lock #t)
                ))
          (when (= etat 1)
            (send cercl set-rayon! (distance pt1 evt));on fai évoluer le cercle avec la souris
            )))))

(define ajouteCercle3PSouris;fonction pour ajouter des cercle avec 3 points
  (let ((etat 0)(pt1 #f)(pt2 #f))
    (lambda (evt)
      (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
        (case etat
          ((0) (set! pt1 (le-bon-point (send  evt get-x) (send evt get-y)));on recupere le premier point
               (set! etat 1))
          ((1) (set! pt2 (le-bon-point (send  evt get-x) (send evt get-y)));le second
               (set! etat 2))
          (else (cercle3P pt1 pt2 (le-bon-point (send  evt get-x) (send evt get-y)));et on trace
                (set! etat 0)
                (set! nombreCercle (+ nombreCercle 1))
                (send EDITEUR lock #f) ;on ajoute sa marque dans l'historique
                (send EDITEUR insert 
                      (format "Cercle C~a ajouté\n" nombreCercle))
                (send EDITEUR lock #t)))))))

(define ajoutePoly4Souris;fonction pour ajouter des poly4 avec 4 points
  (let ((etat 0)(pt1 #f)(pt2 #f)(pt3 #f))
    (lambda (evt)
      (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
        (case etat
          ((0) (set! pt1 (le-bon-point (send  evt get-x) (send evt get-y)));on recupere le premier point
               (set! etat 1))
          ((1) (set! pt2 (le-bon-point (send  evt get-x) (send evt get-y)));le second
               (set! etat 2))
          ((2) (set! pt3 (le-bon-point (send  evt get-x) (send evt get-y)));le troisieme
               (set! etat 3))
          (else (poly4 pt1 pt2 pt3 (le-bon-point (send  evt get-x) (send evt get-y)));et on trace
                (set! etat 0)
                (set! nombrePoly (+ nombrePoly 1))
                (send EDITEUR lock #f) ;on ajoute sa marque dans l'historique
                (send EDITEUR insert 
                      (format "Polygone Poly~a ajouté\n" nombrePoly))
                (send EDITEUR lock #t)))))))


(define V #f);pour bouger les points

(define bougePoint;fonction pour bouger les points a la souris
  (lambda (evt)
    (case (send evt get-event-type)
      ((left-down) (let ((x (send evt get-x))
                         (y (send evt get-y)))
                     (set! V (pt-proche x y POINTS))))
      ((left-up) 
       (when V
         (let* ((x (send evt get-x))
                (y (send evt get-y))
                (pt (pt-proche x y POINTS))
                (nA (numeroP V)))
           (if pt
               (begin
                 (send V set-x! (send pt get-x))
                 (send V set-y! (send pt get-y))
                 (send tmp save (format "(send (recupP ~a) set-x! ~a)" nA (send V get-x)))
                 (send tmp save (format "(send (recupP ~a) set-y! ~a)" nA (send V get-y)))
                 (set! V #f))
               (begin
                 (send tmp save (format "(send (recupP ~a) set-x! ~a)" nA x))
                 (send tmp save (format "(send (recupP ~a) set-y! ~a)" nA y))
                 (set! V #f)))
           )))
      (else (when (and V (not (is-a? V ptI2D%)))
              (send V set-x! (send evt get-x))
              (send V set-y! (send evt get-y))
              )))))

(define (pt-proche x y L);sous fonction pour bougePoint et autres
  (if (null? L)
      #f
      (if
       (is-a? (car L) point<%>)
       (let*  ((x2 (send (car L) get-x))(y2 (send (car L) get-y))(dist? (< (+ (abs (- x x2)) (abs (- y y2))) 7)))
         (if dist? 
             (car L)
             (pt-proche x y (cdr L))))
       (pt-proche x y (cdr L)))))




(define (le-bon-point x y);cette fonction renvoi le point le plus proche de (x;y) ou le point (x;y)
  (let ((res (pt-proche x y POINTS)))
    (if res
        res
        (pointXY x y))))


;les prochaine to-liste serve pour les list-box principalement:

(define (to-liste L n);renvoi un liste contenant le symnole D et les numeros (de 0 a n)
  (if (null? L)
      '()
      (cons (format "D~a" n) (to-liste (cdr L) (- n 1)))))

(define (to-listeS L n);renvoi un liste contenant le symnole S enét les numeros (de 0 a n)
  (if (null? L)
      '()
      (cons (format "S~a" n) (to-listeS (cdr L) (- n 1)))))

(define (to-listeV L n);renvoi un liste contenant le symnole V et les numeros (de 0 a n)
  (if (null? L)
      '()
      (cons (format "V~a" n) (to-listeV (cdr L) (- n 1)))))


(define (recupere-item L n);cette fonction renvoi le n'ieme element de la liste L en sachant qu'il existe
  (if (zero? n)
      (car L)
      (recupere-item (cdr L) (- n 1))))

(define (ajouteParalleleSouris LISTE-DROITE);cette fonction renvoi une fonction qui va etre donné a etat pour placer une parallele à la souris
  (define  (methode evt)
    (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
      (let ((droite (send LISTE-DROITE get-selection)));on recupere la droite selectionner par LIST-DROITE (list-box%)
        (if droite;si une droite est selectioné
            (begin;alors on créé la parallele et on met a jour la lis-box
              (parallele (recupere-item DROITE droite) (le-bon-point (send  evt get-x) (send evt get-y)))
              (send LISTE-DROITE set (to-liste DROITE (length DROITE)))
              (send LISTE-DROITE set-selection (+ 1 droite)))
            void))
      (set! nombrePara (+ nombrePara 1))
      (send EDITEUR lock #f) ;on ajoute sa marque dans l'historique
      (send EDITEUR insert 
            (format "Droite parallèle Para~a ajouté\n" nombrePara))
      (send EDITEUR lock #t)))
  methode)

(define (ajoutePerpendiculaireSouris LISTE-DROITE);cette fonction renvoi une fonction qui va etre donné a etat pour placer une perpendiculaire à la souris
  (define  (methode evt)
    (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
      (let ((droite (send LISTE-DROITE get-selection)));on recupere la droite selectionner par LIST-DROITE (list-box%)
        (if droite;si une droite est selectioné
            (begin;alors on créé la perpendiculaire et on met a jour la lis-box
              (perpendiculaire (recupere-item DROITE droite) (le-bon-point (send  evt get-x) (send evt get-y)))
              (send LISTE-DROITE set (to-liste DROITE (length DROITE)))
              (send LISTE-DROITE set-selection (+ 1 droite))
              (set! nombrePerpen (+ nombrePerpen 1))
              (send EDITEUR lock #f)  ;on ajoute sa marque dans l'historique
              (send EDITEUR insert 
                    (format "Droite Perpendiculaire Perp~a ajouté\n" nombrePerpen))
              (send EDITEUR lock #t))
            void))))
  methode)

(define (ajouteMediatriceSouris LISTE-SEGMENT);cette fonction renvoi une fonction qui va etre donné a etat pour placer une mediatrice à la souris
  (define  (methode evt)
    (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
      (let ((segment (send LISTE-SEGMENT get-selection)));on recupere le segment selectionner par LIST-SEGMENT(list-box%)
        (if segment ;si un segment est selectioné
            (begin ;alors on créé la mediatrice et on met a jour la lis-box
              (mediatrice (recupere-item SEGMENT segment))
              (send LISTE-SEGMENT set (to-listeS SEGMENT (length SEGMENT)))
              (send LISTE-SEGMENT set-selection segment)
              (set! nombreMediatrice (+ nombreMediatrice 1))
              (send EDITEUR lock #f)  ;on ajoute sa marque dans l'historique
              (send EDITEUR insert 
                    (format "Médiatrice M~a ajouté\n" nombreMediatrice))
              (send EDITEUR lock #t))
            void))))
  methode)

(define (ajouteMilieuSSouris LISTE-SEGMENT);cette fonction renvoi une fonction qui va etre donné a etat pour placer un milieu à la souris
  (define  (methode evt)
    (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
      (let ((segment (send LISTE-SEGMENT get-selection)));on recupere le segment selectionner par LIST-SEGMENT(list-box%)
        (if segment ;si un segment est selectioné
            (begin ;alors on créé le milieu et on incrémente le nombre de point
              (milieu (recupere-item SEGMENT segment))
              (set! nombrePoint (+ nombrePoint 1))
              (send EDITEUR lock #f)  ;on ajoute sa marque dans l'historique
              (send EDITEUR insert 
                    (format "Milieu P~a ajouté\n" nombrePoint))
              (send EDITEUR lock #t))
            void))))
  methode)

(define (deplaceVecteurSouris LISTE-VECTEUR);cette fonction renvoi une fonction qui va etre donné a etat pour déplacer un vecteur à la souris
  (let ((etat #f)(vecteurObj #f))
    (define  (methode evt)
      (let ((vecteur (send LISTE-VECTEUR get-selection)));on recupere le vecteur selectionner par LIST-VECTEUR(list-box%)
        (when vecteur ;si un vecteur est selectioné
          (when (not (equal? vecteur etat))
            (set! vecteurObj (recupere-item VECTEUR vecteur)))
          (if (equal? (send evt get-event-type) 'left-down);si on clique gauche
              (begin 
                (let ((pt (pt-proche (send  evt get-x) (send evt get-y) POINTS)))
                  (if pt
                      (begin
                        (send vecteurObj set-origine! pt)
                        (send LISTE-VECTEUR clear))
                      (begin 
                        (send vecteurObj set-origine! (new pt% (x (send  evt get-x)) (y (send  evt get-y))))
                        (send LISTE-VECTEUR clear)))))
              (begin
                (let ((pt (pt-proche (send  evt get-x) (send evt get-y) POINTS)))
                  (if pt
                      (send vecteurObj set-origine! pt)
                      (send vecteurObj set-origine! (new pt% (x (send  evt get-x)) (y (send  evt get-y)))))))))))
    methode))

(define (ajouteIntersection2DSouris LISTE-DROITE);cette fonction renvoi une fonction qui va etre donné à etat pour place l'intersection de deux droite
  (let ((etat 0) (d1 #f))
    (define  (methode evt)
      (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
        (let ((droite (send LISTE-DROITE get-selection)));on recupere la droite selectionner par LIST-DROITE (list-box%)
          (when droite;si une droite est selectioné
            (let ((droiteObj (recupere-item DROITE droite)))
              (if (zero? etat)
                  (begin;on a la premiere droite
                    (set! d1 droiteObj)
                    (set! etat 1))
                  (when (not (eq? d1 droiteObj));et la seconde
                    (intersection2D d1 droiteObj)
                    (set! nombreInter (+ nombreInter 1))
                    (send EDITEUR lock #f)  ;on ajoute sa marque dans l'historique
                    (send EDITEUR insert 
                          (format "Point Intersection I~a ajouté\n" nombreInter))
                    (send EDITEUR lock #t)
                    (set! etat 0))))))))
    methode))

(define (coordonnéePointSouris MESS)
  (define (methode evt)
    (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
      (let ((pt (pt-proche (send  evt get-x) (send evt get-y) POINTS)))
        (when pt
          (send EDITEUR lock #f) 
          (send EDITEUR insert (format "x:~a y:~a\n"(send pt get-x) (send pt get-y)))
          (send EDITEUR lock #t)))))
  methode)

(define (distancePointSouris MESS)
  (let ((pt1 #f)(etat 0))
    (define (methode evt)
      (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
        (let ((pt (pt-proche (send  evt get-x) (send evt get-y) POINTS)))
          (when pt
            (if (zero? etat)
                (begin
                  (set! pt1 pt)
                  (send EDITEUR lock #f) 
                  (send EDITEUR insert (format "x:~a y:~a\n"(send pt get-x) (send pt get-y)))
                  (send EDITEUR lock #t)
                  (set! etat 1))
                (begin 
                  (send EDITEUR lock #f) 
                  (send EDITEUR insert (format "distance : ~a\n" (distance pt1 pt)))
                  (send EDITEUR lock #t)
                  (set! etat 0)))))))
    methode))

(define (cacheFigure LIST-D LIST-S LIST-V)
  (define  (methode evt)
    (define (change obj)
      (if (send obj get-visible)
          (send obj set-visible! #f)
          (send obj set-visible! #t)))
    (when (equal? (send evt get-event-type) 'left-down);si on clique gauche
      (let ((segment (send LIST-S get-selection))
            (droite (send LIST-D get-selection))
            (vecteur (send LIST-V get-selection)));on recupere les objets a modifier
        (when segment ;si un segment est selectioné
          (change (recupere-item SEGMENT segment))
          (send LIST-S set (to-listeS SEGMENT (length SEGMENT))))
        (when droite 
          (change (recupere-item DROITE droite))
          (send LIST-D set (to-liste DROITE (length DROITE))))
        (when vecteur 
          (change (recupere-item VECTEUR vecteur))
          (send LIST-V set (to-listeV VECTEUR (length VECTEUR)))))))
  methode)

(define (intersectionDC droite cercle);fonction renvoyer lintersection d'une droite et d'un cercles
  (let* ((centreCercle (send cercle get-centre)) 
         (x0 (send centreCercle get-x))
         (y0 (send centreCercle get-y))
         (r (sqr (send cercle get-rayon)))
         (eq (send droite get-eq))
         (a (posn-x eq))
         (b (posn-y eq))
         ;ici on a y=ax+b pour l droite et (x+x0)²+(y+y0)²=r (car r = rayon²)
         ;pour le reste de la fonction on considerera que l'on a remplacer y dans l'equation du cercle par sont equation de la droite
         ;pour se ramener a une equation du second degres ,ensuite on calcule le delta et on avise
         ;donc on aura ex²+fx+g=0
         (e (+ 1 (sqr a)))
         (b-y0 (- b y0))
         (f (+ (* -2 x0) (* 2 a b-y0)))
         (g (+ (sqr x0) (sqr b-y0) (- r)))
         (delta (- (sqr f) (* 4 e g))))
    (cond 
      ((zero? delta)  (let* ((x (/ (- f) (* 2 e)))(y (+ (* x a) b)))
                        (send EDITEUR lock #f)                          
                        (send EDITEUR insert (format "Une intersection :(~a,~a)\n"x y))
                        (send EDITEUR lock #t) 
                        (pointXY x y)))
      ((> delta 0)  (let* ((x1 (/ (- (- f) (sqrt delta)) (* 2 e)))
                           (y1 (+ (* x1 a) b))
                           (x2 (/ (+ (- f) (sqrt delta)) (* 2 e)))
                           (y2 (+ (* x2 a) b)))
                      (send EDITEUR lock #f) 
                      (send EDITEUR insert (format " 2 intersections :(~a,~a) et (~a,~a)\n"x1 y1 x2 y2))
                      (send EDITEUR lock #t)
                      (pointXY x1 y1)
                      (pointXY x2 y2)))
      (else 
       (send EDITEUR lock #f) 
       (send EDITEUR insert "pas d'intersection\n")
       (send EDITEUR lock #t)
       ))))







(define (numeroS Segment)
  (define s (reverse SEGMENT))
  (define (iter n s)
    (if (null? s)
        0
        (if (eq? Segment (car s))
            n
            (iter (+ n 1) (cdr s)))))
  (iter 0 s))

(define (recupS ns)
  (define s (reverse SEGMENT))
  (define (iter n s)
    (if (zero? n)
        (car s)
        (iter (- n 1) (cdr s))))
  (iter ns s))

(define (numeroD Droite)
  (define s (reverse DROITE))
  (define (iter n s)
    (if (null? s)
        0
        (if (equal? Droite (car s))
            n
            (iter (+ n 1) (cdr s)))))
  (iter 0 s))
(define (recupD ns)
  (define s (reverse DROITE))
  (define (iter n s)
    (if (zero? n)
        (car s)
        (iter (- n 1) (cdr s))))
  (iter ns s))

(define (numeroP Point)
  (define s (reverse POINTS))
  (define (iter n s)
    (if (null? s)
        0
        (if (eq? Point (car s))
            n
            (iter (+ n 1) (cdr s)))))
  (iter 0 s))
(define (recupP ns)
  (define s (reverse POINTS))
  (define (iter n s)
    (if (zero? n)
        (car s)
        (iter (- n 1) (cdr s))))
  (iter ns s))

(define (numeroV vecteur)
  (define s (reverse VECTEUR))
  (define (iter n s)
    (if (null? s)
        0
        (if (equal? vecteur (car s))
            n
            (iter (+ n 1) (cdr s)))))
  (iter 0 s))
(define (recupV ns)
  (define s (reverse VECTEUR))
  (define (iter n s)
    (if (zero? n)
        (car s)
        (iter (- n 1) (cdr s))))
  (iter ns s))

(define (numeroC cercle)
  (define s (reverse CERCLE))
  (define (iter n s)
    (if (null? s)
        0
        (if (equal? cercle (car s))
            n
            (iter (+ n 1) (cdr s)))))
  (iter 0 s))
(define (recupC ns)
  (define s (reverse CERCLE))
  (define (iter n s)
    (if (zero? n)
        (car s)
        (iter (- n 1) (cdr s))))
  (iter ns s))





(define temp%
  (class object%
    (define p-out (open-output-file "fichier/fichier.tmp" #:exists'replace))
    (define/public (save str)
      (fprintf p-out (format "~a\n" str)))
    (define/public (toFile f)
      (close-output-port p-out)
      (when (file-exists? f)
          (delete-file f))
      (copy-file "fichier/fichier.tmp" f)
      (send EDITEUR lock #f) 
      (send EDITEUR insert (format "Fichier sauvé ~a" f))
      (send EDITEUR lock #t)
      (set! p-out (open-output-file "fichier/fichier.tmp" #:exists'append)))
    (define/public (fin)
      (close-output-port p-out))
    (define/public (init)
      (close-output-port p-out)
      (when (file-exists? "fichier/fichier.tmp")
          (delete-file "fichier/fichier.tmp"))
      (set! p-out (open-output-file "fichier/fichier.tmp" #:exists'replace)))
    (super-new)))

(define tmp (new temp%))

(define (load f)
  (if (file-exists? f)
      (begin
        (init)
        (call-with-input-file f
          (lambda (p-in)
            (define (iter)
              (let* ((x (read p-in)))
                (when (not (eof-object? x))
                  (eval x)
                  (iter))))
            (iter))))
      (begin
        
        (send EDITEUR lock #f) 
        (send EDITEUR insert (format "Fichier inexistant ~a" f))
        (send EDITEUR lock #t))))
