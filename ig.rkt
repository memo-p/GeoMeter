#lang racket/gui
(require embedded-gui)
(require scheme/system)
(require mrlib/bitmap-label)
(require mzlib/string)
(require "classB.rkt");recuperation des classe des pt% et poly4%
(require "classC.rkt");recuperation des classe des cercle% et cercle3P%
(require "classA.rkt");recuperation des classe des segment% et droite%
(require "fonctionb.rkt");recuperation des fonction
(require "exemples.rkt");recuperation des fonction

(define etat bougePoint);fonction de gestion de la souris


(define FramePrinc (new frame% ; frame principale.
                        (label "GEOMETER")
                        ))

;definition de la barre de menu et des elements associer
(define MENUBAR (new menu-bar%	; barre des menus.
                     (parent FramePrinc)))

(define MENU (new menu%        ; le menu Menu.
                  (parent MENUBAR)
                  (label "Menu")
                  ))

(define MENUFigure (new menu%        ; le menu Figure.
                        (parent MENUBAR)
                        (label "Figure")
                        ))

(define Figure (new menu%        ;le menu Figure qui contient les figure sauvegarder qui sont disponibles.
                    (label "Figures disponibles")
                    (parent MENUFigure)
                   ))

;debut des exemple de figure
(define fig1 (new menu-item%
                  (label "Exemple 1")
                  (parent Figure)
                  (callback (lambda (x y)
                              (init)
                              (exemple1)
                              (send CANVAS on-paint)))))

(define fig2 (new menu-item%
                  (label "Exemple 2")
                  (parent Figure)
                  (callback (lambda (x y)
                              (init)
                              (exemple2)
                              (send CANVAS on-paint)))))

(define fig3 (new menu-item%
                  (label "Exemple 3")
                  (parent Figure)
                  (callback (lambda (x y)
                              (init)
                              (exemple3)
                              (send CANVAS on-paint)))))

(define fig4 (new menu-item%
                  (label "Exemple 4")
                  (parent Figure)
                  (callback (lambda (x y)
                              (init)
                              (exemple4)
                              (send CANVAS on-paint)))))

(define fig5 (new menu-item%
                  (label "Exemple 5")
                  (parent Figure)
                  (callback (lambda (x y)
                              (init)
                              (exemple5)
                              (send CANVAS on-paint)))))

(define fig6 (new menu-item%
                  (label "Exemple 6")
                  (parent Figure)
                  (callback (lambda (x y)
                              (init)
                              (exemple6)
                              (send CANVAS on-paint)))))

;fin des exemples 

(define POINTALEATOIRE (new menu-item% ;l'item point aléatoire qui met un point aléatoire sur le canvas
                         (label "Point Aleatoire")
                         (parent MENUFigure)
                         (callback (lambda (x y)
                                     (point)
                                     (send CANVAS on-paint)
                                     )
                                   )))
(void (new separator-menu-item%   ;séparateur des item du menu
     (parent MENUFigure)))

(define name "figure")
(define OuvrirFigC (new menu-item%    ;l'item Ouvrira une figure, qui ouvre la figure courante dans le canvas secondaire
                        (label "Ouvrir Figure dans un Canvas")
                        (parent MENUFigure)
                        (callback (lambda (x y)
                                    (let ((bitmapFig #f)
                                          (frameFig #f)
                                          (canvasFig #f))
                                      (send BITMAP save-file (format "~a.png" name) 'png)
                                      (set! bitmapFig (make-object bitmap% (format "~a.png" name)))
                                      (set! frameFig (new frame%
                                                          (min-width SIZE)
                                                          (min-height SIZE)
                                                          (label name)))
                                      (set! canvasFig (new canvas%
                                                           (parent frameFig)
                                                           (paint-callback (lambda (obj dc)
                                                                             (send dc draw-bitmap bitmapFig 0 0 'solid)))))
                                      (send frameFig show #t)
                                      )))))

(void (new separator-menu-item%   ;séparateur des item du menu
     (parent MENUFigure)))

(define CACHER (new menu-item% ;l'item cacher qui cache une figure.
                    (label "Cacher une figure")
                    (parent MENUFigure)
                    (callback (lambda (x y)
                                (send LIST-VECTEUR set (to-listeV VECTEUR (length VECTEUR)))
                                     (send LIST-VECTEUR show #t)
                                     (send LIST-SEGMENT set (to-listeS SEGMENT (length SEGMENT)))
                                     (send LIST-SEGMENT show #t)
                                     (send LIST-DROITE set (to-liste DROITE (length DROITE)))
                                     (send LIST-DROITE show #t)
                                     (set! etat (cacheFigure LIST-DROITE LIST-SEGMENT LIST-VECTEUR))
                                )
                              )))
(void (new separator-menu-item%   ;séparateur des item du menu
     (parent MENUFigure)))
(define EFFACERTOUT (new menu-item% ;l'item effacer tous qui efface tout le canvas & historique etc.
                         (label "Effacer TOUT")
                         (parent MENUFigure)
                         (callback (lambda (x y)
                                     (init)
                                     (send CANVAS on-paint)
                                     )
                                   )))



(define SauveFIG (new menu-item%      
                      (label "Sauvegarder Figure")
                      (parent MENU)
                      (callback (lambda (x y)
                                  (let ((frame #f)
                                        (mess #f)
                                        (textField #f))
                                      (set! frame (new frame%
                                                          (label "fichier à sauver")))
                                      (set! mess (new message%
                                                      (parent frame)
                                                      (label "nom du fichier à sauvegarder")))
                                    (set! textField (new text-field%
                                                         (parent frame)
                                                         (label "nom : ")
                                                         (callback (λ (tf evt)
                                                                     (when (equal? (send evt get-event-type) 'text-field-enter) ;lors du pressage de la touche entrée
                                                                       (send tmp toFile (format "fichier/~a.fig" (send textField get-value)))
                                                                       (send frame show #f)
                                                                       )
                                                                     ))))
                                      (send frame show #t)
                                      )
                                  ))))

(define ChargerFIG (new menu-item%    
                        (label "Charger Figure")
                        (parent MENU)
                        (callback (lambda (x y)
                                    (let ((frame #f)
                                        (mess #f)
                                        (textField #f))
                                      (set! frame (new frame%
                                                          (label "fichier à charger")))
                                      (set! mess (new message%
                                                      (parent frame)
                                                      (label "nom du fichier à charger")))
                                    (set! textField (new text-field%
                                                         (parent frame)
                                                         (label "nom : ")
                                                         (callback (λ (tf evt)
                                                                     (when (equal? (send evt get-event-type) 'text-field-enter) ;lors du pressage de la touche entrée
                                                                       (load (format "fichier/~a.fig" (send textField get-value)))
                                                                       (send frame show #f)
                                                                       )
                                                                     ))))
                                      (send frame show #t)
                                      )
                                    ))))
(void (new separator-menu-item%   ;séparateur des item du menu
     (parent MENU)))
(define DOC (new menu-item%          ;l'item doc qui permet l'accés a la doc.
                 (parent MENU)
                 (label "Aide/Doc")
                 (shortcut #\h) 
                 (callback (lambda (x y)
                             (when (system "explorer mainSCB.html")
                               (system "open mainSCB.html")
                               )
                                 

                             void))))

(void (new separator-menu-item%   ;séparateur des item du menu
     (parent MENU)))

(define QUITTER (new menu-item%      ;l'item quitter qui quitte le soft.
                     (parent MENU)
                     (label "Quitter")
                     (shortcut #\Q) 
                     (callback (lambda (x y)
                                 (send FRAMEQUITTER show #t)
                                 void))))
;bouton quitter avec les panel pour les allignés.
(define FRAMEQUITTER (new frame%                 ; frame pour quitter.
                          (label "Quitter")
                          (x 500)
                          (y 400)
                          (min-width 300)))
(define VPANELQUI (new vertical-panel%               ;panel vertical pour ordonnée question et réponses.
                       (parent FRAMEQUITTER)))
(define messagequit (new message%                    ;question.
                         (label "Vous etes sur?")
                         (parent VPANELQUI)))      
(define HPANELQUI (new horizontal-panel%             ;panel horizontal pour aligné les boutons réponses.
                       (parent VPANELQUI)
                       (alignment '(center center))))

(define BOUTONQUITTER (new button%                   ;bouton pour confirmer.
                           (parent HPANELQUI)
                           (label "Quitter")
                           (callback (lambda (z e)
                                       (send FRAMEQUITTER show #f)
                                       (send FramePrinc show #f)))))
(define BOUTONQUITTER2 (new button%                   ;bouton pour refuser.
                            (parent HPANELQUI)
                            (label "Revenir au soft")
                            (callback (lambda (z e)
                                        (send FRAMEQUITTER show #f)))))



;barre des menu terminer 

;commencement de la construction de la fenetre principale

(define VPANEL (new vertical-panel%  ;pannel vertical principal 
                    (parent FramePrinc)   ;fenêtre principale
                    (alignment '(left center))))


(define groupe-box-figure (new group-box-panel%;groupement des figure 
                               (label "Figure")
                               (parent VPANEL)))
(define groupe-box-figure-panel (new horizontal-panel%
                                     (parent groupe-box-figure)))

(define groupe-box-modification (new group-box-panel%
                                     (label "Modification & Information")
                                     (parent VPANEL)))
(define groupe-box-modification-panel (new horizontal-panel%
                                           (parent groupe-box-modification)))


(define groupe-box-construction (new group-box-panel%;groupement des construction
                                     (label "Construction")
                                     (parent VPANEL)))
(define groupe-box-construction-panel (new horizontal-panel%
                                           (parent groupe-box-construction)))


;definition des bouttons de figure :

(define bitmapPoint (make-object bitmap% "images/point.png")) ;bouton qui crée des points
(define Point (new button%
                   (parent groupe-box-figure-panel)
                   (label (make-bitmap-label "" bitmapPoint))
                   (callback (lambda (obj evt)
                               (cache)
                               (set! etat ajoutePointSouris)
                               void))))
(define bitmapSegment (make-object bitmap% "images/segment.png")) ;bouton qui crée des segments
(define Segment (new button%
                     (parent groupe-box-figure-panel)
                     (label (make-bitmap-label "" bitmapSegment))
                     (callback (lambda (obj evt)
                                 (cache)
                                 (set! etat ajouteSegmentSouris)
                                 void))))
(define bitmapDroite (make-object bitmap% "images/droites.png"))   ;bouton qui crée des droites
(define Droite (new button%
                    (parent groupe-box-figure-panel)
                    (label (make-bitmap-label "" bitmapDroite))
                    (callback (lambda (obj evt)
                                (cache)
                                (set! etat ajouteDroiteSouris)
                                void))))
(define bitmapVect (make-object bitmap% "images/vecteur.png")) ;bouton qui crée des vecteur.
(define Vect (new button%
                  (parent groupe-box-figure-panel)
                  (label (make-bitmap-label "" bitmapVect))
                  (callback (lambda (obj evt)
                              (cache)
                              (set! etat ajouteVecteurSouris)
                              void))))

(define bitmapPoly (make-object bitmap% "images/polygone.png")) ;bouton qui ajoute des polygones.
(define Poly (new button%
                  (parent groupe-box-figure-panel)
                  (label (make-bitmap-label "" bitmapPoly))
                  (callback (lambda (obj evt)
                              (cache)
                              (set! etat ajoutePoly4Souris)
                              void))))
(define bitmapCerc2P (make-object bitmap% "images/cercle2p.png")) ; bouton qui crée des cercles avec 2 points.
(define Cerc2P (new button%
                    (parent groupe-box-figure-panel)
                    (label (make-bitmap-label "" bitmapCerc2P))
                    (callback (lambda (obj evt)
                                (cache)
                                (set! etat ajouteCercleSouris)
                                void))))
(define bitmapCerc3P (make-object bitmap% "images/cercle3p.png")) ;bouton qui crée des cercles avec 3 points.
(define Cerc3P (new button%
                    (parent groupe-box-figure-panel)
                    (label (make-bitmap-label "" bitmapCerc3P))
                    (callback (lambda (obj evt)
                                (cache)
                                (set! etat ajouteCercle3PSouris)
                                void))))

(define bitmapCercR (make-object bitmap% "images/CercleR.png")) ;bouton qui crés des cercles a partir de l'origine et du rayon.
(define CercR (new button%
                   (parent groupe-box-figure-panel)
                   (label (make-bitmap-label "" bitmapCercR))
                   (callback (lambda (obj evt)
                               (cache)
                               (set! etat ajouteCerclePRSouris)
                               void))))




;definition des bouttons de modification :

(define bitmapMoveP (make-object bitmap% "images/movep.png")) ;bouton qui permet de bouger un point.
(define MoveP (new button%
                   (parent groupe-box-modification-panel)
                   (label (make-bitmap-label "" bitmapMoveP))
                   (callback (lambda (obj evt)
                               (cache)
                               (set! etat bougePoint)
                               void))))
(define bitmapMoveV (make-object bitmap% "images/movevect.png")) ;bouton qui permet de bouger un vecteur.
(define MoveV (new button%
                   (parent groupe-box-modification-panel)
                   (label (make-bitmap-label "" bitmapMoveV))
                   (callback (lambda (obj evt)
                               (cache)
                               (send LIST-VECTEUR set (to-listeV VECTEUR (length VECTEUR)))
                               (send LIST-VECTEUR show #t)
                               (set! etat (deplaceVecteurSouris LIST-VECTEUR))
                               void))))
(define bitmapCoord (make-object bitmap% "images/coord.png"))  ;bouton qui donne les coordonées d'un point.
(define Coord (new button%
                   (parent groupe-box-modification-panel)
                   (label (make-bitmap-label "" bitmapCoord))
                   (callback (lambda (obj evt)
                               (cache)
                               (set! etat (coordonnéePointSouris TEXTINFO))
                               void))))
(define bitmapDist (make-object bitmap% "images/distance.png")) ;bouton qui donne la distance entre deux points.
(define Dist (new button%
                  (parent groupe-box-modification-panel)
                  (label (make-bitmap-label "" bitmapDist))
                  (callback (lambda (obj evt)
                              (cache)
                              (set! etat (distancePointSouris TEXTINFO))
                              void))))
(define bitmapMilieu (make-object bitmap% "images/Milieu.png")) ;bouton qui affiche le milieu d'un segment
(define Milieu (new button%
                    (parent groupe-box-modification-panel)
                    (label (make-bitmap-label "" bitmapMilieu))
                    (callback (lambda (obj evt)
                                (cache)
                                (send LIST-SEGMENT set (to-listeS SEGMENT (length SEGMENT)))
                                (send LIST-SEGMENT show #t)
                                (set! etat (ajouteMilieuSSouris LIST-SEGMENT))
                                void))))



;definition des bouttons de construction :

(define bitmapPara (make-object bitmap% "images/paralléle.png")) ; bouton paralléle avec son image
(define Paralléle (new button%
                       (parent groupe-box-construction-panel)
                       (label (make-bitmap-label "" bitmapPara))
                       (callback (lambda (obj evt)        
                                   (cache)
                                   (send LIST-DROITE set (to-liste DROITE (length DROITE)))
                                   (send LIST-DROITE show #t)
                                   (set! etat (ajouteParalleleSouris LIST-DROITE))
                                   void))))
(define bitmapPerpen (make-object bitmap% "images/perpendiculaire.png")) ;bouton perpendiculaire avec son image.
(define Perpendiculaire (new button%
                             (parent groupe-box-construction-panel)
                             (label (make-bitmap-label "" bitmapPerpen))
                             (callback (lambda (obj evt)
                                         (cache)
                                         (send LIST-DROITE set (to-liste DROITE (length DROITE)))
                                         (send LIST-DROITE show #t)
                                         (set! etat (ajoutePerpendiculaireSouris LIST-DROITE))
                                         void))))

(define bitmapInter (make-object bitmap% "images/intersec.png"))     ;bouton intersection et son image
(define Intersection (new button%
                          (parent groupe-box-construction-panel)
                          (label (make-bitmap-label "" bitmapInter))
                          (callback (lambda (obj evt)
                                      (cache)
                                      (send LIST-DROITE set (to-liste DROITE (length DROITE)))
                                      (send LIST-DROITE show #t)
                                      (set! etat (ajouteIntersection2DSouris LIST-DROITE))
                                      void))))

(define bitmapMedia (make-object bitmap% "images/mediatrice.png"))    ;boutons médiatrice et son bitmap
(define BoutonMediatrice (new button%
                              (label (make-bitmap-label "" bitmapMedia))
                              (parent groupe-box-construction-panel)
                              (callback (lambda (obj evt)
                                          (cache)
                                          (send LIST-SEGMENT set (to-listeS SEGMENT (length SEGMENT)))
                                          (send LIST-SEGMENT show #t)
                                          (set! etat (ajouteMediatriceSouris LIST-SEGMENT))
                                          void))))

;bouton terminer

;la ligne de commande et son resultat

(define TEXTINFO (new message%  ; message qui donne les coordonée ou la distance.
                      (parent VPANEL)
                      (label "Tapez vos commande ci-dessous")
                      (min-width 500)
                      (min-height 17)))

(define COMMANDE (new text-field%      ; ligne de commande qui va prendre le text écrit dans le text field le renvoyez dans une liste 
                      (parent VPANEL)  ;, l'interpréter et lancer le on paint.
                      (label "Commandes : ")
                      (min-width 500)
                      (callback (lambda (obj evt)
                                  (when (equal? (send evt get-event-type) 'text-field-enter) ;lors du pressage de la touche entrée
                                    (let* ((TEV (send COMMANDE get-value))(L (read-from-string-all TEV)))
                                      (for-each (lambda (expr)
                                                  (eval expr)
                                                  (send COMMANDE set-value "")
                                                  (send TEXTINFO set-label TEV)
                                                  (send CANVAS on-paint)
                                                  )
                                                L)))))))

(define HPanelCanvasHisto (new horizontal-panel% ; Un panel horizontale pour aligner le canvas avec l'historique et les liste.
                               (parent VPANEL)
                               ))

;liste des droite segment et vecteur pour les differentes modification

(define groupe-box-droite (new group-box-panel%;groupement des figure 
                               (label "Droites")
                               (parent HPanelCanvasHisto)))
(define LIST-DROITE (new list-box%;liste de choix des droites
                         (parent groupe-box-droite)
                         (choices '())
                         (label "")
                         (min-width 35)
                         (callback (lambda (obj evt)
                                     (send CANVAS on-paint)))))

(define groupe-box-segment (new group-box-panel%;groupement des figure 
                                (label "Segments")
                                (parent HPanelCanvasHisto)))
(define LIST-SEGMENT (new list-box%;liste de choix des segment
                          (parent groupe-box-segment)
                          (choices '())
                          (label "")
                          (min-width 35)
                          (callback (lambda (obj evt)
                                      (send CANVAS on-paint)))))

(define groupe-box-Vecteur (new group-box-panel%;groupement des figure 
                                (label "Vecteur")
                                (parent HPanelCanvasHisto)))
(define LIST-VECTEUR (new list-box%;liste de choix des segment
                          (parent groupe-box-Vecteur)
                          (choices '())
                          (label "")
                          (min-width 35)
                          (callback (lambda (obj evt)
                                      (send CANVAS on-paint)))))

;fonction qui cache les liste de droite ect a gauche de l'ecran
(define (cache)
  (send LIST-DROITE show #f)
  (send LIST-DROITE clear)
  (send LIST-SEGMENT show #f)
  (send LIST-SEGMENT clear)
  (send LIST-VECTEUR show #f)
  (send LIST-VECTEUR clear)
  )
(cache)

;definition des composant du canvas graphique 

(define my-canvas%;definition de la class canvas qui gerera la souris en fonction de etat
  (class canvas%
    (define/override (on-event evt)
      (etat evt);obliger de le faire a chaque mouvement de souris pour le tracer de cercle
      (send this on-paint))
    (super-new)))

(define BITMAP (make-object bitmap% SIZE SIZE))
(define BITMAP-DC (new bitmap-dc% (bitmap BITMAP))) ; le buffer secondaire
(define CANVAS (new my-canvas%
                    (parent HPanelCanvasHisto)
                    (min-width SIZE)
                    (min-height SIZE)
                    (paint-callback (lambda (obj dc)
                                      (send BITMAP-DC clear)
                                      (for-each (lambda (arg)
                                                  (send arg affiche BITMAP-DC));on dessine chacun des composants
                                                CERCLE)
                                      (for-each (lambda (arg)
                                                  (send arg affiche BITMAP-DC))
                                                SEGMENT)
                                      (for-each (lambda (arg)
                                                  (send arg affiche BITMAP-DC))
                                                DROITE)
                                      (for-each (lambda (arg)
                                                  (send arg affiche BITMAP-DC))
                                                POLY4)
                                      (for-each (lambda (arg)
                                                  (send arg affiche BITMAP-DC))
                                                POINTS)
                                      (for-each (lambda (arg)
                                                  (send arg affiche BITMAP-DC))
                                                VECTEUR)
                                      
                                      (let ((droite (send LIST-DROITE get-selection)));on dessine en vert la droite ou segment selectionner
                                        (when droite
                                          (send (recupere-item DROITE droite) force BITMAP-DC)))
                                      
                                      (let ((segment (send LIST-SEGMENT get-selection)));de meme pour les segments
                                        (when segment
                                          (send (recupere-item SEGMENT segment) force BITMAP-DC)))
                                      
                                      (let ((vecteur (send LIST-VECTEUR get-selection)));et les vecteur
                                        (when vecteur
                                          (send (recupere-item VECTEUR vecteur) force BITMAP-DC)))
                                      
                                      (send dc draw-bitmap BITMAP 0 0 'solid)
                                      void))))
;);on affiche le tout



(define groupe-box-historique (new group-box-panel%;groupement des figure 
                                   (label "Historique")
                                   (parent HPanelCanvasHisto)))


(define HISTORIQUE (new editor-canvas%       ;la feuille qui a pour écrivain EDITEUR qui est définis dans un autre module .
                        (parent groupe-box-historique)
                        (editor EDITEUR)
                        (min-width 250)
                        (min-height 600)))




(send FramePrinc show #t)
