;; fichier: "tp2.scm"

;; auteur(s):
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)


(define *distance-minimale* 1/10)
(define *pi* 3.141592)



;;;;; FONCTIONS MATHÉMATIQUES
;; Retourne la valeur d'un angle x en radians
(define degre->radian
  (lambda (x)
    (* *pi* (/ x 180))))

;; Retourne le vecteur milieu sur le segment de droite v2-v1
(define point-milieu
  (lambda (v1 v2)
    (vect (/ (+ (vect-x v1) (vect-x v2)) 2)
          (/ (+ (vect-y v1) (vect-y v2)) 2))))

;; Retourne la distance entre deux vecteurs
(define distance
  (lambda (v1 v2)
    (let ((dx (- (vect-x v1) (vect-x v2)))
          (dy (- (vect-y v1) (vect-y v2))))
      (sqrt (+ (* dx dx) (* dy dy))))))

;; Retourne une liste de vecteur de longueur <= *distance-minimale*
;; entre les vecteurs v1 et v2.
(define liste-vects
  (lambda (t v1 v2 acc)
    (if (< (distance v1 v2) *distance-minimale*)
        (cons (segm (t v1) (t v2)) acc)
        (let ((m (point-milieu v1 v2)))
          (liste-vects t v1 m
                        (liste-vects t m v2 acc))))))

;; Retourne un segment de droite où les deux vecteurs
;; ont été translatés par dx et dy.
(define translate-segm
  (lambda (s dx dy)
    (let ((translate-x (lambda (x) (+ x dx)))
          (translate-y (lambda (y) (+ y dy))))
      (segm (modifier-vect (segm-depart s) translate-x translate-y)
            (modifier-vect (segm-arrivee s) translate-x translate-y)))))

;; Retourne un segment de droite où les deux vecteurs
;; ont été étirés selon des scalaires factx et facty.
(define etire-segm
  (lambda (s factx facty)
    (let ((etire-x (lambda (x) (* x factx)))
          (etire-y (lambda (y) (* y facty))))
      (segm (modifier-vect (segm-depart s) etire-x etire-y)
            (modifier-vect (segm-arrivee s) etire-x etire-y)))))


;; Retourne un vecteur rotationé de `angle` degrés.
(define rotate-vect
  (lambda (v angle)
    (vect (+ (* (vect-x v) (cos angle))
             (* (vect-y v) (sin angle)))
          (- (* (vect-y v) (cos angle))
             (* (vect-x v) (sin angle))))))

;; Retourne un segment de droite où les deux vecteurs
;; ont été rotationé de `angle` degrés.
(define rotate-segm
  (lambda (s angle)
    (segm (rotate-vect (segm-depart s) angle)
          (rotate-vect (segm-arrivee s) angle))))

;; Applique un effet loupe à un vecteur.
(define loupe-vect
  (lambda (v fact)
    (let* ((x (vect-x v))
           (y (vect-y v))
           (m (/ (+ 1 fact)
                 (+ 1 (* fact
                         (+ (* x x)
                            (* y y)))))))
      (vect (* x m) (* y m)))))

;; Retourne un segment de droite où les vecteurs
;; ont été appliqués d'un effet de loupe.
(define loupe-segm
  (lambda (s fact)
    (segm (loupe-vect (segm-depart s) fact)
          (loupe-vect (segm-arrivee s) fact))))
;;;;;




;;;;; FONCTIONS UTILITAIRES
(define id
  (lambda (x)
    x))

;; Retourne (vect (fx v.x) (fy v.y))
(define modifier-vect
  (lambda (v fx fy)
    (vect (fx (vect-x v))
          (fy (vect-y v)))))

;; Retourne la liste des chiffres d'un nombre entier positif.
(define entier->chiffres
  (lambda (n)
    (letrec ((loop
              (lambda (m acc)
                (if (<= m 9)
                    (cons m acc)
                    (loop (quotient m 10) (cons (modulo m 10) acc))))))
      (loop n '()))))


;; Retourne la profondeur d'un arbre
;; arbre vide = 0
;; arbre      = 1 + max(gauche, droite)
(define profondeur-arbre
  (lambda (lst)
    (cond
     ((not (pair? lst)) 0)
     (else (+ 1 (max (profondeur-arbre (car lst))
                     (profondeur-arbre (cdr lst))))))))


;; Retourne le nombre de feuilles (atome ou ()) d'un arbre.
(define compter-feuilles
  (lambda (lst)
    (cond
     ((not (pair? lst)) 1)
     (else (+ (compter-feuilles (car lst))
              (compter-feuilles (cdr lst)))))))
;;;;;



;;;;; DESSINATEURS
;; Dessine un segment entre les points `depart` et `arrivee`.
(define ligne
  (lambda (depart arrivee)
    (lambda (transf)
      (liste-vects transf depart arrivee '()))))


;; Dessine les segments v1->v2, v2->v3, ... vn-1 -> vn
(define parcours->dessinateur
  (lambda (vect-list)
    (lambda (transf)
      (letrec ((loop
                (lambda (lst)
                  (case (length lst)
                    ((0) '())
                    ((1) (liste-vects transf (car lst) (car lst) '()))
                    (else (append (liste-vects transf (car lst) (cadr lst) '())
                                  (loop (cdr lst))))))))
        (loop vect-list)))))


;; Déplace un dessin dans une direction horizontale `dx` et verticale `dy`.
(define translation
  (lambda (dx dy dessinateur)
    (lambda (transf)
      (map (lambda (segment) (translate-segm segment dx dy))
           (dessinateur transf)))))


;; Tourne un dessin autour d'un angle en degrés.
(define rotation
  (lambda (angle dessinateur)
    (lambda (transf)
      (map (lambda (segment) (rotate-segm segment (degre->radian angle)))
           (dessinateur transf)))))


;; Réduit/augmente la taille d'un dessin par un facteur horizontal `factx`
;; et un facteur vertical `facty`.
(define reduction
  (lambda (factx facty dessinateur)
    (lambda (transf)
      (map (lambda (segment) (etire-segm segment factx facty))
           (dessinateur transf)))))


;; Applique un effet de loupe à un dessin.
(define loupe
  (lambda (fact dessinateur)
    (lambda (transf)
      (map (lambda (segment) (loupe-segm segment fact))
           (dessinateur transf)))))


;; Dessine deux dessins l'un par-dessus l'autre.
(define superposition
  (lambda (dessinateur1 dessinateur2)
    (lambda (transf)
      (append (dessinateur1 transf) (dessinateur2 transf)))))


;; Affiche un dessin au-dessus d'un autre selon une proportion donnée.
(define pile
  (lambda (prop dessinateur1 dessinateur2)
    (lambda (transf)
      (let ((m (- 1 prop)))
        (append ((translation 0
                              (- m)
                              (reduction 1 prop dessinateur1)) transf)
                ((translation 0
                              prop
                              (reduction 1 m dessinateur2)) transf))))))


;; Affiche un dessin un à côté de l'autre selon une proportion donnée.
(define cote-a-cote
  (lambda (prop dessinateur1 dessinateur2)
    (lambda (transf)
      (let ((m (- 1 prop)))
        (append ((translation (- m)
                              0
                              (reduction prop 1 dessinateur1)) transf)
                ((translation prop
                              0
                              (reduction m 1 dessinateur2)) transf))))))

;; Dessine le chiffre `d`.
(define chiffre
  (lambda (d)
    (parcours->dessinateur (vector-ref parcours-pour-chiffres d))))


;; Dessine l'entier `n` en affichant côte-à-côte ses chiffres.
(define entier->dessinateur
  (lambda (n)
    (let ((ds (entier->chiffres n)))
      (letrec ((loop
                (lambda (len lst)
                  (if (= len 1)
                      (chiffre (car lst))
                      (cote-a-cote (/ 1 len)
                                   (chiffre (car lst))
                                   (loop (- len 1) (cdr lst)))))))
        (loop (length ds) ds)))))


;; Dessin d'un L.
(define ell
  (parcours->dessinateur (list (vect -1/2 1)
                               (vect -1/2 -1)
                               (vect 1/2 -1))))


;; Dessin d'un losange.
(define losange
  (parcours->dessinateur (list (vect -1 0)
                               (vect 0 1)
                               (vect 1 0)
                               (vect 0 -1)
                               (vect -1 0))))


;; Dessin d'un triangle.
(define triangle
  (parcours->dessinateur (list (vect -1 -1)
                               (vect 0 1)
                               (vect 1 -1)
                               (vect -1 -1))))


;; Dessin vide, utilisé pour "clearer" le canvas.
(define vide
  (parcours->dessinateur '()))


;; Fonction auxiliaire pour arbre->dessinateur
;; * Si l'arbre est () ou un atome: ne rien afficher
;; * Si l'arbre a des branches: afficher une ligne à gauche
;;   et à droite et s'appeler récursivement avec le sous-arbre
;;   gauche et le sous-arbre droit.
(define arbre->dessinateur-aux
  (lambda (lst hauteur largeur x y)
    (if (not (pair? lst))
        vide
        (let* ((noeuds-gauche (compter-feuilles (car lst)))
               (noeuds-droite (compter-feuilles (cdr lst)))
               (new-x1 (- x (/ noeuds-droite largeur)))
               (new-x2 (+ x (/ noeuds-gauche largeur)))
               (new-y (- y hauteur)))
          (superposition
           (ligne (vect x y) (vect new-x1 new-y))
           (superposition
            (ligne (vect x y) (vect new-x2 new-y))
            (superposition
             (arbre->dessinateur-aux (car lst) hauteur largeur new-x1 new-y)
             (arbre->dessinateur-aux (cdr lst) hauteur largeur new-x2 new-y))))))))


;; Dessine un arbre
(define arbre->dessinateur
  (lambda (lst)
    (if (null? lst)
        vide
        (arbre->dessinateur-aux
         lst                            ; l'arbre
         (/ 2 (profondeur-arbre lst))   ; la hauteur d'un niveau
         (compter-feuilles lst)         ; le nombre total de feuilles
         0                              ; position x initiale
         1))))                          ; position y initiale
;;;;;
