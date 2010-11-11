;; fichier: "tp2.scm"

;; auteur(s):
;; Vincent Foley-Bourgon (FOLV08078309)




(define *distance-minimale* 1/10)
(define *pi* 3.141592)



;;;;; Fonctions mathématiques
(define translate-segm
  (lambda (s dx dy)
    (let ((translate-x (lambda (x) (+ x dx)))
          (translate-y (lambda (y) (+ y dy))))
      (segm (modifier-vect (segm-depart s) translate-x translate-y)
            (modifier-vect (segm-arrivee s) translate-x translate-y)))))


(define rotate-vect
  (lambda (v angle)
    (vect (+ (* (vect-x v) (cos angle))
             (* (vect-y v) (sin angle)))
          (- (* (vect-y v) (cos angle))
             (* (vect-x v) (sin angle))))))

(define rotate-segm
  (lambda (s angle)
    (segm (rotate-vect (segm-depart s) angle)
          (rotate-vect (segm-arrivee s) angle))))

(define point-milieu
  (lambda (v1 v2)
    (vect (/ (+ (vect-x v1) (vect-x v2)) 2)
          (/ (+ (vect-y v1) (vect-y v2)) 2))))

(define distance
  (lambda (v1 v2)
    (let ((dx (- (vect-x v1) (vect-x v2)))
          (dy (- (vect-y v1) (vect-y v2))))
      (sqrt (+ (* dx dx) (* dy dy))))))

(define liste-points
  (lambda (t a d acc)
    (if (< (distance a d) *distance-minimale*)
        (cons (segm (t a) (t d)) acc)
        (let ((m (point-milieu d a)))
          (liste-points t a m
                        (liste-points t m d acc))))))
;;;;;




;;;;; Fonctions utilitaires
(define id
  (lambda (x)
    x))


(define o
  (lambda (f1 f2)
    (lambda (x)
      (f1 (f2 x)))))

;; Retourne (vect (fx v.x) (fy v.y))
(define modifier-vect
  (lambda (v fx fy)
    (vect (fx (vect-x v))
          (fy (vect-y v)))))
;;;;;





(define ligne
  (lambda (depart arrivee)
    (lambda (transf)
      (liste-points transf depart arrivee '()))))


(define parcours->dessinateur
  (lambda (vect-list)
    (lambda (transf)
      (letrec ((loop
                (lambda (lst)
                  (case (length lst)
                    ((0) '())
                    ((1) (liste-points transf (car lst) (car lst) '()))
                    (else (append (liste-points transf (car lst) (cadr lst) '())
                                  (loop (cdr lst))))))))
        (loop vect-list)))))


(define translation
  (lambda (dx dy dessinateur)
    (lambda (transf)
      (map (lambda (segment) (translate-segm segment dx dy)) (dessinateur transf)))))


(define rotation
  (lambda (angle dessinateur)
    (lambda (transf)
      (map (lambda (segment) (rotate-segm segment angle)) (dessinateur transf)))))


(define ell
    (parcours->dessinateur (list (vect -1/2 1)
                                 (vect -1/2 -1)
                                 (vect 1/2 -1))))

(define losange
  (parcours->dessinateur (list (vect -1 0)
                               (vect 0 1)
                               (vect 1 0)
                               (vect 0 -1)
                               (vect -1 0))))

(define triangle
  (parcours->dessinateur (list (vect -1 -1)
                               (vect 0 1)
                               (vect 1 -1)
                               (vect -1 -1))))

(define vide
  (parcours->dessinateur '()))
