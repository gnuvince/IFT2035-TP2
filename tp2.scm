;; fichier: "tp2.scm"

;; auteur(s):
;; Vincent Foley-Bourgon (FOLV08078309)


(define *distance-minimale* 1/10)

;; identity function
;; T -> T
(define id
  (lambda (x)
    x))




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

;; (define liste-points
;;   (lambda (t d a)
;;     (if (< (distance d a) *distance-minimale*)
;;         (list (segm (t d) (t a)))
;;         (let ((m (point-milieu d a)))
;;           (append (liste-points t a m)
;;                   (liste-points t m d))))))

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
