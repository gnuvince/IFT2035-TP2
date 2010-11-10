;; fichier: "tp2-def.scm"

;; Copyright (C) 2001-2010 Universite de Montreal, Tous Droits Reserves.

;;-----------------------------------------------------------------------------

;; Definitions pour le TP2.

;;-----------------------------------------------------------------------------

;; Representation des vecteurs et segments.

(define vect cons)
(define vect-x car)
(define vect-y cdr)

(define segm (lambda (depart arrivee) (list depart arrivee)))
(define segm-depart car)
(define segm-arrivee cadr)

;;-----------------------------------------------------------------------------

;; Visualisation d'un dessin.

(define afficher #f) ;; voir plus loin pour la definition

(define dessiner
  (lambda (dessinateur)
    (afficher (dessinateur (lambda (v) v)))))

;;-----------------------------------------------------------------------------

;; Parcours normalises pour les chiffres 0 a 9.

(define parcours-pour-chiffres
  (list->vector
    (list

      (list (vect 0 7/8)        ; 0
            (vect 2/3 1/2)
            (vect 2/3 -1/2)
            (vect 0 -7/8)
            (vect -2/3 -1/2)
            (vect -2/3 1/2)
            (vect 0 7/8))

      (list (vect -1/4 3/4)     ; 1
            (vect 0 7/8)
            (vect 0 -7/8))

      (list (vect -2/3 1/2)     ; 2
            (vect 0 7/8)
            (vect 2/3 1/2)
            (vect -2/3 -7/8)
            (vect 2/3 -7/8))

      (list (vect -2/3 1/2)     ; 3
            (vect 0 7/8)
            (vect 2/3 1/2)
            (vect 0 0)
            (vect 2/3 -1/2)
            (vect 0 -7/8)
            (vect -2/3 -1/2))

      (list (vect 1/2 0)        ; 4
            (vect -1/2 0)
            (vect 1/2 7/8)
            (vect 1/2 -7/8))

      (list (vect 2/3 7/8)      ; 5
            (vect -2/3 7/8)
            (vect -2/3 1/2)
            (vect 2/3 0)
            (vect 2/3 -1/2)
            (vect 0 -7/8)
            (vect -2/3 -1/2))

      (list (vect 2/3 7/8)      ; 6
            (vect 0 7/8)
            (vect -2/3 1/2)
            (vect -2/3 -1/2)
            (vect 0 -7/8)
            (vect 2/3 -1/2)
            (vect 0 0)
            (vect -2/3 0))

      (list (vect -2/3 7/8)     ; 7
            (vect 2/3 7/8)
            (vect 0 0)
            (vect 0 -7/8))

      (list (vect 0 7/8)        ; 8
            (vect 2/3 1/2)
            (vect -2/3 -1/2)
            (vect 0 -7/8)
            (vect 2/3 -1/2)
            (vect -2/3 1/2)
            (vect 0 7/8))

      (list (vect 2/3 0)        ; 9
            (vect 0 0)
            (vect -2/3 1/2)
            (vect 0 7/8)
            (vect 2/3 1/2)
            (vect 2/3 -1/2)
            (vect 0 -7/8)
            (vect -2/3 -1/2))
    )))

;;-----------------------------------------------------------------------------

;; Fonctions utilitaires.

(define foldr
  (lambda (f base lst)
    (if (null? lst)
        base
        (f (car lst) (foldr f base (cdr lst))))))

(define foldl
  (lambda (f base lst)
    (if (null? lst)
        base
        (foldl f (f base (car lst)) (cdr lst)))))

;;-----------------------------------------------------------------------------

;; Cette partie implante le sous-systeme d'affichage graphique.

(define (use-tcltk)

  ;; Utiliser l'interface Tcl/Tk.

  (##namespace ("tcltk#"

   start-event-loop-thread
   join-event-loop-thread
   enter-event-loop

   enable-event-handling
   disable-event-handling

   root-window
   widget?
   widget-name

   set-variable!
   get-variable

   define-procedure
   export-procedure
   remove-procedure

   tcl
   bell
   bind
   bindtags
   bitmap
   button
   canvas
   checkbutton
   clipboard
   destroy
   entry
   event
   focus
   font
   frame
   grab
   grid
   image
   label
   listbox
   lower
   menu
   menubutton
   message
   option
   pack
   photo
   place
   radiobutton
   raise
   scale
   scrollbar
   selection
   send
   text
   tk
   tk_bisque
   tk_chooseColor
   tk_dialog
   tk_focusFollowsMouse
   tk_focusNext
   tk_focusPrev
   tk_getOpenFile
   tk_getSaveFile
   tk_menuSetFocus
   tk_messageBox
   tk_optionMenu
   tk_popup
   tk_setPalette
   tk_textCopy
   tk_textCut
   tk_textPaste
   tkerror
   tkwait
   toplevel
   winfo
   wm
   update

   ))

  ;; La fenetre contient une barre de menu et un canevas:
  ;;
  ;;  +------------------------------------------------------+
  ;;  | TP2                                                  |
  ;;  +------------------------------------------------------+
  ;;  | File                                                 |  <-- menubar
  ;;  +------------------------------------------------------+
  ;;  |                                                      |
  ;;  |                                                      |
  ;;  |                                                      |
  ;;  |                                                      |
  ;;  |                                                      |
  ;;  |                       canvas                         |
  ;;  |                                                      |
  ;;  |                                                      |
  ;;  |                                                      |
  ;;  |                                                      |
  ;;  |                                                      |
  ;;  |                                                      |
  ;;  +------------------------------------------------------+

  (define menubar           ".menubar")
  (define menubar-file      ".menubar.file")
  (define menubar-file-menu ".menubar.file.menu")
  (define canv              ".canv")

  ;; Les operations accessibles dans le menu.

  (define (quit)
    (exit))

  (define (dump)
    (tcl canv 'postscript file: "image.eps"))

  (define (configure)
    (disable-event-handling)
    (let* ((width (string->number (winfo 'width canv)))
           (height (string->number (winfo 'height canv)))
           (ox (quotient width 2))
           (oy (quotient height 2))
           (sx (- ox 25))
           (sy (- oy 25)))
      (redraw segments sx ox (- sy) oy))
    (enable-event-handling))

  (define (redraw segments sx ox sy oy)

    (define (transform x s o)
      (exact->inexact (+ (* s (real-part x)) o)))

    (define (create-line x1 y1 x2 y2 stipple)
      (if stipple
          (tcl canv
               'create
               'line
               (transform x1 sx ox)
               (transform y1 sy oy)
               (transform x2 sx ox)
               (transform y2 sy oy)
               stipple:
               stipple)
          (tcl canv
               'create
               'line
               (transform x1 sx ox)
               (transform y1 sy oy)
               (transform x2 sx ox)
               (transform y2 sy oy)
               width:
               2)))

    (tcl canv 'delete 'all)

    (tcl canv 'create
         'text
         (transform -1 sx ox)
         (transform -1 sy (+ oy 7))
         text:
         "(-1,-1)")
    (tcl canv 'create
         'text
         (transform 1 sx ox)
         (transform 1 sy (- oy 7))
         text:
         "(1,1)")

    (create-line -1 -1 -1 1 "gray50")
    (create-line -1 -1 1 -1 "gray50")
    (create-line -1 1 1 1 "gray50")
    (create-line 1 -1 1 1 "gray50")
    (create-line -1 0 1 0 "gray50")
    (create-line 0 -1 0 1 "gray50")

    (let loop ((lst segments))
      (if (pair? lst)
          (let ((s (car lst)))
            (if (and (pair? s)
                     (pair? (cdr s))
                     (null? (cddr s)))
                (let ((start (segm-depart s))
                      (end (segm-arrivee s)))
                  (if (and (pair? start)
                           (pair? end))
                      (let ((start-x (vect-x start))
                            (start-y (vect-y start))
                            (end-x (vect-x end))
                            (end-y (vect-y end)))
                        (if (and (real? start-x)
                                 (real? end-x)
                                 (real? start-y)
                                 (real? end-y))
                            (create-line start-x start-y end-x end-y #f))))))
            (loop (cdr lst))))))

  (define segments '())

  ;; Creer la fenetre.

  (load "tcltk.o1")

  (disable-event-handling)

  (wm 'title "." "TP2")
  (wm 'geometry "." "306x336")

  ;; Creer barre de menu.

  (frame menubar relief: 'raised bd: 2)

  (menubutton menubar-file text: "File" underline: 0)
  (menu menubar-file-menu tearoff: #f)
  (tcl menubar-file 'configure menu: menubar-file-menu)

  (tcl menubar-file-menu 'add 'command label: "Dump to \"image.eps\""
                         underline: 0 command: dump)
  (tcl menubar-file-menu 'add 'command label: "Quit"
                         underline: 0 command: quit)

  (pack menubar-file side: 'left)

  ;; Creer canevas.

  (canvas canv width: "1c" height: "1c")

  ;; Joindre toutes les sous-fenetres.

  (pack menubar side: 'top fill: 'x)
  (pack canv side: 'top expand: #t fill: 'both)

  ;; Installer un "callback" pour redessiner la fenetre lorsque sa taille
  ;; change.

  (bind canv "<Configure>" configure)

  (enable-event-handling)

  (set! afficher
        (lambda (lst)
          (set! segments lst)
          (if ##tcltk-active?
              (configure)))))

(define (use-jazz)

  ;; Utiliser l'interface Jazz.

  ;; rediriger repl-output
  (##vector-set!
   (##thread-repl-channel-get! (current-thread))
   4
   (current-output-port))

  (set! afficher tp2.view.jazz-afficher))

(if (##unbound? (##global-var-ref (##make-global-var 'tp2.view.jazz-afficher)))
    (use-tcltk)
    (use-jazz))

;;-----------------------------------------------------------------------------
