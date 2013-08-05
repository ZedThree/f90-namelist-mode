;; f90-namelist
;; An extension to f90-mode for Fortran namelists
;;
;; D. Dickinson and P. Hill
;; 2013

;;Define local variables
;;-->Namelist start regexp, note we only match namelists with a name
(set (make-local-variable 'f90-nml-startreg) "^ *&[a-zA-Z1-9_]+")
;;-->Namelist end regexp
(set (make-local-variable 'f90-nml-endreg) "^ */")

(defun inside-nml ()
  "Returns t if currently inside a namelist and nil if not"
  (interactive)
  (setq st (save-excursion (re-search-backward f90-nml-startreg 0 t)))
  (setq en (save-excursion (re-search-backward f90-nml-endreg 0 t)))
  (when (not st)
    (setq st 0))
  (when (not en)
    (setq en 0))
  ( > st en))

(defun insert-f90-namelist-safe ()
  "Insert a new f90 namelist"
  (interactive "")
  (if (inside-nml)
      (message "Inside a namelist!")
    (setq nml-name (read-from-minibuffer "Namelist name:"))
    (insert (format "\n&%s" nml-name))
    (if f90-auto-keyword-case
	(funcall f90-auto-keyword-case -1))
    (insert "\n\n/")
    (previous-line)))

