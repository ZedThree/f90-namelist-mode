;; f90-namelist
;; An extension to f90-mode for Fortran namelists
;;
;; D. Dickinson and P. Hill
;; 2013

;; User options

(defgroup f90-namelist nil
  "An extension to f90-mode for Fortran namelists"
  :prefix "f90-"
  :group 'f90)

(defcustom f90-nml-startreg "^ *&[a-zA-Z1-9_]+"
  "Namelist start regexp, note we only match namelists with a name"
  :type  'string
  :group 'f90-namelist)
(put 'f90-nml-startreg 'safe-local-variable 'stringp)

(defcustom f90-nml-endreg "^ */"
  "Namelist end regexp"
  :type  'string
  :group 'f90-namelist)
(put 'f90-nml-endreg 'safe-local-variable 'stringp)

(defun f90-inside-nml ()
  "Returns t if currently inside a namelist and nil if not"
  (interactive)
  (setq st (save-excursion (re-search-backward f90-nml-startreg 0 t)))
  (setq en (save-excursion (re-search-backward f90-nml-endreg 0 t)))
  (when (not st)
    (setq st 0))
  (when (not en)
    (setq en 0))
  ( > st en))

(defun f90-insert-namelist-safe (nml-name)
  "Insert a new f90 namelist. If point is inside a namelist, then
insert the new one after"
  (interactive "sNamelist name: ")
  (when (f90-inside-nml)
      (f90-next-namelist))
  (insert (format "\n&%s" nml-name))
  (if f90-auto-keyword-case
      (funcall f90-auto-keyword-case -1))
  (insert "\n\n/\n")
  (forward-line -2))

(defun f90-next-namelist ()
  "Find the next namelist"
  (interactive)
  (re-search-forward f90-nml-endreg))

(defun f90-previous-namelist ()
  "Find the next namelist"
  (interactive)
  (re-search-backward f90-nml-startreg))


