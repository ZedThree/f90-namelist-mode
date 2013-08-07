;; f90-namelist
;; An extension to f90-mode for Fortran namelists
;;
;; D. Dickinson and P. Hill
;; 2013

;;Basics for setting up a mode
(defvar f90-namelist-mode-hook nil)

(defvar f90-namelist-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for f90-namelist major mode")


;Basic keywords to highlight
(defconst f90-namelist-font-lock-keywords-1
  (list
   '(f90-namelist-startreg . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for f90-namelist mode")

;More highlighting
(defconst f90-namelist-font-lock-keywords-2
  (append f90-namelist-font-lock-keywords-1
	  (list   '(f90-namelist-endreg. font-lock-keyword-face)
		     '("=" . font-lock-constant-face)))
  "Additional Keywords to highlight in f90-namelist mode")

;Most highlighting
(defconst f90-namelist-font-lock-keywords-3
  (append f90-namelist-font-lock-keywords-2
	  (list   '("!|#" . font-lock-constant-face)))
  "Highest highlighting level in f90-namelist mode")

;;Put the highlighting keywords into a standard var
(defvar f90-namelist-font-lock-keywords f90-namelist-font-lock-keywords-3
  "Default highlighting expressions for f90-namelist mode")

;;Define a function to indent lines, just use the f90-mode
;;function for now
(defun f90-namelist-indent-line ()
  "Indent current line as f90-namelist line"
  (interactive)
  (beginning-of-line)
  (f90-indent-line)
  )

;;This is actually the mode definition, note we derive it from f90-mode
(define-derived-mode f90-namelist-mode f90-mode "f90-namelist"
  "Major mode for editing Fortran namelist files"
  (set (make-local-variable 'font-lock-defaults) '(f90-namelist-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'f90-namelist-indent-line))

;;Now actually provide the mode
(provide 'f90-namelist-mode)

;; User options
(defgroup f90-namelist nil
  "An extension to f90-mode for Fortran namelists"
  :prefix "f90-"
  :group 'f90)

(defcustom f90-namelist-startreg "^ *&[a-zA-Z1-9_]+"
  "Namelist start regexp, note we only match namelists with a name"
  :type  'string
  :group 'f90-namelist)
(put 'f90-namelist-startreg 'safe-local-variable 'stringp)

(defcustom f90-namelist-endreg "^ */"
  "Namelist end regexp"
  :type  'string
  :group 'f90-namelist)
(put 'f90-namelist-endreg 'safe-local-variable 'stringp)

(defun f90-inside-namelist ()
  "Returns t if currently inside a namelist and nil if not"
  (interactive)
  (setq st (save-excursion (re-search-backward f90-namelist-startreg 0 t)))
  (setq en (save-excursion (re-search-backward f90-namelist-endreg 0 t)))
  (when (not st)
    (setq st 0))
  (when (not en)
    (setq en 0))
  ( > st en))

(defun f90-insert-namelist-safe (namelist-name)
  "Insert a new f90 namelist. If point is inside a namelist, then
insert the new one after"
  (interactive "sNamelist name: ")
  (when (f90-inside-namelist)
      (f90-next-namelist))
  (insert (format "\n&%s" namelist-name))
  (if f90-auto-keyword-case
      (funcall f90-auto-keyword-case -1))
  (insert "\n\n/\n")
  (forward-line -2))

(defun f90-next-namelist ()
  "Find the next namelist"
  (interactive)
  (re-search-forward f90-namelist-endreg))

(defun f90-previous-namelist ()
  "Find the next namelist"
  (interactive)
  (re-search-backward f90-namelist-startreg))

(defun f90-buffer-is-input-file ()
  "Determine if current buffer is a Fortran input file.
If so, return true."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq noncomment-regexp "^ *[^!]")
    (re-search-forward noncomment-regexp)
    (beginning-of-line)
    (looking-at (concat "\\(" f90-namelist-startreg "\\|" f90-namelist-endreg "\\)"))))
