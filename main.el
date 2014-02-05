;; ######################################################
;; personalize prelude
;; from: http://batsov.com/prelude/
;(load-theme 'solarized-light t)

;(defun disable-guru-mode ()
;  (guru-mode -1)
;  )
;(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)
;
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq package-user-dir "~/.emacs.d/elpa")
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))


;; ######################################################
;; Hide Icons
(tool-bar-mode -1)


;; ######################################################
(server-start)


;; ######################################################
;; von: http://www.zonix.de/html40/linux/emacsgnus.html
;; hier liegen alle meine Emacs- und Gnus-Einstellungen
(defvar my-elisp-dir "~/.emacs.d/")

;; Datei laden, wenn vorhanden, sonst Warnung
(defun my-load-local-el (part)
  (let ((fullname (concat my-elisp-dir part)))
    (if (file-exists-p fullname)
	(load fullname)
      (message (format "Loading %s (source)...failed" fullname)))))



;; ######################################################
;; Activate UTF-8 mode:
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; 2013-12-10 IRC #Emacs
(set-clipboard-coding-system 'utf-8)

;; http://www.masteringemacs.org/articles/2012/08/09/working-coding-systems-unicode-emacs/
;; in addition to the lines above:

(set-default-coding-systems 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(defvar universal-coding-system-env-list '("PYTHONIOENCODING")
  "List of environment variables \\[universal-coding-system-argument] should set")

(defadvice universal-coding-system-argument (around provide-env-handler activate)
  "Augments \\[universal-coding-system-argument] so it also sets environment variables

Naively sets all environment variables specified in
`universal-coding-system-env-list' to the literal string
representation of the argument `coding-system'.

No guarantees are made that the environment variables set by this advice support
the same coding systems as Emacs."
  (let ((process-environment (copy-alist process-environment)))
    (dolist (extra-env universal-coding-system-env-list)
      (setenv extra-env (symbol-name (ad-get-arg 0))))
    ad-do-it))



;; ######################################################
;; http://sunsite.univie.ac.at/textbooks/emacs/emacs_33.html
;; set start of week to Monday (instead of sunday)
(setq calendar-week-start-day 1)


;; ######################################################
;; damit C-x r o keine TABs benutzt:
(setq-default indent-tabs-mode t)


;; ######################################################
(add-hook 'write-file-hooks 'time-stamp)



;; ######################################################
;; 2011-04-20: turn off backup files
					;(defun turn-off-backup())
					;disable backup
(setq backup-inhibited t)


;; ######################################################
;; Emacs config switch depending on hostname or operating system
;; https://sigquit.wordpress.com/2008/09/28/single-dot-emacs-file/
;; Get current system's name
(defun my-insert-system-name()
  (interactive)
  "Get current system's name"
  (insert (format "%s" system-name))
  )

;; Get current system type
(defun my-insert-system-type()
  (interactive)
  "Get current system type"
  (insert (format "%s" system-type))
  )

;; Check if system is Darwin/Mac OS X
(defun my-system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin")
  )

;; Check if system is GNU/Linux
(defun my-system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux")
  )

(defun my-system-is-gary ()
  (interactive)
  "Return true if the system we are running on is gary"
  (string-equal system-name "gary")
  )
(defun my-system-is-blanche ()
  (interactive)
  "Return true if the system we are running on is blanche"
  (or (string-equal system-name "blanche") (string-equal system-name "blanche.lan"))
  )
(defun my-system-is-grmlvrs ()
  (interactive)
  "Return true if the system we are running on is grmlvrs"
  (string-equal system-name "grmlvrs")
  )
(defun my-system-is-powerplant ()
  (interactive)
  "Return true if the system we are running on is powerplant"
  (or
   (string-equal system-name "powerplant")
   (string-equal system-name "powerplant.lan")
   )
  )
(defun my-system-is-powerplantwin ()
  (interactive)
  "Return true if the system we are running on is powerplant"
  (string-equal system-name "ATGRZ4043268B")
  )

;; example:
;; (when (my-system-is-gary)
;; (setq user-mail-address "my.name@workplace.com")
;; )




;; ######################################################
;; http://www.emacswiki.org/emacs/MacOSTweaks#toc13
;; setting path so that Emacs finds aspell and such
(when (my-system-is-blanche)
  (setenv "PATH"
	  (concat (getenv "PATH")
		  ":/Users/vk/bin:/usr/local/texlive/2010/bin/x86_64-darwin:/opt/local/bin:/opt/local/sbin"))
  (setq exec-path (append exec-path
			  '("/opt/local/bin"
			    "/usr/local/texlive/2010/bin/x86_64-darwin"
			    "/usr/local/teTeX/bin/powerpc-apple-darwin-current"
			    )))
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp")
  )


;; ######################################################
;; setting path so that Emacs finds aspell and such
(when (my-system-is-powerplantwin)
  ;;disabled;(setenv "PATH"
  ;;disabled;               (concat (getenv "PATH")
  ;;disabled;		  ":/Users/vk/bin:/usr/local/texlive/2010/bin/x86_64-darwin:/opt/local/bin:/opt/local/sbin"))
  (setq exec-path (append exec-path
			  '("C:/Program Files (x86)/Aspell/bin"
			    ;;disabled; "/usr/local/texlive/2010/bin/x86_64-darwin"
			    ;;disabled; "/usr/local/teTeX/bin/powerpc-apple-darwin-current"
			    )))
  ;;disabled;(add-to-list 'load-path "/opt/local/share/emacs/site-lisp")
  )


;; ######################################################
;; 2011-04-20: increase/set font size
;; http://www.emacswiki.org/emacs/SetFonts
(defun my-increase-fontsize ()
  (interactive)
  "Sets the font to bigger size"
  (set-face-attribute 'default (selected-frame) :height 130)
  )
(defun my-normal-fontsize ()
  (interactive)
  "Sets the font to normal size"
  (set-face-attribute 'default (selected-frame) :height 100)
  )
;; increase fonts on gary by default
(when (or (my-system-is-gary) (my-system-is-powerplant) (my-system-is-powerplantwin))
  (my-increase-fontsize)
  )

;; ######################################################
;; 2011-04-20: increase/set font size
;; http://www.emacswiki.org/emacs/SetFonts
(when (my-system-is-blanche)
  (set-face-attribute 'default (selected-frame) :height 170)
  )




;; ######################################################
;; Python-specific things only installed on gary:
(when (or (my-system-is-gary) (my-system-is-powerplant))

  ;; ######################################################
  ;; elpy: https://github.com/jorgenschaefer/elpy/wiki/
  (elpy-enable)
  (elpy-use-ipython)

  ;;(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

  ;; ######################################################
  ;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/
  ;; Ropemacs:
  ;;(add-to-list 'load-path "~/.emacs.d/vendor/pymacs-0.24-beta2")
  ;;disabled; (require 'pymacs)
  ;;disabled; (pymacs-load "ropemacs" "rope-")
  ;;disabled; (setq ropemacs-enable-autoimport t)

  ;; ######################################################
  ;; auto-complete mode
  ;;(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete-1.2")
  ;;disabled; (require 'auto-complete-config)
  ;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete-1.2/dict")
  ;;disabled; (ac-config-default)

  ;; ######################################################
  ;; PyFlakes:
  (setq python-check-command "pyflakes")


  ;; ######################################################
  ;; pylookup: https://github.com/tsgates/pylookup

  ;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup
  (setq pylookup-dir "~/.emacs.d/contrib/pylookup")
  (add-to-list 'load-path pylookup-dir)

  ;; load pylookup when compile time
  (eval-when-compile (require 'pylookup))

  ;; set executable file and db file
  (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
  (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

  ;; set search option if you want
  ;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

  ;; to speedup, just load it on demand
  (autoload 'pylookup-lookup "pylookup"
    "Lookup SEARCH-TERM in the Python HTML indexes." t)

  (autoload 'pylookup-update "pylookup"
    "Run pylookup-update and create the database at `pylookup-db-file'." t)

  )


;; ######################################################
;; LaTeX
(when (or (my-system-is-gary) (my-system-is-powerplant))


  ;; ######################################################
  ;; acticate AucTeX and set general preferences
  (require 'tex-site)
  (setq TeX-PDF-mode t)  ;; compile to PDF using pdflatex (instead to DVI)


  ;; ######################################################
  ;; synctex
  ;; http://www.bleedingmind.com/index.php/2010/06/17/synctex-on-linux-and-mac-os-x-with-emacs/
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (setq TeX-source-correlate-method 'synctex)

  (defun okular-make-url () (concat
			     "file://"
			     (expand-file-name (funcall file (TeX-output-extension) t)
					       (file-name-directory (TeX-master-file)))
			     "#src:"
			     (TeX-current-line)
			     (TeX-current-file-name-master-relative))
	 "./"
	 (TeX-current-file-name-master-relative)
	 )

  (defun skim-make-url () (
			   concat
			   (TeX-current-line)
			   " "
			   (expand-file-name (funcall file (TeX-output-extension) t)
					     (file-name-directory (TeX-master-file)))
			   " "
			   (buffer-file-name))
	 )

  (setq TeX-view-program-list '(
				("Okular" "okular --unique %u")
				("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")
				)
	)

  (when (my-system-type-is-gnu)
    (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))
    (eval-after-load "tex"
      '(add-to-list 'TeX-expand-list '("%u" okular-make-url))
      )
    )

  (when (my-system-type-is-darwin)
    (setq TeX-view-program-selection '((output-pdf "Skim")))
    (eval-after-load "tex"
      '(add-to-list 'TeX-expand-list '("%q" skim-make-url))
      )
    )


  ;; (add-hook ‘LaTeX-mode-hook
  ;; (lambda ()
  ;; (add-to-list ‘TeX-expand-list
  ;; ‘(“%u” okular-make-url))))

  ;; ######################################################
  ;; http://www.tug.org/pipermail/macostex-archives/2005-November/018997.html
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'LaTeX-mode-hook '(lambda () (TeX-fold-mode 1)))


  ;; ######################################################
  ;; biblatex and AucTeX
  ;; http://www.mail-archive.com/auctex@gnu.org/msg04137.html
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
		  '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber") t))

  (defun TeX-run-Biber (name command file)
    "Create a process for NAME using COMMAND to format FILE with Biber."
    (let ((process (TeX-run-command name command file)))
      (setq TeX-sentinel-function 'TeX-Biber-sentinel)
      (if TeX-process-asynchronous
	  process
	(TeX-synchronous-sentinel name file process))))

  (defun TeX-Biber-sentinel (process name)
    "Cleanup TeX output buffer after running Biber."
    (goto-char (point-max))
    (cond
     ;; Check whether Biber reports any warnings or errors.
     ((re-search-backward (concat
			   "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
			   "\\(warnings?\\|error messages?\\))") nil t)
      ;; Tell the user their number so that she sees whether the
      ;; situation is getting better or worse.
      (message (concat "Biber finished with %s %s. "
		       "Type `%s' to display output.")
	       (match-string 1) (match-string 2)
	       (substitute-command-keys
		"\\\\[TeX-recenter-output-buffer]")))
     (t
      (message (concat "Biber finished successfully. "
		       "Run LaTeX again to get citations right."))))
    (setq TeX-command-next TeX-command-default))


  ;; ######################################################
  ;; http://staff.science.uva.nl/~dominik/Tools/cdlatex/
  ;; CDLaTeX - more LaTeX functionality
  ;; http://orgmode.org/org.html#CDLaTeX-mode
  ;;disabled;(when (or (my-system-is-gary) (my-system-is-powerplant))
  ;;disabled;  (my-load-local-el "contrib/cdlatex.el")
  ;;disabled;  )

  ;; ######################################################
  ;; http://staff.science.uva.nl/~dominik/Tools/cdlatex/
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (make-variable-buffer-local 'TeX-master) ;; I think this is need because the variable is not buffer local until Auctex is active
  ;;disabled; (defun org-mode-reftex-setup ()
  ;;disabled;   (setq TeX-master t)
  ;;disabled;   (load-library "reftex")
  ;;disabled;   (and (buffer-file-name)
  ;;disabled;	 (file-exists-p (buffer-file-name))
  ;;disabled;	 (progn
  ;;disabled;	   (reftex-parse-all)
  ;;disabled;	   (reftex-set-cite-format "[[cite:%l][%l]]")))
  ;;disabled;   (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  ;;disabled;   (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))
  ;;disabled; (add-hook 'org-mode-hook 'org-mode-reftex-setup)


  );; end LaTeX settings



;; ######################################################
;; http://www.gnu.org/software/auctex/manual/reftex.html#SEC48
;; Specify my bibtex folder
					;(setq reftex-bibpath-environment-variables
					;      '("~/archive/library/"))
;; does not work :-(
;; tested with tagstore.org


;; ######################################################
;; 2011-04-20: allow typing of german umlauts in OS X by Alt-u followed by u,o,a,...
(when (my-system-is-blanche)
  (setq mac-option-modifier nil)
  )


;; ######################################################
;; ditaa
(when (my-system-is-blanche)
  (setq org-ditaa-jar-path "~/data/hosts/blanche/config/ditaa.jar")
  )
(when (or (my-system-is-gary) (my-system-is-powerplant))
  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
  )






;; ######################################################
;; http://blog.zenspider.com/2007/03/locate-and-spotlight.html
(when (my-system-is-blanche)
  (defun locate-make-mdfind-command-line (search-string)
    (list "mdfind" (concat "kMDItemDisplayName=*" search-string "*")))
  (defun spotlight ()
    "Search for files by name using spotlight"
    (interactive)
    (let ((locate-command "mdfind")
	  (locate-make-command-line 'locate-make-mdfind-command-line))
      (call-interactively 'locate nil)))
  (defun spotlight-full ()
    "Search using spotlight"
    (interactive)
    (let ((locate-command "mdfind"))
      (call-interactively 'locate nil)))
  )


;; ######################################################
;; http://stackoverflow.com/questions/4506249/how-to-make-emacs-org-mode-open-links-to-sites-in-google-chrome
(when (or (my-system-is-gary) (my-system-is-powerplant))
  (setq browse-url-browser-function 'browse-url-generic
	;;      browse-url-generic-program "/usr/bin/google-chrome")
	browse-url-generic-program "/usr/bin/firefox")
  )
(when (my-system-is-blanche)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  )

;; (setq browse-url-browser-function 'browse-url-generic
;; ;      browse-url-generic-program "/usr/bin/google-chrome")
;; ;      browse-url-generic-program "/Applications/Firefox.app/Contents/MacOS/firefox-bin")
;;       browse-url-generic-program "/usr/bin/open -a Firefox.app")

;; ######################################################
;; http://stackoverflow.com/questions/4506249/how-to-make-emacs-org-mode-open-links-to-sites-in-google-chrome
;;disabled; (setq browse-url-browser-function 'browse-url-generic
;;disabled;       browse-url-generic-program "chromium-browser")


;; ######################################################
;; https://chrome.google.com/webstore/detail/ljobjlafonikaiipfkggjbhkghgicgoh?hl=de
;; Edit-server for Chrome
					;disabled; ;(require 'edit-server)
					;disabled; (my-load-local-el "contrib/edit-server.el")
					;disabled; (edit-server-start)
;;(if (locate-library "edit-server")
;;    (progn
;;      (require 'edit-server)
;;      (setq edit-server-new-frame nil)
;;      (edit-server-start)))




;; ######################################################
;; http://edward.oconnor.cx/elisp/growl.el
;; use with: M-: (growl "title" "message")
;;(my-load-local-el "contrib/growl.el")

;; ######################################################
;; Org-mode keys for Mac
;; I wanted to map Alt-left|right|... but Alt has to be used by the
;; system in order to type umlauts :-(
;;disabled;(setq mac-command-modifier 'apple)
;;disabled;(global-set-key [(<apple> <up>)] 'org-move-subtree-up)
;;disabled;(global-set-key "\S-<down>" 'org-move-subtree-down)
;;disabled;(global-set-key "\S-<left>" 'org-do-promote)
;;disabled;(global-set-key "\S-<right>" 'org-do-demote)








;; ######################################################
;; flyspell
;; setting path to flyspell-mode.el from MacPorts
(when (my-system-is-blanche)
  (add-to-list 'load-path "/opt/local/share/emacs/lisp/textmodes")
  )

;; (setq ispell-local-dictionary-alist
;;       '(("ndeutsch"
;;  "[a-zA-Z\"]"
;;  "[^a-zA-Z\"]"
;;  "[']" t
;;  ("-C")
;;  "~tex" iso-8859-1)
;;
;; ("ndeutsch8"
;;  "[a-zA-Z\304\326\334\344\366\337\374]"
;;  "[^a-zA-Z\304\326\334\344\366\337\374]"
;;  "[']" t
;;  ("-C" "-d" "ndeutsch")
;;  "~latin1" iso-8859-1)
;; ))

;; User-Dictionary ohne Nachfrage speichern
					;(setq ispell-silently-savep t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flyspell.el <http://kaolin.unice.fr/~serrano/>

;;(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)
;;;(define-key global-map [(f10)] 'flyspell-mode)
;;;(set-default 'ispell-local-dictionary my-german-ispell-dictionary)

;; mein altes Zeug:

;; ######################################################
;; http://www.linuxfaq.de/f/cache/146.html

					;(autoload 'flyspell-mode "flyspell" "On-the-fly ispell." t)
(setq flyspell-issue-welcome-flag nil)
(setq flyspell-default-dictionary "german8")

;;old-method until 2014-01-26;; ;; http://stackoverflow.com/questions/15346723/emacs-and-ispell-error-loading-german8
;;old-method until 2014-01-26;; (eval-after-load "ispell"
;;old-method until 2014-01-26;;   '(add-to-list 'ispell-dictionary-alist
;;old-method until 2014-01-26;; 		'("german8"
;;old-method until 2014-01-26;; 		  "[A-zA-ZäöüßÄÖÜ]" "[^a-zA-ZäöüßÄÖÜ]" "[']" t
;;old-method until 2014-01-26;; 		  ("-C" "-d" "de_DE-neu.multi")
;;old-method until 2014-01-26;; 		  "~latin1" iso-8859-1)))
;;old-method until 2014-01-26;; 
;;old-method until 2014-01-26;; (defun my-ispell-set-deutsch()
;;old-method until 2014-01-26;;   "switch ispell language to Deutsch"
;;old-method until 2014-01-26;;   (interactive)
;;old-method until 2014-01-26;;   (ispell-change-dictionary "german8"))
;;old-method until 2014-01-26;; 
;;old-method until 2014-01-26;; (defun my-ispell-set-USenglish()
;;old-method until 2014-01-26;;   "switch ispell language to US English"
;;old-method until 2014-01-26;;   (interactive)
;;old-method until 2014-01-26;;   (ispell-change-dictionary "american"))
;;old-method until 2014-01-26;; 
;;old-method until 2014-01-26;; (defun my-ispell-set-GBenglish()
;;old-method until 2014-01-26;;   "switch ispell language to British English"
;;old-method until 2014-01-26;;   (interactive)
;;old-method until 2014-01-26;;   (ispell-change-dictionary "british"))
;;old-method until 2014-01-26;; 
;;old-method until 2014-01-26;; (defvar my-toggle-ispell-english-deutsch nil
;;old-method until 2014-01-26;;   "state of english/ngerman toggle. t means english, nil means ngerman")
;;old-method until 2014-01-26;; (make-variable-buffer-local 'my-toggle-ispell-english-deutsch)
;;old-method until 2014-01-26;; 
;;old-method until 2014-01-26;; ;; use british english on powerplant and US english on all other machines:
;;old-method until 2014-01-26;; (if (or (my-system-is-powerplant) (my-system-is-powerplantwin))
;;old-method until 2014-01-26;;     (defun my-toggle-ispell-language ()
;;old-method until 2014-01-26;;       "Toggle ispell-language between british english and ngerman"
;;old-method until 2014-01-26;;       (interactive)
;;old-method until 2014-01-26;;       (cond (my-toggle-ispell-english-deutsch
;;old-method until 2014-01-26;; 	     (setq my-toggle-ispell-english-deutsch nil)
;;old-method until 2014-01-26;; 	     (my-ispell-set-deutsch)
;;old-method until 2014-01-26;; 	     )
;;old-method until 2014-01-26;; 	    (t
;;old-method until 2014-01-26;; 	     (setq my-toggle-ispell-english-deutsch t)
;;old-method until 2014-01-26;; 	     (my-ispell-set-GBenglish)
;;old-method until 2014-01-26;; 	     )
;;old-method until 2014-01-26;; 	    )
;;old-method until 2014-01-26;;       )
;;old-method until 2014-01-26;;   (defun my-toggle-ispell-language ()
;;old-method until 2014-01-26;;     "Toggle ispell-language between english and ngerman"
;;old-method until 2014-01-26;;     (interactive)
;;old-method until 2014-01-26;;     (cond (my-toggle-ispell-english-deutsch
;;old-method until 2014-01-26;; 	   (setq my-toggle-ispell-english-deutsch nil)
;;old-method until 2014-01-26;; 	   (my-ispell-set-deutsch)
;;old-method until 2014-01-26;; 	   )
;;old-method until 2014-01-26;; 	  (t
;;old-method until 2014-01-26;; 	   (setq my-toggle-ispell-english-deutsch t)
;;old-method until 2014-01-26;; 	   (my-ispell-set-USenglish)
;;old-method until 2014-01-26;; 	   )
;;old-method until 2014-01-26;; 	  )
;;old-method until 2014-01-26;;     )
;;old-method until 2014-01-26;;   )

;; from here to my-toggle-ispell-english-deutsch: see id:2014-01-06-aspell-issue
(eval-after-load "ispell"
  '(add-to-list 'ispell-dictionary-alist
                '("german8"
                   "[a-zA-ZäöüßÄÖÜ]" "[^a-zA-ZäöüßÄÖÜ]" "[']" t
                  ("-C" "-d" "de_DE-neu.multi")
                  "~latin1" iso-8859-1)))

(if (my-system-is-powerplantwin)
    ;; use british english on powerplantwin:
    (let ((langs '("german8" "british")))
      (setq lang-ring (make-ring (length langs)))
      (dolist (elem langs) (ring-insert lang-ring elem)))
  ;; use american english on all other systems:
  (let ((langs '("german8" "american")))
    (setq lang-ring (make-ring (length langs)))
    (dolist (elem langs) (ring-insert lang-ring elem)))
  )

(defun my-toggle-ispell-language ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))


;;disabled;(add-hook 'post-mode-hook
;;disabled;         '(lambda ()
;;disabled;            (flyspell-mode t)
;;disabled;            (setq flyspell-generic-check-word-p 'mail-mode-flyspell-verify)))
;; ######################################################


;; http://www.lrde.epita.fr/cgi-bin/twiki/view/Projects/EmacsTricks
;; modes for programming languages; check spelling only in comments/strings
(add-hook          'c-mode-hook 'flyspell-prog-mode)
(add-hook         'sh-mode-hook 'flyspell-prog-mode)
(add-hook        'c++-mode-hook 'flyspell-prog-mode)
(add-hook       'ruby-mode-hook 'flyspell-prog-mode)
(add-hook      'cperl-mode-hook 'flyspell-prog-mode)
(add-hook     'python-mode-hook 'flyspell-prog-mode)
(add-hook   'autoconf-mode-hook 'flyspell-prog-mode)
(add-hook   'autotest-mode-hook 'flyspell-prog-mode)
(add-hook   'makefile-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)





;; ######################################################
;; tabbar
;;disabled;(when (or (my-system-is-gary) (my-system-is-powerplant))
;;disabled;    (my-load-local-el "contrib/tabbar.el")
;;disabled;  )


;; ######################################################
;; yasnippet
;; http://yasnippet.googlecode.com/svn/trunk/doc/index.html
;;disabled;(my-load-local-el "contrib/yasnippet/yasnippet.el")
(require 'yasnippet)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)
(setq yas-indent-line 'fixed) ;; fixes Org-mode issue with yasnippets: https://github.com/capitaomorte/yasnippet/issues/362


;; ######################################################
;; org-mode:
;; additionally: http://stackoverflow.com/questions/3622603/org-mode-setup-problem-when-trying-to-use-capture
(when (or (my-system-is-gary) (my-system-is-blanche) (my-system-is-powerplant) (my-system-is-powerplantwin))

  ;; set paths to manually installed Org-mode (from git)
  ;;disabled;(add-to-list 'load-path "~/.emacs.d/contrib/org-mode/contrib/lisp")
  ;;disabled;(add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/org-mode/lisp"))

  (add-to-list 'load-path "~/.emacs.d/contrib/org-mode/lisp")
  (add-to-list 'load-path "~/.emacs.d/contrib/org-mode/contrib/lisp" t)

  ;; assign file extensions to Org-mode
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

  ;;(require 'org-install) ;; vk 2012-11-20 this line is obsolete
  (require 'org)

  ;; ######################################################
  ;; fix broken mapping (because of prelude and so on)
  ;;disabled;;2014-01-19;;(defun my-org-mode-hook ()
  ;;disabled;;2014-01-19;;  (define-key prelude-mode-map (kbd "C-c +") nil)
  ;;disabled;;2014-01-19;;  (define-key prelude-mode-map (kbd "C-c -") nil)
  ;;disabled;;2014-01-19;;  )
  ;;disabled;;2014-01-19;;(add-hook 'org-mode-hook 'my-org-mode-hook)

  ;; ######################################################
  ;; load Org-mode specific settings:
  ;;disabled;(my-load-local-el "org-mode.el")

  ;; ######################################################
  ;; import iCal to Org-mode
  ;; http://ozymandias.dk/emacs/org-import-calendar.el
  ;; https://raw.github.com/vjohansen/emacs-config/master/org-import-calendar.el
  (my-load-local-el "contrib/org-import-calendar.el")
  (require 'org-import-icalendar)


  ;; ######################################################
  ;; load MISC contrib packages
  (my-load-local-el "contrib/org-mode/contrib/lisp/org-checklist.el")
  (my-load-local-el "contrib/org-mode/contrib/lisp/org-depend.el")
  (my-load-local-el "contrib/org-mode/contrib/lisp/org-expiry.el")
  (my-load-local-el "contrib/org-mode/contrib/lisp/ox-confluence.el")

  ;; ######################################################
  ;; set org-contacts defaults that differ from standard
  (setq org-contacts-address-property "CITY")
  (setq org-contacts-birthday-property "BORN")
  (setq org-contacts-files "~/share/all/org-mode/contacts.org")
  (setq org-contacts-icon-property "PHOTOGRAPH")

  ;; ######################################################
  ;; on an agenda entry: add "-0d" to deadline
  (fset 'my-0d
	[return ?\C-s ?> left ?  ?- ?0 ?d ?\C-x ?b return down])

  ;; ######################################################
  ;; change active timestamp to inactive and cancel event
  ;; in the agenda:
  (fset 'my-agenda-cancel-event-and-set-to-inactive
	(lambda (&optional arg) "Keyboard macro."
	  (interactive "p")
	  (kmacro-exec-ring-item (quote ([return S-up 3 20 99] 0 "%d")) arg)
	  )
	)
  ;; outside of agenda:
  (fset 'my-cancel-event-and-set-to-inactive
	(lambda (&optional arg) "Keyboard macro."
	  (interactive "p")
	  (kmacro-exec-ring-item (quote ([5 18 62 13 S-up 3 20 99] 0 "%d")) arg)
	  )
	)


  ;; ######################################################
  ;; For org appointment reminders
  ;; http://orgmode.org/worg/org-hacks.html#sec-3_1
  ;; Get appointments for today
  (when (or (my-system-is-gary) (my-system-is-powerplant))
    (defun my-org-agenda-to-appt ()
      (interactive)
      (setq appt-time-msg-list nil)
      (let ((org-deadline-warning-days 0))    ;; will be automatic in org 5.23
	(org-agenda-to-appt)))
    ;; Run once, activate and schedule refresh
    (my-org-agenda-to-appt)
    (appt-activate t)
    (run-at-time "24:01" nil 'org-agenda-to-appt)
    ;; 5 minute warnings
    (setq appt-message-warning-time 15)
    (setq appt-display-interval 15)
    ;; Update appt each time agenda opened.
    (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
    ;; Setup zenify, we tell appt to use window, and replace default function
    (setq appt-display-format 'window)
    (setq appt-disp-window-function (function appt-disp-window))
    (defun appt-disp-window (min-to-app new-time msg)
      (save-window-excursion
	(shell-command
	 (concat "/usr/bin/zenity --info --title='Appointment' --text='" msg "' &") nil nil)
	)
      )
    )

  ;; ######################################################
  ;; https://github.com/kiwanami/emacs-calfw
  ;; A calendar framework for Emacs
					;(add-to-list 'load-path "~/.emacs.d/contrib/calfw/")
					;(require 'calfw-org)
  ;; call calendar using:  M-x cfw:open-org-calendar

  ;; ######################################################
  ;; provide a command to use calfw in org-agenda-custom-commands
  ;; https://github.com/kiwanami/emacs-calfw/issues/18
  ;; 2013-01-16: does not work (yet?)
  ;;disabled;(defun open-calfw-agenda-org(&rest args)
  ;;disabled;  (let
  ;;disabled;      (
  ;;disabled;       ;; do not duplicate deadlines
  ;;disabled;       (org-deadline-warning-days 0)
  ;;disabled;       )
  ;;disabled;    (cfw:open-org-calendar)
  ;;disabled;    )
  ;;disabled;  )

  (if (my-system-is-powerplantwin)
      (setq org-agenda-files (append (quote (
					     "c:/Users/karl.voit/share/all/org-mode/phd.org"
					     "c:/Users/karl.voit/share/all/org-mode/r6-stories.org"
					     "c:/Users/karl.voit/share/all/org-mode/infonova.org"
					     "c:/Users/karl.voit/share/all/org-mode/misc.org"
					     "c:/Users/karl.voit/share/all/org-mode/tagstore.org"
					     "c:/Users/karl.voit/share/all/org-mode/ist.org"
					     "c:/Users/karl.voit/share/all/org-mode/contacts.org"
					     "c:/Users/karl.voit/share/all/org-mode/postdoc.org"
					     ;;"c:/Users/karl.voit/share/all/org-mode/foodandbeverages.org"
					     "c:/Users/karl.voit/share/all/org-mode/hardware.org"
					     "c:/Users/karl.voit/share/all/org-mode/notes.org"
					     "c:/Users/karl.voit/share/all/org-mode/outlook.org"
					     "c:/Users/karl.voit/share/all/org-mode/public_voit.org"
					     "c:/Users/karl.voit/share/all/org-mode/errors_public_voit.org"
					     ;;"c:/Users/karl.voit/share/all/org-mode/movies.org"
					     "c:/Users/karl.voit/share/all/org-mode/references.org"
					     ;;"c:/Users/karl.voit/src/lazyblorg/dev/lazyblorg.org"
					     "c:/Users/karl.voit/share/all/org-mode/memacs/error.org"
					     "c:/Users/karl.voit/share/all/org-mode/memacs/git.org"
					     "c:/Users/karl.voit/share/all/org-mode/memacs/ifiles.org"
					     "c:/Users/karl.voit/share/all/org-mode/memacs/phonecalls.org"
					     "c:/Users/karl.voit/share/all/org-mode/memacs/roylog.org"
					     "c:/Users/karl.voit/share/all/org-mode/memacs/SMS.org"
					     )
					    )
				     ;;(file-expand-wildcards "c:/Users/karl.voit/share/all/org-mode/memacs/*.org")
				     )
	    )
    (setq org-agenda-files (append (quote (
					   "~/share/all/org-mode/phd.org"
					   "~/share/all/org-mode/r6-stories.org"
					   "~/share/all/org-mode/infonova.org"
					   ;;			       "~/share/all/org-mode/test-phd.org"
					   "~/share/all/org-mode/misc.org"
					   "~/share/all/org-mode/tagstore.org"
					   "~/share/all/org-mode/ist.org"
					   "~/share/all/org-mode/contacts.org"
					   "~/share/all/org-mode/postdoc.org"
					   "~/share/all/org-mode/foodandbeverages.org"
					   "~/share/all/org-mode/hardware.org"
					   "~/share/all/org-mode/notes.org"
					   "~/share/all/org-mode/movies.org"
					   "~/share/all/org-mode/references.org"
					   "~/share/all/org-mode/outlook.org"
					   "~/share/all/org-mode/public_voit.org"
					   "~/share/all/org-mode/errors_public_voit.org"
					   "~/src/lazyblorg/dev/lazyblorg.org"
					   )
					  )
				   ;;(file-expand-wildcards "~/share/all/org-mode/memacs/*.org")
				   )
	  )
    )



  ;; http://doc.norang.ca/org-mode.html
  ;;
  ;; Standard key bindings
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)

  ;;disabled;(setq mac-command-modifier 'apple)
  ;;disabled;(global-set-key [(<apple> <up>)] 'org-move-subtree-up)
  ;;disabled;(global-set-key "\S-<down>" 'org-move-subtree-down)
  ;;disabled;(global-set-key "\S-<left>" 'org-do-promote)
  ;;disabled;(global-set-key "\S-<right>" 'org-do-demote)


  ;; Make TAB the yas trigger key in the org-mode-hook and enable flyspell mode and autofill
  (add-hook 'org-mode-hook
	    (lambda ()
	      ;;disabled;            ;; yasnippet
	      ;;disabled;            (make-variable-buffer-local 'yas/trigger-key)
	      ;;disabled;            (org-set-local 'yas/trigger-key [tab])
	      ;;disabled;            (define-key yas/keymap [tab] 'yas/next-field)
	      ;; flyspell mode for spell checking everywhere
	      (flyspell-mode 1)
	      ;; Undefine C-c [ and C-c ] since this breaks my org-agenda files when directories are include
	      ;; It expands the files in the directories individually
	      (org-defkey org-mode-map "\C-c["    'undefined)
	      (org-defkey org-mode-map "\C-c]"    'undefined)
	      ;;            (local-set-key (kbd "C-c M-o") 'bh/mail-subtree)
	      )
	    )

  ;;(setq org-agenda-files (quote ("~/misc_documents/org-mode")))

  ;; Custom Key Bindings
  ;;disabled;(global-set-key (kbd "<f12>") 'org-agenda)
  ;;disabled;(global-set-key (kbd "<f5>") 'bh/org-todo)
  ;;disabled;(global-set-key (kbd "<S-f5>") 'bh/widen)
  ;;disabled;(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
  ;;disabled;(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
  ;;disabled;(global-set-key (kbd "<f9> b") 'bbdb)
  ;;disabled;(global-set-key (kbd "<f9> c") 'calendar)
  ;;disabled;(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
  ;;disabled;(global-set-key (kbd "<f9> g") 'gnus)
  ;;disabled;(global-set-key (kbd "<f8>") 'bh/hide-other)

  (defun bh/hide-other ()
    (interactive)
    (save-excursion
      (org-back-to-heading)
      (org-shifttab)
      (org-reveal)
      (org-cycle)))


  (setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAITING(w@/!)" "SOMEDAY(S!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
				  ;;                                (sequence "OPEN(O!)" "|" "CLOSED(C!)")
				  )) )

  (setq org-todo-keyword-faces
	(quote (("TODO"      :foreground "red"          :weight bold)
		("NEXT"      :foreground "blue"         :weight bold)
		("STARTED"   :foreground "blue"         :weight bold)
		("DONE"      :foreground "forest green" :weight bold)
		("WAITING"   :foreground "orange"       :weight bold)
		("SOMEDAY"   :foreground "magenta"      :weight bold)
		("CANCELLED" :foreground "forest green" :weight bold)
		("QUOTE"     :foreground "red"          :weight bold)
		("QUOTED"    :foreground "magenta"      :weight bold)
		("APPROVED"  :foreground "forest green" :weight bold)
		("EXPIRED"   :foreground "forest green" :weight bold)
		("REJECTED"  :foreground "forest green" :weight bold)
		("OPEN"      :foreground "blue"         :weight bold)
		("CLOSED"    :foreground "forest green" :weight bold)
		("PHONE"     :foreground "forest green" :weight bold))))

  (setq org-use-fast-todo-selection t)

  (setq org-treat-S-cursor-todo-selection-as-state-change nil)


  ;; CANCELED -> ARCHIVE: http://article.gmane.org/gmane.emacs.orgmode/64852
  (setq org-todo-state-tags-triggers
	(quote (("CANCELLED"
		 ("ARCHIVE" . t))
		("WAITING"
		 ("WAITING" . t))
		(done
		 ("WAITING"))
		("TODO"
		 ("WAITING")
		 ("CANCELLED"))
		("NEXT"
		 ("WAITING"))
		("STARTED"
		 ("WAITING"))
		("DONE"
		 ("WAITING")
		 ("CANCELLED")))))

  ;; http://orgmode.org/org.html#Setting-up-capture
  (setq org-default-notes-file "~/share/all/org-mode/inbox.org")
  (define-key global-map "\C-cc" 'org-capture)

  ;; I use C-M-r to start capture mode
  ;; (global-set-key (kbd "C-M-r") 'org-capture)

  ;;   ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
  ;;   (setq org-capture-templates (quote (("t" "todo" entry (file "~/misc_documents/org-mode/refile.org") "* TODO %?
  ;; %U
  ;; %a" :clock-in t :clock-resume t)
  ;;                                       ("n" "note" entry (file "~/misc_documents/org-mode/refile.org") "* %?                                                                            :NOTE:
  ;; %U
  ;; %a
  ;; :LOGBOOK:
  ;; :END:" :clock-in t :clock-resume t)
  ;;                                       ("j" "Journal" entry (file+datetree "~/misc_documents/org-mode/diary.org") "* %?
  ;; %U" :clock-in t :clock-resume t)
  ;;                                       ("w" "org-protocol" entry (file "~/misc_documents/org-mode/refile.org") "* TODO Review %c
  ;; %U" :immediate-finish t)
  ;;                                       ("p" "Phone call" entry (file "~/misc_documents/org-mode/refile.org") "* PHONE %(bh/phone-call) - %(gjg/bbdb-company) :PHONE:
  ;; %U
  ;;
  ;; %?" :clock-in t :clock-resume t)
  ;;                                       ("h" "Habit" entry (file "~/misc_documents/org-mode/refile.org") "* TODO %?
  ;; SCHEDULED: %t
  ;; :PROPERTIES:
  ;; :STYLE: habit
  ;; :END:"))))

  ;; 2011-05-16: #org-mode Tipp von madalu: Eingabe von Appointments
  ;; ("a" "Appointment" entry
  ;;   (file "~/org/inbox.org")
  ;;   "* %^{Appt}\n %^t%?\n %U\n %a\n\n %i")


  ;; ;; Remove empty LOGBOOK drawers on clock out
  ;; (defun bh/remove-empty-drawer-on-clock-out ()
  ;;   (interactive)
  ;;   (save-excursion
  ;;     (beginning-of-line 0)
  ;;     (org-remove-empty-drawer-at "LOGBOOK" (point))))
  ;;
  ;; (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)



  ;; Use IDO for target completion
  (setq org-completion-use-ido t)

  ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

  ;; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path (quote file))

  ;; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  ;; Use IDO for both buffer and file completion and ido-everywhere to t
  ;; These are set via the customize interface since setq's do not
  ;; work according to the docstring
  ;;
  ;; '(ido-mode (quote both) nil (ido))
  ;; '(ido-everywhere t)

  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)

  ;; Changed in v7.9.3
  ;; http://orgmode.org/worg/doc.html#org-use-tag-inheritance
  (setq org-agenda-use-tag-inheritance (quote (agenda)))

  ;; disable property inheritance (in order to seed up)
  ;; https://www.gnu.org/software/emacs/manual/html_node/org/Property-inheritance.html
  (setq org-use-property-inheritance nil)


  (setq org-agenda-custom-commands
	(quote (

		("p" "events Prio [#A]"
		 ((agenda "+PRIORITY=\"A\""
			  ((org-agenda-ndays 31)
			   (org-agenda-time-grid nil)
			   (org-agenda-entry-types '(:timestamp :sexp))
			   (org-agenda-skip-function
			    '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")))
			  ;; (
			  ;;  (org-agenda-skip-function 'tag-without-done-or-canceled)
			  ;;  )
			  )))

		("r" "reward tasks" (
				     (tags-todo "reward/!STARTED"
						(
						 (org-agenda-overriding-header "rewards: STARTED")
						 ))
				     (tags-todo "reward/!NEXT"
						(
						 (org-agenda-overriding-header "rewards: NEXT")
						 ))
				     (tags-todo "reward/!TODO"
						(
						 (org-agenda-overriding-header "rewards: TODO")
						 ))
				     (tags-todo "reward/!SOMEDAY"
						(
						 (org-agenda-overriding-header "rewards: SOMEDAY")
						 ))
				     ))

		;;disabled;              ("R" "grab reward" tags-todo "reward/!TODO|SOMEDAY"
		;;disabled;	       (
		;;disabled;		(org-agenda-overriding-header "rewards: TODO or SOMEDAY")
		;;disabled;		))

		("b" "borrowed" tags "+borrowed"
		 (
		  (org-agenda-overriding-header "borrowed or lend")
		  (org-agenda-skip-function 'tag-without-done-or-canceled)
		  ))

		("B" "Besorgungen" tags "+Besorgung"
		 (
		  (org-agenda-overriding-header "Besorgungen")
		  (org-agenda-skip-function 'tag-without-done-or-canceled)
		  ))

		("O" "SOMEDAY" tags-todo "+TODO=\"SOMEDAY\""
		 (
		  (org-agenda-overriding-header "SOMEDAY is today! :-)")
		  ))

		("h" "home @ALW" tags-todo "+@ALW"
		 (
		  (org-agenda-overriding-header "home tasks")
		  ))

		;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-07/msg01374.html
		("E" "events only" agenda ""
		 (
		  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'todo))
		  ))

		("o" "overview Agenda" (
					(agenda ""
						nil )
					;diabled by nil above;			((org-agenda-skip-function '(my-skip-tag "reward"))
					;diabled by nil above;			 (org-agenda-overriding-header "Agenda without rewards: ")))
					(tags "+TODO=\"DONE\"+CLOSED>=\"<today>\""
					      (
					       (org-agenda-overriding-header "DONE today")
					       ))
					;diabled;                (tags "+reward"
					;diabled;                           (
					;diabled;			    (org-agenda-overriding-header "Rewards")
					;diabled;                            ;(org-agenda-skip-function 'bh/skip-non-stuck-projects))
					;diabled;                            ;(org-agenda-todo-ignore-scheduled 'future)
					;diabled;                            ;(org-agenda-todo-ignore-deadlines 'future)
					;diabled;			   )
					;diabled;			   )
					;too slow - dont need;                (tags-todo "-CANCELLED/!"
					;too slow - dont need;                           ((org-agenda-overriding-header "Stuck Projects")
					;too slow - dont need;                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
					;slow;                (tags-todo "-CANCELLED+WAITING/!"
					;slow;                           ((org-agenda-overriding-header "Waiting and Postponed Tasks")
					;slow;                            (org-agenda-skip-function 'bh/skip-stuck-projects)
					;slow;                            (org-tags-match-list-sublevels nil)
					;slow;                            (org-agenda-todo-ignore-scheduled 'future)
					;slow;                            (org-agenda-todo-ignore-deadlines 'future)
					;slow;			    ))
					;;                (tags "REFILE"
					;;                      ((org-agenda-overriding-header "Tasks to Refile")
					;;                       (org-tags-match-list-sublevels nil)))
					;;                (tags "-REFILE/"
					;;                      ((org-agenda-overriding-header "Tasks to Archive")
					;;                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
					;;                       (org-tags-match-list-sublevels nil)))
					;;                (tags-todo "-HOLD-CANCELLED/!"
					;;                           ((org-agenda-overriding-header "Projects")
					;;                            (org-agenda-skip-function 'bh/skip-non-projects)
					;;                            (org-agenda-sorting-strategy
					;;                             '(category-keep))))
					)
		 nil)

		;;disabled;;	      ("x" "Borrowed"
		;;disabled;;	       ((agenda ""
		;;disabled;;			((org-agenda-skip-function 'tag-without-done-or-canceled)
		;;disabled;;			 (org-agenda-overriding-header "Fun: ")))))

		("X" "calfw" open-calfw-agenda-org)


					;2012-12-10 deactivated;	      ("c" . "custom searches") ; description for "c" prefix
					;2012-12-10 deactivated;              ("cr" "rewards" agenda "+reward"
					;2012-12-10 deactivated;	       (
					;2012-12-10 deactivated;		(org-agenda-overriding-header "rewards")
					;2012-12-10 deactivated;		(org-agenda-time-grid nil)
					;2012-12-10 deactivated;		(org-agenda-entry-types '(:deadline))
					;2012-12-10 deactivated;	       ))
					;2012-12-10 deactivated;              ("cr" "rewards" agenda "+reward"
					;2012-12-10 deactivated;	       (
					;2012-12-10 deactivated;		(org-agenda-overriding-header "rewards")
					;2012-12-10 deactivated;		(org-agenda-time-grid nil)
					;2012-12-10 deactivated;		(org-agenda-entry-types '(:deadline))
					;2012-12-10 deactivated;	       ))
					;2012-12-10 deactivated;              ("ct" "@TUG" tags-todo "+@TUG"
					;2012-12-10 deactivated;               ((org-agenda-overriding-header "TUG Tasks")))
					;2012-12-10 deactivated;              ("ca" "@ALW" tags-todo "+@ALW"
					;2012-12-10 deactivated;               ((org-agenda-overriding-header "ALW Tasks")))
					;2012-12-10 deactivated;              ("cb" "Besorgungen" tags-todo "+Besorgung"
					;2012-12-10 deactivated;               ((org-agenda-overriding-header "Besorgungen")))
					;2012-12-10 deactivated;              ("cs" "Started tasks" tags-todo "/!STARTED"
					;2012-12-10 deactivated;               ((org-agenda-overriding-header "Started Tasks")))
					;2012-12-10 deactivated;              ("n" "Next and Started tasks" tags-todo "-WAITING-CANCELLED/!NEXT|STARTED"
					;2012-12-10 deactivated;               ((org-agenda-overriding-header "Next Tasks")))
					;2012-12-10 deactivated;              ("p" "Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED-WAITING-SOMEDAY"
					;2012-12-10 deactivated;               ((org-agenda-skip-function 'bh/skip-non-projects)
					;2012-12-10 deactivated;                (org-agenda-overriding-header "Projects")))
		;; ("o" "Other (Non-Project) tasks" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED-WAITING-SOMEDAY"
		;;  ((org-agenda-skip-function 'bh/skip-projects)
		;;   (org-agenda-overriding-header "Other Non-Project Tasks")))
		;; ("A" "Tasks to be Archived" tags "LEVEL=2-REFILE/DONE|CANCELLED"
		;;  ((org-agenda-overriding-header "Tasks to Archive")
		;;   (org-agenda-skip-function 'bh/skip-non-archivable-tasks)))
					;2012-12-10 deactivated;              ("h" "Habits" tags-todo "STYLE=\"habit\""
					;2012-12-10 deactivated;               ((org-agenda-todo-ignore-with-date nil)
					;2012-12-10 deactivated;                (org-agenda-todo-ignore-scheduled nil)
					;2012-12-10 deactivated;                (org-agenda-todo-ignore-deadlines nil)
					;2012-12-10 deactivated;                (org-agenda-overriding-header "Habits")))
		;;              ("#" "Stuck Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
		;;               ((org-agenda-skip-function 'bh/skip-non-stuck-projects)
		;;                (org-agenda-overriding-header "Stuck Projects")))
		;;               ("*" "All open TODO tasks" tags-todo "-CANCELLED"
		;;               ((org-agenda-overriding-header "All Open TODO tasks")
		;;                (org-agenda-todo-ignore-with-date nil)
		;;                (org-agenda-todo-ignore-scheduled nil)
		;;                (org-agenda-todo-ignore-deadlines nil)
		;;                (org-agenda-todo-ignore-timestamp nil)
		;;                (org-agenda-todo-list-sublevels t)
		;;                (org-tags-match-list-sublevels 'indented)))
		)))


  (setq org-tags-match-list-sublevels nil)



  ;; Tags with fast selection keys
  ;; http://orgmode.org/org.html#Setting-tags
  (setq org-tag-alist (quote (
			      ("Kommunikation" . ?k)
			      ("Besorgung" . ?B)
			      ("nonComputer" . ?n)
			      ("fitness" . ?f)
			      (:startgroup)
			      ("@ALW" . ?a)
			      ("@Infonova" . ?i)
			      ("@Stadt" . ?s)
			      ("@out_of_town" . ?o)
			      ("@Ebreichsdorf" . ?e)
			      ("@TUG" . ?t)
			      (:endgroup)
			      (:startgroup)
			      ("private" . ?p)
			      ("public" . ?P)
			      (:endgroup)
			      (:startgroup)
			      ("bigRock" . ?b)
			      ("MIT" . ?m)
			      ("reward" . ?r)
			      (:endgroup)
			      )))
  ;;			    (:newline)

  ;; Allow setting single tags without the menu
  ;; http://orgmode.org/org.html#Setting-tags
  ;;(setq org-fast-tag-selection-single-key (quote expert))

  ;; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)



  ;; http://orgmode.org/org.html#Weekly_002fdaily-agenda
  (setq org-agenda-span 1)


  ;; 2012-12-09 From: Memnon Anon <gegendosenfleisch@googlemail.com>
  ;; To: news1142@Karl-Voit.at
  ;; Subject: Re: Custom agenda: search by tag and exclude DONE items
  (defun tag-without-done-or-canceled ()
    "Show items with tag \"borrowed\" that are neither in \"DONE\" or \"CANCELED \" state."
    (let ((state (org-entry-get (point) "TODO")))
      (if (and (member "borrowed" (org-get-tags-at (point)))
	       (not (string= state "DONE"))
	       (not (string= state "CANCELED")))
	  nil ; do not skip
	(line-end-position)))) ; skip


  ;; 2012-12-09 From: Memnon Anon <gegendosenfleisch@googlemail.com>
  ;; Newsgroups: gmane.emacs.orgmode
  ;; Subject: Re: Exclude tag from custom agenda
  ;; Date: Sun, 9 Dec 2012 15:59:48 +0000 (UTC)
  ;; Message-ID: <871uezql9d.fsf@mean.albasani.net>
  ;; Based on http://article.gmane.org/gmane.emacs.orgmode/41427
  (defun my-skip-tag(tag)
    "Skip entries that are tagged TAG"
    (let* ((entry-tags (org-get-tags-at (point))))
      (if (member tag entry-tags)
	  (progn (outline-next-heading) (point))
	nil)))

  (defun bh/is-project-p ()
    "Any task with a todo keyword subtask"
    (let ((has-subtask)
	  (subtree-end (save-excursion (org-end-of-subtree t))))
      (save-excursion
	(forward-line 1)
	(while (and (not has-subtask)
		    (< (point) subtree-end)
		    (re-search-forward "^\*+ " subtree-end t))
	  (when (member (org-get-todo-state) org-todo-keywords-1)
	    (setq has-subtask t))))
      has-subtask))

  (defun bh/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	   (has-next (save-excursion
		       (forward-line 1)
		       (and (< (point) subtree-end)
			    (re-search-forward "^\\*+ \\(NEXT\\|STARTED\\) " subtree-end t)))))
      (if (and (bh/is-project-p) (not has-next))
	  nil ; a stuck project, has subtasks but no next task
	subtree-end)))

  (defun bh/skip-non-projects ()
    "Skip trees that are not projects"
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (bh/is-project-p)
	  nil
	subtree-end)))

  (defun bh/skip-projects ()
    "Skip trees that are projects"
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (bh/is-project-p)
	  subtree-end
	nil)))



  ;;(setq org-archive-mark-done t)



  (defun bh/skip-non-archivable-tasks ()
    "Skip trees that are not available for archiving"
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	   (daynr (string-to-int (format-time-string "%d" (current-time))))
	   (a-month-ago (* 60 60 24 (+ daynr 1)))
	   (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
	   (this-month (format-time-string "%Y-%m-" (current-time)))
	   (subtree-is-current (save-excursion
				 (forward-line 1)
				 (and (< (point) subtree-end)
				      (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
      (if subtree-is-current
	  subtree-end ; Has a date in this month or last month, skip it
	;; Make sure the ARCHIVE property is set for this task
	;; Create one at the parent task if necessary
	(save-excursion
	  (save-restriction
	    (widen)
	    (let ((archive-prop (org-entry-get nil "ARCHIVE" 'inherit))
		  (parent-task))
	      (org-up-heading-safe)
	      (setq parent-task (nth 4 (org-heading-components)))
	      (unless archive-prop
		(setq archive-prop (org-entry-put nil "ARCHIVE" (concat "%s_archive::* " parent-task)))))))
	nil)))



  ;; set up babel support

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (ruby . t)
     (gnuplot . t)
     (sh . t)
     (org . t)
     (R . t)
     (emacs-lisp . t)
     (ditaa . t)
     (dot . t)
     ))
  ;;(setq org-babel-load-languages
  ;;      (quote (
  ;;	      (python . t)
  ;;	      (ruby . t)
  ;;	      (html . t)
  ;;	      (gnuplot . t)
  ;;	      (sh . t)
  ;;	      (org . t)
  ;;	      (R . t)
  ;;	      (emacs-lisp . t)
  ;;	      (ditaa . t)
  ;;	      (dot . t)
  ;;	      )))
  ;;
  ;; Do not prompt to confirm evaluation
  ;; This may be dangerous - make sure you understand the consequences
  ;; of setting this -- see the docstring for details
  (setq org-confirm-babel-evaluate nil)



  ;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))

  ;;disabled;(require 'yasnippet)
  ;;disabled;(yas/initialize)
  ;;disabled;(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

  ;; Make TAB the yas trigger key in the org-mode-hook and enable flyspell mode and autofill
  (add-hook 'org-mode-hook
	    (lambda ()
	      ;; yasnippet
	      ;;disabled;            (make-variable-buffer-local 'yas/trigger-key)
	      ;;disabled;            (org-set-local 'yas/trigger-key [tab])
	      ;;disabled;            (define-key yas/keymap [tab] 'yas/next-field-group)
	      ;; flyspell mode for spell checking everywhere
	      (flyspell-mode 1)
	      ;; auto-fill mode on
	      (auto-fill-mode 1)))


  (setq org-show-entry-below (quote ((default))))


  ;; Always hilight the current agenda line
  (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

  ;; ;; The following custom-set-faces create the highlights
  ;; (custom-set-faces
  ;;   ;; custom-set-faces was added by Custom.
  ;;   ;; If you edit it by hand, you could mess it up, so be careful.
  ;;   ;; Your init file should contain only one such instance.
  ;;   ;; If there is more than one, they won't work right.
  ;;  '(highlight ((t (:background "cyan"))))
  ;;  '(hl-line ((t (:inherit highlight :background "darkseagreen2"))))
  ;;  '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))
  ;;
  ;;
  ;; Keep tasks with dates off the global todo lists
  (setq org-agenda-todo-ignore-with-date nil)

  ;; Allow deadlines which are due soon to appear on the global todo lists
  (setq org-agenda-todo-ignore-deadlines (quote far))

  ;; Keep tasks scheduled in the future off the global todo lists
  (setq org-agenda-todo-ignore-scheduled (quote future))

  ;; Remove completed deadline tasks from the agenda view
  (setq org-agenda-skip-deadline-if-done t)

  ;; Remove completed scheduled tasks from the agenda view
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Remove completed items from search results
  ;;(setq org-agenda-skip-timestamp-if-done t)



  ;; Include agenda archive files when searching for things
  (setq org-agenda-text-search-extra-files (quote (agenda-archives)))






  ;; Show all future entries for repeating tasks
  (setq org-agenda-repeating-timestamp-show-all t)

  ;; Show all agenda dates - even if they are empty
  (setq org-agenda-show-all-dates t)

  ;; Sorting order for tasks on the agenda
  (setq org-agenda-sorting-strategy
	(quote ((agenda habit-down time-up user-defined-up priority-down category-keep)
		(todo priority-down category-keep)
		(tags priority-down category-keep)
		(search category-keep))))

  ;; Start the weekly agenda today
  (setq org-agenda-start-on-weekday nil)

  ;; ;; Enable display of the time grid so we can see the marker for the current time
  ;; (setq org-agenda-time-grid
  ;;       ((daily today remove-match)
  ;;        #("----------------" 0 16
  ;;          (org-heading t))
  ;;        (800 1000 1200 1400 1600 1800 2000)))

  ;; Display tags farther right
					;(setq org-agenda-tags-column -102)
  (setq org-agenda-tags-column -120)

  ;;
  ;; Agenda sorting functions
  ;;
  (setq org-agenda-cmp-user-defined 'bh/agenda-sort)

  (defun bh/agenda-sort (a b)
    "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
    (let (result num-a num-b)
      (cond
       ;; time specific items are already sorted first by org-agenda-sorting-strategy

       ;; non-deadline and non-scheduled items next
       ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

       ;; late deadlines next
       ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

       ;; late scheduled items next
       ;;   ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

       ;; deadlines for today next
       ((bh/agenda-sort-test 'bh/is-due-deadline a b))

       ;; scheduled items for today next
       ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

       ;; late deadlines next
       ;;   ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

       ;; late scheduled items next
       ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

       ;; pending deadlines last
       ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

       ;; finally default to unsorted
       (t (setq result nil)))
      result))

  (defmacro bh/agenda-sort-test (fn a b)
    "Test for agenda sort"
    `(cond
      ;; if both match leave them unsorted
      ((and (apply ,fn (list ,a))
	    (apply ,fn (list ,b)))
       (setq result nil))
      ;; if a matches put a first
      ((apply ,fn (list ,a))
       ;; if b also matches leave unsorted
       (if (apply ,fn (list ,b))
	   (setq result nil)
	 (setq result -1)))
      ;; otherwise if b matches put b first
      ((apply ,fn (list ,b))
       (setq result 1))
      ;; if none match leave them unsorted
      (t nil)))

  (defmacro bh/agenda-sort-test-num (fn compfn a b)
    `(cond
      ((apply ,fn (list ,a))
       (setq num-a (string-to-number (match-string 1 ,a)))
       (if (apply ,fn (list ,b))
	   (progn
	     (setq num-b (string-to-number (match-string 1 ,b)))
	     (setq result (if (apply ,compfn (list num-a num-b))
			      -1
			    1)))
	 (setq result -1)))
      ((apply ,fn (list ,b))
       (setq result 1))
      (t nil)))

  (defun bh/is-not-scheduled-or-deadline (date-str)
    (and (not (bh/is-deadline date-str))
	 (not (bh/is-scheduled date-str))))

  (defun bh/is-due-deadline (date-str)
    (string-match "Deadline:" date-str))

  (defun bh/is-late-deadline (date-str)
    (string-match "In *\\(-.*\\)d\.:" date-str))

  (defun bh/is-pending-deadline (date-str)
    (string-match "In \\([^-]*\\)d\.:" date-str))

  (defun bh/is-deadline (date-str)
    (or (bh/is-due-deadline date-str)
	(bh/is-late-deadline date-str)
	(bh/is-pending-deadline date-str)))

  (defun bh/is-scheduled (date-str)
    (or (bh/is-scheduled-today date-str)
	(bh/is-scheduled-late date-str)))

  (defun bh/is-scheduled-today (date-str)
    (string-match "Scheduled:" date-str))

  (defun bh/is-scheduled-late (date-str)
    (string-match "Sched\.\\(.*\\)x:" date-str))




  ;;(add-to-list 'load-path (expand-file-name "~/misc_documents/org-mode-mode/contrib/lisp"))

  (require 'org-checklist)


  (setq org-enforce-todo-dependencies t)

  (setq org-hide-leading-stars t)

  (setq org-startup-indented t)

  ;; http://orgmode.org/worg/org-faq.html
  (setq org-blank-before-new-entry (quote ((heading . t)
					   (plain-list-item . nil))))

  (setq org-insert-heading-respect-content nil)

  (setq org-reverse-note-order nil)

  (setq org-show-following-heading t)
  (setq org-show-hierarchy-above t)
  (setq org-show-siblings nil)

  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-yank-adjusted-subtrees t)

  (setq org-deadline-warning-days 1)

  (setq org-table-export-default-format "orgtbl-to-csv")

  ;; (setq org-link-frame-setup ((vm . vm-visit-folder)
  ;;                             (gnus . org-gnus-no-new-news)
  ;;                             (file . find-file-other-window)))

  (setq org-log-done (quote time))
  (setq org-log-into-drawer t)
  (setq org-log-redeadline (quote note))
  (setq org-log-reschedule (quote time))


  ;; Enable habit tracking (and a bunch of other modules)
  (setq org-modules (quote (org-bbdb org-bibtex org-crypt org-gnus org-id org-info org-jsinfo org-habit org-inlinetask org-irc org-mew org-mhe org-protocol org-rmail org-vm org-wl org-w3m)))
  ;; global STYLE property values for completion
  (setq org-global-properties (quote (("STYLE_ALL" . "habit"))))
  ;; position the habit graph on the agenda to the right of the default
  (setq org-habit-graph-column 50)



  (setq global-auto-revert-mode t)



  (require 'org-crypt)
  ;; Encrypt all entries before saving
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  (setq org-crypt-key "2704CA24")


  (setq org-use-speed-commands t)
  (setq org-speed-commands-user (quote (("0" . delete-window)
					("1" . delete-other-windows)
					("2" . split-window-vertically)
					("3" . split-window-horizontally)
					("h" . hide-other)
					("k" . org-kill-note-or-show-branches)
					("r" . org-reveal)
					("s" . org-save-all-org-buffers)
					("z" . org-add-note)
					("c" . self-insert-command)
					("C" . self-insert-command)
					("J" . org-clock-goto))))



  (setq require-final-newline nil)



  (defun bh/insert-inactive-timestamp ()
    (interactive)
    (org-insert-time-stamp nil t t nil nil nil))

  ;;(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)




  (setq org-return-follows-link t)


  (setq org-remove-highlights-with-change nil)

  (setq org-read-date-prefer-future nil)

  (setq org-list-demote-modify-bullet (quote (("+" . "-")
					      ("*" . "-")
					      ("1." . "-")
					      ("1)" . "-"))))

  (setq split-width-threshold 9999)

  ;; http://orgmode.org/org.html show blocked tasks in agenda in gray color
  (setq org-agenda-dim-blocked-tasks t)

  ;; automatically change to DONE when all children are done
  ;; http://orgmode.org/org.html#Breaking-down-tasks
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  ;; statistic cookies count ALL subtasks not only direkt ones
  (setq org-hierarchical-todo-statistics t)


  ;; http://repo.or.cz/w/org-mode.git?a=blob_plain;f=contrib/lisp/org-expiry.el;hb=HEAD
  ;; Expiry dates handling
  (require 'org-expiry)
  ;; enables automatically CREATED properties:
  (org-expiry-insinuate)
  ;; not checked yet: (setq org-expiry-handler-function 'org-expiry-archive-subtree)

  ;; http://orgmode.org/org.html#MobileOrg
  ;; directory where to store MobileOrg-files
  (setq org-mobile-directory "~/share/all/org-mode/mobile-org/")
  (setq org-directory "~/share/all/org-mode")
  (setq org-mobile-inbox-for-pull "~/share/all/org-mode/inbox.org")
  (setq org-mobile-force-id-on-agenda-items nil) ;; do not generate IDs for all headings
  (setq org-mobile-files (quote
			  (
			   "~/share/all/org-mode/misc.org"
			   "~/share/all/org-mode/contacts.org"
			   "~/share/all/org-mode/hardware.org"
			   )))


  ;; http://orgmode.org/worg/org-hacks.html#sec-1_3
  ;; preserve top level node and tags when archiving
  (defun my-org-inherited-no-file-tags ()
    (let ((tags (org-entry-get nil "ALLTAGS" 'selective))
	  (ltags (org-entry-get nil "TAGS")))
      (mapc (lambda (tag)
	      (setq tags
		    (replace-regexp-in-string (concat tag ":") "" tags)))
	    (append org-file-tags (when ltags (split-string ltags ":" t))))
      (if (string= ":" tags) nil tags)))
  (defadvice org-archive-subtree (around my-org-archive-subtree-low-level activate)
    (let ((tags (my-org-inherited-no-file-tags))
	  (org-archive-location
	   (if (save-excursion (org-back-to-heading)
			       (> (org-outline-level) 1))
	       (concat (car (split-string org-archive-location "::"))
		       "::* "
		       (car (org-get-outline-path)))
	     org-archive-location)))
      ad-do-it
      (with-current-buffer (find-file-noselect (org-extract-archive-file))
	(save-excursion
	  (while (org-up-heading-safe))
	  (org-set-tags-to tags)))))


  ;; show state changes in log-mode of agenda
  (setq org-agenda-log-mode-items (quote (state)))

  ;; http://orgmode.org/worg/org-faq.html
					;(setq org-agenda-skip-additional-timestamps-same-entry t)
  (setq org-agenda-skip-additional-timestamps-same-entry nil)

  ;; 2013-11-13 from Bastien (Org-ML)
  ;; do not search for time in heading when displaying a date-stamp
  (setq org-agenda-search-headline-for-time nil)

  ;; open agenda in same buffer, full size
  (setq org-agenda-window-setup 'current-window)

  ;; add diary entries in agenda view
  ;; http://orgmode.org/org.html#Weekly_002fdaily-agenda
  (setq org-agenda-include-diary t)

  ;; ######################################################
  ;; http://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/
  ;; org-mode and paper references

  (defadvice reftex-format-citation (before eval-citation-format)
    (setq format (eval format)))

  (defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name) (file-exists-p (buffer-file-name))
	 (progn
					;enable auto-revert-mode to update reftex when bibtex file changes on disk
	   (global-auto-revert-mode t)
	   (reftex-parse-all)
					;add a custom reftex cite format to insert links
	   (reftex-set-cite-format
	    '((?b . "[[bib:%l][%l.bib]]")
	      (?r . "[[ref:%l][%l]]")
	      (?p . "[[pdf:%l][%l.pdf]]")
	      (?a . "[[notes:%l][%l-notes.pdf]]")
	      (?s . "[[pdf:%l-self][%l-self.pdf]]")
	      (?t . "%t")
	      (?h . (concat "** %l - %t\n:PROPERTIES:\n:CREATED: "
			    "<" (substring (format-time-string (org-time-stamp-format t t)) 1 -1) ">"
			    "\n:ID: %l\n:END:\n[[bib:%l][%l.bib]]\n[[pdf:%l][%l.pdf]]\n\n*** Abstract\n\n#+BEGIN_QUOTE\n#+END_QUOTE\n\n"))
	      (?n . (concat "*** PDF Annotations: [[notes:%l][%l-notes.pdf]]\n:PROPERTIES:\n:CREATED: "
			    "<" (substring (format-time-string (org-time-stamp-format t t)) 1 -1) ">"
			    "\n:ID: %l-notes\n:END:\n\n"
			    "\#+begin_src sh :results output :eval no-export\n"
			    "${HOME}/bin/vkextract_annotations_to_orgmode_snippet.sh %l\n"
			    "#+end_src"))
	      ))))
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
    (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

					;(add-hook 'org-mode-hook 'org-mode-reftex-setup)
  (add-hook 'org-mode-hook
	    (lambda ()
	      (if (member "CHECK_NEEDED" org-todo-keywords-1)
		  (org-mode-reftex-setup))))


  (defun org-mode-reftex-search ()
    ;;jump to the notes for the paper pointed to at from reftex search
    (interactive)
    (org-open-link-from-string (format "[[ref:%s]]" (reftex-citation t))))

  (setq org-link-abbrev-alist
	'(
	  ("bib" . "~/archive/library/%s.bib")
	  ("ref" . "file:~/share/all/org-mode/references.org::/%s/")
					;	("mynotes" . "file:~/share/all/org-mode/references.org::%s")
					;	("papers" . "~/archive/library/%s.pdf")
	  ("photo" . "file:~/people/all_photographs/%s")
	  ("pdf" . "~/archive/library/%s.pdf")
	  ("notes" . "~/archive/library/%s-notes.pdf")
	  ("contact" . "~/share/all/org-mode/contacts.org::/\*.*%s/")
	  ("tsfile" . "~/share/all/org-mode/memacs/files.org_archive::/\*.*%s/")
	  ("ifile" . "~/org/memacs/ifiles.org::/\*.*%s/")
	  ("mbox2001" . "file:~/archive/events_memories/backup_2002-06-23/2002-06-23/home/vk/Emails_from_approx_2000-07-01_to_2002-06-24.mbox::/\%s/")
	  ("postings2002" . "file:~/archive/usenet/memacs-archive/2002-03-13_to_2002-06-23_postings_Karl_Voit_usenet::%s")
	  ("postings2001" . "file:~/archive/usenet/memacs-archive/2000-07-06_to_2002-01-28_postings_Karl_Voit_usenet::%s")
	  ("bank" . "file:~/institutions/easybank/Memacs-easybank-summary.csv::%s")
	  ("ipd" . "http://product.infonova.at/jira/browse/IPD-%s")
	  ("IPD" . "http://product.infonova.at/jira/browse/IPD-%s")
	  ))

  ;; tries to format a new BibTeX entry according to own format
  ;; from: http://www.mfasold.net/blog/2009/02/using-emacs-org-mode-to-draft-papers/
					;(my-load-local-el "contrib/format-bib.el")
  ;; does not work well enough


  ;; ######################################################
  ;; encrypting whole files:
  ;; http://orgmode.org/worg/org-tutorials/encrypting-files.html
  (require 'epa-file)
  ;;(epa-file-enable) ;; 2013-08-22: already enabled (somewhere)

  ;; http://anirudhs.chaosnet.org/blog/2005.01.21.html
  ;; disabling auto-save for sensitive files
  (define-minor-mode sensitive-mode
    "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
    ;; The initial value.
    nil
    ;; The indicator for the mode line.
    " Sensitive"
    ;; The minor mode bindings.
    nil
    (if (symbol-value sensitive-mode)
	(progn
	  ;; disable backups
	  (set (make-local-variable 'backup-inhibited) t)
	  ;; disable auto-save
	  (if auto-save-default
	      (auto-save-mode -1)))
      ;;resort to default value of backup-inhibited
      (kill-local-variable 'backup-inhibited)
      ;;resort to default auto save setting
      (if auto-save-default
	  (auto-save-mode 1))))
  ;; disabling auto-save for gpg file extension
  (setq auto-mode-alist
	(append '(("\\.gpg$" . sensitive-mode))
		auto-mode-alist))

  ;; do not ask for disabling auto-save
  (setq org-crypt-disable-auto-save nil)


  ;; ######################################################
  ;; contact management with org-contacts
  ;; http://julien.danjou.info/org-contacts.html
  (require 'org-contacts)
  (custom-set-variables
   '(org-contacts-files "~/share/all/org-mode/contacts.org")
   '(org-contacts-address-property "CITY")
   '(org-contacts-birthday-property "BORN")
   '(org-contacts-icon-property "PHOTOGRAPH")
   )


  ;; ######################################################
  ;; templates for capturing C-c c
  ;; http://orgmode.org/org.html#Capture-templates
  (setq org-capture-templates
	'(
	  ("s" "shorts-todo" entry (file+headline "~/share/all/org-mode/misc.org" "shorts")
	   "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("e" "Event" entry (file+headline "~/share/all/org-mode/misc.org" "Events")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("a" "anzuschauen" entry (file+headline "~/share/all/org-mode/misc.org" "Anzuschauen")
	   "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%x\n\n" :empty-lines 1)
	  ("i" "infonova Templates")
	  ("is" "infonova shorts" entry (file+headline "~/share/all/org-mode/infonova.org" "shorts")
	   "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("ie" "infonova event" entry (file+headline "~/share/all/org-mode/infonova.org" "Events")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("ir" "infonova Templates")
	  ("irs" "infonova R6 shorts" entry (file+headline "~/share/all/org-mode/r6-stories.org" "shorts")
	   "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("ire" "infonova R6 event" entry (file+headline "~/share/all/org-mode/r6-stories.org" "Events")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;("ii" "infonova IPD" entry (file+headline "~/share/all/org-mode/infonova.org" "IPDs")
	  ;; "* IPD-%?: \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;("p" "PhD Templates")
	  ;;("ps" "PhD shorts" entry (file+headline "~/share/all/org-mode/phd.org" "shorts")
	  ;; "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;("pe" "PhD event" entry (file+headline "~/share/all/org-mode/phd.org" "Events")
	  ;; "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;("t" "tagstore Templates")
	  ;;("ts" "tagstore shorts" entry (file+headline "~/share/all/org-mode/tagstore.org" "shorts")
	  ;; "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;("te" "tagstore event" entry (file+headline "~/share/all/org-mode/tagstore.org" "Events")
	  ;; "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("b" "Besorgung" entry (file+headline "~/share/all/org-mode/hardware.org" "Besorgungen")
	   "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;("C" "Clipboard" entry (file+headline "~/share/all/org-mode/misc.org" "shorts")
	  ;; "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%x\n\n" :empty-lines 1)
	  ("c" "capture to inbox, refile later" entry (file "~/share/all/org-mode/inbox.org")
	   "\n* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;("m" "movie" entry (file+headline "~/share/all/org-mode/movies.org" "inbox")
	  ;; "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("x" "xlog")
	  ("xf" "xlog FNS" table-line (id "xlog-fns-id") "| %T |")
	  ("xz" "xlog ZNS" table-line (id "xlog-zns-id") "| %T |")
	  ("xh" "xlog hometrainer" table-line (id "xlog-hometrainer") "| %T |  |  |  |")
	  ("xb" "xlog Bettwäsche" table-line (id "xlog-bettwaesche-id") "| %T |  |  |  |  |")
	  ("xg" "xlog Gewicht" table-line (id "xlog-gewicht") "| %T |  |")
	  ("xr" "Reinigung Geschirrspüler" table-line (id "xlog-Geschirrspuelerreinigung") "| %T |")
	  ("xp" "Pollenallergie Symptome" table-line (id "ad5f7363-e280-4566-912d-1fb5699725da") "| %T |")
	  ("xt" "Pollenallergie Tabletteneinnahme" table-line (id "b718be27a93a35207bac9b18ec390cc3") "| %T |")
	  ("xG" "elmex grün" table-line (id "d705cdf9-40e5-4677-9662-e0e17d05798f") "| %T |")
	  ("xR" "elmex rot" table-line (id "fbd9be0e-5077-4ba9-89eb-6041f945991a") "| %T |")
	  ("xD" "dentalux Complex 3" table-line (id "2013-08-06-detalux3") "| %T |")
	  ("xk" "Keyboard Akkus leer" table-line (id "3407c9b7-1b41-443b-9254-32c4af3a54e8") "| %T |")
	  ("xx" "xlogtest" table-line (file+headline "~/share/all/org-mode/misc.org" "xlogtest2012-06-17") "| %T |")
	  )
	)



  ;; ######################################################
  ;; Austrian Holidays
  ;; from: http://paste.lisp.org/display/96464
  ;; ~/Notes/holidays.el
  (require 'holidays)
  (setq holiday-austria-holidays '((holiday-fixed  1  1 "Neujahr (frei)")
				   (holiday-fixed  1  6 "Heilige Drei Könige (frei)")
				   (holiday-easter-etc 1 "Ostermontag (frei)")
				   (holiday-easter-etc -46 "Aschermittwoch")
				   (holiday-easter-etc -2 "Karfreitag")
				   (holiday-fixed  5  1 "Österreichischer Staatsfeiertag (frei)")
				   (holiday-easter-etc 39 "Christi Himmelfahrt (frei)")
				   (holiday-easter-etc 50 "Pfingstmontag (frei)")
				   (holiday-easter-etc 60 "Fronleichnam (frei)")
				   (holiday-float 5 0 2 "Muttertag")
				   (holiday-float 6 0 2 "Vatertag")
				   (holiday-fixed  8 15 "Mariä Himmelfahrt (frei)")
				   (holiday-fixed 10 26 "Nationalfeiertag (frei)")
				   (holiday-fixed 11  1 "Allerheiligen (frei)")
				   (holiday-fixed 12  8 "Maria Empfängnis (frei)")
				   (holiday-fixed 12 24 "Heiliger Abend (nicht frei)")
				   (holiday-fixed 12 25 "Erster Weihnachtstag (frei)")
				   (holiday-fixed 12 26 "Zweiter Weihnachtstag (frei)")))
					;(setq holiday-other-holidays '((holiday-fixed 10  3 "Tag der Deutschen Einheit")))
  (setq holiday-local-holidays holiday-austria-holidays)
  (setq calendar-holidays (append holiday-local-holidays holiday-other-holidays))
  ;; and add (load "~/Notes/holidays" t) to your .emacs and add
  ;; #+CATEGORY: Feiertag
  ;; %%(org-calendar-holiday)
  ;; to an agenda file
  ;; ######################################################
  ;; Muttertag... from http://debianforum.de/forum/viewtopic.php?f=29&t=67024


  ;; ######################################################
  ;; yasnippet and Org-mode
  ;; from: http://orgmode.org/manual/Conflicts.html
  ;;disabled;(add-hook 'org-mode-hook
  ;;disabled;                    (lambda ()
  ;;disabled;                      (org-set-local 'yas/trigger-key [tab])
  ;;disabled;                      (define-key yas/keymap [tab] 'yas/next-field)))
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
  (add-hook 'org-mode-hook
	    (lambda ()
	      (make-variable-buffer-local 'yas/trigger-key)
	      (setq yas/trigger-key [tab])
	      (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
	      (define-key yas/keymap [tab] 'yas/next-field)))

  ;; ######################################################
  ;; opening image files with external viewer
  ;; http://stackoverflow.com/questions/3973896/emacs-org-mode-file-viewer-associations
  (add-hook 'org-mode-hook
	    '(lambda ()
	       (setq org-file-apps
		     (append '(
			       ("\\.png\\'" . default)
			       ("\\.jpg\\'" . default)
			       ("\\.jpeg\\'" . default)
			       ("\\.tiff\\'" . default)
			       ) org-file-apps ))))

  ;; ######################################################
  ;; customizing the agenda export of C-x C-w
  ;; http://orgmode.org/manual/Exporting-Agenda-Views.html
  (setq org-agenda-exporter-settings
	'((ps-number-of-columns 2)
	  (ps-landscape-mode t)
	  ;;disabled;                  (org-agenda-add-entry-text-maxlines 5)
	  (htmlize-output-type 'css)))

  ;; ######################################################
  ;; setting timezone in order to get a correct iCal export
  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00134.html
  (setq org-icalendar-timezone "Europe/Vienna")

  ;; ######################################################
  ;; setting destination file for iCal export
  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00163.html
  ;; http://orgmode.org/worg/org-tutorials/org-google-sync.html
  ;;disabled; does not work
  ;;disabled;(setq org-combined-agenda-icalendar-file "~/public_html/orgmodevk478.ics")

  ;; ######################################################
  ;; iCal-export to Google
  ;; http://orgmode.org/worg/org-tutorials/org-google-sync.html
  ;; define categories that should be excluded
  (setq org-export-exclude-category (list "google" "private"))
  ;; define filter. The filter is called on each entry in the agenda.
  ;; It defines a regexp to search for two timestamps, gets the start
  ;; and end point of the entry and does a regexp search. It also
  ;; checks if the category of the entry is in an exclude list and
  ;; returns either t or nil to skip or include the entry.
  (defun org-mycal-export-limit ()
    "Limit the export to items that have a date, time and a range. Also exclude certain categories."
    (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\
\)>")
    (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
    (save-excursion
      ;; get categories
      (setq mycategory (org-get-category))
      ;; get start and end of tree
      (org-back-to-heading t)
      (setq mystart    (point))
      (org-end-of-subtree)
      (setq myend      (point))
      (goto-char mystart)
      ;; search for timerange
      (setq myresult (re-search-forward org-tstr-regexp myend t))
      ;; search for categories to exclude
      (setq mycatp (member mycategory org-export-exclude-category))
      ;; return t if ok, nil when not ok
      (if (and myresult (not mycatp)) t nil)))
  ;; activate filter and call export function
  (defun org-mycal-export ()
    (let ((org-icalendar-verify-function 'org-mycal-export-limit))
      (org-export-icalendar-combine-agenda-files)))


  ;; ######################################################
  ;; org-mode export of calendar events to Google
  ;; http://orgmode.org/worg/org-tutorials/org-google-sync.html

  ;; define categories that should be excluded
  (setq org-export-exclude-category (list "google" "private"))

  ;;disabled; ;; define filter. The filter is called on each entry in the agenda.
  ;;disabled; ;; It defines a regexp to search for two timestamps, gets the start
  ;;disabled; ;; and end point of the entry and does a regexp search. It also
  ;;disabled; ;; checks if the category of the entry is in an exclude list and
  ;;disabled; ;; returns either t or nil to skip or include the entry.
  ;;disabled;
  ;;disabled; (defun org-mycal-export-limit ()
  ;;disabled;   "Limit the export to items that have a date, time and a range. Also exclude certain categories."
  ;;disabled;   (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\\)>")
  ;;disabled;   (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
  ;;disabled;   (save-excursion
  ;;disabled;     ; get categories
  ;;disabled;     (setq mycategory (org-get-category))
  ;;disabled;     ; get start and end of tree
  ;;disabled;     (org-back-to-heading t)
  ;;disabled;     (setq mystart    (point))
  ;;disabled;     (org-end-of-subtree)
  ;;disabled;     (setq myend      (point))
  ;;disabled;     (goto-char mystart)
  ;;disabled;     ; search for timerange
  ;;disabled;     (setq myresult (re-search-forward org-tstr-regexp myend t))
  ;;disabled;     ; search for categories to exclude
  ;;disabled;     (setq mycatp (member mycategory org-export-exclude-category))
  ;;disabled;     ; return t if ok, nil when not ok
  ;;disabled;     (if (and myresult (not mycatp)) t nil)))
  ;;disabled;
  ;;disabled; ;; activate filter and call export function
  ;;disabled; (defun org-mycal-export ()
  ;;disabled;   (let ((org-icalendar-verify-function 'org-mycal-export-limit))
  ;;disabled;     (org-export-icalendar-combine-agenda-files)))
  ;;disabled;


  ;; ######################################################
  ;; ignore :noexport: entries in iCal-export
  ;; http://comments.gmane.org/gmane.emacs.orgmode/36415
  (setq org-icalendar-honor-noexport-tag t)

  ;; ######################################################
  ;; add tags to iCal-export
  ;; http://comments.gmane.org/gmane.emacs.orgmode/19148
  (setq org-icalendar-categories (quote (all-tags category)))
  ;;disabled;  '(org-icalendar-include-body 1000)
  ;;disabled;  '(org-icalendar-include-sexps nil)
  ;;disabled;  '(org-icalendar-include-todo nil)
  ;;disabled;  '(org-icalendar-store-UID t)
  ;;disabled;  '(org-icalendar-timezone "Europe/Berlin")
  ;;disabled;  '(org-icalendar-use-deadline (quote (event-if-not-todo event-if-todo)))
  ;;disabled;  '(org-icalendar-use-plain-timestamp nil)
  ;;disabled;  '(org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo)))


  ;; ######################################################
  ;; set the timestamps of expiry.el to inactive ones
  ;; http://comments.gmane.org/gmane.emacs.orgmode/20934
  (setq org-expiry-inactive-timestamps t)

  ;; ######################################################
  (defun my-export-agenda()
    "Exports monthly Org-mode agenda to agenda.ics file"
    (interactive)
    (save-some-buffers)
    (org-agenda-list nil nil 60)
					;test;  (org-agenda-list nil nil 15)
    (org-agenda-write "~/share/all/org-mode/agenda-export-raw.ics")
    (setq scriptpath "~/src/postprocess_Org-mode_iCal_export/")
    (setq icspath "~/share/all/org-mode/")
    (shell-command-to-string (concat
			      scriptpath "postprocess_Org-mode_iCal_export.py "
			      "-i " icspath "agenda-export-raw.ics "
			      "-o " icspath "agenda-export-postprocessed.ics "
			      "--overwrite "
			      "--remove-summary-timestamp"
			      )
			     )
    (if (my-system-is-gary)
	(shell-command-to-string "/home/vk/bin/vk-cronjob-gary-do-unison-sync-unattended-share-all_if_host_is_reachable.sh")
      (message "Please do sync using unison!")
      )
    )

  ;; ######################################################
  ;; http://orgmode.org/Changes.html -> New option org-catch-invisible-edits
  ;; prevent accidental deleting of hole subtrees or similar
  (setq org-catch-invisible-edits "smart")


  ;; ######################################################
  ;; from Eric Schulte <eric.schulte@gmx.com>
  ;;      Newsgroups: gmane.emacs.orgmode
  ;;      Subject: Re: org mode in press
  ;;      Date: Sat, 28 Jan 2012 10:06:08 -0700
  ;;      Message-ID: <87ipjv92pr.fsf@gmx.com>
  ;; syntax highlighting in source code:
  (setq org-src-fontify-natively t)


  ;; ;; ######################################################
  ;; ;; using alternative LaTeX exporter
  ;; ;(require 'org-export)
  ;; ;(require 'org-e-latex)
  ;; ;; invoke: M-x org-export-dispatch
  ;;
  ;;
  ;; ;; ######################################################
  ;; ;; http://orgmode.org/worg/exporters/beamer/ox-beamer.html
  ;; ;; new LaTeX exporter: beamer
  ;; (require 'ox-latex)
  ;; (add-to-list 'org-latex-classes
  ;;              '("beamer"
  ;;                "\\documentclass\[presentation\]\{beamer\}"
  ;;                ("\\section\{%s\}" . "\\section*\{%s\}")
  ;;                ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
  ;;                ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
  ;;
  ;; ;; ######################################################
  ;; ;; adding ACM export class format
  ;; ;; compare: http://orgmode.org/worg/org-tutorials/org-latex-export.html
  ;; (unless (boundp 'org-export-latex-classes)
  ;;   (setq org-export-latex-classes nil))
  ;; (add-to-list 'org-export-latex-classes
  ;;              '("article"
  ;;                "\\documentclass{article}"
  ;;                ("\\section{%s}" . "\\section*{%s}")))
  ;;
  ;; (add-to-list 'org-export-latex-classes
  ;;           '("koma-article"
  ;;              "\\documentclass{scrartcl}
  ;;              [NO-DEFAULT-PACKAGES]
  ;;              [EXTRA]"
  ;;              ("\\section{%s}" . "\\section*{%s}")
  ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;;
  ;; (add-to-list 'org-export-latex-classes
  ;;           '("ACM"
  ;;              "\\documentclass{acm_proc_article-sp}
  ;;              [NO-DEFAULT-PACKAGES]
  ;;              [EXTRA]"
  ;;              ("\\section{%s}" . "\\section*{%s}")
  ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;;

  ;; ;; ######################################################
  ;; ;; adding TUGRAZ letter export class format
  ;; ;; compare: http://orgmode.org/worg/org-tutorials/org-latex-export.html
  ;; (add-to-list 'org-export-latex-classes
  ;;              '("TUGRAZletter"
  ;;                "\\documentclass{scrlttr2}
  ;;                 \\usepackage{tugrazletter}
  ;;                 [NO-DEFAULT-PACKAGES]
  ;;                 [EXTRA]"
  ;;                ("\\section{%s}" . "\\section*{%s}")))


  ;; ######################################################
  ;; remembering positions
  (global-set-key (kbd "C-c %") 'org-mark-ring-push)
  (global-set-key (kbd "C-c <left>") 'org-mark-ring-goto)
  (global-set-key (kbd "C-c <down>") 'org-mark-ring-push)

  ;; ######################################################
  ;; http://draketo.de/light/english/free-software/custom-link-completion-org-mode-25-lines-emacs
  ;; Custom Link Completion
  (defun org-make-link (&rest strings);; see Org-mode posting/answer 2012-08-20
    "Concatenate STRINGS."
    (apply 'concat strings))
  (defun string-replace (this withthat in)
    "replace THIS with WITHTHAT' in the string IN"
    (with-temp-buffer
      (insert in)
      (goto-char (point-min))
      (replace-string this withthat)
      (buffer-substring (point-min) (point-max))))

  (defun org-pdf-complete-link (&optional arg)
    "Create a papers link using completion."
    (let (file link)
      (setq file (read-file-name "pdf: " "~/archive/library/"))
      (let ((pwd (file-name-as-directory (expand-file-name ".")))
	    (pwd1 (file-name-as-directory (abbreviate-file-name
					   (expand-file-name ".")))))
	(setq file (string-replace "~/archive/library/" "" file))
	(setq file (string-replace pwd "" (string-replace pwd1 "" file)))
	(setq file (string-replace ".bib" "" file))
	(setq file (string-replace ".pdf" "" file))
	(setq link (concat "pdf:" file)))
      link))

  (defun org-ref-complete-link (&optional arg)
    "Create a reference link using completion."
    (let (file link)
      (setq file (read-file-name "ref: " "~/archive/library/"))
      (let ((pwd (file-name-as-directory (expand-file-name ".")))
	    (pwd1 (file-name-as-directory (abbreviate-file-name
					   (expand-file-name ".")))))
	(setq file (string-replace "~/archive/library/" "" file))
	(setq file (string-replace pwd "" (string-replace pwd1 "" file)))
	(setq file (string-replace ".bib" "" file))
	(setq file (string-replace ".pdf" "" file))
	(setq link (concat "ref:" file)))
      link))

  (defun org-photo-complete-link (&optional arg)
    "Create a reference link using completion."
    (let (file link)
      (setq file (read-file-name "photo: " "~/people/all_photographs/"))
      (let ((pwd (file-name-as-directory (expand-file-name ".")))
	    (pwd1 (file-name-as-directory (abbreviate-file-name
					   (expand-file-name ".")))))
	(setq file (string-replace "~/people/all_photographs/" "" file))
	(setq file (string-replace pwd "" (string-replace pwd1 "" file)))
					;(setq file (string-replace ".jpg" "" file))
					;(setq file (string-replace ".jpeg" "" file))
					;(setq file (string-replace ".tiff" "" file))
	(setq link (concat "photo:" file)))
      link))




  ;; ######################################################
  (setq org-src-prevent-auto-filling t)



  ;;  ;; ######################################################
  ;;  ;; Automatic asynchronous publish calendar in idle time.
  ;;  ;; modified from:
  ;;  ;;   Automatic asynchronous org-mobile-push in idle time.
  ;;  ;;   https://gist.github.com/3111823
  ;;  ;;   Show a notification when a push has been completed
  ;;
  ;;  (require 'notifications)  ;; FIXXME: Cannot open load file: notifications
  ;;  (defun notify-calendar-export (result)
  ;;    (notifications-notify
  ;;     :title "Calendar export complete"
  ;;     :body  (format "mybody: %s" result)
  ;;    )
  ;;  )
  ;;
  ;;  ;; Fork the work
  ;;  (require 'async) ;; FIXXME: Cannot open load file: async
  ;;  (defun my-generate-ical-calendar-export ()
  ;;    (async-start
  ;;     ;; What to do in the child process
  ;;     `(lambda ()
  ;;        ;,(async-inject-variables "org-\\(mobile-\\|directory\\)")
  ;;        ;(org-mobile-push)
  ;;        (org-agenda-list nil nil 60)
  ;;        (org-agenda-write "~/share/all/org-mode/agenda.ics")
  ;;        )
  ;;
  ;;     ; What to do when it finishes
  ;;     (lambda (result)
  ;;       ;(notify-calendar-export result)
  ;;       nil
  ;;       )))
  ;;
  ;;  ;; Define a timer variable
  ;;  (defvar my-generate-ical-export-timer nil
  ;;    "Timer that `my-generate-ical-export-timer' used to reschedule itself, or nil.")
  ;;
  ;;  ;; Push to mobile when the idle timer runs out
  ;;  (defun my-export-ical-with-delay (secs)
  ;;    (when my-generate-ical-export-timer
  ;;      (cancel-timer my-generate-ical-export-timer))
  ;;    (setq my-generate-ical-export-timer
  ;;          (run-with-idle-timer
  ;;           (* 1 secs) nil 'my-generate-ical-calendar-export)))
  ;;
  ;;  ;; After saving files, start a 30 seconds idle timer after which we
  ;;  ;; are going to push
  ;;  (add-hook 'after-save-hook
  ;;   (lambda ()
  ;;     (when (eq major-mode 'org-mode)
  ;;       (dolist (file (org-mobile-files-alist))
  ;;         (if (string= (expand-file-name (car file)) (buffer-file-name))
  ;;             (my-export-ical-with-delay 30)))
  ;;     )))
  ;;
  ;;  ;; At least run it once a day, but no need for a delay this time
  ;;  (run-at-time "00:05" 86400 '(lambda () (my-export-ical-with-delay 1)))

  ;; ######################################################
  ;; Sticky agendas remain opened in the background so that you don't
  ;; need to regenerate them each time you hit the corresponding
  ;; keystroke. This is a big time saver.
  ;;disabled; (setq org-agenda-sticky t)

  ;; ######################################################
  ;;  Non-nil means skip timestamp line if same entry shows because of deadline.
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)

  ;; ######################################################
  ;; From: Bastien <bzg@altern.org>
  ;; Newsgroups: gmane.emacs.orgmode
  ;; Subject: Re: scale inline images in orgmode
  ;; Date: Thu, 30 Aug 2012 15:52:59 +0200
  ;; Message-ID: <87a9xcsczo.fsf@altern.org>
  ;; You can now (from git master) use `org-image-actual-width'.
  ;; (setq org-image-actual-width 300)
  ;;   => always resize inline images to 300 pixels
  ;; (setq org-image-actual-width '(400))
  ;;   => if there is a #+ATTR.*: width="200", resize to 200,
  ;;      otherwise resize to 400
  ;; (setq org-image-actual-width nil)
  ;;   => if there is a #+ATTR.*: width="200", resize to 200,
  ;;      otherwise don't resize
  ;; (setq org-image-actual-width t)
  ;;   => Never resize and use original width (the default)
  (setq org-image-actual-width '(400))


  ;; ######################################################
  ;; From: http://doc.norang.ca/org-mode.html#CustomAgendaViewFilteringContext
  ;; removes things tagged with "lp" or "reward" when typing "/ RET" in agenda
  (defun bh/org-auto-exclude-function (tag)
    "Automatic task exclusion in the agenda with / RET"
    (and (cond
	  ((string= tag "lp") t)
	  ((string= tag "reward") t)
	  ((string= tag "test") t)
	  )
	 (concat "-" tag)))
  (setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)


  ;;disabled; ;; ######################################################
  ;;disabled; ;; change font for DONE tasks
  ;;disabled; ;; https://lists.gnu.org/archive/html/emacs-orgmode/2007-03/msg00179.html
  ;;disabled; (setq org-fontify-done-headline t)
  ;;disabled; (custom-set-faces
  ;;disabled;  '(org-done ((t (:foreground "PaleGreen"
  ;;disabled;                  :weight normal
  ;;disabled;                  :strike-through t))))
  ;;disabled;  '(org-headline-done
  ;;disabled;             ((((class color) (min-colors 16) (background dark))
  ;;disabled;                (:foreground "LightSalmon" :strike-through t)))))


  ;; ######################################################
  ;; managing bookmarks with Org-mode
  ;; http://orgmode.org/worg/org-contrib/org-protocol.html
  (require 'org-protocol)


  ;; ######################################################
  ;; org-favtable
					;deactivated; (require 'org-favtable)
					;deactivated; (setq org-favtable-id "my-favtable")
					;deactivated; (global-set-key (kbd "C-+") 'org-favtable)

  ;; ######################################################
  ;; org-feed - aggregating RSS feeds in news.org file:
  ;;disabled; (setq org-feed-alist
  ;;disabled;       '(
  ;;disabled;	("heise"
  ;;disabled;          "http://www.heise.de/newsticker/heise.rdf"
  ;;disabled;          "~/share/all/org-mode/news.org" "heise")
  ;;disabled;	("Dilbert"
  ;;disabled;          "http://feeds.feedburner.com/DilbertDailyStrip"
  ;;disabled;          "~/share/all/org-mode/news.org" "Dilbert")
  ;;disabled;	)
  ;;disabled;       )


  ;; ######################################################
  ;; adding Org-mode docu
  (require 'info)
  (add-to-list 'Info-additional-directory-list "~/.emacs.d/contrib/org-mode/doc/")

  ;; ######################################################
  ;; 2013-08-22:
  ;; From: Suvayu Ali <fatkasuvayu+linux@gmail.com>
  ;; Newsgroups: gmane.emacs.orgmode
  ;; Subject: Re: Editing HTML blocks: no special environment to edit here
  ;; docu: http://orgmode.org/worg/exporters/ox-overview.html and http://orgmode.org/worg/org-8.0.html
  (require 'ox-html)
  (require 'ox-latex)
  (require 'ox-koma-letter)
  (require 'ox-beamer)
  (require 'ox-ascii)
					;(require 'ox-odt)
  (require 'ox-freemind)
  (require 'ox-taskjuggler)

  ;; ######################################################
  ;; 2013-09-13:
  ;; From: Release Notes v8.1
  ;; http://orgmode.org/worg/agenda-optimization.html
  (setq org-agenda-ignore-drawer-properties '(effort appt category))


  ;; ######################################################
  ;; 2013-10-11
  ;; From: http://orgmode.org/worg/org-hacks.html#sec-1-4-3
  ;; calculating with dates and times:
  ;;     | Date             | Start | Lunch |  Back |   End |  Sum |
  ;;     |------------------+-------+-------+-------+-------+------|
  ;;     | [2011-03-01 Tue] |  8:00 | 12:00 | 12:30 | 18:15 | 9:45 |
  ;;     #+TBLFM: $6='(with-time t (+ (- $5 $4) (- $3 $2)))

  (defun org-time-string-to-seconds (s)
    "Convert a string HH:MM:SS to a number of seconds."
    (cond
     ((and (stringp s)
	   (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" s))
      (let ((hour (string-to-number (match-string 1 s)))
	    (min (string-to-number (match-string 2 s)))
	    (sec (string-to-number (match-string 3 s))))
	(+ (* hour 3600) (* min 60) sec)))
     ((and (stringp s)
	   (string-match "\\([0-9]+\\):\\([0-9]+\\)" s))
      (let ((min (string-to-number (match-string 1 s)))
	    (sec (string-to-number (match-string 2 s))))
	(+ (* min 60) sec)))
     ((stringp s) (string-to-number s))
     (t s)))

  (defun org-time-seconds-to-string (secs)
    "Convert a number of seconds to a time string."
    (cond ((>= secs 3600) (format-seconds "%h:%.2m:%.2s" secs))
	  ((>= secs 60) (format-seconds "%m:%.2s" secs))
	  (t (format-seconds "%s" secs))))

  (defmacro with-time (time-output-p &rest exprs)
    "Evaluate an org-table formula, converting all fields that look
like time data to integer seconds.  If TIME-OUTPUT-P then return
the result as a time value."
    (list
     (if time-output-p 'org-time-seconds-to-string 'identity)
     (cons 'progn
	   (mapcar
	    (lambda (expr)
	      `,(cons (car expr)
		      (mapcar
		       (lambda (el)
			 (if (listp el)
			     (list 'with-time nil el)
			   (org-time-string-to-seconds el)))
		       (cdr expr))))
	    `,@exprs))))




  );; Org-mode



;; ######################################################
;; http://wason21cn.blogspot.com/feeds/posts/default?orderby=updated
;; Highlight matching parenthesis
(show-paren-mode t)



;; ######################################################
;; post-mode
;;(setq post-variable-signature-source "~/daten/nobackup/funnies/good_sigs/allsigs.txt")
;;(setq post-signature-directory "~/daten/nobackup/funnies/good_sigs/")



;; ######################################################
;; http://ikiwiki.info/tips/Emacs_and_markdown/
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.mdwn" . markdown-mode))
;;(add-hook 'markdown-mode-hook
;;           '(lambda ()
;;               (make-local-hook 'write-contents-hooks)
;;                (add-hook 'write-contents-hooks 'ska-untabify nil t)))



;; ######################################################
;; setting path to color-theme-mode.el from MacPorts
(when (my-system-is-blanche)
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp/color-theme-6.6.0")
  )

;;disabled; ;; ######################################################
;;disabled; ;; https://github.com/sellout/emacs-color-theme-solarized
;;disabled; ;; Solarized Color Theme
;;disabled; (unless (my-system-is-powerplantwin)
;;disabled;   ;; 2. Add the `emacs-color-theme-solarized` directory to your Emacs `load-path`.
;;disabled;   (add-to-list 'load-path "~/.emacs.d/contrib/emacs-color-theme-solarized/")
;;disabled;   ;; 3. Add `(require 'color-theme-solarized)` to your Emacs init file (usually `~/.emacs`).
;;disabled;   (require 'color-theme-solarized)
;;disabled;   ;; 4. Use the usual [color-theme] mechanism to select one of the Solarized themes, or `M-x color-theme-solarized-[light|dark]`.
;;disabled;   (color-theme-solarized-light)
;;disabled;
;;disabled;   (defvar my-toggle-color-theme-state nil
;;disabled;     "state of color-theme toggle. t means dark, nil means light")
;;disabled;   (make-variable-buffer-local 'my-toggle-color-theme-state)
;;disabled;
;;disabled;   (defun my-toggle-color-theme ()
;;disabled;     "Toggle color-theme between color-theme-solarized-dark and color-theme-solarized-light"
;;disabled;     (interactive)
;;disabled;     (cond (my-toggle-color-theme-state
;;disabled;	   (setq my-toggle-color-theme-state nil)
;;disabled;	   (color-theme-solarized-light))
;;disabled;	  (t
;;disabled;	   (setq my-toggle-color-theme-state t)
;;disabled;	   (color-theme-solarized-dark))))
;;disabled;   )




;; ######################################################
;; http://www.emacswiki.org/emacs/MiniMap
;; MiniMap for Emacs
(add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/minimap"))
(require 'minimap)
(setq minimap-window-location 'right)


;; ######################################################
;; http://www.neilvandyke.org/erin-twiki-emacs/
;; TWiki syntax highlighting
;;disabled; ;(require 'erin))
;;disabled; (my-load-local-el "contrib/erin.el")


;; ######################################################
;; http://twitter.com/emacs_knight/status/128339316417101825
;; prefer y/n
(fset 'yes-or-no-p 'y-or-n-p)


;; ######################################################
;; http://www.emacswiki.org/emacs/TwitteringMode
;; http://citizen428.net/blog/2011/01/21/emacs-twittering-mode/
;;disabled; (add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/twittering-mode/"))
;;disabled; (require 'twittering-mode)
;;disabled; (setq twittering-timer-interval 300)  ; Update your timeline each 300 seconds (5 minutes)
;;disabled; (setq twittering-url-show-status nil) ; Keeps the echo area from showing all the http processes
;;disabled; (setq twittering-icon-mode t)         ; Show icons
;;disabled; (setq twittering-use-show-minibuffer-length t) ; Show character count in compose buffer
;;disabled; ;; I added is.gd support myself, pull request sent.
;;disabled; ;; standard options are tinyurl and toly
;;disabled; ;(setq twittering-tinyurl-service 'is.gd)
;;disabled; ;; See http://www.reverttoconsole.com/blog/nix/twitter-mode-for-emacs-with-oauth/
;;disabled; (setq twittering-use-master-password t)
;;disabled; ;; This tells twittering-mode which time line buffers
;;disabled; ;; to open when starting
;;disabled; (setq twittering-initial-timeline-spec-string
;;disabled;       '(":friends"
;;disabled;         ":replies"
;;disabled;         ":direct_messages"
;;disabled;         ":search/tugraz/"
;;disabled;         ":search/tagstore/"
;;disabled;	))
;;disabled; ;; some key bindings
;;disabled; (add-hook 'twittering-mode-hook
;;disabled;           (lambda ()
;;disabled;             (mapc (lambda (pair)
;;disabled;                     (let ((key (car pair))
;;disabled;                           (func (cdr pair)))
;;disabled;                       (define-key twittering-mode-map
;;disabled;                         (read-kbd-macro key) func)))
;;disabled;                   '(("R" . twittering-native-retweet)
;;disabled;                     ("l" . twittering-goto-next-thing)))))
;;disabled; ;; enable spell check
;;disabled; (add-hook 'twittering-edit-mode-hook (lambda () (ispell-minor-mode) (flyspell-mode)))
;;disabled; ;; filter by regex  http://www.emacswiki.org/emacs/TwitteringMode -> "10 May 2011"
;;disabled; (setq twittering-tweet-filters '("foobar42" "foobar 23"))
;;disabled; (defun twittering-filter-tweets ()
;;disabled;   (setq non-matching-statuses '())
;;disabled;   (dolist (status twittering-new-tweets-statuses)
;;disabled;     (setq matched-tweets 0)
;;disabled;     (dolist (pat twittering-tweet-filters)
;;disabled;       (if (string-match pat (cdr (assoc 'text status)))
;;disabled;           (setq matched-tweets (+ 1 matched-tweets))))
;;disabled;     (if (= 0 matched-tweets)
;;disabled;         (setq non-matching-statuses (append non-matching-statuses `(,status)))))
;;disabled;   (setq new-statuses non-matching-statuses))
;;disabled; (add-hook 'twittering-new-tweets-hook 'twittering-filter-tweets)
;;disabled;


;; ######################################################
;; http://www.emacswiki.org/emacs/UndoTree
(when (or (my-system-is-gary) (my-system-is-blanche))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/undo-tree"))
  (require 'undo-tree)
  (global-undo-tree-mode)
  )


;; ######################################################
;; http://code.google.com/p/emacs-open-resource/
;;disabled; (add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/emacs-open-resource-read-only"))
;;disabled; (require 'open-resource)
;;disabled; (global-set-key "\C-cr" 'open-resource)


;; ######################################################
;; from Twitter 2012-05-22: @emacs_knight
(when (or (my-system-is-gary) (my-system-is-blanche))
  (whitespace-mode)
  (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace
					;(face trailing lines-tail) whitespace-line-column 80) ;; highlight long lines tails (setq whitespace-style
  )


;; ######################################################
;; http://emacswiki.org/emacs/EdiffMode
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
    (ediff file1 file2)))
(add-to-list 'command-switch-alist '("diff" . command-line-diff))
;; Usage: emacs -diff file1 file2


;; ######################################################
;; deletes duplicate entries of history
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-History.html
(setq history-delete-duplicates t)


;; ######################################################
;; counting words
;; http://www.emacswiki.org/emacs/WordCount
;; http://www.emacswiki.org/emacs/wc.el
(my-load-local-el "contrib/wc.el")


;; ######################################################
;; only one window on startup
;; http://thornydev.blogspot.co.at/2012/08/happiness-is-emacs-trifecta.html
(add-hook 'emacs-startup-hook 'delete-other-windows t)




;; ######################################################
;; magit
(when (or (my-system-is-gary) (my-system-is-powerplant))
  (require 'magit)

  ;; full screen magit-status
  ;; http://whattheemacsd.com//setup-magit.el-01.html
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  )

;; ######################################################
;; http://www.emacswiki.org/emacs-es/RecentFiles
;; recently files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)



;; ######################################################
;; http://emacswiki.org/emacs/ImenuMode
;; jump to definitions in local buffer
					;(imenu-add-to-menubar imenu)
;; Jump to a definition in the current file. (This is awesome.)
					;(global-set-key (kbd "C-x C-i") 'ido-imenu)


;;disabled; ;; ######################################################
;;disabled; ;; paredit
;;disabled; ;; (el-file copied directly from J. Wiegleys dot-emacs repos)
;;disabled; (my-load-local-el "contrib/paredit.el")
;;disabled; (autoload 'enable-paredit-mode "paredit"
;;disabled;   "Turn on pseudo-structural editing of Lisp code."
;;disabled;   t)
;;disabled; ;;   (add-hook M-mode-hook 'enable-paredit-mode)
;;disabled; (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;;disabled; (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
;;disabled; (eval-after-load 'paredit
;;disabled;   '(progn
;;disabled;      (define-key paredit-mode-map (kbd ")") 'paredit-close-round-and-newline)
;;disabled;      (define-key paredit-mode-map (kbd "M-)") 'paredit-close-round)
;;disabled;      ))


;;disabled; ;; ######################################################
;;disabled; ;; 1x C-a -> first character of line; 2x C-a -> leftmost position
;;disabled; ;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;;disabled; (defun smarter-move-beginning-of-line (arg)
;;disabled;   "Move point back to indentation of beginning of line.
;;disabled;
;;disabled; Move point to the first non-whitespace character on this line.
;;disabled; If point is already there, move to the beginning of the line.
;;disabled; Effectively toggle between the first non-whitespace character and
;;disabled; the beginning of the line.
;;disabled;
;;disabled; If ARG is not nil or 1, move forward ARG - 1 lines first.  If
;;disabled; point reaches the beginning or end of the buffer, stop there."
;;disabled;   (interactive "^p")
;;disabled;   (setq arg (or arg 1))
;;disabled;
;;disabled;   ;; Move lines first
;;disabled;   (when (/= arg 1)
;;disabled;     (let ((line-move-visual nil))
;;disabled;       (forward-line (1- arg))))
;;disabled;
;;disabled;   (let ((orig-point (point)))
;;disabled;     (back-to-indentation)
;;disabled;     (when (= orig-point (point))
;;disabled;       (move-beginning-of-line 1))))
;;disabled;
;;disabled; ;; remap C-a to `smarter-move-beginning-of-line'
;;disabled; (global-set-key [remap move-beginning-of-line]
;;disabled;                 'smarter-move-beginning-of-line)

;; ######################################################
;; ert - for using unit tests of yasnippet (see id:2013-02-07yasnippetdebuggen and yasnippet-tests.el)
					; (my-load-local-el "contrib/cl-lib.el")
					; (my-load-local-el "contrib/ert.el")
					; (my-load-local-el "contrib/ert-x.el")


;; ######################################################
;; ediff from command line
;; http://www.emacswiki.org/emacs/EdiffMode
;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
					;    (ediff file1 file2)))
    (ediff-merge-files file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))


(when (or (my-system-is-powerplant) (my-system-is-powerplantwin))

  ;; ######################################################
  ;; editing Confluence wiki pages (up to Confluence 3.x)
  ;; https://code.google.com/p/confluence-el/
  ;; M-x confluence-get-page
  ;(add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/confluence-el-1.5/"))
  (require 'confluence)
  (setq confluence-url "http://product.infonova.at/confluence/rpc/xmlrpc")
  (add-to-list 'auto-mode-alist '("\\.\\(confluence\\)$" . confluence-mode))

  (dolist (hook '(confluence-mode-hook))
    (add-hook hook (lambda ()
		     (flyspell-mode 1)
		     (ispell-change-dictionary "british")
		     (flyspell-buffer)
		     ))
    )

  (defun vk-open-as-confluence-page ()
    "Takes current line (delimited by brackets, two spaces or pipe) and opens it as Confluence page in IR6 space"
    (interactive)
    (save-excursion
      (re-search-backward "\\(\\] \\|  \\|\* \\|| \\)")  ;; search for "] " or "  " or "| "
      (forward-char)
      (forward-char)
      (setq start-pos (point))
      (re-search-forward "\\( \\[\\|  \\| |\\)")  ;; search for " [" or "  " or " |"
      (backward-char)
      (backward-char)
      (setq end-pos (point))
      (setq myname (buffer-substring start-pos end-pos))
					;(message "Confluence page name: [%s]" myname)
      (confluence-get-page myname "IR6")
					;(confluence-get-page myname)
      )
    )


  ;; ######################################################
  ;; editing Outlook emails in Emacs
  ;; http://www.emacswiki.org/emacs/MsOutlook
  (my-load-local-el "contrib/outlookedit.el")
  (require 'outlookedit)

  (defvar mno-get-outlook-body
    "cscript //B //Job:getMessage c:/Users/karl.voit/bin/outlook_emacs.wsf")
  (defvar mno-put-outlook-body
    "cscript //B //Job:putMessage c:/Users/karl.voit/bin/outlook_emacs.wsf")

  ;; ######################################################
  ;; use mail-mode for email or usenet postings:
  (add-to-list 'auto-mode-alist '("\\.\\(mail\\|email\\|posting\\)$" . mail-mode))
  (dolist (hook '(mail-mode-hook))
    (add-hook hook (lambda ()
		     (mail-mode-auto-fill)
		     (auto-fill-mode 1)
		     (flyspell-mode 1)
		     (ispell-change-dictionary "german8")
		     (flyspell-buffer)
		     ))
    )

  ;; http://www.emacswiki.org/emacs/MailMode
  (add-hook 'mail-mode-hook
	    (lambda ()
	      (font-lock-add-keywords nil
				      '(("^[ \t]*>[ \t]*>[ \t]*>.*$"
					 (0 'mail-multiply-quoted-text-face))
					("^[ \t]*>[ \t]*>.*$"
					 (0 'mail-double-quoted-text-face))))))

  )

;; ######################################################
;; http://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs
(defun xml-pretty-print-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))


;; ######################################################
;; fix paste-issue (\344 instead of ä) on Windows 7:
(when (my-system-is-powerplantwin)
  (set-selection-coding-system 'iso-latin-1-dos)
  )


;;disabled; ;; ######################################################
;;disabled; ;; initializing MELPA
;;disabled; (when (my-system-is-powerplantwin)
;;disabled;   (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;disabled;			   ("melpa" . "http://melpa.milkbox.net/packages/")))
;;disabled;   (package-initialize)
;;disabled;   )


;; ######################################################
;; Emacs 24 theme
;; https://github.com/mswift42/soft-stone-theme
					;disabled; (when (my-system-is-powerplantwin)
					;disabled;   (load-theme 'soft-stone)
					;disabled; )

;; ######################################################
;; Windows Integration
(when (my-system-is-powerplantwin)

  ;; http://gregorygrubbs.com/emacs/10-tips-emacs-windows/
  ;; id:2014-01-31-cygwin-emacs
  (if (file-directory-p "c:/cygwin64/bin")
  (add-to-list 'exec-path "c:/cygwin64/bin"))

  ;; http://www.emacswiki.org/emacs/RobertAdesamConfig
  (setenv "PATH" 
	  (concat 
	   "c:\\cygwin64\\usr\\local\\bin" ";"
	   "c:\\cygwin64\\bin" ";"
	   (getenv "PATH")))
  (setq exec-path (cons "c:/cygwin64/bin/" exec-path))
  ;; Adding cygwin mounts
  ;(require 'cygwin-mount)
  ;(cygwin-mount-activate)
  ;; Adding cygwin bash shell
  (setq shell-file-name "c:/cygwin64/bin/bash")
  (setenv "SHELL" shell-file-name)
  (setq explicit-shell-file-name shell-file-name)
  (setq ediff-shell shell-file-name)
  (setq explicit-shell-args '("--login" "-i"))
  (setq w32-quote-process-args ?\") ;"

  )











;; ######################################################
;; load my functions and key-map definitions (is always last!)
;;disabled; (my-load-local-el "my-definitions.el")

;; ######################################################
;; about defining keys
;; http://ergoemacs.org/emacs/keyboard_shortcuts.html


;; ######################################################
;; 2011-04-20, 2013-04-08: defining «C-c C-,» as my own prefix:
;; http://stackoverflow.com/questions/1024374/how-can-i-make-c-p-an-emacs-prefix-key-for-develperlysense
;; http://stackoverflow.com/questions/5682631/what-are-good-custom-keybindings-in-emacs
;; NOTE: (info "(elisp) Key Binding Conventions") warns about user prefixes other than C-c
					;(global-unset-key "\C-c\C-,")
(define-prefix-command 'my-map)
(global-set-key (kbd "C-c C-,") 'my-map)

;; NOTE:
;; tried to use «C-,» but I could not make it work:
;; C-, used by flyspell: (define-key map [(control ?\,)] 'flyspell-goto-next-error)
;; (global-unset-key [(control ?\,)])
;; (global-unset-key "\C-,")
;;does not work;; (global-unset-key "\C-,")
;;does not work;; (defvar my-map (make-keymap)
;;does not work;;      "Keymap for local bindings and functions, prefixed by (^,)")
;;does not work;; (define-key global-map "\C-," 'my-keymap-prefix)
;;does not work;; (fset 'my-keymap-prefix my-map)


;; ######################################################
;; 2013-03-31: http://stackoverflow.com/questions/3124844/what-are-your-favorite-global-key-bindings-in-emacs
;; font sizes:
(define-key my-map "-" 'text-scale-decrease)
(define-key my-map "+" 'text-scale-increase)
(define-key my-map "=" 'text-scale-increase);; because "+" needs "S-=" and I might forget shift
;; Magit status
(define-key my-map "v" 'magit-status)
(define-key my-map "g" 'magit-status)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end]  'end-of-buffer)

(define-key my-map " " 'delete-trailing-whitespace)

;; ######################################################
;; coping with ELISP code
(define-key my-map "er" 'eval-region)
(define-key my-map "el" 'find-library)
(define-key my-map "ef" 'find-function-at-point)



;; ######################################################
;; Toggle between split windows and a single window
;; http://thornydev.blogspot.co.at/2012/08/happiness-is-emacs-trifecta.html
(defun my-toggle-windows-split()
  "Switch back and forth between one window and whatever split of windows we might have in the frame. The idea is to maximize the current buffer, while being able to go back to the previous split of windows in the frame simply by calling this command again."
  (interactive)
  (if (not (window-minibuffer-p (selected-window)))
      (progn
	(if (< 1 (count-windows))
	    (progn
	      (window-configuration-to-register ?u)
	      (delete-other-windows))
	  (jump-to-register ?u))))
					;(my-iswitchb-close)
  )

(define-key my-map "s" 'my-toggle-windows-split)


;; ######################################################
(global-set-key [C-left] 'backward-word)
(global-set-key [C-right] 'forward-word)
(global-set-key [S-left] 'backward-word)
(global-set-key [S-right] 'forward-word)




;; ######################################################
					;(setq ispell-dictionary "german-new8")
					;(setq ispell-dictionary "german-new8")
					;(setq ispell-local-dictionary "german-new8")
(define-key my-map "fm" 'flyspell-mode)
(define-key my-map "fr" 'flyspell-region)
(define-key my-map "fl" 'my-toggle-ispell-language)
(define-key my-map "ft" 'my-toggle-ispell-language);; can't remember if l(anguage) or t(oggle)
(define-key my-map "fn" 'flyspell-goto-next-error)
(define-key my-map "ff" 'flyspell-correct-word-before-point)





;; ######################################################
;; boxquote
;(my-load-local-el "contrib/boxquote.el") 2014-01-19: removed old el and replaced it with package from elpa
(define-key my-map "q" 'boxquote-region)
(define-key my-map "Q" 'boxquote-title)



;; ######################################################
;; Solarized Color Theme
(define-key my-map "c" 'my-toggle-color-theme)



;; ######################################################
;; switching lines
;; http://whattheemacsd.com//editing-defuns.el-02.html
(defun my-move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun my-move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(define-key my-map (kbd "<up>") 'my-move-line-up)
(define-key my-map (kbd "<down>") 'my-move-line-down)


;; ######################################################
;; joining lines: C-c , j
;; http://whattheemacsd.com//key-bindings.el-03.html
(define-key my-map "j" ;; join-line
  (lambda ()
    (interactive)
    (join-line -1)))


;; ######################################################
;; web-jump: C-c w
;; http://www.emacswiki.org/emacs/WebJump
;; http://www.neilvandyke.org/webjump/
;; https://github.com/thefury/site-lisp/blob/master/dotemacs.el
;; http://whattheemacsd.com//my-misc.el-01.html (Urban Dictionary)
(require 'webjump)
(define-key my-map "w" 'webjump)
(setq webjump-sites ;(append   ;; append instead of overwrite
      '(
	;; ------------------------------------------------------------------
	("Emacs Lisp List" . "anc.ed.ac.uk/~stephen/emacs/ell.html")
	("Ohio State Emacs Lisp Archive" .
	 [simple-query "www.cis.ohio-state.edu/emacs-lisp/"
		       "neutral.verbum.org/search?q=" "&archive=archive"])
	("PGP Key Server" .
	 [simple-query "pgp.mit.edu"
		       "pgp.mit.edu:11371/pks/lookup?op=index&search=" ""])
	;; my own jumps
	("Org-mode docu" . "http://orgmode.org/org.html")
	("Org-mode mailinglist" .
	 [simple-query "orgmode.org"
		       "http://search.gmane.org/?group=gmane.emacs.orgmode&query=" ""])
	("Debian Bug Number" .
	 [simple-query "www.debian.org/Bugs/"
		       "bugs.debian.org/cgi-bin/bugreport.cgi?bug="
		       ""])
	("EmacsWiki" .
	 [simple-query "www.emacswiki.org/cgi-bin/wiki.pl"
		       "www.emacswiki.org/cgi-bin/wiki.pl?search="
		       "&dosearch=1"])
	("Google" .
	 [simple-query "www.google.at" "www.google.at/search?q=" ""])
	("IMDB" .
	 [simple-query "www.imdb.com" "www.imdb.com/Find?select=All&for=" ""])
	;; ------------------------------------------------------------------
	) webjump-sample-sites ;)  ;; append instead of overwrite
	  )


;; ######################################################
;; 2011-04-20: C-j t ... timestamp
;;       http://www.sabren.net/articles/emacs.php3
;;       http://www.gnu.org/software/emacs/manual/html_node/emacs/Date-Display-Format.html
;; COMBINED WITH:
;; 2012-12-09:
;; From: Russell Adams <RLAdams@AdamsInfoServ.Com>
;;       Newsgroups: gmane.emacs.orgmode
;;       Subject: Re: Using org-mode for laboratory notes.
;;       Date: Wed, 19 Sep 2012 12:08:21 -0500
;;       Message-ID: <20120919170820.GC31853@cardamom.adamsinfoserv.com>
;; Insert immediate timestamp
(defun my-insert-timestamp()
  "Insert the current time in yyyy-mm-dd format."
  (interactive "*")
  (if (eq major-mode 'org-mode)
      (progn
	(org-insert-time-stamp nil t nil)
	(insert " ")
	)
    (insert (format-time-string "%Y-%m-%d" (current-time)))
    )
  )
(define-key my-map "t" 'my-insert-timestamp)

(defun my-insert-timestamp-inactive()
  "Insert the current time in yyyy-mm-dd format."
  (interactive "*")
  (if (eq major-mode 'org-mode)
      (progn
	(org-insert-time-stamp nil t t)
	(insert " ")
	)
    (insert (format-time-string "%Y-%m-%d" (current-time)))
    )
  )
(define-key my-map "T" 'my-insert-timestamp-inactive)

;; 2012-12-23: C-j d ... datestamp
(defun my-insert-datestamp()
  "Insert the current date in yyyy-mm-dd format."
  (interactive "*")
  (if (eq major-mode 'org-mode)
      (progn
	(org-insert-time-stamp nil nil nil)
	(insert " ")
	)
    (insert (format-time-string "%Y-%m-%d" (current-time)))
    )
  )
(define-key my-map "d" 'my-insert-datestamp)

(defun my-insert-datestamp-inactive()
  "Insert the current date in yyyy-mm-dd format."
  (interactive "*")
  (if (eq major-mode 'org-mode)
      (progn
	(org-insert-time-stamp nil nil t)
	(insert " ")
	)
    (insert (format-time-string "%Y-%m-%d" (current-time)))
    )
  )
(define-key my-map "D" 'my-insert-datestamp-inactive)


;; ######################################################
;; recently files
(define-key my-map "r" 'recentf-open-files)


;; ######################################################
;; command log mode
;; https://github.com/lewang/command-log-mode
(my-load-local-el "contrib/command-log-mode/command-log-mode.el")
(require 'command-log-mode)
(add-hook 'LaTeX-mode-hook 'command-log-mode)

;; (defun my-start-command-log-mode()
;;   "load the command-log mode and start it."
;;   (interactive "*")
;;   ;; M-x clm/open-command-log-buffer
;;   (clm/open-command-log-buffer)
;; )

(define-key my-map "k" 'clm/open-command-log-buffer)


(when (or (my-system-is-powerplant) (my-system-is-powerplantwin))
  (define-key my-map "C" 'vk-open-as-confluence-page)
  (define-key my-map "oe" 'mno-edit-outlook-message)
  (define-key my-map "os" 'mno-save-outlook-message)
  )

(define-key my-map (kbd "<left>") 'org-mark-ring-goto)

(define-key my-map (kbd ".") (lambda()  ;; open main.el
			       (interactive)
			       (find-file "~/.emacs.d/main.el")
			       )
  )

;;OLD;; ######################################################
;;OLD;; fix bad paste for Windows 7 -> underlying issue was fixed via (set-selection-coding-system 'iso-latin-1-dos) -> not needed any more
;;OLD;(when (my-system-is-powerplantwin)
;;OLD;  (define-key my-map "p" (lambda ()
;;OLD;                  (interactive)
;;OLD;                  (recode-region (region-beginning) (region-end) 'latin-1 'utf-8)
;;OLD;		  )
;;OLD;    )
;;OLD;  )



;; END OF FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-contacts-address-property "CITY")
 '(org-contacts-birthday-property "BORN")
 '(org-contacts-files "~/share/all/org-mode/contacts.org")
 '(org-contacts-icon-property "PHOTOGRAPH")
 '(safe-local-variable-values
   (quote
    ((eval ispell-change-dictionary "german8")
     (eval ispell-change-dictionary "american")
     (eval ispell-change-dictionary "en_US")
     (flyspell-default-dictionary . "german8")))))

;; Local Variables:
;; eval: (flyspell-mode 1)
;; eval: (ispell-change-dictionary "american")
;; End:
