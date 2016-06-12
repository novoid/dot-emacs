; -*- orgstruct-heading-prefix-regexp: ";;" -*-

;; ######################################################
(message "######### loading main.el ...")

;; #############################################################################
;;* General settings

;; personalize prelude
;; from: http://batsov.com/prelude/
;(load-theme 'solarized-light t)

;(defun disable-guru-mode ()
;  (guru-mode -1)
;  )
;(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)

(server-start)

(setq backup-inhibited t);; 2011-04-20: turn off backup files
(setq calendar-week-start-day 1);; set start of week to Monday (not sunday) http://sunsite.univie.ac.at/textbooks/emacs/emacs_33.html
(setq-default indent-tabs-mode t);; damit C-x r o keine TABs benutzt:
(add-hook 'write-file-hooks 'time-stamp)
(setq large-file-warning-threshold 100000000);; set warning of opening large files to 100MB
(setq sentence-end-double-space nil);; do not add double space after periods http://www.reddit.com/r/emacs/comments/2l5gtz/real_sentence_in_emacs/
(setq column-number-mode t);; show current column
(setq truncate-lines t);; https://www.emacswiki.org/emacs/TruncateLines - M-x toggle-truncate-lines
(setq-default global-visual-line-mode t) ;; http://stackoverflow.com/questions/7577614/emacs-truncate-lines-in-all-buffers

;; Prevent the cursor from blinking
;(blink-cursor-mode 0)
(set-cursor-color "IndianRed")

;; 2014-05-24: flat mode-line styling from http://www.reddit.com/r/emacs/comments/23l9oi/flat_modeline/
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(setq package-user-dir "~/.emacs.d/elpa")
(package-initialize)
;; http://www.reddit.com/r/emacs/comments/2u1bml/gnu_or_melpa_version_of_yasnippet_both_in_mx/
;; MELPA packages are usually built automatically from a project's repository; the Gnu repository has stable releases that are explicitly submitted to it.
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; 2016-01-26: fix certificate issue: "gnutls.c: [0] (Emacs) fatal error: The TLS connection was non-properly terminated."
;; https://github.com/nicferrier/elmarmalade/issues/55#issuecomment-166271364
(if (fboundp 'gnutls-available-p)
    (fmakunbound 'gnutls-available-p))
(setq tls-program '("gnutls-cli --tofu -p %p %h")
      imap-ssl-program '("gnutls-cli --tofu -p %p %s")
      smtpmail-stream-type 'starttls
      starttls-extra-arguments '("--tofu")
      )

;; see id:2014-03-04-M-l-subword
(global-set-key [M-l] 'downcase-word)
(global-set-key [M-u] 'upcase-word)
(global-set-key [M-c] 'capitalize-word)


;; ######################################################
;; 2015-11-25: https://github.com/jwiegley/use-package
(require 'use-package)

;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ######################################################
;; http://twitter.com/emacs_knight/status/128339316417101825
;; prefer y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; ######################################################
;; only one window on startup
;; http://thornydev.blogspot.co.at/2012/08/happiness-is-emacs-trifecta.html
(add-hook 'emacs-startup-hook 'delete-other-windows t)

;; ######################################################
;; http://wason21cn.blogspot.com/feeds/posts/default?orderby=updated
;; Highlight matching parenthesis
(show-paren-mode t)

;; ######################################################
;; deletes duplicate entries of history
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-History.html
(setq history-delete-duplicates t)




;; ######################################################
;; un-setting keys
;;   \C-v   scroll up
;;   \C-t   transpose-chars
(dolist (key '("\C-v" "\C-t"))
    (global-unset-key key))



;; #############################################################################
;;* Themes

(when (>= emacs-major-version 24)
  ;; ######################################################
  ;; https://github.com/hadronzoo/theme-changer
  ;; set color theme according to day-time:
  ;;disabled;; (setq calendar-location-name "Graz, AT")
  ;;disabled;; (setq calendar-latitude 47.07)
  ;;disabled;; (setq calendar-longitude 15.43)
  ;;disabled;; (use-package theme-changer)
  ;;disabled;; (change-theme 'whiteboard 'misterioso)  ;; day and night theme

  ;; my favorite dark themes: misterioso, zenburn, material
  (load-theme 'wombat t) ;; dark theme
  ;;   (load-theme 'misterioso t)
  ;;   (load-theme 'zenburn t)
  ;;   (load-theme 'material t) ;; from http://www.reddit.com/r/emacs/comments/39dk64/escaping_from_org_mode/
  ;;              issues with *bold* stuff in org-mode :-(
  ;; my favorite light themes: leuven, whiteboard, solarized-light,
  ;;   (load-theme 'leuven t) ;; from http://www.reddit.com/r/emacs/comments/39dk64/escaping_from_org_mode/
  ;;   (load-theme 'whiteboard t)
  ;;   (load-theme 'solarized-light t)

  ;; enhanced highlighting of babel blocks: http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
  ;; issues when trying to apply face instantly: https://www.reddit.com/r/emacs/comments/3ksen6/noob_question_how_to_make_changes_after/cv0cmko
  ;; M-x describe-face  -> show definition
  ;; C-u C-x =          -> show all font information
;  (face-spec-set 'org-block-begin-line
;    '((t (:underline "#FFFFFF" :foreground "#404040" :background "#b3b3b3")))
;    "Face used for the line delimiting the begin of source blocks.")

  ;(defface org-block-begin-line
  ;  '((t (:underline "#FFFFFF" :foreground "#cccccc" :background "#4d4d4d")))
  ;  "Face used for the line delimiting the begin of source blocks.")

  (defface org-block
    ;; defface org-block-background was removed from org:
    ;; http://emacs.stackexchange.com/questions/14824/org-block-background-font-not-having-effect
    ;; read also: https://www.reddit.com/r/emacs/comments/415imd/prettier_orgmode_source_code_blocks/
    '((t (:background "#1a1a1a")))
    "Face used for the source block background.")

  ;(defface org-block-end-line
  ;  '((t (:overline "#FFFFFF" :foreground "#cccccc" :background "#4d4d4d")))
  ;  "Face used for the line delimiting the end of source blocks.")

  ;;test: (set-face-background 'org-block-background "#1a1a1a")

  )


;; #############################################################################
;;* my-PATHS


;; von: http://www.zonix.de/html40/linux/emacsgnus.html
;; hier liegen alle meine Emacs- und Gnus-Einstellungen
(defvar my-elisp-dir "~/.emacs.d/")

(defun my-load-local-el (part)
  "load lisp file and warn if not found"
  (let ((fullname (concat my-elisp-dir part)))
    (if (file-exists-p fullname)
	(load fullname)
      (message (format "Loading %s (source)...failed" fullname)))))


;; #############################################################################
;;* my-system-is-FOOBAR

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
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin")
  )

;; Check if system is Microsoft Windows
(defun my-system-type-is-windows ()
  "Return true if system is Windows-based (at least up to Win7)"
  (string-equal system-type "windows-nt")
  )

;; Check if system is GNU/Linux
(defun my-system-type-is-gnu ()
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux")
  )

(defun my-system-is-gary-or-sherri ()
  "Return true if the system we are running on is gary or sherri"
  (or
    (string-equal system-name "gary")
    (string-equal system-name "gary.lan")
    (string-equal system-name "sherri")
    (string-equal system-name "sherri.lan")
    )
  )
(defun my-system-is-sherri ()
  "Return true if the system we are running on is gary or sherri"
  (or
    (string-equal system-name "sherri")
    (string-equal system-name "sherri.lan")
    )
  )
(defun my-system-is-gary ()
  "Return true if the system we are running on is gary or sherri"
  (or
    (string-equal system-name "gary")
    (string-equal system-name "gary.lan")
    )
  )
(defun my-system-is-blanche ()
  "Return true if the system we are running on is blanche"
  (or (string-equal system-name "blanche") (string-equal system-name "blanche.lan"))
  )
(defun my-system-is-grmlvrs ()
  "Return true if the system we are running on is grmlvrs"
  (string-equal system-name "grmlvrs")
  )
(defun my-system-is-powerplantlinux ()
  "Return true if the system we are running on is powerplant"
  (or
   (string-equal system-name "powerplant")
   (string-equal system-name "powerplant.lan")
   )
  )
(defun my-system-is-powerplantwin ()
  "Return true if the system we are running on is powerplant"
  (string-equal system-name "ATGRZ4043268X")
  )
(defun my-system-is-kva ()
  "Return true if the system we are running on is karl-voit.at"
  (string-equal system-name "friends.grml.info")
  )


;; #############################################################################
;;* UTF-8 and codings

;; Activate UTF-8 mode:
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(cond ((my-system-type-is-windows)
       ;; on Windows, 'utf-8 does not work properly when system
       ;; clipboard gets yanked
       (setq selection-coding-system 'utf-16le-dos)
       )
      ((my-system-type-is-gnu)
       (set-selection-coding-system 'utf-8)
       )
      (t
       (set-selection-coding-system 'utf-8)
       )
      )


;; 2013-12-10 IRC #Emacs
(set-clipboard-coding-system 'utf-8)

;; http://www.masteringemacs.org/articles/2012/08/09/working-coding-systems-unicode-emacs/
;; in addition to the lines above:

(set-default-coding-systems 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    ;; NOTE: default-buffer-file-coding-system is obsolete; use
    ;;       buffer-file-coding-system if found
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

;; English time-stamps in Org-mode (instead of localized German ones):
;; http://lists.gnu.org/archive/html/emacs-orgmode/2011-10/msg01046.html
;;   system locale to use for formatting time values (e.g., timestamps in
;;   Org mode files)
;; "en_US.utf8" did not work for the weekday in the agenda!
(setq system-time-locale "C")


;; #############################################################################
;;* my-map


;; about defining keys
;; http://ergoemacs.org/emacs/keyboard_shortcuts.html


;; 2015-11-10 replaced by bind-key ;; ;; 2011-04-20, 2013-04-08: defining «C-c C-,» as my own prefix:
;; 2015-11-10 replaced by bind-key ;; ;; http://stackoverflow.com/questions/1024374/how-can-i-make-c-p-an-emacs-prefix-key-for-develperlysense
;; 2015-11-10 replaced by bind-key ;; ;; http://stackoverflow.com/questions/5682631/what-are-good-custom-keybindings-in-emacs
;; 2015-11-10 replaced by bind-key ;; ;; NOTE: (info "(elisp) Key Binding Conventions") warns about user prefixes other than C-c
;; 2015-11-10 replaced by bind-key ;; (global-unset-key (kbd "C-c C-,")); causes error: "Invalid modifier in string"
;; 2015-11-10 replaced by bind-key ;; ;; same as: (global-unset-key (kbd "C-c C-,"))
;; 2015-11-10 replaced by bind-key ;; (define-prefix-command 'my-map)
;; 2015-11-10 replaced by bind-key ;; (global-set-key (kbd "C-c C-,") 'my-map)
;; 2015-11-10 replaced by bind-key ;; (global-set-key (kbd "C-c ,") 'my-map)


;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
(require 'bind-key);; https://github.com/emacsattic/bind-key

(bind-keys
 :prefix-map my-map
 :prefix-docstring "My own keyboard map"
 :prefix "C-c C-,"
 ;; 2013-03-31: http://stackoverflow.com/questions/3124844/what-are-your-favorite-global-key-bindings-in-emacs
 ("-" . text-scale-decrease)
 ("+" . text-scale-increase)
 ("=" . text-scale-increase);; because "+" needs "S-=" and I might forget shift
 )

;; examples:
;;   (bind-key "m w" #'mark-word my-map)
;; or:
;;   (bind-keys
;;    :map my-map
;;    ("f" . forward-char)
;;    ("b" . backward-char))


;; #############################################################################
;;* system-specific paths and keys


;; http://www.emacswiki.org/emacs/MacOSTweaks#toc13
;; setting path so that Emacs finds aspell and such
(when (my-system-type-is-darwin)
  (setenv "PATH"
	  (concat (getenv "PATH")
		  ":/Users/vk/bin:/usr/local/texlive/2010/bin/x86_64-darwin:/opt/local/bin:/opt/local/sbin"))
  (setq exec-path (append exec-path
			  '("/opt/local/bin"
			    "/usr/local/texlive/2010/bin/x86_64-darwin"
			    "/usr/local/teTeX/bin/powerpc-apple-darwin-current"
			    )))
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp")

  ;; 2011-04-20: allow typing of german umlauts in OS X by Alt-u followed by u,o,a,...
  (setq mac-option-modifier nil)

  (setq org-ditaa-jar-path "~/data/hosts/blanche/config/ditaa.jar")

  ;; setting path to color-theme-mode.el from MacPorts
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp/color-theme-6.6.0")
  )


;; ######################################################
;; fix paste-issue (\344 instead of ä) on Windows 7:
(when (my-system-type-is-windows)
  (set-selection-coding-system 'iso-latin-1-dos)
  )

(when (my-system-type-is-gnu)
  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
  )


;; setting path so that Emacs finds aspell and such
(if (my-system-type-is-windows)

     ;;disabled;(setenv "PATH"
     ;;disabled;               (concat (getenv "PATH")
     ;;disabled;		  ":/Users/vk/bin:/usr/local/texlive/2010/bin/x86_64-darwin:/opt/local/bin:/opt/local/sbin"))
     (setq exec-path (append exec-path
      			     '("C:/Program Files (x86)/Aspell/bin"
      			       ;;disabled; "/usr/local/texlive/2010/bin/x86_64-darwin"
      			       ;;disabled; "/usr/local/teTeX/bin/powerpc-apple-darwin-current"
      			       )))
     ;;disabled;(add-to-list 'load-path "/opt/local/share/emacs/site-lisp")

  (
   ;; on all other systems:
   )
  )



;; ######################################################
;;** Cygwin (Windows)
(when (my-system-is-powerplantwin)

  ;; http://gregorygrubbs.com/emacs/10-tips-emacs-windows/
  ;; id:2014-01-31-cygwin-emacs
  (if (file-directory-p "c:/cygwin64/bin")
  (add-to-list 'exec-path "c:/cygwin64/bin"))

  ;; http://www.emacswiki.org/emacs/RobertAdesamConfig
  (setenv "PATH"
	  (concat
	   ;;"c:\\cygwin64\\usr\\local\\bin" ";"  ;; Cygwin
	   ;;"c:\\cygwin64\\bin" ";"  ;; Cygwin
	   "C:\\Users\\karl.voit\\.babun\\cygwin\\bin" ";"
	   "C:\\Users\\karl.voit\\.babun\\cygwin\\usr\\local\\bin" ";"
	   (getenv "PATH")))
  ;;(setq exec-path (cons "c:/cygwin64/bin/" exec-path)) ;; Cygwin
  (setq exec-path (cons "C:/Users/karl.voit/.babun/cygwin/bin/" exec-path)) ;; Babun
  ;; Adding cygwin mounts
  ;(use-package cygwin-mount)
  ;(cygwin-mount-activate)
  ;; Adding cygwin bash shell
  ;;(setq shell-file-name "c:/cygwin64/bin/bash") ;; Cygwin
  (setq shell-file-name "C:/Users/karl.voit/.babun/cygwin/bin/zsh") ;; Babun
  (setenv "SHELL" shell-file-name)
  (setq explicit-shell-file-name shell-file-name)
  (setq ediff-shell shell-file-name)
  (setq explicit-shell-args '("--login" "-i"))
  (setq w32-quote-process-args ?\") ;"

  ;; id:2015-11-02-tramp-windows-babel
  (setq tramp-default-method "plink")

  )



;; #############################################################################
;;* font size

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

(when (or (my-system-type-is-gnu) (my-system-is-powerplantwin))
  (my-increase-fontsize);; increase fonts on some hosts by default
  )
(when (my-system-type-is-darwin)
  (set-face-attribute 'default (selected-frame) :height 170);; 2011-04-20: increase/set font size http://www.emacswiki.org/emacs/SetFonts
  )
(when (my-system-is-powerplantwin)
  (set-face-attribute 'default (selected-frame) :height 150)
  )

;;* measure-time()

;; http://stackoverflow.com/questions/23622296/emacs-timing-execution-of-function-calls-in-emacs-lisp
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message " Execution time: %.06f" (float-time (time-since time)))))



;; #############################################################################
;;* Elisp


;;disabled;; ;; ######################################################
;;disabled;; ;; separate color for highlightning () brackets:
;;disabled;; ;; http://compgroups.net/comp.emacs/to-use-special-color-for-brackets-in-emacs-lisp-mo/222015
;;disabled;; (defface paren-face
;;disabled;;   '((((class color) (background dark))
;;disabled;;      (:foreground "grey30"))
;;disabled;;     (((class color) (background light))
;;disabled;;      (:foreground "grey60")))
;;disabled;;   "Face used to dim parentheses.")
;;disabled;; (defun egoge-dim-parens ()
;;disabled;;   (font-lock-add-keywords nil
;;disabled;; 			  '(("(\\|)" . 'paren-face))))
;;disabled;; (add-hook 'emacs-lisp-mode-hook 'egoge-dim-parens)

(add-hook 'emacs-lisp-mode-hook 'turn-off-auto-fill)

;; #############################################################################
;;* Python

(use-package elpy
  ;; :disabled t ;; stop loading if 't'
  :ensure t
  :if (or (my-system-type-is-gnu) (my-system-is-powerplantwin))
  :mode ("\\.py\\'" . elpy-mode)
  :config ;; executed after loading package

  ;; ######################################################
  ;; elpy: https://github.com/jorgenschaefer/elpy/wiki/
  (when (my-system-type-is-gnu)
    (elpy-enable)
    (elpy-use-ipython)
    )

  ;(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  ;(add-to-list 'auto-mode-alist '("\\.py$" . company-mode))

  ;; ######################################################
  ;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/
  ;; Ropemacs:
  ;;(add-to-list 'load-path "~/.emacs.d/vendor/pymacs-0.24-beta2")
  ;;disabled; (use-package pymacs)
  ;;disabled; (pymacs-load "ropemacs" "rope-")
  ;;disabled; (setq ropemacs-enable-autoimport t)

  ;; ######################################################
  ;; auto-complete mode
  ;;(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete-1.2")
  ;;disabled; (use-package auto-complete-config)
  ;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete-1.2/dict")
  ;;disabled; (ac-config-default)

  ;; ######################################################
  ;; PyFlakes:
  (setq python-check-command "pyflakes")

  ;; ######################################################
  ;; fix flymake (PEP8): ignore E501 (long lines)
  ;; see id:2015-04-04-flymake and http://stackoverflow.com/a/1393590
  ;; looks similar: http://people.cs.uct.ac.za/~ksmith/2011/better-python-flymake-integration-in-emacs.html
  (when (load "flymake" t)
    (defun flymake-pyflakes-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "~/bin/pycheckers"  (list local-file))))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.py\\'" flymake-pyflakes-init)))

  ;; ######################################################
  ;; better flymake: http://stackoverflow.com/a/1621489
  (add-hook 'python-mode-hook
      (lambda ()
        (unless (eq buffer-file-name nil) (flymake-mode 1)) ;dont invoke flymake on temporary buffers for the interpreter
        (local-set-key [f5] 'flymake-goto-prev-error)
        (local-set-key [f6] 'flymake-goto-next-error)
        ))


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

  (define-key my-map "P" 'pylookup-lookup)

)


;; #############################################################################
;;* LaTeX

(when (my-system-type-is-gnu)

  (autoload 'tex-site "tex-site.el")  ;; acticate AucTeX and set general preferences
  (setq TeX-PDF-mode t)  ;; compile to PDF using pdflatex (instead to DVI)

  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill) ;; word-wrap in TeX files


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
  ;;disabled;(when (or (my-system-type-is-gnu) (my-system-is-powerplantlinux))
  ;;disabled;  (my-load-local-el "contrib/cdlatex.el")
  ;;disabled;  )

  ;; ######################################################
  ;; http://staff.science.uva.nl/~dominik/Tools/cdlatex/
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;(setq-default TeX-master nil);; 2015-03-22 deactivated because it doesn't seem to have any influence: id:2013-12-31-org-master-file
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


  ;; ######################################################
  ;; http://www.gnu.org/software/auctex/manual/reftex.html#SEC48
  ;; Specify my bibtex folder
  ;;(setq reftex-bibpath-environment-variables
  ;;      '("~/archive/library/"))
  ;; does not work :-(
  ;; tested with tagstore.org

  ;; disabled 2015-03-22 because it did not help ;; ;; 2015-03-22 - see also id:2013-12-31-org-master-file
  ;; disabled 2015-03-22 because it did not help ;; ;; http://www.emacswiki.org/emacs/AUCTeX
  ;; disabled 2015-03-22 because it did not help ;; (defun guess-TeX-master (filename)
  ;; disabled 2015-03-22 because it did not help ;;     "Guess the master file for FILENAME from currently open files according to their extension."
  ;; disabled 2015-03-22 because it did not help ;;     (let ((candidate nil)
  ;; disabled 2015-03-22 because it did not help ;;           (filename (file-name-nondirectory filename)))
  ;; disabled 2015-03-22 because it did not help ;;       (save-excursion
  ;; disabled 2015-03-22 because it did not help ;;         (dolist (buffer (buffer-list))
  ;; disabled 2015-03-22 because it did not help ;;           (with-current-buffer buffer
  ;; disabled 2015-03-22 because it did not help ;;             (let ((name (buffer-name))
  ;; disabled 2015-03-22 because it did not help ;;                   (file buffer-file-name))
  ;; disabled 2015-03-22 because it did not help ;;               ;(if (and file (string-match "\\.\(org\|tex\)$" file))
  ;; disabled 2015-03-22 because it did not help ;;               (if (and file (string-match "\\.org$" file))
  ;; disabled 2015-03-22 because it did not help ;;                   (progn
  ;; disabled 2015-03-22 because it did not help ;;                     (goto-char (point-min))
  ;; disabled 2015-03-22 because it did not help ;;                     (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
  ;; disabled 2015-03-22 because it did not help ;;                         (setq candidate file))
  ;; disabled 2015-03-22 because it did not help ;;                     (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
  ;; disabled 2015-03-22 because it did not help ;;                         (setq candidate file))))))))
  ;; disabled 2015-03-22 because it did not help ;;       (if candidate
  ;; disabled 2015-03-22 because it did not help ;;           (message "TeX master document: %s" (file-name-nondirectory candidate)))
  ;; disabled 2015-03-22 because it did not help ;;       candidate))
  ;; disabled 2015-03-22 because it did not help ;;
  ;; disabled 2015-03-22 because it did not help ;; ;; ONLY for special file modes with a recognized extension!
  ;; disabled 2015-03-22 because it did not help ;; ;; Causes Lisp error (that's a afact) when used with buffers like *scratch* (that's my guess)
  ;; disabled 2015-03-22 because it did not help ;; ;;(setq TeX-master (guess-TeX-master (buffer-file-name)))

  );; end LaTeX settings





;; #############################################################################
;;* OS X: mdfind (disabled)


;;disabled;; ;; ######################################################
;;disabled;; ;; http://blog.zenspider.com/2007/03/locate-and-spotlight.html
;;disabled;; (when (my-system-type-is-darwin)
;;disabled;;   (defun locate-make-mdfind-command-line (search-string)
;;disabled;;     (list "mdfind" (concat "kMDItemDisplayName=*" search-string "*")))
;;disabled;;   (defun spotlight ()
;;disabled;;     "Search for files by name using spotlight"
;;disabled;;     (interactive)
;;disabled;;     (let ((locate-command "mdfind")
;;disabled;; 	  (locate-make-command-line 'locate-make-mdfind-command-line))
;;disabled;;       (call-interactively 'locate nil)))
;;disabled;;   (defun spotlight-full ()
;;disabled;;     "Search using spotlight"
;;disabled;;     (interactive)
;;disabled;;     (let ((locate-command "mdfind"))
;;disabled;;       (call-interactively 'locate nil)))
;;disabled;;   )



;; #############################################################################
;;* system-specific browse-url-browser

;; ######################################################
;; http://stackoverflow.com/questions/4506249/how-to-make-emacs-org-mode-open-links-to-sites-in-google-chrome
;;old;;(when (my-system-is-powerplantlinux)
;;old;;  (setq browse-url-browser-function 'browse-url-generic
;;old;;	;;      browse-url-generic-program "/usr/bin/google-chrome")
;;old;;	browse-url-generic-program "/usr/bin/firefox")
;;old;;  )
;;old;;(when (my-system-is-sherri)
;;old;;  (setq browse-url-browser-function 'browse-url-generic
;;old;;	;;browse-url-generic-program "~/bin/firefox-browser/firefox")
;;old;;	browse-url-generic-program "/usr/bin/iceweasel")
;;old;;  )
;;old;;(when (my-system-type-is-darwin)
;;old;;  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
;;old;;  )
;;old;;(when (my-system-is-gary)
;;old;;  (setq browse-url-browser-function 'browse-url-generic
;;old;;	;;      browse-url-generic-program "/usr/bin/google-chrome")
;;old;;	browse-url-generic-program "/usr/bin/iceweasel")
;;old;;  )
(cond
 ((my-system-type-is-darwin)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  )
 ((file-exists-p "/usr/bin/firefox")
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "/usr/bin/firefox")
  )
 ((file-exists-p "/usr/bin/iceweasel")
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "/usr/bin/iceweasel")
  )
 ((file-exists-p "/usr/bin/google-chrome")
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "/usr/bin/google-chrome")
  )
 )

;; ######################################################
;; http://stackoverflow.com/questions/4506249/how-to-make-emacs-org-mode-open-links-to-sites-in-google-chrome
;;disabled; (setq browse-url-browser-function 'browse-url-generic
;;disabled;       browse-url-generic-program "chromium-browser")


;; ######################################################
;; https://chrome.google.com/webstore/detail/ljobjlafonikaiipfkggjbhkghgicgoh?hl=de
;; Edit-server for Chrome
;;disabled; ;(use-package edit-server)
;;disabled; (my-load-local-el "contrib/edit-server.el")
;;disabled; (edit-server-start)
;;(if (locate-library "edit-server")
;;    (progn
;;      (use-package edit-server)
;;      (setq edit-server-new-frame nil)
;;      (edit-server-start)))




;; #############################################################################
;;* flyspell

;; ######################################################
;; setting path to flyspell-mode.el from MacPorts
(when (my-system-type-is-darwin)
  (add-to-list 'load-path "/opt/local/share/emacs/lisp/textmodes")
  )

;; save to user dictionary without asking:
;;(setq ispell-silently-savep t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flyspell.el <http://kaolin.unice.fr/~serrano/>

;;(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)
;;;(define-key global-map [(f10)] 'flyspell-mode)
;;;(set-default 'ispell-local-dictionary my-german-ispell-dictionary)

;; old stuff:

;; ######################################################
;; http://www.linuxfaq.de/f/cache/146.html

;;(autoload 'flyspell-mode "flyspell" "On-the-fly ispell." t)
(setq flyspell-issue-welcome-flag nil)

(when (my-system-is-powerplantwin)
  (setq flyspell-default-dictionary "german8")
)
(when (my-system-type-is-gnu)
  (setq flyspell-default-dictionary "de_AT")
)

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
;;old-method until 2014-01-26;; (if (or (my-system-is-powerplantlinux) (my-system-is-powerplantwin))
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
  )
(if (my-system-type-is-gnu)
    ;; use US english on powerplantwin:
    (let ((langs '("de_AT" "en_US")))
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
;;disabled;(add-hook          'c-mode-hook 'flyspell-prog-mode)
;;disabled;(add-hook         'sh-mode-hook 'flyspell-prog-mode)
;;disabled;(add-hook        'c++-mode-hook 'flyspell-prog-mode)
;;disabled;(add-hook       'ruby-mode-hook 'flyspell-prog-mode)
;;disabled;(add-hook      'cperl-mode-hook 'flyspell-prog-mode)
;;disabled;(add-hook     'python-mode-hook 'flyspell-prog-mode)
;;disabled;(add-hook   'autoconf-mode-hook 'flyspell-prog-mode)
;;disabled;(add-hook   'autotest-mode-hook 'flyspell-prog-mode)
;;disabled;(add-hook   'makefile-mode-hook 'flyspell-prog-mode)
;;disabled;(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)

;; ######################################################
;;(setq ispell-dictionary "german-new8")
;;(setq ispell-dictionary "german-new8")
;;(setq ispell-local-dictionary "german-new8")
(bind-key "fm" #'flyspell-mode my-map)
(bind-key "fr" #'flyspell-region my-map)
(bind-key "fl" #'my-toggle-ispell-language my-map)
(bind-key "ft" #'my-toggle-ispell-language my-map);; can't remember if l(anguage) or t(oggle)
(bind-key "fn" #'flyspell-goto-next-error my-map)
(bind-key "ff" #'flyspell-correct-word-before-point my-map)


;; #############################################################################
;;* flycheck
;; http://www.flycheck.org/en/latest/guide/quickstart.html

(add-hook 'after-init-hook #'global-flycheck-mode) ; enable everywhere
(setq flycheck-flake8-maximum-line-length 200); http://www.flycheck.org/manual/latest/Configuring-checkers.html#Configuring-checkers


;; #############################################################################
;;* tabbar (disabled)


;;disabled;(when (my-system-type-is-gnu)
;;disabled;    (my-load-local-el "contrib/tabbar.el")
;;disabled;  )


;; #############################################################################
;;* yasnippet

(use-package yasnippet
  :demand t
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :diminish yas-minor-mode
  :defer 10
  :config
  (yas-load-directory "~/.emacs.d/snippets/")
  (yas-global-mode 1)
  ;; http://yasnippet.googlecode.com/svn/trunk/doc/index.html
  ;;disabled;(my-load-local-el "contrib/yasnippet/yasnippet.el")
  ;;(autoload 'yas-minor-mode "yasnippet")

  ;;disabled 2015-04-01 - issues did not vanish;; ;; https://capitaomorte.github.io/yasnippet/faq.html#sec-4
  ;;disabled 2015-04-01 - issues did not vanish;; ;; How to I use alternative keys, i.e. not TAB?
  ;;disabled 2015-04-01 - issues did not vanish;; ;; see id:2015-02-01-yas-expand-not-TAB
  ;;disabled 2015-04-01 - issues did not vanish;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;;disabled 2015-04-01 - issues did not vanish;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;;disabled 2015-04-01 - issues did not vanish;; (define-key yas-minor-mode-map (kbd "<f4>") 'yas-expand)

)


;; #############################################################################
;;* REST
;; REST client    https://github.com/pashky/restclient.el
(use-package restclient
  :disabled t  ;; stop loading if 't'
  :ensure t ;; install package if not found OR: (setq use-package-always-ensure t)
  :defer 10
  :if (my-system-is-powerplantwin)
  :init ;; executed before loading package
  (my-load-local-el "contrib/2del/restclient/json-reformat.el")
  ;;(my-load-local-el "contrib/restclient/restclient.el")
  ;; :mode "\\.rb\\'"
)


;;* Org-mode
;; org-mode-begin:
;; additionally: http://stackoverflow.com/questions/3622603/org-mode-setup-problem-when-trying-to-use-capture

;; (message "############### DEBUG: loading orgmode ...")
;; (use-package org
;;   :ensure t
;;   :pin manual
;;   :if (or
;;        (my-system-type-is-gnu)
;;        (my-system-is-blanche)
;;        (my-system-is-powerplantlinux)
;;        (my-system-is-powerplantwin)
;;        )
;;
;;   ;; set paths to manually installed Org-mode (from git)
;;   :load-path ( "~/.emacs.d/contrib/org-mode/lisp"
;;                "~/.emacs.d/contrib/org-mode/contrib/lisp")
;;
;;   ;; assign file extensions to Org-mode
;;   :mode ("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)
;;
;;   :config ;; executed after loading package
;;
;;   (message "############### DEBUG: config orgmode ...")

(when (or (my-system-type-is-gnu) (my-system-type-is-darwin) (my-system-is-powerplantwin))

  (setq org-babel-safe-header-args nil);; 2014-10-29 test

;;** load Org and misc contrib packages

  ;; set paths to manually installed Org-mode (from git)
  (add-to-list 'load-path "~/.emacs.d/contrib/org-mode/lisp")
  (add-to-list 'load-path "~/.emacs.d/contrib/org-mode/contrib/lisp" t)

  ;; assign file extensions to Org-mode
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

  ;;(setq org-babel-safe-header-args nil);; 2014-10-29 test

  ;;  :bind ("M-o l" . highlight-lines-matching-regexp)
  ;;  :bind (("M-o l" . highlight-lines-matching-regexp)
  ;;         ("M-o r" . highlight-regexp)
  ;;         ("M-o w" . highlight-phrase)))


  (my-load-local-el "contrib/org-mode/contrib/lisp/org-checklist.el")
  (my-load-local-el "contrib/org-mode/contrib/lisp/org-depend.el")
  (my-load-local-el "contrib/org-mode/contrib/lisp/org-expiry.el")
  ;;disabled;; (my-load-local-el "contrib/org-mode/contrib/lisp/ox-confluence.el")
  (my-load-local-el "contrib/org-mode/contrib/lisp/ox-freemind.el")
  (my-load-local-el "contrib/ob-restclient.el/ob-restclient.el")
  (autoload 'org-checklist "org-checklist.el")

  ;; http://repo.or.cz/w/org-mode.git?a=blob_plain;f=contrib/lisp/org-expiry.el;hb=HEAD
  ;; Expiry dates handling
  (autoload 'org-expiry "org-expiry.el")

  ;; managing bookmarks with Org-mode
  ;; http://orgmode.org/worg/org-contrib/org-protocol.html
  (autoload 'org-protocol "org-protocol")

  ;; org-favtable
  ;;deactivated; (require 'org-favtable)
  ;;deactivated; (setq org-favtable-id "my-favtable")
  ;;deactivated; (global-set-key (kbd "C-+") 'org-favtable)

  ;; Enable org modules
  (setq org-modules (quote
                     (org-bbdb
		      org-bibtex
                      org-crypt
                      org-gnus
                      org-id
                      org-info
                      org-habit
                      org-inlinetask
                      org-irc
                      org-mew
                      org-mhe
                      org-protocol
                      org-rmail
                      org-vm
                      org-wl
                      org-w3m
                      )
                     )
        )

  ;; https://github.com/kawabata/ox-pandoc
  (use-package ox-pandoc
    ;;:disabled t  ;; stop loading if 't'
    :ensure t ;; install package if not found OR: (setq use-package-always-ensure t)
    :defer 10
    ;; :if (or (my-system-type-is-gnu) (my-system-is-powerplantwin))
    :init ;; executed before loading package
    ;; org-pandoc-menu-entry -> customize what is shown in menu
    ;; :mode "\\.rb\\'"
  )

;;** general Org-mode settings

  ;; http://www.reddit.com/r/emacs/comments/2m4b7j/help_setting_orgmode_as_the_default_major_mode/
  ;; 2014-12-07
  (add-to-list 'auto-mode-alist '("'" . org-mode) t)

  ;; http://yasnippet.googlecode.com/svn/trunk/doc/index.html
  ;;disabled;(my-load-local-el "contrib/yasnippet/yasnippet.el")
  (add-hook 'org-mode-hook 'yas-minor-mode-on)
  (setq yas-indent-line 'fixed) ;; fixes Org-mode issue with yasnippets: https://github.com/capitaomorte/yasnippet/issues/362


  (setq org-startup-indented t);; Might cause performance issues; http://orgmode.org/manual/Clean-view.html
  (setq org-enforce-todo-dependencies t)
  (setq org-blank-before-new-entry (quote ((heading . t)
					   (plain-list-item . nil))))
  (setq org-insert-heading-respect-content nil)
  (setq org-reverse-note-order nil)
  (setq org-show-following-heading t)
  (setq org-show-hierarchy-above t)
  (setq org-show-siblings nil)
  (setq org-deadline-warning-days 1)
  (setq org-table-export-default-format "orgtbl-to-csv")
  (setq org-log-done (quote time))
  (setq org-log-into-drawer t)
  (setq org-log-redeadline (quote note));; record when the deadline date of a tasks is modified
  (setq org-log-reschedule (quote time))
  (setq org-return-follows-link t)
  (setq org-remove-highlights-with-change nil)
  (setq org-read-date-prefer-future nil)
  (setq org-list-demote-modify-bullet (quote (("+" . "-")
					      ("*" . "-")
					      ("1." . "-")
					      ("1)" . "-"))))
  (setq split-width-threshold 9999);; Minimum width for splitting windows sensibly.
  (setq global-auto-revert-mode t)
  (setq require-final-newline nil)
  (setq org-hide-leading-stars t)
  ;; default state for repeating/recurring events
  ;; see http://orgmode.org/org.html#Repeated-tasks and http://orgmode.org/org.html#fn-77
  (setq org-todo-repeat-to-state "NEXT")

  ;; http://orgmode.org/org.html show blocked tasks in agenda in gray color
  ;;disabled;(setq org-agenda-dim-blocked-tasks t)
  ;; OR: hide blocked tasks completely:
  ;; http://nflath.com/2010/03/org-mode-2/
  ;; http://stackoverflow.com/questions/15750480/org-mode-agenda-blocks-not-obeying-settings
  (setq org-agenda-dim-blocked-tasks 'invisible)


  ;; smart navigation
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  ;; smart yanking
  (setq org-yank-adjusted-subtrees t);; https://www.gnu.org/software/emacs/manual/html_node/org/Structure-editing.html


  ;; ######################################################
  ;; set the timestamps of expiry.el to inactive ones
  ;; http://comments.gmane.org/gmane.emacs.orgmode/20934
  (setq org-expiry-inactive-timestamps t)

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
  ;; 2014-04-04: set to nil in order to avoid performance issues!
  ;; 2015-12-26: set to t to test again these days
  (setq org-src-fontify-natively t)



  (setq org-completion-use-ido t);; Use IDO for target completion

  ;; Targets include this file and any file contributing to the
  ;;    agenda - up to 5 levels deep
  ;; see also: https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
  (setq org-refile-targets (quote (
                                   (org-agenda-files :maxlevel . 4)
                                   ("contacts.org" :maxlevel . 6)
                                   (nil :maxlevel . 3)
                                   )))

  ;; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path (quote file))

  ;; 2014-10-22: activate caching of targets
  (setq org-refile-use-cache t)

  ;; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  ;; disable property inheritance (in order to seed up)
  ;; https://www.gnu.org/software/emacs/manual/html_node/org/Property-inheritance.html
  (setq org-use-property-inheritance nil)

  (setq org-tags-match-list-sublevels nil);; https://www.gnu.org/software/emacs/manual/html_node/org/Matching-tags-and-properties.html


  ;; ######################################################
  ;; automatically change to DONE when all children are done
  ;; http://orgmode.org/org.html#Breaking-down-tasks
  ;;deactivated because WAITING got changed to TODO;; (defun org-summary-todo (n-done n-not-done)
  ;;deactivated because WAITING got changed to TODO;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
  ;;deactivated because WAITING got changed to TODO;;   (let (org-log-done org-log-states)   ; turn off logging
  ;;deactivated because WAITING got changed to TODO;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  ;;deactivated because WAITING got changed to TODO;; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  ;; statistic cookies count ALL subtasks not only direkt ones
  (setq org-hierarchical-todo-statistics t)



  ;; ######################################################
  ;(setq org-src-prevent-auto-filling t)

  ;; ######################################################
  ;; 2013-09-13:
  ;; From: Release Notes v8.1
  ;; http://orgmode.org/worg/agenda-optimization.html
  (setq org-agenda-ignore-drawer-properties '(effort appt stats));; agenda performance

  ;; automatically CREATED properties
  (org-expiry-insinuate)
  ;; not checked yet: (setq org-expiry-handler-function 'org-expiry-archive-subtree)

  ;; ######################################################
  ;; checking org-mode syntax:
  ;;(require 'org-lint)

  ;; ######################################################
  ;; stop the mouse cursor from highlighting lines in the agenda
  ;; http://orgmode.org/worg/org-faq.html
  (add-hook 'org-finalize-agenda-hook
            (lambda () (remove-text-properties
                        (point-min) (point-max) '(mouse-face t))))

  ;; http://endlessparentheses.com/changing-the-org-mode-ellipsis.html
  (setq org-ellipsis " ⤵")

  ;; https://twitter.com/_wilfredh/status/708046038200950787
  (defvar my-cpp-other-file-alist
  '(("\\.org\\'" (".org_archive"))
    ;;("\\.ipp\\'" (".hpp" ".cpp"))
    ;;("\\.hpp\\'" (".ipp" ".cpp"))
    ;;("\\.cxx\\'" (".hxx" ".ixx"))
    ;;("\\.ixx\\'" (".cxx" ".hxx"))
    ;;("\\.hxx\\'" (".ixx" ".cxx"))
    ;;("\\.c\\'" (".h"))
    ;;("\\.h\\'" (".c"))
    ))
  (setq-default ff-other-file-alist 'my-cpp-other-file-alist)
  ;; open corresponding .org_archive file with ~ff-find-other-file~

  ;; http://orgmode.org/manual/The-Export-Dispatcher.html
  (setq org-export-initial-scope 'subtree);; export subtree (by default)

;;** general key bindings

  ;; http://doc.norang.ca/org-mode.html
  ;;
  ;; Standard key bindings
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)

  ;; unset C-c , (org-priority) because I get confused when I mistype C-c C-,
  ;;(global-unset-key (kbd "C-c ,"))
  (global-set-key (kbd "C-c ,") 'my-map)

  ;; ######################################################
  ;; remembering positions
  (global-set-key (kbd "C-c %") 'org-mark-ring-push)
  (global-set-key (kbd "C-c <left>") 'org-mark-ring-goto)
  (global-set-key (kbd "C-c <down>") 'org-mark-ring-push)

  ;;disabled;(setq mac-command-modifier 'apple)
  ;;disabled;(global-set-key [(<apple> <up>)] 'org-move-subtree-up)
  ;;disabled;(global-set-key "\S-<down>" 'org-move-subtree-down)
  ;;disabled;(global-set-key "\S-<left>" 'org-do-promote)
  ;;disabled;(global-set-key "\S-<right>" 'org-do-demote)

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
  ;; fix broken mapping (because of prelude and so on)
  ;;disabled;;2014-01-19;;(defun my-org-mode-hook ()
  ;;disabled;;2014-01-19;;  (define-key prelude-mode-map (kbd "C-c +") nil)
  ;;disabled;;2014-01-19;;  (define-key prelude-mode-map (kbd "C-c -") nil)
  ;;disabled;;2014-01-19;;  )
  ;;disabled;;2014-01-19;;(add-hook 'org-mode-hook 'my-org-mode-hook)

;;** org-mode-hook

;;disabled;; (add-hook 'org-mode-hook
;;disabled;;           (lambda ()
;;disabled;;             ;; yasnippet
;;disabled;;             ;;disabled;            (make-variable-buffer-local 'yas/trigger-key)
;;disabled;;             ;;disabled;            (org-set-local 'yas/trigger-key [tab])
;;disabled;;             ;;disabled;            (define-key yas/keymap [tab] 'yas/next-field-group)
;;disabled;;             ;; flyspell mode for spell checking everywhere
;;disabled;;             ;;disabled; (flyspell-mode 1)
;;disabled;;             ;; auto-fill mode on
;;disabled;;             (auto-fill-mode 1)))

  ;; ######################################################
  ;; Make TAB the yas trigger key in the org-mode-hook and enable flyspell mode and autofill
  (add-hook 'org-mode-hook
	    (lambda ()
	      ;; flyspell mode for spell checking everywhere
	      ;;disabled; (flyspell-mode 1)
	      ;; Undefine C-c [ and C-c ] since this breaks my org-agenda files when directories are include
	      ;; It expands the files in the directories individually
	      (org-defkey org-mode-map "\C-c["    'undefined)
	      (org-defkey org-mode-map "\C-c]"    'undefined)
	      ;;            (local-set-key (kbd "C-c M-o") 'bh/mail-subtree)
	      )
	    )

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


;;** exporters

  (require 'ox-html) ;; 2014-11-16 if replaced with autoload above, it
  (require 'ox-latex)
  (require 'ox-koma-letter)
  (require 'ox-ascii)
  ;;(require 'ox-beamer)
  ;;(require 'ox-odt)
  ;;(require 'ox-freemind)
  ;;(require 'ox-taskjuggler)

  ;; add bibtex to pdf export method:
  ;; http://orgmode.org/worg/exporters/anno-bib-template-worg.html#sec-5
  ;; see id:2015-03-21-org-reftex-export
   (setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))


  ;; disabled 2015-03-22 because it did not help ;; ;; ######################################################
  ;; disabled 2015-03-22 because it did not help ;; ;; Guess the master file for FILENAME from currently open files according to their extension.
  ;; disabled 2015-03-22 because it did not help ;; (add-hook 'org-mode-hook
  ;; disabled 2015-03-22 because it did not help ;; 	    (lambda ()
  ;; disabled 2015-03-22 because it did not help ;;             (setq TeX-master (guess-TeX-master (buffer-file-name)))
  ;; disabled 2015-03-22 because it did not help ;; 	      (message (concat "set master file to: " buffer-file-name))
  ;; disabled 2015-03-22 because it did not help ;; 	      )
  ;; disabled 2015-03-22 because it did not help ;; 	    )



;;** TODO keywords + faces

  ;; ######################################################
  ;; define keywords:
  (setq org-todo-keywords (quote
			   (
			    (sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAITING(w@/!)" "SOMEDAY(S!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
			    ;;                                (sequence "OPEN(O!)" "|" "CLOSED(C!)")
			    )
			   )
	)
  (setq org-todo-keyword-faces
	(quote (("TODO"      :foreground "lightblue"    :weight bold)
		("NEXT"      :foreground "red"          :weight bold)
		("STARTED"   :foreground "red"          :weight bold)
		("DONE"      :foreground "forest green" :weight bold)
		("WAITING"   :foreground "orange"       :weight bold)
		("TEAM"      :foreground "orange"       :weight bold)
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

  ;;disabled;; (setq org-use-fast-todo-selection t);; https://www.gnu.org/software/emacs/manual/html_node/org/TODO-basics.html
  ;;disabled;; (setq org-treat-S-cursor-todo-selection-as-state-change nil);; https://www.gnu.org/software/emacs/manual/html_node/org/TODO-basics.html


  ;; CANCELED -> add ARCHIVE-tag: http://article.gmane.org/gmane.emacs.orgmode/64852
  ;; disabled 2015-12-07: (setq org-todo-state-tags-triggers
  ;; disabled 2015-12-07:       (quote (("CANCELLED"
  ;; disabled 2015-12-07:       	 ("ARCHIVE" . t))
  ;; disabled 2015-12-07:       	("WAITING"
  ;; disabled 2015-12-07:       	 ("WAITING" . t))
  ;; disabled 2015-12-07:       	(done
  ;; disabled 2015-12-07:       	 ("WAITING"))
  ;; disabled 2015-12-07:       	("TODO"
  ;; disabled 2015-12-07:       	 ("WAITING")
  ;; disabled 2015-12-07:       	 ("CANCELLED"))
  ;; disabled 2015-12-07:       	("NEXT"
  ;; disabled 2015-12-07:       	 ("WAITING"))
  ;; disabled 2015-12-07:       	("STARTED"
  ;; disabled 2015-12-07:       	 ("WAITING"))
  ;; disabled 2015-12-07:       	("DONE"
  ;; disabled 2015-12-07:       	 ("WAITING")
  ;; disabled 2015-12-07:       	 ("CANCELLED")))))

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



;;** agenda files
  (if (my-system-is-powerplantwin)
      (setq org-agenda-files (append (quote (
					     ;;"c:/Users/karl.voit/share/all/org-mode/phd.org";;moved to archive
					     "c:/Users/karl.voit/share/all/org-mode/r6-stories.org"
					     "c:/Users/karl.voit/share/all/org-mode/infonova.org"
					     "c:/Users/karl.voit/share/all/org-mode/misc.org"
					     "c:/Users/karl.voit/share/all/org-mode/bwg.org"
					     ;;"c:/Users/karl.voit/share/all/org-mode/tagstore.org";;moved to archive
					     ;;"c:/Users/karl.voit/share/all/org-mode/ist.org";;moved to archive
					     "c:/Users/karl.voit/share/all/org-mode/contacts.org"
					     ;;"c:/Users/karl.voit/share/all/org-mode/postdoc.org";;moved to archive
					     ;;"c:/Users/karl.voit/share/all/org-mode/foodandbeverages.org"
					     "c:/Users/karl.voit/share/all/org-mode/hardware.org"
					     "c:/Users/karl.voit/share/all/org-mode/fhsp.org"
					     "c:/Users/karl.voit/share/all/org-mode/notes.org"
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
					   "~/share/all/org-mode/archive.org"
					   ;;"~/share/all/org-mode/phd.org";;moved to archive
					   "~/share/all/org-mode/r6-stories.org"
					   "~/share/all/org-mode/infonova.org"
					   ;;  "~/share/all/org-mode/test-phd.org"
					   "~/share/all/org-mode/misc.org"
					   "~/share/all/org-mode/bwg.org"
					   ;;"~/share/all/org-mode/tagstore.org";;moved to archive
					   ;;"~/share/all/org-mode/ist.org";;moved to archive
					   "~/share/all/org-mode/contacts.org"
					   ;;"~/share/all/org-mode/postdoc.org";;moved to archive
					   "~/share/all/org-mode/foodandbeverages.org"
					   "~/share/all/org-mode/hardware.org"
					   "~/share/all/org-mode/notes.org"
					   "~/share/all/org-mode/movies.org"
					   "~/share/all/org-mode/references.org"
					   "~/share/all/org-mode/public_voit.org"
					   "~/share/all/org-mode/errors_public_voit.org"
					   "~/share/all/org-mode/errors_orgmode_commits.org"
					   "~/share/all/org-mode/fhsp.org"
					   "~/src/lazyblorg/dev/lazyblorg.org"
					   "~/share/all/org-mode/memacs/roylog.org"
					   ;;"~/share/all/org-mode/memacs/archive.org"
					   "~/share/all/org-mode/memacs/bank.org"
					   ;;"~/share/all/org-mode/memacs/datebk6.org"
					   ;;"~/share/all/org-mode/memacs/delicious.org"
					   "~/share/all/org-mode/memacs/error.org"
					   "~/share/all/org-mode/memacs/files.org"
					   "~/share/all/org-mode/memacs/gcal.org"
					   "~/share/all/org-mode/memacs/git.org"
					   ;;"~/share/all/org-mode/memacs/ifiles.org"
					   "~/share/all/org-mode/memacs/mbox.org"
					   "~/share/all/org-mode/memacs/news.org"
					   "~/share/all/org-mode/memacs/phonecalls.org"
					   "~/share/all/org-mode/memacs/sms.org"
					   "~/share/all/org-mode/memacs/tweets.org"
					   ;;"~/share/all/org-mode/memacs/www.org"
					   "~/share/all/org-mode/memacs/movies.org"
					   "~/share/all/org-mode/memacs/Filmliste.org"
					   )
					  )
				   ;;(file-expand-wildcards "~/share/all/org-mode/memacs/*.org")
				   )
	  )

    )


  ;; ######################################################
;;** my-url-linkify (my-map u)
  ;; replaces URL with Org-mode link including description
  ;; see id:2014-03-09-inbox-to-bookmarks
  ;; 2014-03-18: alternative method: http://orgmode.org/worg/org-hacks.html#sec-1-6-3 "Insert link with HTML title as default description"
  ;; see also: http://orgmode.org/worg/org-hacks.html#orgheadline54 (not this method here!)
  (defun my-www-get-page-title (url)
    "retrieve title of web page.
from: http://www.opensubscriber.com/message/help-gnu-emacs@gnu.org/14332449.html"
    (let ((title))
      (with-current-buffer (url-retrieve-synchronously url)
	(goto-char (point-min))
	(re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
	(setq title (match-string 1))
	(goto-char (point-min))
	(re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
        (string-replace "&nbsp;" " "
                        ;;(decode-coding-string title (intern (match-string 1)))
                        ;; following line fixes charset issues from
                        ;; previous line:
                        (decode-coding-string title 'utf-8)
                        ))
      )
    )
  (defun my-url-linkify ()
    "Make URL at cursor point into an Org-mode link.
If there's a text selection, use the text selection as input.

Example: http://example.com/xyz.htm
becomes
\[\[http://example.com/xyz.htm\]\[Source example.com\]\]

Adapted code from: http://ergoemacs.org/emacs/elisp_html-linkify.html"
    (interactive)
    (let (resultLinkStr bds p1 p2 domainName)
      ;; get the boundary of URL or text selection
      (if (region-active-p)
	  (setq bds (cons (region-beginning) (region-end)) )
	(setq bds (bounds-of-thing-at-point 'url))
	)
      ;; set URL
      (setq p1 (car bds))
      (setq p2 (cdr bds))
      (let (
	    (url (buffer-substring-no-properties p1 p2))
	    )
	;; retrieve title
	(let ((title (my-www-get-page-title url)))
	  (message (concat "title is: " title))
	  ;;(setq url (replace-regexp-in-string "&" "&amp;" url))
	  (let ((resultLinkStr (concat "[[" url "][" title "]]")))
	    ;; delete url and insert the link
	    (delete-region p1 p2)
	    (insert resultLinkStr)
	    )
	  )
	)
      )
    )

  (bind-key "u" #'my-url-linkify my-map)

;; ######################################################
;;** my-insert-orgmode-url-from-clipboard (my-map U)
  ;; inserts URL from clipboard and retrieves title as Org-mode link
  ;; see id:2014-08-10-bookmarks-with-orgmode
  ;; 2015-05-22: Via email arjan: http://www.rexim.me/emacs-as-bookmark-manager-links.html

  (defun straight-string (s)
    ;; Spliting the string and then concatenating it back.
    (mapconcat '(lambda (x) x) (split-string s) " "))

  (defun my-cliplink-format-and-trim-title (title)
    (let (;; Table of replacements which make this title usable for
          ;; org-link. Can be extended.
          (replace-table '(("\\[" . "{")
                           ("\\]" . "}")
                           ("&mdash;" . "—")))
          ;; Maximum length of the title.
          (max-length 100)
          ;; Removing redundant whitespaces from the title.
          (result (straight-string title)))
      ;; Applying every element of the replace-table.
      (dolist (x replace-table)
        (setq result (replace-regexp-in-string (car x) (cdr x) result)))
      ;; Cutting off the title according to its maximum length.
      (when (> (length result) max-length)
        (setq result (concat (substring result 0 max-length) "...")))
      ;; Returning result.
      result))

  (defun extract-title-from-html (html)
    (let (;; Start index of the title.
          (start (string-match "<title>" html))
          ;; End index of the title.
          (end (string-match "</title>" html))
          ;; Amount of characters to skip the openning title tag.
          (chars-to-skip (length "<title>")))
      ;; If title is found ...
      (if (and start end (< start end))
          ;; ... extract it and return.
          (substring html (+ start chars-to-skip) end)
        nil)))

  (defun cliplink-decode-content-and-return-orgmode-link-of-title (buffer url content)
    (let* (;; Decoding the content from UTF-8.
           (decoded-content (decode-coding-string content 'utf-8))
           ;; Extrating and preparing the title.
           (title (my-cliplink-format-and-trim-title
                   (extract-title-from-html decoded-content))))
      ;; Inserting org-link.
      (with-current-buffer buffer
        (insert (format "[[%s][%s]]" url title)))))

  (defun my-insert-orgmode-url-from-clipboard ()
    ;; Of course, this function is interactive. :)
    (interactive)
    (let (;; Remembering the current buffer, 'cause it is a destination
          ;; buffer we are inserting the org-link to.
          (dest-buffer (current-buffer))
          ;; Getting URL from the clipboard. Since it may contain
          ;; some text properties we are using substring-no-properties
          ;; function.
          (url (substring-no-properties (current-kill 0))))
      ;; Retrieving content by URL.
      (url-retrieve
       url
       ;; Performing an action on the retrieved content.
       `(lambda (s)
          (cliplink-decode-content-and-return-orgmode-link-of-title ,dest-buffer ,url
                            (buffer-string))))))

  (bind-key "U" #'my-insert-orgmode-url-from-clipboard my-map)


;;** my-jump-to-lazyblorg-heading-according-to-URL-in-clipboard (my-map l)
  ;; inserts public voit URL from clipboard and jumps to its Org-mode heading
  ;; 2015-05-23: adapted from: http://www.rexim.me/emacs-as-bookmark-manager-links.html

  (defun my-jump-to-lazyblorg-heading-according-to-URL-in-clipboard ()
    "Retrieves an URL from the clipboard, gets its Org-mode source,
     extracts the ID of the article and jumps to its Org-mode heading"
    (interactive)
    (let (
          ;; Getting URL from the clipboard. Since it may contain
          ;; some text properties we are using substring-no-properties
          ;; function
          (url (substring-no-properties (current-kill 0)))
          ;; This is a check string: if the URL in the clipboard
          ;; doesn't start with this, an error message is shown
          (domain "http://karl-voit.at")
  	)
      ;; Check if URL string is from my domain (all other strings do
      ;; not make any sense here)
      (if (string-prefix-p (upcase domain) (upcase url))
	  ;; Retrieving content by URL into new buffer asynchronously
	  (url-retrieve url
                        ;; call this lambda function when URL content is retrieved
			(lambda (status)
			   ;; Extrating and preparing the ID
			   (let* (
                                  ;; Limit the ID search to the top 1000 characters of the buffer
				  (pageheader (buffer-substring 1 1000))
				  ;; Start index of the id
                                  (start (string-match "<meta name=\"orgmode-id\" content=\"" pageheader))
                                  ;; End index of the id
                                  (end (string-match "\" />" pageheader start))
                                  ;; Amount of characters to skip for the openning tag
                                  (chars-to-skip (length "<meta name=\"orgmode-id\" content=\""))
                                  ;; Extract ID
                                  (lazyblorg-id (if (and start end (< start end))
                                                    ;; ... extract it and return.
                                                    (substring pageheader (+ start chars-to-skip) end)
                                                  nil))
                                  )
			     (message (concat "Looking for id:" lazyblorg-id " ..."))
			     (org-open-link-from-string (concat "id:" lazyblorg-id))
			     )
			   )
			)
  	(message (concat "Sorry: the URL \"" (substring url 0 (length domain)) "...\" doesn't start with \"" domain "\". Aborting."))
  	)
      )
    )



;; ######################################################
;;** bookmarks (my-map b)
  ;; smart moving bookmark headings from inbox to notes.org
  ;; see id:2014-03-09-inbox-to-bookmarks
  (defun my-save-bookmark()
    "removes NEXT/Bookmark, (NOT YET: FIXXME: retrieves title),
move time-stamp to CREATED, re-file to bookmarks, invoke Org-mode tagging process"
    (interactive)
    (save-excursion
      ;; get myself to the beginning of the current heading:
      ;;(outline-previous-visible-heading 1)  ;; jump to previous heading
      ;;(outline-next-visible-heading 1)      ;; jumps to beginning of the current (interesting) heading
      (beginning-of-line)                   ;; jump to beginning of line
      (let ((mybegin (point)))              ;; mark beginning of line as start point
	(outline-next-visible-heading 1)    ;; jumps to EOF if it is the last entry
	(save-restriction
	  (narrow-to-region mybegin (point))  ;; ignore everything outside of region
	  ;; search/replace unwanted keywords at the beginning:
	  (goto-char (point-min))
	  (while (search-forward "* NEXT Bookmark " nil t) (replace-match "* " nil t))
	  (goto-char (point-min))
	  (while (search-forward "* NEXT " nil t) (replace-match "* " nil t))
	  (goto-char (point-min))
	  (while (search-forward "* Bookmark " nil t) (replace-match "* " nil t))
	  (goto-char (point-min))
	  (while (search-forward "//m.heise.de" nil t) (replace-match "//heise.de" nil t));; remove mobile heise URL
	  (goto-char (point-min))
	  (while (search-forward "/from/atom10?wt_mc=rss.ho.beitrag.atom" nil t);; remove heise RSS tags
	    (replace-match "" nil t)
	    )
	  (goto-char (point-min))
	  ;; insert second asterisk (modify to second level heading)
	  (insert "*")
	  ;; move time-stamp to properties-drawer:
	  (search-forward-regexp "^\\[20")  ;; jump to second line (with time-stamp) via search
	  (beginning-of-line)
	  (insert ":PROPERTIES:\n:CREATED:  ")
	  (end-of-line)
	  (newline)
	  (insert ":END:\n")
	  ;; move region to end of notes.org
	  (kill-region mybegin (point)) ;; kill region to kill-ring
	  (switch-to-buffer "notes.org")
	  (end-of-buffer)
	  (newline)
	  (yank)
	  ;; add tags
	  (outline-previous-visible-heading 1)  ;; jump to heading
	  (org-set-tags-command)
	  )
	)
      )
    )

  (bind-key "b" #'my-save-bookmark my-map)


;;** misc agenda helper functions

  ;; ######################################################
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

  ;; ######################################################
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




  ;; ######################################################
  (defun my-buffer-exists (bufname)
    (not (eq nil (get-buffer bufname)))
    )


;;** org-agenda-custom-commands
  (setq org-agenda-custom-commands
	(quote (

		;; disabled 2014-08-17 ;;          ;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-07/msg01374.html
		;; disabled 2014-08-17 ;;          ("E" "events only" agenda ""
		;; disabled 2014-08-17 ;;           (
		;; disabled 2014-08-17 ;;            (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'todo))
		;; disabled 2014-08-17 ;;            ))
		("n" "no TODO events +50d"
		 ((agenda "no TODO events +50d"
			  ((org-agenda-ndays 50)
			   (org-agenda-time-grid nil)
			   (org-agenda-entry-types '(:timestamp :sexp))
			   (org-agenda-skip-function
                            '(or
                              (org-agenda-skip-entry-if 'todo 'any);; skip if any TODO state is found
                              (my-skip-tag "lp")
                              )
                            )
                           ;;(org-agenda-skip-function '(my-skip-tag "lp"))
			  ))))

		;; disabled 2015-02-15 - replaced by no TODO events ;; ("p" "events Prio [#A]"
		;; disabled 2015-02-15 - replaced by no TODO events ;;  ((agenda "+PRIORITY=\"A\""
		;; disabled 2015-02-15 - replaced by no TODO events ;; 	  ((org-agenda-ndays 31)
		;; disabled 2015-02-15 - replaced by no TODO events ;; 	   (org-agenda-time-grid nil)
		;; disabled 2015-02-15 - replaced by no TODO events ;; 	   (org-agenda-entry-types '(:timestamp :sexp))
		;; disabled 2015-02-15 - replaced by no TODO events ;; 	   (org-agenda-skip-function
		;; disabled 2015-02-15 - replaced by no TODO events ;; 	    '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")))
		;; disabled 2015-02-15 - replaced by no TODO events ;; 	  ;; (
		;; disabled 2015-02-15 - replaced by no TODO events ;; 	  ;;  (org-agenda-skip-function 'tag-without-done-or-canceled)
		;; disabled 2015-02-15 - replaced by no TODO events ;; 	  ;;  )
		;; disabled 2015-02-15 - replaced by no TODO events ;; 	  )))

		;; disabled 2015-02-15 - replaced by no TODO + normal agenda ;; ("1" "1 month"
		;; disabled 2015-02-15 - replaced by no TODO + normal agenda ;;  ((agenda "1 month"
		;; disabled 2015-02-15 - replaced by no TODO + normal agenda ;; 	  ((org-agenda-ndays 31)
		;; disabled 2015-02-15 - replaced by no TODO + normal agenda ;; 	   (org-agenda-time-grid nil)
		;; disabled 2015-02-15 - replaced by no TODO + normal agenda ;; 	   (org-agenda-entry-types '(:timestamp :sexp))
		;; disabled 2015-02-15 - replaced by no TODO + normal agenda ;; 	   )
		;; disabled 2015-02-15 - replaced by no TODO + normal agenda ;; 	  )))

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
		("i" "issues" (
				     (tags-todo "issue/!STARTED"
						(
						 (org-agenda-overriding-header "issues: STARTED")
						 ))
				     (tags-todo "issue/!NEXT"
						(
						 (org-agenda-overriding-header "issues: NEXT")
						 ))
				     (tags-todo "issue/!TODO"
						(
						 (org-agenda-overriding-header "issues: TODO")
						 ))
				     (tags-todo "issue/!SOMEDAY"
						(
						 (org-agenda-overriding-header "issues: SOMEDAY")
						 ))
				     ))

		;;disabled;              ("R" "grab reward" tags-todo "reward/!TODO|SOMEDAY"
		;;disabled;	       (
		;;disabled;		(org-agenda-overriding-header "rewards: TODO or SOMEDAY")
		;;disabled;		))

		("B" "borrowed" tags "+borrowed"
		 (
		  (org-agenda-overriding-header "borrowed or lend")
		  (org-agenda-skip-function 'tag-without-done-or-canceled)
		  ))

		("$" "Besorgungen" tags "+Besorgung"
		 (
		  (org-agenda-overriding-header "Besorgungen")
		  (org-agenda-skip-function 'tag-without-done-or-canceled)
		  ))

		;; disabled 2015-02-15 - can filter by tag ;; ("O" "SOMEDAY" tags-todo "+TODO=\"SOMEDAY\""
		;; disabled 2015-02-15 - can filter by tag ;;  (
		;; disabled 2015-02-15 - can filter by tag ;;   (org-agenda-overriding-header "SOMEDAY is today! :-)")
		;; disabled 2015-02-15 - can filter by tag ;;   ))
                ;; disabled 2015-02-15 - can filter by tag ;;
		;; disabled 2015-02-15 - can filter by tag ;; ("h" "home @ALW" tags-todo "+@ALW"
		;; disabled 2015-02-15 - can filter by tag ;;  (
		;; disabled 2015-02-15 - can filter by tag ;;   (org-agenda-overriding-header "home tasks")
		;; disabled 2015-02-15 - can filter by tag ;;   ))
                ;; disabled 2015-02-15 - can filter by tag ;;
		;; disabled 2015-02-15 - can filter by tag ;; ("b" "Breitenweg @BWG" tags-todo "+@BWG"
		;; disabled 2015-02-15 - can filter by tag ;;  (
		;; disabled 2015-02-15 - can filter by tag ;;   (org-agenda-overriding-header "home tasks")
		;; disabled 2015-02-15 - can filter by tag ;;   ))

		;; disabled 2014-08-17 because of error ;;;; 2014-02-18: from Org-mode ML Subject: Re: Get a list of tasks completed today
		;; disabled 2014-08-17 because of error ;;("." "Completed today"
                ;; disabled 2014-08-17 because of error ;; ((todo "TODO|DONE|CANCELED"
                ;; disabled 2014-08-17 because of error ;;        ((org-agenda-skip-function
                ;; disabled 2014-08-17 because of error ;;          '(org-agenda-skip-entry-if 'notregexp (format-time-string "CLOSED: \\[%Y-%m-%d")))))
		;; disabled 2014-08-17 because of error ;;  (org-agenda-sorting-strategy '(priority-down)))
		;; disabled 2014-08-17 because of error ;; )

		;; disabled 2015-02-15 - don't use it any more ;; ;; 2014-02-18: from Org-mode ML Subject: Re: Get a list of tasks completed today
		;; disabled 2015-02-15 - don't use it any more ;; ("W" "Closed within a week."
		;; disabled 2015-02-15 - don't use it any more ;;  tags "CLOSED>\"<-1w>\""
		;; disabled 2015-02-15 - don't use it any more ;;  ((org-agenda-sorting-strategy '(priority-down))))

		;; disabled 2015-02-15 - don't know what's for ;; ("o" "overview Agenda" (
		;; disabled 2015-02-15 - don't know what's for ;; 			(agenda ""
		;; disabled 2015-02-15 - don't know what's for ;; 				nil )
		;; disabled 2015-02-15 - don't know what's for ;; 			;;diabled by nil above;			((org-agenda-skip-function '(my-skip-tag "reward"))
		;; disabled 2015-02-15 - don't know what's for ;; 			;;diabled by nil above;			 (org-agenda-overriding-header "Agenda without rewards: ")))
		;; disabled 2015-02-15 - don't know what's for ;; 			(tags "+TODO=\"DONE\"+CLOSED>=\"<today>\""
		;; disabled 2015-02-15 - don't know what's for ;; 			      (
		;; disabled 2015-02-15 - don't know what's for ;; 			       (org-agenda-overriding-header "DONE today")
		;; disabled 2015-02-15 - don't know what's for ;; 			       ))
		;; disabled 2015-02-15 - don't know what's for ;; 			;;diabled;                (tags "+reward"
		;; disabled 2015-02-15 - don't know what's for ;; 			;;diabled;                           (
		;; disabled 2015-02-15 - don't know what's for ;; 			;;diabled;			    (org-agenda-overriding-header "Rewards")
		;; disabled 2015-02-15 - don't know what's for ;; 			;;diabled;                            ;(org-agenda-skip-function 'bh/skip-non-stuck-projects))
		;; disabled 2015-02-15 - don't know what's for ;; 			;;diabled;                            ;(org-agenda-todo-ignore-scheduled 'future)
		;; disabled 2015-02-15 - don't know what's for ;; 			;;diabled;                            ;(org-agenda-todo-ignore-deadlines 'future)
		;; disabled 2015-02-15 - don't know what's for ;; 			;;diabled;			   )
		;; disabled 2015-02-15 - don't know what's for ;; 			;;diabled;			   )
		;; disabled 2015-02-15 - don't know what's for ;; 			;;too slow - dont need;                (tags-todo "-CANCELLED/!"
		;; disabled 2015-02-15 - don't know what's for ;; 			;;too slow - dont need;                           ((org-agenda-overriding-header "Stuck Projects")
		;; disabled 2015-02-15 - don't know what's for ;; 			;;too slow - dont need;                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
		;; disabled 2015-02-15 - don't know what's for ;; 			;;slow;                (tags-todo "-CANCELLED+WAITING/!"
		;; disabled 2015-02-15 - don't know what's for ;; 			;;slow;                           ((org-agenda-overriding-header "Waiting and Postponed Tasks")
		;; disabled 2015-02-15 - don't know what's for ;; 			;;slow;                            (org-agenda-skip-function 'bh/skip-stuck-projects)
		;; disabled 2015-02-15 - don't know what's for ;; 			;;slow;                            (org-tags-match-list-sublevels nil)
		;; disabled 2015-02-15 - don't know what's for ;; 			;;slow;                            (org-agenda-todo-ignore-scheduled 'future)
		;; disabled 2015-02-15 - don't know what's for ;; 			;;slow;                            (org-agenda-todo-ignore-deadlines 'future)
		;; disabled 2015-02-15 - don't know what's for ;; 			;;slow;			    ))
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                (tags "REFILE"
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                      ((org-agenda-overriding-header "Tasks to Refile")
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                       (org-tags-match-list-sublevels nil)))
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                (tags "-REFILE/"
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                      ((org-agenda-overriding-header "Tasks to Archive")
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                       (org-tags-match-list-sublevels nil)))
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                (tags-todo "-HOLD-CANCELLED/!"
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                           ((org-agenda-overriding-header "Projects")
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                            (org-agenda-skip-function 'bh/skip-non-projects)
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                            (org-agenda-sorting-strategy
		;; disabled 2015-02-15 - don't know what's for ;; 			;;                             '(category-keep))))
		;; disabled 2015-02-15 - don't know what's for ;; 			)
		;; disabled 2015-02-15 - don't know what's for ;;  nil)

		;;disabled;;	      ("x" "Borrowed"
		;;disabled;;	       ((agenda ""
		;;disabled;;			((org-agenda-skip-function 'tag-without-done-or-canceled)
		;;disabled;;			 (org-agenda-overriding-header "Fun: ")))))

		;;disabled 2014-08-17;; ("X" "calfw" open-calfw-agenda-org)

		;;2012-12-10 deactivated;	      ("c" . "custom searches") ; description for "c" prefix
		;;2012-12-10 deactivated;              ("cr" "rewards" agenda "+reward"
		;;2012-12-10 deactivated;	       (
		;;2012-12-10 deactivated;		(org-agenda-overriding-header "rewards")
		;;2012-12-10 deactivated;		(org-agenda-time-grid nil)
		;;2012-12-10 deactivated;		(org-agenda-entry-types '(:deadline))
		;;2012-12-10 deactivated;	       ))
		;;2012-12-10 deactivated;              ("cr" "rewards" agenda "+reward"
		;;2012-12-10 deactivated;	       (
		;;2012-12-10 deactivated;		(org-agenda-overriding-header "rewards")
		;;2012-12-10 deactivated;		(org-agenda-time-grid nil)
		;;2012-12-10 deactivated;		(org-agenda-entry-types '(:deadline))
		;;2012-12-10 deactivated;	       ))
		;;2012-12-10 deactivated;              ("ct" "@TUG" tags-todo "+@TUG"
		;;2012-12-10 deactivated;               ((org-agenda-overriding-header "TUG Tasks")))
		;;2012-12-10 deactivated;              ("ca" "@ALW" tags-todo "+@ALW"
		;;2012-12-10 deactivated;               ((org-agenda-overriding-header "ALW Tasks")))
		;;2012-12-10 deactivated;              ("cb" "Besorgungen" tags-todo "+Besorgung"
		;;2012-12-10 deactivated;               ((org-agenda-overriding-header "Besorgungen")))
		;;2012-12-10 deactivated;              ("cs" "Started tasks" tags-todo "/!STARTED"
		;;2012-12-10 deactivated;               ((org-agenda-overriding-header "Started Tasks")))
		;;2012-12-10 deactivated;              ("n" "Next and Started tasks" tags-todo "-WAITING-CANCELLED/!NEXT|STARTED"
		;;2012-12-10 deactivated;               ((org-agenda-overriding-header "Next Tasks")))
		;;2012-12-10 deactivated;              ("p" "Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED-WAITING-SOMEDAY"
		;;2012-12-10 deactivated;               ((org-agenda-skip-function 'bh/skip-non-projects)
		;;2012-12-10 deactivated;                (org-agenda-overriding-header "Projects")))
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


;;** agenda settings

  ;; start Agenda in follow-mode:
  ;(setq org-agenda-start-with-follow-mode t)

  ;; t = do not initialize agenda Org files when generating (only) agenda
  ;; nil = initialize normal
  ;; performance issue when not "t": https://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
  ;;(setq org-agenda-inhibit-startup nil);; slower but visibility of buffers is correctly shown
  (setq org-agenda-inhibit-startup t);; faster with no hidden headings (agenda performance)

  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)

  ;; Changed in v7.9.3
  ;; http://orgmode.org/worg/doc.html#org-use-tag-inheritance
  ;; performance issue when not nil: https://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
  (setq org-agenda-use-tag-inheritance (quote (agenda)));; agenda performance

  ;; http://orgmode.org/org.html#Weekly_002fdaily-agenda
  (setq org-agenda-span 1)

  ;; For tag searches ignore tasks with scheduled and deadline dates
  ;;disabled;; (setq org-agenda-tags-todo-honor-ignore-options t)

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


  ;; ######################################################
  ;;  Non-nil means skip timestamp line if same entry shows because of deadline.
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)


  ;; ######################################################
  ;;
  ;; Agenda sorting functions
  ;;
  (setq org-agenda-cmp-user-defined 'bh/agenda-sort)



  ;; ;; Enable display of the time grid so we can see the marker for the current time
  ;; (setq org-agenda-time-grid
  ;;       ((daily today remove-match)
  ;;        #("----------------" 0 16
  ;;          (org-heading t))
  ;;        (800 1000 1200 1400 1600 1800 2000)))

  ;; Display tags farther right
  ;;(setq org-agenda-tags-column -102)
  (setq org-tags-column -101); for powerplantwin 23" TFT turned 90
					; degrees; should *not* differ between
					; systems! Otherwise Org-files gets
					; re-formatted after switching
                                        ; system
  ;;(when (my-system-is-powerplantwin)
  ;;    ;;(setq org-agenda-tags-column -103);; for 23" TFT turned 90 degrees
  ;;      (setq org-agenda-tags-column -117);; for 24" TFT turned 90 degrees
  ;;      )
  ;;(when (my-system-is-sherri)
  ;;    (setq org-agenda-tags-column -117);; -117 for 23" TFT sherri, rotated 90°
  ;;    )
  (setq org-agenda-tags-column (- (- (window-total-width) 3))) ;; total width minus 3

  ;; ######################################################
  ;; Sticky agendas remain opened in the background so that you don't
  ;; need to regenerate them each time you hit the corresponding
  ;; keystroke. This is a big time saver.
  ;;disabled; (setq org-agenda-sticky t)


;;** my-org-agenda (my-map a)

  ;; ######################################################
  ;; 2014-06-03: switch to open Agenda or open new one:
  (defun my-org-agenda ()
    "Opens the already opened agenda or opens new one instead"
    (interactive)

    (setq my-org-agenda-tags-column (- (- (window-total-width) 3)))
    (setq org-agenda-tags-column my-org-agenda-tags-column) ;; total width minus 3

    (if (my-buffer-exists "*Org Agenda*")
	(switch-to-buffer "*Org Agenda*")
      (org-agenda-list)
      )
    )
  (bind-key "a" #'my-org-agenda my-map)


;;** my-memacs-org-agenda (my-map m)

  ;; ######################################################
  ;; 2014-06-28: Memacs org-agenda shortcut
  (defun my-memacs-org-agenda ()
    "Opens an org-agenda with activated archive"
    (interactive)
    (org-agenda-list)
    ;;(call-interactively 'org-agenda-log-mode)
    (org-agenda-log-mode '(4))
    (call-interactively 'org-agenda-archives-mode)
    (org-agenda-archives-mode 'files)
    )
  ;;disabled because I needed "m";; (bind-key "m" #'my-memacs-org-agenda my-map)
  (global-set-key "\C-cm" 'my-memacs-org-agenda)


;;** my-export-agenda
  ;; ######################################################
  (defun my-export-agenda()
    "Exports monthly Org-mode agenda to agenda.ics file"
    (interactive)
    (save-some-buffers)
    (org-agenda-list nil nil 60)
    (message "after org-agenda-list")
    ;;test;  (org-agenda-list nil nil 15)
    (org-agenda-write "~/share/all/org-mode/agenda-export-raw.ics")
    (message "after org-agenda-write")
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
    (shell-command-to-string (concat
			      scriptpath "postprocess_Org-mode_iCal_export.py "
			      "-i " icspath "agenda-export-raw.ics "
			      "-o " icspath "agenda-export-freebusy.ics "
			      "--overwrite "
			      "--obfuscate"
			      )
			     )
    (message "after shell-command-to-string")
    (if (my-system-type-is-gnu)
	(shell-command-to-string "/home/vk/bin/vk-cronjob-gary-do-unison-sync-unattended-share-all_if_host_is_reachable.sh")
      (message "Please do sync using unison!")
      )
    )
;;** Org-contacts
  ;; set org-contacts defaults that differ from standard
  (setq org-contacts-address-property "CITY")
  (setq org-contacts-birthday-property "BORN")
  (setq org-contacts-files "~/share/all/org-mode/contacts.org")
  (setq org-contacts-icon-property "PHOTOGRAPH")


  ;; ######################################################
  ;; contact management with org-contacts
  ;; http://julien.danjou.info/org-contacts.html
  ;; 2016-04-19 sherri hangs endlessly ;; (require 'org-contacts)
  (custom-set-variables
   '(org-contacts-files "~/share/all/org-mode/contacts.org")
   '(org-contacts-address-property "CITY")
   '(org-contacts-birthday-property "BORN")
   '(org-contacts-icon-property "PHOTOGRAPH")
   )


  ;; ######################################################
;;** my-0d
  ;; on an agenda entry: add "-0d" to deadline
  (fset 'my-0d
	[return ?\C-s ?> left ?  ?- ?0 ?d ?\C-x ?b return down])


;; change active timestamp to inactive and cancel event (disabled)
  ;;disabled;; ; ######################################################
  ;;disabled;; ; change active timestamp to inactive and cancel event
  ;;disabled;; ; in the agenda:
  ;;disabled;; fset 'my-agenda-cancel-event-and-set-to-inactive
  ;;disabled;; 	(lambda (&optional arg) "Keyboard macro."
  ;;disabled;; 	  (interactive "p")
  ;;disabled;; 	  (kmacro-exec-ring-item (quote ([return S-up 3 20 99] 0 "%d")) arg)
  ;;disabled;; 	  )
  ;;disabled;; 	)
  ;;disabled;; ; outside of agenda:
  ;;disabled;; fset 'my-cancel-event-and-set-to-inactive
  ;;disabled;; 	(lambda (&optional arg) "Keyboard macro."
  ;;disabled;; 	  (interactive "p")
  ;;disabled;; 	  (kmacro-exec-ring-item (quote ([5 18 62 13 S-up 3 20 99] 0 "%d")) arg)
  ;;disabled;; 	  )
  ;;disabled;; 	)


;;** my-org-agenda-to-appt (disabled)
  ;;disabled;; ;; ######################################################
  ;;disabled;; ;; For org appointment reminders
  ;;disabled;; ;; http://orgmode.org/worg/org-hacks.html#sec-3_1
  ;;disabled;; ;; Get appointments for today
  ;;disabled;; (when (or (my-system-type-is-gnu) (my-system-is-powerplantlinux))
  ;;disabled;;   (defun my-org-agenda-to-appt ()
  ;;disabled;;     (interactive)
  ;;disabled;;     (setq appt-time-msg-list nil)
  ;;disabled;;     (let ((org-deadline-warning-days 0))    ;; will be automatic in org 5.23
  ;;disabled;; 	(org-agenda-to-appt)))
  ;;disabled;;   ;; Run once, activate and schedule refresh
  ;;disabled;;   (my-org-agenda-to-appt)
  ;;disabled;;   (appt-activate t)
  ;;disabled;;   (run-at-time "24:01" nil 'org-agenda-to-appt)
  ;;disabled;;   ;; 5 minute warnings
  ;;disabled;;   (setq appt-message-warning-time 15)
  ;;disabled;;   (setq appt-display-interval 15)
  ;;disabled;;   ;; Update appt each time agenda opened.
  ;;disabled;;   (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
  ;;disabled;;   ;; Setup zenify, we tell appt to use window, and replace default function
  ;;disabled;;   (setq appt-display-format 'window)
  ;;disabled;;   (setq appt-disp-window-function (function appt-disp-window))
  ;;disabled;;   (defun appt-disp-window (min-to-app new-time msg)
  ;;disabled;;     (save-window-excursion
  ;;disabled;; 	(shell-command
  ;;disabled;; 	 (concat "/usr/bin/zenity --info --title='Appointment' --text='" msg "' &") nil nil)
  ;;disabled;; 	)
  ;;disabled;;     )
  ;;disabled;;   )

;;** icons (categories)

  ;; adding icons to categories: http://julien.danjou.info/blog/2010/icon-category-support-in-org-mode
  (setq org-agenda-category-icon-alist nil)
  (when (my-system-is-powerplantwin)
    (add-to-list 'org-agenda-category-icon-alist
		 '(".*" '(space . (:width (16))))
		 )
    (add-to-list 'org-agenda-category-icon-alist
		  '("infonova" "C:/Users/karl.voit/infonova/templates_logos/R6-logo_18x12.jpg" nil nil :ascent center)
		  )
    ;;(add-to-list 'org-agenda-category-icon-alist
    ;;		 '("misc" '(space . (:width (18))))
    ;;		 )
    )
  (when (my-system-type-is-gnu)
    (add-to-list 'org-agenda-category-icon-alist
		 '(".*" '(space . (:width (16))))
		 )
    (add-to-list 'org-agenda-category-icon-alist
		 '("contacts" "/usr/share/icons/oxygen/16x16/places/user-identity.png" nil nil :ascent center)
		 ;;/usr/share/icons/gnome/16x16/emotes/face-smile.png
		  )
    (add-to-list 'org-agenda-category-icon-alist
		 '("public_voit" "/usr/share/icons/oxygen/16x16/mimetypes/application-rss+xml.png" nil nil :ascent center)
		  )
    (add-to-list 'org-agenda-category-icon-alist
		 '("misc" "/usr/share/icons/oxygen/16x16/emblems/emblem-new.png" nil nil :ascent center)
		  )
    (add-to-list 'org-agenda-category-icon-alist
		 '("hardware" "/usr/share/icons/oxygen/16x16/devices/camera-photo.png" nil nil :ascent center)
		  )
    (add-to-list 'org-agenda-category-icon-alist
		 '("bwg" "/usr/share/icons/oxygen/16x16/actions/go-home.png" nil nil :ascent center)
		  )
    )

;;** calfw (disabled)
  ;; ######################################################
  ;; https://github.com/kiwanami/emacs-calfw
  ;; A calendar framework for Emacs
  ;;(add-to-list 'load-path "~/.emacs.d/contrib/calfw/")
  ;;(use-package calfw-org)
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


  ;; ######################################################
;;** capture

  ;; capture:    http://orgmode.org/org.html#Setting-up-capture
  (setq org-default-notes-file "~/share/all/org-mode/inbox.org")
  (define-key global-map "\C-cc" 'org-capture)

  ;; http://storax.github.io/blog/2016/05/02/org-capture-tricks/
  ;; id:2016-05-05-yasnippet-like-capture-templates
  ;; example:  * JIRA Ticket %(my-capture-promt "JIRA Project" 'jr-prj)-%(my-capture-promt "JIRA Ticket No." 'jr-no)
  ;; example:  For project %(my-capture-insert 'jr-prj) I have to do the following stuff:
  ;; example:  %(my-capture-optional "triage" (format
  ;; example:   "** Triage %s-%s
  ;; example:  Do the triage."
  ;; example:   jr-prj jr-no))
  ;; example:  %(my-capture-optional "implementation" (format
  ;; example:   "** Implement %s-%s
  ;; example:  Implement stuff
  ;; example:  - [ ] Tests pass?"
  ;; example:   jr-prj jr-no))
  (defvar my-capture-promt-history nil
    "History of prompt answers for org capture.")
  (defun my-capture-prompt (prompt variable)
    "PROMPT for string, save it to VARIABLE and insert it."
    (make-local-variable variable)
    (set variable (read-string (concat prompt ": ") nil my-capture-promt-history)))
  (defun my-capture-prompt-date (prompt variable)
    "PROMPT for a date, save it to VARIABLE and insert it."
    (make-local-variable variable)
    (set variable
         (format-time-string
          (org-time-stamp-format nil nil)
          (org-read-date nil t nil prompt))
         ))
  (defun my-capture-insert (variable)
    "Insert content of VARIABLE."
    (symbol-value variable))
  (defun my-capture-optional (what text)
    "Ask user to include WHAT.  If user agrees return TEXT."
    (when (y-or-n-p (concat "Include " what "?"))
      text))
  (defun my-capture-selection (list variable)
    "Let the user choose between a pre-defined set of strings"
    (make-local-variable variable)
    (let ((selected-value (ido-completing-read "Select from list: " list)))
      (set variable selected-value)
      selected-value)
    )

  ;; capture templates:
  (setq my-capture-template-next "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
  (setq my-capture-template-r6story "** TODO [[IPD:%(my-capture-prompt \"IPD number\" 'my-ipd)]] %(my-capture-prompt \"Story title\" 'my-title) [1/11]                         :US_%(my-capture-prompt \"Short title\" 'my-short-title):
:PROPERTIES:
:CREATED:  [%<%Y-%m-%d %a %H:%M>]
:ID: %(format-time-string \"%Y-%m-%d\")-Story-%(my-capture-insert 'my-short-title)
:END:

| *IPD* | *Confluence* | *Champ* |
| [[IPD:%(my-capture-insert 'my-ipd)][%(my-capture-insert 'my-ipd)]]  | %(my-capture-insert 'my-title) |     |

*** NEXT create Jira [[IPD:%(my-capture-insert 'my-ipd)]]
:PROPERTIES:
:CREATED:  [%<%Y-%m-%d %a %H:%M>]
:ID:    %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-create-jira-ipd
:BLOCKER:
:TRIGGER:  %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-define-champ(NEXT) %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-get-estimation(NEXT)
:END:

- fill out:
  - [ ] set reporter
  - [ ] set level red
  - [ ] fixVersion

*** NEXT create Confluence page with template
SCHEDULED: <%(format-time-string \"%Y-%m-%d\")>
:PROPERTIES:
:CREATED:  [%<%Y-%m-%d %a %H:%M>]
:ID:    %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-create-confluence-page
:BLOCKER:
:TRIGGER:  %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-write-acceptance-criteria(NEXT)
:END:

- fill out:
  - [ ] add Jira-Link [[IPD:%(my-capture-insert 'my-ipd)]]
  - [ ] PO
  - [ ] Title
  - [ ] Business Value
- [ ] add Confluence-short-URL to story table above

*** TODO write Acceptance Criteria, Docu, Perms
:PROPERTIES:
:CREATED:  [%<%Y-%m-%d %a %H:%M>]
:ID: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-write-acceptance-criteria
:BLOCKER: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-create-confluence-page
:TRIGGER: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-confidence-green(NEXT) %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-hand-over-team(NEXT)
:END:

*** TODO add Champ to Confluence and Jira                                    :refinement:
:PROPERTIES:
:CATEGORY: refinement
:CREATED:  [%<%Y-%m-%d %a %H:%M>]
:ID: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-define-champ
:BLOCKER:
:END:

*** TODO get Estimation on [[IPD:%(my-capture-insert 'my-ipd)]]                                           :refinement:
:PROPERTIES:
:CREATED:  [%<%Y-%m-%d %a %H:%M>]
:CATEGORY: refinement
:ID: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-estimation
:BLOCKER: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-create-jira-ipd
:TRIGGER:
:END:

- Estimation:

*** TODO get confidence-level green on [[IPD:%(my-capture-insert 'my-ipd)]]                                :refinement:
:PROPERTIES:
:CATEGORY: refinement
:CREATED:  [%<%Y-%m-%d %a %H:%M>]
:ID: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-confidence-green
:BLOCKER: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-write-acceptance-criteria %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-estimation
:TRIGGER:
:END:

*** TODO hand over to team
:PROPERTIES:
:CREATED:  [%<%Y-%m-%d %a %H:%M>]
:BLOCKER: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-write-acceptance-criteria %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-estimation
:ID: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-hand-over-team
:TRIGGER:  %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-accept(WAITING)
:END:

*** acceptance + finish US
:PROPERTIES:
:CREATED:  [%<%Y-%m-%d %a %H:%M>]
:ID: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-accept
:BLOCKER: %(format-time-string \"%Y-%m-%d\")-%(my-capture-insert 'my-short-title)-hand-over-team
:END:
")

  (setq my-capture-template-kabarett "
** Kabarettabendorga: %(my-capture-prompt \"ISO-Datum Veranstaltung\" 'my-event-date) %(my-capture-prompt \"Künstler\" 'my-artist)                   :%(my-capture-insert 'my-artist):Kabarett:
:PROPERTIES:
:ID: %(my-capture-insert 'my-event-date)-y-Kabarett
:END:

- Titel                   : %(my-capture-prompt \"Programmname\" 'my-programname)

- %(my-capture-prompt \"Anzahl der reservierten Plätze\" 'my-num-seats) Plätze gesamt
  - 2: Kathi und ich
  - 2:
  - 2:
  - 2:

*** NEXT Email: %(my-capture-insert 'my-num-seats)-2 Plätze noch zu vergeben
SCHEDULED: <%(my-capture-prompt \"ISO-Datum Email-Aussendung\" 'my-email-date)>
:PROPERTIES:
:ID: %(my-capture-insert 'my-event-date)-k-email1
:END:

- Email an \"kabarettinteressierte\" (mutt-alias)
- Eintragen in Liste oben

#+BEGIN_QUOTE
Kabarett: %(my-capture-insert 'my-artist) am %(my-capture-insert 'my-event-date)

Hallo Kabarettfreunde!

Es ist wieder soweit:

  Wer:    %(my-capture-insert 'my-artist)
  Was:    \"%(my-capture-insert 'my-programname)\"
  Wann:   %(my-capture-insert 'my-event-date) 19:15 Abendkassa
  Wo:     %(my-capture-selection '(\"Theatercafé\" \"Orpheum\" \"FIXXME\") 'my-location)

Wer zuerst sich anmeldet, der bekommt die reservierten Karten. Mein
Kontingent ist dieses Mal begrenzt auf %(my-capture-insert 'my-num-seats) Karten.

	  Ich bitte um verbindliche Zusagen per Email.

Wie üblich: reservierte Karten sind an der Abendkasse selber abzuholen
und zu bezahlen. Reserviert wurde auf »Karl Voit«. Vergünstigungen
bekommst du mit AKCard, Studentenausweis (Alterslimit!) oder (nur für
den *Eintritt* ins Theatercafe) bis zu 20% durch die Kabanussis.

Wir freuen uns!



Wenn du in Zukunft keine solchen Emails mehr von mir bekommen
möchtest, so schick' mir bitte ein Email. Du wirst dann von der
Kabarett-Emailliste entfernt.

#+END_QUOTE

*** NEXT Kabarett-Reminder an alle angemeldeten
SCHEDULED: <%(my-capture-prompt \"ISO-Datum Erinnerungsemail\" 'my-reminder-date)>
:PROPERTIES:
:BLOCKER: %(my-capture-insert 'my-event-date)-k-email1
:ID: %(my-capture-insert 'my-event-date)-k-email2
:END:

#+BEGIN_QUOTE
Erinnerung: Kabarett %(my-capture-insert 'my-artist) am %(my-capture-insert 'my-event-date)

Hallo Kabarettinteressierter!

Du hast dich bei mir für das kommende Kabarett angemeldet:

  Wer:    %(my-capture-insert 'my-artist)
  Was:    \"%(my-capture-insert 'my-programname)\"
  Wann:   %(my-capture-insert 'my-event-date) 19:15 Abendkassa
  Wo:     %(my-capture-insert 'my-location)

Wie üblich: reservierte Karten sind an der Abendkasse selber abzuholen
und zu bezahlen. Reserviert wurde auf »Karl Voit«. Vergünstigungen
bekommst du mit AKCard, Studentenausweis (Alterslimit!) oder (nur für
den *Eintritt* ins Theatercafe) bis zu 20% durch die Kabanussis.

Wir freuen uns!

#+END_QUOTE

*** <%(my-capture-insert 'my-event-date) 19:00-20:00> Kabarett-Vorglühen              :@Stadt:
:PROPERTIES:
:ID: %(my-capture-insert 'my-event-date)-kabarettvorgluehen
:END:

*** <%(my-capture-insert 'my-event-date) 20:00-23:30> %(my-capture-insert 'my-artist): \"%(my-capture-insert 'my-programname)\" (%(my-capture-insert 'my-location), DND) :@Stadt:
:PROPERTIES:
:ID: %(my-capture-insert 'my-event-date)-kabarettabend
:END:

- http://members.aon.at/hinwider/S.4_Kuenstler.htm
#+BEGIN_QUOTE
#+END_QUOTE
")


  (setq my-capture-template-contact "** %(my-capture-prompt \"Vorname\" 'my-firstname) %(my-capture-prompt \"Nachname\" 'my-lastname)     :%(my-capture-insert 'my-firstname)%(my-capture-insert 'my-lastname):
:PROPERTIES:
:TYPE: %(my-capture-selection '(\"person\" \"company\") 'my-person-or-company)
:TITLE:
:EMAIL: %(my-capture-prompt \"Email\" 'my-email)
:URL:
:MOBILE: 0043/
:HOMEPHONE:
:WORKPHONE:
:PHONE:
:COMPANY:
:STREET:
:POSTALCODE:
:CITY:
:COUNTRY: Österreich
:PHOTOGRAPH: [[photo:%(my-capture-insert 'my-firstname)%(my-capture-insert 'my-lastname).jpg]]
:BORN:
:ITOLDTHEM_EMAIL:
:ITOLDTHEM_ADDRESS:
:ITOLDTHEM_PHONE:
:ADDRESS_CHANGE_METHOD:
:END:

- Erstkontakt:

\n\n")

  (setq my-capture-template-releasenotes "***** NEXT [#A] Release Notes Sprint 16-%(my-capture-prompt \"Sprint\" 'my-sprint) [0/7]
SCHEDULED: %(my-capture-prompt-date \"Friday\" 'my-friday)
:PROPERTIES:
:NOTordered: t
:END:

| Release   | [[https://product.infonova.at/jira/plugins/servlet/project-config/IPD/versions][Successor created]] | [[https://product.infonova.at/jira/plugins/servlet/project-config/IPD/versions][Jira Released]] | [[https://product.infonova.at/jira/browse/IPD/?selectedTab=com.atlassian.jira.jira-projects-plugin:versions-panel][Notes done]] | URL sent | Notes |
|-----------+-------------------+---------------+------------+----------+-------|

****** Status

#+BEGIN_SRC sh :results output :wrap quote
c:/Users/karl.voit/src/jira-defect-analysis.py/jira-defect-analysis.py
#+END_SRC

****** NEXT Scrum-Master: Defects bitte closen
SCHEDULED: %(my-capture-prompt-date \"Wednesday\" 'my-wednesday)
:PROPERTIES:
:ID: release-notes-process-2016-%(my-capture-insert 'my-sprint)-defects-closed
:END:

****** NEXT Create Jira Successor
SCHEDULED: %(my-capture-prompt-date \"Thursday\" 'my-thursday)
:PROPERTIES:
:ID: release-notes-process-2016-%(my-capture-insert 'my-sprint)-create-jira-release-successors
:END:

****** NEXT [#A] Beno bescheid geben, wenn alle Defects auf closed sind
SCHEDULED: %(my-capture-insert 'my-thursday)
:PROPERTIES:
:ID: release-notes-process-2016-%(my-capture-insert 'my-sprint)-beno-notified
:END:

****** NEXT Tasks: Fixversion löschen; Stories: nicht in Service Packs
SCHEDULED: %(my-capture-insert 'my-thursday)
:PROPERTIES:
:ID: release-notes-process-2016-%(my-capture-insert 'my-sprint)-tasks-and-stories
:END:

****** NEXT Release Notes erstellen
SCHEDULED: %(my-capture-insert 'my-friday)
:PROPERTIES:
:ID: release-notes-process-2016-%(my-capture-insert 'my-sprint)-release-notes-created
:BLOCKER: release-notes-process-2016-%(my-capture-insert 'my-sprint)-beno-notified
:END:

https://product.infonova.at/jira/browse/IPD/?selectedTab=com.atlassian.jira.jira-projects-plugin:versions-panel

- replace-regexp
  : &create.*

****** NEXT Release Note-URLs verschicken
SCHEDULED: <%(my-capture-insert 'my-friday)>
:PROPERTIES:
:ID: release-notes-process-2016-%(my-capture-insert 'my-sprint)-release-notes-sent
:BLOCKER: release-notes-process-2016-%(my-capture-insert 'my-sprint)-release-notes-created
:END:

- [[mailto:BPt-DL AT GRZ R6Teams <ise.r6teams@bearingpoint.com>; BPt-DL AT GRZ PM Team <pmteam@infonova.com>; BPt-DL AT R6 Release and Deployment Management <r6rdm@infonova.com>; BPt-FM Infonova R6 Support <support@infonova.com>; Brantner, Stefan <stefan.brantner@infonova.com>; Koerner, Christian <Christian.koerner@infonova.com>; Laber, Elke <Elke.laber@infonova.com>; Voit, Karl <Karl.voit@infonova.com>; Lipp, Hannes <hannes.lipp@infonova.com>; Poetz, Patrick <Patrick.poetz@infonova.com>; Kleva, Nusa <nusa.kleva@infonova.com>; Zurman, Beno <Beno.zurman@infonova.com>; Gaisbauer, Mansuet <Mansuet.gaisbauer@infonova.com>; Hoeserle, Christian <christian.hoeserle@infonova.com>?subject=Infonova R6 Service Pack Release Notes&body=Here are the Service Pack Release Notes for following releases. Please log in to Jira before accessing the URLs below.]]
- *nicht* die URLs in Emacs pasten, da compose-mode backslash/slash vertauscht

****** NEXT Release-Note-Orgmode-Struktur für nächsten Sprint erstellen (capture)
SCHEDULED: <%(my-capture-insert 'my-friday)>
:PROPERTIES:
:ID: release-notes-process-2016-%(my-capture-insert 'my-sprint)-next-orgmode-struct-done
:END:

")




;;test  (setq org-capture-templates
;;test	`(
;;test	  ("1" "first version with string" entry (file+headline "~/share/all/org-mode/misc.org" "shorts")
;;test	   "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
;;test	  ("2" "second version with variable" entry (file+headline "~/share/all/org-mode/misc.org" "shorts")
;;test	   ,my-capture-template-next :empty-lines 1)
;;test          ))

  ;; ######################################################
  ;; templates:  http://orgmode.org/org.html#Capture-templates
  ;; elements:   http://orgmode.org/org.html#Template-elements
  (setq org-capture-templates
	`(
	  ("s" "shorts-todo" entry (file+headline "~/share/all/org-mode/misc.org" "shorts")
	   ,my-capture-template-next :empty-lines 1)
	  ("e" "Event" entry (file+headline "~/share/all/org-mode/misc.org" "Events")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("k" "Kabarett" entry (file+headline "~/share/all/org-mode/misc.org" "Events")
	   ,my-capture-template-kabarett :empty-lines 1)
	  ("b" "Bookmark" entry (file+headline "~/share/all/org-mode/notes.org" "Bookmarks")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("c" "Contact" entry (file+headline "~/share/all/org-mode/contacts.org" "Inbox")
	   ,my-capture-template-contact :empty-lines 1)
	   ;;(file "~/.emacs.d/data/capture_contact.org") :empty-lines 1)
	  ("p" "public voit" entry (file+headline "~/share/all/org-mode/public_voit.org" "Blogbeiträge")
	   "* NEXT %?        :blog:%^g\n:PROPERTIES:\n:CREATED: %U\n:ID: %^{prompt}\n:END:\n\n" :empty-lines 1)
	  ("a" "anzuschauen" entry (file+headline "~/share/all/org-mode/misc.org" "Anzuschauen")
	   "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%x\n\n" :empty-lines 1)
	  ("h" "hardware")
	  ("hs" "sherri" entry (file+olp "~/share/all/org-mode/hardware.org" "Inventar" "intel NUC (<2015-07-25 Sat>, € 486.84, e-tec)" "shorts")
	   ,my-capture-template-next :empty-lines 1)
	  ("hg" "gary" entry (file+olp "~/share/all/org-mode/hardware.org" "Inventar" "lenovo X200s (IST, 2009-01-??)" "shorts")
	   ,my-capture-template-next :empty-lines 1)
	  ("hc" "Chromebook" entry (file+olp "~/share/all/org-mode/hardware.org" "Inventar" "Toshiba Chromebook 2 (<2016-03-14 Mon>, 334,02€, Amazon)" "shorts")
	   ,my-capture-template-next :empty-lines 1)
	  ("hp" "RasPlay" entry (file+olp "~/share/all/org-mode/hardware.org" "Inventar" "Raspberry Pi 2 Model B (<2015-06-29 Mon>, 38€, Pollin.de)")
	   ,my-capture-template-next :empty-lines 1)
	  ;;old;;("hb" "blanche" entry (file+olp "~/share/all/org-mode/hardware.org" "Inventar" "Mac Mini mit OS X 10.5 (2009-0?0??)" "shorts")
	  ;;old;; "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("hw" "Winora T3" entry (file+olp "~/share/all/org-mode/hardware.org" "Inventar" "Fahrrad: Winora T3 ([[contact:Kotnik][Kotnik]], 2464€, <2013-08-02 Fri>)")
	   ,my-capture-template-next :empty-lines 1)
	  ("w" "Breitenweg")
	  ("ws" "Breitenweg shorts" entry (file+headline "~/share/all/org-mode/bwg.org" "shorts")
	   ,my-capture-template-next :empty-lines 1)
	  ("we" "Breitenweg event" entry (file+headline "~/share/all/org-mode/bwg.org" "Events")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;old;;("wa" "Breitenweg Ausgaben" table-line (file+headline "~/share/all/org-mode/bwg.org" "getätigte Ausgaben") "| %t | %? ||||")
	  ("i" "infonova Templates")
	  ("is" "infonova shorts" entry (file+headline "~/share/all/org-mode/infonova.org" "shorts")
	   ,my-capture-template-next :empty-lines 1)
	  ("in" "infonova Release Notes" entry (file+headline "~/share/all/org-mode/infonova.org" "shorts")
	   ,my-capture-template-releasenotes :empty-lines 1)
	  ("ie" "infonova event" entry (file+headline "~/share/all/org-mode/infonova.org" "Events")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("ir" "r6 Templates")
	  ("irs" "R6 shorts" entry (file+headline "~/share/all/org-mode/r6-stories.org" "shorts")
	   ,my-capture-template-next :empty-lines 1)
	  ("iru" "Userstory" entry (file+headline "~/share/all/org-mode/r6-stories.org" "New Stories")
	   ,my-capture-template-r6story :empty-lines 1)
	  ("f" "FH St. Pölten")
	  ("fs" "FH shorts" entry (file+headline "~/share/all/org-mode/fhsp.org" "shorts")
	   ,my-capture-template-next :empty-lines 1)
	  ("fe" "FH event" entry (file+headline "~/share/all/org-mode/fhsp.org" "Events")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;("ii" "infonova IPD" entry (file+headline "~/share/all/org-mode/infonova.org" "IPDs")
	  ;; "* IPD-%?: \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;("p" "PhD Templates")
	  ;;("ps" "PhD shorts" entry (file+headline "~/share/all/org-mode/phd.org" "shorts")
	  ;; ,my-capture-template-next :empty-lines 1)
	  ;;("pe" "PhD event" entry (file+headline "~/share/all/org-mode/phd.org" "Events")
	  ;; "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;("t" "tagstore Templates")
	  ;;("ts" "tagstore shorts" entry (file+headline "~/share/all/org-mode/tagstore.org" "shorts")
	  ;; ,my-capture-template-next :empty-lines 1)
	  ;;("te" "tagstore event" entry (file+headline "~/share/all/org-mode/tagstore.org" "Events")
	  ;; "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("B" "Besorgung" entry (file+headline "~/share/all/org-mode/hardware.org" "Besorgungen")
	   ,my-capture-template-next :empty-lines 1)
	  ;;("C" "Clipboard" entry (file+headline "~/share/all/org-mode/misc.org" "shorts")
	  ;; "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%x\n\n" :empty-lines 1)
	  ("I" "inbox, refile later" entry (file "~/share/all/org-mode/inbox.org")
	   "\n* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;("m" "movie" entry (file+headline "~/share/all/org-mode/movies.org" "inbox")
	  ;; "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ;;old;;("x" "xlog")
	  ;;old;;("xf" "xlog FNS" table-line (id "xlog-fns-id") "| %T |")
	  ;;old;;("xz" "xlog ZNS" table-line (id "xlog-zns-id") "| %T |")
	  ;;old;;("xh" "xlog hometrainer" table-line (id "xlog-hometrainer") "| %T |  |  |  |")
	  ;;old;;("xb" "xlog Bettwäsche" table-line (id "xlog-bettwaesche-id") "| %T |  |  |  |  |")
	  ;;old;;("xg" "xlog Gewicht" table-line (id "xlog-gewicht") "| %T |  |")
	  ;;old;;("xr" "Reinigung Geschirrspüler" table-line (id "xlog-Geschirrspuelerreinigung") "| %T |")
	  ;;old;;("xp" "Pollenallergie Symptome" table-line (id "ad5f7363-e280-4566-912d-1fb5699725da") "| %T |")
	  ;;old;;("xt" "Pollenallergie Tabletteneinnahme" table-line (id "b718be27a93a35207bac9b18ec390cc3") "| %T |")
	  ;;old;;("xG" "elmex grün" table-line (id "d705cdf9-40e5-4677-9662-e0e17d05798f") "| %T |")
	  ;;old;;("xR" "elmex rot" table-line (id "fbd9be0e-5077-4ba9-89eb-6041f945991a") "| %T |")
	  ;;old;;("xD" "dentalux Complex 3" table-line (id "2013-08-06-detalux3") "| %T |")
	  ;;old;;("xk" "Keyboard Akkus leer" table-line (id "3407c9b7-1b41-443b-9254-32c4af3a54e8") "| %T |")
	  ;;old;;("xx" "xlogtest" table-line (file+headline "~/share/all/org-mode/misc.org" "xlogtest2012-06-17") "| %T |")
	  )
	)


;;** org-tag-alist
  ;; Tags with fast selection keys
  ;; http://orgmode.org/org.html#Setting-tags
  (setq org-tag-alist (quote (
			      ;;("Kommunikation" . ?k)
			      ("Besorgung" . ?B)
			      ("nonComputer" . ?n)
			      ("fitness" . ?f)
			      (:startgroup)
			      ("@ALW" . ?a)
			      ("@BWG" . ?b)
			      ;;("@Infonova" . ?i)
			      ;;("@out_of_town" . ?o)
			      ;;("@FHStP" . ?F)
                              ;;("@Ebreichsdorf" . ?e)
                              ;;("@TUG" . ?t)
			      (:endgroup)
			      (:startgroup)
			      ("private" . ?p)
			      ("public" . ?P)
			      (:endgroup)
			      (:startgroup)
			      ;;("bigRock" . ?b)
			      ("MIT" . ?m)
			      ("lp" . ?l)
			      ("reward" . ?r)
			      (:endgroup)
			      )))

  ;; ######################################################
  ;; Allow setting single tags without the menu
  ;; http://orgmode.org/org.html#Setting-tags
  ;;(setq org-fast-tag-selection-single-key (quote expert))



  ;; ######################################################
;;** babel

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
     (sql . t)
     (restclient . t)
     ))

  ;; Inhibit evaluation of code blocks during export
  ;; http://orgmode.org/manual/Exporting-code-blocks.html
  (setq org-export-babel-evaluate nil)

  ;; Do not prompt to confirm evaluation
  ;; This may be dangerous - make sure you understand the consequences
  ;; of setting this -- see the docstring for details
  (setq org-confirm-babel-evaluate nil)

  ;; http://orgmode.org/manual/Sparse-trees.html#index-org_002dshow_002dentry_002dbelow-179
  (setq org-show-entry-below (quote ((default))))

  ;; see id:2014-12-21-org-screen
  ;;(require 'org-screen)
  ;;
  ;;(require 'ob-screen)
  ;;(defvar org-babel-default-header-args:screen
  ;;  '(
  ;;    (:results . "silent")
  ;;    (:session . "default")
  ;;    (:cmd . "/bin/zsh")
  ;;    (:terminal . "/usr/bin/gnome-terminal"))
  ;;  "Default arguments to use when running screen source blocks.")

  ;; http://kitchingroup.cheme.cmu.edu/blog/2014/12/21/Capturing-stderr-from-Python-in-org-mode-take-2/
  ;;2016-04-08;doesnt work; (if (my-system-type-is-gnu)
  ;;2016-04-08;doesnt work;     ;; does not seem to work with Windows:
  ;;2016-04-08;doesnt work;     (setq org-babel-python-command "python -i -c \"import sys; sys.stderr = sys.stdout\"")
  ;;2016-04-08;doesnt work;     (setq org-babel-python-command "python")
  ;;2016-04-08;doesnt work;     )

  ;; id:2015-01-11-redirect-org-babel-sh-stderr-to-stdout
  ;; (if (my-system-type-is-gnu)
  ;;   ;outdated: org-babel-sh-command was removed with org-mode v8.3:
  ;;   ;outdated;  (setq org-babel-sh-command
  ;;   ;outdated;        "~/bin/zsh_stderr_redirected_to_stdout.sh")
  ;;   (setq shell-file-name "~/.emacs.d/data/zsh_stderr_redirected_to_stdout.sh");; id:2015-01-11-redirect-org-babel-sh-stderr-to-stdout
  ;;   )

;;** habits
  ;; ######################################################
  ;; global STYLE property values for completion
  (setq org-global-properties (quote (("STYLE_ALL" . "habit"))))
  ;; position the habit graph on the agenda to the right of the default
  (setq org-habit-graph-column 50)


;;** org-crypt

  (when (my-system-type-is-gnu)
    (require 'org-crypt)

    ;; Encrypt all entries before saving
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    ;; GPG key to use for encryption
    (setq org-crypt-key "8A614641")


    ;; ######################################################
    ;; encrypting whole files:
    ;; http://orgmode.org/worg/org-tutorials/encrypting-files.html
    (require 'epa-file)
    ;;(epa-file-enable) ;; 2013-08-22: already enabled (somewhere)


    ;; ######################################################
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
                  auto-mode-alist)
          )

    ;; do not ask for disabling auto-save
    (setq org-crypt-disable-auto-save nil)

    )



  ;; ######################################################
;;** Org-Mobile
  ;; http://orgmode.org/org.html#MobileOrg
  ;; directory where to store MobileOrg-files
  (setq org-directory "~/share/all/org-mode")
  (setq org-mobile-directory "~/share/all/org-mode/mobile-org/")
  (setq org-mobile-inbox-for-pull "~/share/all/org-mode/inbox.org")
  (setq org-mobile-force-id-on-agenda-items nil) ;; do not generate IDs for all headings: http://orgmode.org/manual/Pushing-to-MobileOrg.html
  ;; not needed ;; (setq org-mobile-files (quote
  ;; not needed ;; 			  (
  ;; not needed ;; 			   "~/share/all/org-mode/misc.org"
  ;; not needed ;; 			   "~/share/all/org-mode/contacts.org"
  ;; not needed ;; 			   "~/share/all/org-mode/hardware.org"
  ;; not needed ;; 			   )))
  (defun my-mobile-org-import ()
    "Imports mobile-org-data from mobile-org to inbox.org and openes inbox.org"
    (interactive "P")
    (org-mobile-pull)
    (find-file "~/org/inbox.org")
    )


  ;; ######################################################
;;** preserve top level node and tags when archiving
  ;; http://orgmode.org/worg/org-hacks.html#sec-1-7-1
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


;;** org-link
  (setq org-link-abbrev-alist
	'(
	  ("bib" . "~/archive/library/%s.bib")
	  ("cite" . "file:~/share/all/org-mode/references.org::/%s/")
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

  ;; tries to format a new BibTeX entry according to own format
  ;; from: http://www.mfasold.net/blog/2009/02/using-emacs-org-mode-to-draft-papers/
  ;;(my-load-local-el "contrib/format-bib.el")
  ;; does not work well enough

  ;; http://superuser.com/questions/71786/can-i-create-a-link-to-a-specific-email-message-in-outlook
  ;; id:2016-05-03-outlook-links
  (org-add-link-type "outlook" 'org-outlook-open)

  (defun org-outlook-open (id)
    "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
    (w32-shell-execute "open" (concat "outlook:" id)))


  ;; ######################################################

;;** iCal
;;*** iCal -> Org (disabled)
  ;;disabled;; ;; ######################################################
  ;;disabled;; ;; import iCal to Org-mode
  ;;disabled;; ;; http://ozymandias.dk/emacs/org-import-calendar.el
  ;;disabled;; ;; https://raw.github.com/vjohansen/emacs-config/master/org-import-calendar.el
  ;;disabled;; (my-load-local-el "contrib/org-import-calendar.el")
  ;;disabled;; (use-package org-import-icalendar)



  ;; ######################################################
;;*** Austrian Holidays
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
    ;;(setq holiday-other-holidays '((holiday-fixed 10  3 "Tag der Deutschen Einheit")))
    (setq holiday-local-holidays holiday-austria-holidays)
    (setq calendar-holidays (append holiday-local-holidays holiday-other-holidays))
    ;; and add (load "~/Notes/holidays" t) to your .emacs and add
    ;; #+CATEGORY: Feiertag
    ;; %%(org-calendar-holiday)
    ;; to an agenda file
    ;; ######################################################
    ;; Muttertag... from http://debianforum.de/forum/viewtopic.php?f=29&t=67024


  ;; ######################################################
;;*** org-agenda-exporter-settings
  ;; customizing the agenda export of C-x C-w
  ;; http://orgmode.org/manual/Exporting-Agenda-Views.html
  (setq org-agenda-exporter-settings
	'((ps-number-of-columns 2)
	  (ps-landscape-mode t)
	  ;;disabled;                  (org-agenda-add-entry-text-maxlines 5)
	  (htmlize-output-type 'css)))

  ;; ######################################################
;;*** time-zone for iCal
  ;; setting timezone in order to get a correct iCal export
  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00134.html
  (setq org-icalendar-timezone "Europe/Vienna")

  ;; ######################################################
;;*** setting destination file for iCal export
  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00163.html
  ;; http://orgmode.org/worg/org-tutorials/org-google-sync.html
  ;;disabled; does not work
  ;;disabled;(setq org-combined-agenda-icalendar-file "~/public_html/orgmodevk478.ics")

;;*** org-mycal-export-limit (disabled)
  ;; ######################################################
  ;;disabled;; ;; define filter. The filter is called on each entry in the agenda.
  ;;disabled;; ;; It defines a regexp to search for two timestamps, gets the start
  ;;disabled;; ;; and end point of the entry and does a regexp search. It also
  ;;disabled;; ;; checks if the category of the entry is in an exclude list and
  ;;disabled;; ;; returns either t or nil to skip or include the entry.
  ;;disabled;; (defun org-mycal-export-limit ()
  ;;disabled;;   "Limit the export to items that have a date, time and a range. Also exclude certain categories."
  ;;disabled;;   (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\\)>")
  ;;disabled;;   (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
  ;;disabled;;   (save-excursion
  ;;disabled;;     ;; get categories
  ;;disabled;;     (setq mycategory (org-get-category))
  ;;disabled;;     ;; get start and end of tree
  ;;disabled;;     (org-back-to-heading t)
  ;;disabled;;     (setq mystart    (point))
  ;;disabled;;     (org-end-of-subtree)
  ;;disabled;;     (setq myend      (point))
  ;;disabled;;     (goto-char mystart)
  ;;disabled;;     ;; search for timerange
  ;;disabled;;     (setq myresult (re-search-forward org-tstr-regexp myend t))
  ;;disabled;;     ;; search for categories to exclude
  ;;disabled;;     (setq mycatp (member mycategory org-export-exclude-category))
  ;;disabled;;     ;; return t if ok, nil when not ok
  ;;disabled;;     (if (and myresult (not mycatp)) t nil)))
  ;;disabled;; ;; activate filter and call export function
  ;;disabled;; (defun org-mycal-export ()
  ;;disabled;;   (let ((org-icalendar-verify-function 'org-mycal-export-limit))
  ;;disabled;;     (org-export-icalendar-combine-agenda-files)))


  ;; ######################################################
;;*** org-mode export of calendar events to Google
  ;; http://orgmode.org/worg/org-tutorials/org-google-sync.html

;;*** define categories that should be excluded
  (setq org-export-exclude-category (list "google" "private"))


  ;; ######################################################
;;*** ignore :noexport: entries in iCal-export
  ;; http://comments.gmane.org/gmane.emacs.orgmode/36415
  (setq org-icalendar-honor-noexport-tag t)

  ;; ######################################################
;;*** add tags to iCal-export
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


  ;; ;; ######################################################
;;** using alternative LaTeX exporter (disabled)
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



;;** LaTeX-classes: scrartcl, scrartclsmall, ...

  ;; ######################################################
  ;; http://orgmode.org/org.html#Header-and-sectioning
  ;; customized LaTeX class
  (add-to-list 'org-latex-classes
               '("scrartcl"
                 "\\documentclass\[a4paper,parskip=half\]\{scrartcl\}"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
  ;; 2014-10-16: does NOT work yet. Most probably because savetrees does not work well with inputenc
  (add-to-list 'org-latex-classes
               '("scrartclsmall"
                 "\\documentclass\[a4paper,parskip=half\]\{scrartcl\}\\usepackage\[savetrees\]"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))


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


;;** images

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
;;** org-feed RSS (disabled)
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
;;** Org-mode docu
  (autoload 'info "info.el")
  (add-to-list 'Info-additional-directory-list "~/.emacs.d/contrib/org-mode/doc/")


;;** Reference handling (Reftex)

  ;; Default bibliography
  (setq reftex-default-bibliography '("~/archive/library/references.bib"))

  ;; ######################################################
  ;; http://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/
  ;; org-mode and paper references

  (defadvice reftex-format-citation (before eval-citation-format)
    (setq format (eval format)))

  (defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name) (file-exists-p (buffer-file-name))
	 (progn
           ;;enable auto-revert-mode to update reftex when bibtex file changes on disk
	   (global-auto-revert-mode t)
	   (reftex-parse-all)
           ;;add a custom reftex cite format to insert links
	   (reftex-set-cite-format
	    '((?b . "[[bib:%l][%l.bib]]")
	      (?c . "[[cite:%l][%l]]")
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

  ;;disabled 2015-05-14 - double code from above?;; ;; http://orgmode.org/worg/org-faq.html#using-reftex-in-org-mode
  ;;disabled 2015-05-14 - double code from above?;; (defun org-mode-reftex-setup ()
  ;;disabled 2015-05-14 - double code from above?;;   (load-library "reftex")
  ;;disabled 2015-05-14 - double code from above?;;   (and (buffer-file-name)
  ;;disabled 2015-05-14 - double code from above?;;        (file-exists-p (buffer-file-name))
  ;;disabled 2015-05-14 - double code from above?;;        (reftex-parse-all))
  ;;disabled 2015-05-14 - double code from above?;;   (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
  ;;disabled 2015-05-14 - see id:2015-05-14-disable-orgmode-reftex-autoload;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)

  ;;disabled 2015-05-14 - I don't use CHECK_NEEDED;;(add-hook 'org-mode-hook
  ;;disabled 2015-05-14 - I don't use CHECK_NEEDED;;          (lambda ()
  ;;disabled 2015-05-14 - I don't use CHECK_NEEDED;;            (if (member "CHECK_NEEDED" org-todo-keywords-1)
  ;;disabled 2015-05-14 - I don't use CHECK_NEEDED;;		  (org-mode-reftex-setup))))

  (defun org-mode-reftex-search ()
    ;;jump to the notes for the paper pointed to at from reftex search
    (interactive)
    (org-open-link-from-string (format "[[cite:%s]]" (reftex-citation t))))


;;** calculating with dates and times

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


;;** my-lazyblorg-test
  (if (my-system-type-is-gnu)
      (defun my-lazyblorg-test()
	"Saves current blog entry to file and invoke lazyblorg process with it"
	(interactive)
	(save-excursion
	  (search-backward ":blog:");; search begin of current (previous) blog entry
	  (beginning-of-line nil)
	  (set-mark-command nil);; set mark
	  (org-cycle);; close org-mode heading and sub-headings
	  (next-line);; goto next org-mode heading (this should be next line after blog entry)
	  (beginning-of-line nil)
	  (let ((p (point));; copy region
		(m (mark)))
	    (if (< p m)
		(kill-ring-save p m)
	      (kill-ring-save m p)))
	  (find-file "/tmp/lazyblorg-preview.org");; fixed temporary file (will be overwritten)
	  (erase-buffer);; I told you!
	  (yank);; paste region from above
	  (save-buffer);; save to disk
	  (kill-buffer "lazyblorg-preview.org");; destroy last evidence
	  (previous-line);;
	  (org-cycle);; close org-mode heading and sub-headings
	  ;; invoke lazyblorg:
	  (shell-command-to-string "/home/vk/src/lazyblorg/preview_blogentry.sh");; invoke shell script
	  )
	)
    )

;;** orgaggregate
  ;; https://github.com/tbanel/orgaggregate
  (my-load-local-el "contrib/orgaggregate/orgtbl-aggregate.el")
  (my-load-local-el "contrib/orgaggregate/org-insert-dblock.el")

;;** org-bullets

  ;; nice looking bullets for headings
  (use-package org-bullets
    ;; :disabled t
    :ensure t
    ;;:if (or (my-system-is-gary-or-sherri) (my-system-is-powerplantlinux))
    :config ;; executed after loading package
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )


  );; end-of-Org-mode

(message "############### DEBUG: config orgmode finished.")

;; #############################################################################
;;* misc modes/packages

;; #############################################################################
;;** post-mode (disabled)
;;disabled;; (setq post-variable-signature-source "~/daten/nobackup/funnies/good_sigs/allsigs.txt")
;;disabled;; (setq post-signature-directory "~/daten/nobackup/funnies/good_sigs/")




;; #############################################################################
;;** MiniMap
;; http://www.emacswiki.org/emacs/MiniMap
;; MiniMap for Emacs
;;deactivated;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/minimap"))
;;deactivated;; ;;(autoload 'minimap "minimap.el")
;;deactivated;; (use-package minimap.el)
;;deactivated;; (setq minimap-window-location 'right)
(use-package minimap
  :disabled t
  :if (my-system-type-is-gnu)
  :ensure t
  :diminish minimap-mode
  :defer 10
  :config ;; executed after loading package
  (setq minimap-window-location 'right)
)



;; #############################################################################
;;** TWiki (disabled)
;; http://www.neilvandyke.org/erin-twiki-emacs/
;; TWiki syntax highlighting
;;disabled; ;(use-package erin))
;;disabled; (my-load-local-el "contrib/erin.el")


;; #############################################################################
;;** Twitter (disabled)


;; ######################################################
;; http://www.emacswiki.org/emacs/TwitteringMode
;; http://citizen428.net/blog/2011/01/21/emacs-twittering-mode/
;;disabled; (add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/twittering-mode/"))
;;disabled; (use-package twittering-mode)
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

;; #############################################################################
;;** UndoTree
;; http://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  ;; :disabled t
  :ensure t
  ;;:if (or (my-system-type-is-gnu) (my-system-type-is-darwin))
  :diminish undo-tree-mode
  :config ;; executed after loading package
  (autoload 'undo-tree "undo-tree.el")
)



;; #############################################################################
;;** open-resource (disabled)
;; http://code.google.com/p/emacs-open-resource/
;;disabled; (add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/emacs-open-resource-read-only"))
;;disabled; (use-package open-resource)
;;disabled; (global-set-key "\C-cr" 'open-resource)

;; #############################################################################
;;** whitespace-mode + style
;; from Twitter 2012-05-22: @emacs_knight
;;(when (or (my-system-type-is-gnu) (my-system-is-blanche))
(whitespace-mode)
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace
  ;;(face trailing lines-tail) whitespace-line-column 80) ;; highlight long lines tails (setq whitespace-style
;;  )

;; #############################################################################
;;** (e)diff
;; ediff from command line
;; http://www.emacswiki.org/emacs/EdiffMode
;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
    ;;    (ediff file1 file2)))
    (ediff-merge-files file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; #############################################################################
;;** counting words
;; http://www.emacswiki.org/emacs/WordCount
;; http://www.emacswiki.org/emacs/wc.el
(my-load-local-el "contrib/wc.el")


;; #############################################################################
;;** magit

(use-package magit
  ;; :disabled t  ;; stop loading if 't'
  :ensure t ;; install package if not found OR: (setq use-package-always-ensure t)
  ;;:if (or (my-system-type-is-gnu) (my-system-is-powerplantlinux))
  ;;  :bind (:map magit-status-mode-map ("q" magit-quit-session))
  :config ;; executed after loading package

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



  ;; #############################################################################
;;** git: highlight regions by last updated time (my-map c)

  ;; smeargle - Highlighting Regions by Last Updated Time
  ;; https://github.com/syohex/emacs-smeargle/
  ;; M-x smeargle  -  Highlight regions by last updated time.
  ;; M-x smeargle-age  -  Highlight regions by age of changes.
  ;; M-x smeargle-clear  - Clear overlays in current buffer
  ;OLD;(my-load-local-el "contrib/emacs-smeargle/smeargle.el")
  ;OLD;(bind-key "c" #'smeargle my-map)
  (use-package smeargle
    ;; :disabled t
    :ensure t
    :defer 10
    :config ;; executed after loading package
    :bind (:map my-map ("c" . smeargle))
  )


  )

;; #############################################################################
;;** recent files
;; http://www.emacswiki.org/emacs-es/RecentFiles
;; recently files
(autoload 'recentf "recentf.el")
(recentf-mode 1)
(setq recentf-max-menu-items 50)


;; #############################################################################
;;** ert (disabled)
;; for using unit tests of yasnippet (see id:2013-02-07yasnippetdebuggen and yasnippet-tests.el)
;;disabled;; (my-load-local-el "contrib/cl-lib.el")
;;disabled;; (my-load-local-el "contrib/ert.el")
;;disabled;; (my-load-local-el "contrib/ert-x.el")


;; #############################################################################
;;** Confluence (disabled)

;;disabled 2015-01-23;; (when (or (my-system-is-powerplantlinux) (my-system-is-powerplantwin))
;;disabled 2015-01-23;;
;;disabled 2015-01-23;;   ;; ######################################################
;;disabled 2015-01-23;;   ;; editing Confluence wiki pages (up to Confluence 3.x)
;;disabled 2015-01-23;;   ;; https://code.google.com/p/confluence-el/
;;disabled 2015-01-23;;   ;; M-x confluence-get-page
;;disabled 2015-01-23;;   ;(add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/confluence-el-1.5/"))
;;disabled 2015-01-23;;   (use-package confluence)
;;disabled 2015-01-23;;   (setq confluence-url "http://product.infonova.at/confluence/rpc/xmlrpc")
;;disabled 2015-01-23;;   (add-to-list 'auto-mode-alist '("\\.\\(confluence\\)$" . confluence-mode))
;;disabled 2015-01-23;;
;;disabled 2015-01-23;;   (dolist (hook '(confluence-mode-hook))
;;disabled 2015-01-23;;     (add-hook hook (lambda ()
;;disabled 2015-01-23;; 		     (flyspell-mode 1)
;;disabled 2015-01-23;; 		     (ispell-change-dictionary "british")
;;disabled 2015-01-23;; 		     (flyspell-buffer)
;;disabled 2015-01-23;; 		     ))
;;disabled 2015-01-23;;     )
;;disabled 2015-01-23;;
;;disabled 2015-01-23;;   (defun vk-open-as-confluence-page ()
;;disabled 2015-01-23;;     "Takes current line (delimited by brackets, two spaces or pipe) and opens it as Confluence page in IR6 space"
;;disabled 2015-01-23;;     (interactive)
;;disabled 2015-01-23;;     (save-excursion
;;disabled 2015-01-23;;       (re-search-backward "\\(\\] \\|  \\|\* \\|| \\)")  ;; search for "] " or "  " or "| "
;;disabled 2015-01-23;;       (forward-char)
;;disabled 2015-01-23;;       (forward-char)
;;disabled 2015-01-23;;       (setq start-pos (point))
;;disabled 2015-01-23;;       (re-search-forward "\\( \\[\\|  \\| |\\)")  ;; search for " [" or "  " or " |"
;;disabled 2015-01-23;;       (backward-char)
;;disabled 2015-01-23;;       (backward-char)
;;disabled 2015-01-23;;       (setq end-pos (point))
;;disabled 2015-01-23;;       (setq myname (buffer-substring start-pos end-pos))
;;disabled 2015-01-23;; 					;(message "Confluence page name: [%s]" myname)
;;disabled 2015-01-23;;       (confluence-get-page myname "IR6")
;;disabled 2015-01-23;; 					;(confluence-get-page myname)
;;disabled 2015-01-23;;       )
;;disabled 2015-01-23;;     )
;;disabled 2015-01-23;;  )

;; #############################################################################
;;** Outlook (disabled)

;;disabled 2015-01-23;; (when (or (my-system-is-powerplantlinux) (my-system-is-powerplantwin))
;;disabled 2015-01-23;;
;;disabled 2015-01-23;;  ;; ######################################################
;;disabled 2015-01-23;;  ;; editing Outlook emails in Emacs
;;disabled 2015-01-23;;  ;; http://www.emacswiki.org/emacs/MsOutlook
;;disabled 2015-01-23;;  (my-load-local-el "contrib/outlookedit.el")
;;disabled 2015-01-23;;  (use-package outlookedit)
;;disabled 2015-01-23;;
;;disabled 2015-01-23;;  (defvar mno-get-outlook-body
;;disabled 2015-01-23;;    "cscript //B //Job:getMessage c:/Users/karl.voit/bin/outlook_emacs.wsf")
;;disabled 2015-01-23;;  (defvar mno-put-outlook-body
;;disabled 2015-01-23;;    "cscript //B //Job:putMessage c:/Users/karl.voit/bin/outlook_emacs.wsf")
;;disabled 2015-01-23;;
;;disabled 2015-01-23;;  ;; ######################################################
;;disabled 2015-01-23;;  ;; use mail-mode for email or usenet postings:
;;disabled 2015-01-23;;  (add-to-list 'auto-mode-alist '("\\.\\(mail\\|email\\|posting\\)$" . mail-mode))
;;disabled 2015-01-23;;  (dolist (hook '(mail-mode-hook))
;;disabled 2015-01-23;;    (add-hook hook (lambda ()
;;disabled 2015-01-23;;		     (mail-mode-auto-fill)
;;disabled 2015-01-23;;		     (auto-fill-mode 1)
;;disabled 2015-01-23;;		     (flyspell-mode 1)
;;disabled 2015-01-23;;		     (ispell-change-dictionary "german8")
;;disabled 2015-01-23;;		     (flyspell-buffer)
;;disabled 2015-01-23;;		     ))
;;disabled 2015-01-23;;    )
;;disabled 2015-01-23;;
;;disabled 2015-01-23;;  ;; http://www.emacswiki.org/emacs/MailMode
;;disabled 2015-01-23;;  (add-hook 'mail-mode-hook
;;disabled 2015-01-23;;	    (lambda ()
;;disabled 2015-01-23;;	      (font-lock-add-keywords nil
;;disabled 2015-01-23;;				      '(("^[ \t]*>[ \t]*>[ \t]*>.*$"
;;disabled 2015-01-23;;					 (0 'mail-multiply-quoted-text-face))
;;disabled 2015-01-23;;					("^[ \t]*>[ \t]*>.*$"
;;disabled 2015-01-23;;					 (0 'mail-double-quoted-text-face))))))
;;disabled 2015-01-23;;
;;disabled 2015-01-23;;  )


;; #############################################################################
;;** xml-prettyprint commands
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

;;** my-xml-pretty-print
(defun my-xml-pretty-print()
  "mark whole buffer and call xml-pretty-print-region"
  (interactive)
  (mark-whole-buffer)
  (xml-pretty-print-region);; FIXXME: provide (begin end)
  )



;; #############################################################################
;;** auto-save (disabled)
;; automatically save more often within certain modes:
;; http://www.emacswiki.org/emacs/AutoSave
;; http://www.litchie.net/programs/real-auto-save.html
;; ... installed 2014-03-05 via Marmalade
;(use-package real-auto-save)
;(add-hook 'org-mode-hook 'turn-on-real-auto-save)
;;; Auto save interval is 10 seconds by default. You can change it:
;(when (my-system-is-powerplantwin)
;  (setq real-auto-save-interval 10)
;)
;(when (my-system-type-is-gnu)
;  (setq real-auto-save-interval 30)
;)

;; #############################################################################
;;** spray (speed-reading; my-map S)
;; A speed reading mode for Emacs.
;; The algorithm is taken from OpenSpritz
;; https://github.com/zk-phi/spray
;; ... installed 2014-06-13 via manual download from github (for testing)
;OLD;(my-load-local-el "contrib/spray/spray.el")
;OLD;(bind-key "S" #'spray-mode my-map)
(use-package spray
  ;; :disabled t
  :ensure t
  :defer 10
  :config ;; executed after loading package
  :bind (:map my-map ("s" . spray-mode))
)



;; #############################################################################
;;** yafolding
;; https://github.com/zenozeng/yafolding.el
;; Folding based on identation
;;(my-load-local-el "contrib/yafolding/yafolding.el")
;;(add-to-list 'auto-mode-alist '("\\.xml$" . yafolding-mode))
;;(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
;;(global-set-key (kbd "<C-S-return>") 'yafolding-toggle-all)
;;(global-set-key (kbd "<C-return>") 'yafolding-toggle-element)

(use-package yafolding
  :ensure t
  :defer 10
  :mode ("\\.xml\\'" . yafolding-mode)

  :config ;; executed after loading package

  (add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
  ;;(global-set-key (kbd "<C-S-return>") 'yafolding-toggle-all)
  ;;(global-set-key (kbd "<C-return>") 'yafolding-toggle-element)
  :bind (("<C-S-return>" . yafolding-toggle-all)
         ("<C-return>" . yafolding-toggle-element))
)

;; #############################################################################
;;** full screen Emacs   (F12)
;; full-screen, naked Emacs without distractions
;; thanks to Bastien; adopted to my requirements
;; https://gist.github.com/bzg/8578998

(when (my-system-type-is-gnu)

  (defvar my-toggle-naked-emacs-status nil
    "state of fullscreen/naked Emacs mode. t means fullscreen, nil means normal")
  (make-variable-buffer-local 'my-toggle-naked-emacs-status)

  ;; See http://bzg.fr/emacs-hide-mode-line.html
  (defvar-local hidden-mode-line-mode nil)
  (defvar-local hide-mode-line nil)

  (define-minor-mode hidden-mode-line-mode
    "Minor mode to hide the mode-line in the current buffer."
    :init-value nil
    :global nil
    :variable hidden-mode-line-mode
    :group 'editing-basics
    (if hidden-mode-line-mode
	(setq hide-mode-line mode-line-format
	      mode-line-format nil)
      (setq mode-line-format hide-mode-line
	    hide-mode-line nil))
    (force-mode-line-update)
    ;; Apparently force-mode-line-update is not always enough to
    ;; redisplay the mode-line
    (redraw-display)
    (when (and (called-interactively-p 'interactive)
	       hidden-mode-line-mode)
      (run-with-idle-timer
       0 nil 'message
       (concat "Hidden Mode Line Mode enabled.  "
	       "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

  ;; A small minor mode to use a big fringe
  (defvar bzg-big-fringe-mode nil)
  (define-minor-mode bzg-big-fringe-mode
    "Minor mode to hide the mode-line in the current buffer."
    :init-value nil
    :global t
    :variable bzg-big-fringe-mode
    :group 'editing-basics
    (if (not bzg-big-fringe-mode)
	(set-fringe-style nil)
      (set-fringe-mode
       (/ (- (frame-pixel-width)
	     (* 100 (frame-char-width)))
	  3)
       )
      ))

  ;; Command to toggle the display of the mode-line as a header
  (defvar-local header-line-format nil)
  (defun mode-line-in-header ()
    (interactive)
    (if (not header-line-format)
	(setq header-line-format mode-line-format
	      mode-line-format nil)
      (setq mode-line-format header-line-format
	    header-line-format nil))
    (set-window-buffer nil (current-buffer)))

  (defun my-toggle-naked-emacs ()
    "Toggle fullscreen/naked Emacs and normal Emacs"
    (interactive)
    (cond (my-toggle-naked-emacs-status
	   ;; make it naked!

	   (setq my-toggle-naked-emacs-status nil)
	   ;; Prevent the cursor from blinking
	   (blink-cursor-mode 0)
	   ;; Don't let Emacs hurt your ears
	   (setq visible-bell t)

	   ;; This is bound to f11 in Emacs 24.4
	   (toggle-frame-fullscreen)
	   ;; ;; Who use the bar to scroll?
	   ;; (scroll-bar-mode 0)

	   (menu-bar-mode 0)

	   ;; You can also set the initial frame parameters
	   ;; (setq initial-frame-alist
	   ;;       '((menu-bar-lines . 0)
	   ;;         (tool-bar-lines . 0)))

	   ;; Activate hidden-mode-line-mode
	   (hidden-mode-line-mode 1)

	   ;; If you want to hide the mode-line in all new buffers
	   ;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

	   ;; Alternatively, you can paint your mode-line in White but then
	   ;; you'll have to manually paint it in black again
	   ;; (custom-set-faces
	   ;;  '(mode-line-highlight ((t nil)))
	   ;;  '(mode-line ((t (:foreground "white" :background "white"))))
	   ;;  '(mode-line-inactive ((t (:background "white" :foreground "white")))))

	   ;; reset fringe with:
	   ;; (set-fringe-mode nil)

	   ;; Now activate this global minor mode
	   (bzg-big-fringe-mode 1)

	   ;; To activate the fringe by default and deactivate it when windows
	   ;; are split vertically, uncomment this:
	   ;; (add-hook 'window-configuration-change-hook
	   ;;           (lambda ()
	   ;;             (if (delq nil
	   ;;                       (let ((fw (frame-width)))
	   ;;                         (mapcar (lambda(w) (< (window-width w) fw))
	   ;;                                 (window-list))))
	   ;;                 (bzg-big-fringe-mode 0)
	   ;;               (bzg-big-fringe-mode 1))))

	   ;; Use a minimal cursor
	   ;; (setq cursor-type 'hbar)

	   ;; ;; Get rid of the indicators in the fringe
	   ;; (mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
	   ;;         fringe-bitmaps)
	   ;;
	   ;; ;; Set the color of the fringe
	   ;; (custom-set-faces
	   ;;  '(fringe ((t (:background "white")))))
	   ;;
	   ;; (custom-set-faces
	   ;;   '(default ((t (:background "black" :foreground "grey"))))
	   ;;   '(fringe ((t (:background "black")))))

	   ;;(global-set-key (kbd "C-s-SPC") 'mode-line-in-header)
	   (bind-key "h" #'mode-line-in-header my-map)

	   (message "Enjoy your concentration!")

	   )
	  (t
	   ;; normal mode
	   (setq my-toggle-naked-emacs-status t)

	   (blink-cursor-mode t)
	   (setq visible-bell nil)
	   (toggle-frame-fullscreen)
	   ;(scroll-bar-mode 1)
	   ;;(menu-bar-mode 1)
	   (hidden-mode-line-mode nil)

	   ;; If you want to hide the mode-line in all new buffers
	   ;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

	   (bzg-big-fringe-mode nil)
	   (set-fringe-mode nil)

	   (message "See everything.")
	   )
	  )
    )
)

;; #############################################################################
;;** my-open-in-external-app
;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
;; open dired file in external app (specified by the operating system)
(defun my-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.

The app is chosen from your OS's preference."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))
           (file (list file)))))

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ") ) )

    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )



;; #############################################################################
;;** browse-kill-ring (M-y)
;; https://github.com/browse-kill-ring/browse-kill-ring
;; browse-kill-ring
(browse-kill-ring-default-keybindings); map M-y to browse-kill-ring

;;** pdf-mode
;; https://github.com/mishoo/pdf-mode.el
;;2014-11-16 works but I disable it for now;; (my-load-local-el "contrib/pdf-mode/pdf-mode.el")


;;** guide-key
;; via reddit -> https://github.com/kai2nenobu/guide-key
(use-package guide-key
  ;; :disabled t  ;; stop loading if 't'
  :ensure nil ;; install package if not found OR: (setq use-package-always-ensure t)
  :config ;; executed after loading package
  (setq guide-key/guide-key-sequence '("C-c C-,"))
  (guide-key-mode 1)  ; Enable guide-key-mode
)


;;** vcard-mode
;; via https://github.com/dochang/vcard-mode
(use-package vcard-mode
  ;; :disabled t  ;; stop loading if 't'
  :ensure nil ;; install package if not found OR: (setq use-package-always-ensure t)
  :load-path "contrib/vcard-mode/" ;; relative to emacs dir
  :pin manual
  :mode ("\\.vc\\(f\\|ard\\)\\'" . vcard-mode)
)

;;** sunrise-mode
;; via http://www.emacswiki.org/emacs/Sunrise_Commander and GitHub
(use-package sunrise-commander
  :disabled t  ;; disabled 2015-08-31
  :ensure nil ;; install package if not found OR: (setq use-package-always-ensure t)
  :mode ("\\.py\\'" . elpy-mode)
  ;; 3) Choose some unused extension for files to be opened in Sunrise VIRTUAL
  ;; mode and add it to `auto-mode-alist', e.g. if you want to name your virtual
  ;; directories like *.svrm just add to your .emacs file a line like the
  ;; following:
  :mode ("\\.srvm\\'" . sr-virtual-mode)
)


;;** RAML-mode
;; via http://www2.tcs.ifi.lmu.de/~hoffmann/raml/raml-mode.el
(when (my-system-is-powerplantwin)
  (my-load-local-el "contrib/raml-mode/raml-mode.el")
)

;;** synonyms (my-map S)
;; via http://www.emacswiki.org/emacs/ThesauriAndSynonyms and id:2015-08-28-synonyms.el
;; The file names are absolute, not relative, locations
;;     - e.g. /foobar/mthesaur.txt.cache, not mthesaur.txt.cache
(use-package synonyms
  :ensure t ;; install package if not found OR: (setq use-package-always-ensure t)
  :init ;; executed before loading package
  (setq synonyms-file        "~/.emacs.d/data/mthes10/mthesaur.txt")
  (setq synonyms-cache-file  "~/.emacs.d/data/vkcachefile")
  :config
  (defun my-synonym-current-word ()
    "Lookup synonyms for current word."
    (interactive)
    (synonyms-lookup (thing-at-point 'word) nil nil))
  :bind (:map my-map ("S" . my-synonym-current-word))
)

;;** nyan-mode
;; via https://www.reddit.com/r/emacs/comments/3xoins/totally_useless_and_utterly_awesome_packages/
;; show the percentage within current buffer as Nyan Cat
;;     - e.g. /foobar/mthesaur.txt.cache, not mthesaur.txt.cache
(use-package nyan-mode
  :ensure t ;; install package if not found OR: (setq use-package-always-ensure t)
  :config
  (nyan-mode t)
)

;;** highlight-tail
;; via https://www.reddit.com/r/emacs/comments/3xoins/totally_useless_and_utterly_awesome_packages/
;; update settings via M-x highlight-tail-reload
;;     - e.g. /foobar/mthesaur.txt.cache, not mthesaur.txt.cache
(use-package highlight-tail
  :disabled t  ;; disabled 2015-12-26 due issues with jumping cursor
  :ensure t ;; install package if not found OR: (setq use-package-always-ensure t)
  :init
  (setq highlight-tail-colors '(("#1c1c1c" . 0)
                                ;;("#bc2525" . 25)
                                ;;("black" . 66)
                                ))
  :config
  (highlight-tail-mode)
)

;;** anzu-mode (showing number of matches when searching)
(use-package anzu
;;  :disabled t  ;; disabled 2015-12-26 due issues with jumping cursor
  :ensure t ;; install package if not found OR: (setq use-package-always-ensure t)
  :diminish anzu-mode
  :config
  (global-anzu-mode +1)
)

;;** smart-mode-line
;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :ensure t ;; install package if not found OR: (setq use-package-always-ensure t)
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful) ;; select theme: light, dark, respectful
  ;; hiding minor modes from mode line (don't forget the leading space)
  (setq rm-blacklist '(" Fill" " Ind" " MRev" " hl-p" " Guide" " OrgStruct" " ,"))
  ;; replacing path names with abbrevations:
  (add-to-list 'sml/replacer-regexp-list '("^~/hosts/all/config/emacs.d" ":ED:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/share/all/org-mode" ":org:") t)
  (smart-mode-line-enable)
)


;;** display-time-mode
;; http://emacs.stackexchange.com/questions/13227/easy-way-to-give-the-time-its-own-face-in-modeline
(setq display-time-string-forms
      '((propertize (format-time-string "%A %F %R" now) 'face 'bold)))
;; http://www.emacswiki.org/emacs/DisplayTime
(display-time-mode t)


;;** dash
;; required by eno (and most probably others in future)
(use-package dash
  :ensure t
  :defer 10
  )

;;** edit-at-point
;; required by eno (and most probably others in future)
(use-package edit-at-point
  :ensure t
  :defer 10
  )

;;** eno-mode (ace-jump/easymotion provides "goto certain char in view")
;; https://github.com/enoson/eno.el
(use-package eno
  ;; :disabled t
  :ensure t
  ;;  :diminish eno
  :defer 11
  ;;  :config ;; executed after loading package
  :bind (:map my-map ("RET" . eno-word-goto))
)

;;** Markdown
;; http://ikiwiki.info/tips/Emacs_and_markdown/
(use-package markdown-mode
  ;;:disabled t
  :ensure t
  :if (my-system-is-powerplantwin)
  ;;:diminish whitespace-mode
  :defer 10
  :mode ("\\.md\\'" . markdown-mode)
)

;;** Ox-Reveal
;; https://github.com/yjwen/org-reveal
(use-package ox-reveal
  ;;:disabled t
  :ensure t
  ;;:if (my-system-is-powerplantwin)
  ;;:diminish whitespace-mode
  :defer 10
  :config
  ;;(setq org-reveal-root "file:///d:/reveal.js")
  (cond ((my-system-type-is-gnu)
         (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
         (setq org-reveal-extra-css "file:///home/vk/.emacs.d/data/reveal_theme_night_local.css"))
        ((my-system-is-powerplantwin)
         (setq org-reveal-root "file:///C:/Users/karl.voit/.emacs.d/data/reveal.js/")
         (setq org-reveal-extra-css "file:///C:/Users/karl.voit/.emacs.d/data/reveal_theme_night_local.css"))
        (t
         (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))
    )
  (setq org-reveal-hlevel 2)
  (setq org-reveal-postamble "<p> Created by Karl. </p>")
  (setq org-reveal-center nil)
  (setq org-reveal-progress t)
  (setq org-reveal-history nil)
  (setq org-reveal-control t)
  (setq org-reveal-keyboard t)
  (setq org-reveal-overview nil)
  (setq org-reveal-transition "default")
  ;; - transitions:
  ;;   - default
  ;;   - cube
  ;;   - page
  ;;   - concave
  ;;   - zoom
  ;;   - linear
  ;;   - fade
  ;;   - none
  (setq org-reveal-theme "night")
  ;; - Styles:
  ;;   - black (default)
  ;;   - white
  ;;   - league
  ;;     - gray one
  ;;   - sky
  ;;     - nice bright one
  ;;   - beige
  ;;     - nice bright one
  ;;   - simple
  ;;     - bright
  ;;   - serif
  ;;     - old school
  ;;   - blood
  ;;     - cool!
  ;;   - *night*
  ;;     - very nice
  ;;   - moon
  ;;   - solarized
)

;;** Swiper
;; https://github.com/abo-abo/swiper
;; http://pragmaticemacs.com/emacs/dont-search-swipe/
(use-package swiper
  ;;:disabled t
  :ensure t
  ;;:if (my-system-is-powerplantwin)
  ;;:diminish whitespace-mode
  :defer 10
  :config

  (setq ivy-display-style 'fancy) ;; fancy highlighting

  ;;advise swiper to recenter on exit
  (defun bjm-swiper-recenter (&rest args)
    "recenter display after swiper"
    (recenter)
    )
  (when (my-system-type-is-gnu)
    (advice-add 'swiper :after #'bjm-swiper-recenter)
    )

  ;(bind-key "C-s" 'swiper)
  ;;(global-set-key "\C-s" 'swiper)
)

;;** char-menu
;; http://irreal.org/blog/?p=4926 -> https://github.com/mrkkrp/char-menu
;; add characters: "M-x customize-group char-menu RET"
(use-package char-menu
  ;;:disabled t
  :ensure t
;;  :if (my-system-type-is-gnu)
  :defer 10
  :config
  (setq char-menu
        '("→" "…" "š" "·" "•" "№" "★" "°" "–" "—" "§" "«»" "»«" "‘’" "“”" "∅" "©" "†"
         ("Arrows"     "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
         ("Math"       "¬" "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
         ("Greek"      "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ"
          "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω")
         ("Hatschek"   "Ǎ" "ǎ" "Č" "č" "Ď" "ď" "Ě" "ě" "Ǧ" "ǧ" "Ȟ" "ȟ"
          "Ǐ" "ǐ" "ǰ" "Ǩ" "ǩ" "Ľ" "ľ" "Ň" "ň" "Ǒ" "ǒ" "Ř" "ř" "Š" "š" "Ť" "ť"
          "Ǔ" "ǔ" "Ǚ" "ǚ" "Ž" "ž" "Ǯ" "ǯ")
         ("Umlaute"  "ä" "ö" "ü" "Ä" "Ö" "Ü" "ß")
        ))
  (global-set-key [f7] 'char-menu)
)

;;** smartparens
;; nice overview: http://danmidwood.com/content/2014/11/21/animated-paredit.html
;; nice overview: https://ebzzry.github.io/emacs-pairs.html
(use-package smartparens
  :disabled t
  :init
  (smartparens-global-mode 1)
  (show-smartparens-global-mode +1)

  :bind (;; ("M-n" . sp-next-sexp)
         ;; ("M-p" . sp-previous-sexp)
         ("M-f" . sp-forward-sexp)
         ("M-b" . sp-backward-sexp)
         )

  :config
  ;; Enable smartparens everywhere
  (use-package smartparens-config)

  ;; ;; Require and disable paredit because some packages rely on it.
  ;; (use-package paredit)
  ;; (disable-paredit-mode)

  (setq
   smartparens-strict-mode t
   sp-autoinsert-if-followed-by-word t
   sp-autoskip-closing-pair 'always
   ;;sp-base-key-bindings 'paredit
   sp-hybrid-kill-entire-symbol nil)

  ;; (sp-use-paredit-bindings)

  ;; (sp-with-modes '(markdown-mode gfm-mode rst-mode)
  ;;   (sp-local-pair "*" "*" :bind "C-*")
  ;;   (sp-local-tag "2" "**" "**")
  ;;   (sp-local-tag "s" "```scheme" "```")
  ;;   (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

  ;; ;; Close a backtick with another backtick in clojure-mode
  ;; (sp-local-pair 'clojure-mode "`" "`" :when '(sp-in-string-p))

  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  )

;;** counsel
(use-package counsel
  ;; :disabled t
  :ensure t
  ;;:if my-system-is-sherri
  ;;:if (or (my-system-is-gary-or-sherri) (my-system-is-powerplantlinux))
  ;;:diminish whitespace-mode
  :defer 10
)

;;** helm
(use-package helm
  ;; :disabled t
  :ensure t
  :defer 10
)

;;** wttrin.el
;; http://pragmaticemacs.com/emacs/weather-in-emacs/
;; https://github.com/bcbcarl/emacs-wttrin
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :defer 15
  :init
  (setq wttrin-default-cities '("graz" "ebreichsdorf" "st.poelten" "schladming"))
  (cond ((my-system-is-sherri)
         (bind-key "w" #'(lambda () (interactive) (wttrin-query "graz") (toggle-truncate-lines)) my-map)
         )
        ((my-system-is-powerplantwin)
         (bind-key "w" #'(lambda () (interactive) (wttrin-query "unterpremstatten") (toggle-truncate-lines)) my-map)
          )
        (t
         (bind-key "w" #'(lambda () (interactive) (wttrin) (toggle-truncate-lines)) my-map)
         )
        )
  )

        (global-set-key (kbd "C-f") '(lambda () (interactive) (forward-line 5)))

;;* my helper functions

;; #############################################################################
;;** Infonova Functions for Working Hour Calculation
(defun my-minutes-of-hm-string(hm-string)
  "returns the minutes of a string like 9:42 -> 42 (and 0 if there are no minutes)"
  (let (
	;; minutes is the second element after splitting with ":"
	(minutes (nth 1 (split-string hm-string ":")))
	)
    ;; if there is no second element, return "0" (instead of nil)
    (if (eq minutes 'nil)
	0
      (string-to-number minutes)
      )
    )
  )

(defun my-hours-of-hm-string(hm-string)
  "returns the hours of a string like 9:42 -> 9"
  (string-to-number
   (car
    (split-string hm-string ":")
    )
   )
)

;; EXAMPLE USAGE:
;; | [2015-01-13 Di] | Tue | 08:53-17:23 |   |   | 8:30 | 8:30 | 100 | Product Development |       |
;; |                 |     |             |   |   |      | korr |   % | Was                 | Notiz |
;; #+TBLFM: $7=$6::$9=Product Development::$8 = '(my-percentage-of-hm-string-with-day $7 $2)

(defun my-percentage-of-hm-string-with-day(hm-string day)
  "percentage of HH:MM when 8h30min (Mon-Thu) or 4h30min (Fri) are 100 percent"
  (let (
	(hours (my-hours-of-hm-string hm-string));; integer of hours from hm-string
	(minutes (my-minutes-of-hm-string hm-string));; integer of minutes from hm-string
        (norm-hour-minutes (cond
                            ((string= day "Mon") 8.5)
                            ((string= day "Mo")  8.5)
                            ((string= day "Tue") 8.5)
                            ((string= day "Di")  8.5)
                            ((string= day "Wed") 8.5)
                            ((string= day "Mi")  8.5)
                            ((string= day "Thu") 8.5)
                            ((string= day "Do")  8.5)
                            ((string= day "Fri") 4.5)
                            ((string= day "Fr")  4.5)
                            )
                           )
	)
    ;;debug;;(message (concat "norm-hour-minutes for " day " is " (number-to-string norm-hour-minutes)))
    (let (
	  (hoursminutes (+ hours (/ minutes 60.00))) ;; 8h30min -> 8.5h
	  )
      (round (* 100 (/ hoursminutes norm-hour-minutes)));; hoursminutes in relation to norm-hoursminutes
      )
    )
  )

;; #############################################################################
;;** Proper English Title Capitalization of a Marked Region
;; additionally to the list defined in title-capitalization:
(defvar my-do-not-capitalize-words '("lazyblorg" "mutt")
  "My personal list of words that doesn't get capitalized in titles.")


(defun title-capitalization (beg end)
  "Proper English title capitalization of a marked region"
  ;; - before: the presentation of this heading of my own from my keyboard and yet
  ;; - after:  The Presentation of This Heading of My Own from My Keyboard and Yet
  ;; - before: a a a a a a a a
  ;; - after:  A a a a a a a A
  (interactive "r")
  (save-excursion
    (let* (
	   ;; basic list of words which don't get capitalized according to simplified rules:
	   ;; http://karl-voit.at/2015/05/25/elisp-title-capitalization/
           (do-not-capitalize-basic-words '("a" "ago" "an" "and" "as" "at" "but" "by" "for"
                                            "from" "in" "into" "it" "next" "nor" "of" "off"
                                            "on" "onto" "or" "over" "past" "so" "the" "till"
                                            "to" "up" "yet"
                                            "n" "t" "es" "s"))
	   ;; if user has defined 'my-do-not-capitalize-words, append to basic list:
           (do-not-capitalize-words (if (boundp 'my-do-not-capitalize-words)
                                        (append do-not-capitalize-basic-words my-do-not-capitalize-words )
                                      do-not-capitalize-basic-words
                                      )
                                    )
           )
      ;; go to begin of first word:
      (goto-char beg)
      (capitalize-word 1)
      ;; go through the region, word by word:
      (while (< (point) end)
        (skip-syntax-forward "^w" end)
;;        (let ((word (thing-at-point 'word t)))
        (let ((word (thing-at-point 'word)))
          (if (stringp word)
              ;; capitalize current word except it is list member:
              (if (member (downcase word) do-not-capitalize-words)
                  (downcase-word 1)
                (capitalize-word 1)))))
      ;; capitalize last word in any case:
      (backward-word 1)
      (if (and (>= (point) beg)
;;               (not (member (or (thing-at-point 'word t) "s")
               (not (member (or (thing-at-point 'word) "s")
                            '("n" "t" "es" "s"))))
          (capitalize-word 1))))
)

(ert-deftest my-title-capitalization ()
  "Tests proper English title capitalization"
  (should (string= (with-temp-buffer
		     (insert "the presentation of this heading of my own from my keyboard and yet\n")
		     (goto-char (point-min))
		     (set-mark-command nil)
		     (goto-char (point-max))
		     ;(transient-mark-mode 1)
		     (title-capitalization)
		     (buffer-string))
		   "The Presentation of This Heading of My Own from My Keyboard and Yet\n"
		   )))

;; #############################################################################
;;** my-toggle-windows-split
;; http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun my-toggle-windows-split ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(bind-key "|" #'my-toggle-windows-split my-map)


;; #############################################################################
;;** my-toggle-beginner-setup

(defvar my-toggle-beginner-setup-status nil
  "state of Emacs setup which is least confusing for beginners. t means beginner, nil means normal")
;;(make-variable-buffer-local 'my-toggle-beginner-setup-status)

(defun my-emacs-normal-setup ()
  "Hide things for my normal usage"
  (interactive)
  (if (functionp 'tool-bar-mode) (tool-bar-mode -1)) ;; hide icons
  (menu-bar-mode 0) ;; hide menu-bar
  (when (not (my-system-is-kva))
    (scroll-bar-mode 0) ;; hide scroll-bar, I do have Nyan-mode! :-)
    )
  (setq debug-on-quit t);; show debug information on canceling endless loops and so forth

  ;; http://www.emacswiki.org/emacs/sylecn
  ;;show nothing in *scratch* when started
  (setq initial-scratch-message nil)

  ;; ######################################################
  ;; handle CamelCaseParts as distinct words
  ;; http://ergoemacs.org/emacs/emacs_adv_tips.html
  (global-subword-mode 1) ; 1 for on, 0 for off
  ;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
  ;; FIXXME: this is a test for getting it work independent of
  ;; CamelCase words

  ;;(setq org-hide-leading-stars t)
)

(defun my-emacs-beginner-setup ()
  "Make things nice for beginners"
  (interactive)
  (tool-bar-mode) ;; show icons
  (menu-bar-mode) ;; show menu-bar
  (scroll-bar-mode 1) ;; show scroll-bar
  (setq debug-on-quit nil);; no debug information on canceling endless loops and so forth

  ;; http://www.emacswiki.org/emacs/sylecn
  ;;show nothing in *scratch* when started
  (setq initial-scratch-message ";; This buffer is for notes you don't want to save\n;; If you want to create a file, visit that file with C-x C-f,\n;; then enter the text in that file's own buffer.\n\n")

  ;; http://ergoemacs.org/emacs/emacs_adv_tips.html
  (global-subword-mode 0) ; 1 for on, 0 for off
  ;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
  ;; FIXXME: this is a test for getting it work independent of CamelCase words

  ;;(setq org-hide-leading-stars nil)


  ;; http://stackoverflow.com/questions/24684979/how-to-add-a-tool-bar-button-in-emacs
  ;;(add-hook 'after-init-hook
  ;;          (lambda ()
  ;;            (define-key global-map [tool-bar pdf-button]
  ;;              '(menu-item "Export to PDF" org-latex-export-to-pdf
  ;;                          :image (image :type png :file "~/.emacs.d/data/icon_PDF_16x16.png")
  ;;                          ))))
  (define-key global-map [tool-bar pdf-button]
    '(menu-item "Export to PDF" org-latex-export-to-pdf
                :image (image :type png :file "~/.emacs.d/data/icon_PDF_16x16.png")
                ))
)


(my-emacs-normal-setup)

(defun my-toggle-beginner-setup ()
  "Toggle Emacs beginner setup and normal Emacs"
  (interactive)
  (cond (my-toggle-beginner-setup-status
         ;; normal mode
         (my-emacs-normal-setup)
         (message "As you please")
         (setq my-toggle-beginner-setup-status nil)
         )
        (t
         ;; make it easy for beginners
         (my-emacs-beginner-setup)
         (message "Welcome to Emacs!")
         (setq my-toggle-beginner-setup-status t)
         )
        )
  )

(global-set-key [f1] 'my-toggle-beginner-setup)

;;** my-org-mobile-push

(defun my-org-mobile-push (&optional arg)
  (interactive)
  ;; when called with universal argument (C-u), Emacs will be closed
  ;;      after pushing to files
  ;; save original agenda in temporary variable
  (setq ORIGSAVED-org-agenda-custom-commands org-agenda-custom-commands)
  ;; set agenda for MobileOrg (omit some agenda
  ;; views I do not need on my phone):
  (setq org-agenda-custom-commands
        (quote (

                ("1" "1 month"
                 ((agenda "1 month"
                          ((org-agenda-ndays 31)
                           (org-agenda-time-grid nil)
                           (org-agenda-entry-types '(:timestamp :sexp))
                           )
                          )))

                ("B" "borrowed" tags "+borrowed"
                 (
                  (org-agenda-overriding-header "borrowed or lend")
                  (org-agenda-skip-function 'tag-without-done-or-canceled)
                  ))

                ("$" "Besorgungen" tags "+Besorgung"
                 (
                  (org-agenda-overriding-header "Besorgungen")
                  (org-agenda-skip-function 'tag-without-done-or-canceled)
                  ))

                )))
  ;; generate MobileOrg export:
  (org-mobile-push)
  ;; restore previously saved agenda:
  (setq org-agenda-custom-commands
        ORIGSAVED-org-agenda-custom-commands)
  (if (equal arg '(4))
      ;; save buffers and exit emacs;; FIXXME: not working yet
      (save-buffers-kill-terminal 't)
    )
  )



;;** my-reset-org

(defun my-reset-org ()
  "Clears all kinds of Org-mode caches and re-builds them if possible"
  (interactive)
  (measure-time
   (org-element-cache-reset)
   (org-refile-cache-clear)
   (org-refile-get-targets)
   (when (my-buffer-exists "*Org Agenda*")
     (kill-buffer "*Org Agenda*")
     (org-agenda-list)
     )
   )
  )

(defun my-reset-org-and-mobile-push ()
  "Clears all kinds of Org-mode caches and re-builds them if possible"
  (interactive)
  (measure-time
   (my-reset-org)
   (my-org-mobile-push)
   )
)

;;** message-outlook.el - sending mail with Outlook
(when (my-system-type-is-windows)
  (my-load-local-el "contrib/message-outlook.el")
)

;;** my-search-method-according-to-numlines()

;; see id:2016-04-09-chosing-emacs-search-method for the whole story!

;;see below;; (defun my-search-method-according-to-numlines ()
;;see below;;   "Determines the number of lines of current buffer and chooses a search method accordingly"
;;see below;;   (interactive)
;;see below;;   (if (< (count-lines (point-min) (point-max)) 20000)
;;see below;;       (swiper)
;;see below;;     (isearch-forward)
;;see below;;     )
;;see below;;   )
;;see below;; ;;fancy search:  (global-set-key "\C-s" 'swiper)
;;see below;; ;;normal search: (global-set-key "\C-s" 'isearch-forward)

(defun my-search-method-according-to-numlines ()
  (interactive)
  (if (and (buffer-file-name)
           (not (my-system-type-is-windows))
           (not (ignore-errors
                  (file-remote-p (buffer-file-name))))
           (if (eq major-mode 'org-mode)
               (> (buffer-size) 60000)
             (> (buffer-size) 300000)))
      (progn
        (save-buffer)
        (counsel-grep))
    (if (my-system-type-is-windows)
        (isearch-forward)
      (swiper--ivy (swiper--candidates))
      )
    ))

(global-set-key "\C-s" 'my-search-method-according-to-numlines)

;; #############################################################################

;;** my-export-month-agenda-to-png-via-screenshot
;; id:2016-04-12-my-export-month-agenda-to-png-via-screenshot

(defun my-export-month-agenda-to-png-via-screenshot()
  (interactive)
  (when (my-system-is-sherri)
    (message "Generating agenda ...")
    (org-agenda nil "n") ; generates agenda "n" (one month without todos)
    (if (my-buffer-exists "*Org Agenda*")
	(switch-to-buffer "*Org Agenda*")
      (org-agenda-list)
      )
    (message "Waiting for Screenshot ...")
    (sit-for 1) ; (sleep 1) ... doesn't re-display and thus screenshot
                ; showed buffer before switching to agenda
    (message "Say cheese ...")

    (setq myoutput
          (shell-command-to-string "/usr/bin/import -window root /home/vk/share/roy/from_sherri/agenda.png"))
    (message (concat "Screenshot done (" myoutput ")"))
    )
  )

;;** my-yank-windows (my-map y)
;; id:2016-05-22-my-yank-windows

(when (my-system-type-is-windows)
  (defun my-yank-windows ()
    "yanks from clipboard and replaces typical (list) markup"
    (interactive)
    (let ((mybegin (point)))              ;; mark beginning of line as start point
      (clipboard-yank)
      (save-restriction
        (narrow-to-region mybegin (point))  ;; ignore everything outside of region
        (goto-char (point-min))
        (while (search-forward "\"	" nil t)
  	(replace-match "- " nil t))
        (while (search-forward "o	" nil t)
  	(replace-match "  - " nil t))
        ;;(while (search-forward "1.	" nil t) ;; FIXXME: replace with regex-methods for numbers in general
        ;; (replace-match "1. " nil t))
        ))
    )

  (bind-key "y" #'my-yank-windows my-map)
  )


;;* Key bindings

;;** general navigation keys
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end]  'end-of-buffer)

(global-set-key [C-left] 'backward-word)
(global-set-key [C-right] 'forward-word)
(global-set-key [S-left] 'backward-word)
(global-set-key [S-right] 'forward-word)

;; 2016-03-23: PageUp/Down (instead of line up/down) for keyboards
;; without PgUp/Dn-keys:
(global-set-key (kbd "C-p") 'scroll-down-command)
(global-set-key (kbd "C-n") 'scroll-up-command)

;; https://www.reddit.com/r/emacs/comments/445w6s/whats_some_small_thing_in_your_dotemacs_that_you/czntjs2
(global-set-key (kbd "C-, l")          'avy-goto-line)
(global-set-key (kbd "C-, w")          'avy-goto-word-0)
;;(global-set-key (kbd "C-, .")          'winner-redo)
;;(global-set-key (kbd "C-, m")          'winner-undo)
;;(global-set-key (kbd "C-, w")          'ace-window)
;;(global-set-key (kbd "C-, s")          'workspace-goto)
;;(global-set-key (kbd "C-, ]")          'evil-jump-to-tag)
(global-set-key (kbd "C-, n")          'neotree-toggle)
;;** Magit status (my-map g)
;(bind-key "v" #'magit-status my-map)
(bind-key "g" #'magit-status my-map)
;;** fullscreen (F12)
(when (my-system-type-is-gnu)
  (global-set-key [f12] 'my-toggle-naked-emacs)
)
;;** Elisp
(bind-key "er" #'eval-region my-map)
(bind-key "el" #'find-library my-map)
(bind-key "ef" #'find-function-at-point my-map)
;;** Toggle between split windows and a single window (my-map s)
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

;2015-11-03: deactivated for synonym (bind-key "s" #'my-toggle-windows-split my-map)
;;** boxquote (my-map [qQ])
;(my-load-local-el "contrib/boxquote.el") 2014-01-19: removed old el and replaced it with package from elpa
(bind-key "q" #'boxquote-region my-map)
(bind-key "Q" #'boxquote-title my-map)
;;** switching lines (my-map <up/down>)
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

(bind-key (kbd "<up>") #'my-move-line-up my-map)
(bind-key (kbd "<down>") #'my-move-line-down my-map)

;;** joining lines: (my-map j)
;; http://whattheemacsd.com//key-bindings.el-03.html
(bind-key "j" ;; join-line
  (lambda ()
    (interactive)
    (join-line -1))
  my-map
)


(bind-key "l" #'my-jump-to-lazyblorg-heading-according-to-URL-in-clipboard my-map)

;;** web-jump (disabled)
;;disabled;; ;; ######################################################
;;disabled;; ;; web-jump: C-c w
;;disabled;; ;; http://www.emacswiki.org/emacs/WebJump
;;disabled;; ;; http://www.neilvandyke.org/webjump/
;;disabled;; ;; https://github.com/thefury/site-lisp/blob/master/dotemacs.el
;;disabled;; ;; http://whattheemacsd.com//my-misc.el-01.html (Urban Dictionary)
;;disabled;; (require 'webjump)
;;disabled;; (bind-key "w" #'webjump my-map)
;;disabled;; (setq webjump-sites ;(append   ;; append instead of overwrite
;;disabled;;       '(
;;disabled;; 	;; ------------------------------------------------------------------
;;disabled;; 	("Emacs Lisp List" . "anc.ed.ac.uk/~stephen/emacs/ell.html")
;;disabled;; 	("Ohio State Emacs Lisp Archive" .
;;disabled;; 	 [simple-query "www.cis.ohio-state.edu/emacs-lisp/"
;;disabled;; 		       "neutral.verbum.org/search?q=" "&archive=archive"])
;;disabled;; 	("PGP Key Server" .
;;disabled;; 	 [simple-query "pgp.mit.edu"
;;disabled;; 		       "pgp.mit.edu:11371/pks/lookup?op=index&search=" ""])
;;disabled;; 	;; my own jumps
;;disabled;; 	("Org-mode docu" . "http://orgmode.org/org.html")
;;disabled;; 	("Org-mode mailinglist" .
;;disabled;; 	 [simple-query "orgmode.org"
;;disabled;; 		       "http://search.gmane.org/?group=gmane.emacs.orgmode&query=" ""])
;;disabled;; 	("Debian Bug Number" .
;;disabled;; 	 [simple-query "www.debian.org/Bugs/"
;;disabled;; 		       "bugs.debian.org/cgi-bin/bugreport.cgi?bug="
;;disabled;; 		       ""])
;;disabled;; 	("EmacsWiki" .
;;disabled;; 	 [simple-query "www.emacswiki.org/cgi-bin/wiki.pl"
;;disabled;; 		       "www.emacswiki.org/cgi-bin/wiki.pl?search="
;;disabled;; 		       "&dosearch=1"])
;;disabled;; 	("Google" .
;;disabled;; 	 [simple-query "www.google.at" "www.google.at/search?q=" ""])
;;disabled;; 	("IMDB" .
;;disabled;; 	 [simple-query "www.imdb.com" "www.imdb.com/Find?select=All&for=" ""])
;;disabled;; 	;; ------------------------------------------------------------------
;;disabled;; 	) webjump-sample-sites ;)  ;; append instead of overwrite
;;disabled;; 	  )


;;** inserting time-stamps (my-map [dDtT])
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
(bind-key "t" #'my-insert-timestamp my-map)

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
(bind-key "T" #'my-insert-timestamp-inactive my-map)

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
(bind-key "d" #'my-insert-datestamp my-map)

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
(bind-key "D" #'my-insert-datestamp-inactive my-map)



;;** recently files (my-map r)
(bind-key "r" #'recentf-open-files my-map)


;;** helm-do-grep (my-map G)
;; helm-do-grep to grep in current folder
(bind-key "G" #'helm-do-grep-ag my-map)

;;** helm-org-headlines (my-map H)
(bind-key "H" #'helm-org-agenda-files-headings my-map)


;;** command log mode (my-map k)
;; https://github.com/lewang/command-log-mode
;;(my-load-local-el "contrib/command-log-mode/command-log-mode.el")
(use-package command-log-mode
  :disabled t
  :ensure t
  :defer 10
  :diminish command-log-mode
  :config ;; executed after loading package
  (add-hook 'LaTeX-mode-hook 'command-log-mode)
)


;; (defun my-start-command-log-mode()
;;   "load the command-log mode and start it."
;;   (interactive "*")
;;   ;; M-x clm/open-command-log-buffer
;;   (clm/open-command-log-buffer)
;; )

(bind-key "k" #'clm/open-command-log-buffer my-map)



;;** Confluence/Outlook (disabled)
;disabled; (when (or (my-system-is-powerplantlinux) (my-system-is-powerplantwin))
;disabled;   (bind-key "C" #'vk-open-as-confluence-page my-map)
;disabled;   (bind-key "oe" #'mno-edit-outlook-message my-map)
;disabled;   (bind-key "os" #'mno-save-outlook-message my-map)
;disabled;   )

;;** mark-ring-goto (my-map <left>)
(bind-key (kbd "<left>") #'org-mark-ring-goto my-map)

;;** main.el (my-map .)
(bind-key (kbd ".") (lambda()  ;; open main.el
			       (interactive)
			       (find-file "~/.emacs.d/main.el")
			       )
          my-map
          )


;;** org-mode teaser (my-map o)
(bind-key (kbd "o") (lambda()
			       (interactive)
                               (when (my-system-type-is-gnu)
                                 (find-file
                               "~/institutions/tugraz/schulungen_voit/org-mode/kursmaterial/featureshow/org-mode-teaser.org")
                                 )
                               (when (my-system-is-powerplantwin)
                                 (find-file
                               "c:/Users/karl.voit/fromweb/src/org-mode-workshop/featureshow/org-mode-teaser.org")
                                 )
			       )
          my-map
  )

;;** org-mode manual (my-map O)
(bind-key (kbd "O") (lambda()
			       (interactive)
                               (info "(org)")
			       )
  my-map
  )


;; 2015-11-10 deactivated ;; ;;** OrgStruct folding
;; 2015-11-10 deactivated ;; ;; 2014-03-19: OrgStruct-mode: folding and unfoldung:
;; 2015-11-10 deactivated ;; (bind-key "[" (lambda () (interactive) (org-cycle t)) my-map)
;; 2015-11-10 deactivated ;; (bind-key "]" (lambda () (interactive) (org-cycle)) my-map)

;;** Org-mobile import (my-map i)
(bind-key "i" (lambda ()
                         """foobar"""
                         (interactive) (my-mobile-org-import)) my-map )
;;** Org-mobile push (my-map I)


(bind-key "I" #'my-org-mobile-push my-map)


;;** Filter open Org-tasks by tag (my-map F)
;; see id:2014-11-02-filter-org-tasks-by-tag

(defun my-sparse-tree-with-tag-filter()
  "asks for a tag and generates sparse tree for all open tasks in current Org buffer
  that are associated with this tag"
  (interactive "*")
  (setq tag-for-filter
        (org-trim
         (org-icompleting-read "Tags: "
                               'org-tags-completion-function
                               nil nil nil 'org-tags-history))
        )
  (org-occur
   (concat "^\\*+ \\(NEXT\\|TODO\\|WAITING\\|STARTED\\) .+:"
           tag-for-filter
           ":")
   )
  )
(bind-key "F" #'my-sparse-tree-with-tag-filter my-map)

;;** my-fix-drawer-order (my-map C)
;; searches for :END: followed by :PROPERTIES: (assuming first drawer
;; is :LOGBOOK: and toggles order of them
(fset 'my-fix-drawer-order
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 58 69 78 68 58 17 10 58 80 82 79 80 69 82 84 73 69 83 58 return 1 67108896 19 58 69 78 68 58 5 23 18 58 76 79 71 66 79 79 75 58 return 25 return down] 0 "%d")) arg)))
(bind-key "C" #'my-fix-drawer-order my-map)

;;** my-title-capitalization (my-map C)
(bind-key "c" #'title-capitalization my-map)

;;** hippie-expand (M-/)
;; http://emacswiki.org/emacs/HippieExpand

;; issues when overriding undo-tree-undo (C-/): http://emacs.stackexchange.com/questions/2530/global-key-binding-overriden-by-undo-tree
;; http://stackoverflow.com/questions/5332221/globally-overriding-emacs-keybindings
;(add-hook 'undo-tree-mode (lambda () (local-unset-key "C-/")))
;;failed;(global-set-key (kbd "C-c /") 'hippie-expand)

;; could not make it work with C-/ - so I stick to M-/
(global-set-key (kbd "M-/") 'hippie-expand)

;;** neotree (f8)
;; https://github.com/jaypei/emacs-neotree
;; installed on 2015-03-22
(use-package neotree
  ;; :disabled t
  :ensure t
  :defer 20
  :config ;; executed after loading package
(global-set-key [f8] 'neotree-toggle)
)

;;** org-mode-reftex-setup (my-map R)
;; see id:2015-05-14-disable-orgmode-reftex-autoload
(bind-key (kbd "R") #'org-mode-reftex-setup my-map)
;; 2015-05-14: does NOT work (yet). See id:2015-05-14-disable-orgmode-reftex-autoload

;;** my-org-region-to-property (my-map p)
;; see id:2015-05-28-ask-for-properties

;; original function:
(defun my-org-region-to-property (&optional property)
  (interactive)
  ;; if no region is defined, do nothing
  (if (use-region-p)
      ;; if a region string is found, ask for a property and set property to
      ;; the string in the region
      (let ((val (replace-regexp-in-string
                  "\\`[ \t\n]*" ""
                  (replace-regexp-in-string "[ \t\n]*\\'" ""
                                            (substring (buffer-string)
                                                       (- (region-beginning) 1)
                                                       (region-end))))
                 )
            ;; if none was stated by user, read property from user
            (prop (or property
                      (org-read-property-name))))
        ;; set property
        (org-set-property prop val))))

;; ----------------------------------------------------

;; version that tries to read only properties of current entry:
;; From: "Antoine R. Dumont" <antoine.romain.dumont@gmail.com>
;; http://article.gmane.org/gmane.emacs.orgmode/106364
(defun org-read-entry-property-name ()
  "Read a property name from the current entry."
  (let ((completion-ignore-case t)
        (default-prop (or (and (org-at-property-p)
                               (org-match-string-no-properties 2))
                          org-last-set-property)))
    (org-completing-read
     (format "Property [%s]: " (if default-prop default-prop ""))
     (org-entry-properties nil nil)
     nil nil nil nil default-prop)))

(defun my-org-region-to-property (&optional property)
  (interactive)
  ;; if no region is defined, do nothing
  (if (use-region-p)
      ;; if a region string is found, ask for a property and set property to
      ;; the string in the region
      (let ((val (replace-regexp-in-string
                  "\\`[ \t\n]*" ""
                  (replace-regexp-in-string "[ \t\n]*\\'" ""
                                            (substring (buffer-string)
                                                       (- (region-beginning) 1)
                                                       (region-end))))
                 )
            ;; if none was stated by user, read property from user
            (prop (or property
                      (org-read-entry-property-name))))
        ;; set property
        (org-set-property prop val))))

(bind-key (kbd "p") #'my-org-region-to-property my-map)


;;** highlight-symbol (my-map h)
(use-package highlight-symbol
  :ensure t
  :defer 10
  ;;(bind-key (kbd "h") #'highlight-symbol my-map)
  :bind (:map my-map ("h" . highlight-symbol))
  )
;; original: (global-set-key [(control f3)] 'highlight-symbol)
;; original: (global-set-key [f3] 'highlight-symbol-next)
;; original: (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; original: (global-set-key [(meta f3)] 'highlight-symbol-query-replace)


;;** mark word|line|sentence|sexp|defun


(bind-key (kbd "mw") #'mark-word my-map)

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "m l" 'mark-line my-map)

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "m s" #'mark-sentence my-map)
(bind-key "m x" #'mark-sexp my-map)
(bind-key "m d" #'mark-defun my-map)


;;** delete-trailing-whitespace (my-map SPC)

  ;;(bind-key (kbd "SPC") #'delete-trailing-whitespace my-map)
;  (define-key org-mode-map (kbd "C-c C-, SPC") #'delete-trailing-whitespace);; workaround since line above doesn't work

;; 2016-02-06: https://www.reddit.com/r/emacs/comments/445w6s/whats_some_small_thing_in_your_dotemacs_that_you/
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;** maximize frame window
;; id:2016-03-27-maximize-window-init.el
(toggle-frame-maximized)

;;* autostart.bat.el

(message "######### Maximizing frame ...")
(toggle-frame-maximized)
(toggle-frame-maximized)

(sleep-for 2)
(setq my-org-agenda-tags-column (- (- (window-total-width) 3)))
(message (concat "######### Setting agenda tags column to " (number-to-string my-org-agenda-tags-column)))
(setq org-agenda-tags-column my-org-agenda-tags-column) ;; total width minus 3

(message "######### Creating agenda ...")
(my-org-agenda)


;;* custom variables

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

(message "######### finished loading main.el.")
;; Local Variables:
;; eval: (orgstruct++-mode)
;; End:

;;; main.el ends here
