
;; ######################################################
;; about defining keys
;; http://ergoemacs.org/emacs/keyboard_shortcuts.html


;; ######################################################
(server-start)


;; ######################################################
;;; von: http://www.zonix.de/html40/linux/emacsgnus.html
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


;; ######################################################
;; 2011-04-20: defining C-j as my own prefix:
;; http://stackoverflow.com/questions/1024374/how-can-i-make-c-p-an-emacs-prefix-key-for-develperlysense
;; NOTE: (info "(elisp) Key Binding Conventions") warns about user prefixes other than C-c
(define-prefix-command 'my-map)
(global-set-key (kbd "C-,") 'my-map)

;; ######################################################
;; 2013-03-31: http://stackoverflow.com/questions/3124844/what-are-your-favorite-global-key-bindings-in-emacs
;; font sizes:
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
;; Magit status
(global-set-key "\C-cv" 'magit-status)
;; MISC
(global-set-key "\C-cr"  'eval-region)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end]  'end-of-buffer)


;; ######################################################
;; from http://www.yak.net/fqa/124.html
;(global-unset-key "\C-p")
;; (global-set-key "\C-pl" 'goto-line)
(global-set-key "\C-cg" 'goto-line)
;; Ersetzt durch C-pg:
;;(global-set-key "\C-xl" 'goto-line)

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

(define-key global-map (kbd "C-1") 'my-toggle-windows-split)


;; ######################################################
;; Icons weg
(tool-bar-mode -1)

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
;(global-set-key [M-left] 'backward-word)
;(global-set-key [M-right] 'forward-word)
(global-set-key [S-left] 'backward-word)
(global-set-key [S-right] 'forward-word)



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
(when (my-system-is-gary)
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
(when (my-system-is-gary)

  ;; ######################################################
  ;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/
  ;; Ropemacs:
  ;(add-to-list 'load-path "~/.emacs.d/vendor/pymacs-0.24-beta2")
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t)

  ;; ######################################################
  ;; auto-complete mode
  ;(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete-1.2")
  (require 'auto-complete-config)
  ;(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete-1.2/dict")
  (ac-config-default)

  ;; ######################################################
  ;; PyFlakes:
  (setq python-check-command "pyflakes")

  )


;; ######################################################
;; LaTeX
(when (my-system-is-gary)


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
  (when (my-system-is-gary)
    (my-load-local-el "contrib/cdlatex.el")
    )
  
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
(when (my-system-is-gary)
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
(when (my-system-is-gary)
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

;(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)
;;;(define-key global-map [(f10)] 'flyspell-mode)
;;;(set-default 'ispell-local-dictionary my-german-ispell-dictionary)

;;; mein altes Zeug:

;; ######################################################
;; http://www.linuxfaq.de/f/cache/146.html

(autoload 'flyspell-mode "flyspell" "On-the-fly ispell." t)
(setq flyspell-issue-welcome-flag nil)
(setq flyspell-default-dictionary "de_AT")

(defun my-ispell-set-deutsch()
 "switch ispell language to deutsch"
  (interactive)
  (ispell-change-dictionary "de_AT"))

(defun my-ispell-set-english()
 "switch ispell language to english"
 (interactive)
 (ispell-change-dictionary "en_US"))

(defvar my-toggle-ispell-english-deutsch nil
 "state of english/ngerman toggle. t means english, nil means ngerman")
(make-variable-buffer-local 'my-toggle-ispell-english-deutsch)

(defun my-toggle-ispell-language ()
  "Toggle ispell-language between english and ngerman"
  (interactive)
  (cond (my-toggle-ispell-english-deutsch
	 (setq my-toggle-ispell-english-deutsch nil)
	 (my-ispell-set-deutsch)
	 )
	(t
	 (setq my-toggle-ispell-english-deutsch t)
	 (my-ispell-set-english)
	 )
	)
  )

;(add-hook 'post-mode-hook
;         '(lambda ()
;            (flyspell-mode t)
;            (setq flyspell-generic-check-word-p 'mail-mode-flyspell-verify)))
;; ######################################################

;; ######################################################
;(setq ispell-dictionary "german-new8")
;(setq ispell-dictionary "german-new8")
;(setq ispell-local-dictionary "german-new8")
(global-set-key "\C-cf" 'flyspell-mode)
(global-set-key "\C-cl" 'my-toggle-ispell-language)

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
;; boxquote
(my-load-local-el "contrib/boxquote.el")
(global-set-key "\C-cq" 'boxquote-region)
(global-set-key "\C-cQ" 'boxquote-title)


;; ######################################################
;; tabbar
(when (my-system-is-gary)
    (my-load-local-el "contrib/tabbar.el")
  )


;; ######################################################
;; yasnippet
;; http://yasnippet.googlecode.com/svn/trunk/doc/index.html
(my-load-local-el "contrib/yasnippet/yasnippet.el")
(require 'yasnippet)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)


;; ######################################################
;; org-mode
;; additionally: http://stackoverflow.com/questions/3622603/org-mode-setup-problem-when-trying-to-use-capture
(when (or (my-system-is-gary) (my-system-is-blanche))

  ;; set paths to manually installed Org-mode (from git)
  (add-to-list 'load-path "~/.emacs.d/contrib/org-mode/contrib/lisp")
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/org-mode/lisp"))

  ;; assign file extensions to Org-mode
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

  ;;(require 'org-install) ;; vk 2012-11-20 this line is obsolete
  (require 'org) 

  ;; load Org-mode specific settings:
  (my-load-local-el "org-mode.el")

  )



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

;; ######################################################
;; https://github.com/sellout/emacs-color-theme-solarized
;; Solarized Color Theme
;; 2. Add the `emacs-color-theme-solarized` directory to your Emacs `load-path`.
(add-to-list 'load-path "~/.emacs.d/contrib/emacs-color-theme-solarized/")
;; 3. Add `(require 'color-theme-solarized)` to your Emacs init file (usually `~/.emacs`).
(require 'color-theme-solarized)
;; 4. Use the usual [color-theme] mechanism to select one of the Solarized themes, or `M-x color-theme-solarized-[light|dark]`.
(color-theme-solarized-light)

(defvar my-toggle-color-theme-state nil
 "state of color-theme toggle. t means dark, nil means light")
(make-variable-buffer-local 'my-toggle-color-theme-state)

(defun my-toggle-color-theme ()
 "Toggle color-theme between color-theme-solarized-dark and color-theme-solarized-light"
 (interactive)
 (cond (my-toggle-color-theme-state
 (setq my-toggle-color-theme-state nil)
        (color-theme-solarized-light))
       (t
        (setq my-toggle-color-theme-state t)
        (color-theme-solarized-dark))))

(global-set-key "\C-cs" 'my-toggle-color-theme)




;; ######################################################
;; http://www.emacswiki.org/emacs/MiniMap
;; MiniMap for Emacs
(when (or (my-system-is-gary) (my-system-is-blanche))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/minimap"))
    (require 'minimap)
    )


;; ######################################################
;; http://www.neilvandyke.org/erin-twiki-emacs/
;; TWiki syntax highlighting
;disabled; ;(require 'erin)
;disabled; (my-load-local-el "contrib/erin.el")


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
;;disabled; 	))
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
(add-to-list 'load-path (expand-file-name "~/.emacs.d/contrib/emacs-open-resource-read-only"))
(require 'open-resource)
(global-set-key "\C-cr" 'open-resource)


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

(global-set-key (kbd "ESC <up>") 'my-move-line-up)
(global-set-key (kbd "ESC <down>") 'my-move-line-down)


;; ######################################################
;; joining lines: C-c j
;; http://whattheemacsd.com//key-bindings.el-03.html
(global-set-key (kbd "C-c j")
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
(global-set-key (kbd "C-c w") 'webjump)
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
;; magit
;;disabled;; (require 'magit)
;;disabled;; 
;;disabled;; ;; full screen magit-status
;;disabled;; ;; http://whattheemacsd.com//setup-magit.el-01.html
;;disabled;; (defadvice magit-status (around magit-fullscreen activate)
;;disabled;;   (window-configuration-to-register :magit-fullscreen)
;;disabled;;   ad-do-it
;;disabled;;   (delete-other-windows))
;;disabled;; 
;;disabled;; (defun magit-quit-session ()
;;disabled;;   "Restores the previous window configuration and kills the magit buffer"
;;disabled;;   (interactive)
;;disabled;;   (kill-buffer)
;;disabled;;   (jump-to-register :magit-fullscreen))
;;disabled;; 
;;disabled;; (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)


;; ######################################################
;; http://www.emacswiki.org/emacs-es/RecentFiles
;; recently files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; ######################################################
;; http://emacswiki.org/emacs/ImenuMode
;; jump to definitions in local buffer
(imenu-add-to-menubar imenu)
;; Jump to a definition in the current file. (This is awesome.)
;(global-set-key (kbd "C-x C-i") 'ido-imenu)


;; END OF FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; eval: (flyspell-mode 1)
;; eval: (ispell-change-dictionary "en_US")
;; End:
