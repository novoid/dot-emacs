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
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end]  'end-of-buffer)

(define-key my-map "g" 'goto-line)
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
;(global-set-key [M-left] 'backward-word)
;(global-set-key [M-right] 'forward-word)
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
(my-load-local-el "contrib/boxquote.el")
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



;; ######################################################
;; recently files
(define-key my-map "r" 'recentf-open-files)

;; END OF FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; eval: (flyspell-mode 1)
;; eval: (ispell-change-dictionary "en_US")
;; End:
