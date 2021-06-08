;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename:      $HOME/.emacs.d/init.el
;; Time-stamp:    <2021-06-06 17:53:58 vk>
;; Source:        https://github.com/novoid/dot-emacs
;; Purpose:       configuration file for Emacs
;; Authors:       Karl Voit
;; License:       This file is licensed under the GPL v2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ================================================================================================
;; For a brief description of my configuration, please take a look at config.org in this directory!
;; ================================================================================================



;; log starting time to come up with a duration afterwards

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar my-init-el-start-time (current-time) "Time when init.el was started")
(setq my-user-emacs-directory "~/.emacs.d/")

;; set paths to manually installed Org-mode (from git; instead of built-in Org-mode)
(add-to-list 'load-path (concat my-user-emacs-directory "contrib/org-mode/contrib/lisp"))
(add-to-list 'load-path (concat my-user-emacs-directory "contrib/org-mode/lisp"))
;(setq load-path (delete "/usr/share/emacs/28.0.50/lisp/org" load-path));; disabling built-in org on floyd - didn't help
(require 'org)



;; =======================================================================================
;; The init.el file looks for "config.org" and tangles its elisp blocks (matching
;; the criteria described below) to "config.el" which is loaded as Emacs configuration.
;; Inspired and copied from: http://www.holgerschurig.de/en/emacs-init-tangle/
;; As of 2021-02-05, the Domain "holgerschurig.de" doesn't exist any more.
;; Visit archived page on https://archive.org/search.php?query=http%3A%2F%2Fwww.holgerschurig.de%2Fen%2Femacs-init-tangle%2F
;; =======================================================================================


;; from: http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
(defvar current-date-time-format "%a %b %d %Y-%m-%dT%H:%M:%S "
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

;; from: http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")


(defun my-tangle-config-org ()
  "This function will write all source blocks from =config.org= into =config.el= that are ...

- not marked as =tangle: no=
- doesn't have the TODO state =DISABLED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
         (output-file (concat my-user-emacs-directory "config.el"))
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "—————• Re-generating %s …" output-file)
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks (concat my-user-emacs-directory "config.org")
	  (let* (
		 (org_block_info (org-babel-get-src-block-info 'light))
		 ;;(block_name (nth 4 org_block_info))
		 (tfile (cdr (assq :tangle (nth 2 org_block_info))))
		 (match_for_TODO_keyword)
		 )
	    (save-excursion
	      (catch 'exit
		;;(when (string= "" block_name)
		;;  (message "Going to write block name: " block_name)
		;;  (add-to-list 'body-list (concat "message(\"" block_name "\")"));; adding a debug statement for named blocks
		;;  )
		(org-back-to-heading t)
		(when (looking-at org-outline-regexp)
		  (goto-char (1- (match-end 0))))
		(when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
		  (setq match_for_TODO_keyword (match-string 1)))))
	    (unless (or (string= "no" tfile)
			(string= "DISABLED" match_for_TODO_keyword)
			(not (string= "emacs-lisp" lang)))
	      (add-to-list 'body-list (concat "\n\n;; #####################################################################################\n"
					      "(message \"config • " (org-get-heading) " …\")\n\n")
			   )
	      (add-to-list 'body-list body)
	      ))))
      (with-temp-file output-file
        (insert ";; ============================================================\n")
        (insert ";; Don't edit this file, edit config.org' instead ...\n")
        (insert ";; Auto-generated at " (format-time-string current-date-time-format (current-time)) " on host " system-name "\n")
        (insert ";; ============================================================\n\n")
        (insert (apply 'concat (reverse body-list))))
      (message "—————• Wrote %s" output-file))))


;; following lines are executed only when my-tangle-config-org-hook-func()
;; was not invoked when saving config.org which is the normal case:
(let ((orgfile (concat my-user-emacs-directory "config.org"))
      (elfile (concat my-user-emacs-directory "config.el"))
      (gc-cons-threshold most-positive-fixnum))
  (when (or (not (file-exists-p elfile))
            (file-newer-than-file-p orgfile elfile))
    (my-tangle-config-org)
    ;;(save-buffers-kill-emacs);; TEST: kill Emacs when config has been re-generated due to many issues when loading newly generated config.el
    )
  (load-file elfile))

;; when config.org is saved, re-generate config.el:
(defun my-tangle-config-org-hook-func ()
  (when (string= "config.org" (buffer-name))
	(let ((orgfile (concat my-user-emacs-directory "config.org"))
		  (elfile (concat my-user-emacs-directory "config.el")))
	  (my-tangle-config-org))))
(add-hook 'after-save-hook 'my-tangle-config-org-hook-func)


(message "→★ loading init.el in %.2fs" (float-time (time-subtract (current-time) my-init-el-start-time)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "Sent" :query "folder:archive")
     (:name "this_week" :query "date:\"this week\"")
     (:name "today¬me" :query "date:today and not from:karl.voit")))
 '(org-agenda-files
   '("~/org/rise.org" "~/org/misc.org" "~/org/issues.org" "~/org/projects.org" "~/org/finanzen_behoerden_versicherungen.org" "~/org/bwg.org" "~/org/contacts.org" "~/org/hardware.org" "~/org/fhsp.org" "~/org/notes.org" "~/org/public_voit.org" "~/org/errors.org" "~/org/errors_public_voit.org" "~/org/memacs/error.org"))
 '(org-contacts-address-property "CITY" t)
 '(org-contacts-birthday-property "BORN" t)
 '(org-contacts-icon-property "PHOTOGRAPH" t)
 '(package-selected-packages
   '(helm-pass password-generator pass image-dired+ eglot lsp-docker which-key dap-python dap-mode helm-lsp lsp-ui lsp-mode keycast projectile elisp-bug-hunter bug-hunter spatial-navigate org-edna org-ql org-super-links quelpa-use-package quelpa orly helm-org-rifle ace-window vimgolf lorem-ipsum gif-screencast pandoc-mode tabbar session pod-mode notmuch muttrc-mode mutt-alias markdown-mode initsplit htmlize graphviz-dot-mode folding ess eproject diminish csv-mode browse-kill-ring boxquote bm bar-cursor auto-complete apache-mode ag yankpad yafolding wttrin use-package unicode-fonts spray smart-mode-line scss-mode pcre2el ox-reveal ox-pandoc ox-gfm org-table-sticky-header org-pdfview org-bullets nyan-mode mode-icons magit json-mode hydra highlight-symbol helm-org helm-dired-history gnuplot gcmh flycheck elpy dumb-jump counsel char-menu anzu alert adoc-mode))
 '(safe-local-variable-values
   '((eval org-expiry-deinsinuate)
     (eval setq org-image-actual-width 600)
     (eval setq org-image-actual-width 20)
     (eval remove-hook 'org-after-tags-change-hook 'org-expiry-insert-created t)
     (eval make-local-variable 'org-after-tags-change-hook)
     (eval remove-hook 'org-after-todo-state-change-hook 'org-expiry-insert-created t)
     (eval make-local-variable 'org-after-todo-state-change-hook)
     (eval remove-hook 'org-insert-heading-hook 'org-expiry-insert-created t)
     (eval make-local-variable 'org-insert-heading-hook)
     (eval ispell-change-dictionary "german8")
     (eval ispell-change-dictionary "american")
     (eval ispell-change-dictionary "en_US")
     (flyspell-default-dictionary . "german8")))
 '(tramp-default-user "vk" nil (tramp)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
