
;; ######################################################
;; load MISC contrib packages
(my-load-local-el "contrib/org-mode/contrib/lisp/org-checklist.el")
(my-load-local-el "contrib/org-mode/contrib/lisp/org-depend.el")
(my-load-local-el "contrib/org-mode/contrib/lisp/org-expiry.el")


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
(when (my-system-is-gary)
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
(add-to-list 'load-path "~/.emacs.d/contrib/calfw/")
(require 'calfw-org)
;; call calendar using:  M-x cfw:open-org-calendar

;; ######################################################
;; provide a command to use calfw in org-agenda-custom-commands
;; https://github.com/kiwanami/emacs-calfw/issues/18
;; 2013-01-16: does not work (yet?)
(defun open-calfw-agenda-org(&rest args)
  (let
	(
	 ;; do not duplicate deadlines
	 (org-deadline-warning-days 0)
	 )
    (cfw:open-org-calendar)
    )
  )


(setq org-agenda-files (append (quote (
			       "~/share/all/org-mode/phd.org"
			       "~/share/all/org-mode/infonova.org"
;			       "~/share/all/org-mode/test-phd.org"
			       "~/share/all/org-mode/misc.org"
			       "~/share/all/org-mode/tagstore.org"
			       "~/share/all/org-mode/IST.org"
			       "~/share/all/org-mode/contacts.org"
			       "~/share/all/org-mode/postdoc.org"
			       "~/share/all/org-mode/foodandbeverages.org"
			       "~/share/all/org-mode/hardware.org"
			       "~/share/all/org-mode/notes.org"
			       "~/share/all/org-mode/movies.org"
			       "~/share/all/org-mode/references.org"
			       "~/src/lazyblorg/dev/lazyblorg.org"
			       )
				      )
			       (file-expand-wildcards "~/share/all/org-mode/memacs/*.org")
			       )
      )

;; (setq org-agenda-files (quote (
;; 			       "~/share/all/org-mode/misc.org"
;; 			       "~/share/all/org-mode/tagstore.org"
;; 			       "~/share/all/org-mode/IST.org"
;; 			       "~/share/all/org-mode/contacts.org"
;; 			       "~/share/all/org-mode/postdoc.org"
;; 			       "~/share/all/org-mode/foodandbeverages.org"
;; 			       "~/share/all/org-mode/hardware.org"
;; 			       "~/share/all/org-mode/notes.org"
;; 			       "~/share/all/org-mode/movies.org"
;; 			       "~/share/all/org-mode/references.org"
;;       '(file-expand-wildcards "~/share/all/org-mode/memacs/*.org")
;;       '(file-expand-wildcards "~/share/all/org-mode/memacs/git/*.org")
;; 			       )
;; 			      )
;;       )



(setq org-contacts-files "~/share/all/org-mode/contacts.org")


;;; http://doc.norang.ca/org-mode.html
;;;
;;; Org Mode
;;;
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;(setq mac-command-modifier 'apple)
;(global-set-key [(<apple> <up>)] 'org-move-subtree-up)
;(global-set-key "\S-<down>" 'org-move-subtree-down)
;(global-set-key "\S-<left>" 'org-do-promote)
;(global-set-key "\S-<right>" 'org-do-demote)


;; Make TAB the yas trigger key in the org-mode-hook and enable flyspell mode and autofill
(add-hook 'org-mode-hook
          (lambda ()
;            ;; yasnippet
;            (make-variable-buffer-local 'yas/trigger-key)
;            (org-set-local 'yas/trigger-key [tab])
;            (define-key yas/keymap [tab] 'yas/next-field)
            ;; flyspell mode for spell checking everywhere
            (flyspell-mode 1)
            ;; Undefine C-c [ and C-c ] since this breaks my org-agenda files when directories are include
            ;; It expands the files in the directories individually
            (org-defkey org-mode-map "\C-c["    'undefined)
            (org-defkey org-mode-map "\C-c]"    'undefined)
;            (local-set-key (kbd "C-c M-o") 'bh/mail-subtree)
	    )
	  )

;(setq org-agenda-files (quote ("~/misc_documents/org-mode")))

;; Custom Key Bindings
;(global-set-key (kbd "<f12>") 'org-agenda)
;(global-set-key (kbd "<f5>") 'bh/org-todo)
;(global-set-key (kbd "<S-f5>") 'bh/widen)
;(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
;(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
;(global-set-key (kbd "<f9> b") 'bbdb)
;(global-set-key (kbd "<f9> c") 'calendar)
;(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
;(global-set-key (kbd "<f9> g") 'gnus)
;(global-set-key (kbd "<f8>") 'bh/hide-other)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-shifttab)
    (org-reveal)
    (org-cycle)))

;; (defun bh/set-truncate-lines ()
;;   "Toggle value of truncate-lines and refresh window display."
;;   (interactive)
;;   (setq truncate-lines (not truncate-lines))
;;   ;; now refresh window display (an idiom from simple.el):
;;   (save-excursion
;;     (set-window-s1tart (selected-window)
;;                       (window-start (selected-window)))))
;;
;; (global-set-key (kbd "<f9> i") 'info)
;;
;; (global-set-key (kbd "<f9> I") 'bh/clock-in)
;; (global-set-key (kbd "<f9> O") 'bh/clock-out)
;;
;; (defun bh/make-org-scratch ()
;;   (interactive)
;;   (find-file "/tmp/publish/org-scratch.org")
;;   (gnus-make-directory "/tmp/publish"))
;;
;; (global-set-key (kbd "<f9> o") 'bh/make-org-scratch)
;;
;; (global-set-key (kbd "<f9> r") 'boxquote-region)
;; (global-set-key (kbd "<f9> s") 'bh/switch-to-org-scratch)
;;
;; (defun bh/switch-to-org-scratch ()
;;   "Switch to a temp Org buffer.
;; If the region is active, insert it."
;;   (interactive)
;;   (let ((contents
;;          (and (region-active-p)
;;               (buffer-substring (region-beginning)
;;                                 (region-end)))))
;;     (find-file "/tmp/org-scratch.org")
;;     (if contents (insert contents))))
;;
;; (global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
;; (global-set-key (kbd "<f9> u") 'bh/untabify)
;;
;; (defun bh/untabify ()
;;   (interactive)
;;   (untabify (point-min) (point-max)))
;;
;(global-set-key (kbd "<f9> v") 'visible-mode)
;(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
;(global-set-key (kbd "C-<f9>") 'previous-buffer)
;(global-set-key (kbd "C-x n r") 'narrow-to-region)
;(global-set-key (kbd "C-<f10>") 'next-buffer)
;; (global-set-key (kbd "<f11>") 'org-clock-goto)
;; (global-set-key (kbd "C-<f11>") 'org-clock-in)
;(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
;; (global-set-key (kbd "M-<f11>") 'org-resolve-clocks)
;;(global-set-key (kbd "C-M-r") 'org-capture)
;(global-set-key (kbd "M-<f9>") (lambda ()
;                                 (interactive)
;                                 (unless (buffer-modified-p)
;                                   (kill-buffer (current-buffer)))
;                                 (delete-frame)))

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

;;; http://orgmode.org/org.html#Setting-up-capture
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

;;; 2011-05-16: #org-mode Tipp von madalu: Eingabe von Appointments
;;; ("a" "Appointment" entry
;;;   (file "~/org/inbox.org")
;;;   "* %^{Appt}\n %^t%?\n %U\n %a\n\n %i")


;; ;; Remove empty LOGBOOK drawers on clock out
;; (defun bh/remove-empty-drawer-on-clock-out ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line 0)
;;     (org-remove-empty-drawer-at "LOGBOOK" (point))))
;;
;; (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)



; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
; These are set via the customize interface since setq's do not
; work according to the docstring
;
; '(ido-mode (quote both) nil (ido))
; '(ido-everywhere t)

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
;2012-12-10 deactivated;	      ("w" "Waiting and Postponed tasks" todo "WAITING|SOMEDAY"
;2012-12-10 deactivated;               ((org-agenda-overriding-header "Waiting Tasks")))
;;              ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE"
;;               ((org-agenda-todo-ignore-with-date nil)
;;                (org-agenda-todo-ignore-deadlines nil)
;;                (org-agenda-todo-ignore-scheduled nil)
;;                (org-agenda-todo-ignore-timestamp nil)
;;                (org-agenda-overriding-header "Tasks to Refile")))
;;              ("N" "Notes" tags "NOTE"
;;               ((org-agenda-overriding-header "Notes")))

	      ;; 2012-12-07 ideas from: http://doc.norang.ca/org-mode.html#CustomAgendaViews

;use agenda and / RET instead;              ("i" "important tasks (-reward)" agenda ""
;use agenda and / RET instead;	       (
;use agenda and / RET instead;		(org-agenda-skip-function '(my-skip-tag "reward"))
;use agenda and / RET instead;;		(org-agenda-skip-function '(my-skip-tag "lp"))
;use agenda and / RET instead;		))

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

;              ("R" "grab reward" tags-todo "reward/!TODO|SOMEDAY"
;	       (
;		(org-agenda-overriding-header "rewards: TODO or SOMEDAY")
;		))

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

;;disabled;; 	      ("x" "Borrowed"
;;disabled;; 	       ((agenda ""
;;disabled;; 			((org-agenda-skip-function 'tag-without-done-or-canceled)
;;disabled;; 			 (org-agenda-overriding-header "Fun: ")))))

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



; Tags with fast selection keys
;;; http://orgmode.org/org.html#Setting-tags
(setq org-tag-alist (quote (
                            ("Kommunikation" . ?k)
                            ("Besorgung" . ?B)
                            ("nonComputer" . ?n)
                            ("fitness" . ?f)
			    (:startgroup)
                            ("@ALW" . ?a)
                            ("@TUG" . ?t)
                            ("@Stadt" . ?s)
                            ("@out_of_town" . ?o)
                            ("@Ebreichsdorf" . ?e)
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
;			    (:newline)

; Allow setting single tags without the menu
; http://orgmode.org/org.html#Setting-tags
;(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)



;;; http://orgmode.org/org.html#Weekly_002fdaily-agenda
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
;;; Based on http://article.gmane.org/gmane.emacs.orgmode/41427
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



;(setq org-archive-mark-done t)



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
;(require 'org-babel)
;(require 'org-babel-init)
;(require 'org-babel-gnuplot)

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
;;; Do not prompt to confirm evaluation
;;; This may be dangerous - make sure you understand the consequences
;;; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)



(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))

;(require 'yasnippet)
;(yas/initialize)
;(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

;; Make TAB the yas trigger key in the org-mode-hook and enable flyspell mode and autofill
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
;            (make-variable-buffer-local 'yas/trigger-key)
;            (org-set-local 'yas/trigger-key [tab])
;            (define-key yas/keymap [tab] 'yas/next-field-group)
            ;; flyspell mode for spell checking everywhere
            (flyspell-mode 1)
            ;; auto-fill mode on
            (auto-fill-mode 1)))



; (global-set-key (kbd "<f5>") 'bh/org-todo)
;
; (defun bh/org-todo ()
;   (interactive)
;   (org-narrow-to-subtree)
;   (org-show-todo-tree nil))
;
; (global-set-key (kbd "<S-f5>") 'bh/widen)
;
; (defun bh/widen ()
;   (interactive)
;   (widen)
;   (org-reveal))



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
;(setq org-agenda-skip-timestamp-if-done t)



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
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

     ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

     ; late scheduled items next
  ;;   ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

     ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

     ; late deadlines next
  ;;   ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

     ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     ; if b also matches leave unsorted
     (if (apply ,fn (list ,b))
         (setq result nil)
       (setq result -1)))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
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




;(add-to-list 'load-path (expand-file-name "~/misc_documents/org-mode-mode/contrib/lisp"))

(require 'org-checklist)


(setq org-enforce-todo-dependencies t)

(setq org-hide-leading-stars nil)

(setq org-startup-indented t)

;;; http://orgmode.org/worg/org-faq.html
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


; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-bbdb org-bibtex org-crypt org-gnus org-id org-info org-jsinfo org-habit org-inlinetask org-irc org-mew org-mhe org-protocol org-rmail org-vm org-wl org-w3m)))
; global STYLE property values for completion
(setq org-global-properties (quote (("STYLE_ALL" . "habit"))))
; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)



(setq global-auto-revert-mode t)



(require 'org-crypt)
; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; GPG key to use for encryption
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

;(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)




(setq org-return-follows-link t)




(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))



(setq org-remove-highlights-with-change nil)

(setq org-read-date-prefer-future nil)

(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-"))))

(setq split-width-threshold 9999)

;;; http://orgmode.org/org.html show blocked tasks in agenda in gray color
(setq org-agenda-dim-blocked-tasks t)

;;; automatically change to DONE when all children are done
;;; http://orgmode.org/org.html#Breaking-down-tasks
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
;;; statistic cookies count ALL subtasks not only direkt ones
(setq org-hierarchical-todo-statistics t)


;;; http://repo.or.cz/w/org-mode.git?a=blob_plain;f=contrib/lisp/org-expiry.el;hb=HEAD
;;; Expiry dates handling
(require 'org-expiry)
;;; enables automatically CREATED properties:
(org-expiry-insinuate)
;; not checked yet: (setq org-expiry-handler-function 'org-expiry-archive-subtree)

;;; http://orgmode.org/org.html#MobileOrg
;;; directory where to store MobileOrg-files
(setq org-mobile-directory "~/share/all/org-mode/mobile-org/")
(setq org-directory "~/share/all/org-mode")
(setq org-mobile-inbox-for-pull "~/share/all/org-mode/inbox.org")
(setq org-mobile-force-id-on-agenda-items nil) ;; do not generate IDs for all headings
(setq org-mobile-files (quote
		    (
		     "/Volumes/moe/data/share/all/org-mode/misc.org"
		     "/Volumes/moe/data/share/all/org-mode/contacts.org"
		     "/Volumes/moe/data/share/all/org-mode/hardware.org"
		     )))


;;; http://orgmode.org/worg/org-hacks.html#sec-1_3
;;; preserve top level node and tags when archiving
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


;;; show state changes in log-mode of agenda
(setq org-agenda-log-mode-items (quote (state)))

;;; http://orgmode.org/worg/org-faq.html
;(setq org-agenda-skip-additional-timestamps-same-entry t)
(setq org-agenda-skip-additional-timestamps-same-entry nil)

;;; open agenda in same buffer, full size
(setq org-agenda-window-setup 'current-window)

;;; add diary entries in agenda view
;;; http://orgmode.org/org.html#Weekly_002fdaily-agenda
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
	("mbox2001" . "file:~/archive/events_memories/backup_2002-06-23/2002-06-23/home/vk/Emails_from_approx_2000-07-01_to_2002-06-24.mbox::/\%s/")
	("postings2002" . "file:~/archive/usenet/memacs-archive/2002-03-13_to_2002-06-23_postings_Karl_Voit_usenet::%s")
	("postings2001" . "file:~/archive/usenet/memacs-archive/2000-07-06_to_2002-01-28_postings_Karl_Voit_usenet::%s")
	("bank" . "file:~/institutions/easybank/Memacs-easybank-summary.csv::%s")
	))

;; tries to format a new BibTeX entry according to own format
;; from: http://www.mfasold.net/blog/2009/02/using-emacs-org-mode-to-draft-papers/
;(my-load-local-el "contrib/format-bib.el")
;; does not work well enough


;; ######################################################
;; encrypting whole files:
;; http://orgmode.org/worg/org-tutorials/encrypting-files.html
(require 'epa-file)
;(epa-file-enable) ;; 2013-08-22: already enabled (somewhere)

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
    ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;resort to default auto save setting
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
	("p" "PhD Templates")
	("ps" "PhD shorts" entry (file+headline "~/share/all/org-mode/phd.org" "shorts")
	 "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("pe" "PhD event" entry (file+headline "~/share/all/org-mode/phd.org" "Events")
	 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("t" "tagstore Templates")
	("ts" "tagstore shorts" entry (file+headline "~/share/all/org-mode/tagstore.org" "shorts")
	 "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("te" "tagstore event" entry (file+headline "~/share/all/org-mode/tagstore.org" "Events")
	 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("b" "Besorgung" entry (file+headline "~/share/all/org-mode/hardware.org" "Besorgungen")
	 "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("C" "Clipboard" entry (file+headline "~/share/all/org-mode/misc.org" "shorts")
	 "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%x\n\n" :empty-lines 1)
	("c" "capture to inbox, refile later" entry (file "~/share/all/org-mode/inbox.org")
	 "\n* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("m" "movie" entry (file+headline "~/share/all/org-mode/movies.org" "inbox")
	 "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
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
;(add-hook 'org-mode-hook
;                    (lambda ()
;                      (org-set-local 'yas/trigger-key [tab])
;                      (define-key yas/keymap [tab] 'yas/next-field)))
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
;                  (org-agenda-add-entry-text-maxlines 5)
                  (htmlize-output-type 'css)))

;; ######################################################
;; setting timezone in order to get a correct iCal export
;; http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00134.html
(setq org-icalendar-timezone "Europe/Vienna")

;; ######################################################
;; setting destination file for iCal export
;; http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00163.html
;; http://orgmode.org/worg/org-tutorials/org-google-sync.html
; does not work
;(setq org-combined-agenda-icalendar-file "~/public_html/orgmodevk478.ics")

;; ######################################################
;; iCal-export to Google
;; http://orgmode.org/worg/org-tutorials/org-google-sync.html
;;; define categories that should be excluded
(setq org-export-exclude-category (list "google" "private"))
;;; define filter. The filter is called on each entry in the agenda.
;;; It defines a regexp to search for two timestamps, gets the start
;;; and end point of the entry and does a regexp search. It also
;;; checks if the category of the entry is in an exclude list and
;;; returns either t or nil to skip or include the entry.
(defun org-mycal-export-limit ()
  "Limit the export to items that have a date, time and a range. Also exclude certain categories."
  (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\
\)>")
  (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
  (save-excursion
    ; get categories
    (setq mycategory (org-get-category))
    ; get start and end of tree
    (org-back-to-heading t)
    (setq mystart    (point))
    (org-end-of-subtree)
    (setq myend      (point))
    (goto-char mystart)
    ; search for timerange
    (setq myresult (re-search-forward org-tstr-regexp myend t))
    ; search for categories to exclude
    (setq mycatp (member mycategory org-export-exclude-category))
    ; return t if ok, nil when not ok
    (if (and myresult (not mycatp)) t nil)))
;;; activate filter and call export function
(defun org-mycal-export ()
  (let ((org-icalendar-verify-function 'org-mycal-export-limit))
    (org-export-icalendar-combine-agenda-files)))


;; ######################################################
;; org-mode export of calendar events to Google
;; http://orgmode.org/worg/org-tutorials/org-google-sync.html

;;; define categories that should be excluded
(setq org-export-exclude-category (list "google" "private"))

;;disabled; ;;; define filter. The filter is called on each entry in the agenda.
;;disabled; ;;; It defines a regexp to search for two timestamps, gets the start
;;disabled; ;;; and end point of the entry and does a regexp search. It also
;;disabled; ;;; checks if the category of the entry is in an exclude list and
;;disabled; ;;; returns either t or nil to skip or include the entry.
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
;;disabled; ;;; activate filter and call export function
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
  (org-agenda-list nil nil 60)
  (org-agenda-write "~/share/all/org-mode/org-export.ics")
  (setq scriptpath "~/src/postprocess_Org-mode_iCal_export/")
  (setq icspath "~/share/all/org-mode/")
  (shell-command-to-string (concat
			    scriptpath "postprocess_Org-mode_iCal_export.py "
			    "-i " icspath "org-export.ics "
			    "-o " icspath "agenda.ics "
			    "--overwrite"
			    )
			   )
  (when (my-system-is-gary)
    (shell-command-to-string "/home/vk/bin/vk-do-unison-sync-unattended-share-all_if_host_is_reachable.sh")
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

;; ######################################################
;; Custom Link Completion
;; http://draketo.de/light/english/free-software/custom-link-completion-org-mode-25-lines-emacs
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



;  ;; ######################################################
;  ;; Automatic asynchronous publish calendar in idle time.
;  ;; modified from:
;  ;;   Automatic asynchronous org-mobile-push in idle time.
;  ;;   https://gist.github.com/3111823
;  ;;   Show a notification when a push has been completed
;
;  (require 'notifications)  ;; FIXXME: Cannot open load file: notifications
;  (defun notify-calendar-export (result)
;    (notifications-notify
;     :title "Calendar export complete"
;     :body  (format "mybody: %s" result)
;    )
;  )
;
;  ;; Fork the work
;  (require 'async) ;; FIXXME: Cannot open load file: async
;  (defun my-generate-ical-calendar-export ()
;    (async-start
;     ;; What to do in the child process
;     `(lambda ()
;        ;,(async-inject-variables "org-\\(mobile-\\|directory\\)")
;        ;(org-mobile-push)
;        (org-agenda-list nil nil 60)
;        (org-agenda-write "~/share/all/org-mode/agenda.ics")
;        )
;
;     ; What to do when it finishes
;     (lambda (result)
;       ;(notify-calendar-export result)
;       nil
;       )))
;
;  ;; Define a timer variable
;  (defvar my-generate-ical-export-timer nil
;    "Timer that `my-generate-ical-export-timer' used to reschedule itself, or nil.")
;
;  ;; Push to mobile when the idle timer runs out
;  (defun my-export-ical-with-delay (secs)
;    (when my-generate-ical-export-timer
;      (cancel-timer my-generate-ical-export-timer))
;    (setq my-generate-ical-export-timer
;          (run-with-idle-timer
;           (* 1 secs) nil 'my-generate-ical-calendar-export)))
;
;  ;; After saving files, start a 30 seconds idle timer after which we
;  ;; are going to push
;  (add-hook 'after-save-hook
;   (lambda ()
;     (when (eq major-mode 'org-mode)
;       (dolist (file (org-mobile-files-alist))
;         (if (string= (expand-file-name (car file)) (buffer-file-name))
;             (my-export-ical-with-delay 30)))
;     )))
;
;  ;; At least run it once a day, but no need for a delay this time
;  (run-at-time "00:05" 86400 '(lambda () (my-export-ical-with-delay 1)))

;; ######################################################
;; Sticky agendas remain opened in the background so that you don't
;; need to regenerate them each time you hit the corresponding
;; keystroke. This is a big time saver.
;(setq org-agenda-sticky t)

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


; ;; ######################################################
; ;; change font for DONE tasks
; ;; https://lists.gnu.org/archive/html/emacs-orgmode/2007-03/msg00179.html
; (setq org-fontify-done-headline t)
; (custom-set-faces
;  '(org-done ((t (:foreground "PaleGreen"
;                  :weight normal
;                  :strike-through t))))
;  '(org-headline-done
;             ((((class color) (min-colors 16) (background dark))
;                (:foreground "LightSalmon" :strike-through t)))))


;; ######################################################
;; managing bookmarks with Org-mode
;; http://orgmode.org/worg/org-contrib/org-protocol.html
(require 'org-protocol)


;; ######################################################
;; org-favtable
(require 'org-favtable)
(setq org-favtable-id "my-favtable")
(global-set-key (kbd "C-+") 'org-favtable)

;; ######################################################
;; org-feed - aggregating RSS feeds in news.org file:
; (setq org-feed-alist
;       '(
; 	("heise"
;          "http://www.heise.de/newsticker/heise.rdf"
;          "~/share/all/org-mode/news.org" "heise")
; 	("Dilbert"
;          "http://feeds.feedburner.com/DilbertDailyStrip"
;          "~/share/all/org-mode/news.org" "Dilbert")
; 	)
;       )


;; ######################################################
;; adding Org-mode docu
(require 'info)
(add-to-list 'Info-additional-directory-list "~/.emacs.d/contrib/org-mode/doc/")

;; ######################################################
;;; 2013-08-22:
;;; From: Suvayu Ali <fatkasuvayu+linux@gmail.com>
;;; Newsgroups: gmane.emacs.orgmode
;;; Subject: Re: Editing HTML blocks: no special environment to edit here
;;; docu: http://orgmode.org/worg/exporters/ox-overview.html and http://orgmode.org/worg/org-8.0.html
(require 'ox-html)
(require 'ox-latex)
(require 'ox-koma-letter)
(require 'ox-beamer)
(require 'ox-ascii)
;(require 'ox-odt)
(require 'ox-freemind)
(require 'ox-taskjuggler)


;; END OF FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
