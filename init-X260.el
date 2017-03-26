;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename:      $HOME/.emacs
;; Purpose:       configuration file for Emacs
;; Authors:       Karl Voit
;; License:       This file is licensed under the GPL v2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ######################################################
;; http://www.tbray.org/ongoing/When/201x/2012/09/24/Typographic-notes
;; setting font size
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; END OF FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("~/share/all/org-mode/archive.org" "~/share/all/org-mode/misc.org" "~/share/all/org-mode/issues.org" "~/share/all/org-mode/projects.org" "~/share/all/org-mode/finanzen_behoerden_versicherungen.org" "~/share/all/org-mode/bwg.org" "~/share/all/org-mode/contacts.org" "~/share/all/org-mode/foodandbeverages.org" "~/share/all/org-mode/hardware.org" "~/share/all/org-mode/notes.org" "~/share/all/org-mode/movies.org" "~/share/all/org-mode/references.org" "~/share/all/org-mode/public_voit.org" "~/share/all/org-mode/errors_public_voit.org" "~/share/all/org-mode/errors_orgmode_commits.org" "~/share/all/org-mode/fhsp.org" "~/src/lazyblorg/lazyblorg.org" "~/share/all/org-mode/memacs/roylog.org" "~/share/all/org-mode/memacs/bank.org" "~/share/all/org-mode/memacs/error.org" "~/share/all/org-mode/memacs/files.org" "~/share/all/org-mode/memacs/git.org" "~/share/all/org-mode/memacs/mbox.org" "~/share/all/org-mode/memacs/news.org" "~/share/all/org-mode/memacs/phonecalls.org" "~/share/all/org-mode/memacs/SMS.org" "~/share/all/org-mode/memacs/tweets.org" "~/share/all/org-mode/memacs/movies.org" "~/share/all/org-mode/memacs/Filmliste.org")))
 '(org-contacts-address-property "CITY" t)
 '(org-contacts-birthday-property "BORN" t)
 '(org-contacts-files "~/share/all/org-mode/contacts.org" t)
 '(org-contacts-icon-property "PHOTOGRAPH" t)
 '(package-selected-packages
   (quote
    (yankpad yafolding wttrin use-package unicode-fonts undo-tree synonyms suggest spray smeargle smart-mode-line scss-mode restclient ox-reveal ox-pandoc ox-clip ox-asciidoc org-table-sticky-header org-bullets nyan-mode neotree mode-icons markdown-mode magit json-mode highlight-symbol helm git-timemachine flycheck eno elpy counsel char-menu bm anzu adoc-mode)))
 '(safe-local-variable-values
   (quote
    ((TeX-master . "misc")
     (TeX-master . "references")
     (reftex-default-bibliography quote
				  ("~/archive/library"))
     (eval ispell-change-dictionary "german8")
     (eval ispell-change-dictionary "american")
     (eval ispell-change-dictionary "en_US")
     (flyspell-default-dictionary . "german8")))))

;; ######################################################
;;; rest of the (common) configuration
(load "~/.emacs.d/main.el")
