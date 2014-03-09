;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename:      $HOME/.emacs
;; Purpose:       configuration file for Emacs
;; Authors:       Karl Voit
;; License:       This file is licensed under the GPL v2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ######################################################
;;; rest of the (common) configuration
(load "~/.emacs.d/main.el")

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
 '(safe-local-variable-values
   (quote
    ((flyspell-default-dictionary . "en_US")
     (eval ispell-change-dictionary "american")
     (eval ispell-change-dictionary "german8")
     (flyspell-default-dictionary . "german8")
     (eval ispell-change-dictionary "de_AT")
     (eval ispell-change-dictionary "en_US")
     (eval flyspell-mode 1))))
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-safe-themes
   (quote
    ("d070fa185078bf753dcfd873ec63be19fa36a55a0c97dc66848a6d20c5fffdad" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default)))
 '(inhibit-startup-screen t)
)

