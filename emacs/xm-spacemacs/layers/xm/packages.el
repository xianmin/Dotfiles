;;; packages.el --- xm layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Xianmin Chen <xianmin12@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq xm-packages
      '(
        writeroom-mode
        ;; blog-admin
        org-brain
        easy-hugo
        ox-hugo
        fcitx
        ))

(defun xm/init-writeroom-mode ()
  (use-package writeroom-mode
    :init
    (progn
      (setq writeroom-fullscreen-effect 'maximized)
      (global-set-key (kbd "<S-f11>") 'writeroom-mode)
      )))

(defun xm/init-org-brain ()
  (use-package org-brain
    :init
    (progn
      (setq org-brain-path "~/Dropbox/Write/brain")
      (setq org-id-track-globally t)
      (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
      (setq org-brain-visualize-default-choices 'all)
      )))

(defun xm/init-easy-hugo ()
  (use-package easy-hugo
    :init
    (progn
      (setq easy-hugo-basedir "~/Dropbox/Write/blog/")
      (setq easy-hugo-url "http://xianmin.org")
      (setq easy-hugo-previewtime "300")
      (spacemacs/set-leader-keys "ah" 'easy-hugo)
      (setq easy-hugo-default-ext ".md")
      (setq easy-hugo-postdir "content/post")
      )))

(defun xm/init-ox-hugo ()
  (use-package ox-hugo
    :init
    (progn
      (setq org-hugo-auto-set-lastmod t)
      (setq org-hugo-default-section-directory "post")
      (setq org-hugo-external-file-extensions-allowed-for-copying
            '("jpg" "jpeg" "tiff" "png" "svg" "pdf" "odt" "gif"))
      (spacemacs/set-leader-keys "ae" 'org-hugo-export-subtree-to-md)
      )
    :after ox
     ))

(defun xm/init-fcitx ()
  (use-package fcitx
    :init
    (progn
      ;; Make sure the following comes before `(fcitx-aggressive-setup)'
      (setq fcitx-active-evil-states '(insert emacs hybrid)) ; if you use hybrid mode
      (fcitx-aggressive-setup)
      (fcitx-prefix-keys-add "M-m") ; M-m is common in Spacemacs
      (setq fcitx-use-dbus t) ; uncomment if you're using Linux
    )))

;;; packages.el ends here
