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
      (setq org-brain-path "~/Write/brain")
      (setq org-id-track-globally t)
      (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
      (setq org-brain-visualize-default-choices 'all)
      )))

(defun xm/init-easy-hugo ()
  (use-package easy-hugo
    :init
    (progn
      (setq easy-hugo-basedir "~/test/quickstart/")
      (setq easy-hugo-url "https://xianmin.org")
      (setq easy-hugo-previewtime "300")
      (spacemacs/set-leader-keys "ah" 'easy-hugo)
      (setq easy-hugo-default-ext ".org")
      (setq easy-hugo-postdir "content/posts")
      )))

;;; packages.el ends here
