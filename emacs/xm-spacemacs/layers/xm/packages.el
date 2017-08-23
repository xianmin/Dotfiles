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

;; (defun xm/init-blog-admin ()
;;   (use-package blog-admin
;;     :init
;;     (progn
;;       (setq blog-admin-backend-type 'nikola)
;;       (setq blog-admin-backend-path "~/Write/blog")
;;       (setq blog-admin-backend-new-post-in-drafts t)
;;       (setq blog-admin-backend-nikola-executable "/home/xm/.virtualenvs/python3/bin/nikola") ;; path to nikola executable
;;       (setq blog-admin-backend-nikola-config-file "conf.py") ;; conf.py is default
;;       )))

;;; packages.el ends here
