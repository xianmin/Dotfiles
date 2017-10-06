(setq org-export-default-language "zh_CN")

(setq site-path "~/Write/1-site/")
(setq publish-path "~/Write/2-publish/")
;; (setq images-path "~/Write/Xm-Website/images/")
(setq org-html-validation-link nil)
(setq org-confirm-babel-evaluate nil)

(setq postamble (with-temp-buffer
                  (insert-file-contents "~/Write/1-site/templates/postamble.html")
                  (buffer-string)))
(setq preamble (with-temp-buffer
                 (insert-file-contents "~/Write/1-site/templates/preamble.html")
                 (buffer-string)))
(setq header (with-temp-buffer
               (insert-file-contents "~/Write/1-site/templates/header.html")
               (buffer-string)))


(defun set-org-publish-project-alist ()
  "Set publishing projects for Orgweb and Worg."
  (interactive)
  (setq org-publish-project-alist
    `(("site-posts"
       ;; Directory for source files in org format
       :author "贤民"
       :email "xianmin12@qq.com"
       :language "zh_CN"
       :base-directory ,site-path
        :base-extension "org"
        :html-doctype "html5"
        :html-head ,header
        :html-html5-fancy t
        :html-preamble ,preamble
        :html-postamble ,postamble
        ;; HTML directory
        :publishing-directory ,publish-path
        :publishing-function org-html-publish-to-html
        :recursive t
        :headline-levels 4
        :with-sub-superscript nil
        :section-numbers nil
        :auto-preamble nil
        :exclude "^draft"
        :html-head-include-default-style nil
        )
       ("site-static"
         :base-directory ,site-path
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|ico"
         :publishing-directory ,publish-path
         :recursive t
         :publishing-function org-publish-attachment
         )
       ;; ("images"
       ;;   :base-directory ,images-path
       ;;   :base-extension "png\\|jpg\\|gif"
       ;;   :publishing-directory ,publish-path
       ;;   :recursive t
       ;;   :publishing-function org-publish-attachment
       ;;   )
       ;; ("rss"
       ;;   :base-directory ,blog-path
       ;;   :base-extension "org"
       ;;   :html-link-home ,config-base-url
       ;;   :html-link-use-abs-url t
       ;;   :rss-extension "xml"
       ;;   :publishing-directory ,publish-path
       ;;   :publishing-function (org-rss-publish-to-rss)
       ;;   :section-numbers nil
       ;;   :exclude ".*"            ;; To exclude all files...
       ;;   :include ("index.org")   ;; ... except index.org.
       ;;   :table-of-contents nil)
       ("blog" :components ("site-posts" "site-static"))
       )))
(set-org-publish-project-alist)

(provide 'site-publish)

;;; site-publish.el ends here
