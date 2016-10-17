(setq org-export-default-language "zh_CN")

(setq blog-path "~/Write/Xm-Website/")
(setq publish-path "~/Write/Xm-Website/publish/")
;; (setq images-path "~/Write/Xm-Website/images/")
(setq org-html-validation-link nil)
(setq org-confirm-babel-evaluate nil)

(setq postamble (with-temp-buffer
                  (insert-file-contents "~/Write/Xm-Website/templates/postamble.html")
                  (buffer-string)))
(setq preamble (with-temp-buffer
                 (insert-file-contents "~/Write/Xm-Website/templates/preamble.html")
                 (buffer-string)))
(setq header (with-temp-buffer
               (insert-file-contents "~/Write/Xm-Website/templates/header.html")
               (buffer-string)))


(defun set-org-publish-project-alist ()
  "Set publishing projects for Orgweb and Worg."
  (interactive)
  (setq org-publish-project-alist
    `(("blog-posts"
       ;; Directory for source files in org format
       :author "贤民"
       :email "xianmin12@qq.com"
       :language "zh_CN"
       :base-directory ,blog-path
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
       ("blog-static"
         :base-directory ,blog-path
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
       ("blog" :components ("blog-posts" "blog-static"))
       )))
(set-org-publish-project-alist)

(provide 'blog-publish)

;;; blog-publish.el ends here
