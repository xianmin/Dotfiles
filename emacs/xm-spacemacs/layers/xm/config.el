;; 段首空格
(defun insert-big-spc (p)
  "insert a Chinese spc."
  (interactive "*p")
  (insert "　　")) ;; 中文空格
(define-key global-map "\C-x，" 'insert-big-spc)


;; 更好的书写模式
;; (global-visual-line-mode 1)
;; (setq word-wrap nil)
;; (setq overflow-newline-into-fringe nil)
(add-hook 'org-mode-hook '(lambda ()
                            ;; (setq visual-line-fringe-indicators t)
                            (visual-line-mode)
                            (if visual-line-mode
                                (setq word-wrap nil))))


;; 透明度
(defun adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(global-set-key (kbd "M-C-8") '(lambda () (interactive) (adjust-opacity nil -10)))
(global-set-key (kbd "M-C-9") '(lambda () (interactive) (adjust-opacity nil 10)))
(global-set-key (kbd "M-C-0") '(lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))


;;;; org-mode
;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-brain-agenda)
;; (setq org-image-actual-width '(400))  ;; 控制在 orgmode 中显示图片的大小为 400 px

;; orgmode 编辑 src 会空两格的问题
(setq org-edit-src-content-indentation 0)
(setq org-src-tab-acts-natively t)

;; refile
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; export markdown
(eval-after-load "org"
  '(require 'ox-md nil t))


(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                          (sequence "WAITING(w)" "|")
                          (sequence "|" "CANCELED(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "magenta" :weight bold)
        ("CANCELED" :foreground "dim gray" :weight bold)
        ("READING" :foreground "red" :weight bold)
        ))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Write/brain/GTD.org" "INBOX")
         "* TODO %?\n%U\n%a\n")
        ("j" "Journal" entry (file "~/Dropbox/Write/brain/Journal.org")
         "* %u %?")))


;; org-hugo capture
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* (;; http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/
           (date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
           (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "** TODO " title "     :@随笔:")
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ;; ,(concat ":EXPORT_DATE: " date) ;Enter current date and time
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+headline "~/Dropbox/Write/blog/orgpost/0000-posts.org" "INBOX")
                 (function org-hugo-new-subtree-post-capture-template))))

(spacemacs/set-leader-keys "av" 'org-capture)


;; from http://stackoverflow.com/questions/1218238/how-to-make-part-of-a-word-bold-in-org-mode
;; (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
;; (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;; load-file
(push "~/.emacs.d/xm/" load-path)
(require 'auto-save)            ;; 加载自动保存模块
(auto-save-enable)              ;; 开启自动保存功能
(setq auto-save-slient t)       ;; 自动保存的时候静悄悄的， 不要打扰我

(require 'chinese-font)

(require 'site-publish)
