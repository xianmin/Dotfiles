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
                            ;;                            (setq visual-line-fringe-indicators t)
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

;; evil key
;; (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
;; (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-visual-line)
;; (define-key evil-insert-state-map (kbd "C-e") 'end-of-visual-line)
;; (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-visual-line)
;; (define-key evil-insert-state-map (kbd "C-n") 'evil-next-visual-line)
;; (define-key evil-insert-state-map (kbd "C-y") 'evil-paste-after)


;; org

;; export markdown
(eval-after-load "org"
  '(require 'ox-md nil t))

;; export
(setq org-export-default-language "zh_CN")
(setq org-html-head-include-default-style nil)

(setq org-publish-project-alist
      '(("website"
         :author "贤民"
         :email "xianmin12@qq.com"
         :language "zh_CN"
         :base-directory "~/Write/Xm-Website/src/"
         :publishing-directory "~/Write/Xm-Website/"
         :base-extension "org"
         :exclude "^data\\|^draft\\|^gtd"
         :recursive t
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :html-head "<link rel=\"stylesheet\"
                       href=\"style.css\" type=\"text/css\"/>"
         :html-postamble "<!-- Duoshuo Comment BEGIN -->
    <div class=\"ds-thread\"></div>
    <script type=\"text/javascript\">
    var duoshuoQuery = {short_name:\"xianmin\"};
    (function() {
             var ds = document.createElement('script');
             ds.type = 'text/javascript';ds.async = true;
             ds.src = 'http://static.duoshuo.com/embed.js';
             ds.charset = 'UTF-8';
             (document.getElementsByTagName('head')[0]
                                           || document.getElementsByTagName('body')[0]).appendChild(ds);
                                           })();
                                           </script>
                                           <!-- Duoshuo Comment END -->
<div class='footer'>
Copyright 2014 %a.<br>
Last updated %C. <br>
Built with %c.
</div>"
         )
        ("web"
         :components ("website"))
        ))


(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                          (sequence "WAITING(w)" "|")
                          (sequence "|" "CANCELED(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "magenta" :weight bold)
        ("CANCELED" :foreground "dim gray" :weight bold)
        ))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Write/0-GTD/0-gtd.org" "INBOX")
         "* TODO %?")
        ("j" "Journal" entry (file "~/Write/journal.org")
         "* %u %?")))


;; blog
;;  (require 'blog-admin)

;; (setq blog-admin-backend-type 'nikola)
;; (setq blog-admin-backend-path "~/Write/blog")
;; (setq blog-admin-backend-new-post-in-drafts t)
;; (setq blog-admin-backend-nikola-executable "/home/xm/.virtualenvs/python3/bin/nikola") ;; path to nikola executable
;; (setq blog-admin-backend-nikola-config-file "conf.py") ;; conf.py is default

;; load-file
(push "~/.emacs.d/xm/" load-path)
(require 'auto-save)            ;; 加载自动保存模块
(auto-save-enable)              ;; 开启自动保存功能
(setq auto-save-slient t)       ;; 自动保存的时候静悄悄的， 不要打扰我
