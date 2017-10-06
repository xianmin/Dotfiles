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
        ))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Write/brain/GTD.org" "INBOX")
         "* TODO %?\n%U\n%a\n")
        ("j" "Journal" entry (file "~/Write/brain/Journal.org")
         "* %u %?")))

;; from http://stackoverflow.com/questions/1218238/how-to-make-part-of-a-word-bold-in-org-mode
;; (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
;; (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;; load-file
(push "~/.emacs.d/xm/" load-path)
(require 'auto-save)            ;; 加载自动保存模块
(auto-save-enable)              ;; 开启自动保存功能
(setq auto-save-slient t)       ;; 自动保存的时候静悄悄的， 不要打扰我

(require 'site-publish)
