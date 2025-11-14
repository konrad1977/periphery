;;; test-visible-background.el --- Test with highly visible background -*- lexical-binding: t -*-

;;; Code:

(require 'periphery-config)

;; Test with different darkness levels to find what's visible
(let ((test-buffer (get-buffer-create "*Visible Background Test*")))
  (with-current-buffer test-buffer
    (erase-buffer)

    (let ((fg "#7AA89F"))  ; Your font-lock-type-face color

      (insert "Testing different background darkness levels:\n")
      (insert "==============================================\n\n")

      ;; Test different darkness percentages
      (dolist (darkness '(20 30 40 50 60 70 80))
        (let* ((bg (periphery--darken-color fg darkness))
               (text (format "Darkness %d%%: 'identifier' (FG:%s BG:%s)\n"
                           darkness fg bg))
               (start (point)))
          (insert text)
          (let* ((match-pos (string-match "'identifier'" text))
                 (match-start (+ start match-pos))
                 (match-end (+ match-start 12)))
            (put-text-property match-start match-end 'face
                             (list :foreground fg :background bg :weight 'bold)))))

      (insert "\n")
      (insert "Testing with contrasting background:\n")
      (insert "=====================================\n\n")

      ;; Test with a very obvious background color
      (let* ((text "Obvious background: 'identifier'\n")
             (start (point)))
        (insert text)
        (let ((match-start (+ start 20))
              (match-end (+ start 32)))
          (put-text-property match-start match-end 'face
                           '(:foreground "#7AA89F" :background "#ffff00" :weight bold))))

      (insert "\n")
      (insert "If you can see the YELLOW background above but not the others,\n")
      (insert "then the darkness level is too high.\n")
      (insert "Try setting: (setq periphery-identifier-background-darkness 30)\n"))

    (display-buffer test-buffer)
    (switch-to-buffer-other-window test-buffer)
    (message "Check the *Visible Background Test* buffer!")))

(provide 'test-visible-background)
;;; test-visible-background.el ends here
