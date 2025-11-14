;;; test-background-rendering.el --- Test if backgrounds actually render -*- lexical-binding: t -*-

;;; Code:

(require 'periphery-config)

(periphery--clear-color-cache)

(message "")
(message "====================================")
(message "BACKGROUND RENDERING TEST")
(message "====================================")
(message "")

;; Test different ways to apply face with background
(let ((test-buffer (get-buffer-create "*Background Test*")))
  (with-current-buffer test-buffer
    (erase-buffer)

    (insert "Testing different face application methods:\n")
    (insert "============================================\n\n")

    ;; Method 1: Using our function that returns a plist
    (let* ((face-spec (periphery--get-identifier-face))
           (text "Method 1 (plist): 'identifier'\n"))
      (insert "Face spec: " (format "%S" face-spec) "\n")
      (let ((start (point)))
        (insert text)
        (let ((match-start (+ start 19))
              (match-end (+ start 31)))
          (put-text-property match-start match-end 'face face-spec))))

    (insert "\n")

    ;; Method 2: Using explicit plist directly
    (let* ((fg (face-foreground 'font-lock-type-face nil t))
           (bg (when (stringp fg) (periphery--darken-color fg 60)))
           (text "Method 2 (explicit plist): 'identifier'\n"))
      (insert "FG: " (format "%S" fg) " BG: " (format "%S" bg) "\n")
      (when bg
        (let ((start (point)))
          (insert text)
          (let ((match-start (+ start 28))
                (match-end (+ start 40)))
            (put-text-property match-start match-end 'face
                               (list :foreground fg :background bg :weight 'bold))))))

    (insert "\n")

    ;; Method 3: Using fallback color
    (let* ((fg "#fab387")
           (bg (periphery--darken-color fg 60))
           (text "Method 3 (fallback color): 'identifier'\n"))
      (insert "FG: " fg " BG: " bg "\n")
      (let ((start (point)))
        (insert text)
        (let ((match-start (+ start 28))
              (match-end (+ start 40)))
          (put-text-property match-start match-end 'face
                             (list :foreground fg :background bg :weight 'bold)))))

    (insert "\n")

    ;; Method 4: Using a named temporary face
    (let* ((fg "#fab387")
           (bg (periphery--darken-color fg 60))
           (text "Method 4 (temp face): 'identifier'\n"))
      (insert "FG: " fg " BG: " bg "\n")
      ;; Create a temporary face
      (defface temp-identifier-test-face
        `((t (:foreground ,fg :background ,bg :weight bold)))
        "Temporary test face")
      (let ((start (point)))
        (insert text)
        (let ((match-start (+ start 23))
              (match-end (+ start 35)))
          (put-text-property match-start match-end 'face 'temp-identifier-test-face))))

    (insert "\n\n")
    (insert "====================================\n")
    (insert "Look at each 'identifier' above.\n")
    (insert "Which ones have a visible background?\n")
    (insert "====================================\n")

    ;; Show the buffer
    (display-buffer test-buffer)
    (message "Check the *Background Test* buffer to see which methods work!")))

(provide 'test-background-rendering)
;;; test-background-rendering.el ends here
