;;; debug-identifier-colors.el --- Debug identifier color generation -*- lexical-binding: t -*-

;;; Commentary:
;; Visualize the colors being generated for identifiers

;;; Code:

(require 'periphery-config)

;; Clear cache to ensure fresh colors
(periphery--clear-color-cache)

(message "=== Identifier Color Debug ===")
(message "")

;; Show the face inheritance
(message "Face definition:")
(message "  periphery-identifier-face inherits from: %s"
         (face-attribute 'periphery-identifier-face :inherit))

;; Show the base face foreground - using different methods
(message "")
(message "Color resolution:")
(let ((fg-attr (face-attribute 'periphery-identifier-face :foreground nil t))
      (fg-func (face-foreground 'periphery-identifier-face nil t)))
  (message "  Using face-attribute: %s" fg-attr)
  (message "  Using face-foreground: %s" fg-func)
  (message "  font-lock-type-face foreground: %s"
           (face-foreground 'font-lock-type-face nil t)))

(message "")
(message "Darkness settings:")
(message "  Severity badges: %d%%" periphery-background-darkness)
(message "  Identifiers: %d%%" periphery-identifier-background-darkness)

(message "")
(message "Color generation test:")

;; Test with different darkness levels
(dolist (darkness '(40 50 60 70 80 85 90))
  (let* ((fg "#fab387")
         (bg (periphery--darken-color fg darkness)))
    (message "  %d%% darkness: FG=%s BG=%s" darkness fg bg)))

(message "")
(message "Actual identifier face with background:")
(let ((face-spec (periphery--get-identifier-face)))
  (message "  Complete spec: %S" face-spec)
  (when (plistp face-spec)
    (message "    :foreground = %s" (plist-get face-spec :foreground))
    (message "    :background = %s" (plist-get face-spec :background))
    (message "    :weight = %s" (plist-get face-spec :weight))
    (message "    :inherit = %s" (plist-get face-spec :inherit))))

(message "")
(message "=== Color Comparison ===")
(message "Testing visibility with a sample string:")
(message "")

;; Create a test string with the identifier face applied
(let* ((test-string "  capture 'self' was never used  ")
       (face-spec (periphery--get-identifier-face))
       (start (string-match "'self'" test-string))
       (end (match-end 0)))
  (when start
    (put-text-property start end 'face face-spec test-string)
    (message "Sample: %s" test-string)
    (message "")
    (message "If you can't see the background on 'self' above,")
    (message "try lowering periphery-identifier-background-darkness")
    (message "to 40 or 50 for better visibility:")))

(message "")
(message "(setq periphery-identifier-background-darkness 40)")
(message "(periphery--clear-color-cache)")

(provide 'debug-identifier-colors)
;;; debug-identifier-colors.el ends here
