;;; test-quote-highlighting.el --- Test quote highlighting fix -*- lexical-binding: t -*-

;;; Commentary:
;; Test that items in single quotes are properly highlighted with darkened background

;;; Code:

(require 'periphery)

;; Test input matching the user's examples
(defvar test-warnings
  "AppDelegate.swift:462:15: warning: capture 'self' was never used
AppDelegate.swift:515:9: warning: result of call to 'subscribeToUpdates' is unused
ClassViewController.swift:147:25: warning: 'contentEdgeInsets' was deprecated in iOS 15.0: This property is ignored when using UIButtonConfiguration")

(message "Testing quote highlighting with darkened background...")
(message "Quote pattern: %s" (alist-get 'quotes periphery-highlight-patterns))
(message "Identifier face defined: %s" (facep 'periphery-identifier-face))
(message "Identifier background darkness: %d%%" periphery-identifier-background-darkness)

;; Test the face with background generation
(let ((face-with-bg (periphery--get-identifier-face)))
  (message "Identifier face with background: %s" face-with-bg)
  (message "  Foreground: %s" (plist-get face-with-bg :foreground))
  (message "  Background: %s (darkened by %d%%)"
           (plist-get face-with-bg :background)
           periphery-identifier-background-darkness)
  (message "  Weight: %s" (plist-get face-with-bg :weight)))

;; Parse the warnings
(periphery-run-parser test-warnings :compiler)

;; Check if the buffer was created
(if (get-buffer "*Periphery*")
    (progn
      (message "✓ Periphery buffer created successfully")
      (with-current-buffer "*Periphery*"
        (goto-char (point-min))
        (let ((content (buffer-string))
              (found-bg nil))
          (message "Buffer content:\n%s" content)
          ;; Check if text properties with background are applied
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let* ((face-prop (get-text-property (point) 'face)))
                (when (and face-prop
                           (listp face-prop)
                           (plist-get face-prop :background))
                  (setq found-bg t)
                  (message "Found identifier with background at pos %d: %s" (point) face-prop)))
              (goto-char (1+ (point)))))
          (if found-bg
              (message "✓ Identifiers are highlighted with darkened background")
            (message "✗ No darkened backgrounds found - check face application")))))
  (message "✗ Periphery buffer not created"))

(provide 'test-quote-highlighting)
;;; test-quote-highlighting.el ends here
