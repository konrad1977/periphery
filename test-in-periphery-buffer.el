;;; test-in-periphery-buffer.el --- Test backgrounds in actual Periphery buffer -*- lexical-binding: t -*-

;;; Code:

(require 'periphery)

(periphery--clear-color-cache)

(message "")
(message "Testing in actual Periphery buffer...")
(message "")

;; Create test data with quoted identifiers
(let ((test-data "AppDelegate.swift:462:15: warning: capture 'self' was never used
AppDelegate.swift:515:9: warning: result of call to 'subscribeToUpdates' is unused
ClassViewController.swift:147:25: warning: 'contentEdgeInsets' was deprecated in iOS 15.0"))

  (message "Input data:")
  (message "%s" test-data)
  (message "")

  ;; Parse it
  (periphery-run-parser test-data :compiler)

  (message "✓ Parsed. Check the *Periphery* buffer.")
  (message "")
  (message "Look at the Message column for:")
  (message "  - 'self'")
  (message "  - 'subscribeToUpdates'")
  (message "  - 'contentEdgeInsets'")
  (message "")
  (message "These should have a colored background.")
  (message "")
  (message "Current identifier background darkness: %d%%"
           periphery-identifier-background-darkness)
  (message "")
  (message "If you can't see the background, try:")
  (message "  (setq periphery-identifier-background-darkness 20)")
  (message "  (periphery--clear-color-cache)")
  (message "  Then reload this test.")

  ;; Switch to the buffer so user can see it
  (when-let ((buf (get-buffer "*Periphery*")))
    (display-buffer buf)
    (with-current-buffer buf
      ;; Print some debug info about the buffer contents
      (let ((found-face nil))
        (save-excursion
          (goto-char (point-min))
          (message "")
          (message "Buffer content analysis:")
          (while (not (eobp))
            (let ((face-prop (get-text-property (point) 'face)))
              (when (and face-prop
                         (listp face-prop)
                         (or (plist-member face-prop :background)
                             (and (listp (car face-prop))
                                  (plist-member (car face-prop) :background))))
                (setq found-face t)
                (message "  Found face with background at position %d: %S"
                         (point) face-prop)))
            (forward-char 1)))
        (if found-face
            (message "  ✓ Background faces found in buffer!")
          (message "  ✗ No background faces found - this is the problem!"))))))

(provide 'test-in-periphery-buffer)
;;; test-in-periphery-buffer.el ends here
