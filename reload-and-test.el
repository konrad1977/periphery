;;; reload-and-test.el --- Reload periphery and test identifier colors -*- lexical-binding: t -*-

;;; Commentary:
;; Complete reload and test of identifier highlighting

;;; Code:

(message "")
(message "===== RELOADING PERIPHERY =====")
(message "")

;; 1. Clear the color cache
(message "1. Clearing color cache...")
(periphery--clear-color-cache)

;; 2. Show current face configuration
(message "")
(message "2. Current face configuration:")
(message "   periphery-identifier-face:")
(let ((fg-direct (face-attribute 'periphery-identifier-face :foreground nil nil))
      (fg-inherit (face-attribute 'periphery-identifier-face :foreground nil t))
      (inherit (face-attribute 'periphery-identifier-face :inherit nil nil)))
  (message "     :foreground (direct) = %S" fg-direct)
  (message "     :foreground (resolved) = %S" fg-inherit)
  (message "     :inherit = %S" inherit)

  (when inherit
    (message "")
    (message "   Inherited face (%s):" inherit)
    (message "     :foreground = %S" (face-attribute inherit :foreground nil t))))

;; 3. Test color generation
(message "")
(message "3. Testing color generation:")
(let ((face-spec (periphery--get-identifier-face)))
  (message "   Generated face spec:")
  (message "     %S" face-spec)
  (message "")
  (if (plist-get face-spec :background)
      (progn
        (message "   ✓ SUCCESS: Background color generated!")
        (message "     Foreground: %s" (plist-get face-spec :foreground))
        (message "     Background: %s" (plist-get face-spec :background)))
    (progn
      (message "   ✗ PROBLEM: No background color!")
      (message "     Face spec contains :inherit, which means color resolution failed")
      (message ""))))

;; 4. Visual test
(message "")
(message "4. Visual test:")
(let* ((test-str "capture 'self' was never used")
       (face-spec (periphery--get-identifier-face))
       (start (string-match "'self'" test-str))
       (end (match-end 0)))
  (when start
    ;; Apply the face
    (setq test-str (copy-sequence test-str))
    (put-text-property start end 'face face-spec test-str)
    (message "   %s" test-str)
    (message "")
    (if (plist-get face-spec :background)
        (message "   The word 'self' above should have a colored background")
      (message "   The word 'self' above will NOT have a background (color resolution failed)"))))

;; 5. Test with actual periphery parsing
(message "")
(message "5. Testing with actual parser:")
(let ((test-input "AppDelegate.swift:462:15: warning: capture 'self' was never used"))
  (message "   Input: %s" test-input)
  (message "   Parsing...")
  (periphery-run-parser test-input :compiler)
  (message "   ✓ Check the *Periphery* buffer to see if 'self' has a background"))

(message "")
(message "===== TEST COMPLETE =====")
(message "")
(message "NEXT STEPS:")
(message "  1. Check the *Periphery* buffer")
(message "  2. Look for 'self' in the message column")
(message "  3. It should have a colored background")
(message "")
(message "If background is still not visible:")
(message "  - Run: M-x describe-face RET font-lock-type-face RET")
(message "  - Check what color it shows")
(message "  - If it shows 'unspecified', your theme may not set this face")
(message "")

(provide 'reload-and-test)
;;; reload-and-test.el ends here
