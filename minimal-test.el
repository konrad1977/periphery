;;; minimal-test.el --- Minimal test of face application -*- lexical-binding: t -*-

;;; Code:

(require 'periphery-config)

(periphery--clear-color-cache)

(message "")
(message "=== MINIMAL FACE TEST ===")
(message "")

;; Test 1: Can we get a color from font-lock-type-face?
(let ((color (face-foreground 'font-lock-type-face nil t)))
  (message "1. font-lock-type-face foreground: %S" color)
  (if (and color (stringp color) (not (eq color 'unspecified)))
      (message "   ✓ Valid color found")
    (message "   ✗ No valid color - this is the problem!")))

;; Test 2: Can we generate a face spec?
(message "")
(let ((face-spec (periphery--get-identifier-face)))
  (message "2. Generated face spec: %S" face-spec)
  (if (plist-get face-spec :background)
      (message "   ✓ Has background: %s" (plist-get face-spec :background))
    (message "   ✗ No background generated")))

;; Test 3: Can we apply it to text?
(message "")
(message "3. Applying to test text:")
(let* ((text "Test 'word' here")
       (face-spec (periphery--get-identifier-face))
       (start 5)
       (end 11))
  ;; Make a mutable copy
  (setq text (copy-sequence text))
  ;; Apply the face
  (put-text-property start end 'face face-spec text)
  ;; Check what we got
  (message "   Text: %s" text)
  (message "   Face at position 6: %S" (get-text-property 6 'face text))
  (message "")
  (message "   Look at 'word' above - does it have a background color?"))

(message "")
(message "=== END TEST ===")

(provide 'minimal-test)
;;; minimal-test.el ends here
