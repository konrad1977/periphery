;;; test-inheritance.el --- Test face inheritance resolution -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive test of face inheritance and color generation

;;; Code:

(require 'periphery-config)

;; Clear cache first
(periphery--clear-color-cache)

(message "")
(message "========================================")
(message "FACE INHERITANCE TEST")
(message "========================================")
(message "")

;; 1. Check how periphery-identifier-face is defined
(message "1. Face Definition:")
(message "   periphery-identifier-face spec: %S"
         (get 'periphery-identifier-face 'face-defface-spec))

;; 2. Check inheritance
(message "")
(message "2. Inheritance Chain:")
(let ((inherit (face-attribute 'periphery-identifier-face :inherit nil 'default)))
  (message "   :inherit attribute: %S" inherit))

;; 3. Try different methods to get foreground color
(message "")
(message "3. Foreground Color Resolution:")
(message "   Method 1 - face-attribute:")
(let ((fg (face-attribute 'periphery-identifier-face :foreground nil t)))
  (message "     Result: %S (type: %s)" fg (type-of fg)))

(message "   Method 2 - face-foreground:")
(let ((fg (face-foreground 'periphery-identifier-face nil t)))
  (message "     Result: %S (type: %s)" fg (type-of fg)))

(message "   Method 3 - face-foreground with 'default frame:")
(let ((fg (face-foreground 'periphery-identifier-face 'default t)))
  (message "     Result: %S (type: %s)" fg (type-of fg)))

(message "   Method 4 - Direct from font-lock-type-face:")
(let ((fg (face-foreground 'font-lock-type-face nil t)))
  (message "     Result: %S (type: %s)" fg (type-of fg)))

;; 4. Test the actual function
(message "")
(message "4. Testing periphery--get-identifier-face:")
(let ((result (periphery--get-identifier-face)))
  (message "   Result type: %s" (type-of result))
  (message "   Result: %S" result)
  (when (listp result)
    (message "   :foreground = %S" (plist-get result :foreground))
    (message "   :background = %S" (plist-get result :background))
    (message "   :weight = %S" (plist-get result :weight))
    (message "   :inherit = %S" (plist-get result :inherit))))

;; 5. Test manual color generation
(message "")
(message "5. Manual Color Generation Test:")
(let* ((fg (face-foreground 'font-lock-type-face nil t)))
  (if (and fg (stringp fg) (not (eq fg 'unspecified)))
      (let ((bg (periphery--darken-color fg 60)))
        (message "   Foreground: %s" fg)
        (message "   Background (60%% dark): %s" bg)
        (message "   ✓ Colors generated successfully!"))
    (message "   ✗ Could not get valid foreground color from font-lock-type-face")
    (message "   This might mean the theme hasn't set this face yet")))

;; 6. Create a test string
(message "")
(message "6. Visual Test:")
(message "   Creating test string with identifier face...")
(let* ((test-str "   Test: 'identifier' here   ")
       (face-spec (periphery--get-identifier-face))
       (start (string-match "'identifier'" test-str))
       (end (match-end 0)))
  (when start
    (put-text-property start end 'face face-spec test-str)
    (message "   %s" test-str)
    (message "")
    (if (plist-get face-spec :background)
        (message "   ✓ Background should be visible on 'identifier' above")
      (message "   ✗ No background in face spec - using inheritance fallback"))))

(message "")
(message "========================================")
(message "")

;; Show recommendations
(message "RECOMMENDATIONS:")
(message "")
(message "If background is not visible, try:")
(message "  1. Check if font-lock-type-face has a foreground color:")
(message "     M-x describe-face RET font-lock-type-face RET")
(message "")
(message "  2. Set explicit color instead of inheriting:")
(message "     (set-face-attribute 'periphery-identifier-face nil")
(message "                         :foreground \"#your-color-here\"")
(message "                         :inherit nil)")
(message "")
(message "  3. Adjust darkness for better visibility:")
(message "     (setq periphery-identifier-background-darkness 40)")
(message "     (periphery--clear-color-cache)")

(provide 'test-inheritance)
;;; test-inheritance.el ends here
