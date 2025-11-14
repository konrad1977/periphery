;;; debug-application.el --- Debug face application in detail -*- lexical-binding: t -*-

;;; Code:

(require 'periphery)

(periphery--clear-color-cache)

(message "")
(message "===== DETAILED FACE APPLICATION DEBUG =====")
(message "")

;; Step 1: Test periphery--mark-all-symbols directly
(message "1. Testing periphery--mark-all-symbols:")
(let* ((input "capture 'self' was never used")
       (regex (alist-get 'quotes periphery-highlight-patterns))
       (face-spec (periphery--get-identifier-face))
       (property (list 'face face-spec))
       (result (periphery--mark-all-symbols :input input :regex regex :property property)))
  (message "   Input: %s" input)
  (message "   Regex: %s" regex)
  (message "   Face spec: %S" face-spec)
  (message "   Property: %S" property)
  (message "   Result string: %s" result)
  (message "   Checking face at position 10 (should be in 'self'):")
  (let ((face-at-10 (get-text-property 10 'face result)))
    (message "   Face property: %S" face-at-10)
    (if face-at-10
        (message "   ✓ Face applied!")
      (message "   ✗ No face applied - regex didn't match?"))))

(message "")

;; Step 2: Check what the parser actually produces
(message "2. Parsing test input:")
(let ((test-input "AppDelegate.swift:462:15: warning: capture 'self' was never used"))
  (message "   Input: %s" test-input)
  (periphery-run-parser test-input :compiler)

  ;; Check what's in periphery-errorList
  (message "   Errors found: %d" (length periphery-errorList))
  (when periphery-errorList
    (let* ((first-error (car periphery-errorList))
           (entry (cadr first-error))
           (message-text (aref entry 3)))
      (message "   First error entry:")
      (message "     Path: %s" (car first-error))
      (message "     Message text: %s" message-text)
      (message "     Message type: %s" (type-of message-text))

      ;; Check if message has face properties
      (message "   Checking message for face properties:")
      (let ((found-bg nil)
            (pos 0))
        (while (< pos (length message-text))
          (let ((face-prop (get-text-property pos 'face message-text)))
            (when (and face-prop
                       (listp face-prop)
                       (plist-member face-prop :background))
              (setq found-bg t)
              (message "     Found background at position %d: %S" pos face-prop)))
          (setq pos (1+ pos)))
        (if found-bg
            (message "   ✓ Message has background faces!")
          (message "   ✗ Message has NO background faces - application failed!"))))))

(message "")
(message "===== END DEBUG =====")

(provide 'debug-application)
;;; debug-application.el ends here
