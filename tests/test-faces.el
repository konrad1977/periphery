;;; Test face definitions with dynamic background system

;; Load just the config to test faces
(add-to-list 'load-path "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/")
(require 'periphery-config)

;; Test that base face definitions are available
(let ((test-faces '(periphery-error-face
                    periphery-warning-face
                    periphery-message-face
                    periphery-todo-face
                    periphery-fix-face
                    periphery-note-face
                    periphery-hack-face
                    periphery-performance-face
                    periphery-mark-face)))

  (message "\n=== BASE FACE TEST ===")
  (dolist (face test-faces)
    (if (facep face)
        (message "✓ Face %s is defined" face)
      (message "✗ Face %s NOT defined" face))))

;; Test dynamic background generation
(message "\n=== DYNAMIC BACKGROUND GENERATION TEST ===")
(let ((test-colors '(("#f38ba8" . "error")
                     ("#f9e2af" . "warning")
                     ("#74c7ec" . "todo")
                     ("#89b4fa" . "fix"))))
  (dolist (color-pair test-colors)
    (let* ((fg-color (car color-pair))
           (name (cdr color-pair))
           (bg-color (periphery--create-background-from-foreground fg-color)))
      (message "✓ %s: FG=%s → BG=%s (darkness: %d%%)"
               name fg-color bg-color periphery-background-darkness))))

;; Test face-with-background function
(message "\n=== FACE WITH BACKGROUND TEST ===")
(let ((test-faces '(periphery-error-face
                    periphery-warning-face
                    periphery-todo-face)))
  (dolist (face test-faces)
    (let ((face-spec (periphery--get-face-with-background face)))
      (message "✓ %s with background: %S" face face-spec))))

;; Test syntax faces configuration
(if (boundp 'periphery-syntax-faces)
    (progn
      (message "✓ periphery-syntax-faces is defined")
      (message "Syntax faces: %S" periphery-syntax-faces))
  (message "✗ periphery-syntax-faces NOT defined"))

(message "Face test completed.")