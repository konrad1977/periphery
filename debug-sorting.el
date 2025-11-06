;;; debug-sorting.el --- Debug periphery sorting -*- lexical-binding: t -*-

;; Enable debug mode
(setq periphery-debug t)

;; Load periphery-core
(load-file "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/periphery-core.el")

;; Create test entries similar to what the parser creates
(let ((note-entry (list "/path/to/file.swift:10:5"
                        (vector
                         (propertize "  NOTE  " 'face 'periphery-note-face)
                         (propertize "file" 'face 'periphery-filename-face)
                         (propertize "10" 'face 'periphery-linenumber-face)
                         "some note message")))
      (todo-entry (list "/path/to/file.swift:20:5"
                        (vector
                         (propertize "  TODO  " 'face 'periphery-todo-face)
                         (propertize "file" 'face 'periphery-filename-face)
                         (propertize "20" 'face 'periphery-linenumber-face)
                         "some todo message")))
      (hack-entry (list "/path/to/file.swift:30:5"
                        (vector
                         (propertize "  HACK  " 'face 'periphery-hack-face)
                         (propertize "file" 'face 'periphery-filename-face)
                         (propertize "30" 'face 'periphery-linenumber-face)
                         "some hack message"))))

  ;; Debug: Check what severity values we get
  (message "\n=== DEBUG: Severity extraction ===")
  (message "NOTE entry structure: %S" note-entry)
  (message "NOTE severity string: '%s'" (aref (cadr note-entry) 0))
  (message "NOTE severity value: %d" (periphery-core--get-severity note-entry))
  (message "")
  (message "TODO entry structure: %S" todo-entry)
  (message "TODO severity string: '%s'" (aref (cadr todo-entry) 0))
  (message "TODO severity value: %d" (periphery-core--get-severity todo-entry))
  (message "")
  (message "HACK entry structure: %S" hack-entry)
  (message "HACK severity string: '%s'" (aref (cadr hack-entry) 0))
  (message "HACK severity value: %d" (periphery-core--get-severity hack-entry))
  (message "")

  ;; Test sorting
  (message "=== DEBUG: Sorting test ===")
  (let ((sorted (periphery-core--sort-results (list note-entry todo-entry hack-entry))))
    (message "Sorting order:")
    (dolist (entry sorted)
      (message "  %s (severity: %d)"
               (string-trim (aref (cadr entry) 0))
               (periphery-core--get-severity entry)))

    ;; Check if sorted correctly
    (let ((first-type (string-trim (aref (cadr (nth 0 sorted)) 0)))
          (second-type (string-trim (aref (cadr (nth 1 sorted)) 0)))
          (third-type (string-trim (aref (cadr (nth 2 sorted)) 0))))
      (message "")
      (if (and (string= first-type "HACK")
               (string= second-type "TODO")
               (string= third-type "NOTE"))
          (message "✓ SUCCESS: Correct order (HACK, TODO, NOTE)")
        (message "✗ FAILURE: Wrong order (%s, %s, %s)" first-type second-type third-type)))))

;; Disable debug mode
(setq periphery-debug nil)

;;; debug-sorting.el ends here
