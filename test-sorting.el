;;; test-sorting.el --- Test periphery sorting -*- lexical-binding: t -*-

;; Load periphery-core first
(load-file "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/periphery-core.el")

;; Test severity ordering
(message "Testing periphery sorting...")

;; Create test entries for NOTE and TODO
(let ((note-entry (list "/path/to/file.swift:10:5"
                        (vector "NOTE" "file" "10" "some note message")))
      (todo-entry (list "/path/to/file.swift:20:5"
                        (vector "TODO" "file" "20" "some todo message"))))

  ;; Test severity values
  (message "NOTE severity: %d" (periphery-core--get-severity note-entry))
  (message "TODO severity: %d" (periphery-core--get-severity todo-entry))

  ;; Test sorting
  (let ((sorted (periphery-core--sort-results (list note-entry todo-entry))))
    (message "First entry after sorting: %s" (aref (cadr (car sorted)) 0))
    (message "Second entry after sorting: %s" (aref (cadr (cadr sorted)) 0))

    (if (string= "TODO" (aref (cadr (car sorted)) 0))
        (message "✓ SUCCESS: TODO comes before NOTE")
      (message "✗ FAILURE: NOTE still comes before TODO"))))

;;; test-sorting.el ends here
