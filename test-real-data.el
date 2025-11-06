;;; test-real-data.el --- Test sorting with real bruce-ios data -*- lexical-binding: t -*-

;; Load periphery components
(add-to-list 'load-path "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery")
(require 'periphery-core)
(require 'periphery-config)
(require 'periphery-parsers)

;; Simulate real entries from bruce-ios
(let ((note-entry
       (list "Bruce/Libs/SideMenu/UISideMenuNavigationController.swift:230:16"
             (vector
              (propertize "  NOTE  " 'face 'periphery-note-face)
              (propertize "UISideMenuNavigationController" 'face 'periphery-filename-face)
              (propertize "230" 'face 'periphery-linenumber-face)
              "pushViewController is called by init(rootViewController: UIViewController)")))
      (todo-entry
       (list "Bruce/Core/Client.swift:91:68"
             (vector
              (propertize "  TODO  " 'face 'periphery-todo-face)
              (propertize "Client" 'face 'periphery-filename-face)
              (propertize "91" 'face 'periphery-linenumber-face)
              "Investigate call sites and always do this")))
      (hack-entry
       (list "Bruce/Libs/SideMenu/SideMenuTransition.swift:524:48"
             (vector
              (propertize "  HACK  " 'face 'periphery-hack-face)
              (propertize "SideMenuTransition" 'face 'periphery-filename-face)
              (propertize "524" 'face 'periphery-linenumber-face)
              "If zero, the animation briefly flashes in iOS 11."))))

  (message "\n========================================")
  (message "Testing Periphery Sorting with Real Data")
  (message "========================================\n")

  ;; Test severity extraction
  (message "Severity values:")
  (message "  HACK: %d (expected: 3)" (periphery-core--get-severity hack-entry))
  (message "  TODO: %d (expected: 6)" (periphery-core--get-severity todo-entry))
  (message "  NOTE: %d (expected: 7)" (periphery-core--get-severity note-entry))
  (message "")

  ;; Create unsorted list (NOTE, TODO, HACK)
  (let* ((unsorted (list note-entry todo-entry hack-entry))
         (sorted (periphery-core--sort-results unsorted)))

    (message "Before sorting:")
    (dolist (entry unsorted)
      (message "  %s" (string-trim (substring-no-properties (aref (cadr entry) 0)))))
    (message "")

    (message "After sorting:")
    (dolist (entry sorted)
      (message "  %s (severity: %d)"
               (string-trim (substring-no-properties (aref (cadr entry) 0)))
               (periphery-core--get-severity entry)))
    (message "")

    ;; Verify correct order
    (let ((first-type (string-trim (substring-no-properties (aref (cadr (nth 0 sorted)) 0))))
          (second-type (string-trim (substring-no-properties (aref (cadr (nth 1 sorted)) 0))))
          (third-type (string-trim (substring-no-properties (aref (cadr (nth 2 sorted)) 0)))))

      (if (and (string= first-type "HACK")
               (string= second-type "TODO")
               (string= third-type "NOTE"))
          (progn
            (message "✓ SUCCESS: Correct sorting order!")
            (message "  1. HACK (severity 3)")
            (message "  2. TODO (severity 6)")
            (message "  3. NOTE (severity 7)"))
        (progn
          (message "✗ FAILURE: Incorrect sorting order!")
          (message "  Got: %s, %s, %s" first-type second-type third-type)
          (message "  Expected: HACK, TODO, NOTE")))))

  (message "\n========================================\n"))

;;; test-real-data.el ends here
