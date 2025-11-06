;;; test-simple.el --- Simple sorting test -*- lexical-binding: t -*-

;; Define the sorting functions directly
(defun periphery-core--get-severity (entry)
  "Get numeric severity from ENTRY for sorting."
  (let* ((severity-str (or (ignore-errors (aref (cadr entry) 0)) ""))
         (type (downcase (string-trim (substring-no-properties severity-str)))))
    (cond
     ((string-match-p "error" type) 1)
     ((string-match-p "warning" type) 2)
     ((string-match-p "hack" type) 3)
     ((string-match-p "fixme\\|fix" type) 4)
     ((string-match-p "perf\\|performance" type) 5)
     ((string-match-p "todo" type) 6)
     ((string-match-p "note\\|info" type) 7)
     (t 8))))

(defun periphery-core--sort-results (results)
  "Sort RESULTS by severity and file."
  (sort results
        (lambda (a b)
          (let ((severity-a (periphery-core--get-severity a))
                (severity-b (periphery-core--get-severity b)))
            (if (equal severity-a severity-b)
                (string< (car a) (car b))
              (< severity-a severity-b))))))

;; Create test entries
(let ((note-entry
       (list "UISideMenuNavigationController.swift:230"
             (vector "  NOTE  " "file" "230" "message")))
      (todo-entry
       (list "Client.swift:91"
             (vector "  TODO  " "file" "91" "message")))
      (hack-entry
       (list "SideMenuTransition.swift:524"
             (vector "  HACK  " "file" "524" "message")))
      (perf-entry
       (list "PenaltiesHistoryViewModel.swift:342"
             (vector "  PERF  " "file" "342" "message"))))

  (message "\n=== Severity Extraction Test ===")
  (message "HACK severity: %d (expected: 3)" (periphery-core--get-severity hack-entry))
  (message "PERF severity: %d (expected: 5)" (periphery-core--get-severity perf-entry))
  (message "TODO severity: %d (expected: 6)" (periphery-core--get-severity todo-entry))
  (message "NOTE severity: %d (expected: 7)" (periphery-core--get-severity note-entry))

  ;; Test sorting
  (message "\n=== Sorting Test ===")
  (let* ((unsorted (list note-entry todo-entry perf-entry hack-entry))
         (sorted (periphery-core--sort-results unsorted)))

    (message "Before: NOTE, TODO, PERF, HACK")
    (message "After sorting:")
    (dolist (entry sorted)
      (let ((type (string-trim (aref (cadr entry) 0)))
            (severity (periphery-core--get-severity entry)))
        (message "  %s (severity: %d)" type severity)))

    ;; Verify
    (let ((types (mapcar (lambda (e) (string-trim (aref (cadr e) 0))) sorted)))
      (if (equal types '("HACK" "PERF" "TODO" "NOTE"))
          (message "\n✓ SUCCESS: Correct order (HACK, PERF, TODO, NOTE)")
        (message "\n✗ FAILURE: Wrong order: %S" types)))))

;;; test-simple.el ends here
