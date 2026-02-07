;;; periphery-parsers.el --- Parser implementations for periphery -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains the actual parser implementations for different types of output.

;;; Code:

(require 'periphery-config)
(require 'periphery-core)
(require 'cl-lib)

;; Ensure builtin patterns are available
(unless (boundp 'periphery-builtin-patterns)
  (load "periphery-config"))

;; Compiler Error Parser
;;;###autoload
(defun periphery-parser-compiler (line)
  "Parse compiler error/warning from LINE."
  (save-match-data
    ;; Try the standard compiler pattern
    (when (string-match (alist-get 'compiler periphery-builtin-patterns) line)
      (let* ((file (match-string 1 line))
             (line-num (match-string 2 line))
             (column (match-string 3 line))
             (severity (match-string 4 line))
             (raw-message (string-trim (or (match-string 5 line) "")))
             (path (if column
                       (format "%s:%s:%s" file line-num column)
                     (format "%s:%s" file line-num)))
             ;; Apply highlighting to the message
             (highlighted-message (periphery-parser--apply-highlighting raw-message)))
        (when (and file line-num severity)
          (periphery-core-build-entry
           :path path
           :file file
           :line line-num
           :column column
           :severity severity
           :message highlighted-message
           :face-fn #'periphery-parser--severity-face))))))

;; XCTest Parser
;;;###autoload
(defun periphery-parser-xctest (line)
  "Parse XCTest output from LINE."
  (save-match-data
    (when (string-match (alist-get 'xctest periphery-builtin-patterns) line)
      (let* ((file (match-string 1 line))
             (line-num (match-string 2 line))
             (test-name (match-string 4 line))
             (message (match-string 5 line))
             (path (format "%s:%s" file line-num)))
        (periphery-core-build-entry
         :path path
         :file file
         :line line-num
         :severity "Failed"
         :message (format "%s: %s" test-name message)
         :face-fn #'periphery-parser--severity-face)))))

;; Search Result Parser
;;;###autoload
(defun periphery-parser-search (line &optional query)
  "Parse search result from LINE with optional QUERY highlighting."
  (save-match-data
    (when (string-match (alist-get 'search periphery-builtin-patterns) line)
      (let* ((file (match-string 1 line))
             (line-num (match-string 2 line))
             (column (match-string 3 line))
             (content (string-trim-left (match-string 4 line)))
             (path (format "%s:%s:%s" file line-num column)))

        (when periphery-debug
          (message "periphery-parser-search: content='%s'" content))

        ;; Check if it's a TODO/FIXME by trimming comment chars and other noise first
        (let* ((trimmed-content (string-trim content))
               ;; Remove leading //, /*, #, ;, etc and whitespace
               (clean-content (replace-regexp-in-string "^\\(//+\\|/\\*+\\|#+\\|;+\\)[[:space:]]*" "" trimmed-content)))
          (when periphery-debug
            (message "  clean-content='%s'" clean-content)
            (message "  checking TODO match against: %s" (alist-get 'todos periphery-builtin-patterns))
            (message "  match result: %s"
                     (string-match (alist-get 'todos periphery-builtin-patterns) clean-content)))

          (if (string-match (alist-get 'todos periphery-builtin-patterns) clean-content)
              (progn
                (when periphery-debug
                  (message "  -> Matched TODO! Calling todo-from-content"))
                (periphery-parser-todo-from-content clean-content path file line-num))
            ;; Regular search match
            (when periphery-debug
              (message "  -> No TODO match, using regular Match"))
            (periphery-core-build-entry
             :path path
             :file file
             :line line-num
             :column column
             :severity "Match"
             :message (if query
                          (periphery-parser--highlight-match content query)
                        content)
             :face-fn #'periphery-parser--match-face)))))))

;; TODO/FIXME Parser
;;;###autoload
(defun periphery-parser-todo (content)
  "Parse TODO/FIXME from CONTENT."
  (save-match-data
    ;; First trim any leading comment characters and whitespace
    (let ((trimmed-content (replace-regexp-in-string "^[/;#]+\\s*" "" (string-trim content))))
      (when (string-match (alist-get 'todos periphery-builtin-patterns) trimmed-content)
        (let* ((keyword (match-string 1 trimmed-content))
               (comment (string-trim (match-string 2 trimmed-content))))
          (list keyword comment))))))

(defun periphery-parser-todo-from-content (content path file line-num)
  "Parse TODO from CONTENT with PATH FILE and LINE-NUM."
  (when-let* ((todo-data (periphery-parser-todo content)))
    (let* ((keyword (car todo-data))
           (comment (cadr todo-data))
           ;; Get the non-full face (without background) for the message
           (message-face (periphery-parser--todo-message-face keyword))
           ;; Apply face to the message
           (styled-message (propertize comment 'face message-face)))
      (periphery-core-build-entry
       :path path
       :file file
       :line line-num
       :severity keyword
       :message styled-message
       :face-fn #'periphery-parser--todo-face))))

;; Ktlint Parser
;;;###autoload
(defun periphery-parser-ktlint (line)
  "Parse ktlint output from LINE."
  (save-match-data
    (when (string-match (alist-get 'ktlint periphery-builtin-patterns) line)
      (let* ((file (match-string 1 line))
             (line-num (match-string 2 line))
             (column (match-string 3 line))
             (message (string-trim (match-string 4 line)))
             (rule (match-string 5 line))
             (path (format "%s:%s:%s" file line-num column)))
        (periphery-core-build-entry
         :path path
         :file file
         :line line-num
         :column column
         :severity "warning"
         :message (format "%s (%s)" message rule)
         :face-fn #'periphery-parser--severity-face)))))

;; SwiftLint Parser
;;;###autoload
(defun periphery-parser-swiftlint (line)
  "Parse SwiftLint output from LINE."
  (save-match-data
    (when (string-match "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.*\\)" line)
      (let* ((file (match-string 1 line))
             (line-num (match-string 2 line))
             (column (match-string 3 line))
             (severity (match-string 4 line))
             (message (match-string 5 line))
             (path (format "%s:%s:%s" file line-num column)))
        (periphery-core-build-entry
         :path path
         :file file
         :line line-num
         :column column
         :severity severity
         :message message
         :face-fn #'periphery-parser--severity-face)))))

;; Helper Functions
(defun periphery-parser--highlight-match (text query)
  "Highlight QUERY in TEXT."
  (if query
      (replace-regexp-in-string
       (regexp-quote query)
       (lambda (match)
         (propertize match 'face 'periphery-identifier-face))
       text t t)
    text))

(defun periphery-parser--severity-face (severity)
  "Get face for SEVERITY."
  (let ((type (upcase (string-trim severity))))
    (cond
     ((string-match-p "ERROR\\|FAILED" type) 'periphery-error-face-full)
     ((string-match-p "WARNING" type) 'periphery-warning-face-full)
     ((string-match-p "ANALY[ZS]E" type) 'periphery-analyzer-face-full)
     ((string-match-p "NOTE\\|INFO" type) 'periphery-note-face-full)
     ((string-match-p "MATCH" type) 'periphery-warning-face-full)
     (t 'periphery-info-face-full))))

(defun periphery-parser--todo-face (keyword)
  "Get face for TODO KEYWORD (full face with background)."
  (let ((type (upcase keyword)))
    (cond
     ((string= type "TODO") 'periphery-todo-face-full)
     ((string-match-p "FIX\\|FIXME" type) 'periphery-fix-face-full)
     ((string= type "HACK") 'periphery-hack-face-full)
     ((string= type "NOTE") 'periphery-note-face-full)
     ((string= type "PERF") 'periphery-performance-face-full)
     ((string= type "MARK") 'periphery-mark-face-full)
     (t 'periphery-info-face-full))))

(defun periphery-parser--todo-message-face (keyword)
  "Get face for TODO KEYWORD message (no background, just foreground)."
  (let ((type (upcase keyword)))
    (cond
     ((string= type "TODO") 'periphery-todo-face)
     ((string-match-p "FIX\\|FIXME" type) 'periphery-fix-face)
     ((string= type "HACK") 'periphery-error-face)  ; HACK uses error color
     ((string= type "NOTE") 'periphery-note-face)
     ((string= type "PERF") 'periphery-performance-face)
     ((string= type "MARK") 'periphery-info-face)
     (t 'periphery-info-face))))

(defun periphery-parser--match-face (_)
  "Get face for search match."
  'periphery-warning-face-full)

(defun periphery-parser--apply-highlighting (message)
  "Apply syntax highlighting to MESSAGE for strings, parentheses, etc.
Optimized: uses a single temp buffer for all highlighting passes
instead of creating one per pattern."
  (when periphery-debug
    (message "Applying highlighting to: %s" message))
  (if (not (boundp 'periphery-highlight-patterns))
      message
    ;; Build the list of (regex . face) pairs to apply, in order
    (let ((patterns
           (delq nil
                 (list
                  (let ((r (alist-get 'quote-content periphery-highlight-patterns))
                        (f (alist-get 'quote-content periphery-syntax-faces)))
                    (when (and r f) (cons r f)))
                  (let ((r (alist-get 'string-content periphery-highlight-patterns))
                        (f (alist-get 'string-content periphery-syntax-faces)))
                    (when (and r f) (cons r f)))
                  (let ((r (alist-get 'quote-marks periphery-highlight-patterns))
                        (f (alist-get 'quote-marks periphery-syntax-faces)))
                    (when (and r f) (cons r f)))
                  (let ((r (alist-get 'string-marks periphery-highlight-patterns))
                        (f (alist-get 'string-marks periphery-syntax-faces)))
                    (when (and r f) (cons r f)))
                  (let ((r (alist-get 'parentheses periphery-highlight-patterns))
                        (f (alist-get 'parentheses periphery-syntax-faces)))
                    (when (and r f) (cons r f)))))))
      ;; Apply all patterns in a single temp buffer
      (with-temp-buffer
        (insert message)
        (dolist (pattern patterns)
          (goto-char (point-min))
          (let ((regex (car pattern))
                (face (cdr pattern)))
            (while (re-search-forward regex nil t)
              (let ((start (match-beginning 0))
                    (end (match-end 0)))
                (when (< start end)
                  (add-text-properties start end (list 'face face))
                  (goto-char end))))))
        (when periphery-debug
          (message "Final highlighted: %s" (buffer-string)))
        (buffer-string)))))

;; Static Analyzer Parser
;;;###autoload
(defun periphery-parser-analyzer (line)
  "Parse a static analyzer diagnostic from LINE.
Same format as compiler output (file:line:col: severity: message),
but relabels warnings as Analyzer to visually distinguish them."
  (save-match-data
    (when (string-match (alist-get 'compiler periphery-builtin-patterns) line)
      (let* ((file (match-string 1 line))
             (line-num (match-string 2 line))
             (column (match-string 3 line))
             (severity (match-string 4 line))
             (raw-message (string-trim (or (match-string 5 line) "")))
             (path (if column
                       (format "%s:%s:%s" file line-num column)
                     (format "%s:%s" file line-num)))
             ;; Remap warning -> Analyze, keep error as-is
             (display-severity (if (string-equal-ignore-case severity "warning")
                                   "Analyze"
                                 severity))
             (highlighted-message (periphery-parser--apply-highlighting raw-message)))
        (when (and file line-num severity)
          (periphery-core-build-entry
           :path path
           :file file
           :line line-num
           :column column
           :severity display-severity
           :message highlighted-message
           :face-fn #'periphery-parser--severity-face))))))

;; Register default parsers
;;;###autoload
(defun periphery-parsers-initialize ()
  "Initialize and register all default parsers."
  
  (periphery-register-parser
   'compiler
   :name "Compiler"
   :regex (alist-get 'compiler periphery-builtin-patterns)
   :type :compiler
   :priority 100
   :parse-fn #'periphery-parser-compiler
   :face-fn #'periphery-parser--severity-face)
  
  (periphery-register-parser
   'xctest
   :name "XCTest"
   :regex (alist-get 'xctest periphery-builtin-patterns)
   :type :test
   :priority 90
   :parse-fn #'periphery-parser-xctest
   :face-fn #'periphery-parser--severity-face)
  
  (periphery-register-parser
   'ktlint
   :name "Ktlint"
   :regex (alist-get 'ktlint periphery-builtin-patterns)
   :type :linter
   :priority 60
   :parse-fn #'periphery-parser-ktlint
   :face-fn #'periphery-parser--severity-face)
  
  (periphery-register-parser
   'swiftlint
   :name "SwiftLint"
   :regex "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.*\\)"
   :type :linter
   :priority 65
   :parse-fn #'periphery-parser-swiftlint
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'search
   :name "Search Results"
   :regex (alist-get 'search periphery-builtin-patterns)
   :type :search
   :priority 80
   :parse-fn #'periphery-parser-search
   :face-fn #'periphery-parser--match-face)

  (periphery-register-parser
   'cdtool
   :name "Core Data Tool"
   :regex "\\(^cdtool:\\|xcdatamodel::\\)"
   :type :compiler
   :priority 95
   :parse-fn #'periphery-parser-cdtool
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'xcodebuild-error
   :name "Xcodebuild Errors"
   :regex "^xcodebuild: error:"
   :type :compiler
   :priority 98
   :parse-fn #'periphery-parser-xcodebuild-error
   :filter-fn #'periphery-parser-xcodebuild-error-filter
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'device-error
   :name "Device/Platform Errors"
   :regex "error:.*iOS [0-9.]+ is not installed"
   :type :compiler
   :priority 97
   :parse-fn #'periphery-parser-device-error
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'project-error
   :name "Project-level Errors"
   :regex "\\.xcodeproj: error:"
   :type :compiler
   :priority 96
   :parse-fn #'periphery-parser-project-error
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'build-warning
   :name "Build Warnings"
   :regex "^\\(warning\\|note\\): "
   :type :compiler
   :priority 85
   :parse-fn #'periphery-parser-build-warning
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'deployment-target
   :name "Deployment Target Mismatch"
   :regex "Compiling for iOS [0-9.]+, but module"
   :type :compiler
   :priority 99
   :parse-fn #'periphery-parser-deployment-target
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'provisioning-profile
   :name "Provisioning Profile Errors"
   :regex "error: No profile\\|error: Provisioning profile\\|error:.*provisioning profile"
   :type :compiler
   :priority 98
   :parse-fn #'periphery-parser-provisioning-profile
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'code-signing
   :name "Code Signing Errors"
   :regex "error:.*Code Sign\\|error:.*codesign\\|error:.*Signing"
   :type :compiler
   :priority 98
   :parse-fn #'periphery-parser-code-signing
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'missing-sdk
   :name "Missing SDK/Platform"
   :regex "error:.*SDK.*not found\\|error:.*platform.*not found\\|SDK \".*\" cannot be located"
   :type :compiler
   :priority 97
   :parse-fn #'periphery-parser-missing-sdk
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'architecture-mismatch
   :name "Architecture Mismatch"
   :regex "building for .* but attempting to link\\|Could not find module .* for target"
   :type :compiler
   :priority 96
   :parse-fn #'periphery-parser-architecture-mismatch
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'linker-error
   :name "Linker Errors"
   :regex "ld: \\|Undefined symbol\\|duplicate symbol\\|framework not found\\|library not found"
   :type :compiler
   :priority 95
   :parse-fn #'periphery-parser-linker-error
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'entitlements
   :name "Entitlements Errors"
   :regex "error:.*entitlement\\|error:.*capability"
   :type :compiler
   :priority 94
   :parse-fn #'periphery-parser-entitlements
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'swift-version-mismatch
   :name "Swift Version Mismatch"
   :regex "compiled with.*Swift.*but.*uses\\|Module compiled with Swift [0-9.]+"
   :type :compiler
   :priority 93
   :parse-fn #'periphery-parser-swift-version
   :face-fn #'periphery-parser--severity-face)

  (periphery-register-parser
   'missing-module
   :name "Missing Module/Import"
   :regex "No such module\\|cannot find .* in scope\\|Missing required module"
   :type :compiler
   :priority 92
   :parse-fn #'periphery-parser-missing-module
   :face-fn #'periphery-parser--severity-face)

  ;; Static analyzer parser (same regex as compiler, but relabels severity)
  (periphery-register-parser
   'analyzer
   :name "Static Analyzer"
   :regex (alist-get 'compiler periphery-builtin-patterns)
   :type :analyzer
   :priority 100
   :parse-fn #'periphery-parser-analyzer
   :face-fn #'periphery-parser--severity-face))

;; Package/Build Error Parsers
;;;###autoload
(defun periphery-parser-cdtool (line)
  "Parse cdtool (Core Data model) errors from LINE."
  (save-match-data
    (cond
     ;; cdtool: Failed to parse model XML with failure reason ...
     ((string-match "^cdtool: Failed to parse model XML with failure reason \\(.*\\)$" line)
      (let ((reason (match-string 1 line)))
        (periphery-core-build-entry
         :path "Core Data Model"
         :file "*.xcdatamodeld"
         :line "1"
         :severity "error"
         :message (format "Core Data model error: %s" reason)
         :face-fn #'periphery-parser--severity-face)))
     ;; cdtool hash unarchiving error: ...
     ((string-match "^cdtool hash unarchiving error: \\(.*\\)$" line)
      (let ((reason (match-string 1 line)))
        (periphery-core-build-entry
         :path "Core Data Model"
         :file "*.xcdatamodeld"
         :line "1"
         :severity "error"
         :message (format "Core Data unarchiving error: %s" reason)
         :face-fn #'periphery-parser--severity-face)))
     ;; .xcdatamodel:: error: cdtool cannot compile
     ((string-match "\\(/[^:]+\\.xcdatamodel\\):: error: \\(.*\\)$" line)
      (let ((file (match-string 1 line))
            (message (match-string 2 line)))
        (periphery-core-build-entry
         :path file
         :file file
         :line "1"
         :severity "error"
         :message message
         :face-fn #'periphery-parser--severity-face))))))

;;;###autoload
(defun periphery-parser-package-error (input)
  "Parse package dependency errors from INPUT."
  (when (string-match-p "^xcodebuild: error: Could not resolve package dependencies:" input)
    (periphery-core-build-entry
     :path "Package Dependencies"
     :file "Package.swift"
     :line "1"
     :severity "error"
     :message input
     :face-fn #'periphery-parser--severity-face)))

;;;###autoload
(defun periphery-parser-build-failure (input)
  "Parse build failures from INPUT."
  (when (string-match-p "^The following build commands failed:" input)
    (periphery-core-build-entry
     :path "_Build Failure"
     :file "_Build Failure"
     :line "999"
     :severity "error"
     :message input
     :face-fn #'periphery-parser--severity-face)))

;;;###autoload
(defun periphery-parser-xcodebuild-error-filter (input)
  "Filter INPUT to combine multi-line xcodebuild errors into single lines."
  (let ((lines (split-string input "\n"))
        (result '())
        (current-error nil))
    (dolist (line lines)
      (cond
       ;; Start of xcodebuild error
       ((string-match "^xcodebuild: error: " line)
        ;; If we had a previous error, save it
        (when current-error
          (push current-error result))
        ;; Start accumulating new error
        (setq current-error line))

       ;; Continuation line (starts with whitespace) or empty line while accumulating
       ((and current-error
             (or (string-match "^[[:space:]]+" line)
                 (string-empty-p line)))
        ;; Append to current error with newline preserved
        (setq current-error (concat current-error "\n" line)))

       ;; Any other non-empty line that doesn't start with whitespace
       ((not (string-empty-p line))
        ;; If we were accumulating, save it
        (when current-error
          (push current-error result)
          (setq current-error nil))
        ;; Add the current line
        (push line result))

       ;; Empty line when not accumulating
       (t
        (push line result))))

    ;; Don't forget the last error if still accumulating
    (when current-error
      (push current-error result))

    ;; Return reconstructed input
    (string-join (nreverse result) "\n")))

;;;###autoload
(defun periphery-parser-xcodebuild-error (input)
  "Parse xcodebuild errors from INPUT."
  (when (string-match "^xcodebuild: error: " input)
    (let ((message (substring input (match-end 0))))
      (periphery-core-build-entry
       :path "Xcode Build"
       :file "Xcode Build Configuration"
       :line "1"
       :severity "error"
       :message message
       :face-fn #'periphery-parser--severity-face))))

;;;###autoload
(defun periphery-parser-device-error (input)
  "Parse device/platform error messages from INPUT."
  (when (string-match "error:\\s*\\(iOS [0-9.]+ is not installed\\..*\\)" input)
    (periphery-core-build-entry
     :path "Xcode Components"
     :file "Xcode Build Configuration"
     :line "1"
     :severity "error"
     :message (match-string 1 input)
     :face-fn #'periphery-parser--severity-face)))

;;;###autoload
(defun periphery-parser-project-error (input)
  "Parse project-level errors from INPUT (e.g., .xcodeproj: error: ...)."
  (when (string-match "\\(/[^:]+\\.xcodeproj\\): error: \\(.*\\)" input)
    (let ((project-file (match-string 1 input))
          (message (match-string 2 input)))
      (periphery-core-build-entry
       :path project-file
       :file (file-name-nondirectory project-file)
       :line "1"
       :severity "error"
       :message message
       :face-fn #'periphery-parser--severity-face))))

;;;###autoload
(defun periphery-parser-build-warning (input)
  "Parse build-level warnings/notes from INPUT (e.g., warning: ..., note: ...)."
  (when (string-match "^\\(warning\\|note\\): \\(.*\\)" input)
    (let ((severity (match-string 1 input))
          (message (match-string 2 input)))
      (periphery-core-build-entry
       :path "Build System"
       :file "Xcode Build"
       :line "1"
       :severity severity
       :message message
       :face-fn #'periphery-parser--severity-face))))

;;;###autoload
(defun periphery-parser-deployment-target (input)
  "Parse deployment target mismatch errors from INPUT.
Matches errors like: Compiling for iOS 15.0, but module 'Foo' has a minimum deployment target of iOS 17.6"
  (when (string-match "Compiling for \\(iOS [0-9.]+\\), but module '\\([^']+\\)' has a minimum deployment target of \\(iOS [0-9.]+\\)\\(?:: \\(.*\\)\\)?" input)
    (let* ((compile-target (match-string 1 input))
           (module-name (match-string 2 input))
           (min-target (match-string 3 input))
           (module-path (or (match-string 4 input) ""))
           (target-file (if (string-empty-p module-path)
                            (format "%s.xcodeproj" module-name)
                          (file-name-nondirectory module-path)))
           (message (format "Target '%s' compiles for %s but depends on '%s' which requires %s. Update deployment target to %s."
                            target-file compile-target module-name min-target min-target)))
      (periphery-core-build-entry
       :path (if (string-empty-p module-path) "Build Settings" module-path)
       :file target-file
       :line "1"
       :severity "error"
       :message message
       :face-fn #'periphery-parser--severity-face))))

;;;###autoload
(defun periphery-parser-provisioning-profile (input)
  "Parse provisioning profile errors from INPUT."
  (cond
   ;; No profile for team/bundle
   ((string-match "error: No profile for team '\\([^']+\\)' matching '\\([^']+\\)'" input)
    (let ((team (match-string 1 input))
          (bundle-id (match-string 2 input)))
      (periphery-core-build-entry
       :path "Signing & Capabilities"
       :file "Project Settings"
       :line "1"
       :severity "error"
       :message (format "No provisioning profile for team '%s' with bundle ID '%s'. Create/download profile in Apple Developer Portal or enable Automatic Signing."
                        team bundle-id)
       :face-fn #'periphery-parser--severity-face)))

   ;; Profile doesn't include device
   ((string-match "error: Provisioning profile \"\\([^\"]+\\)\" doesn't include the currently selected device" input)
    (let ((profile (match-string 1 input)))
      (periphery-core-build-entry
       :path "Signing & Capabilities"
       :file "Project Settings"
       :line "1"
       :severity "error"
       :message (format "Profile '%s' doesn't include this device. Add device UDID to profile in Apple Developer Portal."
                        profile)
       :face-fn #'periphery-parser--severity-face)))

   ;; Profile has expired
   ((string-match "error: Provisioning profile \"\\([^\"]+\\)\" has expired" input)
    (let ((profile (match-string 1 input)))
      (periphery-core-build-entry
       :path "Signing & Capabilities"
       :file "Project Settings"
       :line "1"
       :severity "error"
       :message (format "Profile '%s' has expired. Renew in Apple Developer Portal and download again."
                        profile)
       :face-fn #'periphery-parser--severity-face)))

   ;; Generic provisioning profile error
   ((string-match "error:.*provisioning profile\\(.*\\)" input)
    (let ((details (string-trim (match-string 1 input))))
      (periphery-core-build-entry
       :path "Signing & Capabilities"
       :file "Project Settings"
       :line "1"
       :severity "error"
       :message (format "Provisioning profile error: %s" details)
       :face-fn #'periphery-parser--severity-face)))))

;;;###autoload
(defun periphery-parser-code-signing (input)
  "Parse code signing errors from INPUT."
  (cond
   ;; No signing certificate
   ((string-match "error: No signing certificate \"\\([^\"]+\\)\" found" input)
    (let ((cert-type (match-string 1 input)))
      (periphery-core-build-entry
       :path "Signing & Capabilities"
       :file "Keychain Access"
       :line "1"
       :severity "error"
       :message (format "Certificate '%s' not found. Install from Apple Developer Portal or enable Automatic Signing."
                        cert-type)
       :face-fn #'periphery-parser--severity-face)))

   ;; Certificate has expired
   ((string-match "error:.*certificate.*\\(has expired\\|is not valid\\)" input)
    (periphery-core-build-entry
     :path "Signing & Capabilities"
     :file "Keychain Access"
     :line "1"
     :severity "error"
     :message "Signing certificate has expired or is invalid. Renew in Apple Developer Portal."
     :face-fn #'periphery-parser--severity-face))

   ;; Code signing identity not found
   ((string-match "Code Sign error: No code signing identities found" input)
    (periphery-core-build-entry
     :path "Signing & Capabilities"
     :file "Keychain Access"
     :line "1"
     :severity "error"
     :message "No code signing identities found. Install certificates from Apple Developer Portal."
     :face-fn #'periphery-parser--severity-face))

   ;; Ambiguous signing identity
   ((string-match "error:.*Ambiguous.*signing\\|Multiple signing identities" input)
    (periphery-core-build-entry
     :path "Signing & Capabilities"
     :file "Build Settings"
     :line "1"
     :severity "error"
     :message "Multiple matching signing identities. Specify CODE_SIGN_IDENTITY in Build Settings."
     :face-fn #'periphery-parser--severity-face))

   ;; Generic code signing error
   ((string-match "error:.*[Cc]ode [Ss]ign\\(.*\\)" input)
    (let ((details (string-trim (or (match-string 1 input) ""))))
      (periphery-core-build-entry
       :path "Signing & Capabilities"
       :file "Build Settings"
       :line "1"
       :severity "error"
       :message (format "Code signing error: %s" (if (string-empty-p details) input details))
       :face-fn #'periphery-parser--severity-face)))))

;;;###autoload
(defun periphery-parser-missing-sdk (input)
  "Parse missing SDK/platform errors from INPUT."
  (cond
   ;; SDK cannot be located
   ((string-match "SDK \"\\([^\"]+\\)\" cannot be located" input)
    (let ((sdk (match-string 1 input)))
      (periphery-core-build-entry
       :path "Xcode Components"
       :file "Xcode Preferences"
       :line "1"
       :severity "error"
       :message (format "SDK '%s' not found. Install via Xcode > Settings > Platforms, or run: xcodebuild -downloadPlatform %s"
                        sdk (car (split-string sdk)))
       :face-fn #'periphery-parser--severity-face)))

   ;; Platform not found
   ((string-match "error:.*platform.*\"\\([^\"]+\\)\".*not found" input)
    (let ((platform (match-string 1 input)))
      (periphery-core-build-entry
       :path "Xcode Components"
       :file "Xcode Preferences"
       :line "1"
       :severity "error"
       :message (format "Platform '%s' not installed. Install via Xcode > Settings > Platforms."
                        platform)
       :face-fn #'periphery-parser--severity-face)))

   ;; Simulator runtime not available
   ((string-match "error:.*Simulator.*\\(iOS [0-9.]+\\).*is not available" input)
    (let ((version (match-string 1 input)))
      (periphery-core-build-entry
       :path "Xcode Components"
       :file "Xcode Preferences"
       :line "1"
       :severity "error"
       :message (format "Simulator runtime for %s not installed. Install via Xcode > Settings > Platforms."
                        version)
       :face-fn #'periphery-parser--severity-face)))

   ;; Generic SDK error
   ((string-match "SDK.*not found\\|platform.*not found" input)
    (periphery-core-build-entry
     :path "Xcode Components"
     :file "Xcode Preferences"
     :line "1"
     :severity "error"
     :message (format "SDK/Platform error: %s" input)
     :face-fn #'periphery-parser--severity-face))))

;;;###autoload
(defun periphery-parser-architecture-mismatch (input)
  "Parse architecture mismatch errors from INPUT."
  (cond
   ;; Building for X but attempting to link with file built for Y
   ((string-match "building for \\([^,]+\\), but attempting to link with file built for \\([^)]+\\)" input)
    (let ((target-arch (match-string 1 input))
          (file-arch (match-string 2 input)))
      (periphery-core-build-entry
       :path "Build Settings"
       :file "ARCHS"
       :line "1"
       :severity "error"
       :message (format "Architecture mismatch: building for '%s' but library is built for '%s'. Rebuild dependency for correct architecture or adjust ARCHS/EXCLUDED_ARCHS."
                        target-arch file-arch)
       :face-fn #'periphery-parser--severity-face)))

   ;; Could not find module for target
   ((string-match "Could not find module '\\([^']+\\)' for target '\\([^']+\\)'" input)
    (let ((module (match-string 1 input))
          (target (match-string 2 input)))
      (periphery-core-build-entry
       :path "Dependencies"
       :file module
       :line "1"
       :severity "error"
       :message (format "Module '%s' not built for '%s'. Clean build folder and rebuild, or check EXCLUDED_ARCHS settings."
                        module target)
       :face-fn #'periphery-parser--severity-face)))

   ;; arm64/x86_64 specific errors
   ((string-match "\\(arm64\\|x86_64\\|armv7\\).*\\(not found\\|missing\\|incompatible\\)" input)
    (periphery-core-build-entry
     :path "Build Settings"
     :file "ARCHS"
     :line "1"
     :severity "error"
     :message (format "Architecture error: %s. Check ARCHS and EXCLUDED_ARCHS in Build Settings." input)
     :face-fn #'periphery-parser--severity-face))))

;;;###autoload
(defun periphery-parser-linker-error (input)
  "Parse linker errors from INPUT."
  (cond
   ;; Undefined symbol
   ((string-match "Undefined symbol[s]?: \\(.*\\)" input)
    (let ((symbols (match-string 1 input)))
      (periphery-core-build-entry
       :path "Link Binary With Libraries"
       :file "Build Phases"
       :line "1"
       :severity "error"
       :message (format "Undefined symbol: %s. Add missing framework/library or implement missing function."
                        (string-trim symbols))
       :face-fn #'periphery-parser--severity-face)))

   ;; Duplicate symbol
   ((string-match "duplicate symbol[s]? \\([^ ]+\\)\\(?: in\\)?" input)
    (let ((symbol (match-string 1 input)))
      (periphery-core-build-entry
       :path "Build Phases"
       :file "Compile Sources"
       :line "1"
       :severity "error"
       :message (format "Duplicate symbol '%s'. Check for duplicate definitions or conflicting libraries."
                        symbol)
       :face-fn #'periphery-parser--severity-face)))

   ;; Framework not found
   ((string-match "framework not found \\([^ ]+\\)" input)
    (let ((framework (match-string 1 input)))
      (periphery-core-build-entry
       :path "Link Binary With Libraries"
       :file "Build Phases"
       :line "1"
       :severity "error"
       :message (format "Framework '%s' not found. Add to 'Link Binary With Libraries' or install via CocoaPods/SPM/Carthage."
                        framework)
       :face-fn #'periphery-parser--severity-face)))

   ;; Library not found
   ((string-match "library not found for -l\\([^ ]+\\)" input)
    (let ((library (match-string 1 input)))
      (periphery-core-build-entry
       :path "Link Binary With Libraries"
       :file "Build Phases"
       :line "1"
       :severity "error"
       :message (format "Library '%s' not found. Check LIBRARY_SEARCH_PATHS or add library to project."
                        library)
       :face-fn #'periphery-parser--severity-face)))

   ;; Generic ld error/warning - detect severity from content
   ((string-match "^ld: \\(.*\\)" input)
    (let* ((error-msg (match-string 1 input))
           ;; Check if it's actually a warning
           (severity (if (string-match-p "^warning:" error-msg)
                         "warning"
                       "error")))
      (periphery-core-build-entry
       :path "Linker"
       :file "Build Settings"
       :line "1"
       :severity severity
       :message (format "Linker %s: %s" severity error-msg)
       :face-fn #'periphery-parser--severity-face)))))

;;;###autoload
(defun periphery-parser-entitlements (input)
  "Parse entitlements errors from INPUT."
  (cond
   ;; Missing entitlement
   ((string-match "error:.*Missing entitlement.*\"\\([^\"]+\\)\"\\|Missing entitlement: \\([^ ]+\\)" input)
    (let ((entitlement (or (match-string 1 input) (match-string 2 input))))
      (periphery-core-build-entry
       :path "Signing & Capabilities"
       :file "Entitlements"
       :line "1"
       :severity "error"
       :message (format "Missing entitlement '%s'. Add capability in Signing & Capabilities tab or update .entitlements file."
                        entitlement)
       :face-fn #'periphery-parser--severity-face)))

   ;; Capability not allowed
   ((string-match "error:.*Capability '\\([^']+\\)' is not allowed" input)
    (let ((capability (match-string 1 input)))
      (periphery-core-build-entry
       :path "Signing & Capabilities"
       :file "App ID"
       :line "1"
       :severity "error"
       :message (format "Capability '%s' not enabled for this App ID. Enable in Apple Developer Portal > Identifiers."
                        capability)
       :face-fn #'periphery-parser--severity-face)))

   ;; Entitlement value mismatch
   ((string-match "error:.*entitlement.*value.*doesn't match\\|Entitlement.*has incorrect value" input)
    (periphery-core-build-entry
     :path "Signing & Capabilities"
     :file "Entitlements"
     :line "1"
     :severity "error"
     :message "Entitlement value mismatch. Ensure .entitlements file matches provisioning profile capabilities."
     :face-fn #'periphery-parser--severity-face))

   ;; Generic entitlement error
   ((string-match "error:.*entitlement\\(.*\\)\\|error:.*capability\\(.*\\)" input)
    (let ((details (string-trim (or (match-string 1 input) (match-string 2 input) ""))))
      (periphery-core-build-entry
       :path "Signing & Capabilities"
       :file "Entitlements"
       :line "1"
       :severity "error"
       :message (format "Entitlements error: %s" (if (string-empty-p details) input details))
       :face-fn #'periphery-parser--severity-face)))))

;;;###autoload
(defun periphery-parser-swift-version (input)
  "Parse Swift version mismatch errors from INPUT."
  (cond
   ;; Module compiled with different Swift version
   ((string-match "Module compiled with Swift \\([0-9.]+\\).*cannot be imported by.*Swift \\([0-9.]+\\)" input)
    (let ((module-version (match-string 1 input))
          (project-version (match-string 2 input)))
      (periphery-core-build-entry
       :path "Build Settings"
       :file "SWIFT_VERSION"
       :line "1"
       :severity "error"
       :message (format "Swift version mismatch: module compiled with Swift %s, project uses Swift %s. Rebuild dependency or adjust SWIFT_VERSION."
                        module-version project-version)
       :face-fn #'periphery-parser--severity-face)))

   ;; Compiled with older/newer Swift
   ((string-match "compiled with Swift \\([0-9.]+\\).*but.*\\(?:uses\\|requires\\) Swift \\([0-9.]+\\)" input)
    (let ((compiled (match-string 1 input))
          (required (match-string 2 input)))
      (periphery-core-build-entry
       :path "Build Settings"
       :file "SWIFT_VERSION"
       :line "1"
       :severity "error"
       :message (format "Module built with Swift %s but Swift %s required. Clean build and rebuild all dependencies."
                        compiled required)
       :face-fn #'periphery-parser--severity-face)))

   ;; Generic Swift version error
   ((string-match "Swift [0-9.]+.*\\(incompatible\\|mismatch\\|cannot\\)" input)
    (periphery-core-build-entry
     :path "Build Settings"
     :file "SWIFT_VERSION"
     :line "1"
     :severity "error"
     :message (format "Swift version incompatibility: %s. Try Product > Clean Build Folder and rebuild." input)
     :face-fn #'periphery-parser--severity-face))))

;;;###autoload
(defun periphery-parser-missing-module (input)
  "Parse missing module/import errors from INPUT."
  (cond
   ;; No such module
   ((string-match "No such module '\\([^']+\\)'" input)
    (let ((module (match-string 1 input)))
      (periphery-core-build-entry
       :path "Dependencies"
       :file "Package.swift / Podfile"
       :line "1"
       :severity "error"
       :message (format "Module '%s' not found. Add dependency via SPM/CocoaPods/Carthage, or check target membership."
                        module)
       :face-fn #'periphery-parser--severity-face)))

   ;; Cannot find X in scope
   ((string-match "cannot find '\\([^']+\\)' in scope" input)
    (let ((symbol (match-string 1 input)))
      (periphery-core-build-entry
       :path "Source Code"
       :file "Imports"
       :line "1"
       :severity "error"
       :message (format "'%s' not found. Add missing import statement or check spelling."
                        symbol)
       :face-fn #'periphery-parser--severity-face)))

   ;; Missing required module
   ((string-match "Missing required module '\\([^']+\\)'" input)
    (let ((module (match-string 1 input)))
      (periphery-core-build-entry
       :path "Dependencies"
       :file "Project Dependencies"
       :line "1"
       :severity "error"
       :message (format "Required module '%s' is missing. Ensure dependency is correctly configured and built."
                        module)
       :face-fn #'periphery-parser--severity-face)))))

;; Auto-initialize on load
(periphery-parsers-initialize)

(provide 'periphery-parsers)
;;; periphery-parsers.el ends here
