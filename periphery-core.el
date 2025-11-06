;;; periphery-core.el --- Core functionality for periphery system -*- lexical-binding: t -*-

;;; Commentary:
;; Core parsing engine that uses the configuration system to process various inputs.

;;; Code:

(require 'periphery-config)
(require 'periphery-helper)
(require 'cl-lib)

;; Forward declaration to avoid circular dependency
(defvar periphery-debug)

(defvar periphery-core-error-list '()
  "Current list of parsed errors/warnings/matches.")

(defvar periphery-core-last-input nil
  "Cache of last processed input for debugging.")

;;;###autoload
(cl-defun periphery-core-parse (&key input type parsers callback query)
  "Parse INPUT using parsers of given TYPE or specific PARSERS list.
TYPE can be :compiler, :search, :linter, or :test.
PARSERS can be a list of parser IDs to use.
CALLBACK is called with the parsed results.
QUERY is an optional search query for highlighting in search results."
  (when periphery-debug
    (message "periphery-core-parse called with type: %s, query: %s" type query))

  (setq periphery-core-last-input input)
  (setq periphery-core-error-list '())

  (let ((parsers-to-use (or parsers
                             (mapcar #'car (periphery-get-parsers-by-type type)))))

    (when periphery-debug
      (message "Parsers to use: %S" parsers-to-use))

    ;; Process input through each parser
    (dolist (parser-id parsers-to-use)
      (when-let ((config (periphery-get-parser parser-id)))
        (when periphery-debug
          (message "Applying parser: %s" parser-id))
        (periphery-core--apply-parser input config query)))

    ;; Remove duplicates (based on path, not full content) and sort
    (setq periphery-core-error-list
          (periphery-core--sort-results
           (periphery-core--remove-duplicate-paths periphery-core-error-list)))

    (when periphery-debug
      (message "DEBUG: After sorting, first 10 entries:")
      (dotimes (i (min 10 (length periphery-core-error-list)))
        (let* ((entry (nth i periphery-core-error-list))
               (type (string-trim (substring-no-properties (aref (cadr entry) 0))))
               (severity (periphery-core--get-severity entry)))
          (message "  %d. %s (severity: %d)" (1+ i) type severity))))

    (when periphery-debug
      (message "Found %d errors" (length periphery-core-error-list)))

    ;; Call callback if provided
    (when callback
      (funcall callback periphery-core-error-list))

    periphery-core-error-list))

(defun periphery-core--apply-parser (input config &optional query)
  "Apply parser CONFIG to INPUT and collect results.
Optional QUERY is passed to the parser function if it accepts it."
  (when-let ((parse-fn (plist-get config :parse-fn)))
    ;; Apply filter if provided
    (let ((filter-fn (plist-get config :filter-fn)))
      (when filter-fn
        (setq input (funcall filter-fn input))))

    ;; Parse each line
    (dolist (line (split-string input "\n"))
      (when (and line (not (string-empty-p line)))
        (when periphery-debug
          (message "  Parsing line (%d chars): %s" (length line) line))
        (let ((result (if query
                          (funcall parse-fn line query)
                        (funcall parse-fn line))))
          (when result
            (when periphery-debug
              (message "    -> Got result!"))
            (push result periphery-core-error-list)))))))

(defun periphery-core--remove-duplicate-paths (results)
  "Remove duplicate entries from RESULTS based on path (first element).
When duplicates are found, prefer entries with capitalized messages."
  (let ((seen (make-hash-table :test 'equal))
        (unique '()))
    (dolist (entry results)
      (let* ((path (car entry))
             (existing (gethash path seen)))
        (if existing
            ;; If we've seen this path, keep the one with better capitalization
            (let* ((existing-msg (aref (cadr existing) 3))
                   (new-msg (aref (cadr entry) 3))
                   ;; Prefer messages that start with uppercase
                   (prefer-new (and (> (length new-msg) 0)
                                   (> (length existing-msg) 0)
                                   (let ((new-first (substring new-msg 0 1))
                                         (existing-first (substring existing-msg 0 1)))
                                     (and (string= (upcase new-first) new-first)
                                          (string= (downcase existing-first) existing-first))))))
              (when prefer-new
                (puthash path entry seen)))
          ;; First time seeing this path
          (puthash path entry seen))))
    ;; Extract all unique entries from hash table
    (maphash (lambda (_path entry) (push entry unique)) seen)
    unique))

(defun periphery-core--sort-results (results)
  "Sort RESULTS by severity and file."
  (sort results
        (lambda (a b)
          (let ((severity-a (periphery-core--get-severity a))
                (severity-b (periphery-core--get-severity b)))
            (if (equal severity-a severity-b)
                (string< (car a) (car b))
              (< severity-a severity-b))))))

(defun periphery-core--get-severity (entry)
  "Get numeric severity from ENTRY for sorting.
Errors = 1, Warnings = 2, HACK = 3, FIXME = 4, PERF = 5, TODO = 6, NOTE = 7, etc."
  (let* ((severity-str (or (ignore-errors (aref (cadr entry) 0)) ""))
         ;; Strip text properties and trim whitespace
         (type (downcase (string-trim (substring-no-properties severity-str)))))
    (when periphery-debug
      (message "DEBUG get-severity: raw='%s' type='%s'" severity-str type))
    (cond
     ((string-match-p "error" type) 1)
     ((string-match-p "warning" type) 2)
     ((string-match-p "hack" type) 3)
     ((string-match-p "fixme\\|fix" type) 4)
     ((string-match-p "perf\\|performance" type) 5)
     ((string-match-p "todo" type) 6)
     ((string-match-p "note\\|info" type) 7)
     (t 8))))

;;;###autoload
(cl-defun periphery-core-build-entry (&key path file line column 
                                           severity message face-fn)
  "Build a standard periphery entry.
PATH is the full path with line:column.
FILE is the file path.
LINE is the line number.
COLUMN is optional column number.
SEVERITY is the error/warning type.
MESSAGE is the description.
FACE-FN is a function to determine face from severity."
  (let ((face (if face-fn 
                  (funcall face-fn severity)
                'periphery-error-face)))
    (list path 
          (vector
           (periphery-core--propertize-severity severity face)
           (propertize (file-name-sans-extension 
                        (file-name-nondirectory file)) 
                       'face 'periphery-filename-face)
           (propertize (or line "0") 'face 'periphery-linenumber-face)
           ;; Don't overwrite existing face properties in message
           (if (periphery-core--has-face-properties message)
               message  ; Keep existing properties
             (propertize message 'face 'periphery-message-face))))))

(defun periphery-core--has-face-properties (string)
  "Check if STRING has any face properties."
  (let ((pos 0)
        (len (length string))
        (has-face nil))
    (while (and (< pos len) (not has-face))
      (when (get-text-property pos 'face string)
        (setq has-face t))
      (setq pos (1+ pos)))
    has-face))

(defun periphery-core--propertize-severity (severity face)
  "Format and colorize SEVERITY with FACE."
  (let* ((type (upcase (string-trim severity)))
         (display (if (> (string-width type) 8)
                      (substring type 0 8)
                    type)))
    (propertize (format " %s " (periphery-core--center-text display))
                'face face)))

(defun periphery-core--center-text (text)
  "Center TEXT to standard width."
  (let* ((target-width 8)
         (text-width (string-width text))
         (padding (/ (- target-width text-width) 2))
         (result (concat (make-string padding ?\s) text)))
    (while (< (string-width result) (1- target-width))
      (setq result (concat result " ")))
    result))

;;;###autoload
(defun periphery-core-run-async (input type callback)
  "Parse INPUT asynchronously using parsers of TYPE.
CALLBACK is called with results when complete."
  (run-with-idle-timer 
   0.1 nil
   (lambda ()
     (condition-case err
         (periphery-core-parse 
          :input input 
          :type type 
          :callback callback)
       (error 
        (message "Periphery async parse failed: %s" 
                 (error-message-string err)))))))

;;;###autoload
(defun periphery-core-clear ()
  "Clear current error list and cached data."
  (interactive)
  (setq periphery-core-error-list '()
        periphery-core-last-input nil)
  (message "Periphery data cleared"))

;; Compatibility layer - moved to main periphery.el to avoid circular dependencies

(provide 'periphery-core)
;;; periphery-core.el ends here