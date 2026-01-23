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

(defun periphery-core--deduplicate (list)
  "Remove exact duplicates from LIST.
Uses path + severity + message as composite key to allow multiple
different errors/warnings on the same line."
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (dolist (item list)
      (let* ((path (car item))
             (vector (cadr item))
             ;; Create composite key: path + severity-type + first 50 chars of message
             (severity (when (vectorp vector) (aref vector 0)))
             (message (when (vectorp vector) 
                        (substring (aref vector 3) 0 (min 50 (length (aref vector 3))))))
             (key (format "%s|%s|%s" path severity message)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push item result))))
    (nreverse result)))

;;;###autoload
(cl-defun periphery-core-parse (&key input type parsers callback query)
  "Parse INPUT using parsers of given TYPE or specific PARSERS list.
TYPE can be :compiler, :search, :linter, or :test.
PARSERS can be a list of parser IDs to use.
CALLBACK is called with the parsed results.
QUERY is an optional search query for highlighting in search results."
  (when periphery-debug
    (message "periphery-core-parse called with type: %s, query: %s" type query)
    (message "  Input length: %d chars" (length input))
    (message "  First 200 chars: %s" (substring input 0 (min 200 (length input)))))

  (setq periphery-core-last-input input)
  (setq periphery-core-error-list '())

  (let ((parsers-to-use (or parsers
                             (mapcar #'car (periphery-get-parsers-by-type type)))))

    (when periphery-debug
      (message "Parsers to use: %S" parsers-to-use)
      (when (null parsers-to-use)
        (message "WARNING: No parsers found for type %s!" type)
        (message "  Registered parsers hash size: %d" (hash-table-count periphery-registered-parsers))))

    ;; Process input through each parser
    (dolist (parser-id parsers-to-use)
      (when-let ((config (periphery-get-parser parser-id)))
        (when periphery-debug
          (message "Applying parser: %s (regex: %s)" 
                   parser-id 
                   (truncate-string-to-width (or (plist-get config :regex) "nil") 50)))
        (periphery-core--apply-parser input config query)))

    ;; Remove duplicates (O(n) with hash table) and sort
    (setq periphery-core-error-list
          (periphery-core--sort-results
           (periphery-core--deduplicate periphery-core-error-list)))

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
      (when (and line
                 (not (string-empty-p line))
                 ;; Skip xcodebuild internal commands (export, set, cd, etc.)
                 (not (string-match-p "^[[:space:]]*\\(export\\|set\\|cd\\|setenv\\|builtin\\) " line))
                 ;; Skip lines that are just paths or very long without useful content
                 (or (< (length line) 500)
                     (string-match-p "\\(error\\|warning\\|note\\|failed\\):" line)))
        (when periphery-debug
          (message "  Parsing line (%d chars): %s" (length line) (substring line 0 (min 100 (length line)))))
        (let ((result (if query
                          (funcall parse-fn line query)
                        (funcall parse-fn line))))
          (when result
            (when periphery-debug
              (message "    -> Got result!"))
            (push result periphery-core-error-list)))))))

(defun periphery-core--sort-results (results)
  "Sort RESULTS by severity and file."
  (sort results
        (lambda (a b)
          (let ((severity-a (periphery-core--get-severity a))
                (severity-b (periphery-core--get-severity b)))
            (if (equal severity-a severity-b)
                (string< (car a) (car b))
              (< severity-a severity-b))))))

(defun periphery-core--get-severity (item)
  "Get numeric severity from ITEM for sorting.
ITEM is a list (PATH VECTOR) where VECTOR contains [severity file line message].
Errors = 1, Warnings = 2, Info = 3, etc."
  (let* ((vector (if (vectorp (cadr item)) (cadr item) nil))
         (severity-str (when vector (ignore-errors (aref vector 0))))
         (type (downcase (or severity-str ""))))
    (cond
     ((string-match-p "error" type) 1)
     ((string-match-p "warning" type) 2)
     ((string-match-p "fixme\\|fix" type) 3)
     ((string-match-p "todo" type) 4)
     ((string-match-p "note\\|info" type) 5)
     (t 6))))

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