;;; periphery-core.el --- Core functionality for periphery system -*- lexical-binding: t -*-

;;; Commentary:
;; Core parsing engine that uses the configuration system to process various inputs.

;;; Code:

(require 'periphery-config)
(require 'periphery-helper)
(require 'cl-lib)
(require 'seq)

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
QUERY is an optional search query for highlighting in search results.

Optimized: splits input once, applies all parsers per line, and
uses early-exit regex filtering to skip irrelevant lines."
  (when periphery-debug
    (message "periphery-core-parse called with type: %s, query: %s" type query))

  (setq periphery-core-last-input input)
  (setq periphery-core-error-list '())

  (let* ((parser-ids (or parsers
                         (mapcar #'car (periphery-get-parsers-by-type type))))
         ;; Build parser configs once, applying filters upfront
         (parser-configs
          (delq nil
                (mapcar
                 (lambda (id)
                   (when-let* ((config (periphery-get-parser id)))
                     (let ((parse-fn (plist-get config :parse-fn))
                           (filter-fn (plist-get config :filter-fn))
                           (regex (plist-get config :regex)))
                       (when parse-fn
                         (list :id id
                               :parse-fn parse-fn
                               :filter-fn filter-fn
                               :regex regex)))))
                 parser-ids)))
         ;; Build a combined pre-filter regex for quick rejection of non-matching lines
         (pre-filter-regex
          (periphery-core--build-pre-filter parser-configs))
         ;; Split input ONCE (not once per parser)
         (lines (split-string input "\n"))
         ;; Collect parsers that have filters (need full input, not per-line)
         (filter-parsers (seq-filter (lambda (p) (plist-get p :filter-fn)) parser-configs))
         (normal-parsers (seq-filter (lambda (p) (not (plist-get p :filter-fn))) parser-configs)))

    (when periphery-debug
      (message "Parsers to use: %S (pre-filter: %s)" 
               (mapcar (lambda (p) (plist-get p :id)) parser-configs)
               (if pre-filter-regex "yes" "no")))

    ;; Process parsers that need filtered input (rare, e.g. xcodebuild-error)
    (dolist (parser filter-parsers)
      (let* ((filter-fn (plist-get parser :filter-fn))
             (parse-fn (plist-get parser :parse-fn))
             (filtered-input (funcall filter-fn input)))
        (dolist (line (split-string filtered-input "\n"))
          (when (and line (not (string-empty-p line)))
            (when-let* ((result (if query
                                    (funcall parse-fn line query)
                                  (funcall parse-fn line))))
              (push result periphery-core-error-list))))))

    ;; Process normal parsers: iterate lines once, try all parsers per line
    (dolist (line lines)
      (when (and line (not (string-empty-p line)))
        ;; Quick pre-filter: skip lines that can't match ANY parser
        (when (or (null pre-filter-regex)
                  (string-match-p pre-filter-regex line))
          (when periphery-debug
            (message "  Parsing line (%d chars): %s" (length line) line))
          ;; Try each parser on this line (first match wins for efficiency)
          (let ((matched nil))
            (dolist (parser normal-parsers)
              (unless matched
                (let ((parse-fn (plist-get parser :parse-fn)))
                  (when-let* ((result (if query
                                          (funcall parse-fn line query)
                                        (funcall parse-fn line))))
                    (when periphery-debug
                      (message "    -> Got result from %s!" (plist-get parser :id)))
                    (push result periphery-core-error-list)
                    (setq matched t)))))))))

    ;; Remove duplicates (sorting is done once in periphery--listing-command)
    (setq periphery-core-error-list
          (delete-dups periphery-core-error-list))

    (when periphery-debug
      (message "Found %d errors" (length periphery-core-error-list)))

    ;; Call callback if provided
    (when callback
      (funcall callback periphery-core-error-list))

    periphery-core-error-list))

(defun periphery-core--build-pre-filter (parser-configs)
  "Build a combined regex from PARSER-CONFIGS for quick line rejection.
Returns nil if no useful pre-filter can be built."
  (let ((keywords '()))
    (dolist (parser parser-configs)
      (let ((id (plist-get parser :id)))
        ;; Add cheap string patterns that identify potentially relevant lines
        (pcase id
          ('compiler (push "\\(?:error\\|warning\\|note\\):" keywords))
          ('analyzer (push "\\(?:error\\|warning\\|note\\):" keywords))
          ('xctest (push "failed" keywords))
          ('build-warning (push "^\\(?:warning\\|note\\): " keywords))
          ('deployment-target (push "Compiling for" keywords))
          ('linker-error (push "\\(?:ld: \\|Undefined symbol\\|duplicate symbol\\|framework not found\\|library not found\\)" keywords))
          ('xcodebuild-error nil)  ; Uses filter-fn, handled separately
          ('cdtool (push "\\(?:cdtool\\|xcdatamodel\\)" keywords))
          ;; For search/swiftlint/ktlint we can't easily pre-filter
          (_ nil))))
    (when keywords
      (mapconcat #'identity keywords "\\|"))))

(defun periphery-core--sort-results (results)
  "Sort RESULTS by severity and file."
  (sort results
        (lambda (a b)
          (let ((severity-a (periphery-core--get-severity (car a)))
                (severity-b (periphery-core--get-severity (car b))))
            (if (equal severity-a severity-b)
                (string< (car a) (car b))
              (< severity-a severity-b))))))

(defun periphery-core--get-severity (entry)
  "Get numeric severity from ENTRY for sorting.
Errors = 1, Warnings = 2, Analyzer = 3, Info = 4, etc."
  (let ((type (downcase (or (ignore-errors (aref (cadr entry) 0)) ""))))
    (cond
     ((string-match-p "error" type) 1)
     ((string-match-p "warning" type) 2)
     ((string-match-p "analy" type) 3)
     ((string-match-p "fixme\\|fix" type) 4)
     ((string-match-p "todo" type) 5)
     ((string-match-p "note\\|info" type) 6)
     (t 7))))

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
