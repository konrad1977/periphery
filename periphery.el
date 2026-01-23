;;; periphery.el --- A simple package to parse output from compile commands -*- lexical-binding: t; -*-

;;; Commentary:
;;; --- A simple package

;;; Code:

(require 'periphery-core)
(require 'periphery-config)
(require 'periphery-parsers)

;; Initialize parsers when loading periphery
(periphery-parsers-initialize)

(defconst periphery-buffer-name "*Periphery*")

(defcustom periphery-debug nil
  "Enable debug mode periphery."
  :type 'boolean
  :group 'periphery)

(defvar default-length 8)

(defvar periphery-mode-map nil
  "Keymap for periphery.")

(defcustom periphery-trim-message-prefix nil
  "When non-nil, trim the message prefix (up to the first colon) in error messages."
  :type 'boolean
  :group 'periphery)

(setq periphery-mode-map (make-sparse-keymap))
(define-key periphery-mode-map (kbd "RET") #'periphery--open-current-line)
(define-key periphery-mode-map (kbd "<return>") 'periphery--open-current-line)
(define-key periphery-mode-map (kbd "o") 'periphery--open-current-line)
(define-key periphery-mode-map (kbd "?") 'periphery-filter-menu)
(define-key periphery-mode-map (kbd "f") 'periphery-filter-menu)
(define-key periphery-mode-map (kbd "q") 'quit-window)

(defvar-local periphery-errorList '())
(defvar-local periphery--unfiltered-list '()
  "Original unfiltered error list.")
(defvar periphery--current-filter nil
  "Currently active filter, or nil for no filter. Persists across builds.")

(define-derived-mode periphery-mode tabulated-list-mode "Periphery-mode"
  "Periphery mode.  A mode to show compile errors like Flycheck."
  (setq tabulated-list-format [
                               ("Type" 9 nil)
                               ("File" 28 t)
                               ("Line" 4 nil)
                               ("Message" 0 nil)  ; 0 = no width limit, show full message
                               ]
        tabulated-list-padding 1
        tabulated-list-sort-key (cons "Line" nil))
  (use-local-map periphery-mode-map)
  (setq truncate-lines t)  ; No wrapping, truncate at window edge
  (tabulated-list-init-header))

(defun periphery--open-current-line ()
  "Open current current line."
  (interactive)
  (open-current-line-with (tabulated-list-get-id)))

(defun periphery--severity-priority (severity)
  "Return numeric priority for SEVERITY (lower number = higher priority)."
  (if (not (stringp severity))
      4  ; Default priority for non-strings
    (let ((type (downcase (string-trim severity))))
      (cond
       ((string-prefix-p "error" type) 1)
       ((string-prefix-p "warning" type) 2)
       ((string-prefix-p "note" type) 3)
       (t 4)))))

(defun periphery--get-severity-type (entry)
  "Extract clean severity type from ENTRY (error, warning, note, etc.)."
  (let* ((vector (cadr entry))
         (severity-raw (aref vector 0))
         ;; Remove text properties and trim whitespace
         (severity-clean (string-trim (substring-no-properties severity-raw))))
    (downcase severity-clean)))

(defun periphery--apply-current-filter (entries)
  "Apply current filter to ENTRIES if one is active."
  (if (null periphery--current-filter)
      entries
    (seq-filter
     (lambda (entry)
       (let ((severity (periphery--get-severity-type entry)))
         (pcase periphery--current-filter
           ("error" (string= severity "error"))
           ("warning" (string= severity "warning"))
           ("note" (string= severity "note"))
           ("error+warning" (or (string= severity "error")
                                (string= severity "warning")))
           (_ t))))
     entries)))

(defun periphery--listing-command (errorList)
  "Create an ERRORLIST for the current mode, prioritizing errors."
  (let ((sorted-list (sort errorList
                           (lambda (a b)
                             (let* ((entry-a (cadr a))
                                    (entry-b (cadr b))
                                    (severity-a (aref entry-a 0))
                                    (severity-b (aref entry-b 0))
                                    (file-a (aref entry-a 1))
                                    (file-b (aref entry-b 1))
                                    (priority-a (periphery--severity-priority severity-a))
                                    (priority-b (periphery--severity-priority severity-b)))
                               (if (= priority-a priority-b)
                                   (string< file-a file-b)
                                 (< priority-a priority-b)))))))

    (let* ((buffer (get-buffer-create periphery-buffer-name))
           (window (get-buffer-window buffer))
           (entry-count 0))
      
      ;; Update buffer content - always use with-current-buffer for buffer-local vars
      (with-current-buffer buffer
        (unless (derived-mode-p 'periphery-mode)
          (periphery-mode))
        
        ;; Store unfiltered list and apply filter
        (setq periphery--unfiltered-list (delq nil sorted-list))
        (setq tabulated-list-entries (periphery--apply-current-filter periphery--unfiltered-list))
        (setq entry-count (length tabulated-list-entries))
        
        (tabulated-list-print t)
        
        ;; Always scroll to top after inserting new items
        (goto-char (point-min)))
      
      ;; Display buffer if not already visible
      (unless window
        (display-buffer buffer))
      
      ;; Show count message
      (when (> entry-count 0)
        (periphery-message-with-count
         :tag (if periphery--current-filter (format "[%s]" periphery--current-filter) "")
         :text "Errors or warnings"
         :count (format "%d" entry-count)
         :attributes 'periphery-todo-face))
      
      ;; Process pending events to prevent blocking build callbacks
      ;; This is critical to allow installation to proceed immediately
      (sit-for 0)

      ;; Auto-open first error (only if it's actually an error, not a warning)
      (run-at-time 0.2 nil
                   (lambda ()
                     (when-let ((buf (get-buffer periphery-buffer-name)))
                       (with-current-buffer buf
                         (goto-char (point-min))
                         (when-let* ((entry (tabulated-list-get-entry))
                                     (severity (aref entry 0)))
                           (when (string-match-p "error" (downcase severity))
                             (periphery--open-current-line))))))))))

(defun periphery--propertize-severity (severity)
  "Colorize TEXT using SEVERITY."
  (if-let* ((type (upcase (string-trim-left severity))))
      (let ((display-type (if (> (string-width type) 8)
                              "ERROR"
                            type)))
        (propertize (format " %s " (periphery--center-text display-type))
                    'face (periphery--full-color-from-keyword severity)))))

(defun periphery--center-text (word)
  "Center WORD to default length."
  (let* ((word-len (string-width word))
         (padding (/ (- default-length word-len) 3))
         (result (concat (make-string padding ?\s) word)))
    (while (< (string-width result) (- default-length 1))
      (setq result (concat result " ")))
    result))

(defun periphery--color-from-keyword (keyword)
  "Get color face from KEYWORD."
  (let ((type (upcase (string-trim-left keyword))))
    (pcase type
      ((or "WARNING" "MATCH") 'periphery-warning-face)
      ("INFO" 'periphery-note-face)
      ("ERROR" 'periphery-error-face)
      ("NOTE" 'periphery-note-face)
      ((or "FIX" "FIXME") 'periphery-fix-face)
      ((or "PERF" "PERFORMANCE") 'periphery-performance-face)
      ("TODO" 'periphery-todo-face)
      ("HACK" 'periphery-hack-face)
      (_ 'periphery-error-face))))

(defun periphery--full-color-from-keyword (keyword)
  "Get full color face from KEYWORD."
  (let ((type (upcase (string-trim-left keyword))))
    (pcase type
      ((or "WARNING" "MATCH") 'periphery-warning-face-full)
      ("INFO" 'periphery-note-face-full)
      ("ERROR" 'periphery-error-face-full)
      ("NOTE" 'periphery-note-face-full)
      ((or "FIX" "FIXME") 'periphery-fix-face-full)
      ((or "PERF" "PERFORMANCE") 'periphery-performance-face-full)
      ("TODO" 'periphery-todo-face-full)
      ("HACK" 'periphery-hack-face-full)
      (_ 'periphery-error-face-full))))

(defun periphery--apply-all-highlights (input patterns)
  "Apply all highlight PATTERNS to INPUT in a single buffer pass.
PATTERNS is a list of (REGEX PROPERTY GROUP) tuples where:
  REGEX is the pattern to match
  PROPERTY is the text property list to apply
  GROUP is the capture group to highlight (default 0 for full match).
Returns the highlighted string. This is more efficient than multiple
calls to mark individual patterns as it uses a single temp buffer."
  (if (or (null patterns) (string-empty-p input))
      input
    (with-temp-buffer
      (insert input)
      (dolist (pattern patterns)
        (let ((regex (nth 0 pattern))
              (property (nth 1 pattern))
              (group (or (nth 2 pattern) 0)))
          (when regex
            (goto-char (point-min))
            (while (re-search-forward regex nil t)
              (let ((start (match-beginning group))
                    (end (match-end group)))
                (when (and start end (< start end))
                  (add-text-properties start end property)))))))
      (buffer-string))))

(defun parse-missing-package-product (buffer)
  "Parse missing package product errors from the current BUFFER."
  (let ((regex-missing-product ": error: Missing package product '\\([^']+\\)' (in target '\\([^']+\\)' from project '\\([^']+\\)')")
        (errors '()))
    (goto-char (point-min))
    (while (re-search-forward regex-missing-product nil t)
      (let* ((product (match-string 1))
             (target (match-string 2))
             (project (match-string 3))
             (failure-msg (format "Missing package product '%s' in target '%s' from project '%s'"
                                product target project)))
        (push (periphery--build-list
               :path (format "%s/%s" project target)
               :file "Package.swift"
               :line "1"
               :keyword "error"
               :result failure-msg
               :regex (periphery--get-highlight-pattern 'quotes))
              errors)))
    errors))

(defun parse-package-dependency-errors (buffer)
  "Parse package dependency errors from the current BUFFER."
  (let ((regex-package-error "^xcodebuild: error: Could not resolve package dependencies:")
        (errors '()))
    (goto-char (point-min))
    (when (re-search-forward regex-package-error nil t)
      (let ((failure-msg (buffer-substring-no-properties (point) (pos-eol))))
        (forward-line)
        ;; Collect all related package error messages
        (while (and (not (eobp))
                    (or (looking-at "^  ") (looking-at "^$")))
          (unless (looking-at "^$") ; Skip empty lines
            (setq failure-msg (concat failure-msg "\n"
                                    (buffer-substring-no-properties (point) (pos-eol)))))
          (forward-line))
        (push (periphery--build-list
               :path "Package Dependencies"
               :file "Package.swift"
               :line "1"
               :keyword "error"
               :result failure-msg
               :regex (periphery--get-highlight-pattern 'quotes))
              errors)))
    errors))

(defun parse-build-failures (buffer)
  "Parse build failures from the current BUFFER."
  (let ((regex-build-failure "^The following build commands failed:")
        (errors '()))
    (goto-char (point-min))
    (when (re-search-forward regex-build-failure nil t)
      (let ((failure-msg (buffer-substring-no-properties (point) (pos-eol))))
        (forward-line)
        (while (and (not (eobp)) (looking-at "\t"))
          (setq failure-msg (concat failure-msg "\n"
                                    (buffer-substring-no-properties (point) (pos-eol))))
          (forward-line))
        ;; Shorten the path in the failure message
        (setq failure-msg
              (replace-regexp-in-string
               "\\(/Users/[^/]+/.*?/\\)\\([^/]+/[^/]+\\.build/.*\\)"
               "\\2"
               failure-msg))
        (push (periphery--build-list
               :path "_Build Failure"
               :file "_Build Failure"
               :line "999"
               :keyword "error"
               :result failure-msg
               :regex (periphery--get-highlight-pattern 'quotes))
              errors)))
    errors))

(cl-defun periphery-run-parser (input &rest config)
  "Run parser on INPUT with dynamic CONFIG. Return t if errors found.
CONFIG can be:
- :config PARSERS - Use specific parser list (e.g., '(compiler ktlint))
- :todo - Parse for TODO/FIXME/HACK comments  
- :ktlint - Parse ktlint output
- :compiler - Parse compiler output (default)
- :search QUERY - Parse search results with optional query highlighting
- :linter - Parse generic linter output
- :test - Parse test output"
  (when periphery-debug
    (message "periphery-run-parser called with %d chars of input and config: %S" 
             (length input) config))
  
  (let ((type :compiler)
        (parsers nil)
        (query nil))
    
    ;; Process configuration parameters
    (while config
      (let ((key (pop config)))
        (cond
         ((eq key :config)
          (setq parsers (pop config)))
         ((eq key :todo)
          (setq type :search)  ; TODOs are found via search
          (setq parsers '(search))) ; Use search parser to detect TODO patterns
         ((eq key :ktlint)
          (setq type :linter)
          (setq parsers '(ktlint)))
         ((eq key :compiler)
          (setq type :compiler)
          (setq parsers '(compiler)))
         ((eq key :search)
          (setq type :search)
          (setq parsers '(search))
          (when config (setq query (pop config))))
         ((eq key :linter)
          (setq type :linter))
         ((eq key :test)
          (setq type :test)
          (setq parsers '(xctest)))
         (t
          (message "Unknown periphery-run-parser config: %s" key)))))
    
    ;; Use the core parsing system with dynamic configuration
    (condition-case err
        (let ((errors (periphery-core-parse
                       :input input
                       :type type
                       :parsers parsers
                       :query query)))
          (setq periphery-errorList errors)
          (when periphery-debug
            (message "periphery-run-parser: parsed %d errors" (length errors)))
          (when (or (periphery--is-buffer-visible) periphery-errorList)
            (periphery--listing-command periphery-errorList))
          (not (null periphery-errorList)))
      (error
       (message "periphery-run-parser ERROR: %s" (error-message-string err))
       nil)))) ; Return nil on error

(defun periphery-sort-error (errors)
  "Sort ERRORS by severity (Error, Warning, Note) and then by path."
  (sort (nreverse errors)
        (lambda (a b)
          (let* ((entry-a (cadr a))
                 (entry-b (cadr b))
                 (priority-a (periphery--severity-priority (aref entry-a 0)))
                 (priority-b (periphery--severity-priority (aref entry-b 0))))
            ;; First compare by severity priority
            (if (= priority-a priority-b)
                ;; If same severity, sort by path
                (string< (car a) (car b))
              ;; Otherwise sort by severity (lower priority number comes first)
              (< priority-a priority-b))))))

(defun periphery--is-buffer-visible ()
  "Check if periphery buffer is visible."
  (when-let* ((buffer (get-buffer periphery-buffer-name)))
    (get-buffer-window buffer)))

(defun periphery-kill-buffer ()
  "Kill the periphery buffer."
  (interactive)
  (when-let* ((buffer (get-buffer periphery-buffer-name)))
    (kill-buffer buffer)))

;;; ###autoload
(defun periphery-toggle-buffer ()
  "Toggle visibility of the Periphery buffer window."
  (interactive)
  (if-let* ((buffer (get-buffer periphery-buffer-name)))
      (if (get-buffer-window buffer)
          (delete-window (get-buffer-window buffer))
        (display-buffer buffer))
    (message "Buffer %s does not exist" periphery-buffer-name)))

(defun periphery-toggle-debug ()
  "Toggle the periphery debug mode on or off."
  (interactive)
  (setq periphery-debug (not periphery-debug))
  (message "Periphery debug mode %s" (if periphery-debug "enabled" "disabled")))

(cl-defun periphery-run-test-parser (input succesCallback)
  "Parse test input using dynamic parser system."
  (if (periphery-run-parser input :test)
      (periphery--listing-command (delete-dups periphery-errorList))
    (funcall succesCallback)))

;;; - Bartycrouch parsing
(defun periphery--clean-up-comments (text)
  "Cleanup comments from (as TEXT) fixmes and todos."
  (save-match-data
    (and (string-match (alist-get 'todos periphery-builtin-patterns) text)
         (if-let* ((keyword (match-string 1 text))
                (comment (match-string 2 text)))
             (list keyword comment)))))

(cl-defun periphery-message-with-count (&key tag &key text &key count &key attributes)
  "Print a TAG and TEXT with ATTRIBUTES with Count."
  (if (not (string= text ""))
      (mode-line-hud:update :message (format "%s %s '%s'" tag  (propertize count 'face 'periphery-error-face)  (propertize text 'face attributes)))
    (mode-line-hud:update :message (format "%s %s" tag count ))))

(defun parse--search-query (text query)
  "Parse error and notes (as TEXT) and QUERY."
  (setq default-length 8)
  (setq-local case-fold-search nil) ;; Make regex case sensitive
  (save-match-data
    (and (string-match (alist-get 'search periphery-builtin-patterns) text)
         (let* ((file (match-string 1 text))
                (line (match-string 2 text))
                (column (match-string 3 text))
                (message (string-trim-left (match-string 4 text)))
                (fileWithLine (format "%s:%s:%s" file line column)))

           (if-let* ((todo (periphery--clean-up-comments message)))
                 (periphery--build-todo-list
                  :path fileWithLine
                  :file file
                  :line line
                  :keyword (nth 0 todo)
                  :result (nth 1 todo)
                  :regex (format "\\(%s\\)" query))

             (periphery--build-list
              :path fileWithLine
              :file file
              :line line
              :keyword "Match"
              :result message
              :regex (format "\\(%s\\)" query)))))))

;; Delegate to new core building function
(cl-defun periphery--build-list (&key path file line keyword result regex)
  "Build list from (as PATH FILE LINE KEYWORD RESULT REGEX)."
  (let* ((processed-msg (periphery--process-message result))
         (patterns
          (delq nil
                (list
                 (when-let ((strings-regex (periphery--get-highlight-pattern 'strings)))
                   (list strings-regex '(face highlight) 0))
                 (when-let ((parens-regex (periphery--get-highlight-pattern 'parentheses)))
                   (list parens-regex '(face periphery-warning-face) 0))
                 (when regex
                   (list regex '(face periphery-identifier-face) 0)))))
         (highlighted-msg (periphery--apply-all-highlights processed-msg patterns)))
    (periphery-core-build-entry
     :path path
     :file file
     :line line
     :severity keyword
     :message highlighted-msg
     :face-fn #'periphery--full-color-from-keyword)))

(defun periphery--process-message (message)
  "Process MESSAGE, optionally trimming the prefix and applying face properties."
  (let* ((trimmed-message (if periphery-trim-message-prefix
                              (periphery--trim-message-prefix message)
                            message))
         (propertized-message (propertize trimmed-message 'face 'periphery-message-face)))
    (if periphery-trim-message-prefix
        propertized-message
      (periphery--highlight-first-sentence propertized-message))))

(defun periphery--trim-message-prefix (message)
  "Trim the prefix of MESSAGE up to the first colon, if there's text after it."
  (if-let* ((colon-pos (string-match ":" message))
            (rest (string-trim (substring message (1+ colon-pos))))
            ((not (string-empty-p rest))))
      rest
    message))

(defun periphery--highlight-first-sentence (text)
  "Highlight the first sentence (up to the first colon) in TEXT."
  (let ((colon-pos (string-match ":" text)))
    (if colon-pos
        (concat
         (propertize (substring text 0 (1+ colon-pos))
                     'face 'periphery-first-sentence-face)
         (substring text (1+ colon-pos)))
      text)))

(cl-defun periphery--build-todo-list (&key path &key file &key line &key keyword &key result &key regex)
  "Build list from (as PATH FILE LINE KEYWORD RESULT REGEX)."
  (let* ((base-text (propertize result 'face (periphery--color-from-keyword keyword)))
         (patterns
          (delq nil
                (list
                 (when-let ((strings-regex (periphery--get-highlight-pattern 'strings)))
                   (list strings-regex '(face highlight) 0))
                 (when-let ((parens-regex (periphery--get-highlight-pattern 'parentheses)))
                   (list parens-regex '(face periphery-warning-face) 0))
                 (when regex
                   (list regex '(face periphery-identifier-face) 0)))))
         (highlighted-text (periphery--apply-all-highlights base-text patterns)))
    (list path (vector
                (periphery--propertize-severity keyword)
                (propertize (file-name-sans-extension (file-name-nondirectory file)) 'face 'periphery-filename-face)
                (propertize line 'face 'periphery-linenumber-face)
                highlighted-text))))

(cl-defun periphery-parse-search-result (&key text query)
  "Parse search result (as TITLE TEXT QUERY)."
  (setq default-length 8)
  (if (string-empty-p text)
      (periphery-message-with-count
       :tag "No matches found"
       :text ""
       :count "0"
       :attributes 'periphery-error-face)
    ;; Use new dynamic parser system
    (let ((found-errors (periphery-run-parser text :search query)))
      (when found-errors
        (progn
          (if (proper-list-p tabulated-list-entries)
              (periphery-message-with-count
               :tag "Found"
               :text ""
               :count (format "%d" (length periphery-errorList))
               :attributes 'success))
          (switch-to-buffer-other-window periphery-buffer-name))))))

;;;###autoload
(defun periphery-parse-ktlint-result (input)
  "Parse Klint result."
  (periphery-run-parser input :ktlint))

;;;###autoload
(defun svg-color-from-tag (tag)
  "Get color from (as TAG)."
  (cond
   ((string-match-p "TODO" tag) 'periphery-todo-face-full)
   ((string-match-p "NOTE:" tag) 'periphery-note-face-full)
   ((string-match-p "HACK" tag) 'periphery-hack-face-full)
   ((string-match-p "PERF" tag) 'periphery-performance-face-full)
   ((string-match-p "FIXME\\|FIX" tag) 'periphery-fix-face-full)
   ((string-match-p "MARK" tag) 'periphery-mark-face-full)
   (t 'periphery-hack-face-full)))

;;;###autoload
(defun periphery--remove-leading-keyword (tag)
  "Remove leading keyword and C style -comment from (as TAG)."
  (replace-regexp-in-string "^[;|\/]+\\W+\\w+\\b:\\W?" "" tag))

;;;###autoload
(defun periphery--remove-comments (tag)
  "Remove comments from (as TAG)."
  (string-trim-left
   (replace-regexp-in-string ":" ""
                             (replace-regexp-in-string "^[;|\/]+\\W+" "" tag))))

;;;###autoload
(defun periphery-svg-tags ()
  "Get svg tags."
  '(
    ("^[ \t]*\\([;|\/][;|\/]?\\W+\\w+\\b:.*\\)" . ((lambda (tag)
                                                     (svg-tag-make (periphery--remove-leading-keyword (concat tag "\t"))
                                                                   :face (svg-color-from-tag tag)
                                                                   :inverse t))))
    ("^[ \t]*\\([;|\/][;|\/]?\\W+\\w+\\b:\\)" . ((lambda (tag)
                                                   (svg-tag-make (periphery--remove-comments tag)
                                                                 :face (svg-color-from-tag tag)
                                                                 :crop-right t))))

    ("^[ \t]*//\\S+\\(swiftlint:[^\s].*\\)$" . ((lambda (tag)
                                                          (let* ((parts (split-string tag ":" t))
                                                                 (action (nth 1 parts))
                                                                 (text (string-join (cddr parts) " "))
                                                                 (face (if (string= action "enable")
                                                                      'periphery-hack-face-full
                                                                    'periphery-fix-face-full)))
                                                            (svg-tag-make text :face face :crop-left t)
                                                            (svg-tag-make action :face face :inverse t)))))))

;;;###autoload
(defun periphery-test-parse-region (start end)
  "Parse region from START to END as compiler output for testing."
  (interactive "r")
  (let ((periphery-debug t)
        (input (buffer-substring-no-properties start end)))
    (message "Testing periphery parser with %d chars of input..." (length input))
    (periphery-run-parser input :compiler)))

;;;###autoload
(defun periphery-diagnose ()
  "Display diagnostic information about periphery configuration."
  (interactive)
  (let ((buf (get-buffer-create "*Periphery Diagnostics*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Periphery Diagnostics ===\n\n")
      (insert (format "Debug mode: %s\n" periphery-debug))
      (insert (format "Buffer name: %s\n" periphery-buffer-name))
      (insert (format "Registered parsers: %d\n" (hash-table-count periphery-registered-parsers)))
      (insert "\nParsers by type:\n")
      (dolist (type '(:compiler :linter :test :search))
        (let ((parsers (periphery-get-parsers-by-type type)))
          (insert (format "  %s: %d parsers\n" type (length parsers)))
          (dolist (p parsers)
            (insert (format "    - %s (priority %d, enabled: %s)\n"
                            (car p)
                            (plist-get (cdr p) :priority)
                            (plist-get (cdr p) :enabled))))))
      (insert "\nCompiler pattern: ")
      (insert (or (alist-get 'compiler periphery-builtin-patterns) "NOT FOUND"))
      (insert "\n\n--- Test parsing a sample error ---\n")
      (let* ((test-line "/test/file.swift:10:5: error: test error message")
             (result (periphery-parser-compiler test-line)))
        (insert (format "Test line: %s\n" test-line))
        (insert (format "Parse result: %s\n" (if result "SUCCESS" "FAILED")))))
    (pop-to-buffer buf)))

;;; Filtering

(defun periphery--filter-by-type (type)
  "Filter entries to show only TYPE (error, warning, note, etc.)."
  (when-let ((buf (get-buffer periphery-buffer-name)))
    (with-current-buffer buf
      ;; Save original list if not already saved
      (when (null periphery--unfiltered-list)
        (setq periphery--unfiltered-list tabulated-list-entries))
      (setq periphery--current-filter type)
      (let ((filtered (if type
                          (seq-filter
                           (lambda (entry)
                             (let ((severity (periphery--get-severity-type entry)))
                               (string= severity type)))
                           periphery--unfiltered-list)
                        periphery--unfiltered-list)))
        (setq tabulated-list-entries filtered)
        (tabulated-list-print t)
        (goto-char (point-min))
        (message "Filter: %s (%d items)"
                 (or type "all")
                 (length filtered))))))

(defun periphery-filter-errors ()
  "Show only errors."
  (interactive)
  (periphery--filter-by-type "error"))

(defun periphery-filter-warnings ()
  "Show only warnings."
  (interactive)
  (periphery--filter-by-type "warning"))

(defun periphery-filter-notes ()
  "Show only notes."
  (interactive)
  (periphery--filter-by-type "note"))

(defun periphery-filter-errors-and-warnings ()
  "Show errors and warnings."
  (interactive)
  (when-let ((buf (get-buffer periphery-buffer-name)))
    (with-current-buffer buf
      (when (null periphery--unfiltered-list)
        (setq periphery--unfiltered-list tabulated-list-entries))
      (setq periphery--current-filter "error+warning")
      (let ((filtered (seq-filter
                       (lambda (entry)
                         (let ((severity (periphery--get-severity-type entry)))
                           (or (string= severity "error")
                               (string= severity "warning"))))
                       periphery--unfiltered-list)))
        (setq tabulated-list-entries filtered)
        (tabulated-list-print t)
        (goto-char (point-min))
        (message "Filter: errors+warnings (%d items)" (length filtered))))))

(defun periphery-filter-clear ()
  "Clear filter and show all entries."
  (interactive)
  (when-let ((buf (get-buffer periphery-buffer-name)))
    (with-current-buffer buf
      (when periphery--unfiltered-list
        (setq tabulated-list-entries periphery--unfiltered-list)
        (setq periphery--current-filter nil)
        (tabulated-list-print t)
        (goto-char (point-min))
        (message "Filter cleared (%d items)" (length tabulated-list-entries))))))

;;; Transient menu

(require 'transient)

(defun periphery--transient-status ()
  "Return current status for transient header."
  (let* ((buf (get-buffer periphery-buffer-name))
         (count (when buf
                  (with-current-buffer buf
                    (length tabulated-list-entries))))
         (total (when buf
                  (with-current-buffer buf
                    (length periphery--unfiltered-list)))))
    (concat
     (format "Filter: %s"
             (propertize (or periphery--current-filter "none")
                         'face 'font-lock-constant-face))
     (when (and count total (not (= count total)))
       (format " | Showing: %s/%s"
               (propertize (number-to-string count) 'face 'warning)
               (number-to-string total)))
     (when (and count (= count (or total count)))
       (format " | Items: %s"
               (propertize (number-to-string count) 'face 'success))))))

(transient-define-prefix periphery-filter-menu ()
  "Filter periphery results."
  [:description periphery--transient-status]
  ["Filter by severity"
   ("e" "Errors only" periphery-filter-errors)
   ("w" "Warnings only" periphery-filter-warnings)
   ("n" "Notes only" periphery-filter-notes)
   ("b" "Errors + Warnings" periphery-filter-errors-and-warnings)]
  ["Actions"
   ("c" "Clear filter (show all)" periphery-filter-clear)
   ("g" "Refresh" revert-buffer)
   ("q" "Quit menu" transient-quit-one)])

;;;###autoload
(transient-define-prefix periphery-transient ()
  "Periphery - Build Errors & Warnings."
  [:description periphery--transient-status]
  ["Filter"
   ("e" "Errors only" periphery-filter-errors)
   ("w" "Warnings only" periphery-filter-warnings)
   ("n" "Notes only" periphery-filter-notes)
   ("b" "Errors + Warnings" periphery-filter-errors-and-warnings)
   ("c" "Clear filter" periphery-filter-clear)]
  ["Actions"
   ("t" "Toggle buffer" periphery-toggle-buffer)
   ("k" "Kill buffer" periphery-kill-buffer)
   ("g" "Refresh" revert-buffer)]
  ["Navigation"
   ("RET" "Open error at point" periphery--open-current-line)
   ("j" "Next error" next-line)
   ("k" "Previous error" previous-line)]
  [("q" "Quit" transient-quit-one)])

(provide 'periphery)
;;; periphery.el ends here
