;;; periphery-config.el --- Configuration system for periphery parsers -*- lexical-binding: t -*-

;;; Commentary:
;; This file provides a generic configuration system for periphery parsers.
;; It allows easy registration and management of different parser types.

;;; Code:

(require 'cl-lib)

(defgroup periphery-config nil
  "Configuration for periphery parser system."
  :group 'periphery)

;;; Customizable variables

(defcustom periphery-background-darkness 85
  "Percentage to darken foreground colors for backgrounds.
Higher values create darker backgrounds (85 means 85% darker).
Used when generating background colors from foreground colors."
  :type 'integer
  :group 'periphery-config)

(defcustom periphery-use-theme-colors t
  "When non-nil, use theme colors for error, warning, and info faces.
When nil, use the default Catppuccin colors."
  :type 'boolean
  :group 'periphery-config)

;;; Color utility functions

(defvar periphery--color-cache (make-hash-table :test 'equal)
  "Cache for computed colors to avoid repeated calculations.")

(defun periphery--get-cached-color (cache-key color-fn &rest args)
  "Get color from cache or compute and cache it using COLOR-FN with ARGS.
CACHE-KEY is used to store and retrieve the computed color."
  (or (gethash cache-key periphery--color-cache)
      (puthash cache-key (apply color-fn args) periphery--color-cache)))

(defun periphery--clear-color-cache ()
  "Clear the color cache.
Call this after changing theme or color customization variables."
  (interactive)
  (clrhash periphery--color-cache))

(defun periphery--color-to-rgb (color)
  "Convert COLOR (hex or name) to RGB components (0-255)."
  (let ((cache-key (format "rgb-%s" color)))
    (periphery--get-cached-color
     cache-key
     (lambda (c)
       (let ((rgb (color-values c)))
         (if rgb
             (mapcar (lambda (x) (/ x 256)) rgb)
           (error "Invalid color: %s" c))))
     color)))

(defun periphery--rgb-to-hex (r g b)
  "Convert R G B components (0-255) to hex color string."
  (format "#%02x%02x%02x"
          (min 255 (max 0 (floor r)))
          (min 255 (max 0 (floor g)))
          (min 255 (max 0 (floor b)))))

(defun periphery--darken-color (color percent)
  "Darken COLOR by PERCENT (0-100).
100 percent means completely black, 0 means no change."
  (let ((cache-key (format "darken-%s-%d" color percent)))
    (periphery--get-cached-color
     cache-key
     (lambda (c p)
       (let* ((rgb (periphery--color-to-rgb c))
              (darkened (mapcar (lambda (component)
                                  (* component (- 100 p) 0.01))
                                rgb)))
         (apply #'periphery--rgb-to-hex darkened)))
     color percent)))

(defun periphery--get-theme-face-color (face-name attribute &optional fallback)
  "Get color for FACE-NAME's ATTRIBUTE from current theme.
If not found, return FALLBACK color."
  (let ((color (face-attribute face-name attribute nil t)))
    (if (or (eq color 'unspecified) (not color))
        fallback
      color)))

(defun periphery--create-background-from-foreground (fg-color &optional darkness)
  "Create a darker background color from FG-COLOR.
DARKNESS is the percentage to darken (default `periphery-background-darkness').
Higher values create darker backgrounds."
  (let* ((darkness (or darkness periphery-background-darkness))
         (cache-key (format "bg-%s-%d" fg-color darkness)))
    (periphery--get-cached-color
     cache-key
     (lambda (fg d)
       (periphery--darken-color fg d))
     fg-color darkness)))

(defun periphery--get-face-colors (face-keyword &optional theme-face fallback-fg)
  "Get foreground and background colors for FACE-KEYWORD.
THEME-FACE is the theme face to query when `periphery-use-theme-colors' is non-nil.
FALLBACK-FG is the fallback foreground color.
Returns a cons cell (FG-COLOR . BG-COLOR)."
  (let ((fg (if (and periphery-use-theme-colors theme-face)
                (periphery--get-theme-face-color theme-face :foreground fallback-fg)
              fallback-fg)))
    (cons fg (periphery--create-background-from-foreground fg))))

(defun periphery--get-face-with-background (face-symbol)
  "Create a face specification with background from FACE-SYMBOL.
Takes the foreground color from FACE-SYMBOL and generates a darker
background using `periphery-background-darkness'."
  (let* ((fg-color (face-attribute face-symbol :foreground nil t))
         (bg-color (when (and fg-color (not (eq fg-color 'unspecified)))
                     (periphery--create-background-from-foreground fg-color))))
    (if bg-color
        (list :foreground fg-color
              :background bg-color
              :weight 'bold
              :distant-foreground fg-color)
      ;; Fallback if we can't get the foreground color
      (list :inherit face-symbol :weight 'bold))))

;; Face definitions
(defface periphery-filename-face
  '((t (:inherit link)))
  "Filename face."
  :group 'periphery)

(defface periphery-linenumber-face
  '((t (:inherit line-number)))
  "Line number face."
  :group 'periphery)

(defface periphery-warning-face
  '((t (:foreground "#f9e2af")))
  "Warning face."
  :group 'periphery)

(defface periphery-error-face
  '((t (:foreground "#f38ba8")))
  "Error face."
  :group 'periphery)

(defface periphery-message-face
  '((t (:foreground "#fbfafb")))
  "Message face."
  :group 'periphery)

(defface periphery-fix-face
  '((t (:foreground "#89b4fa")))
  "FIX|FIXME face."
  :group 'periphery)

(defface periphery-note-face
  '((t (:foreground "#a6e3a1")))
  "Info/Note face."
  :group 'periphery)

(defface periphery-info-face
  '((t (:inherit periphery-note-face)))
  "Info face (inherits from note)."
  :group 'periphery)

(defface periphery-performance-face
  '((t (:foreground "#cba6f7")))
  "Performance face."
  :group 'periphery)

(defface periphery-hack-face
  '((t (:foreground "#f38ba8")))
  "Hack face."
  :group 'periphery)

(defface periphery-todo-face
  '((t (:foreground "#74c7ec")))
  "TODO face."
  :group 'periphery)

(defface periphery-mark-face
  '((t (:foreground "#9399b2")))
  "Mark face."
  :group 'periphery)

(defface periphery-first-sentence-face
  '((t (:foreground "#9399b2")))
  "Face for the first sentence of the message (up to the first colon)."
  :group 'periphery)

(defcustom periphery-parser-configs nil
  "Alist of parser configurations.
Each entry is (PARSER-ID . PLIST) where PLIST contains:
  :name         - Display name for the parser
  :regex        - Main regex pattern for parsing
  :type         - Parser type (:compiler :search :linter :test)
  :enabled      - Whether parser is enabled
  :priority     - Sort priority (higher = earlier)
  :parse-fn     - Function to parse a line/match
  :face-fn      - Function to determine face from keyword
  :filter-fn    - Optional function to filter/transform input"
  :type '(alist :key-type symbol :value-type plist)
  :group 'periphery-config)

(defvar periphery-registered-parsers (make-hash-table :test 'eq)
  "Hash table of registered parser configurations.")

;;;###autoload
(cl-defun periphery-register-parser (id &key name regex type parse-fn
                                        face-fn filter-fn priority enabled)
  "Register a parser with given ID and configuration.
ID is a unique symbol identifying the parser.
NAME is the display name.
REGEX is the pattern to match.
TYPE is one of :compiler :search :linter :test.
PARSE-FN is a function (LINE) -> ENTRY or nil.
FACE-FN is a function (KEYWORD) -> FACE.
FILTER-FN is optional function (INPUT) -> FILTERED-INPUT.
PRIORITY determines sort order (default 50).
ENABLED determines if parser is active (default t)."
  (let ((config (list :name (or name (symbol-name id))
                      :regex regex
                      :type (or type :compiler)
                      :parse-fn parse-fn
                      :face-fn face-fn
                      :filter-fn filter-fn
                      :priority (or priority 50)
                      :enabled (if (null enabled) t enabled))))
    (puthash id config periphery-registered-parsers)
    config))

;;;###autoload
(defun periphery-get-parser (id)
  "Get parser configuration by ID."
  (gethash id periphery-registered-parsers))

;;;###autoload
(defun periphery-get-parsers-by-type (type)
  "Get all parsers of given TYPE sorted by priority."
  (let (parsers)
    (maphash (lambda (id config)
               (when (and (plist-get config :enabled)
                          (eq (plist-get config :type) type))
                 (push (cons id config) parsers)))
             periphery-registered-parsers)
    (sort parsers (lambda (a b)
                    (> (plist-get (cdr a) :priority)
                       (plist-get (cdr b) :priority))))))

;;;###autoload
(defun periphery-enable-parser (id)
  "Enable parser with given ID."
  (interactive (list (periphery--select-parser "Enable parser: ")))
  (when-let* ((config (gethash id periphery-registered-parsers)))
    (plist-put config :enabled t)
    (puthash id config periphery-registered-parsers)
    (message "Enabled parser: %s" (plist-get config :name))))

;;;###autoload
(defun periphery-disable-parser (id)
  "Disable parser with given ID."
  (interactive (list (periphery--select-parser "Disable parser: ")))
  (when-let* ((config (gethash id periphery-registered-parsers)))
    (plist-put config :enabled nil)
    (puthash id config periphery-registered-parsers)
    (message "Disabled parser: %s" (plist-get config :name))))

(defun periphery--select-parser (prompt)
  "Select a parser interactively with PROMPT."
  (let (parsers)
    (maphash (lambda (id config)
               (push (cons (format "%s [%s]"
                                   (plist-get config :name)
                                   (if (plist-get config :enabled) "ON" "OFF"))
                           id)
                     parsers))
             periphery-registered-parsers)
    (cdr (assoc (completing-read prompt parsers nil t) parsers))))

;; Syntax highlighting configuration
(defcustom periphery-highlight-patterns
  '((parentheses . "\\(\(.+?\)\\)")
    (strings . "\\(\"[^\"]+\"\\)")
    (quotes . "\\('[^']+'\\)")
    ;; Separate patterns for marks and content
    (quote-marks . "\\('\\)")
    (quote-content . "'\\([^']+\\)'")
    (string-marks . "\\(\"\\)")
    (string-content . "\"\\([^\"]+\\)\""))
  "Regex patterns for syntax highlighting in messages.
Each entry is (ELEMENT . PATTERN) where ELEMENT is the syntax element
and PATTERN is the regex to match it."
  :type '(alist :key-type symbol :value-type string)
  :group 'periphery-config)

(defcustom periphery-syntax-faces
  '((parentheses . periphery-error-face)
    (strings . highlight)
    (quotes . highlight)
    (quote-content . periphery-identifier-face)
    (quote-marks . periphery-identifier-face)
    (string-content . periphery-identifier-face)
    (string-marks . periphery-identifier-face))
  "Face configuration for syntax highlighting in error messages.
Each entry is (ELEMENT . FACE) where ELEMENT is the syntax element
and FACE is the face to apply."
  :type '(alist :key-type symbol :value-type face)
  :group 'periphery-config)

;;;###autoload
(defun periphery-add-highlight-pattern (element pattern &optional face)
  "Add a new highlight pattern for ELEMENT with PATTERN and optional FACE.
ELEMENT is a symbol identifying the syntax element.
PATTERN is the regex pattern to match.
FACE is the face to apply (defaults to periphery-identifier-face)."
  (interactive "SElement name: \nsRegex pattern: ")
  (let ((face (or face 'periphery-identifier-face)))
    ;; Add pattern
    (setf (alist-get element periphery-highlight-patterns) pattern)
    ;; Add face mapping
    (setf (alist-get element periphery-syntax-faces) face)
    (message "Added highlight pattern for %s" element)))

;;;###autoload
(defun periphery-remove-highlight-pattern (element)
  "Remove highlight pattern for ELEMENT."
  (interactive
   (list (intern (completing-read "Remove pattern: "
                                  (mapcar #'symbol-name
                                          (mapcar #'car periphery-highlight-patterns))
                                  nil t))))
  ;; Remove from both alists
  (setq periphery-highlight-patterns 
        (assq-delete-all element periphery-highlight-patterns))
  (setq periphery-syntax-faces 
        (assq-delete-all element periphery-syntax-faces))
  (message "Removed highlight pattern for %s" element))

;;;###autoload
(defun periphery-list-highlight-patterns ()
  "List all configured highlight patterns."
  (interactive)
  (message "Configured highlight patterns:")
  (dolist (pattern periphery-highlight-patterns)
    (let ((element (car pattern))
          (regex (cdr pattern))
          (face (alist-get (car pattern) periphery-syntax-faces)))
      (message "  %s: %s -> %s" element regex face))))

;; Built-in parser configurations  
(defconst periphery-builtin-patterns
  '((compiler . "\\(/[^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.*\\)")
    (xctest . "^\\([^:]+\\):\\([0-9]+\\):\\w?\\([^:]*\\)[^.]+\\.\\([^:|]*\\)\\s?:\\(.*\\)")
    (search . "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).\\(.*\\)")
    (todos . "\\(TODO\\|PERF\\|NOTE\\|FIXME\\|FIX\\|HACK\\|MARK\\)[[:space:]]*:?[[:space:]]*\\(.*\\)")
    (ktlint . "\\(^[^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\([^(]+\\) (\\(standard:[^)]+\\))")
    (package-error . "^xcodebuild: error: Could not resolve package dependencies:")
    (build-failure . "^The following build commands failed:"))
  "Built-in regex patterns for common parsing scenarios.")

;;;###autoload
(defun periphery-config-initialize-defaults ()
  "Initialize default parser configurations."
  
  ;; Compiler errors/warnings parser
  (periphery-register-parser
   'compiler
   :name "Compiler Errors"
   :regex (alist-get 'compiler periphery-builtin-patterns)
   :type :compiler
   :priority 100
   :parse-fn #'periphery--parse-compiler-line
   :face-fn #'periphery--face-from-severity)
  
  ;; XCTest parser
  (periphery-register-parser
   'xctest
   :name "XCTest Results"
   :regex (alist-get 'xctest periphery-builtin-patterns)
   :type :test
   :priority 90
   :parse-fn #'periphery--parse-xctest-line
   :face-fn #'periphery--face-from-severity)
  
  ;; Search results parser
  (periphery-register-parser
   'search
   :name "Search Results"
   :regex (alist-get 'search periphery-builtin-patterns)
   :type :search
   :priority 80
   :parse-fn #'periphery--parse-search-line
   :face-fn #'periphery--face-from-match-type)
  
  ;; TODO/FIXME parser
  (periphery-register-parser
   'todos
   :name "TODOs and FIXMEs"
   :regex (alist-get 'todos periphery-builtin-patterns)
   :type :search
   :priority 70
   :parse-fn #'periphery--parse-todo-line
   :face-fn #'periphery--face-from-todo-keyword)
  
  ;; Ktlint parser
  (periphery-register-parser
   'ktlint
   :name "Ktlint"
   :regex (alist-get 'ktlint periphery-builtin-patterns)
   :type :linter
   :priority 60
   :parse-fn #'periphery--parse-ktlint-line
   :face-fn #'periphery--face-from-severity))

;; Parser function stubs (to be implemented in periphery-parsers.el)
(defun periphery--parse-compiler-line (line)
  "Parse a compiler error/warning LINE."
  nil) ; Placeholder

(defun periphery--parse-xctest-line (line)
  "Parse an XCTest result LINE."
  nil) ; Placeholder

(defun periphery--parse-search-line (line)
  "Parse a search result LINE."
  nil) ; Placeholder

(defun periphery--parse-todo-line (line)
  "Parse a TODO/FIXME LINE."
  nil) ; Placeholder

(defun periphery--parse-ktlint-line (line)
  "Parse a ktlint warning LINE."
  nil) ; Placeholder

(defun periphery--face-from-severity (severity)
  "Get face with background for given SEVERITY level."
  (let ((base-face (intern (format "periphery-%s-face" (downcase severity)))))
    (periphery--get-face-with-background base-face)))

(defun periphery--face-from-match-type (type)
  "Get face for given match TYPE."
  'periphery-warning-face)

(defun periphery--face-from-todo-keyword (keyword)
  "Get face for given TODO KEYWORD."
  (intern (format "periphery-%s-face" (downcase keyword))))

(provide 'periphery-config)
;;; periphery-config.el ends here
