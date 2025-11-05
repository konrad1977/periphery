<div align="center">
  <img src="logo/periphery.png" alt="Periphery Logo" width="200"/>

  # Periphery

  **Generic Parser Framework for Emacs**

  A flexible, configurable parsing framework for displaying compilation errors, test results, search matches, and linter output in a unified interface within Emacs.
</div>

## Features

- **Modular Parser System**: Register and configure parsers for different output types
- **Unified Display**: All results shown in a consistent tabulated list format
- **Extensible**: Easy to add new parsers for different tools
- **Performance Optimized**: Async parsing support for large outputs
- **Rich Syntax Highlighting**: Customizable faces for different severity levels and syntax elements

## Screenshots

### Compiler Errors and Warnings
![Periphery displaying compiler errors](screenshots/periphery-errors.png)

### TODO/FIXME Tracking
![Periphery displaying TODO items](screenshots/periphery-todo.png)

## Installation

Load periphery in your Emacs configuration:

```elisp
(require 'periphery)
```

## Public API

### Parsing Functions

#### `periphery-run-parser`
Parse input with automatic parser selection.

```elisp
;; Parse compiler output
(periphery-run-parser compiler-output :compiler)

;; Parse search results with query highlighting
(periphery-run-parser search-output :search "TODO")

;; Parse linter output
(periphery-run-parser ktlint-output :ktlint)

;; Parse test results
(periphery-run-parser test-output :test)
```

#### `periphery-core-parse`
Lower-level parsing function with more control.

```elisp
(periphery-core-parse
  :input some-output
  :type :compiler  ; :compiler, :search, :linter, or :test
  :callback (lambda (results)
              (message "Found %d issues" (length results))))
```

#### `periphery-core-run-async`
Parse input asynchronously (useful for large outputs).

```elisp
(periphery-core-run-async
  compiler-output
  :compiler
  (lambda (results)
    (message "Async parse complete: %d issues" (length results))))
```

### Parser Management

#### `periphery-register-parser`
Register a new parser for your tool.

```elisp
(periphery-register-parser
  'my-parser-id
  :name "Display Name"
  :regex "regex-pattern"
  :type :linter  ; :compiler, :search, :linter, or :test
  :priority 50   ; Higher = processed first (default: 50)
  :parse-fn #'my-parse-function
  :face-fn #'my-face-function)
```

#### `periphery-get-parser`
Get a parser configuration by ID.

```elisp
(periphery-get-parser 'compiler)
```

#### `periphery-get-parsers-by-type`
Get all enabled parsers of a specific type.

```elisp
(periphery-get-parsers-by-type :linter)
```

#### `periphery-enable-parser` / `periphery-disable-parser`
Interactively enable or disable parsers.

```elisp
M-x periphery-enable-parser
M-x periphery-disable-parser
```

### Buffer Management

#### `periphery:toggle-buffer`
Toggle the visibility of the Periphery results buffer.

```elisp
M-x periphery:toggle-buffer
```

#### `periphery-kill-buffer`
Close and kill the Periphery buffer.

```elisp
M-x periphery-kill-buffer
```

### Syntax Highlighting

#### `periphery-add-highlight-pattern`
Add custom syntax highlighting patterns for error messages.

```elisp
(periphery-add-highlight-pattern
  'backticks
  "`\\([^`]+\\)`"
  'periphery-identifier-face)
```

#### `periphery-remove-highlight-pattern`
Remove a syntax highlighting pattern.

```elisp
M-x periphery-remove-highlight-pattern
```

#### `periphery-list-highlight-patterns`
List all configured highlight patterns.

```elisp
M-x periphery-list-highlight-patterns
```

### Configuration Variables

- `periphery-debug` - Enable debug output (default: `nil`)
- `periphery-trim-message-prefix` - Trim message prefixes up to first colon (default: `nil`)

## Creating a New Parser

Follow these steps to create a parser for a new tool:

### 1. Write a Parse Function

Your parse function receives a line of text and should return an entry or `nil`:

```elisp
(defun my-tool-parser (line)
  "Parse output from my-tool."
  (when (string-match "\\([^:]+\\):\\([0-9]+\\): \\(\\w+\\) - \\(.*\\)" line)
    (let ((file (match-string 1 line))
          (line-num (match-string 2 line))
          (severity (match-string 3 line))
          (message (match-string 4 line)))
      (periphery-core-build-entry
        :path (format "%s:%s" file line-num)
        :file file
        :line line-num
        :severity severity
        :message message
        :face-fn #'my-tool-face-function))))
```

### 2. Create a Face Function (Optional)

Map severity levels to display faces:

```elisp
(defun my-tool-face-function (severity)
  "Return face for SEVERITY level."
  (pcase (downcase severity)
    ("error" 'periphery-error-face-full)
    ("warning" 'periphery-warning-face-full)
    ("info" 'periphery-info-face-full)
    (_ 'periphery-note-face-full)))
```

### 3. Register Your Parser

Register the parser to make it active:

```elisp
(periphery-register-parser
  'my-tool
  :name "My Tool"
  :regex "\\([^:]+\\):\\([0-9]+\\): \\(\\w+\\) - \\(.*\\)"
  :type :linter
  :priority 60
  :parse-fn #'my-tool-parser
  :face-fn #'my-tool-face-function)
```

### 4. Use Your Parser

```elisp
;; Parse my-tool output
(periphery-run-parser my-tool-output :linter)

;; Or specifically use your parser
(periphery-core-parse :input my-tool-output :parsers '(my-tool))
```

### Complete Example: ESLint Parser

Here's a complete example showing all steps:

```elisp
;; Step 1: Parse function
(defun my-eslint-parser (line)
  "Parse ESLint output from LINE."
  (when (string-match "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\) - \\(.*\\)" line)
    (let ((file (match-string 1 line))
          (line-num (match-string 2 line))
          (col (match-string 3 line))
          (severity (match-string 4 line))
          (message (match-string 5 line)))
      (periphery-core-build-entry
        :path (format "%s:%s:%s" file line-num col)
        :file file
        :line line-num
        :column col
        :severity severity
        :message message
        :face-fn #'my-eslint-face))))

;; Step 2: Face function
(defun my-eslint-face (severity)
  "Return face for ESLint SEVERITY."
  (pcase (downcase severity)
    ("error" 'periphery-error-face-full)
    ("warning" 'periphery-warning-face-full)
    (_ 'periphery-info-face-full)))

;; Step 3: Register parser
(periphery-register-parser
  'eslint
  :name "ESLint"
  :regex "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\) - \\(.*\\)"
  :type :linter
  :priority 70
  :parse-fn #'my-eslint-parser
  :face-fn #'my-eslint-face)

;; Step 4: Use it
(periphery-run-parser eslint-output :linter)
```

## Built-in Parsers

Periphery includes parsers for:

- **Compiler**: Xcode/Swift compiler errors and warnings
- **XCTest**: Xcode test results
- **Search**: Generic search results with TODO/FIXME detection
- **Ktlint**: Kotlin linter output
- **SwiftLint**: Swift linter output

## Available Faces

### Severity Faces
- `periphery-error-face` / `periphery-error-face-full`
- `periphery-warning-face` / `periphery-warning-face-full`
- `periphery-info-face` / `periphery-info-face-full`
- `periphery-note-face` / `periphery-note-face-full`

### Special Keyword Faces
- `periphery-todo-face` / `periphery-todo-face-full`
- `periphery-fix-face` / `periphery-fix-face-full`
- `periphery-performance-face` / `periphery-performance-face-full`
- `periphery-hack-face-full`

### UI Element Faces
- `periphery-filename-face` - File names in results
- `periphery-linenumber-face` - Line numbers
- `periphery-identifier-face` - Highlighted identifiers
- `periphery-message-face` - Message text

Note: `-full` variants include background colors for the severity badges.

## License
MIT
