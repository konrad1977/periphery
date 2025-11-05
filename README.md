# Periphery - Generic Parser Framework for Emacs

Periphery is a flexible, configurable parsing framework for displaying compilation errors, test results, search matches, and linter output in a unified interface.

## Features

- **Modular Parser System**: Register and configure parsers for different output types
- **Unified Display**: All results shown in a consistent tabulated list format
- **Extensible**: Easy to add new parsers for different tools
- **Performance Optimized**: Async parsing support for large outputs
- **Rich Faces**: Customizable colors and faces for different severity levels

## Architecture

The system is split into several components:

### periphery-config.el
Configuration and registration system for parsers. Provides:
- Parser registration API
- Enable/disable parsers dynamically
- Query parsers by type
- Built-in pattern definitions

### periphery-core.el
Core parsing engine that:
- Processes input through registered parsers
- Sorts and deduplicates results
- Provides async parsing support
- Maintains compatibility layer for existing code

### periphery-parsers.el
Implementation of actual parsers for:
- Compiler errors/warnings
- XCTest results
- Search results with TODO/FIXME detection
- Ktlint output
- SwiftLint output

### periphery.el
Main UI and display logic:
- Tabulated list display
- Navigation and opening files
- Face definitions
- Buffer management

## Usage

### Basic Usage

```elisp
;; Parse compiler output
(periphery-run-parser compiler-output)

;; Parse search results
(periphery-parse-search-result :text search-output :query "TODO")

;; Parse with specific parser type
(periphery-core-parse 
  :input some-output
  :type :compiler  ; or :search, :linter, :test
  :callback (lambda (results) 
              (message "Found %d issues" (length results))))
```

### Registering Custom Parsers

```elisp
(periphery-register-parser
  'my-linter
  :name "My Custom Linter"
  :regex "\\([^:]+\\):\\([0-9]+\\): \\(.*\\)"
  :type :linter
  :priority 50
  :parse-fn (lambda (line)
              ;; Parse line and return entry or nil
              (when (string-match my-regex line)
                (periphery-core-build-entry
                  :path (format "%s:%s" (match-string 1 line) (match-string 2 line))
                  :file (match-string 1 line)
                  :line (match-string 2 line)
                  :severity "warning"
                  :message (match-string 3 line)
                  :face-fn #'my-face-function)))
  :face-fn (lambda (severity)
             ;; Return appropriate face for severity
             'periphery-warning-face))
```

### Managing Parsers

```elisp
;; Enable/disable parsers interactively
M-x periphery-enable-parser
M-x periphery-disable-parser

;; Get all parsers of a type
(periphery-get-parsers-by-type :linter)

;; Get specific parser config
(periphery-get-parser 'compiler)
```

## Configuration

### Custom Faces

Periphery provides many faces for customization:

- `periphery-error-face` - Error text
- `periphery-warning-face` - Warning text  
- `periphery-info-face` - Informational text
- `periphery-todo-face` - TODO items
- `periphery-fix-face` - FIXME items
- `periphery-performance-face` - Performance notes
- `periphery-filename-face` - File names
- `periphery-linenumber-face` - Line numbers

Each severity also has a `-full` variant for backgrounds.

### Variables

- `periphery-debug` - Enable debug output
- `periphery-trim-message-prefix` - Trim message prefixes
- `periphery-buffer-name` - Name of the display buffer

## Parser Types

The system supports four parser types:

1. **:compiler** - Compilation errors/warnings
2. **:search** - Search results and TODOs
3. **:linter** - Linter output (ktlint, swiftlint, etc.)
4. **:test** - Test results (XCTest, etc.)

## Extending

To add support for a new tool:

1. Define the regex pattern
2. Create a parse function
3. Register the parser
4. Optionally add a face function

Example for ESLint:

```elisp
(defun my-parse-eslint (line)
  (when (string-match "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)\\s+\\(\\w+\\)\\s+\\(.*\\)" line)
    (periphery-core-build-entry
      :path (format "%s:%s:%s" 
                    (match-string 1 line)
                    (match-string 2 line) 
                    (match-string 3 line))
      :file (match-string 1 line)
      :line (match-string 2 line)
      :column (match-string 3 line)
      :severity (match-string 4 line)
      :message (match-string 5 line)
      :face-fn #'periphery-parser--severity-face)))

(periphery-register-parser
  'eslint
  :name "ESLint"
  :regex "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)\\s+\\(\\w+\\)\\s+\\(.*\\)"
  :type :linter
  :priority 70
  :parse-fn #'my-parse-eslint)
```

## Migration from Old System

The new system maintains backward compatibility through wrapper functions:
- `periphery-run-parser` - Works as before
- `periphery-parse-search-result` - Works as before
- `periphery-parse-ktlint-result` - Can be updated to use new system

To migrate custom code:
1. Replace direct regex matching with parser registration
2. Use `periphery-core-parse` instead of manual parsing
3. Use `periphery-core-build-entry` for consistent entry format

## Performance

- Async parsing available via `periphery-core-run-async`
- Automatic deduplication of results
- Intelligent sorting by severity
- Lazy rendering in tabulated list mode

## License

Part of Emacs configuration - see main repository for license details.