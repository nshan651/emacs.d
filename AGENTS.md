# AGENTS.md - Emacs Configuration Agent Guide

## Overview

This is a GNU Emacs configuration repository (~/.emacs.d) managed by nshan651.
The main source of truth is `README.org`, which tangles to `.el` files.

## Project Structure

```
~/.emacs.d/
├── README.org              # Main config source (tangles to .el files)
├── init.el                 # Entry point, loads lisp/*.el
├── early-init.el           # Early startup config
├── lisp/                   # Modular config files
│   ├── evil-config.el      # Vim keybindings (evil-mode)
│   ├── ui-config.el        # UI packages (consult, vertico, etc.)
│   ├── org-config.el       # Org-mode configuration
│   ├── org-roam-config.el # Org-roam (PKM system)
│   ├── development-config.el # LSP, magit, debugging
│   ├── theme-config.el     # Colors, fonts, modeline
│   ├── window-config.el    # Window/perspective management
│   ├── terminal-config.el  # Shell configuration
│   ├── dired-config.el     # File manager
│   └── programming-languages-config.el
└── etc/
    └── feed.el             # RSS feeds for elfeed
```

## Build/Lint/Test Commands

### Emacs Lisp Validation

```bash
# Check syntax of a single elisp file
emacs --batch -Q -l /path/to/file.el -f batch-byte-compile 2>&1

# Evaluate a single function or test load
emacs --batch -Q -l /path/to/file.el -eval "(message \"Loaded successfully\")"

# Check for undefined functions/variables
emacs --batch -Q -l /path/to/file.el -eval "(mapatoms (lambda (s) (when (and (boundp s) (symbol-file s) (string-match \"^nil$\" (prin1-to-string (symbol-file s)))) (message \"%s\" s)))))"

# Byte-compile all lisp files to check for errors
emacs --batch -Q --eval "(dolist (f (directory-files-recursively \"lisp\" \"\\.el$\")) (condition-case err (progn (load f) (message \"OK: %s\" f)) (error (message \"ERROR in %s: %s\" f err))))"
```

### Org Babel Tangling

```bash
# Retangle all source blocks in README.org
# Open README.org in Emacs and run: M-x org-babel-tangle

# Tangle specific file
# In README.org: C-c C-v t (org-babel-tangle)
```

### Testing Changes

```bash
# Test configuration by launching Emacs with --debug-init
emacs --debug-init

# Test with temporary config directory
emacs -q -l /path/to/test-config.el

# Evaluate buffer (quick test): C-M-x (eval-defun)
# Evaluate region: C-x C-e (eval-last-sexp)
# Load file: M-x load-file
```

## Code Style Guidelines

### Naming Conventions

- **Prefix**: All custom functions/variables use `ns/` prefix
- **Leader keys**: Defined with `ns/leader-*` pattern
  - `ns/leader-spc` - General prefix (SPC)
  - `ns/leader-t` - Toggle/cosmetic prefix (t)
  - `ns/leader-m` - Magit operations (m)
  - `ns/leader-ca` - Window management (C-a)
  - `ns/leader-ct` - Misc operations (C-t)
- **Predicates**: End with `p` (e.g., `ns/foo-p`)
- **Private functions**: Use `--` separator (e.g., `ns--helper-function`)
- **Constants**: Use `+` suffix or SCREAMING_SNAKE_CASE

### Formatting

- **Indentation**: 4 spaces (no tabs)
- **Fill column**: 80 characters
- **Line endings**: Unix (LF)
- **Trailing whitespace**: Avoid

```elisp
;; Good
(setq some-long-variable-name
      initial-value)

;; Bad (tab indentation, 2 spaces)
  (setq foo
    bar)
```

### use-package Style

```elisp
(use-package package-name
  :commands (cmd1 cmd2)           ; Autoload commands
  :bind (("C-c k" . cmd))        ; Keybindings
  :general                         ; general.el keybindings
  :hook (mode . hook-func)        ; Hook into modes
  :custom                          ; Customizable vars
  :init                            ; Before package loads
  :config                          ; After package loads
  :after (dep1 dep2)              ; Load after deps
  :disabled t)                    ; Disable package
```

### Comments

- Use `;;` for single-line comments
- Use `;; === Section ===` for major sections
- Inline comments on same line, space after code:

```elisp
(setq foo t) ; This is a comment explaining why
```

### Documentation Strings

- Always provide docstrings for interactive functions
- Use imperative mood ("Toggle..." not "Toggles...")
- First line should be a complete sentence

```elisp
(defun ns/toggle-feature ()
  "Toggle the feature.
Does not affect other features."
  (interactive)
  ...)
```

### Error Handling

- Use `condition-case` for graceful error handling:

```elisp
(condition-case err
    (risky-operation)
  (error (message "Failed: %s" (error-message-string err))))
```

- Use `user-error` for expected user mistakes (interactive functions)

### Packages to Use

- `use-package` for all package configuration
- `general.el` for keybindings (not `define-key` directly)
- `evil-mode` integration via `evil-collection`
- `eglot` (not lsp-mode) for language servers
- `vertico` + `consult` + `marginalia` + `corfu` for completion

### Package Keywords

- `:ensure nil` - For built-in packages (org, dired)
- `:defer t` - Lazy loading
- `:disabled t` - Temporarily disabled packages
- `:diminish` - Hide minor mode from modeline

### File Organization

1. Package requires at top
2. Custom variables/setters
3. Hook functions
4. Interactive commands
5. Non-interactive helper functions
6. use-package configurations ordered by dependency

### Keybinding Patterns

```elisp
;; Global keys
(keymap-global-set "C-x C-b" 'ibuffer)
(keymap-global-set "<escape>" 'keyboard-escape-quit)

;; Leader key bindings
(ns/leader-spc
  "f"  'find-file
  "k"  'kill-buffer)

;; Mode-specific
(general-def 'normal
  :prefix "g"
  "c" 'compile)

;; With which-key descriptions
(ns/leader-spc
  "f"  '(find-file :wk "find file"))
```

### Prohibited Patterns

- Don't use `package-initialize` manually
- Don't use `M-x customize` - write elisp directly
- Don't add customizations to init.el directly
- Don't hardcode paths outside of `~/.emacs.d`

## Common Tasks

### Adding a New Package

1. Add source block to `README.org` under appropriate section
2. Include `:tangle "lisp/<file>.el"` header
3. Use `org-babel-tangle` (C-c C-v t) to generate `.el` file
4. Add file to `ns/file-list` in `init.el` (if in lisp/ directory)

### Adding a Keybinding

1. Find or create appropriate `general-create-definer`
2. Add binding in appropriate config file
3. Include `:wk` description for which-key

### Modifying Existing Config

1. Edit source in `README.org` (not the tangled `.el`)
2. Re-tangle with `C-c C-v t` in README.org buffer
3. Test with `M-x load-file` on the tangled file

## Development Tools

- `eglot` - Language server protocol (auto-format on save)
- `flymake` - Syntax checking (enabled in prog-mode)
- `helpful` - Enhanced help commands
- `magit` - Git interface (SPC m prefix)
- `project.el` - Project management (built-in)

## Configuration Philosophy

- Prefer built-in Emacs features when possible
- Use package.el + use-package for package management
- Keep config modular by feature/domain
- Source of truth is README.org (tangles to .el files)
- No custom theme - use modus-vivendi with custom palette
- Evil-mode for vim keybindings throughout
