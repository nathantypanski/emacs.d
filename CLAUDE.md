# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

This is a highly modular Emacs configuration built around Evil-mode (Vim emulation) with modern package management. The configuration uses a systematic `my-` namespace prefix and follows a strict modular loading order.

### Package Management
- **straight.el** as the primary package manager (bootstrapped in `init.el`)
- **use-package** for declarative package configuration
- Built-in packages (`org`, `eglot`, `eldoc`, `python`) are excluded from straight.el management

### Configuration Structure

**Main Entry Point:**
- `init.el` - Bootstraps package management and loads modules in specific order
- `custom.el` - Contains Emacs customization variables (separate from main config)

**Module Organization:**
- `pkg/` - In-house packages, like `claude-agent`
- `config/` - Main configuration modules, all prefixed with `my-`
- `config/eyecandy/` - Theme and visual configurations  
- `config/languages/` - Language-specific configurations
- `elisp/` - Custom elisp functions and utilities

### Loading Order

The configuration has a strict loading hierarchy defined in `init.el`:

1. **Core Infrastructure** - `my-env`, `my-core`, `my-functions`
2. **UI & Integration** - `my-eyecandy`, `my-keychain`, `my-dired`, `my-buffers`
3. **Development Tools** - `my-completion`, `my-projects`, `my-ag`, `my-tags`
4. **Keybindings** - `my-keys`, `my-leader-keys` (loaded last)
5. **Evil Mode** - `my-evil` (loaded after most other systems)

**Conditional Loading:**
- `my-tmux-frames` only loads when `$TMUX` environment variable is set
- Platform-specific configurations for Mac vs Linux

## Key Architectural Patterns

### The `after` Macro
A custom lazy-loading macro defined in `my-core.el`:
```elisp
(after 'feature &rest body) ; Evaluates body after feature is loaded
```

### Use-Package Conventions
- `:demand t` for critical packages that must load immediately
- `:ensure t` or `:ensure <package-name>` for package installation
- Extensive use of hooks and custom configurations

### Evil Mode Integration
- Comprehensive Vim emulation with custom cursor shapes
- TTY cursor support using DECSCUSR escape sequences
- Two-tier keybinding system: basic keys + leader keys (`,` as leader)
- Custom text objects and operators

### Completion System
Modern completion stack:
- **vertico** + **orderless** + **marginalia** + **consult** for minibuffer completion
- **company** for in-buffer completion
- **prescient** for intelligent ranking

## Development Practices

### File Conventions
- All files use `lexical-binding: t`
- Custom functions/variables use `my-` prefix
- Configuration files follow `my-<domain>.el` pattern

### Language Support
- Each language gets its own `my-<lang>.el` file in `config/languages/`
- LSP integration centralized in `my-lsp.el`
- Tree-sitter support via `my-tree-sitter.el`

### System Integration
- Terminal emulator preference: `foot` (configurable via `my-terminal-emulator`)
- Font configuration for graphical vs terminal use
- Clipboard integration with `wl-copy`/`wl-paste` on Linux

## Common Tasks

### Adding New Configuration
1. Create new `my-<name>.el` file in appropriate directory
2. Add `(require 'my-<name>)` to `init.el` in correct load order
3. Use `my-` prefix for all custom functions/variables
4. Use `after` macro for conditional loading of package-dependent code

### Modifying Keybindings
- Basic keybindings go in `my-keys.el`
- Leader key mappings go in `my-leader-keys.el` 
- Use `general.el` for sophisticated keybinding management
- Evil-specific bindings use state-specific definitions

### Language Support
- Add new language file to `config/languages/my-<lang>.el`
- Add require statement to `config/languages/my-languages.el`
- Follow existing patterns for LSP integration and keybindings

## Claude Agent Integration

### Architecture
The configuration includes a sophisticated Claude agent integration that bridges independent packages with the gptel framework:

- **`pkg/claude-agent/`** - Standalone Claude agent package with full API capabilities
  - Complete tool implementations (read_file, list_files, bash, edit_file, grep)
  - Security functions with path restrictions and command safety checks
  - Comprehensive test suite (`pkg/claude-agent/test/`)
  - API integration for agentic conversations with tool calling
  - Works independently of gptel

- **`config/my-gpt.el`** - Integration layer that exposes claude-agent to gptel
  - Bridges claude-agent functionality to gptel framework
  - Functions like `my-gptel-enhanced-*` that call into claude-agent
  - No duplicate implementations - pure glue code
  - Maintains separation of concerns

### Security Model
Claude agent operates with strict security controls:
- **Allowed paths**: `~/src`, `~/dotfiles`, `~/.emacs.d` by default
- **Command safety**: Blocks dangerous commands (`rm -rf`, `sudo`, etc.)
- **Confirmation**: Required for `bash`, `edit`, `delete` operations
- **File size limits**: 1MB maximum for file operations

### Testing
Run tests with:
```bash
# Standalone tests (no API dependencies)
cd pkg/claude-agent/test && emacs --batch --load claude-agent-test-standalone.el

# Full test suite (requires request package)
cd pkg/claude-agent/test && emacs --batch --load ../../../init.el --eval "(straight-use-package 'request)" --load run-tests.el
```

### Integration Pattern
This demonstrates the preferred pattern for extending Emacs functionality:
1. **Independent packages** in `pkg/` with their own tests
2. **Integration layers** in `config/` that expose functionality to frameworks
3. **Clean separation** - no duplicate implementations across layers
