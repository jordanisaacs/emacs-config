# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Development Commands

This is a Nix-based Emacs configuration using flakes and literate programming with Org mode.

### Build Commands
- `nix build .#emacs-jd` - Build the complete Emacs configuration
- `nix develop` - Enter development shell with all dependencies
- `nix develop --command emacs` - Run Emacs directly with the configuration

### Package Management Commands
- `nix run .#lock --impure` - Update lockfile when adding new packages
- `nix flake update --flake lock/` - Update all existing packages to latest versions
- `nix flake update --flake lock/ <package-name>` - Update specific package to latest version

### Development Workflow
The configuration is defined in `init.org` using literate programming. Changes to the configuration should be made in `init.org`, not in generated files.

### Package Management with twist.nix
This configuration uses `twist.nix`, a Nix-based Emacs package manager that provides reproducible, declarative package management.

#### How twist.nix Works
- **Package Discovery**: Automatically scans `init.org` for `use-package` declarations
- **Recipe Resolution**: Looks up packages in registries (MELPA, GNU ELPA, NonGNU ELPA)
- **Lock Generation**: Creates `lock/flake.nix` with pinned package versions
- **Nix Integration**: Builds packages as Nix derivations for reproducibility

#### Directory Structure
- `lock/flake.nix` - Auto-generated lockfile with pinned package versions
- `recipes/` - Custom MELPA-style recipes for packages not in standard repositories
- `nix/registries.nix` - Registry configuration (MELPA, GNU ELPA, etc.)
- `nix/inputOverrides.nix` - Custom input overrides for specific packages
- `nix/packageOverrides.nix` - Package-specific build overrides

#### Custom Recipes
When a package isn't available in standard repositories, create a MELPA-style recipe in `recipes/<package-name>`:
```elisp
(:repo "owner/repo-name" :fetcher github)
;; or for other hosts:
;; (:repo "owner/repo-name" :fetcher gitlab)
;; (:repo "owner/repo-name" :fetcher codeberg)
;; (:url "https://example.com/repo.git" :fetcher git)

;; Optional: specify files to include/exclude
;; (:files ("*.el" "dir/*.el"))
;; (:files (:defaults "extensions/*.el"))
```

#### Package Overrides
- `inputOverrides.nix` - Override package sources (e.g., use different git repo)
- `packageOverrides.nix` - Override build process (e.g., add system dependencies)

## Architecture Overview

### Core Structure
This is a literate Emacs configuration that generates `init.el` from `init.org` using org-babel tangling.

### Key Components

#### Nix Integration
- Uses `flake.nix` as the main entry point
- Leverages `twist.nix` for Emacs package management
- Integrates with `emacs-overlay` for bleeding-edge Emacs
- Uses `envrc` for direnv integration

#### Configuration Organization
- `early-init.el` - Early initialization (performance optimizations, security)
- `init.org` - Main configuration in literate format
- `nix/` - Nix-specific configuration files
- `recipes/` - Custom package recipes for packages not in standard repositories

#### Package Management Philosophy
- Uses `use-package` for package configuration
- Packages are installed via Nix, not Emacs package managers
- Heavy use of deferred loading for performance

#### Key Features
- **LSP Integration**: Uses `eglot` (not lsp-mode) for language server support
- **Completion**: `vertico` + `consult` + `marginalia` + `corfu` stack
- **Git Integration**: `magit` with `diff-hl` for git gutters
- **Project Management**: Built-in `project.el` with custom Nix store project finder
- **AI Integration**: `gptel`, `mcp`, and `claude-code` packages

### Performance Optimizations
- Deferred package loading via `use-package`
- Custom hooks for lazy loading (`first-file-hook`, `first-buffer-hook`)
- Nix store path filtering for faster startup
- GC optimization during startup

### Language Support
Configured languages with eglot integration:
- Python (python-ts-mode)
- Nix (nix-ts-mode)
- C/C++ (c-ts-mode, c++-ts-mode)
- Rust (rust-mode + rustic)
- Go (go-ts-mode)
- Zig (zig-ts-mode)
- Shell scripts (sh-mode, bash-ts-mode)

### Development Environment
- Uses `apheleia` for code formatting
- `flymake` for syntax checking
- `jinx` for spell checking
- `dape` for debugging
- `compile-multi` for build management

## Important Notes

### Nix-Specific Considerations
- The configuration runs from a read-only Nix store path
- User data is redirected to `$XDG_STATE_HOME/emacs/`
- Custom project finder optimizes performance in `/nix/store`
- Magit WIP mode has special handling for Nix environments

### Performance Considerations
- Many packages use line-count limits for large files
- Treesitter is used extensively for syntax highlighting and folding
- JIT font lock is configured for better performance on large files

### Security Features
- Stricter TLS/SSL configuration
- Sandboxed package loading via Nix
- Custom security settings for network connections

## Common Development Patterns

When adding new packages:
1. Add to `init.org` with `use-package` declaration
2. If not in standard repositories, add recipe to `recipes/`
3. Run `nix run .#lock --impure` to update the lockfile with new packages
4. Run `nix build .#emacs-jd` to rebuild
5. Use `nix develop` to test changes