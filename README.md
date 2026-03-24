# Modular Emacs Configuration

A high-performance Emacs 29+ configuration designed for speed, mnemonic efficiency, and modern language intelligence. This setup is cross-platform and optimized for both macOS and Linux environments.

## Core Features

- **Modal Editing:** Full Evil-mode integration with a Spacebar leader key.
- **AI Integration:** Integrated agent-shell for a native Emacs interface to LLM agents (Gemini, Claude) using the Agent Client Protocol.
- **Language Intelligence:** Native LSP support via Eglot for C++ (clangd) and Python (pyright).
- **Git Integration:** Magit interface with diff-hl fringe indicators for real-time status tracking.
- **Modern Interface:** High-contrast Kaolin themes and Smart-mode-line for a polished, functional UI.

## Configuration Structure

The configuration is organized into logical modules within the `config/` directory to ensure maintainability and deferred loading:

- `ui.el`: Visuals, fonts (Source Code Pro), and theme settings.
- `editor.el`: Global behavior, performance optimizations, and system-specific key handling.
- `packages.el`: Core infrastructure including Evil, Ivy, Projectile, and Undo-tree.
- `languages.el`: Consolidated support for Elisp, C++, Python, Web, and LaTeX.

## Essential Mnemonic Keybindings

The leader key is set to **SPC**. Commands are grouped by logical mnemonics for rapid access:

| Key | Action | Mnemonic |
| :--- | :--- | :--- |
| SPC w s | Split Window Below | Window Split |
| SPC w v | Split Window Right | Window Vertical |
| SPC w d | Delete Window | Window Delete |
| SPC w m | Maximize Window | Window Maximize |
| SPC p f | Find File in Project | Project File |
| SPC a | Open AI Agent Shell | Agent |
| SPC m | Magit Status | Magit |

## Installation

### 1. Install Emacs 29+

#### macOS
```bash
brew tap d12frosted/emacs-plus
brew install emacs-plus@29 --with-native-comp
```

#### Linux (Debian/Ubuntu)
```bash
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs29
```

### 2. Install Language Servers

For full IDE functionality, ensure the following are installed and available in your PATH:
- **C++:** `clangd` (typically provided by the `llvm` package)
- **Python:** `pyright`

### 3. Final Setup
1. Clone this repository to `~/.emacs.d`.
2. Launch Emacs. The `use-package` system will automatically download and configure all dependencies on the first startup.
