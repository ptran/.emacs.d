# Modular Emacs Configuration

A high-performance Emacs 29+ configuration designed for speed, mnemonic efficiency, and modern language intelligence. This setup is cross-platform and optimized for both macOS and Linux environments.

## Core Features

- **Modal Editing:** Full Evil-mode integration with a Spacebar leader key and `undo-tree` visualization.
- **AI Integration:** 
  - **gptel:** Native LLM interaction (Gemini) for chatting and on-the-fly code rewriting.
  - **agent-shell:** Interface for Agent Client Protocol (ACP) compliant LLM agents.
- **Language Intelligence:** Native LSP support via Eglot for C++, Python, and more.
- **Expanded Support:** Optimized environments for CUDA, CMake, Web development (HTML/CSS/JS), and LaTeX.
- **Git Integration:** Magit interface with diff-hl fringe indicators for real-time status tracking.
- **Modern Interface:** High-contrast Kaolin themes, Smart-mode-line, and contextual parenthesis highlighting.

## Configuration Structure

The configuration is organized into logical modules within the `config/` directory:

- `ui.el`: Visuals, font management (JetBrains Mono/Source Code Pro), and theme settings.
- `editor.el`: Global behavior, performance optimizations, and system-specific key handling.
- `packages.el`: Core infrastructure including Evil, Ivy/Counsel, Projectile, and AI packages.
- `languages.el`: Consolidated support for Elisp, C/C++, CUDA, Python, Web, CMake, and LaTeX.

## Essential Mnemonic Keybindings

The leader key is set to **SPC**. Commands are grouped by logical mnemonics:

### Navigation & Buffers
| Key | Action | Mnemonic |
| :--- | :--- | :--- |
| SPC b | Switch Buffer | Buffer |
| SPC k | Kill Buffer | Kill |
| SPC s | Swiper Search | Swiper |
| SPC w [h/j/k/l] | Navigate Windows | Window Left/Down/Up/Right |
| SPC w s / v | Split Window (Horiz/Vert) | Window Split/Vertical |
| SPC w m | Maximize Window | Window Maximize |

### Project & Search
| Key | Action | Mnemonic |
| :--- | :--- | :--- |
| SPC p p | Switch Project | Project |
| SPC p f | Find File in Project | Project File |
| SPC p b | Switch Project Buffer | Project Buffer |
| SPC p s g | Project Grep | Project Search Grep |
| SPC x | M-x (Command Palette) | execute |

### AI & Tools
| Key | Action | Mnemonic |
| :--- | :--- | :--- |
| SPC g | gptel Menu (Gemini) | gptel |
| SPC a | Open AI Agent Shell | Agent |
| SPC m | Magit Status | Magit |
| SPC : | Eval Expression | : (Lisp) |

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

### 2. Install Language Servers & Tools

For full functionality, ensure the following are in your PATH:
- **LSP:** `clangd` (C++), `pyright` (Python)
- **Optional:** `ipython` & `jupyter` (Python/Notebooks), `texlive` (LaTeX)

### 3. Environment Variables
To enable AI features, set your API keys in your shell environment (e.g., `~/.zshrc`):
```bash
export GEMINI_API_KEY="your-api-key-here"
```

### 4. Final Setup
1. Clone this repository to `~/.emacs.d`.
2. Launch Emacs. The `use-package` system will automatically download all dependencies.
