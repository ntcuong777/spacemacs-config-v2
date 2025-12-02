# TTY Configuration Layer

This layer provides terminal (TTY) specific configuration for Spacemacs.

## Features

- **Mouse Support**: Enable xterm mouse mode for terminal mouse interaction
- **Region Selection**: Full cursor-based region selection support
  - Mouse click and drag to select regions
  - Shift+Arrow keys to select text
  - Works in both Emacs and Evil modes
- **Clipboard Integration**: Terminal clipboard support via clipetty
- **Cursor Shape**: Evil mode cursor shape changes in terminal
- **Kitty Keyboard Protocol**: Support for Kitty terminal keyboard protocol
- **Zellij Support**: Key remapping for Zellij terminal multiplexer
- **Window Title**: Dynamic window title updates in terminal

## Region Selection

This layer enables comprehensive region selection in terminal Emacs:

### Mouse Selection

- **Left Click**: Set cursor position
- **Left Click + Drag**: Select region
- **Right Click**: Extend/kill selection
- **Mouse Wheel**: Scroll up/down

### Keyboard Selection

- **Shift + Arrow Keys**: Select text character by character or line by line
- **Visual Mode**: Press `v` in Evil normal mode to enter visual selection mode
- Works in both Emacs and Evil insert modes

## Packages

- `clipetty` - Terminal clipboard support
- `evil-terminal-cursor-changer` - Cursor shape changes for Evil mode in terminal
- `kkp` - Kitty keyboard protocol support

## Configuration

All configuration is automatically applied when running Emacs in a terminal (non-GUI mode).

## Usage

This layer is automatically loaded when Spacemacs detects it's running in a terminal.

To manually enable/disable features, you can modify the configuration in `config.el`.
