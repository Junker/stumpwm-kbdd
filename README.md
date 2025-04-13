# StumpWM KBDD

Per window XKB layout switcher and modeline module for StumpWM.

## Requirements

- [kbdd](https://github.com/qnikst/kbdd) daemon installed
- [DBUS](https://github.com/death/dbus) library

## Installation

```bash
cd ~/.stumpwm.d/modules/
git clone https://github.com/Junker/stumpwm-kbdd kbdd
```

```lisp
(stumpwm:add-to-load-path "~/.stumpwm.d/modules/kbdd")
(load-module "kbdd")
```

## Usage

```lisp
;; Optional. can be defined in Xorg config
(run-shell-command "setxkbmap -layout us,ru")

;; Required. start KBDD daemon
(run-shell-command "/usr/bin/kbdd")

;; init module
(setf kbdd:*locales* '((0 . :US) (1 . :RU)))
(kbdd:kbdd)
(define-key *top-map* (kbd "s-l") "kbdd-switch-layout")
```

### Commands

- kbdd-switch-layout
- kbdd-next-layout
- kbdd-prev-layout

### Parameters

- `kbdd:*layouts*` - Reuqired. List of layouts defined in xorg
  (can see with: "setxkbmap -query | grep layout")

### Modeline

`%L` - keyboard layout formatter

### Modeline mouse interaction

- **left button:** switch layout
- **right button:** switch layout
- **wheel up:** next layout
- **wheel down:** previous layout
