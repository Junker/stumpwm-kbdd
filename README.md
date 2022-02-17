# StumpWM KBDD

Keyboard per window layout switcher and modeline module for StumpWM

## Requirements

- [kbdd](https://github.com/qnikst/kbdd) installed

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
;; Required:
(run-shell-command "/usr/bin/kbdd")
;; Optional. can be defined in Xorg config
(run-shell-command "setxkbmap -layout us,ru")

;; init module
(setf kbdd:*locales* '((0 . :US) (1 . :RU)))
(kbdd:kbdd)
```

### Functions

```lisp
(kbdd:get-current-layout) ; :EN
(kbdd:get-current-layout-id) ; 0
(kbdd:set-layout-id 0)
(kbdd:set-layout :EN)
(kbdd:next-layout)
(kbdd:prev-layout)
```

### Parameters

- kbdd:\*layouts\* - required. set list of layouts defined in xorg

### Modeline

%L - keyboard layout formatter
