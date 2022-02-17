# StumpWM KBDD

Per window XKB layout switcher and modeline module for StumpWM.

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
(kbdd:get-current-layout) ; :US
(kbdd:get-current-layout-id) ; 0
(kbdd:set-layout-id 0)
(kbdd:set-layout :US)
(kbdd:next-layout)
(kbdd:prev-layout)
```

### Parameters

- kbdd:\*layouts\* - List of layouts defined in xorg
  (can see with: "setxkbmap -query|grep layout")

### Modeline

%L - keyboard layout formatter
