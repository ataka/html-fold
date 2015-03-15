# html-fold.el

html-fold provides support for hiding and unhiding HTML/XML elements.

html-fold is a minor mode, so that you can use it with some HTML/XML editing major mode,
ex. YaHTML, nxml-mode and psgml-mode.

## Install html-fold.el

1. Put html-fold.el into your load-path
2. Byte-compile it, if you would like
3. Put following code into your .emacs

```elisp
(autoload 'html-fold-mode "html-fold" "Minor mode for hiding and revealing elements." t)
```

## Usage

* Initialize buffer with `M-x html-fold-buffer`

### Folding/Unfolding commands list

* `C-c C-o C-b` / `C-c C-o b`
  folding/unfolding buffer
* `C-c C-o C-r` / `C-c C-o r`
  folding/unfolding region
* `C-c C-o C-p` / `C-c C-o p`
  folding/unfolding paragraph
* `C-c C-o C-o`
  folding and unfolding case by case
* `C-c C-o C-e`
  folding block elements
* `C-c C-o C-m`
  folding inline elements

### Tips

You can skip `M-x html-fold-buffer` with `add-hook`.

This is an example for add-hook for html-mode.

```elisp
(add-hook 'html-mode-hook 'html-fold-mode)
```

## Acknowledgements

html-fold is depeloped based on `tex-fold.el` in AUCTeX, written by
[Ralf Angeli](mailto:angeli@iwi.uni-sb.de).
