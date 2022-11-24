# What is markmacro?
markmacro is a plugin that apply keyboard macro for marked objects.

Let me show you some examples to expand the concepts of markmacro:

### Case 1
<img src="./images/case1.gif">

```python
fruit = apple watermelon peaches

=>

fruit = ["apple", "watermelon", "peaches"]
```

* select last three words
* `markmacro-mark-words` **mark words** in region and start kmacro record automatically
* Type `"` character, `forward-word`, type `"` character and `,` character
* `markmacro-apply-all` apply kmacro to all **mark words**

### Case 2
<img src="./images/case2.gif">

```python
it-is-ver-long-variable

=>

it_is_ver_long_variable
```

* `markmacro-mark-words` **mark words** in symbol and start kmacro record automatically
* delete `-` and type '_' character
* `markmacro-apply-all-except-first` apply kmacro to all **mark words** except first word

### Case 3
<img src="./images/case3.gif">

```python
flex-auto
flex-col

flex-col-reverse
flex-grow
flex-initial

flex-no-wrap
flex-none
flex-nowrap
flex-row

=>

prefix-flex-auto.txtt
prefix-flex-col.txtt

prefix-flex-col-reverse.txtt
prefix-flex-grow.txtt
prefix-flex-initial.txtt

prefix-flex-no-wrap.txtt
prefix-flex-none.txtt
prefix-flex-nowrap.txtt
prefix-flex-row.txt
```

* Select buffer
* `markmacro-mark-lines` **mark lines** in buffer and start kmacro record automatically
* Move to end of line, type `.txt` and move to beginning of line type `prefix-`
* `markmacro-apply-all` apply kmacro to all **mark lines** 

### Case 4
<img src="./images/case4.gif">

```elisp
("s-/" . markmacro-mark-WORDS)
("s-?" . markmacro-mark-LINES)
("s-<" . markmacro-apply-ALL)
("s->" . markmacro-apply-FIRST)

=>

("s-/" . markmacro-words)
("s-?" . markmacro-lines)
("s-<" . markmacro-all)
("s->" . markmacro-first)
```

* `markmacro-rect-set` record point at first line of rectangle, then move cursor to last line
* `markmacro-rect-mark-symbols` mark **all symbols** in rectangle area
* Move to end of line call `downcase-word`, and delete left word 
* `markmacro-apply-all` apply kmacro to all **mark symbols** 

### Case 5
<img src="./images/case4.gif">

```elisp
("s-/" . markmacro-mark-words)
("s-?" . markmacro-mark-lines)
("s-<" . markmacro-apply-all)
("s->" . markmacro-apply-all-except-first)
("s-M" . markmacro-rect-set)
("s-D" . markmacro-rect-delete)
("s-F" . markmacro-rect-replace)
("s-I" . markmacro-rect-insert)
("s-C" . markmacro-rect-mark-columns)
("s-S" . markmacro-rect-mark-symbols)

=>

(global-set-key (kbd "s-/") 'markmacro-mark-words)
(global-set-key (kbd "s-?") 'markmacro-mark-lines)
(global-set-key (kbd "s-<") 'markmacro-apply-all)
(global-set-key (kbd "s->") 'markmacro-apply-all-except-first)
(global-set-key (kbd "s-M") 'markmacro-rect-set)
(global-set-key (kbd "s-D") 'markmacro-rect-delete)
(global-set-key (kbd "s-F") 'markmacro-rect-replace)
(global-set-key (kbd "s-I") 'markmacro-rect-insert)
(global-set-key (kbd "s-C") 'markmacro-rect-mark-columns)
(global-set-key (kbd "s-S") 'markmacro-rect-mark-symbols)
```

* `markmacro-rect-set` record point at first line of rectangle, then move cursor to last line
* `markmacro-rect-insert` insert `(global-set-key `
* `markmacro-rect-set` record point at first line of rectangle, then move cursor to last line, `forward-char`
* `markmacro-rect-replace` replace `(` with `(kbd `
* `markmacro-rect-set` record point at first line of rectangle, then move cursor to last line
* `markmacro-rect-insert` insert `)`
* `markmacro-rect-set` record point at first line of rectangle, then move cursor to last line
* `markmacro-rect-delete` delete ` . `
* `markmacro-rect-set` record point at first line of rectangle, then move cursor to last line
* `markmacro-rect-mark-columns` mark **all columns** in rectangle area
* Delete right character and type `'`

## Installation

Clone or download this repository (path of the folder is the
`<path-to-markmacro>` used below).

In your `~/.emacs`, add the following two lines:

```Elisp
(add-to-list 'load-path "<path-to-markmacro>") ; add markmacro to your load-path
(require 'markmacro)

(global-set-key (kbd "s-/") 'markmacro-mark-words)
(global-set-key (kbd "s-?") 'markmacro-mark-lines)
(global-set-key (kbd "s-<") 'markmacro-apply-all)
(global-set-key (kbd "s->") 'markmacro-apply-all-except-first)
(global-set-key (kbd "s-M") 'markmacro-rect-set)
(global-set-key (kbd "s-D") 'markmacro-rect-delete)
(global-set-key (kbd "s-F") 'markmacro-rect-replace)
(global-set-key (kbd "s-I") 'markmacro-rect-insert)
(global-set-key (kbd "s-C") 'markmacro-rect-mark-columns)
(global-set-key (kbd "s-S") 'markmacro-rect-mark-symbols)
```

## Contributor

<a href = "https://github.com/manateelazycat/markmacro/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/markmacro"/>
</a>
