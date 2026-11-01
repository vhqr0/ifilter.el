# ifilter

Filter a list in its buffer incrementally.

Unlike a selection, nothing waits for a result: the buffer persists, and
its filter can be changed at any time.

## ifilter-mode

`ifilter-mode` is the parent of the modes of such buffers.  The first line
of the buffer is the filter field and an Update button, as the search
field of Customize, and each line after it shows a candidate.

- A candidate is `(STRING . DATA)`.  STRING is shown and matched, with its
  newlines shown as `^J`.  The candidates of empty STRING are skipped.
- The filter is a regexp, but spaces match any string, as `.*`.  It is
  converted by `ifilter-regexp-function`, and matches ignoring case as
  `case-fold-search`.
- The candidates not matching the filter are hidden by overlays, so
  filtering never changes the text, and `undo` only undoes the edits of
  the filter.

In the buffer:

- `RET` in the filter field or on the button, and `C-c C-u`, update the
  filter.
- `C-c C-/`, and `/` outside the filter field, read the filter in the
  minibuffer, and apply it.
- `C-c C-s` suspends updating the filter automatically.
- `g` collects the candidates again, keeping the filter.

A tool is a child mode of `ifilter-mode` and a command opening its buffer
by `ifilter-open`, which collects the candidates and creates the buffer
only if there is no such buffer.  The mode:

- binds the commands on the candidates, which get the candidate at point
  by `ifilter-candidate`;
- sets `revert-buffer-function` to collect the candidates again and call
  `ifilter-render`;
- calls `(ifilter-setup-auto-update DELAY)` to update the filter
  automatically: if DELAY is positive, the seconds to wait after the last
  command; if 0 or nil, after each command; if negative, never.

## Tools

- `ifilter-imenu` shows the index of the current buffer.  `RET` jumps to
  an item in another window, and `M-RET` only displays it.
- `ifilter-outline` shows the outline headings of the current buffer, each
  as the path of the lines of it and its ancestors.  `RET` and `M-RET` are
  as in `ifilter-imenu`.
- `ifilter-kill-ring` shows the kill ring.  `RET` makes a kill the latest
  one.

## Configuration

To update the filter 0.2 seconds after the last command, instead of after
each command:

```elisp
(setq ifilter-imenu-delay 0.2
      ifilter-outline-delay 0.2
      ifilter-kill-ring-delay 0.2)
```
