;;; ifilter.el --- Filter a list in its buffer incrementally -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: matching, convenience

;;; Commentary:

;; A buffer listing candidates under a filter field.  Unlike a
;; selection, nothing waits for a result: the buffer persists, and its
;; filter can be changed at any time.
;;
;; The first line of the buffer is the filter field and an Update
;; button, and each line after it shows a candidate.  The candidates not
;; matching the filter are hidden by overlays, so filtering never
;; changes the text, and `undo' only undoes the edits of the filter.
;;
;; In the buffer:
;;
;; - `RET' in the filter field or on the button, and `C-c C-u', update
;;   the filter.
;; - `C-c C-s' suspends updating the filter automatically.
;; - `g' collects the candidates again, keeping the filter.
;;
;; A tool is a child mode of `ifilter-mode' and a command opening its
;; buffer by `ifilter-open'.  The mode binds the commands on the
;; candidates, which get the candidate at point by `ifilter-candidate',
;; sets `revert-buffer-function' to collect the candidates again and call
;; `ifilter-render', and calls `ifilter-setup-auto-update'.
;;
;; The tools:
;;
;; - `ifilter-imenu' shows the index of the current buffer.  `RET'
;;   jumps to an item in another window, and `M-RET' only displays it.
;; - `ifilter-outline' shows the outline headings of the current buffer,
;;   each as the path of the lines of it and its ancestors.  `RET' and
;;   `M-RET' are as in `ifilter-imenu'.
;; - `ifilter-kill-ring' shows the kill ring.  `RET' makes a kill the
;;   latest one.
;;
;; To update the filter 0.2 seconds after the last command:
;;
;;   (setq ifilter-imenu-delay 0.2
;;         ifilter-outline-delay 0.2
;;         ifilter-kill-ring-delay 0.2)

;;; Code:

(require 'wid-edit)

(defgroup ifilter nil
  "Filter a list in its buffer incrementally."
  :group 'convenience)

;;; core

;;;; render

(defcustom ifilter-field-width 40
  "Minimum width of the filter field."
  :type 'natnum)

(defvar-local ifilter--widget nil
  "Filter field of the current buffer.")

(defvar-local ifilter--last-input nil
  "Input of the applied filter, nil if none applied.")

(defconst ifilter--newline
  (propertize " " 'display (propertize "^J" 'face 'escape-glyph))
  "String replacing the newlines of the candidates shown.")

;; The field has its keymap as the property `local-map', replacing the
;; keymap of the major mode.
(defvar-keymap ifilter-field-map
  :parent widget-field-keymap
  "C-c C-u" #'ifilter-update
  "C-c C-s" #'ifilter-toggle-suspend)

(defun ifilter-render (candidates)
  "Show CANDIDATES in the current buffer under the filter field.
A candidate is (STRING . DATA).  Each line shows STRING, with its
newlines shown as \"^J\", and has the candidate as the property
`ifilter-candidate'.  The candidates of empty STRING are skipped.
Keep the input of the filter, apply it, and clear the undo history."
  (let ((input (if ifilter--widget (widget-value ifilter--widget) ""))
        (inhibit-read-only t)
        (buffer-undo-list t))
    (remove-overlays (point-min) (point-max) 'ifilter t)
    (when ifilter--widget
      (widget-delete ifilter--widget))
    (erase-buffer)
    ;; As the search field of Customize.
    (setq ifilter--widget
          (widget-create 'editable-field
                         :size ifilter-field-width
                         :keymap ifilter-field-map
                         :action (lambda (&rest _) (ifilter-update))
                         input))
    (widget-insert " ")
    (widget-create-child-and-convert
     ifilter--widget 'push-button
     :tag " Update "
     ;; Pressed by `RET', even if bound in the mode.
     :keymap widget-keymap
     :action (lambda (&rest _) (ifilter-update)))
    (widget-setup)
    ;; Insert the newline before each line, so the last line has none.
    (dolist (candidate candidates)
      ;; An empty line could not have the property.
      (unless (string-empty-p (car candidate))
        (insert "\n" (propertize (string-replace "\n" ifilter--newline (car candidate))
                                 'ifilter-candidate candidate)))))
  (setq buffer-undo-list nil
        ifilter--last-input nil)
  (ifilter-update))

(defun ifilter-candidate ()
  "Return the candidate at point, nil if none."
  (get-text-property (pos-bol) 'ifilter-candidate))

(defun ifilter--candidate-or-error ()
  "Return the candidate at point, or signal `user-error' if none."
  (or (ifilter-candidate)
      (user-error "No candidate")))

;;;; update

(defface ifilter-match
  '((t :inherit match))
  "Face of the parts of candidates matching the filter.")

(defun ifilter-regexp-default (input)
  "Return the regexp of INPUT.
INPUT is a regexp, but spaces match any string."
  (string-replace " " ".*" input))

(defcustom ifilter-regexp-function #'ifilter-regexp-default
  "Function converting the input to the regexp matching the candidates."
  :type 'function)

(defun ifilter--make-overlay (beg end prop value)
  "Make an overlay from BEG to END with PROP as VALUE.
It has the property `ifilter', to be deleted by updating the filter."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'ifilter t)
    (overlay-put overlay prop value)))

(defun ifilter-update ()
  "Apply the filter, if changed.
Match it in the text of each line, hide the lines not matching it and
highlight the matches, by overlays made again.  Move point to the
beginning of the buffer, unless in the filter field.
Unlike updating automatically, it is not suspended."
  (interactive)
  (let ((input (widget-value ifilter--widget)))
    (unless (equal input ifilter--last-input)
      ;; Hooks and timers run with `inhibit-quit', and a `quit' signaled
      ;; in `post-command-hook' would remove the update from it.
      (condition-case err
          (let ((regexp (funcall ifilter-regexp-function input))
                (inhibit-quit nil))
            ;; Signal an invalid regexp before changing any line.
            (string-match-p regexp "")
            (remove-overlays (point-min) (point-max) 'ifilter t)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (ifilter-candidate)
                  (if (re-search-forward regexp (pos-eol) t)
                      (when (< (match-beginning 0) (match-end 0))
                        (ifilter--make-overlay (match-beginning 0) (match-end 0)
                                               'face 'ifilter-match))
                    ;; With the newline before the line, so the line
                    ;; numbers of the lines shown are their own.
                    (ifilter--make-overlay (1- (pos-bol)) (pos-eol) 'invisible 'ifilter)))
                (forward-line 1)))
            (setq ifilter--last-input input)
            (unless (widget-field-at (point))
              (goto-char (point-min))))
        ;; The input is often an invalid regexp while typing it.
        (invalid-regexp
         (message "Invalid regexp: %s" (cadr err)))
        (quit
         (message "Quit"))))))

(defvar-local ifilter--suspend-p nil
  "Non-nil if updating the filter automatically is suspended.")

(defvar-local ifilter--timer nil
  "Timer updating the filter automatically after the last command.")

(defun ifilter--auto-update (buffer)
  "Apply the filter of BUFFER, unless suspended or BUFFER is killed.
Run by the hook or timer of `ifilter-setup-auto-update'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (or ifilter--suspend-p (null ifilter--widget))
        (ifilter-update)))))

(defun ifilter-toggle-suspend ()
  "Toggle suspending updating the filter automatically.
Resuming updates it at once."
  (interactive)
  (setq ifilter--suspend-p (not ifilter--suspend-p))
  (unless ifilter--suspend-p
    (ifilter-update))
  (message (if ifilter--suspend-p "Suspended" "Resumed")))

(defun ifilter--cancel-timer ()
  "Cancel the timer of the current buffer, if any."
  (when ifilter--timer
    (cancel-timer ifilter--timer)
    (setq ifilter--timer nil)))

(defun ifilter-setup-auto-update (delay)
  "Update the filter of the current buffer automatically, by DELAY.
If DELAY is positive, update DELAY seconds after the last command, by a
timer replacing the one of the command before.  If 0 or nil, update
after each command.  If negative, do nothing.
Call it in the body of a child mode of `ifilter-mode'."
  (let ((delay (or delay 0)))
    (unless (minusp delay)
      (add-hook 'post-command-hook
                (lambda ()
                  (if (plusp delay)
                      (progn
                        (ifilter--cancel-timer)
                        (setq ifilter--timer
                              (run-with-timer delay nil #'ifilter--auto-update
                                              (current-buffer))))
                    (ifilter--auto-update (current-buffer))))
                nil t)
      (add-hook 'kill-buffer-hook #'ifilter--cancel-timer nil t)
      (add-hook 'change-major-mode-hook #'ifilter--cancel-timer nil t))))

;;;; mode

(defvar-keymap ifilter-mode-map
  ;; `widget-keymap' presses the buttons, and moves between the widgets.
  :parent (make-composed-keymap widget-keymap special-mode-map)
  "C-c C-u" #'ifilter-update
  "C-c C-s" #'ifilter-toggle-suspend)

(define-derived-mode ifilter-mode special-mode "IFilter"
  "Parent mode of the buffers filtering a list.
Show the candidates by `ifilter-render'.  The filter is updated
automatically only if set up by `ifilter-setup-auto-update'."
  ;; The filter field is editable, and the text outside it is protected
  ;; by `widget-before-change'.
  (setq buffer-read-only nil)
  (setq truncate-lines t)
  (add-to-invisibility-spec 'ifilter))

(defun ifilter-open (name mode collect &optional setup)
  "Switch to the buffer NAME in another window.
If there is no such buffer, get the candidates by calling COLLECT, then
create it in MODE, call SETUP in it if non-nil, and show the candidates.
Signal `user-error' in a buffer of `ifilter-mode'."
  (when (derived-mode-p 'ifilter-mode)
    (user-error "Already in an ifilter buffer"))
  (let ((buffer (get-buffer name)))
    (unless buffer
      (let ((candidates (funcall collect)))
        (setq buffer (generate-new-buffer name))
        (with-current-buffer buffer
          (funcall mode)
          (when setup
            (funcall setup))
          (ifilter-render candidates))))
    (switch-to-buffer-other-window buffer)))

;;; imenu

(defvar imenu-flatten)
(defvar imenu--rescan-item)
(declare-function imenu "imenu")
(declare-function imenu-flush-cache "imenu")
(declare-function imenu--make-index-alist "imenu")
(declare-function imenu--flatten-index-alist "imenu")

(defcustom ifilter-imenu-delay nil
  "Delay of updating the filter automatically in `ifilter-imenu'.
See `ifilter-setup-auto-update' for the values."
  :type '(choice (const nil) number))

(defvar-local ifilter-imenu--buffer nil
  "Buffer of the index shown in the current buffer.")

(defun ifilter-imenu--candidates (buffer)
  "Return the index items of BUFFER as candidates, rescanning it."
  (with-current-buffer buffer
    (imenu-flush-cache)
    ;; Flatten the nested items by concatenating the names, as by
    ;; `imenu-flatten' nil.
    (let ((imenu-flatten nil))
      (mapcan (lambda (item)
                (when (stringp (car item))
                  (list (cons (car item) item))))
              (imenu--flatten-index-alist
               (remove imenu--rescan-item (imenu--make-index-alist t))
               t)))))

(defun ifilter-imenu--revert (&rest _)
  "Show the index of `ifilter-imenu--buffer' again, rescanning it."
  (unless (buffer-live-p ifilter-imenu--buffer)
    (user-error "Buffer of the index killed"))
  (ifilter-render (ifilter-imenu--candidates ifilter-imenu--buffer)))

(defun ifilter-imenu-goto (&optional display)
  "Jump to the index item at point in another window.
If DISPLAY is non-nil, only display it, keeping the selected window."
  (interactive)
  (let ((item (cdr (ifilter--candidate-or-error))))
    (unless (buffer-live-p ifilter-imenu--buffer)
      (user-error "Buffer of the index killed"))
    (with-selected-window (if display
                              (display-buffer ifilter-imenu--buffer t)
                            (switch-to-buffer-other-window ifilter-imenu--buffer)
                            (selected-window))
      (imenu item))))

(defun ifilter-imenu-display ()
  "Display the index item at point in another window."
  (interactive)
  (ifilter-imenu-goto t))

(defvar-keymap ifilter-imenu-mode-map
  "RET" #'ifilter-imenu-goto
  "M-RET" #'ifilter-imenu-display)

(define-derived-mode ifilter-imenu-mode ifilter-mode "IFilter-Imenu"
  "Mode of the buffers filtering the index of a buffer."
  (setq-local revert-buffer-function #'ifilter-imenu--revert)
  (ifilter-setup-auto-update ifilter-imenu-delay))

;;;###autoload
(defun ifilter-imenu ()
  "Show the index of the current buffer, as by `imenu', to filter."
  (interactive)
  (require 'imenu)
  (let ((buffer (current-buffer)))
    (ifilter-open (format "*ifilter-imenu: %s*" (buffer-name))
                  #'ifilter-imenu-mode
                  (lambda () (ifilter-imenu--candidates buffer))
                  (lambda () (setq ifilter-imenu--buffer buffer)))))

;;; outline

(defcustom ifilter-outline-delay nil
  "Delay of updating the filter automatically in `ifilter-outline'.
See `ifilter-setup-auto-update' for the values."
  :type '(choice (const nil) number))

(defvar-local ifilter-outline--buffer nil
  "Buffer of the headings shown in the current buffer.")

(defun ifilter-outline--line (beg)
  "Return the line from BEG to its end, keeping only its faces."
  (let* ((end (save-excursion (goto-char beg) (pos-eol)))
         (line (buffer-substring-no-properties beg end))
         (pos beg))
    (font-lock-ensure beg end)
    (while (< pos end)
      (let ((next (next-property-change pos nil end))
            (face (or (get-text-property pos 'face)
                      (get-text-property pos 'font-lock-face))))
        (when face
          (put-text-property (- pos beg) (- next beg) 'face face line))
        (setq pos next)))
    line))

(defun ifilter-outline--candidates (buffer)
  "Return the headings of BUFFER as candidates.
A heading is shown as its path, the lines of it and its ancestors joined
by \" / \", and has its marker as data."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((regexp (concat "^\\(?:" outline-regexp "\\)"))
            stack candidates)
        (while (re-search-forward regexp nil t)
          (let ((beg (match-beginning 0))
                (end (match-end 0))
                level)
            ;; `outline-level' may use the match data, as of Markdown.
            (goto-char beg)
            (setq level (max (or (funcall outline-level) 1) 1))
            (while (>= (length stack) level)
              (pop stack))
            ;; Fill the skipped levels.
            (while (< (length stack) (1- level))
              (push "" stack))
            (push (ifilter-outline--line beg) stack)
            (push (cons (string-join (reverse stack) " / ") (copy-marker beg))
                  candidates)
            (goto-char end)
            (when (= end beg)
              (forward-line 1))))
        (nreverse candidates)))))

(defun ifilter-outline--revert (&rest _)
  "Show the headings of `ifilter-outline--buffer' again."
  (unless (buffer-live-p ifilter-outline--buffer)
    (user-error "Buffer of the headings killed"))
  (ifilter-render (ifilter-outline--candidates ifilter-outline--buffer)))

(defun ifilter-outline-goto (&optional display)
  "Jump to the heading at point in another window.
If DISPLAY is non-nil, only display it, keeping the selected window."
  (interactive)
  (let ((marker (cdr (ifilter--candidate-or-error))))
    (unless (buffer-live-p ifilter-outline--buffer)
      (user-error "Buffer of the headings killed"))
    (with-selected-window (if display
                              (display-buffer ifilter-outline--buffer t)
                            (switch-to-buffer-other-window ifilter-outline--buffer)
                            (selected-window))
      (goto-char marker))))

(defun ifilter-outline-display ()
  "Display the heading at point in another window."
  (interactive)
  (ifilter-outline-goto t))

(defvar-keymap ifilter-outline-mode-map
  "RET" #'ifilter-outline-goto
  "M-RET" #'ifilter-outline-display)

(define-derived-mode ifilter-outline-mode ifilter-mode "IFilter-Outline"
  "Mode of the buffers filtering the outline headings of a buffer."
  (setq-local revert-buffer-function #'ifilter-outline--revert)
  (ifilter-setup-auto-update ifilter-outline-delay))

;;;###autoload
(defun ifilter-outline ()
  "Show the outline headings of the current buffer to filter."
  (interactive)
  (let ((buffer (current-buffer)))
    (ifilter-open (format "*ifilter-outline: %s*" (buffer-name))
                  #'ifilter-outline-mode
                  (lambda () (ifilter-outline--candidates buffer))
                  (lambda () (setq ifilter-outline--buffer buffer)))))

;;; kill ring

(defcustom ifilter-kill-ring-width 200
  "Maximum number of characters shown of a kill."
  :type 'natnum)

(defcustom ifilter-kill-ring-delay nil
  "Delay of updating the filter automatically in `ifilter-kill-ring'.
See `ifilter-setup-auto-update' for the values."
  :type '(choice (const nil) number))

(defun ifilter-kill-ring--candidates ()
  "Return the kills as candidates."
  (mapcar (lambda (kill)
            (cons (substring-no-properties kill 0 (min (length kill) ifilter-kill-ring-width))
                  kill))
          kill-ring))

(defun ifilter-kill-ring--revert (&rest _)
  "Show the kill ring again."
  (ifilter-render (ifilter-kill-ring--candidates)))

(defun ifilter-kill-ring-select ()
  "Make the kill at point the latest kill, by `kill-new'."
  (interactive)
  (kill-new (cdr (ifilter--candidate-or-error))))

(defvar-keymap ifilter-kill-ring-mode-map
  "RET" #'ifilter-kill-ring-select)

(define-derived-mode ifilter-kill-ring-mode ifilter-mode "IFilter-Kill-Ring"
  "Mode of the buffer filtering the kill ring."
  (setq-local revert-buffer-function #'ifilter-kill-ring--revert)
  (ifilter-setup-auto-update ifilter-kill-ring-delay))

;;;###autoload
(defun ifilter-kill-ring ()
  "Show the kill ring to filter."
  (interactive)
  (ifilter-open "*ifilter-kill-ring*" #'ifilter-kill-ring-mode
                #'ifilter-kill-ring--candidates))

(provide 'ifilter)
;;; ifilter.el ends here
