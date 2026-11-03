;;; ifilter-tests.el --- Tests for ifilter -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'ifilter)

(defconst ifilter-test-candidates
  '(("foo bar" . 1) ("Foo baz" . 2) ("qux" . 3)))

(defmacro ifilter-test-with-buffer (&rest body)
  "Run BODY in a new `ifilter-mode' buffer showing the test candidates.
The filter is not updated automatically."
  (declare (indent 0))
  `(with-temp-buffer
     (ifilter-mode)
     (ifilter-render ifilter-test-candidates)
     ,@body))

(defun ifilter-test-input (input)
  "Set the filter field to INPUT and update the filter."
  (widget-value-set ifilter--widget input)
  (ifilter-update))

(defun ifilter-test-shown ()
  "Return the candidates shown."
  (save-excursion
    (goto-char (point-min))
    (let (candidates)
      (while (not (eobp))
        (when (and (ifilter-candidate) (not (invisible-p (point))))
          (push (ifilter-candidate) candidates))
        (forward-line 1))
      (nreverse candidates))))

(defun ifilter-test-highlighted ()
  "Return the highlighted strings."
  (mapcar (lambda (overlay)
            (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))
          (sort (seq-filter (lambda (overlay)
                              (eq (overlay-get overlay 'face) 'ifilter-match))
                            (overlays-in (point-min) (point-max)))
                :key #'overlay-start)))

(ert-deftest ifilter-test-render ()
  (ifilter-test-with-buffer
    (should (equal (widget-value ifilter--widget) ""))
    (should (equal (buffer-substring-no-properties
                    (progn (goto-char (point-min)) (forward-line 1) (point))
                    (point-max))
                   "foo bar\nFoo baz\nqux"))
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(1 2 3)))
    (should (null buffer-undo-list))))

(ert-deftest ifilter-test-render-newline ()
  (with-temp-buffer
    (ifilter-mode)
    (ifilter-render '(("a\nb" . 1) ("c" . 2)))
    (should (equal (count-lines (point-min) (point-max)) 3))
    (forward-line 1)
    (should (equal (ifilter-candidate) '("a\nb" . 1)))
    ;; Matched in the line, with the newline replaced.
    (ifilter-test-input "^a.b$")
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(1)))
    (ifilter-test-input "^c$")
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(2)))))

(ert-deftest ifilter-test-render-empty ()
  (with-temp-buffer
    (ifilter-mode)
    (ifilter-render '(("" . 1) ("a" . 2) ("" . 3)))
    (should (equal (count-lines (point-min) (point-max)) 2))
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(2)))))

(ert-deftest ifilter-test-update ()
  (ifilter-test-with-buffer
    (ifilter-test-input "foo")
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(1 2)))
    (ifilter-test-input "o b")
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(1 2)))
    (let ((case-fold-search nil))
      (ifilter-test-input "Foo")
      (should (equal (mapcar #'cdr (ifilter-test-shown)) '(2))))
    (ifilter-test-input "")
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(1 2 3)))))

(ert-deftest ifilter-test-read-filter ()
  (ifilter-test-with-buffer
    (ifilter-test-input "foo")
    ;; On the candidate 2, kept by the filter.
    (goto-char (point-min))
    (forward-line 2)
    (forward-char 2)
    (let ((pos (point)))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt initial) (concat initial " baz"))))
        (call-interactively (keymap-lookup ifilter-mode-map "/")))
      ;; Not shifted, as the field is padded to its width.
      (should (= (point) pos)))
    (should (equal (cdr (ifilter-candidate)) 2))
    (should (eq (keymap-lookup ifilter-field-map "C-c C-/") #'ifilter-read-filter))
    (should (equal (widget-value ifilter--widget) "foo baz"))
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(2)))))

(ert-deftest ifilter-test-hide-last ()
  (ifilter-test-with-buffer
    (ifilter-test-input "foo")
    ;; No empty line is left after the last line shown.
    (goto-char (point-max))
    (should (invisible-p (1- (pos-bol))))
    (ifilter-test-input "")
    (goto-char (point-max))
    (should-not (invisible-p (1- (pos-bol))))))

(ert-deftest ifilter-test-hide-first ()
  (ifilter-test-with-buffer
    (ifilter-test-input "qux")
    ;; The newline before the first line, ending the filter field line.
    (goto-char (point-min))
    (should (invisible-p (pos-eol)))
    (ifilter-test-input "")
    (should-not (invisible-p (pos-eol)))))

(ert-deftest ifilter-test-highlight ()
  (ifilter-test-with-buffer
    (ifilter-test-input "ba")
    (should (equal (ifilter-test-highlighted) '("ba" "ba")))
    (ifilter-test-input "")
    (should (null (ifilter-test-highlighted)))))

(ert-deftest ifilter-test-invalid-regexp ()
  (ifilter-test-with-buffer
    (ifilter-test-input "foo")
    (ifilter-test-input "[")
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(1 2)))))

(ert-deftest ifilter-test-revert ()
  (ifilter-test-with-buffer
    (setq-local revert-buffer-function
                (lambda (&rest _)
                  (ifilter-render (cons '("foo qux" . 4) ifilter-test-candidates))))
    (ifilter-test-input "foo")
    (revert-buffer)
    (should (equal (widget-value ifilter--widget) "foo"))
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(4 1 2)))))

(ert-deftest ifilter-test-undo ()
  (ifilter-test-with-buffer
    (buffer-enable-undo)
    (goto-char (point-min))
    (undo-boundary)
    (insert "qux")
    (ifilter-update)
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(3)))
    (primitive-undo 1 buffer-undo-list)
    (ifilter-update)
    (should (equal (widget-value ifilter--widget) ""))
    (should (equal (mapcar #'cdr (ifilter-test-shown)) '(1 2 3)))))

(ert-deftest ifilter-test-read-only ()
  (ifilter-test-with-buffer
    (goto-char (point-max))
    (should-error (insert "x") :type 'text-read-only)))

(ert-deftest ifilter-test-button ()
  (with-temp-buffer
    (ifilter-imenu-mode)
    (ifilter-render ifilter-test-candidates)
    (goto-char (point-min))
    (widget-forward 1)
    (should (eq (key-binding (kbd "RET")) #'widget-button-press))
    (should (eq (key-binding (kbd "C-c C-u")) #'ifilter-update))))

(defmacro ifilter-test-with-outline-source (dir &rest body)
  "Run BODY in a new buffer of headings with DIR as `default-directory'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ";;; a\n;;; b\n")
     (setq-local outline-regexp ";;;+ ")
     (setq-local outline-level (lambda () (- (match-end 0) (match-beginning 0) 3)))
     (setq default-directory ,dir)
     ,@body))

(ert-deftest ifilter-test-outline-open ()
  (let ((name "*ifilter-outline*"))
    (when (get-buffer name)
      (kill-buffer name))
    (unwind-protect
        (save-window-excursion
          (ifilter-test-with-outline-source "/a/"
            (let ((source (current-buffer)))
              (ifilter-outline)
              (should (equal (buffer-name) name))
              (should (eq ifilter-outline--buffer source))
              (should (equal default-directory "/a/"))
              (should-error (ifilter-outline) :type 'user-error)
              (ifilter-test-input "b")
              ;; From the same buffer, keep the filter.
              (with-current-buffer source
                (ifilter-outline))
              (should (equal (widget-value ifilter--widget) "b"))
              (ifilter-test-with-outline-source "/b/"
                (let ((source (current-buffer))
                      (old (get-buffer name)))
                  ;; From another buffer, show its headings in a new
                  ;; buffer, clearing the filter.
                  (ifilter-outline)
                  (should-not (buffer-live-p old))
                  (should (eq ifilter-outline--buffer source))
                  (should (equal default-directory "/b/"))
                  (should (equal (widget-value ifilter--widget) ""))
                  (should (= (length widget-field-list) 1))
                  (should (equal (mapcar #'car (ifilter-test-shown))
                                 '(";;; a" ";;; b")))
                  (should-not (seq-some (lambda (overlay) (overlay-get overlay 'ifilter))
                                        (overlays-in (point-min) (point-max)))))))))
      (when (get-buffer name)
        (kill-buffer name)))))

(ert-deftest ifilter-test-line ()
  (let ((name "*ifilter-line*"))
    (when (get-buffer name)
      (kill-buffer name))
    (unwind-protect
        (save-window-excursion
          (with-temp-buffer
            (insert "foo\n\nbar\n")
            (let ((source (current-buffer)))
              (ifilter-line)
              ;; Without the empty line.
              (should (equal (ifilter-test-shown) '(("foo" . 1) ("bar" . 6))))
              (ifilter-test-input "bar")
              (goto-char (point-max))
              (ifilter-line-goto)
              (should (eq (current-buffer) source))
              (should (= (point) 6)))))
      (when (get-buffer name)
        (kill-buffer name)))))

(ert-deftest ifilter-test-line-at-point ()
  (let ((name "*ifilter-line*"))
    (when (get-buffer name)
      (kill-buffer name))
    (unwind-protect
        (save-window-excursion
          (with-temp-buffer
            (emacs-lisp-mode)
            (insert "a.b\naxb\n")
            (goto-char (point-min))
            (let ((source (current-buffer)))
              (ifilter-line)
              (ifilter-test-input "x")
              ;; From the same buffer, replace the filter kept.
              (with-current-buffer source
                (ifilter-line-at-point))
              (with-current-buffer name
                (should (equal (widget-value ifilter--widget) "a\\.b"))
                (should (equal (mapcar #'cdr (ifilter-test-shown)) '(1)))))))
      (when (get-buffer name)
        (kill-buffer name)))))

(defun ifilter-test-outline (text)
  "Return the outline candidates of TEXT, as (PATH . POSITION)."
  (with-temp-buffer
    (insert text)
    (setq-local outline-regexp ";;;+ ")
    (setq-local outline-level (lambda () (- (match-end 0) (match-beginning 0) 3)))
    (mapcar (lambda (candidate)
              (cons (substring-no-properties (car candidate))
                    (cdr candidate)))
            (ifilter-outline--candidates (current-buffer)))))

(ert-deftest ifilter-test-outline ()
  (should (equal (ifilter-test-outline ";;; a\n;;;; b\nx\n;;;;;; c\n;;; d\n")
                 '((";;; a" . 1)
                   (";;; a / ;;;; b" . 7)
                   (";;; a / ;;;; b /  / ;;;;;; c" . 16)
                   (";;; d" . 25)))))

(provide 'ifilter-tests)
;;; ifilter-tests.el ends here
