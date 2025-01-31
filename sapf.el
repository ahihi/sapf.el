;;; sapf.el --- Interact with sapf for livecoding music  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl)
(require 'comint)
(require 'subr-x)

(defvar sapf-buffer
  "*sapf*"
  "*The name of the sapf process buffer (default=*sapf*).")

(defvar sapf-interpreter
  "sapf"
  "*The command to run sapf.")

(defvar sapf-interpreter-arguments
  '())

(defvar sapf-mode-map (make-sparse-keymap))

(defvar sapf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) "((" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)

    ;; - and _ are word constituents
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)
    
    ;; both double quotes make strings
    (modify-syntax-entry ?\" "\"" st)

    ;; comments
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)

    st))

(define-derived-mode
  sapf-mode
  prog-mode
  "sapf"
  "Minor mode for interacting with a sapf process."
  ;; (set (make-local-variable 'paragraph-start) "\f\\|[ \t]*$")
  ;; (set (make-local-variable 'paragraph-separate) "[ \t\f]*$")
  (set (make-local-variable 'comment-start) "; ")
  (turn-on-font-lock))

(defun sapf-start ()
  "Start sapf."
  (interactive)
  (if (comint-check-proc sapf-buffer)
      (error "A sapf process is already running")
    (apply
     'make-comint
     "sapf"
     sapf-interpreter
     nil
     sapf-interpreter-arguments)
    (sapf-see-output))
  ;; (sapf-send-string (concat ":script " sapf-boot-script-path))
  )

(defun sapf-stop ()
  "Stop haskell."
  (interactive)
  (let ((process (get-buffer-process sapf-buffer)))
    (if process (kill-process process))))

(defun sapf-see-output ()
  "Show sapf output."
  (interactive)
  (when (comint-check-proc sapf-buffer)
    (with-current-buffer sapf-buffer
      (let ((window (display-buffer (current-buffer))))
        (goto-char (point-max))
        (save-selected-window
          (set-window-point window (point-max)))))))

(defun sapf-send-string (s)
  (if (comint-check-proc sapf-buffer)
      (let ((cs (sapf-chunk-string 64 (concat (string-trim s) "\n")))
            (buf (current-buffer)))
        (set-buffer sapf-buffer)
        ;; (delete-region (point-min) (point-max))
        (set-buffer buf)
        (mapcar (lambda (c) (comint-send-string sapf-buffer c)) cs))
    (error "no sapf process running?")))

(defun sapf-chunk-string (n s)
  "Split a string S into chunks of N characters."
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (sapf-chunk-string n (substring s n))))))

(defun sapf-eval-buffer-interval (a b &optional transform-text)
  (interactive)
  (let* ((l (min a b))
         (r (max a b))
         (s (buffer-substring-no-properties l r))
         (s (if transform-text (funcall transform-text s) s)))
    (sapf-send-string s)
    (nav-flash-show l r)))

(defun sapf-foreach-paragraph (fun)
  (interactive)
  (cl-destructuring-bind (start end)
      (if (use-region-p)
          (list (region-beginning) (region-end))
        (list (buffer-end -1) (buffer-end 1)))
    (deactivate-mark)
    (goto-char start)
    (while (< (point) end)
      (save-mark-and-excursion
        (funcall-interactively fun))
      (mark-paragraph)
      (goto-char (region-end))
      (deactivate-mark)
      )))

(defun sapf-run-paragraph (&optional transform-text)
  "Send the current region to the interpreter as a single line."
  (interactive)
  (save-mark-and-excursion
    (sapf-eval-multiple-lines transform-text)))

(defun sapf-eval-multiple-lines (&optional transform-text)
  "Eval the current region in the interpreter as a single line."
  (mark-paragraph)
  (sapf-eval-buffer-interval (mark) (point) transform-text))

(defun sapf-run-multiple-lines (&optional transform-text)
  (interactive)
  (if (use-region-p)
      (sapf-foreach-paragraph
       (lambda () (sapf-run-paragraph transform-text)))
    (sapf-run-paragraph transform-text)))

(provide 'sapf)
;;; sapf.el ends here
