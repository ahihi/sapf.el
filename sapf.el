;;; sapf.el --- Interact with sapf for livecoding music  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl)
(require 'comint)
(require 'pulse)
(require 'subr-x)

(defvar sapf-buffer
  "*sapf*"
  "*The name of the sapf process buffer (default=*sapf*).")

(defvar sapf-interpreter
  "sapf"
  "*The command to run sapf (default=sapf).")

(defvar sapf-interpreter-arguments
  nil
  "*Command-line arguments to be passed to the sapf interpreter (default=nil).")

(defvar sapf-terminal
  'auto
  "*Which type of terminal to run the sapf interpreter in: vterm, comint, or auto (default=auto).")

(defvar sapf-highlight
  #'pulse-momentary-highlight-region
  "*Function to momentarily highlight code being evaluated (default=pulse-momentary-highlight-region). Takes two arguments specifying the endpoints of the region containing the code.")

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
  (if (sapf--running-p)
      (error "A sapf process is already running")
    (sapf--start-process)
    (sapf-see-output))
  ;; (sapf-send-string (concat ":script " sapf-boot-script-path))
  )

(defun sapf-stop ()
  "Stop sapf."
  (interactive)
  (sapf--stop-process))

(defun sapf-see-output ()
  "Show sapf output."
  (interactive)
  (when (sapf--running-p)
    (with-current-buffer sapf-buffer
      (let ((window (display-buffer (current-buffer))))
        (goto-char (point-max))
        (save-selected-window
          (set-window-point window (point-max)))))))

(defun sapf-send-string (s)
  (if (sapf--running-p)
      (let ((cs (sapf-chunk-string 64 (concat (string-trim s) "\n"))))
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
    (if sapf-highlight
        (funcall sapf-highlight l r))))

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

(defun sapf--terminal ()
  (let ((use-vterm (or (eq sapf-terminal 'vterm)
                       (and (eq sapf-terminal 'auto)
                            (fboundp 'vterm)))))
    (if use-vterm 'vterm 'comint)))

(defun sapf--comint-running-p ()
  (comint-check-proc sapf-buffer))

(defun sapf--vterm-running-p ()
  (vterm-check-proc sapf-buffer))

(defun sapf--running-p ()
  (case (sapf--terminal)
    (vterm (sapf--vterm-running-p))
    (t (sapf--comint-running-p))))

(defun sapf--comint-start-process ()
  (apply
   'make-comint
   "sapf"
   sapf-interpreter
   nil
   sapf-interpreter-arguments))

(defun sapf--vterm-start-process ()
  (let* ((vterm-buffer-name sapf-buffer)
         (vterm-kill-buffer-on-exit nil)
         (vterm-shell (concat sapf-interpreter
                              " "
                              (mapconcat 'shell-quote-argument sapf-interpreter-arguments " ")))
         (existing-buf (get-buffer sapf-buffer))
         (existing-buf-wins (and existing-buf
                                 (get-buffer-window-list existing-buf nil t))))
    (when existing-buf
      ;; rename existing buffer
      (with-current-buffer existing-buf
        (rename-buffer (generate-new-buffer-name "*sapf-previous*"))))
    (vterm--internal (lambda (buf &rest rest)
                       (if existing-buf-wins
                           ;; switch all the buffers showing the existing buffer to the new one
                           (dolist (win (get-buffer-window-list existing-buf nil t))
                             (set-window-buffer win buf))
                         (apply 'pop-to-buffer buf rest))))
    (when existing-buf
      ;; kill existing buffer
      (kill-buffer existing-buf))))

(defun sapf--start-process ()
  (case (sapf--terminal)
    (vterm (sapf--vterm-start-process))
    (t (sapf--comint-start-process))))

(defun sapf--stop-process ()
  (let ((process (get-buffer-process sapf-buffer)))
    (when process
      (when (eq (sapf--terminal) 'vterm)
        ;; erase the buffer - this is the easiest way to ensure that the "process killed" message becomes visible despite vterm shenanigans
        (with-current-buffer sapf-buffer
          (let ((inhibit-read-only t))
            (erase-buffer))))
      (kill-process process))))

;; remove incorrect smartparens pairs
(with-eval-after-load 'smartparens
  (sp-local-pair 'sapf-mode "`" nil :actions nil)
  (sp-local-pair 'sapf-mode "'" nil :actions nil))

(provide 'sapf)
;;; sapf.el ends here
