;;; vmd.el --- vmd interaction mode                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Omar Polo

;; Author: Omar Polo <op@omarpolo.com>
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((transient "0.3.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; vmd.el is an interface to list and manage OpenBSD' vmd virtual
;; machines.
;;
;; To start, run `M-x vmd RET' to bring up a *vmd* buffer with the list
;; of virtual machines.  Then press `x' to bring up a transient with
;; the action aviable.

;;; Code:

(require 'term)
(require 'transient)

(defgroup vmd nil
  "Vmd."
  :group 'vmd)

(defcustom vmd-vmctl-cmd "vmctl"
  "Path to the vmctl command to use."
  :type 'string)

(defcustom vmd-console-function #'vmd-run-in-term
  "Function to use for the `vmd-console' command.
It takes two arguments, the string name and the cmd arguments
list, and should pop a buffer with a terminal running the
commands in cmd."
  :type 'function)

(defun vmd-run-in-term (name cmd)
  "Run CMD inside a term buffer called NAME."
  (pop-to-buffer (apply #'term-ansi-make-term
                        (concat "*" name "*")
                        (car cmd) nil (cdr cmd))))

(defun vmd--update-table ()
  "Update the `tabulated-list-mode' with the list of virtual machines."
  (let ((columns [("ID"     3  t)
                  ("PID"    5  nil)
                  ("VCPUS"  5  nil)
                  ("MAXMEM" 6  nil)
                  ("CURMEM" 6  nil)
                  ("TTY"    10 t)
                  ("OWNER"  8  t)
                  ("STATE"  8  t)
                  ("NAME"   30 t)])
        (rows (mapcar (lambda (x) `(,(car x) ,(apply #'vector x)))
                      (mapcar (lambda (x)
                                (split-string x nil t))
                              (cdr
                               (split-string (shell-command-to-string
                                              (concat vmd-vmctl-cmd " status"))
                                             "\n" t))))))
    (setq tabulated-list-format columns
          tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun vmd--vm-at-point ()
  "Return the vm name at point."
  (unless (derived-mode-p 'vmd-mode)
    (error "Not in vmd-mode"))
  (let* ((row (tabulated-list-get-entry))
         (len (length row)))
    (when (or (null row)
              (= len 0))
      (error "No vm at point"))
    (aref row (1- len))))

(defun vmd--vmctl (&rest args)
  "Run a vmctl command with ARGS."
  (shell-command (mapconcat #'shell-quote-argument (cons vmd-vmctl-cmd args) " ")))

(defun vmd-console (vm)
  "Open a console for the VM at point."
  (interactive (list (vmd--vm-at-point)) vmd-mode)
  (funcall vmd-console-function
           (concat "vmd console " vm)
           (mapcar #'shell-quote-argument
                   (list vmd-vmctl-cmd "console" vm))))

(defun vmd-pause (vm)
  "Pause the VM at point."
  (interactive (list (vmd--vm-at-point)) vmd-mode)
  (vmd--vmctl "pause" vm)
  (vmd--update-table))

(defun vmd-start (vm)
  "Start the VM at point."
  (interactive (list (vmd--vm-at-point)) vmd-mode)
  (vmd--vmctl "start" vm)
  (vmd--update-table))

(defun vmd-stop (vm)
  "Stop the VM at point."
  (interactive (list (vmd--vm-at-point)) vmd-mode)
  (vmd--vmctl "stop" vm)
  (vmd--update-table))

(defun vmd-unpause (vm)
  "Unpause the VM at point."
  (interactive (list (vmd--vm-at-point)) vmd-mode)
  (vmd--vmctl "unpause" vm)
  (vmd--update-table))

(transient-define-prefix vmd--transient ()
  "Vmd."
  ["Action"
   [("c" "console"    vmd-console)
    ("P" "pause"      vmd-pause)
    ("s" "start"      vmd-start)
    ("S" "stop"       vmd-stop)
    ("u" "unpause"    vmd-unpause)]])

(defvar vmd-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "c") #'vmd-console)
    (define-key m (kbd "P") #'vmd-pause) ; don't conflict with previous-line
    (define-key m (kbd "s") #'vmd-start)
    (define-key m (kbd "S") #'vmd-stop)
    (define-key m (kbd "u") #'vmd-unpause)

    ;; one transient to rule them all
    (define-key m (kbd "x") #'vmd--transient)
    m))

(define-derived-mode vmd-mode tabulated-list-mode "vmd"
  "Vmd mode."
  (vmd--update-table)
  (add-hook 'tabulated-list-revert-hook #'vmd--update-table nil t))

(defun vmd ()
  "Start vmd."
  (interactive)
  (switch-to-buffer "*vmd*")
  (vmd-mode))

(provide 'vmd)
;;; vmd.el ends here
