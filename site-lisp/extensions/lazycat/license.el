;; license.el --- insert a license header into the active buffer

;; Copyright  (C)  2001, 2005  Marcelo Toledo <marcelo@marcelotoledo.org>

;; Version: 3.1
;; Keywords: licence, gpl, lgpl, fdl, gnu
;; Author: Marcelo Toledo <marcelo@marcelotoledo.org>
;; Maintainer: Marcelo Toledo <marcelo@marcelotoledo.org>
;; URL: http://www.marcelotoledo.org/stuff/projetos/license
;;      http://www.emacswiki.org/cgi-bin/wiki?LicenseCopyright

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;; Commentary: 
;;
;; * Installing
;;
;; To install, you just need to load it, an option is:
;;
;; (load-library "~/.emacs-dir/license.el")
;;
;; I recommend the following setup:
;;
;;     (setq comment-style 'extra-line)
;;     (add-hook 'scheme-mode-hook
;;         (lambda ()
;;             (set (make-local-variable 'comment-add) 1)))
;;
;; * Using
;;
;; If you run license like this:
;;
;; M-x license
;;
;; It will insert the license described in the 
;; 'license-default'variable, the default value is GPL. If you would
;; like to use another license try:
;;
;; C-u M-x license
;;
;; It will prompt you for the license you want to use, you may see the 
;; available options by hitting tab.
;;
;; If you would like to modify the default license, try:
;;
;; (setq license-default "license")

;; Code:

(defvar license-version "3.1"
  "Version of license.")

(defgroup license nil
  "*Add license to your files."
  :prefix "license-"
  :group 'programming)

(defconst license-gnu-gpl (concat
                           "This program is free software; you can redistribute it and/or modify\n"
                           "it under the terms of the GNU General Public License as published by\n"
                           "the Free Software Foundation; either version 2 of the License, or\n"
                           "(at your option) any later version.\n"
                           "\n"
                           "This program is distributed in the hope that it will be useful,\n"
                           "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
                           "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
                           "GNU General Public License for more details.\n"
                           "\n"
                           "You should have received a copy of the GNU General Public License\n"
                           "along with this program; if not, write to the Free Software\n"
                           "Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA\n")
  "GNU GPL License.")

(defconst license-gnu-lgpl (concat
                            "This library is free software; you can redistribute it and/or\n"
                            "modify it under the terms of the GNU Lesser General Public\n"
                            "License as published by the Free Software Foundation; either\n"
                            "version 2.1 of the License, or (at your option) any later version.\n"
                            "\n" 
                            "This library is distributed in the hope that it will be useful,\n"
                            "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
                            "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
                            "Lesser General Public License for more details.\n"
                            "\n" 
                            "You should have received a copy of the GNU Lesser General Public\n"
                            "License along with this library; if not, write to the Free Software\n"
                            "Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA\n")
  "GNU LGPL License.")

(defconst license-gnu-fdl (concat
                           "Permission is granted to copy, distribute and/or modify this document\n"
                           "under the terms of the GNU Free Documentation License, Version 1.2\n"
                           "or any later version published by the Free Software Foundation;\n"
                           "with no Invariant Sections, no Front-Cover Texts, and no Back-Cover\n"
                           "Texts.  A copy of the license is included in the section entitled \"GNU\n"
                           "Free Documentation License\".\n")
  "GNU FDL license.")

(defconst license-mit (concat
                       "Permission is hereby granted, free of charge, to any person\n"
                       "obtaining a copy of this software and associated documentation files\n"
                       "(the \"Software\"), to deal in the Software without restriction,\n"
                       "including without limitation the rights to use, copy, modify, merge,\n"
                       "publish, distribute, sublicense, and/or sell copies of the Software,\n"
                       "and to permit persons to whom the Software is furnished to do so,\n"
                       "subject to the following conditions:\n"
                       "\n"
                       "The above copyright notice and this permission notice shall be\n"
                       "included in all copies or substantial portions of the Software.\n"
                       "\n"
                       "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,\n"
                       "EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF\n"
                       "MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND\n"
                       "NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS\n"
                       "BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN\n"
                       "ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN\n"
                       "CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE\n"
                       "SOFTWARE.")
  "MIT License.")

(defconst license-bsd (concat "Redistribution and use in source and binary forms, with or without"
                              "modification, are  permitted provided that the following conditions are\n"
                              "met:\n"
                              "\n"
                              "Redistributions of source code must retain the above copyright notice,\n"
                              "this list of conditions and the following disclaimer. \n"
                              "\n"
                              "Redistributions in binary form must reproduce the above copyright\n"
                              "notice, this list of conditions and the following disclaimer in the\n"
                              "documentation and/or other materials provided with the distribution.\n"
                              "\n"
                              "Neither the name of the <ORGANIZATION> nor the names of its contributors\n"
                              "may be used to endorse or promote products derived from this software\n"
                              "without specific prior written permission.\n"
                              "\n"
                              "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS\n"
                              "IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED\n"
                              "TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A\n"
                              "PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER\n"
                              "OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,\n"
                              "EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,\n"
                              "PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR\n"
                              "PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF\n"
                              "LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING\n"
                              "NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\n"
                              "SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n")
  "BSD License.")
                              
(defvar license-default "gpl" 
  "*The default license.\n
Options are:\n
 * GPL  - General Public License
 * LGPL - Lesser General Public License
 * FDL  - Free Documentation License
 * MIT  - MIT License
 * BSD  - BSD License")

(defconst license-end-header (concat
                              "\n"
                              "Commentary: \n"
                              "\n"
                              "\n"
                              "\n"
                              "Code:\n\n"))

(defun license-begin-header ()
  "The begin header with buffer name, short description, copyright and all other necessary information."
  (concat (buffer-name) " --- short description\n\n"
          "Copyright  (C)  " (format-time-string "%Y") "  " (license-user-name) " <" (license-user-email) ">\n"
          "\n"
          "Version: 1.0\n"
          "Keywords: \n" 
          "Author: " (license-user-name) " <" (license-user-email) ">\n"
          "Maintainer: " (license-user-name) " <" (license-user-email) ">\n"
          "URL: http://\n"
          "\n"))

(defun license-user-name ()
  "The user full name."
  (or (user-full-name)
      (user-login-name)
      user-full-name
      user-login-name
      "Write_your_name_here"))

(defun license-user-email ()
  "The user email address."
  (or user-mail-address
      "Write_your_email_here"))

(defun license (prefix)
  "Insert a license header into the active buffer."
  (interactive "P")
  (let ((option (if prefix
                    (completing-read
                     "Which license do you want to use? "
                     '(("gpl" 1) ("lgpl" 2) ("fdl" 3) ("mit" 4) ("bsd" 5))
                     nil t nil)
                  (if (equal license-default nil)
                      (error "Variable license-default must be set.")
                    license-default))))
    (if option (progn
		 (goto-char (point-min))
                 (insert-string (license-begin-header))
		 (cond ((string= option "gpl")
                        (insert-string license-gnu-gpl))
		       ((string= option "lgpl")
                        (insert-string license-gnu-lgpl))
		       ((string= option "fdl")
                        (insert-string license-gnu-fdl))
		       ((string= option "mit")
                        (insert-string license-mit))
		       ((string= option "bsd")
                        (insert-string license-bsd)))
                 (insert-string license-end-header)
                 (let ((end (point)))
                   (beginning-of-buffer)
                   (comment-region 1 end))))))

(provide 'license) 
;; license.el ends here
