        Emacs-wget USAGE  ($Date: 2004/10/19 07:13:30 $)

This document describes HOW-TO use emacs-wget.

1. wget commands

   In this section, we describe fundamental commands of emacs-wget.

   The file retrieved by wget goes to wget-download-directory, which
   default value is ~/download.  We call the directory that retrieved
   files go ``download directory''.

1.1. M-x wget

     The most fundamental command for download.

     Input URL of the file which you want to download.

     Called with prefix argument (C-u M-x wget), emacs-wget asks wget
     options and download directory.  Emacs-wget supports completion
     for long options.  Type TAB in the prompt for completing; SPACE is
     not for completion, but a separator of options.

     *CAUTION*: Old GNU wget does not support long type options.  Check
      your wget whether accepting long option.  Anyway, completion list
      is created along the info file of GNU wget v.1.9.1, so some
      options are not supported in old wget.

1.2. M-x wget-web-page

     Download file from URL and all pages linked to it.

     With prefix argument (C-u M-x wget-web-page), ask wget options and
     where to save referring file.

1.3. M-x wget-quit

     Terminate wget process.  If multiple download processes are
     proceeding, choose one of them with completion.

     Emacs-wget provides *wget* buffer, easy to terminate wget process.


2. *wget* buffer

   A buffer as below will open when the emacs-wget starts download.  It
   is called as *wget* buffer.  *wget* buffer displays the current state
   of download.

      ==================================================================
        -- Wget Process ---
        [ 24%]**         http://www.foo.org/emacs-wget/wget-buffer.txt
        =*=DOWNLOADED=*= ~/download/bar.jpg
      ==== *wget* ======== (wget) ======================================

   In *wget* buffer, some keys are pre-defined.  Key bindings are as
   followings:

     key   Operation
    ----- -----------
      d    Kill the wget process under the cursor.
      q    Hide *wget* buffer (Wget process is still alive in background.)
      Q    Quit all wget processes and kill *wget* buffer.
      g    Revert *wget* buffer display.
      i    Update information for wget process under the cursor.
      n    To the next wget process.
      p    To the previous wget process.
    ----- -----------


3. Download directory filter

   Emacs-wget provides the download directory filter.  You can change
   the download directory using regexp match, or set the alias for
   download directories, or both of them, etc...

   You should set wget-download-directory-filter and
   wget-download-directory.  The value of wget-download-directory-filter
   is name of filter function.  wget-download-directory should be
   changed depending on the filter function.

3.1. Specifying filter function

3.1.1. Set download directory by regexp

     (setq wget-download-directory-filter #'wget-download-dir-filter-regexp)

     wget-download-directory should be alist of (REGEXP . DIR).  If
     REGEXP matches URI, set download directory DIR.  If no REGEXP
     matches URI, ask download directory.

     ex.
     (setq wget-download-directory
          '(("\\.\\(jpe?g\\|png\\)$" . "~/pictures")
            ("\\.el$" . "~/site-lisp")
            ("." . "~/download")))

     In this case, *.jpg, *.jpeg, and *.png go to ~/pictures, *.el goes
     to ~/site-lisp, and the others go to ~/download.

3.1.2. Ask alias of download directory

     (setq wget-download-directory-filter #'wget-download-dir-filter-alias)

     wget-download-directory should be alist of (ALIST . DIR).

     Ask ALIAS and set download directory DIR.

     C-u M-x wget ask download directory after asking ALIAS.  This will
     be useful when you set download directory under the ALIAS dir.

     An alias "default" is special.  If ("default" . DIR) is in
     wget-download-directory, M-x wget does not ask ALIAS but use DIR of
     "default".

     ex.
     (setq wget-download-directory
          '(("pics" . "~/pictures")
            ("elisp" . "~/site-lisp")
            ("default" . "~/download")))

     In this case, if M-x wget, set download directory ~/download
     because its ALIAS is "default".

     C-u M-x wget ask ALIAS and then ask download directory.

3.1.3. Set download directory by regexp and ask alias

     (setq wget-download-directory-filter #'wget-download-dir-filter-regexp-and-alias)

     Call wget-download-directory-filter-alias after
     wget-download-directory-filter-regexp.


3.1.4. Check the current directory under the download directory

     (setq wget-download-directory-filter #'wget-download-dir-filter-current-dir)

     No need to change wget-download-directory.

     If current directory is under the wget-download-directory, set
     download directory current dir.  Otherwise, ask download directory.

3.2. Making your filter function

     Filter function takes three arguments and returns download
     directory.  The prototype of filter function is as followed.

     (defun my-wget-download-dir-filter (arg uri dir)
        body...
     )

    The first argument ARG is `non-nil' if C-u is specified.

    The second argument URI is the uri of retrieving file.

    The third argument DIR is the value of wget-download-directory.

    The return value should be a string of download directory.


4. Download log

   Emacs-wget takes log for download if variable wget-download-log-file
   is specified (default is nil).

   ex.
   (setq wget-download-log-file "log.txt")

   You can control the format of log by wget-download-log-format and
   wget-download-log-time-format.  By default, the format of log is as
   followed:

     2004-01-01 01:02:34	http://www.foo.org/bar.txt

   The log is added at the end of log file.  If variable
   wget-add-download-log-eof is `nil', log is added at the beginning of
   log file.

   You can control whether creating new log file by variable
   wget-download-create-log.  If 'always (default), always create log
   file.  If 'ask, ask whether creating log file if it does not exist.
   If nil, do not create log file.


5. Cooperation with web browser on Emacs

   Emacs/W3 and Emacs-w3m are web browser on Emacs.

   Emacs/W3:
     http://www.cs.indiana.edu/elisp/w3/docs.html

   Emacs-w3m:
     http://emacs-w3m.namazu.org/

   Setting c-1. in README file substitutes the command for download from
   default download command of Emacs-w3m (w3m-download-this-url) to
   one of emacs-wget (w3m-wget).

   Setting c-2. in README file adds new command for download (w3-wget).
   Set command your favourite key.  I can not start up Emacs/W3 now, so
   please tell me how to substitute key like Emacs-w3m.


6. Customization

   This section explains the user variables.  You can change them using
   custom package.  The line next to variable name is the default value.

6.1. wget-command
     "wget"

     Program name of `wget'.  If wget command is not in PATH, set it
     using absolute path.

6.2. download directory

6.2.1. wget-download-directory
       "~/download"

     Directory name that retrieved file(s) will go.

     When its value is `nil', emacs-wget always ask the download
     directory.

     If you set wget-download-directory-filter, change
     wget-download-directory depending on the filter function.

6.2.2. wget-download-directory-filter
       nil

     Function that defines the filtering of download directory.

     Following filter functions are prepared:

       wget-download-directory-filter-regexp
         Change download directory by regexp
       wget-download-directory-filter-alias
         Ask alias of download directory
       wget-download-directory-filter-regexp-and-alias
         Change download directory by regexp and ask alias
       wget-download-directory-filter-current-dir
         Ask download dir if current dir is not under wget-download-directory

     You should change wget-download-directory depending on the filter
     function.

6.3. wget options

6.3.1. wget-default-options
       nil

     List of default wget options.

     See your info file of wget for more information.

6.3.2. wget-web-page-options
       '("-r" "-L" "-np")

     List of default wget options when download all web pages.
     wget-web-page-options takes over the value of wget-default-options
     until setting the value explicitly.

     See your info file of wget for more information.

6.3.3. wget-ftp-default-options
       nil

     List of wget options when download from FTP site.  If nil, use
     wget-default-options instead.

     This is an example to download using --passive-ftp option from FTP:

       (add-hook 'wget-load-hook
                 (lambda ()
	           (setq wget-ftp-default-options
                         (append wget-default-options '("--passive-ftp")))))

6.4. wget-executable-file-extension-list
     nil

     List of file extensions.  If the downloaded file matches one of
     them, emacs-wget change its file permission executable.

     ex.
     (setq wget-executable-file-extension-list '("sh" "csh" "pl" "rb"))

6.5. log file

6.5.1. wget-download-log-file
       nil

     File name of download log file.

     If nil, emacs-wget does not take log.

6.5.2. wget-download-create-log
       'always

     Behaviour when log file is not found.

       'always	create log file always
       'ask	ask whether creating log file
       nil	do not create log file

     When nil, you should make empty log file in order to take log.

6.5.3. wget-download-log-format
       "%T\t%U\n"

     Format string for download log.

       %T	Replaced by wget-download-log-time-format
       %t	Replaced by title which is asked when download
       %U	Replaced by URI

     `\t' is for <tab>, and `\n' for newline.

6.5.4. wget-download-log-time-format
       "%Y-%m-%d %H:%M:%S"

     Format string for time, which replace `%T' in
     wget-download-log-format.

     See function format-time-string for format-string.

6.5.5. wget-add-download-log-eof
       t

     If `non-nil', log is added to end of file.
     If nil, log is added to beginning of file.

6.6. hooks

6.6.1. wget-hook
       nil

     A hook run after emacs-wget commands are called.

6.6.2. wget-after-hook
       nil

     A hook run after finishing downloading file.

6.6.3. wget-load-hook
       nil

     A hook run after loading wget.el.

6.7. wget-process-buffer
     "*wget*"

     Buffer name of emacs-wget progress buffer.

     If nil, do not display *wget* buffer.


7. TIPS

7.1. Set download directory current dir.

     (setq wget-download-directory "./")

8. Acknowledgments in USAGE.

   Thanks to Yoichi NAKAYAMA <yoichi@eken.phys.nagoya-u.ac.jp> for
   sending me a translation of USAGE.ja.

   Thanks to SHINYA Akihiko <shinya@sonicteam.com> for his doc fix
   report.



Local Variables:
mode: indented-text
fill-column: 72
End:
