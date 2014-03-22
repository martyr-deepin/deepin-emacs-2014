(add-to-list 'load-path default-directory)

(require 'rails)

(byte-compile-disable-warning 'cl-functions)
(mapc #'byte-compile-file
      (directory-files "./" t "\\.el$"))
