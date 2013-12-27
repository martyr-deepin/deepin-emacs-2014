;; Autoloads for nXthml
;;
;; This file should be updated by `nxhtmlmaint-get-file-autoloads',
;; `nxhtmlmaint-get-dir-autoloads' or `nxhtmlmaint-get-all-autoloads'.
(eval-when-compile (require 'nxhtml-base))
(eval-when-compile (require 'web-vcs))
;;;### (autoloads (cancel-secondary-selection set-secondary-selection
;;;;;;  anchored-transpose) "anchored-transpose" "util/anchored-transpose.el"
;;;;;;  (19333 33326))
;;; Generated autoloads from util/anchored-transpose.el
(web-autoload-require 'anchored-transpose 'lp '(nxhtml-download-root-url nil) "util/anchored-transpose" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'anchored-transpose `(lp '(nxhtml-download-root-url nil) "util/anchored-transpose" nxhtml-install-dir) "\
Transpose portions of the region around an anchor phrase.\n\n`this phrase but not that word'    can be transposed into\n`that word but not this phrase'\n\nI want this phrase but not that word.\n       |----------------------------|. .This is the entire phrase.\n                  |-------|. . . . . . .This is the anchor phrase.\n\nFirst select the entire phrase and type \\[anchored-transpose].\nThis set the secondary selection.\n\nThen select the anchor phrase and type \\[anchored-transpose]\nagain.  Alternatively you can do the selections like this:\n\nI want this phrase but not that word.\n       |----------|       |---------|   Separate phrase selection.\n\nBy default the anchor phrase will automatically include\nany surrounding whitespace even if you don't explicitly select\nit.  Also, it won't include certain trailing punctuation.  See\n`anchored-transpose-do-fuzzy' for details.  A prefix arg prior to\neither selection means `no fuzzy logic, use selections\nliterally'.\n\nYou can select the regions to be swapped separately in any\norder.\n\nAfter swapping both primary and secondary selection are still\nactive.  They will be canceled after second next command if you\ndo not swap regions again.  (Second because this allow you to\nadjust the regions and try again.)\n\nYou can also swap text between different buffers this way.\n\nTyping \\[anchored-transpose] with nothing selected clears any\nprior selection, ie secondary selection.\n\n(fn BEG1 END1 FLG1 &optional BEG2 END2 FLG2 WIN2)" t nil)

(nxhtml-autoload 'set-secondary-selection `(lp '(nxhtml-download-root-url nil) "util/anchored-transpose" nxhtml-install-dir) "\
Set the secondary selection to the current region.\nThis must be bound to a mouse drag event.\n\n(fn BEG END)" t nil)

(nxhtml-autoload 'cancel-secondary-selection `(lp '(nxhtml-download-root-url nil) "util/anchored-transpose" nxhtml-install-dir) "\
Not documented\n\n(fn)" t nil)

;;;***

;;;### (autoloads (appmenu-mode appmenu-add appmenu) "appmenu" "util/appmenu.el"
;;;;;;  (19275 41782))
;;; Generated autoloads from util/appmenu.el
(web-autoload-require 'appmenu 'lp '(nxhtml-download-root-url nil) "util/appmenu" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'appmenu 'custom-loads))) (if (member '"appmenu" loads) nil (put 'appmenu 'custom-loads (cons '"appmenu" loads))))

(nxhtml-autoload 'appmenu-add `(lp '(nxhtml-download-root-url nil) "util/appmenu" nxhtml-install-dir) "\
Add entry to `appmenu-alist'.\nAdd an entry to this list with ID, PRIORITY, TEST, TITLE and\nDEFINITION as explained there.\n\n(fn ID PRIORITY TEST TITLE DEFINITION)" nil nil)

(defvar appmenu-mode nil "\
Non-nil if Appmenu mode is enabled.\nSee the command `appmenu-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `appmenu-mode'.")

(nxhtml-custom-autoload 'appmenu-mode 'appmenu nil)

(nxhtml-autoload 'appmenu-mode `(lp '(nxhtml-download-root-url nil) "util/appmenu" nxhtml-install-dir) "\
Use a context sensitive popup menu.\nAppMenu (appmenu.el) is a framework for creating cooperative\ncontext sensitive popup menus with commands from different major\nand minor modes. Using this different modes may cooperate about\nthe use of popup menus.\n\nThere is also the command `appmenu-as-help' that shows the key\nbindings at current point in the help buffer.\n\nThe popup menu and the help buffer version are on these keys:\n\n\\{appmenu-mode-map}\n\nThe variable `appmenu-alist' is where the popup menu entries\ncomes from.\n\nIf there is a `keymap' property at point then relevant bindings\nfrom this is also shown in the popup menu.\n\nYou can write functions that use whatever information you want in\nEmacs to construct these entries. Since this information is only\ncollected when the popup menu is shown you do not have to care as\nmuch about computation time as for entries in the menu bar.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (as-external-mode as-external-for-wiki as-external-for-mail-mode
;;;;;;  as-external-for-xhtml as-external) "as-external" "util/as-external.el"
;;;;;;  (19292 28108))
;;; Generated autoloads from util/as-external.el
(web-autoload-require 'as-external 'lp '(nxhtml-download-root-url nil) "util/as-external" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'as-external 'custom-loads))) (if (member '"as-external" loads) nil (put 'as-external 'custom-loads (cons '"as-external" loads))))

(nxhtml-autoload 'as-external-for-xhtml `(lp '(nxhtml-download-root-url nil) "util/as-external" nxhtml-install-dir) "\
Setup for Firefox addon It's All Text to edit XHTML.\nIt's All Text is a Firefox add-on for editing textareas with an\nexternal editor.\nSee URL `https://addons.mozilla.org/en-US/firefox/addon/4125'.\n\nIn this case Emacs is used to edit textarea fields on a web page.\nThe text will most often be part of a web page later, like on a\nblog.  Therefore turn on these:\n\n- `nxhtml-mode' since some XHTML tags may be allowed.\n- `nxhtml-validation-header-mode' since it is not a full page.\n- `wrap-to-fill-column-mode' to see what you are writing.\n- `html-write-mode' to see it even better.\n\nAlso bypass the question for line end conversion when using\nemacsw32-eol.\n\n(fn)" t nil)

(nxhtml-autoload 'as-external-for-mail-mode `(lp '(nxhtml-download-root-url nil) "util/as-external" nxhtml-install-dir) "\
Setup for Firefox addon It's All Text to edit mail.\nSet normal mail comment markers in column 1 (ie >).\n\nSet `fill-column' to 90 and enable `wrap-to-fill-column-mode' so\nthat it will look similar to how it will look in the sent plain\ntext mail.\n\nSee also `as-external-mode'.\n\n(fn)" t nil)

(nxhtml-autoload 'as-external-for-wiki `(lp '(nxhtml-download-root-url nil) "util/as-external" nxhtml-install-dir) "\
Setup for Firefox addon It's All Text to edit MediaWikis.\n\n(fn)" t nil)

(defvar as-external-mode nil "\
Non-nil if As-External mode is enabled.\nSee the command `as-external-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `as-external-mode'.")

(nxhtml-custom-autoload 'as-external-mode 'as-external nil)

(nxhtml-autoload 'as-external-mode `(lp '(nxhtml-download-root-url nil) "util/as-external" nxhtml-install-dir) "\
If non-nil check if Emacs is called as external editor.\nWhen Emacs is called as an external editor for example to edit\ntext areas on a web page viewed with Firefox this library tries\nto help to setup the buffer in a useful way. It may for example\nset major and minor modes for the buffer.\n\nThis can for example be useful when blogging or writing comments\non blogs.\n\nSee `as-external-alist' for more information.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (buffer-bg-set-color) "buffer-bg" "util/buffer-bg.el"
;;;;;;  (19254 42506))
;;; Generated autoloads from util/buffer-bg.el
(web-autoload-require 'buffer-bg 'lp '(nxhtml-download-root-url nil) "util/buffer-bg" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'buffer-bg-set-color `(lp '(nxhtml-download-root-url nil) "util/buffer-bg" nxhtml-install-dir) "\
Add an overlay with background color COLOR to buffer BUFFER.\nIf COLOR is nil remove previously added overlay.\n\n(fn COLOR BUFFER)" t nil)

;;;***

;;;### (autoloads (chartg-make-chart chartg-complete) "chartg" "util/chartg.el"
;;;;;;  (19277 59684))
;;; Generated autoloads from util/chartg.el
(web-autoload-require 'chartg 'lp '(nxhtml-download-root-url nil) "util/chartg" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'chartg-complete `(lp '(nxhtml-download-root-url nil) "util/chartg" nxhtml-install-dir) "\
Not documented\n\n(fn)" t nil)

(nxhtml-autoload 'chartg-make-chart `(lp '(nxhtml-download-root-url nil) "util/chartg" nxhtml-install-dir) "\
Try to make a new chart.\nIf region is active then make a new chart from data in the\nselected region.\n\nElse if current buffer is in `chartg-mode' then do it from the\nchart specifications in this buffer.  Otherwise create a new\nbuffer and initialize it with `chartg-mode'.\n\nIf the chart specifications are complete enough to make a chart\nthen do it and show the resulting chart image.  If not then tell\nuser what is missing.\n\nNOTE: This is beta, no alpha code. It is not ready.\n\nBelow are some examples.  To test them mark an example and do\n\n  M-x chartg-make-chart\n\n* Example, simple x-y chart:\n\n  Output-file: \"~/temp-chart.png\"\n  Size: 200 200\n  Data: 3 8 5 | 10 20 30\n  Type: line-chartg-xy\n\n* Example, pie:\n\n  Output-file: \"~/temp-depression.png\"\n  Size: 400 200\n  Data:\n  2,160,000\n  3,110,000\n  1,510,000\n  73,600\n  775,000\n  726,000\n  8,180,000\n  419,000\n  Type: pie-3-dimensional\n  Chartg-title: \"Depression hits on Google\"\n  Legends:\n  \"SSRI\"\n  | \"Psychotherapy\"\n  | \"CBT\"\n  | \"IPT\"\n  | \"Psychoanalysis\"\n  | \"Mindfulness\"\n  | \"Meditation\"\n  | \"Exercise\"\n\n\n* Example, pie:\n\n  Output-file: \"~/temp-panic.png\"\n  Size: 400 200\n  Data:\n  979,000\n  969,000\n  500,000\n  71,900\n  193,000\n  154,000\n  2,500,000\n  9,310,000\n  Type: pie-3-dimensional\n  Chartg-title: \"Depression hits on Google\"\n  Legends:\n  \"SSRI\"\n  | \"Psychotherapy\"\n  | \"CBT\"\n  | \"IPT\"\n  | \"Psychoanalysis\"\n  | \"Mindfulness\"\n  | \"Meditation\"\n  | \"Exercise\"\n\n\n* Example using raw:\n\n  Output-file: \"~/temp-chartg-slipsen-kostar.png\"\n  Size: 400 130\n  Data: 300 1000 30000\n  Type: bar-chartg-horizontal\n  Chartg-title: \"Vad killen i slips tjänar jämfört med dig och mig\"\n  Google-chartg-raw: \"&chds=0,30000&chco=00cd00|ff4500|483d8b&chxt=y,x&chxl=0:|Killen+i+slips|Partiledarna|Du+och+jag&chf=bg,s,ffd700\"\n\n\n(fn)" t nil)

;;;***

;;;### (autoloads (css-color-test css-color-global-mode css-color-mode
;;;;;;  css-color) "css-color" "util/css-color.el" (19266 15016))
;;; Generated autoloads from util/css-color.el
(web-autoload-require 'css-color 'lp '(nxhtml-download-root-url nil) "util/css-color" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'css-color 'custom-loads))) (if (member '"css-color" loads) nil (put 'css-color 'custom-loads (cons '"css-color" loads))))

(nxhtml-autoload 'css-color-mode `(lp '(nxhtml-download-root-url nil) "util/css-color" nxhtml-install-dir) "\
Show hex color literals with the given color as background.\nIn this mode hexadecimal colour specifications like #6600ff are\ndisplayed with the specified colour as background.\n\nCertain keys are bound to special colour editing commands when\npoint is at a hexadecimal colour:\n\n\\{css-color-map}\n\n(fn &optional ARG)" t nil)

(defvar css-color-global-mode nil "\
Non-nil if Css-Color-Global mode is enabled.\nSee the command `css-color-global-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `css-color-global-mode'.")

(nxhtml-custom-autoload 'css-color-global-mode 'css-color nil)

(nxhtml-autoload 'css-color-global-mode `(lp '(nxhtml-download-root-url nil) "util/css-color" nxhtml-install-dir) "\
Toggle Css-Color mode in every possible buffer.\nWith prefix ARG, turn Css-Color-Global mode on if and only if ARG is positive.\nCss-Color mode is enabled in all buffers where `css-color-turn-on-in-buffer' would do it.\nSee `css-color-mode' for more information on Css-Color mode.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'css-color-test `(lp '(nxhtml-download-root-url nil) "util/css-color" nxhtml-install-dir) "\
Test colors interactively.\nThe colors are displayed in the echo area. You can specify the\ncolors as any viable css color.  Example:\n\n  red\n  #f00\n  #0C0\n  #b0ff00\n  hsla(100, 50%, 25%)\n  rgb(255,100,120)\n\n(fn FG-COLOR BG-COLOR)" t nil)

;;;***

;;;### (autoloads (css-palette-global-mode css-palette css-palette-mode)
;;;;;;  "css-palette" "util/css-palette.el" (19234 45588))
;;; Generated autoloads from util/css-palette.el
(web-autoload-require 'css-palette 'lp '(nxhtml-download-root-url nil) "util/css-palette" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'css-palette-mode `(lp '(nxhtml-download-root-url nil) "util/css-palette" nxhtml-install-dir) "\
Minor mode for palettes in CSS.\n\nThe mode `css-palette-mode' acts on the first COLORS declaration in your\n  file of the form:\n\nCOLORS:\n(\nc0 \"#6f5d25\"	;tainted sand\nc1 \"#000000\"	;Black\nc2 \"#cca42b\"	;goldenslumber\nc3 \"#6889cb\"	;far off sky\nc4 \"#fff\"	;strange aeons\n)\n\nSuch declarations should appear inside a block comment, in order\n  to be parsed properly by the LISP reader.\n\nType \\[css-palette-update-all], and any occurence of\n\n  color: #f55; /*[c3]*/\n\nwill be updated with\n\n  color: #6899cb; /*[c3]*/\n\nThe following commands are available to insert key-value pairs\n  and palette declarations:\n  \\{css-palette-mode-map}\n\nYou can extend or redefine the types of palettes by defining a\n  new palette specification of the form (PATTERN REGEXP\n  REF-FOLLOWS-VALUE), named according to the naming scheme\n  css-palette:my-type, where\n\nPATTERN is a pattern containing two (%s) format directives which\n  will be filled in with the variable and its value,\n\nREGEXP is a regular expression to match a value - variable\n  pattern,\n\nand REF-FOLLOWS-VALUE defined whether or not the reference comes\n  after the value. This allows for more flexibility.\n\nNote that, although the w3c spec at URL\n  `http://www.w3.org/TR/CSS2/syndata.html#comments' says that\n  comments \" may occur anywhere between tokens, and their\n  contents have no influence on the rendering\", Internet\n  Explorer does not think so. Better keep all your comments after\n  a \"statement\", as per the default. This means `css-palette'\n  is ill-suited for use within shorthands.\n\nSee variable `css-palette:colors' for an example of a palette\n  type.\n\nThe extension mechanism means that palette types can be used to\n  contain arbitrary key-value mappings.\n\nBesides the colors palette, css-palette defines the palette\n  definition variables `css-palette:colors-outside' and\n  `css-palette:files', for colors with the reference outside and\n  for file url()'s respectively.\n\nYou can fine-control which palette types css-palette should look\n  at via the variable `css-palette-types'.\n\n(fn &optional ARG)" t nil)

(let ((loads (get 'css-palette 'custom-loads))) (if (member '"css-palette" loads) nil (put 'css-palette 'custom-loads (cons '"css-palette" loads))))

(defvar css-palette-global-mode nil "\
Non-nil if Css-Palette-Global mode is enabled.\nSee the command `css-palette-global-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `css-palette-global-mode'.")

(nxhtml-custom-autoload 'css-palette-global-mode 'css-palette nil)

(nxhtml-autoload 'css-palette-global-mode `(lp '(nxhtml-download-root-url nil) "util/css-palette" nxhtml-install-dir) "\
Toggle Css-Palette mode in every possible buffer.\nWith prefix ARG, turn Css-Palette-Global mode on if and only if ARG is positive.\nCss-Palette mode is enabled in all buffers where `css-palette-turn-on-in-buffer' would do it.\nSee `css-palette-mode' for more information on Css-Palette mode.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (cusnu-export-my-skin-options customize-for-new-user)
;;;;;;  "cus-new-user" "util/cus-new-user.el" (19173 34542))
;;; Generated autoloads from util/cus-new-user.el
(web-autoload-require 'cus-new-user 'lp '(nxhtml-download-root-url nil) "util/cus-new-user" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'customize-for-new-user `(lp '(nxhtml-download-root-url nil) "util/cus-new-user" nxhtml-install-dir) "\
Show special customization page for new user.\n\n(fn &optional NAME)" t nil)

(nxhtml-autoload 'cusnu-export-my-skin-options `(lp '(nxhtml-download-root-url nil) "util/cus-new-user" nxhtml-install-dir) "\
Export to file FILE custom options in `cusnu-my-skin-options'.\nThe options is exported to elisp code that other users can run to\nset the options that you have added to `cusnu-my-skin-options'.\n\nFor more information about this see `cusnu-export-cust-group'.\n\n(fn FILE)" t nil)

;;;***

;;;### (autoloads (ediff-url) "ediff-url" "util/ediff-url.el" (19362
;;;;;;  12660))
;;; Generated autoloads from util/ediff-url.el
(web-autoload-require 'ediff-url 'lp '(nxhtml-download-root-url nil) "util/ediff-url" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'ediff-url `(lp '(nxhtml-download-root-url nil) "util/ediff-url" nxhtml-install-dir) "\
Compare current buffer to a web URL using `ediff-buffers'.\nCheck URL using `ediff-url-redirects' before fetching the file.\n\nThis is for checking downloaded file.  A the file may have a comment\ntelling the download URL of thise form in the header:\n\n   ;; URL: http://the-server.net/the-path/the-file.el\n\nIf not the user is asked for the URL.\n\n(fn URL)" t nil)

;;;***

;;;### (autoloads (ffip-find-file-in-dirtree ffip-set-current-project)
;;;;;;  "ffip" "util/ffip.el" (19257 3834))
;;; Generated autoloads from util/ffip.el
(web-autoload-require 'ffip 'lp '(nxhtml-download-root-url nil) "util/ffip" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'ffip-set-current-project `(lp '(nxhtml-download-root-url nil) "util/ffip" nxhtml-install-dir) "\
Setup ffip project NAME with top directory ROOT of type TYPE.\nROOT can either be just a directory or a list of directory where\nthe first used just for prompting purposes and the files in the\nrest are read into the ffip project.\n\nType is a type in `ffip-project-file-types'.\n\n(fn NAME ROOT TYPE)" nil nil)

(nxhtml-autoload 'ffip-find-file-in-dirtree `(lp '(nxhtml-download-root-url nil) "util/ffip" nxhtml-install-dir) "\
Find files in directory tree ROOT.\n\n(fn ROOT)" t nil)

;;;***

;;;### (autoloads (fold-dwim-turn-on-outline-and-hide-all fold-dwim-turn-on-hs-and-hide
;;;;;;  fold-dwim-unhide-hs-and-outline fold-dwim-mode fold-dwim-toggle
;;;;;;  fold-dwim) "fold-dwim" "util/fold-dwim.el" (19218 20582))
;;; Generated autoloads from util/fold-dwim.el
(web-autoload-require 'fold-dwim 'lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'fold-dwim 'custom-loads))) (if (member '"fold-dwim" loads) nil (put 'fold-dwim 'custom-loads (cons '"fold-dwim" loads))))

(nxhtml-autoload 'fold-dwim-toggle `(lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir) "\
Toggle visibility or some other visual things.\nTry toggling different visual things in this order:\n\n- Images shown at point with `inlimg-mode'\n- Text at point prettified by `html-write-mode'.\n\nFor the rest it unhides if possible, otherwise hides in this\norder:\n\n- `org-mode' header or something else using that outlines.\n- Maybe `fold-dwim-toggle-selective-display'.\n- `Tex-fold-mode' things.\n- In html if `outline-minor-mode' and after heading hide content.\n- `hs-minor-mode' things.\n- `outline-minor-mode' things. (Turns maybe on this.)\n\nIt uses `fold-dwim-show' to show any hidden text at point; if no\nhidden fold is found, try `fold-dwim-hide' to hide the\nconstruction at the cursor.\n\nNote: Also first turn on `fold-dwim-mode' to get the keybinding\nfor this function from it.\n\n(fn)" t nil)

(defvar fold-dwim-mode nil "\
Non-nil if Fold-Dwim mode is enabled.\nSee the command `fold-dwim-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `fold-dwim-mode'.")

(nxhtml-custom-autoload 'fold-dwim-mode 'fold-dwim nil)

(nxhtml-autoload 'fold-dwim-mode `(lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir) "\
Key binding for `fold-dwim-toggle'.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'fold-dwim-unhide-hs-and-outline `(lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir) "\
Unhide everything hidden by Hide/Show and Outline.\nIe everything hidden by `hs-minor-mode' and\n`outline-minor-mode'.\n\n(fn)" t nil)

(nxhtml-autoload 'fold-dwim-turn-on-hs-and-hide `(lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir) "\
Turn on minor mode `hs-minor-mode' and hide.\nIf major mode is derived from `nxml-mode' call `hs-hide-block'\nelse call `hs-hide-all'.\n\n(fn)" t nil)

(nxhtml-autoload 'fold-dwim-turn-on-outline-and-hide-all `(lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir) "\
Turn on `outline-minor-mode' and call `hide-body'.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (foldit-global-mode foldit-mode foldit) "foldit"
;;;;;;  "util/foldit.el" (19275 41782))
;;; Generated autoloads from util/foldit.el
(web-autoload-require 'foldit 'lp '(nxhtml-download-root-url nil) "util/foldit" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'foldit 'custom-loads))) (if (member '"foldit" loads) nil (put 'foldit 'custom-loads (cons '"foldit" loads))))

(nxhtml-autoload 'foldit-mode `(lp '(nxhtml-download-root-url nil) "util/foldit" nxhtml-install-dir) "\
Minor mode providing visual aids for folding.\nShows some hints about what you have hidden and how to reveal it.\n\nSupports `hs-minor-mode', `outline-minor-mode' and major modes\nderived from `outline-mode'.\n\n(fn &optional ARG)" t nil)

(defvar foldit-global-mode nil "\
Non-nil if Foldit-Global mode is enabled.\nSee the command `foldit-global-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `foldit-global-mode'.")

(nxhtml-custom-autoload 'foldit-global-mode 'foldit nil)

(nxhtml-autoload 'foldit-global-mode `(lp '(nxhtml-download-root-url nil) "util/foldit" nxhtml-install-dir) "\
Toggle Foldit mode in every possible buffer.\nWith prefix ARG, turn Foldit-Global mode on if and only if ARG is positive.\nFoldit mode is enabled in all buffers where `(lambda nil (foldit-mode 1))' would do it.\nSee `foldit-mode' for more information on Foldit mode.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (gimpedit-can-edit gimpedit-edit-buffer gimpedit-edit-file
;;;;;;  gimpedit) "gimpedit" "util/gimpedit.el" (19275 41782))
;;; Generated autoloads from util/gimpedit.el
(web-autoload-require 'gimpedit 'lp '(nxhtml-download-root-url nil) "util/gimpedit" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'gimpedit 'custom-loads))) (if (member '"gimpedit" loads) nil (put 'gimpedit 'custom-loads (cons '"gimpedit" loads))))

(nxhtml-autoload 'gimpedit-edit-file `(lp '(nxhtml-download-root-url nil) "util/gimpedit" nxhtml-install-dir) "\
Edit IMAGE-FILE with GIMP.\nSee also `gimpedit-edit-file'.\n\n(fn IMAGE-FILE &optional EXTRA-ARGS)" t nil)

(nxhtml-autoload 'gimpedit-edit-buffer `(lp '(nxhtml-download-root-url nil) "util/gimpedit" nxhtml-install-dir) "\
Edit image file in current buffer with GIMP.\nSee also `gimpedit-edit-file'.\n\nYou may also be interested in gimpedit-mode with which you can edit\ngimp files from within Emacs using GIMP's scripting\npossibilities. See\n\n  URL `http://www.emacswiki.org/emacs/GimpMode'\n\n(fn)" t nil)

(nxhtml-autoload 'gimpedit-can-edit `(lp '(nxhtml-download-root-url nil) "util/gimpedit" nxhtml-install-dir) "\
Not documented\n\n(fn FILE-NAME)" nil nil)

;;;***

;;;### (autoloads (gpl-mode) "gpl" "util/gpl.el" (18795 5710))
;;; Generated autoloads from util/gpl.el
(web-autoload-require 'gpl 'lp '(nxhtml-download-root-url nil) "util/gpl" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'gpl-mode `(lp '(nxhtml-download-root-url nil) "util/gpl" nxhtml-install-dir) "\
Mode for font-locking and editing color palettes of the GPL format.\n\nSuch palettes are used and produced by free software applications\nsuch as the GIMP, Inkscape, Scribus, Agave and on-line tools such\nas http://colourlovers.com.\n\nYou can also use\nURL `http://niels.kicks-ass.org/public/elisp/css-palette.el' to import\nsuch palette into a css-file as hexadecimal color palette.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (hfyview-frame hfyview-window hfyview-region hfyview-buffer
;;;;;;  hfyview-quick-print-in-files-menu) "hfyview" "util/hfyview.el"
;;;;;;  (19405 63628))
;;; Generated autoloads from util/hfyview.el
(web-autoload-require 'hfyview 'lp '(nxhtml-download-root-url nil) "util/hfyview" nxhtml-install-dir 'nxhtml-byte-compile-file)


(defvar hfyview-quick-print-in-files-menu nil "\
Add Quick print entries to File menu if non-nil.\nIf you set this to nil you have to restart Emacs to get rid of\nthe Quick Print entry.")

(nxhtml-custom-autoload 'hfyview-quick-print-in-files-menu 'hfyview nil)

(nxhtml-autoload 'hfyview-buffer `(lp '(nxhtml-download-root-url nil) "util/hfyview" nxhtml-install-dir) "\
Convert buffer to html preserving faces and show in web browser.\nWith command prefix ARG also show html source in other window.\n\n(fn ARG)" t nil)

(nxhtml-autoload 'hfyview-region `(lp '(nxhtml-download-root-url nil) "util/hfyview" nxhtml-install-dir) "\
Convert region to html preserving faces and show in web browser.\nWith command prefix ARG also show html source in other window.\n\n(fn ARG)" t nil)

(nxhtml-autoload 'hfyview-window `(lp '(nxhtml-download-root-url nil) "util/hfyview" nxhtml-install-dir) "\
Convert window to html preserving faces and show in web browser.\nWith command prefix ARG also show html source in other window.\n\n(fn ARG)" t nil)

(nxhtml-autoload 'hfyview-frame `(lp '(nxhtml-download-root-url nil) "util/hfyview" nxhtml-install-dir) "\
Convert frame to html preserving faces and show in web browser.\nMake an XHTML view of the current Emacs frame. Put it in a buffer\nnamed *hfyview-frame* and show that buffer in a web browser.\n\nIf WHOLE-BUFFERS is non-nil then the whole content of the buffers\nis shown in the XHTML page, otherwise just the part that is\nvisible currently on the frame.\n\nIf you turn on the minor mode `hfyview-frame-mode' you can also\nget the minibuffer/echo area in the output. See this mode for\ndetails.\n\nWith command prefix also show html source in other window.\n\n(fn WHOLE-BUFFERS)" t nil)

;;;***

;;;### (autoloads (hl-needed-mode hl-needed) "hl-needed" "util/hl-needed.el"
;;;;;;  (19405 63628))
;;; Generated autoloads from util/hl-needed.el
(web-autoload-require 'hl-needed 'lp '(nxhtml-download-root-url nil) "util/hl-needed" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'hl-needed 'custom-loads))) (if (member '"hl-needed" loads) nil (put 'hl-needed 'custom-loads (cons '"hl-needed" loads))))

(defvar hl-needed-mode nil "\
Non-nil if Hl-Needed mode is enabled.\nSee the command `hl-needed-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `hl-needed-mode'.")

(nxhtml-custom-autoload 'hl-needed-mode 'hl-needed nil)

(nxhtml-autoload 'hl-needed-mode `(lp '(nxhtml-download-root-url nil) "util/hl-needed" nxhtml-install-dir) "\
Try to highlight current line and column when needed.\nThis is a global minor mode.  It can operate in some different\nways:\n\n- Highlighting can be on always, see `hl-needed-always'.\n\nOr, it can be turned on depending on some conditions.  In this\ncase highlighting is turned off after each command and turned on\nagain in the current window when either:\n\n- A new window was selected, see `hl-needed-on-new-window'.\n- A new buffer was selected, see `hl-needed-on-new-buffer'.\n- Window configuration was changed, see `hl-needed-on-config-change'.\n- Buffer was scrolled see `hl-needed-on-scrolling'.\n- A window was clicked with the mouse, see `hl-needed-on-mouse'.\n\nAfter this highlighting may be turned off again, normally after a\nshort delay, see `hl-needed-flash'.\n\nIf either highlighting was not turned on or was turned off again\nit will be turned on when\n\n- Emacs has been idle for `hl-needed-idle-time' seconds.\n\nSee also `hl-needed-not-in-modes' and `hl-needed-currently-fun'.\n\nNote 1: For columns to be highlighted vline.el must be available.\n\nNote 2: This mode depends on `hl-line-mode' and `vline-mode' and\ntries to cooperate with them. If you turn on either of these that\noverrides the variables for turning on the respective\nhighlighting here.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (html-write-mode html-write) "html-write" "util/html-write.el"
;;;;;;  (19275 41782))
;;; Generated autoloads from util/html-write.el
(web-autoload-require 'html-write 'lp '(nxhtml-download-root-url nil) "util/html-write" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-write 'custom-loads))) (if (member '"html-write" loads) nil (put 'html-write 'custom-loads (cons '"html-write" loads))))

(nxhtml-autoload 'html-write-mode `(lp '(nxhtml-download-root-url nil) "util/html-write" nxhtml-install-dir) "\
Minor mode for convenient display of some HTML tags.\nWhen this mode is on a tag in `html-write-tag-list' is displayed as\nthe inner text of the tag with a face corresponding to the tag.\nBy default for example <i>...</i> is displayed as italic and\n<a>...</a> is displayed as an underlined clickable link.\n\nOnly non-nested tags are hidden.  The idea is just that it should\nbe easier to read and write, not that it should look as html\nrendered text.\n\nSee the customization group `html-write' for more information about\nfaces.\n\nThe following keys are defined when you are on a tag handled by\nthis minor mode:\n\n\\{html-write-keymap}\n\nIMPORTANT: Most commands you use works also on the text that is\nhidden.  The movement commands is an exception, but as soon as\nyou edit the buffer you may also change the hidden parts.\n\nHint: Together with `wrap-to-fill-column-mode' this can make it\neasier to see what text you are actually writing in html parts of\na web file.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (inlimg-toggle-slicing inlimg-toggle-display inlimg-global-mode
;;;;;;  inlimg-mode inlimg) "inlimg" "util/inlimg.el" (19269 11410))
;;; Generated autoloads from util/inlimg.el
(web-autoload-require 'inlimg 'lp '(nxhtml-download-root-url nil) "util/inlimg" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'inlimg 'custom-loads))) (if (member '"inlimg" loads) nil (put 'inlimg 'custom-loads (cons '"inlimg" loads))))

(nxhtml-autoload 'inlimg-mode `(lp '(nxhtml-download-root-url nil) "util/inlimg" nxhtml-install-dir) "\
Display images inline.\nSearch buffer for image tags.  Display found images.\n\nImage tags are setup per major mode in `inlimg-mode-specs'.\n\nImages are displayed on a line below the tag referencing them.\nThe whole image or a slice of it may be displayed, see\n`inlimg-slice'.  Margins relative text are specified in\n`inlimg-margins'.\n\nSee also the commands `inlimg-toggle-display' and\n`inlimg-toggle-slicing'.\n\nNote: This minor mode uses `font-lock-mode'.\n\n(fn &optional ARG)" t nil)

(defvar inlimg-global-mode nil "\
Non-nil if Inlimg-Global mode is enabled.\nSee the command `inlimg-global-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `inlimg-global-mode'.")

(nxhtml-custom-autoload 'inlimg-global-mode 'inlimg nil)

(nxhtml-autoload 'inlimg-global-mode `(lp '(nxhtml-download-root-url nil) "util/inlimg" nxhtml-install-dir) "\
Toggle Inlimg mode in every possible buffer.\nWith prefix ARG, turn Inlimg-Global mode on if and only if ARG is positive.\nInlimg mode is enabled in all buffers where `inlimg--global-turn-on' would do it.\nSee `inlimg-mode' for more information on Inlimg mode.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'inlimg-toggle-display `(lp '(nxhtml-download-root-url nil) "util/inlimg" nxhtml-install-dir) "\
Toggle display of image at point POINT.\nSee also the command `inlimg-mode'.\n\n(fn POINT)" t nil)

(nxhtml-autoload 'inlimg-toggle-slicing `(lp '(nxhtml-download-root-url nil) "util/inlimg" nxhtml-install-dir) "\
Toggle slicing of image at point POINT.\nSee also the command `inlimg-mode'.\n\n(fn POINT)" t nil)

;;;***

;;;### (autoloads (majmodpri majmodpri-apply-priorities majmodpri-apply
;;;;;;  majmodpri-sort-lists) "majmodpri" "util/majmodpri.el" (19407
;;;;;;  1190))
;;; Generated autoloads from util/majmodpri.el
(web-autoload-require 'majmodpri 'lp '(nxhtml-download-root-url nil) "util/majmodpri" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'majmodpri-sort-lists `(lp '(nxhtml-download-root-url nil) "util/majmodpri" nxhtml-install-dir) "\
Sort the list used when selecting major mode.\nOnly sort those lists choosen in `majmodpri-lists-to-sort'.\nSort according to priorities in `majmodpri-mode-priorities'.\nKeep the old order in the list otherwise.\n\nThe lists can be sorted when loading elisp libraries, see\n`majmodpri-sort-after-load'.\n\nSee also `majmodpri-apply-priorities'.\n\n(fn)" t nil)

(nxhtml-autoload 'majmodpri-apply `(lp '(nxhtml-download-root-url nil) "util/majmodpri" nxhtml-install-dir) "\
Sort major mode lists and apply to existing buffers.\nNote: This function is suitable to add to\n`desktop-after-read-hook'. It will restore the multi major modes\nin buffers.\n\n(fn)" nil nil)

(nxhtml-autoload 'majmodpri-apply-priorities `(lp '(nxhtml-download-root-url nil) "util/majmodpri" nxhtml-install-dir) "\
Apply major mode priorities.\nFirst run `majmodpri-sort-lists' and then if CHANGE-MODES is\nnon-nil apply to existing file buffers.  If interactive ask\nbefore applying.\n\n(fn CHANGE-MODES)" t nil)

(let ((loads (get 'majmodpri 'custom-loads))) (if (member '"majmodpri" loads) nil (put 'majmodpri 'custom-loads (cons '"majmodpri" loads))))

;;;***

;;;### (autoloads (markchars-global-mode markchars-mode markchars)
;;;;;;  "markchars" "util/markchars.el" (19375 45890))
;;; Generated autoloads from util/markchars.el
(web-autoload-require 'markchars 'lp '(nxhtml-download-root-url nil) "util/markchars" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'markchars 'custom-loads))) (if (member '"markchars" loads) nil (put 'markchars 'custom-loads (cons '"markchars" loads))))

(nxhtml-autoload 'markchars-mode `(lp '(nxhtml-download-root-url nil) "util/markchars" nxhtml-install-dir) "\
Mark special characters.\nWhich characters to mark are defined by `markchars-keywords'.\n\nThe default is to mark non-IDN, non-ascii chars with a magenta\nunderline.\n\nFor information about IDN chars see `idn-is-recommended'.\n\nIf you change anything in the customization group `markchars' you\nmust restart this minor mode for the changes to take effect.\n\n(fn &optional ARG)" t nil)

(defvar markchars-global-mode nil "\
Non-nil if Markchars-Global mode is enabled.\nSee the command `markchars-global-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `markchars-global-mode'.")

(nxhtml-custom-autoload 'markchars-global-mode 'markchars nil)

(nxhtml-autoload 'markchars-global-mode `(lp '(nxhtml-download-root-url nil) "util/markchars" nxhtml-install-dir) "\
Toggle Markchars mode in every possible buffer.\nWith prefix ARG, turn Markchars-Global mode on if and only if ARG is positive.\nMarkchars mode is enabled in all buffers where `(lambda nil (markchars-mode 1))' would do it.\nSee `markchars-mode' for more information on Markchars mode.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (mlinks-global-mode mlinks-mode mlinks) "mlinks"
;;;;;;  "util/mlinks.el" (19364 34616))
;;; Generated autoloads from util/mlinks.el
(web-autoload-require 'mlinks 'lp '(nxhtml-download-root-url nil) "util/mlinks" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'mlinks 'custom-loads))) (if (member '"mlinks" loads) nil (put 'mlinks 'custom-loads (cons '"mlinks" loads))))

(nxhtml-autoload 'mlinks-mode `(lp '(nxhtml-download-root-url nil) "util/mlinks" nxhtml-install-dir) "\
Recognizes certain parts of a buffer as hyperlinks.\nThe hyperlinks are created in different ways for different major\nmodes with the help of the functions in the list\n`mlinks-mode-functions'.\n\nThe hyperlinks can be hilighted when point is over them.  Use\n`mlinks-toggle-hilight' to toggle this feature for the current\nbuffer.\n\nAll keybindings in this mode are by default done under the prefi§x\nkey\n\n  C-c RET\n\nwhich is supposed to be a kind of mnemonic for link (alluding to\nthe RET key commonly used in web browser to follow a link).\n(Unfortunately this breaks the rules in info node `Key Binding\nConventions'.) Below are the key bindings defined by this mode:\n\n\\{mlinks-mode-map}\n\nFor some major modes `mlinks-backward-link' and\n`mlinks-forward-link' will take you to the previous/next link.\nBy default the link moved to will be active, see\n`mlinks-active-links'.\n\n(fn &optional ARG)" t nil)

(defvar mlinks-global-mode nil "\
Non-nil if Mlinks-Global mode is enabled.\nSee the command `mlinks-global-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `mlinks-global-mode'.")

(nxhtml-custom-autoload 'mlinks-global-mode 'mlinks nil)

(nxhtml-autoload 'mlinks-global-mode `(lp '(nxhtml-download-root-url nil) "util/mlinks" nxhtml-install-dir) "\
Toggle Mlinks mode in every possible buffer.\nWith prefix ARG, turn Mlinks-Global mode on if and only if ARG is positive.\nMlinks mode is enabled in all buffers where `mlinks-turn-on-in-buffer' would do it.\nSee `mlinks-mode' for more information on Mlinks mode.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (mumamo-multi-major-modep mumamo-list-defined-multi-major-modes
;;;;;;  mumamo-mark-for-refontification mumamo-hi-lock-faces mumamo
;;;;;;  mumamo-add-to-defined-multi-major-modes define-mumamo-multi-major-mode)
;;;;;;  "mumamo" "util/mumamo.el" (19412 8766))
;;; Generated autoloads from util/mumamo.el
(web-autoload-require 'mumamo 'lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'define-mumamo-multi-major-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir) "\
Define a function that turn on support for multiple major modes.\nDefine a function FUN-SYM that set up to divide the current\nbuffer into chunks with different major modes.\n\nThe documentation string for FUN-SYM should contain the special\ndocumentation in the string SPEC-DOC, general documentation for\nfunctions of this type and information about chunks.\n\nThe new function will use the definitions in CHUNKS (which is\ncalled a \"chunk family\") to make the dividing of the buffer.\n\nThe function FUN-SYM can be used to setup a buffer instead of a\nmajor mode function:\n\n- The function FUN-SYM can be called instead of calling a major\n  mode function when you want to use multiple major modes in a\n  buffer.\n\n- The defined function can be used instead of a major mode\n  function in for example `auto-mode-alist'.\n\n- As the very last thing FUN-SYM will run the hook FUN-SYM-hook,\n  just as major modes do.\n\n- There is also a general hook, `mumamo-turn-on-hook', which is\n  run when turning on mumamo with any of these functions.  This\n  is run right before the hook specific to any of the functions\n  above that turns on the multiple major mode support.\n\n- The multi major mode FUN-SYM has a keymap named FUN-SYM-map.\n  This overrides the major modes' keymaps since it is handled as\n  a minor mode keymap.\n\n- There is also a special mumamo keymap, `mumamo-map' that is\n  active in every buffer with a multi major mode.  This is also\n  handled as a minor mode keymap and therefor overrides the major\n  modes' keymaps.\n\n- However when this support for multiple major mode is on the\n  buffer is divided into chunks, each with its own major mode.\n\n- The chunks are fontified according the major mode assigned to\n  them for that.\n\n- Indenting is also done according to the major mode assigned to\n  them for that.\n\n- The actual major mode used in the buffer is changed to the one\n  in the chunk when moving point between these chunks.\n\n- When major mode is changed the hooks for the new major mode,\n  `after-change-major-mode-hook' and `change-major-mode-hook' are\n  run.\n\n- There will be an alias for FUN-SYM called mumamo-alias-FUN-SYM.\n  This can be used to check whic multi major modes have been\n  defined.\n\n** A little bit more technical description:\n\nThe dividing of a buffer into chunks is done during fontification\nby `mumamo-get-chunk-at'.\n\nThe name of the function is saved in in the buffer local variable\n`mumamo-multi-major-mode' when the function is called.\n\nAll functions defined by this macro is added to the list\n`mumamo-defined-multi-major-modes'.\n\nBasically Mumamo handles only major modes that uses jit-lock.\nHowever as a special effort also `nxml-mode' and derivatives\nthereof are handled.  Since it seems impossible to me to restrict\nthose major modes fontification to only a chunk without changing\n`nxml-mode' the fontification is instead done by\n`html-mode'/`sgml-mode' for chunks using `nxml-mode' and its\nderivates.\n\nCHUNKS is a list where each entry have the format\n\n  (CHUNK-DEF-NAME MAIN-MAJOR-MODE SUBMODE-CHUNK-FUNCTIONS)\n\nCHUNK-DEF-NAME is the key name by which the entry is recognized.\nMAIN-MAJOR-MODE is the major mode used when there is no chunks.\nIf this is nil then `major-mode' before turning on this mode will\nbe used.\n\nSUBMODE-CHUNK-FUNCTIONS is a list of the functions that does the\nchunk division of the buffer.  They are tried in the order they\nappear here during the chunk division process.\n\nIf you want to write new functions for chunk divisions then\nplease see `mumamo-find-possible-chunk'.  You can perhaps also\nuse `mumamo-quick-static-chunk' which is more easy-to-use\nalternative.  See also the file mumamo-fun.el where there are\nmany routines for chunk division.\n\nWhen you write those new functions you may want to use some of\nthe functions for testing chunks:\n\n `mumamo-test-create-chunk-at'  `mumamo-test-create-chunks-at-all'\n `mumamo-test-easy-make'        `mumamo-test-fontify-region'\n\nThese are in the file mumamo-test.el.\n\n(fn FUN-SYM SPEC-DOC CHUNKS)" nil (quote macro))

(nxhtml-autoload 'mumamo-add-to-defined-multi-major-modes `(lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir) "\
Not documented\n\n(fn ENTRY)" nil nil)

(let ((loads (get 'mumamo 'custom-loads))) (if (member '"mumamo" loads) nil (put 'mumamo 'custom-loads (cons '"mumamo" loads))))

(let ((loads (get 'mumamo-hi-lock-faces 'custom-loads))) (if (member '"mumamo" loads) nil (put 'mumamo-hi-lock-faces 'custom-loads (cons '"mumamo" loads))))

(nxhtml-autoload 'mumamo-mark-for-refontification `(lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir) "\
Mark region between MIN and MAX for refontification.\n\n(fn MIN MAX)" nil nil)

(nxhtml-autoload 'mumamo-list-defined-multi-major-modes `(lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir) "\
List currently defined multi major modes.\nIf SHOW-DOC is non-nil show the doc strings added when defining\nthem. (This is not the full doc string. To show the full doc\nstring you can click on the multi major mode in the list.)\n\nIf SHOW-CHUNKS is non-nil show the names of the chunk dividing\nfunctions each multi major mode uses.\n\nIf MATCH then show only multi major modes whos names matches.\n\n(fn SHOW-DOC SHOW-CHUNKS MATCH)" t nil)

(nxhtml-autoload 'mumamo-multi-major-modep `(lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir) "\
Return t if VALUE is a multi major mode function.\n\n(fn VALUE)" nil nil)

;;;***

;;;### (autoloads (python-rst-mumamo-mode latex-haskell-mumamo-mode
;;;;;;  latex-clojure-mumamo-mode markdown-html-mumamo-mode xsl-sgml-mumamo-mode
;;;;;;  xsl-nxml-mumamo-mode mako-html-mumamo-mode org-mumamo-mode
;;;;;;  asp-html-mumamo-mode noweb2-mumamo-mode mumamo-noweb2 csound-sgml-mumamo-mode
;;;;;;  laszlo-nxml-mumamo-mode metapost-mumamo-mode ruby-heredoc-mumamo-mode
;;;;;;  python-heredoc-mumamo-mode cperl-heredoc-mumamo-mode perl-heredoc-mumamo-mode
;;;;;;  php-heredoc-mumamo-mode sh-heredoc-mumamo-mode eruby-javascript-mumamo-mode
;;;;;;  eruby-html-mumamo-mode eruby-mumamo-mode jsp-html-mumamo-mode
;;;;;;  gsp-html-mumamo-mode ssjs-html-mumamo-mode smarty-html-mumamo-mode
;;;;;;  mjt-html-mumamo-mode genshi-html-mumamo-mode django-html-mumamo-mode
;;;;;;  embperl-html-mumamo-mode mason-html-mumamo-mode nxml-mumamo-mode
;;;;;;  html-mumamo-mode mumamo-define-html-file-wide-keys) "mumamo-fun"
;;;;;;  "util/mumamo-fun.el" (19410 1572))
;;; Generated autoloads from util/mumamo-fun.el
(web-autoload-require 'mumamo-fun 'lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'mumamo-define-html-file-wide-keys `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Define keys in multi major mode keymap for html files.\n\n(fn)" nil nil)

(nxhtml-autoload 'html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for (X)HTML with main mode `html-mode'.\nThis covers inlined style and javascript and PHP." t)

(nxhtml-autoload 'nxml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for (X)HTML with main mode `nxml-mode'.\nThis covers inlined style and javascript and PHP.\n\nSee also `mumamo-alt-php-tags-mode'." t)

(nxhtml-autoload 'mason-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Mason using main mode `html-mode'.\nThis covers inlined style and javascript." t)

(nxhtml-autoload 'embperl-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Embperl files with main mode `html-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'django-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Django with main mode `html-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'genshi-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Genshi with main mode `html-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'mjt-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for MJT with main mode `html-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'smarty-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Smarty with main mode `html-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'ssjs-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for SSJS with main mode `html-mode'.\nThis covers inlined style and javascript." t)

(nxhtml-autoload 'gsp-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for GSP with main mode `html-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'jsp-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for JSP with main mode `html-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'eruby-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major mode for eRuby with unspecified main mode.\nCurrent major-mode will be used as the main major mode." t)

(nxhtml-autoload 'eruby-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for eRuby with main mode `html-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'eruby-javascript-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for eRuby with main mode `javascript-mode'." t)

(nxhtml-autoload 'sh-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for sh heredoc document.\nSee `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'php-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for PHP heredoc document.\nSee `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'perl-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Perl heredoc document.\nSee `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'cperl-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Perl heredoc document.\nSee `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'python-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Perl heredoc document.\nSee `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'ruby-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Ruby heredoc document.\nSee `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'metapost-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for MetaPost." t)

(nxhtml-autoload 'laszlo-nxml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for OpenLaszlo." t)

(nxhtml-autoload 'csound-sgml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on mutiple major modes for CSound orc/sco Modes." t)

(let ((loads (get 'mumamo-noweb2 'custom-loads))) (if (member '"mumamo-fun" loads) nil (put 'mumamo-noweb2 'custom-loads (cons '"mumamo-fun" loads))))

(nxhtml-autoload 'noweb2-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Multi major mode for noweb files." t)

(nxhtml-autoload 'asp-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for ASP with main mode `html-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'org-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for `org-mode' files with main mode `org-mode'.\n** Note about HTML subchunks:\nUnfortunately this only allows `html-mode' (not `nxhtml-mode') in\nsub chunks." t)

(nxhtml-autoload 'mako-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Mako with main mode `html-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'xsl-nxml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multi major mode for XSL with main mode `nxml-mode'.\nThis covers inlined style and javascript." t)

(nxhtml-autoload 'xsl-sgml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multi major mode for XSL with main mode `sgml-mode'.\nThis covers inlined style and javascript." t)

(nxhtml-autoload 'markdown-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multi major markdown mode in buffer.\nMain major mode will be `markdown-mode'.\nInlined html will be in `html-mode'.\n\nYou need `markdown-mode' which you can download from URL\n`http://jblevins.org/projects/markdown-mode/'." t)

(nxhtml-autoload 'latex-clojure-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multi major mode latex+clojure.\nMain major mode will be `latex-mode'.\nSubchunks will be in `clojure-mode'.\n\nYou will need `clojure-mode' which you can download from URL\n`http://github.com/jochu/clojure-mode/tree'." t)

(nxhtml-autoload 'latex-haskell-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multi major mode latex+haskell.\nMain major mode will be `latex-mode'.\nSubchunks will be in `haskell-mode'.\n\nYou will need `haskell-mode' which you can download from URL\n`http://projects.haskell.org/haskellmode-emacs/'." t)

(nxhtml-autoload 'python-rst-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Python with RestructuredText docstrings." t)

;;;***

;;;### (autoloads (mumamo-add-region-from-string mumamo-add-region)
;;;;;;  "mumamo-regions" "util/mumamo-regions.el" (19275 41782))
;;; Generated autoloads from util/mumamo-regions.el
(web-autoload-require 'mumamo-regions 'lp '(nxhtml-download-root-url nil) "util/mumamo-regions" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'mumamo-add-region `(lp '(nxhtml-download-root-url nil) "util/mumamo-regions" nxhtml-install-dir) "\
Add a mumamo region from selection.\nMumamo regions are like another layer of chunks above the normal chunks.\nThey does not affect the normal chunks, but they overrides them.\n\nTo create a mumamo region first select a visible region and then\ncall this function.\n\nIf the buffer is not in a multi major mode a temporary multi\nmajor mode will be created applied to the buffer first.\nTo get out of this and get back to a single major mode just use\n\n  M-x normal-mode\n\n(fn)" t nil)

(nxhtml-autoload 'mumamo-add-region-from-string `(lp '(nxhtml-download-root-url nil) "util/mumamo-regions" nxhtml-install-dir) "\
Add a mumamo region from string at point.\nWorks as `mumamo-add-region' but for string or comment at point.\n\nBuffer must be fontified.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (n-back-game n-back) "n-back" "util/n-back.el"
;;;;;;  (19277 59684))
;;; Generated autoloads from util/n-back.el
(web-autoload-require 'n-back 'lp '(nxhtml-download-root-url nil) "util/n-back" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'n-back 'custom-loads))) (if (member '"n-back" loads) nil (put 'n-back 'custom-loads (cons '"n-back" loads))))

(nxhtml-autoload 'n-back-game `(lp '(nxhtml-download-root-url nil) "util/n-back" nxhtml-install-dir) "\
Emacs n-Back game.\nThis game is supposed to increase your working memory and fluid\nintelligence.\n\nIn this game something is shown for half a second on the screen\nand maybe a sound is played.  You should then answer if parts of\nit is the same as you have seen or heard before.  This is\nrepeated for about 20 trials.\n\nYou answer with the keys shown in the bottom window.\n\nIn the easiest version of the game you should answer if you have\njust seen or heard what is shown now.  By default the game gets\nharder as you play it with success.  Then first the number of\nitems presented in a trial grows.  After that it gets harder by\nthat you have to somehow remember not the last item, but the item\nbefore that (or even earlier). That is what \"n-Back\" stands\nfor.\n\nNote that remember does not really mean remember clearly.  The\ngame is for training your brain getting used to keep those things\nin the working memory, maybe as a cross-modal unit.  You are\nsupposed to just nearly be able to do what you do in the game.\nAnd you are supposed to have fun, that is what your brain like.\n\nYou should probably not overdue this. Half an hour a day playing\nmight be an optimal time according to some people.\n\nThe game is shamelessly modeled after Brain Workshop, see URL\n`http://brainworkshop.sourceforge.net/' just for the fun of\ngetting it into Emacs.  The game resembles but it not the same as\nthat used in the report by Jaeggi mentioned at the above URL.\n\nNot all features in Brain Workshop are implemented here, but some\nnew are maybe ... - and you have it available here in Emacs.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (nxhtmltest-run nxhtmltest-run-indent) "nxhtmltest-suites"
;;;;;;  "tests/nxhtmltest-suites.el" (19359 50232))
;;; Generated autoloads from tests/nxhtmltest-suites.el
(web-autoload-require 'nxhtmltest-suites 'lp '(nxhtml-download-root-url nil) "tests/nxhtmltest-suites" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtmltest-run-indent `(lp '(nxhtml-download-root-url nil) "tests/nxhtmltest-suites" nxhtml-install-dir) "\
Run indentation tests.\n\n(fn)" t nil)

(nxhtml-autoload 'nxhtmltest-run `(lp '(nxhtml-download-root-url nil) "tests/nxhtmltest-suites" nxhtml-install-dir) "\
Run all tests defined for nXhtml.\nCurrently there are only tests using ert.el defined.\n\nNote that it is currently expected that the following tests will\nfail (they corresponds to known errors in nXhtml/Emacs):\n\n  `nxhtml-ert-nxhtml-changes-jump-back-10549'\n  `nxhtml-ert-nxhtml-changes-jump-back-7014'\n\n(fn)" t nil)

;;;***

;;;### (autoloads (nxhtmltest-run-Q) "nxhtmltest-Q" "tests/nxhtmltest-Q.el"
;;;;;;  (19264 15086))
;;; Generated autoloads from tests/nxhtmltest-Q.el
(web-autoload-require 'nxhtmltest-Q 'lp '(nxhtml-download-root-url nil) "tests/nxhtmltest-Q" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtmltest-run-Q `(lp '(nxhtml-download-root-url nil) "tests/nxhtmltest-Q" nxhtml-install-dir) "\
Run all tests defined for nXhtml in fresh Emacs.\nSee `nxhtmltest-run' for more information about the tests.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (ert-run-tests-interactively ert-deftest) "ert"
;;;;;;  "tests/ert.el" (19173 34542))
;;; Generated autoloads from tests/ert.el
(web-autoload-require 'ert 'lp '(nxhtml-download-root-url nil) "tests/ert" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'ert-deftest `(lp '(nxhtml-download-root-url nil) "tests/ert" nxhtml-install-dir) "\
Define NAME (a symbol) as a test.\n\n(fn NAME () [:documentation DOCSTRING] [:expected-result TYPE] BODY...)" nil (quote macro))

(nxhtml-autoload 'ert-run-tests-interactively `(lp '(nxhtml-download-root-url nil) "tests/ert" nxhtml-install-dir) "\
Run the tests specified by SELECTOR and display the results in a buffer.\n\n(fn SELECTOR &optional OUTPUT-BUFFER-NAME MESSAGE-FN)" t nil)

;;;***

;;;### (autoloads (ocr-user-mode) "ocr-user" "util/ocr-user.el" (19290
;;;;;;  28))
;;; Generated autoloads from util/ocr-user.el
(web-autoload-require 'ocr-user 'lp '(nxhtml-download-root-url nil) "util/ocr-user" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'ocr-user-mode `(lp '(nxhtml-download-root-url nil) "util/ocr-user" nxhtml-install-dir) "\
Color up digits three by three.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ourcomments-warning ourcomments-M-x-menu-mode
;;;;;;  ourcomments-paste-with-convert-mode use-custom-style info-open-file
;;;;;;  replace-read-files rdir-query-replace ldir-query-replace
;;;;;;  grep-query-replace emacs-Q-nxhtml emacs-Q emacs--no-desktop
;;;;;;  emacs--debug-init emacs-buffer-file emacs emacs-restart ourcomments-ido-ctrl-tab
;;;;;;  ourcomments-ido-buffer-raise-frame ourcomments-ido-buffer-other-frame
;;;;;;  ourcomments-ido-buffer-other-window describe-symbol describe-defstruct
;;;;;;  describe-custom-group narrow-to-comment buffer-narrowed-p
;;;;;;  describe-command ourcomments-ediff-files find-emacs-other-file
;;;;;;  ourcomments-insert-date-and-time describe-timers ourcomments-copy+paste-set-point
;;;;;;  better-fringes-mode describe-key-and-map-briefly ourcomments-move-end-of-line
;;;;;;  ourcomments-move-beginning-of-line ourcomments-mark-whole-buffer-or-field
;;;;;;  fill-dwim unfill-individual-paragraphs unfill-region unfill-paragraph
;;;;;;  define-toggle-old define-toggle popup-menu-at-point ourcomments-indirect-fun)
;;;;;;  "ourcomments-util" "util/ourcomments-util.el" (19412 8766))
;;; Generated autoloads from util/ourcomments-util.el
(web-autoload-require 'ourcomments-util 'lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'ourcomments-indirect-fun `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Get the alias symbol for function FUN if any.\n\n(fn FUN)" nil nil)

(nxhtml-autoload 'popup-menu-at-point `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Popup the given menu at point.\nThis is similar to `popup-menu' and MENU and PREFIX has the same\nmeaning as there.  The position for the popup is however where\nthe window point is.\n\n(fn MENU &optional PREFIX)" nil nil)

(nxhtml-autoload 'define-toggle `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Declare SYMBOL as a customizable variable with a toggle function.\nThe purpose of this macro is to define a defcustom and a toggle\nfunction suitable for use in a menu.\n\nThe arguments have the same meaning as for `defcustom' with these\nrestrictions:\n\n- The :type keyword cannot be used.  Type is always 'boolean.\n- VALUE must be t or nil.\n\nDOC and ARGS are just passed to `defcustom'.\n\nA `defcustom' named SYMBOL with doc-string DOC and a function\nnamed SYMBOL-toggle is defined.  The function toggles the value\nof SYMBOL.  It takes no parameters.\n\nTo create a menu item something similar to this can be used:\n\n    (define-key map [SYMBOL]\n      (list 'menu-item \"Toggle nice SYMBOL\"\n            'SYMBOL-toggle\n            :button '(:toggle . SYMBOL)))\n\n(fn SYMBOL VALUE DOC &rest ARGS)" nil (quote macro))

(nxhtml-autoload 'define-toggle-old `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Not documented\n\n(fn SYMBOL VALUE DOC &rest ARGS)" nil (quote macro))

(nxhtml-autoload 'unfill-paragraph `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Unfill the current paragraph.\n\n(fn)" t nil)

(nxhtml-autoload 'unfill-region `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Unfill the current region.\n\n(fn)" t nil)

(nxhtml-autoload 'unfill-individual-paragraphs `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Unfill individual paragraphs in the current region.\n\n(fn)" t nil)

(nxhtml-autoload 'fill-dwim `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Fill or unfill paragraph or region.\nWith prefix ARG fill only current line.\n\n(fn ARG)" t nil)

(nxhtml-autoload 'ourcomments-mark-whole-buffer-or-field `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Mark whole buffer or editable field at point.\n\n(fn)" t nil)

(nxhtml-autoload 'ourcomments-move-beginning-of-line `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Move point to beginning of line or indentation.\nSee `beginning-of-line' for ARG.\n\nIf `line-move-visual' is non-nil then the visual line beginning\nis first tried.\n\nIf in a widget field stay in that.\n\n(fn ARG)" t nil)

(nxhtml-autoload 'ourcomments-move-end-of-line `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Move point to end of line or after last non blank char.\nSee `end-of-line' for ARG.\n\nSimilar to `ourcomments-move-beginning-of-line' but for end of\nline.\n\n(fn ARG)" t nil)

(nxhtml-autoload 'describe-key-and-map-briefly `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Try to print names of keymap from which KEY fetch its definition.\nLook in current active keymaps and find keymap variables with the\nsame value as the keymap where KEY is bound.  Print a message\nwith those keymap variable names.  Return a list with the keymap\nvariable symbols.\n\nWhen called interactively prompt for KEY.\n\nINSERT and UNTRANSLATED should normall be nil (and I am not sure\nwhat they will do ;-).\n\n(fn &optional KEY INSERT UNTRANSLATED)" t nil)

(defvar better-fringes-mode nil "\
Non-nil if Better-Fringes mode is enabled.\nSee the command `better-fringes-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `better-fringes-mode'.")

(nxhtml-custom-autoload 'better-fringes-mode 'ourcomments-util nil)

(nxhtml-autoload 'better-fringes-mode `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Choose another fringe bitmap color and bottom angle.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'ourcomments-copy+paste-set-point `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Set point for copy+paste here.\nEnable temporary minor mode `ourcomments-copy+paste-mode'.\nHowever if point for copy+paste already is set then cancel it and\ndisable the minor mode.\n\nThe purpose of this command is to make it easy to grab a piece of\ntext and paste it at current position.  After this command you\nshould select a piece of text to copy and then call the command\n`ourcomments-copy+paste'.\n\n(fn)" t nil)

(nxhtml-autoload 'describe-timers `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Show timers with readable time format.\n\n(fn)" t nil)

(nxhtml-autoload 'ourcomments-insert-date-and-time `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Insert date and time.\nSee option `ourcomments-insert-date-and-time' for how to\ncustomize it.\n\n(fn)" t nil)

(nxhtml-autoload 'find-emacs-other-file `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Find corresponding file to source or installed elisp file.\nIf you have checked out and compiled Emacs yourself you may have\nEmacs lisp files in two places, the checked out source tree and\nthe installed Emacs tree.  If buffer contains an Emacs elisp file\nin one of these places then find the corresponding elisp file in\nthe other place. Return the file name of this file.\n\nRename current buffer using your `uniquify-buffer-name-style' if\nit is set.\n\nWhen DISPLAY-FILE is non-nil display this file in other window\nand go to the same line number as in the current buffer.\n\n(fn DISPLAY-FILE)" t nil)

(nxhtml-autoload 'ourcomments-ediff-files `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
In directory DEF-DIR run `ediff-files' on files FILE-A and FILE-B.\nThe purpose of this function is to make it eaiser to start\n`ediff-files' from a shell through Emacs Client.\n\nThis is used in EmacsW32 in the file ediff.cmd where Emacs Client\nis called like this:\n\n  @%emacs_client% -e \"(setq default-directory \\\"%emacs_cd%\\\")\"\n  @%emacs_client% -n  -e \"(ediff-files \\\"%f1%\\\" \\\"%f2%\\\")\"\n\nIt can of course be done in a similar way with other shells.\n\n(fn DEF-DIR FILE-A FILE-B)" nil nil)

(nxhtml-autoload 'describe-command `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Like `describe-function', but prompts only for interactive commands.\n\n(fn COMMAND)" t nil)

(nxhtml-autoload 'buffer-narrowed-p `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Return non-nil if the current buffer is narrowed.\n\n(fn)" nil nil)

(nxhtml-autoload 'narrow-to-comment `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Not documented\n\n(fn)" t nil)

(nxhtml-autoload 'describe-custom-group `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Describe customization group SYMBOL.\n\n(fn SYMBOL)" t nil)

(nxhtml-autoload 'describe-defstruct `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Not documented\n\n(fn SYMBOL)" t nil)

(nxhtml-autoload 'describe-symbol `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Show information about SYMBOL.\nShow SYMBOL plist and whether is is a variable or/and a\nfunction.\n\n(fn SYMBOL)" t nil)

(nxhtml-autoload 'ourcomments-ido-buffer-other-window `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Show buffer in other window.\n\n(fn)" t nil)

(nxhtml-autoload 'ourcomments-ido-buffer-other-frame `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Show buffer in other frame.\n\n(fn)" t nil)

(nxhtml-autoload 'ourcomments-ido-buffer-raise-frame `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Raise frame showing buffer.\n\n(fn)" t nil)

(defvar ourcomments-ido-ctrl-tab nil "\
Non-nil if Ourcomments-Ido-Ctrl-Tab mode is enabled.\nSee the command `ourcomments-ido-ctrl-tab' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `ourcomments-ido-ctrl-tab'.")

(nxhtml-custom-autoload 'ourcomments-ido-ctrl-tab 'ourcomments-util nil)

(nxhtml-autoload 'ourcomments-ido-ctrl-tab `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Enable buffer switching using C-Tab with function `ido-mode'.\nThis changes buffer switching with function `ido-mode' the\nfollowing way:\n\n- You can use C-Tab.\n\n- You can show the selected buffer in three ways independent of\n  how you entered function `ido-mode' buffer switching:\n\n  * S-return: other window\n  * C-return: other frame\n  * M-return: raise frame\n\nThose keys are selected to at least be a little bit reminiscent\nof those in for example common web browsers.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'emacs-restart `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Restart Emacs and start `server-mode' if on before.\n\n(fn)" t nil)

(nxhtml-autoload 'emacs `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start a new Emacs with default parameters.\nAdditional ARGS are passed to the new Emacs.\n\nSee also `ourcomments-started-emacs-use-output-buffer'.\n\n(fn &rest ARGS)" t nil)

(nxhtml-autoload 'emacs-buffer-file `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start a new Emacs showing current buffer file.\nGo to the current line and column in that file.\nIf there is no buffer file then instead start with `dired'.\n\nThis calls the function `emacs' with argument --no-desktop and\nthe file or a call to dired.\n\n(fn)" t nil)

(nxhtml-autoload 'emacs--debug-init `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start a new Emacs with --debug-init parameter.\nThis calls the function `emacs' with added arguments ARGS.\n\n(fn &rest ARGS)" t nil)

(nxhtml-autoload 'emacs--no-desktop `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start a new Emacs with --no-desktop parameter.\nThis calls the function `emacs' with added arguments ARGS.\n\n(fn &rest ARGS)" t nil)

(nxhtml-autoload 'emacs-Q `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start a new Emacs with -Q parameter.\nStart new Emacs without any customization whatsoever.\nThis calls the function `emacs' with added arguments ARGS.\n\n(fn &rest ARGS)" t nil)

(nxhtml-autoload 'emacs-Q-nxhtml `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start new Emacs with -Q and load nXhtml.\nThis calls the function `emacs' with added arguments ARGS.\n\n(fn &rest ARGS)" t nil)

(nxhtml-autoload 'grep-query-replace `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Do `query-replace-regexp' of FROM with TO, on all files in *grep*.\nThird arg DELIMITED (prefix arg) means replace only word-delimited matches.\nIf you exit (\\[keyboard-quit], RET or q), you can resume the query replace\nwith the command \\[tags-loop-continue].\n\n(fn FROM TO &optional DELIMITED)" t nil)

(nxhtml-autoload 'ldir-query-replace `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Replace FROM with TO in FILES in directory DIR.\nThis runs `query-replace-regexp' in files matching FILES in\ndirectory DIR.\n\nSee `tags-query-replace' for DELIMETED and more information.\n\n(fn FROM TO FILES DIR &optional DELIMITED)" t nil)

(nxhtml-autoload 'rdir-query-replace `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Replace FROM with TO in FILES in directory tree ROOT.\nThis runs `query-replace-regexp' in files matching FILES in\ndirectory tree ROOT.\n\nSee `tags-query-replace' for DELIMETED and more information.\n\n(fn FROM TO FILE-REGEXP ROOT &optional DELIMITED)" t nil)

(nxhtml-autoload 'replace-read-files `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Read files arg for replace.\n\n(fn REGEXP &optional REPLACE)" nil nil)

(nxhtml-autoload 'info-open-file `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Open an info file in `Info-mode'.\n\n(fn INFO-FILE)" t nil)

(nxhtml-autoload 'use-custom-style `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Setup like in `Custom-mode', but without things specific to Custom.\n\n(fn)" nil nil)

(defvar ourcomments-paste-with-convert-mode nil "\
Non-nil if Ourcomments-Paste-With-Convert mode is enabled.\nSee the command `ourcomments-paste-with-convert-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `ourcomments-paste-with-convert-mode'.")

(nxhtml-custom-autoload 'ourcomments-paste-with-convert-mode 'ourcomments-util nil)

(nxhtml-autoload 'ourcomments-paste-with-convert-mode `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Pasted text may be automatically converted in this mode.\nThe functions in `ourcomments-paste-with-convert-hook' are run\nafter commands in `ourcomments-paste-with-convert-commands' if any\nof the functions returns non-nil that text is inserted instead of\nthe original text.\n\nFor exampel when this mode is on and you paste an html link in an\n`org-mode' buffer it will be directly converted to an org style\nlink. (This is the default behaviour.)\n\nTip: The Firefox plugin Copy as HTML Link is handy, see URL\n     `https://addons.mozilla.org/en-US/firefox/addon/2617'.\n\nNote: This minor mode will defadvice the paste commands.\n\n(fn &optional ARG)" t nil)

(defvar ourcomments-M-x-menu-mode nil "\
Non-nil if Ourcomments-M-X-Menu mode is enabled.\nSee the command `ourcomments-M-x-menu-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `ourcomments-M-x-menu-mode'.")

(nxhtml-custom-autoload 'ourcomments-M-x-menu-mode 'ourcomments-util nil)

(nxhtml-autoload 'ourcomments-M-x-menu-mode `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Add commands started from Emacs menus to M-x history.\nThe purpose of this is to make it easier to redo them and easier\nto learn how to do them from the command line (which is often\nfaster if you know how to do it).\n\nOnly commands that are not already in M-x history are added.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'ourcomments-warning `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Not documented\n\n(fn FORMAT-STRING &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (major-modep major-or-multi-majorp) "ourcomments-widgets"
;;;;;;  "util/ourcomments-widgets.el" (19275 41782))
;;; Generated autoloads from util/ourcomments-widgets.el
(web-autoload-require 'ourcomments-widgets 'lp '(nxhtml-download-root-url nil) "util/ourcomments-widgets" nxhtml-install-dir 'nxhtml-byte-compile-file)

 (nxhtml-autoload 'command "ourcomments-widgets")

(nxhtml-autoload 'major-or-multi-majorp `(lp '(nxhtml-download-root-url nil) "util/ourcomments-widgets" nxhtml-install-dir) "\
Return t if VALUE is a major or multi major mode function.\n\n(fn VALUE)" nil nil)

(nxhtml-autoload 'major-modep `(lp '(nxhtml-download-root-url nil) "util/ourcomments-widgets" nxhtml-install-dir) "\
Return t if VALUE is a major mode function.\n\n(fn VALUE)" nil nil)
 (nxhtml-autoload 'major-mode-function "ourcomments-widgets")

;;;***

;;;### (autoloads (pause-start-in-new-emacs pause-mode pause) "pause"
;;;;;;  "util/pause.el" (19335 37324))
;;; Generated autoloads from util/pause.el
(web-autoload-require 'pause 'lp '(nxhtml-download-root-url nil) "util/pause" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'pause 'custom-loads))) (if (member '"pause" loads) nil (put 'pause 'custom-loads (cons '"pause" loads))))

(defvar pause-mode nil "\
Non-nil if Pause mode is enabled.\nSee the command `pause-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `pause-mode'.")

(nxhtml-custom-autoload 'pause-mode 'pause nil)

(nxhtml-autoload 'pause-mode `(lp '(nxhtml-download-root-url nil) "util/pause" nxhtml-install-dir) "\
This minor mode tries to make you take a break.\nIt will jump up and temporary stop your work - even if you are\nnot in Emacs.  If you are in Emacs it will however try to be\ngentle and wait until you have been idle with the keyboard for a\nshort while. (If you are not in Emacs it can't be gentle. How\ncould it?)\n\nThen it will show you a special screen with a link to a yoga\nexercise you can do when you pause.\n\nAfter the pause you continue your work where you were\ninterrupted.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'pause-start-in-new-emacs `(lp '(nxhtml-download-root-url nil) "util/pause" nxhtml-install-dir) "\
Start pause with interval AFTER-MINUTES in a new Emacs instance.\nThe new Emacs instance will be started with -Q.  However if\n`custom-file' is non-nil it will be loaded so you can still\ncustomize pause.\n\nOne way of using this function may be to put in your .emacs\nsomething like\n\n  ;; for just one Emacs running pause\n  (when server-mode (pause-start-in-new-emacs 15))\n\nSee `pause-start' for more info.\n\n(fn AFTER-MINUTES)" t nil)

;;;***

;;;### (autoloads (global-pointback-mode pointback-mode) "pointback"
;;;;;;  "util/pointback.el" (19023 25498))
;;; Generated autoloads from util/pointback.el
(web-autoload-require 'pointback 'lp '(nxhtml-download-root-url nil) "util/pointback" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'pointback-mode `(lp '(nxhtml-download-root-url nil) "util/pointback" nxhtml-install-dir) "\
Restore previous window point when switching back to a buffer.\n\n(fn &optional ARG)" t nil)

(defvar global-pointback-mode nil "\
Non-nil if Global-Pointback mode is enabled.\nSee the command `global-pointback-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-pointback-mode'.")

(nxhtml-custom-autoload 'global-pointback-mode 'pointback nil)

(nxhtml-autoload 'global-pointback-mode `(lp '(nxhtml-download-root-url nil) "util/pointback" nxhtml-install-dir) "\
Toggle Pointback mode in every possible buffer.\nWith prefix ARG, turn Global-Pointback mode on if and only if ARG is positive.\nPointback mode is enabled in all buffers where `pointback-on' would do it.\nSee `pointback-mode' for more information on Pointback mode.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (popcmp-completing-read popcmp-completion-style
;;;;;;  popcmp) "popcmp" "util/popcmp.el" (19365 12162))
;;; Generated autoloads from util/popcmp.el
(web-autoload-require 'popcmp 'lp '(nxhtml-download-root-url nil) "util/popcmp" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'popcmp 'custom-loads))) (if (member '"popcmp" loads) nil (put 'popcmp 'custom-loads (cons '"popcmp" loads))))

(defvar popcmp-completion-style (cond (t 'popcmp-popup)) "\
Completion style.\nThe currently available completion styles are:\n\n- popcmp-popup: Use OS popup menus (default).\n- emacs-default: Emacs default completion.\n- Company Mode completion.\n- anything: The Anything elisp lib completion style.\n\nThe style of completion set here is not implemented for all\ncompletions.  The scope varies however with which completion\nstyle you have choosen.\n\nFor information about Company Mode and how to use it see URL\n`http://www.emacswiki.org/emacs/CompanyMode'.\n\nFor information about Anything and how to use it see URL\n`http://www.emacswiki.org/emacs/Anything'.\n\nSee also the options `popcmp-short-help-beside-alts' and\n`popcmp-group-alternatives' which are also availabe when popup\ncompletion is available.")

(nxhtml-custom-autoload 'popcmp-completion-style 'popcmp nil)

(nxhtml-autoload 'popcmp-completing-read `(lp '(nxhtml-download-root-url nil) "util/popcmp" nxhtml-install-dir) "\
Read a string in the minubuffer with completion, or popup a menu.\nThis function can be used instead `completing-read'. The main\npurpose is to provide a popup style menu for completion when\ncompletion is tighed to text at point in a buffer. If a popup\nmenu is used it will be shown at window point. Whether a popup\nmenu or minibuffer completion is used is governed by\n`popcmp-completion-style'.\n\nThe variables PROMPT, TABLE, PREDICATE, REQUIRE-MATCH,\nINITIAL-INPUT, POP-HIST, DEF and INHERIT-INPUT-METHOD all have the\nsame meaning is for `completing-read'.\n\nALT-HELP should be nil or a hash variable or an association list\nwith the completion alternative as key and a short help text as\nvalue.  You do not need to supply help text for all alternatives.\nThe use of ALT-HELP is set by `popcmp-short-help-beside-alts'.\n\nALT-SETS should be nil or an association list that has as keys\ngroups and as second element an alternative that should go into\nthis group.\n\n(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT POP-HIST DEF INHERIT-INPUT-METHOD ALT-HELP ALT-SETS)" nil nil)

;;;***

;;;### (autoloads (rebind-keys-mode rebind) "rebind" "util/rebind.el"
;;;;;;  (19291 55616))
;;; Generated autoloads from util/rebind.el
(web-autoload-require 'rebind 'lp '(nxhtml-download-root-url nil) "util/rebind" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'rebind 'custom-loads))) (if (member '"rebind" loads) nil (put 'rebind 'custom-loads (cons '"rebind" loads))))

(defvar rebind-keys-mode nil "\
Non-nil if Rebind-Keys mode is enabled.\nSee the command `rebind-keys-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `rebind-keys-mode'.")

(nxhtml-custom-autoload 'rebind-keys-mode 'rebind nil)

(nxhtml-autoload 'rebind-keys-mode `(lp '(nxhtml-download-root-url nil) "util/rebind" nxhtml-install-dir) "\
Rebind keys as defined in `rebind-keys'.\nThe key bindings will override almost all other key bindings\nsince it is put on emulation level, like for example ``cua-mode'\nand `viper-mode'.\n\nThis is for using for example C-a to mark the whole buffer (or a\nfield). There are some predifined keybindings for this.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rnc-mode) "rnc-mode" "util/rnc-mode.el" (18775
;;;;;;  38406))
;;; Generated autoloads from util/rnc-mode.el
(web-autoload-require 'rnc-mode 'lp '(nxhtml-download-root-url nil) "util/rnc-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'rnc-mode `(lp '(nxhtml-download-root-url nil) "util/rnc-mode" nxhtml-install-dir) "\
Major mode for editing RELAX NG Compact Syntax schemas.\n\\{rnc-mode-map}\n\n(fn)" t nil)

;;;***

;;;### (autoloads (search-form) "search-form" "util/search-form.el"
;;;;;;  (19275 41782))
;;; Generated autoloads from util/search-form.el
(web-autoload-require 'search-form 'lp '(nxhtml-download-root-url nil) "util/search-form" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'search-form `(lp '(nxhtml-download-root-url nil) "util/search-form" nxhtml-install-dir) "\
Display a form for search and replace.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (sex-mode sex) "sex-mode" "util/sex-mode.el" (19218
;;;;;;  20584))
;;; Generated autoloads from util/sex-mode.el
(web-autoload-require 'sex-mode 'lp '(nxhtml-download-root-url nil) "util/sex-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'sex 'custom-loads))) (if (member '"sex-mode" loads) nil (put 'sex 'custom-loads (cons '"sex-mode" loads))))

(defvar sex-mode nil "\
Non-nil if Sex mode is enabled.\nSee the command `sex-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `sex-mode'.")

(nxhtml-custom-autoload 'sex-mode 'sex-mode nil)

(nxhtml-autoload 'sex-mode `(lp '(nxhtml-download-root-url nil) "util/sex-mode" nxhtml-install-dir) "\
Open certain files in external programs.\nSee `sex-get-file-open-cmd' for how to determine which files to\nopen by external applications.  Note that this selection is\nnearly the same as in `org-mode'.  The main difference is that\nthe fallback always is to open a file in Emacs. (This is\nnecessary to avoid to disturb many of Emacs operations.)\n\nThis affects all functions that opens files, like `find-file',\n`find-file-noselect' etc.\n\nHowever it does not affect files opened through Emacs client.\n\nUrls can also be handled, see `sex-handle-urls'.\n\nWhen opening a file with the shell a (temporary) dummy buffer is\ncreated in Emacs with major mode `sex-file-mode' and an external\nprogram is called to handle the file.  How this dummy buffer is\nhandled is governed by `sex-keep-dummy-buffer'.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (sml-modeline-mode sml-modeline) "sml-modeline"
;;;;;;  "util/sml-modeline.el" (19362 27488))
;;; Generated autoloads from util/sml-modeline.el
(web-autoload-require 'sml-modeline 'lp '(nxhtml-download-root-url nil) "util/sml-modeline" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'sml-modeline 'custom-loads))) (if (member '"sml-modeline" loads) nil (put 'sml-modeline 'custom-loads (cons '"sml-modeline" loads))))

(defvar sml-modeline-mode nil "\
Non-nil if Sml-Modeline mode is enabled.\nSee the command `sml-modeline-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `sml-modeline-mode'.")

(nxhtml-custom-autoload 'sml-modeline-mode 'sml-modeline nil)

(nxhtml-autoload 'sml-modeline-mode `(lp '(nxhtml-download-root-url nil) "util/sml-modeline" nxhtml-install-dir) "\
Show buffer size and position like scrollbar in mode line.\nYou can customize this minor mode, see option `sml-modeline-mode'.\n\nNote: If you turn this mode on then you probably want to turn off\noption `scroll-bar-mode'.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (tabkey2-emma-without-tabkey2 tabkey2-mode tabkey2)
;;;;;;  "tabkey2" "util/tabkey2.el" (19277 43758))
;;; Generated autoloads from util/tabkey2.el
(web-autoload-require 'tabkey2 'lp '(nxhtml-download-root-url nil) "util/tabkey2" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'tabkey2 'custom-loads))) (if (member '"tabkey2" loads) nil (put 'tabkey2 'custom-loads (cons '"tabkey2" loads))))

(defvar tabkey2-mode nil "\
Non-nil if Tabkey2 mode is enabled.\nSee the command `tabkey2-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `tabkey2-mode'.")

(nxhtml-custom-autoload 'tabkey2-mode 'tabkey2 nil)

(nxhtml-autoload 'tabkey2-mode `(lp '(nxhtml-download-root-url nil) "util/tabkey2" nxhtml-install-dir) "\
More fun with Tab key number two (completion etc).\nThis global minor mode by default binds Tab in a way that let you\ndo completion with Tab in all buffers (where it is possible).\n\nThe Tab key is easy to type on your keyboard.  Then why not use\nit for completion, something that is very useful?  Shells usually\nuse Tab for completion so many are used to it.  This was the idea\nof Smart Tabs and this is a generalization of that idea.\n\nHowever in Emacs the Tab key is usually used for indentation.\nThe idea here is that if Tab has been pressed once for\nindentation, then as long as point stays further Tab keys might\nas well do completion.\n\nSo you kind of do Tab-Tab for first completion (and then just\nTab for further completions as long as point is not moved).\n\nAnd there is even kind of Tab-Tab-Tab completion: If completion\nfails the next completion function will be the one you try with\nnext Tab. (You get some notification of this, of course.)\n\nSee `tabkey2-first' for more information about usage.\n\nNote: If you do not want the Tab-Tab behaviour above, but still\nwant an easy way to reach the available completion functions,\nthen you can instead of turning on tabkey2-mode enter this in\nyour .emacs:\n\n (global-set-key [f8] 'tabkey2-cycle-completion-functions)\n\nAfter hitting f8 you will then be in the same state as after the\nfirst in tabkey2-mode.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'tabkey2-emma-without-tabkey2 `(lp '(nxhtml-download-root-url nil) "util/tabkey2" nxhtml-install-dir) "\
Not documented\n\n(fn)" nil nil)

;;;***

;;;### (autoloads (tyda-mode) "tyda" "util/tyda.el" (19275 41782))
;;; Generated autoloads from util/tyda.el
(web-autoload-require 'tyda 'lp '(nxhtml-download-root-url nil) "util/tyda" nxhtml-install-dir 'nxhtml-byte-compile-file)


(defvar tyda-mode nil "\
Non-nil if Tyda mode is enabled.\nSee the command `tyda-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `tyda-mode'.")

(nxhtml-custom-autoload 'tyda-mode 'tyda nil)

(nxhtml-autoload 'tyda-mode `(lp '(nxhtml-download-root-url nil) "util/tyda" nxhtml-install-dir) "\
Minor mode for key bindings for `tyda-lookup-word'.\nIt binds Alt-Mouse-1 just as the Tyda add-on does in Firefox.\nHere are all key bindings\n\n\\{tyda-mode-map}\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (udev-call-first-step) "udev" "util/udev.el" (19412
;;;;;;  8766))
;;; Generated autoloads from util/udev.el
(web-autoload-require 'udev 'lp '(nxhtml-download-root-url nil) "util/udev" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'udev-call-first-step `(lp '(nxhtml-download-root-url nil) "util/udev" nxhtml-install-dir) "\
Set up and call first step.\nSet up buffer LOG-BUFFER to be used for log messages and\ncontroling of the execution of the functions in list STEPS which\nare executed one after another.\n\nWrite HEADER at the end of LOG-BUFFER.\n\nCall first step.\n\nIf FINISH-FUN non-nil it should be a function. This is called\nafter last step with LOG-BUFFER as parameter.\n\n(fn LOG-BUFFER STEPS HEADER FINISH-FUN)" nil nil)

;;;***

;;;### (autoloads (udev-ecb-customize-startup udev-ecb-update) "udev-ecb"
;;;;;;  "util/udev-ecb.el" (19255 49348))
;;; Generated autoloads from util/udev-ecb.el
(web-autoload-require 'udev-ecb 'lp '(nxhtml-download-root-url nil) "util/udev-ecb" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'udev-ecb-update `(lp '(nxhtml-download-root-url nil) "util/udev-ecb" nxhtml-install-dir) "\
Fetch and install ECB from the devel sources.\nTo determine where to store the sources see `udev-ecb-dir'.\nFor how to start ECB see `udev-ecb-load-ecb'.\n\n(fn)" t nil)

(nxhtml-autoload 'udev-ecb-customize-startup `(lp '(nxhtml-download-root-url nil) "util/udev-ecb" nxhtml-install-dir) "\
Customize ECB dev nXhtml startup group.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (udev-rinari-update) "udev-rinari" "util/udev-rinari.el"
;;;;;;  (19255 49348))
;;; Generated autoloads from util/udev-rinari.el
(web-autoload-require 'udev-rinari 'lp '(nxhtml-download-root-url nil) "util/udev-rinari" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'udev-rinari-update `(lp '(nxhtml-download-root-url nil) "util/udev-rinari" nxhtml-install-dir) "\
Fetch and install Rinari from the devel sources.\nTo determine where to store the sources and how to start rinari\nsee `udev-rinari-dir' and `udev-rinari-load-rinari'.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (viper-tutorial) "viper-tut" "util/viper-tut.el"
;;;;;;  (19388 57730))
;;; Generated autoloads from util/viper-tut.el
(web-autoload-require 'viper-tut 'lp '(nxhtml-download-root-url nil) "util/viper-tut" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'viper-tutorial `(lp '(nxhtml-download-root-url nil) "util/viper-tut" nxhtml-install-dir) "\
Run a tutorial for Viper.\n\nA simple classic tutorial in 5 parts that have been used by many\npeople starting to learn vi keys.  You may learn enough to start\nusing `viper-mode' in Emacs.\n\nSome people find that vi keys helps against repetetive strain\ninjury, see URL\n\n  `http://www.emacswiki.org/emacs/RepeatedStrainInjury'.\n\nNote: There might be a few clashes between vi key binding and\nEmacs standard key bindings.  You will be notified about those in\nthe tutorial.  Even more, if your own key bindings comes in\nbetween you will be notified about that too.\n\n(fn PART &optional DONT-ASK-FOR-REVERT)" t nil)

;;;***

;;;### (autoloads (vline-global-mode vline-mode) "vline" "util/vline.el"
;;;;;;  (19156 46106))
;;; Generated autoloads from util/vline.el
(web-autoload-require 'vline 'lp '(nxhtml-download-root-url nil) "util/vline" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'vline-mode `(lp '(nxhtml-download-root-url nil) "util/vline" nxhtml-install-dir) "\
Display vertical line mode.\n\n(fn &optional ARG)" t nil)

(defvar vline-global-mode nil "\
Non-nil if Vline-Global mode is enabled.\nSee the command `vline-global-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `vline-global-mode'.")

(nxhtml-custom-autoload 'vline-global-mode 'vline nil)

(nxhtml-autoload 'vline-global-mode `(lp '(nxhtml-download-root-url nil) "util/vline" nxhtml-install-dir) "\
Display vertical line mode as globally.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (whelp) "whelp" "util/whelp.el" (19277 43758))
;;; Generated autoloads from util/whelp.el
(web-autoload-require 'whelp 'lp '(nxhtml-download-root-url nil) "util/whelp" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'whelp 'custom-loads))) (if (member '"whelp" loads) nil (put 'whelp 'custom-loads (cons '"whelp" loads))))

;;;***

;;;### (autoloads (wikipedia-draft-buffer wikipedia-draft-page wikipedia-draft
;;;;;;  wikipedia-mode) "wikipedia-mode" "related/wikipedia-mode.el"
;;;;;;  (19277 43758))
;;; Generated autoloads from related/wikipedia-mode.el
(web-autoload-require 'wikipedia-mode 'lp '(nxhtml-download-root-url nil) "related/wikipedia-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'wikipedia-mode `(lp '(nxhtml-download-root-url nil) "related/wikipedia-mode" nxhtml-install-dir) "\
Major mode for editing wikimedia style wikis.\nMajor mode for editing articles written in the markup language\nused by Wikipedia, the free on-line\nencyclopedia (see URL `http://www.wikipedia.org').\n\nThere are several ways to use wikipedia-mode:\n\n- You can simply cut and paste articles between Emacs and your\n  web browser's text box.\n- If you are using Firefox you can use the It's All Text add-on\n  for Firefox.\n- You can use MozEx, a Mozilla/Firefox web browser extension that\n  allows you to call Emacs from a text\n  box (see URL `http://mozex.mozdev.org/').\n- Another way is to use the PERL script ee-helper, which allows\n  you to up and download wiki texts.\n\nWikipedia articles are usually unfilled: newline characters are not\nused for breaking paragraphs into lines. Unfortunately, Emacs does not\nhandle word wrapping yet. As a workaround, wikipedia-mode turns on\nlonglines-mode automatically. In case something goes wrong, the\nfollowing commands may come in handy:\n\n\\[wikipedia-fill-article] fills the buffer.\n\\[wikipedia-unfill-article] unfills the buffer.\nBe warned that function can be dead  slow, better use wikipedia-unfill-paragraph-or-region.\n\\[wikipedia-unfill-paragraph-or-region] unfills the paragraph\n\\[wikipedia-unfill-paragraph-simple] doehe same but simpler.\n\n\n\nThe following commands put in markup structures.\n\n\\[wikipedia-insert-bold-italic] bold+italic\n\\[wikipedia-insert-bold] bold text\n\\[wikipedia-insert-italics] italics\n\\[wikipedia-insert-nowiki] no wiki markup\n\\[wikipedia-insert-link-wiki] inserts a link\n\nThe following commands are also defined:\n\\[wikipedia-insert-user] inserts user name\n\\[wikipedia-insert-signature] inserts ~~~~\n\\[wikipedia-insert-enumerate] inserts enumerate type structures\n\\[wikipedia-insert-itemize] inserts itemize type structures\n\\[wikipedia-insert-hline] inserts a hline\n\nThe draft functionality\n\\[wikipedia-draft]\n\\[wikipedia-draft-region]\n\\[wikipedia-draft-view-draft]\n\\[wikipedia-draft-page]\n\\[wikipedia-draft-buffer]\n\nReplying and sending functionality\n\\[wikipedia-reply-at-point-simple]\n\\[wikipedia-draft-reply]\n\n\nThe register functionality\n\\[wikipedia-copy-page-to-register]\n\\[defun wikipedia-insert-page-to-register]\n\n\nSome simple editing commands.\n\\[wikipedia-enhance-indent]\n\\[wikipedia-yank-prefix]\n\\[wikipedia-unfill-paragraph-or-region]\n\n\n\n\\[wikipedia-terminate-paragraph]     starts a new list item or paragraph in a context-aware manner.\n\n(fn)" t nil)

(nxhtml-autoload 'wikipedia-draft `(lp '(nxhtml-download-root-url nil) "related/wikipedia-mode" nxhtml-install-dir) "\
Open a temporary buffer in wikipedia mode for editing an\n wikipedia draft, which an arbitrary piece of data. After\n finishing the editing either use \\[wikipedia-draft-buffer] to\n send the data into the wikipedia-draft-data-file, or send the\n buffer using `wikipedia-draft-send-to-mozex' and insert it later\n into a wikipedia article.\n\n(fn)" t nil)

(nxhtml-autoload 'wikipedia-draft-page `(lp '(nxhtml-download-root-url nil) "related/wikipedia-mode" nxhtml-install-dir) "\
Not documented\n\n(fn)" t nil)

(nxhtml-autoload 'wikipedia-draft-buffer `(lp '(nxhtml-download-root-url nil) "related/wikipedia-mode" nxhtml-install-dir) "\
Wikipedia-draft-buffer sends the contents of the current (temporary)\nbuffer to the wikipedia-draft-buffer, see the variable\nwikipedia-draft-data-file.\n\n(fn)" t nil)

(defvar wikipedia-draft-send-archive t "\
*Archive the reply.")

;;;***

;;;### (autoloads (visual-basic-mode) "visual-basic-mode" "related/visual-basic-mode.el"
;;;;;;  (19234 45588))
;;; Generated autoloads from related/visual-basic-mode.el
(web-autoload-require 'visual-basic-mode 'lp '(nxhtml-download-root-url nil) "related/visual-basic-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'visual-basic-mode `(lp '(nxhtml-download-root-url nil) "related/visual-basic-mode" nxhtml-install-dir) "\
A mode for editing Microsoft Visual Basic programs.\nFeatures automatic indentation, font locking, keyword capitalization,\nand some minor convenience functions.\nCommands:\n\\{visual-basic-mode-map}\n\n(fn)" t nil)

;;;***

;;;### (autoloads (tt-mode) "tt-mode" "related/tt-mode.el" (18602
;;;;;;  59730))
;;; Generated autoloads from related/tt-mode.el
(web-autoload-require 'tt-mode 'lp '(nxhtml-download-root-url nil) "related/tt-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'tt-mode `(lp '(nxhtml-download-root-url nil) "related/tt-mode" nxhtml-install-dir) "\
Major mode for editing Template Toolkit files.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (smarty-mode smarty) "smarty-mode" "related/smarty-mode.el"
;;;;;;  (19234 45588))
;;; Generated autoloads from related/smarty-mode.el
(web-autoload-require 'smarty-mode 'lp '(nxhtml-download-root-url nil) "related/smarty-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'smarty 'custom-loads))) (if (member '"smarty-mode" loads) nil (put 'smarty 'custom-loads (cons '"smarty-mode" loads))))

(nxhtml-autoload 'smarty-mode `(lp '(nxhtml-download-root-url nil) "related/smarty-mode" nxhtml-install-dir) "\
Smarty Mode\n***********\n\nSmarty Mode is a GNU XEmacs major mode for editing Smarty templates.\n\n1 Introduction\n**************\n\nSmarty-Mode is a mode allowing easy edit of Smarty templates:\nhighlight, templates, navigation into source files...\n\n\n\nFeatures (new features in bold) :\n\n   * Completion\n\n   * Customizable\n\n   * Highlight\n\n   * Menu\n\n   * Stuttering\n\n   * Templates\n        - Built-in Functions\n\n        - User Functions\n\n        - Variable Modifiers\n\n        - Plugin (Functions)\n             * BlockRepeatPlugin\n\n             * ClipCache\n\n             * Smarty Formtool\n\n             * Smarty Paginate\n\n             * Smarty Validate\n\n        - Plugin (Variable Modifiers)\n             * AlternativeDateModifierPlugin\n\n             * B2Smilies\n\n             * BBCodePlugin\n\n        - Fonctions Non-Smarty\n\n\n\nThis manual describes Smarty Mode version 0.0.5.\n\n2 Installation\n**************\n\n2.1 Requirements\n================\n\nSmarty Mode is a XEmacs major mode that needs the following\nsoftware/packages:\n\n   * XEmacs (http://www.xemacs.org/).\n\n   * `font-lock' mode generaly installed with XEmacs.\n\n   * `assoc' mode generaly installed with XEmacs.\n\n   * `easymenu' mode generaly installed with XEmacs.\n\n   * `hippie-exp' mode generaly installed with XEmacs.\n\nBefore continuing, you must be sure to have all this packages\ninstalled.\n\n2.2 Download\n============\n\nTwo internet address to download Smarty Mode :\n\n   * Principal: Smarty-Mode 0.0.5\n     (http://deboutv.free.fr/lisp/smarty/download/smarty-0.0.5.tar.gz)\n     (http://deboutv.free.fr/lisp/smarty/)\n\n   * Secondary: Smarty-Mode 0.0.5\n     (http://www.morinie.fr/lisp/smarty/download/smarty-0.0.5.tar.gz)\n     (http://www.morinie.fr/lisp/smarty/)\n\n   * Old releases: Smarty-Mode\n     (http://deboutv.free.fr/lisp/smarty/download.php)\n     (http://deboutv.free.fr/lisp/smarty/)\n\n2.3 Installation\n================\n\n2.3.1 Installation\n------------------\n\nTo install Smarty Mode you need to choose an installation directory\n(for example `/usr/local/share/lisp' or `c:lisp'). The administrator\nmust have the write rights on this directory.\n\nWith your favorite unzip software, unzip the archive in the\ninstallation directory.\n\nExample:\n     cd /usr/local/share/lisp\n     tar zxvf smarty-0.0.5.tar.gz\nNow you have a `smarty' directory in the installation directory. This\ndirectory contains 2 files `smarty-mode.el' and `smarty-mode.elc' and\nanother directory `docs' containing the documentation.\n\nYou need to configure XEmacs. open you initialization file `init.el'\n(open the file or start XEmacs then choose the Options menu and Edit\nInit File). Add the following lines (the installation directory in\nthis example is `/usr/local/share/lisp') :\n\n     (setq load-path\n           (append (list \"/usr/local/share/lisp/\") load-path))\n     (nxhtml-autoload 'smarty-mode \"smarty-mode\" \"Smarty Mode\" t)\n\n2.3.2 Update\n------------\n\nThe update is easy. You need to unzip the archive in the installation\ndirectory to remove the old release.\n\nExample:\n     cd /usr/local/share/lisp\n     rm -rf smarty\n     tar zxvf smarty-0.0.5.tar.gz\n\n2.4 Invoke Smarty-Mode\n======================\n\nYou have two possibilities to invoke the Smarty Mode.\n\n   - Manually: At each file opening you need to launch Smarty Mode\n     with the following command:\n\n     `M-x smarty-mode'\n\n   - Automatically: Add the following linesin your initialization\n     file `init.el' :\n\n          (setq auto-mode-alist\n                (append\n                 '((\"\\.tpl$\" . smarty-mode))\n          	 auto-mode-alist))\n\n\n3 Customization\n***************\n\nThis chapter describes the differents parameters and functions that\nyou can change to customize Smarty Mode.  To do that, open a Smarty\nfile, click on the Smarty menu and choose Options then Browse\nOptions....\n\n3.1 Parameters\n==============\n\n3.1.1 Mode\n----------\n\nSmarty Mode has 2 modes allowing to simplify the writing of Smarty\ntemplates. You can enable/disable each mode individually.\n\n`smarty-electric-mode'\n     Type: boolean\n     Default value: `t'\n     Description: If `t'; enable automatic generation of template.\n     If `nil'; template generators can still be invoked through key\n     bindings and menu. Is indicated in the modeline by \"/e\" after\n     the mode name and can be toggled by `smarty-electric-mode'.\n\n`smarty-stutter-mode'\n     Type: boolean\n     Default value: `t'\n     Description: If `t'; enable the stuttering. Is indicated in the\n     modeline by \"/s\" after the mode name and can be toggled by\n     `smarty-stutter-mode'.\n\n3.1.2 Menu\n----------\n\nSmarty Mode has also 1 menu that you can enable/disable. The menu\nSources is specific to each Smarty files opened.\n\n`smarty-source-file-menu'\n     Type: boolean\n     Default value: `t'\n     Description: If `t'; the Sources menu is enabled. This menu\n     contains the list of Smarty file located in the current\n     directory. The Sources menu scans the directory when a file is\n     opened.\n\n3.1.3 Menu\n----------\n\n`smarty-highlight-plugin-functions'\n     Type: boolean\n     Default value: `t'\n     Description: If `t'; the functions described in the smarty\n     plugins are highlighted.\n\n3.1.4 Templates\n---------------\n\n3.1.4.1 Header\n..............\n\n`smarty-file-header'\n     Type: string\n     Default value: `\"\"'\n     Description: String or file to insert as file header. If the\n     string specifies an existing file name the contents of the file\n     is inserted; otherwise the string itself is inserted as file\n     header.\n     Type `C-j' for newlines.\n     The follonwing keywords are supported:\n     <filename>: replaced by the file name.\n     <author>: replaced by the user name and email address.\n     <login>: replaced by `user-login-name'.\n     <company>: replaced by `smarty-company-name' content.\n     <date>: replaced by the current date.\n     <year>: replaced by the current year.\n     <copyright>: replaced by `smarty-copyright-string' content.\n     <cursor>: final cursor position.\n\n`smarty-file-footer'\n     Type: string\n     Default value: `\"\"'\n     Description: String or file to insert as file footer.  See\n     `smarty-file-header'\n\n`smarty-company-name'\n     Type: string\n     Default value: `\"\"'\n     Description: Name of the company to insert in file header.\n\n`smarty-copyright-string'\n     Type: string\n     Default value: `\"\"'\n     Description: Coryright string to insert in file header.\n\n`smarty-date-format'\n     Type: string\n     Default value: `\"%Y-%m-%d\"'\n     Description: Date format.\n\n`smarty-modify-date-prefix-string'\n     Type: string\n     Default value: `\"\"'\n     Description: Prefix string of modification date in Smarty file\n     header.\n\n`smarty-modify-date-on-saving'\n     Type: bool\n     Default value: `nil'\n     Description: If `t'; update the modification date when the\n     buffer is saved.\n\n3.1.5 Miscellaneous\n-------------------\n\n`smarty-left-delimiter'\n     Type: string\n     Default value: `\"\"'\n     Description: Left escaping delimiter for Smarty templates.\n\n`smarty-right-delimiter'\n     Type: string\n     Default value: `\"\"'\n     Description: Right escaping delimiter for Smarty templates.\n\n`smarty-intelligent-tab'\n     Type: bool\n     Default value: `t'\n     Description: If `t'; TAB does indentation; completion and insert\n     tabulations. If `nil'; TAB does only indentation.\n\n`smarty-word-completion-in-minibuffer'\n     Type: bool\n     Default value: `t'\n     Description: If `t'; enable completion in the minibuffer.\n\n`smarty-word-completion-case-sensitive'\n     Type: bool\n     Default value: `nil'\n     Description: If `t'; completion is case sensitive.\n\n3.2 Functions\n=============\n\n3.2.1 Mode\n----------\n\n`smarty-electric-mode'\n     Menu: Smarty -> Options -> Mode -> Electric Mode\n     Keybinding: `C-c C-m C-e'\n     Description: This functions is used to enable/disable the\n     electric mode.\n\n`smarty-stutter-mode'\n     Menu: Smarty -> Options -> Mode -> Stutter Mode\n     Keybinding: `C-c C-m C-s'\n     Description: This function is used to enable/disable the stutter\n     mode.\n\n4 Menus\n*******\n\nThere are 2 menus: Smarty and Sources. All theses menus can be\naccessed from the menubar or from the right click. This chapter\ndescribes each menus.\n\n4.1 Smarty\n==========\n\nThis is the main menu of Smarty Mode. It allows an easy access to the\nmain features of the Smarty Mode: Templates (see *Note Templates::)\nand Options (see *Note Customization::).\n\nThis menu contains also 3 functions that are discussed in the next\npart.\n\n4.1.1 Functions\n---------------\n\n`smarty-show-messages'\n     Menu: Smarty -> Show Messages\n     Keybinding: `C-c M-m'\n     Description: This function opens the *Messages* buffer to\n     display previous error messages.\n\n`smarty-doc-mode'\n     Menu: Smarty -> Smarty Mode Documentation\n     Keybinding: `C-c C-h'\n     Description: This function opens the *Help* buffer and prints in\n     it the Smarty Mode documentation.\n\n`smarty-version'\n     Menu: Smarty -> Version\n     Keybinding: `C-c C-v'\n     Description: This function displays in the minibuffer the\n     current Smarty Mode version with the timestamp.\n\n4.2 Sources\n===========\n\nThe Sources menu shows the Smarty files in the current directory. If\nyou add or delete a file in the current directory, you need to\nrefresh the menu.\n\n4.2.1 Customization\n-------------------\n\n`smarty-source-file-menu'\n     Type: boolean\n     Default value: `t'\n     Description: If `t'; the Sources menu is enabled. This menu\n     contains the list of Smarty file located in the current\n     directory. The Sources menu scans the directory when a file is\n     opened.\n\n4.2.2 Functions\n---------------\n\n`smarty-add-source-files-menu'\n     Menu: Sources -> *Rescan*\n     Keybinding: `C-c C-s C-u'\n     Description: This function is used to refresh the Sources menu.\n\n5 Stuttering\n************\n\nThe stutter mode is a mode that affects a function to a key. For\nexample, when you use the `ENTER' key, the associated function will\ncreate a new line and indent it.\n\n5.1 Customization\n=================\n\n`smarty-stutter-mode'\n     Type: boolean\n     Default value: `t'\n     Description: If `t'; enable the stuttering. Is indicated in the\n     modeline by \"/s\" after the mode name and can be toggled by\n     `smarty-stutter-mode'.\n\n5.2 Functions\n=============\n\n`SPACE'\n     If in comment, indent the comment and add new line if necessary.\n     In other case, add a space.\n\n`('\n     If the previous character is a `(', the `((' will be replaced by\n     `['.\n     If the previous character is a `[', the `[(' will be replaced by\n     `{'.\n     In other case, insert a `('.\n\n`)'\n     If the previous character is a `)', the `))' will be replaced by\n     `]'.\n     If the previous character is a `]', the `])' will be replaced by\n     `}'.\n     In other case, insert a `)'.\n\n6 Templates\n***********\n\nIn the Smarty Mode, the Smarty functions (like if, while, for, fopen,\nfclose) are predefined in functions called \"Templates\".\n\nEach template can be invoked by the function name or by using the\n<SPACE> key after the Smarty function name in the buffer (Note, using\n`M-<SPACE>' disable the template).\n\nA template can be aborted by using the `C-g' or by lefting empty the\ntempate prompt (in the minibuffer).\n\n6.1 Customization\n=================\n\n`smarty-electric-mode'\n     Type: boolean\n     Default value: `t'\n     Description: If `t'; enable automatic generation of template.\n     If `nil'; template generators can still be invoked through key\n     bindings and menu. Is indicated in the modeline by \"/e\" after\n     the mode name and can be toggled by `smarty-electric-mode'.\n\nFor a complete description of the template customizable variables,\nsee *Note Cu01-Pa01-Template::\n\n6.2 Functions\n=============\n\n6.2.1 Smarty Functions\n----------------------\n\nFor Smarty functions, see PDF or HTML documentation.\n\n6.2.2 Non-Smarty Functions\n--------------------------\n\n`smarty-template-header'\n     Menu: Smarty -> Templates -> Insert Header\n     Keybinding: `C-c C-t C-h'\n     Description: This function is used to insert a header in the\n     current buffer.\n\n`smarty-template-footer'\n     Menu: Smarty -> Templates -> Insert Footer\n     Keybinding: `C-c C-t C-f'\n     Description: This function is used to insert a footer in the\n     current buffer.\n\n`smarty-template-insert-date'\n     Menu: Smarty -> Templates -> Insert Date\n     Keybinding: `C-c C-t C-d i'\n     Description: This function is used to insert the date in the\n     current buffer.\n\n`smarty-template-modify'\n     Menu: Smarty -> Templates -> Modify Date\n     Keybinding: `C-c C-t C-d m'\n     Description: This function is used to modify the last\n     modification date in the current buffer.\n\n7 Bugs, Help\n************\n\n   * To report bugs: Bugtracker\n     (http://bugtracker.morinie.fr/lisp/set_project.php?project_id=2)\n\n   * To obtain help you can post on the dedicated forum: Forum\n     (http://forum.morinie.fr/lisp/)\n\n8 Key bindings\n**************\n\n\\{smarty-mode-map}\n\n(fn)" t nil)

;;;***

;;;### (autoloads (php-mode php-file-patterns php) "php-mode" "related/php-mode.el"
;;;;;;  (19218 20582))
;;; Generated autoloads from related/php-mode.el
(web-autoload-require 'php-mode 'lp '(nxhtml-download-root-url nil) "related/php-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'php 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php 'custom-loads (cons '"php-mode" loads))))

(defvar php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'") "\
List of file patterns for which to automatically invoke `php-mode'.")

(nxhtml-custom-autoload 'php-file-patterns 'php-mode nil)

(nxhtml-autoload 'php-mode `(lp '(nxhtml-download-root-url nil) "related/php-mode" nxhtml-install-dir) "\
Major mode for editing PHP code.\n\n\\{php-mode-map}\n\n(fn)" t nil)

;;;***

;;;### (autoloads (global-mozadd-mirror-mode mozadd-mirror-mode global-mozadd-refresh-edited-on-save-mode
;;;;;;  mozadd-refresh-edited-on-save-mode) "mozadd" "related/mozadd.el"
;;;;;;  (19234 45588))
;;; Generated autoloads from related/mozadd.el
(web-autoload-require 'mozadd 'lp '(nxhtml-download-root-url nil) "related/mozadd" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'mozadd-refresh-edited-on-save-mode `(lp '(nxhtml-download-root-url nil) "related/mozadd" nxhtml-install-dir) "\
Refresh mozadd edited file in Firefox when saving file.\nThe mozadd edited file is the file in the last buffer visited in\n`mozadd-mirror-mode'.\n\nYou can use this for example when you edit CSS files.\n\nThe mozadd edited file must be shown in Firefox and visible.\n\n(fn &optional ARG)" t nil)

(defvar global-mozadd-refresh-edited-on-save-mode nil "\
Non-nil if Global-Mozadd-Refresh-Edited-On-Save mode is enabled.\nSee the command `global-mozadd-refresh-edited-on-save-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-mozadd-refresh-edited-on-save-mode'.")

(nxhtml-custom-autoload 'global-mozadd-refresh-edited-on-save-mode 'mozadd nil)

(nxhtml-autoload 'global-mozadd-refresh-edited-on-save-mode `(lp '(nxhtml-download-root-url nil) "related/mozadd" nxhtml-install-dir) "\
Toggle Mozadd-Refresh-Edited-On-Save mode in every possible buffer.\nWith prefix ARG, turn Global-Mozadd-Refresh-Edited-On-Save mode on if and only if ARG is positive.\nMozadd-Refresh-Edited-On-Save mode is enabled in all buffers where `(lambda nil (when (or (derived-mode-p (quote css-mode)) (mozadd-html-buffer-file-p)) (mozadd-refresh-edited-on-save-mode 1)))' would do it.\nSee `mozadd-refresh-edited-on-save-mode' for more information on Mozadd-Refresh-Edited-On-Save mode.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'mozadd-mirror-mode `(lp '(nxhtml-download-root-url nil) "related/mozadd" nxhtml-install-dir) "\
Mirror content of current file buffer immediately in Firefox.\nWhen you turn on this mode the file will be opened in Firefox.\nEvery change you make in the buffer will trigger a redraw in\nFirefox - regardless of if you save the file or not.\n\nFor the mirroring to work the edited file must be shown in\nFirefox and visible.\n\nIf `nxml-where-mode' is on the marks will also be shown in\nFirefox as CSS outline style.  You can customize the style\nthrough the option `mozadd-xml-path-outline-style'.\n\nSee also `mozadd-refresh-edited-on-save-mode'.\n\n(fn &optional ARG)" t nil)

(defvar global-mozadd-mirror-mode nil "\
Non-nil if Global-Mozadd-Mirror mode is enabled.\nSee the command `global-mozadd-mirror-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-mozadd-mirror-mode'.")

(nxhtml-custom-autoload 'global-mozadd-mirror-mode 'mozadd nil)

(nxhtml-autoload 'global-mozadd-mirror-mode `(lp '(nxhtml-download-root-url nil) "related/mozadd" nxhtml-install-dir) "\
Toggle Mozadd-Mirror mode in every possible buffer.\nWith prefix ARG, turn Global-Mozadd-Mirror mode on if and only if ARG is positive.\nMozadd-Mirror mode is enabled in all buffers where `(lambda nil (when (mozadd-html-buffer-file-p) (mozadd-mirror-mode 1)))' would do it.\nSee `mozadd-mirror-mode' for more information on Mozadd-Mirror mode.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (inferior-moz-mode moz-minor-mode) "moz" "related/moz.el"
;;;;;;  (19047 46040))
;;; Generated autoloads from related/moz.el
(web-autoload-require 'moz 'lp '(nxhtml-download-root-url nil) "related/moz" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'moz-minor-mode `(lp '(nxhtml-download-root-url nil) "related/moz" nxhtml-install-dir) "\
MozRepl minor mode for interaction with Firefox.\nWith no argument, this command toggles the mode.\nNon-null prefix argument turns on the mode.\nNull prefix argument turns off the mode.\n\nWhen this minor mode is enabled, some commands become available\nto send current code area (as understood by c-mark-function) or\nregion or buffer to an inferior MozRepl process (which will be\nstarted as needed).\n\nThe following keys are bound in this minor mode:\n\n\\{moz-minor-mode-map}\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'inferior-moz-mode `(lp '(nxhtml-download-root-url nil) "related/moz" nxhtml-install-dir) "\
Major mode for interacting with Firefox via MozRepl.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (iss-mumamo-mode) "iss-mumamo" "related/iss-mumamo.el"
;;;;;;  (19294 32444))
;;; Generated autoloads from related/iss-mumamo.el
(web-autoload-require 'iss-mumamo 'lp '(nxhtml-download-root-url nil) "related/iss-mumamo" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'iss-mumamo-mode `(lp '(nxhtml-download-root-url nil) "related/iss-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes Inno Setup .iss files.\nThe main major mode will be `iss-mode'.\nThe [code] section, if any, will be in `pascal-mode'." t)

;;;***

;;;### (autoloads (iss-mode) "iss-mode" "related/iss-mode.el" (19294
;;;;;;  32444))
;;; Generated autoloads from related/iss-mode.el
(web-autoload-require 'iss-mode 'lp '(nxhtml-download-root-url nil) "related/iss-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'iss-mode `(lp '(nxhtml-download-root-url nil) "related/iss-mode" nxhtml-install-dir) "\
Major mode for editing InnoSetup script files. Upon startup iss-mode-hook is run.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (flymake-js-load flymake-js) "flymake-js" "related/flymake-js.el"
;;;;;;  (19218 20582))
;;; Generated autoloads from related/flymake-js.el
(web-autoload-require 'flymake-js 'lp '(nxhtml-download-root-url nil) "related/flymake-js" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'flymake-js 'custom-loads))) (if (member '"flymake-js" loads) nil (put 'flymake-js 'custom-loads (cons '"flymake-js" loads))))

(nxhtml-autoload 'flymake-js-load `(lp '(nxhtml-download-root-url nil) "related/flymake-js" nxhtml-install-dir) "\
Not documented\n\n(fn)" nil nil)

;;;***

;;;### (autoloads (flymake-java-1-load) "flymake-java-1" "related/flymake-java-1.el"
;;;;;;  (19264 5406))
;;; Generated autoloads from related/flymake-java-1.el
(web-autoload-require 'flymake-java-1 'lp '(nxhtml-download-root-url nil) "related/flymake-java-1" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'flymake-java-1-load `(lp '(nxhtml-download-root-url nil) "related/flymake-java-1" nxhtml-install-dir) "\
Not documented\n\n(fn)" nil nil)

;;;***

;;;### (autoloads (flymake-css-load) "flymake-css" "related/flymake-css.el"
;;;;;;  (19291 55616))
;;; Generated autoloads from related/flymake-css.el
(web-autoload-require 'flymake-css 'lp '(nxhtml-download-root-url nil) "related/flymake-css" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'flymake-css-load `(lp '(nxhtml-download-root-url nil) "related/flymake-css" nxhtml-install-dir) "\
Not documented\n\n(fn)" nil nil)

;;;***

;;;### (autoloads (django-mode) "django" "related/django.el" (19410
;;;;;;  52648))
;;; Generated autoloads from related/django.el
(web-autoload-require 'django 'lp '(nxhtml-download-root-url nil) "related/django" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'django-mode `(lp '(nxhtml-download-root-url nil) "related/django" nxhtml-install-dir) "\
Simple Django mode for use with mumamo.\nThis mode only provides syntax highlighting.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (csharp-mode csharp-mode-hook) "csharp-mode" "related/csharp-mode.el"
;;;;;;  (19412 5728))
;;; Generated autoloads from related/csharp-mode.el
(web-autoload-require 'csharp-mode 'lp '(nxhtml-download-root-url nil) "related/csharp-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(defvar csharp-mode-hook nil "\
*Hook called by `csharp-mode'.")

(nxhtml-custom-autoload 'csharp-mode-hook 'csharp-mode t)

(nxhtml-autoload 'csharp-mode `(lp '(nxhtml-download-root-url nil) "related/csharp-mode" nxhtml-install-dir) "\
Major mode for editing C# code. This mode is derived from CC Mode to\nsupport C#.\n\nThe hook `c-mode-common-hook' is run with no args at mode\ninitialization, then `csharp-mode-hook'.\n\nThis mode will automatically add a regexp for Csc.exe error and warning\nmessages to the `compilation-error-regexp-alist'.\n\nKey bindings:\n\\{csharp-mode-map}\n\n(fn)" t nil)

;;;***

;;;### (autoloads (winsav-switch-config winsav-save-full-config winsav-save-mode
;;;;;;  winsav winsav-put-window-tree) "winsav" "util/winsav.el"
;;;;;;  (19295 16484))
;;; Generated autoloads from util/winsav.el
(web-autoload-require 'winsav 'lp '(nxhtml-download-root-url nil) "util/winsav" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'winsav-put-window-tree `(lp '(nxhtml-download-root-url nil) "util/winsav" nxhtml-install-dir) "\
Put window structure SAVED-TREE into WINDOW.\nRestore a structure SAVED-TREE returned from\n`winsav-get-window-tree' into window WINDOW.\n\nIf COPY-WIN-OVL is non-nil then overlays having a 'window\nproperty pointing to one of the windows in SAVED-TREE where this\nwindow still is shown will be copied to a new overlay with\n'window property pointing to the corresponding new window.\n\nIf WIN-OVL-ALL-BUFS is non-nil then all buffers will be searched\nfor overlays with a 'window property of the kind above.\n\nAt the very end of this function the hook `winsav-after-put' is\nrun.\n\n(fn SAVED-TREE WINDOW &optional COPY-WIN-OVL WIN-OVL-ALL-BUFS)" nil nil)

(let ((loads (get 'winsav 'custom-loads))) (if (member '"winsav" loads) nil (put 'winsav 'custom-loads (cons '"winsav" loads))))

(defvar winsav-save-mode nil "\
Non-nil if Winsav-Save mode is enabled.\nSee the command `winsav-save-mode' for a description of this minor mode.")

(nxhtml-custom-autoload 'winsav-save-mode 'winsav nil)

(nxhtml-autoload 'winsav-save-mode `(lp '(nxhtml-download-root-url nil) "util/winsav" nxhtml-install-dir) "\
Toggle winsav configuration saving mode.\nWith numeric ARG, turn winsav saving on if ARG is positive, off\notherwise.\n\nWhen this mode is turned on, winsav configurations are saved from\none session to another.  A winsav configuration consists of\nframes, windows and visible buffers configurations plus\noptionally buffers and files managed by the functions used by\noption `desktop-save-mode'\n\nBy default this is integrated with `desktop-save-mode'.  If\n`desktop-save-mode' is on and `winsav-handle-also-desktop' is\nnon-nil then save and restore also desktop.\n\nSee the command `winsav-switch-config' for more information and\nother possibilities.\n\nNote: If you want to avoid saving when you exit just turn off\nthis minor mode.\n\nFor information about what is saved and restored and how to save\nand restore additional information see the function\n`winsav-save-configuration'.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'winsav-save-full-config `(lp '(nxhtml-download-root-url nil) "util/winsav" nxhtml-install-dir) "\
Saved current winsav configuration in directory DIRNAME.\nThen change to this configuration.\n\nSee also `winsav-switch-config'.\n\n(fn DIRNAME)" nil nil)

(nxhtml-autoload 'winsav-switch-config `(lp '(nxhtml-download-root-url nil) "util/winsav" nxhtml-install-dir) "\
Change to winsav configuration in directory DIRNAME.\nIf DIRNAME is the current winsav configuration directory then\noffer to save it or restore it from saved values.\n\nOtherwise, before switching offer to save the current winsav\nconfiguration.  Then finally switch to the new winsav\nconfiguration, creating it if it does not exist.\n\nIf option `desktop-save-mode' is on then buffers and files are also\nrestored and saved the same way.\n\nSee also option `winsav-save-mode' and command\n`winsav-tell-configuration'.\n\n(fn DIRNAME)" t nil)

;;;***

;;;### (autoloads (winsav-rotate winsize-set-mode-line-colors winsize-save-window-configuration
;;;;;;  winsize-balance-siblings resize-windows) "winsize" "util/winsize.el"
;;;;;;  (19292 28108))
;;; Generated autoloads from util/winsize.el
(web-autoload-require 'winsize 'lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'resize-windows `(lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir) "\
Start window resizing.\nDuring resizing a window is selected.  You can move its\nborders. In the default configuration the arrow keys moves the\nright or bottom border if they are there. To move the opposite\nborder use S-arrowkeys.\n\nYou can also do other window operations, like splitting, deleting\nand balancing the sizes.  The keybindings below describes the key\nbindings during resizing:\\<winsize-keymap>\n\n  `balance-windows'                      \\[balance-windows]\n  `winsize-balance-siblings'             \\[winsize-balance-siblings]\n  `fit-window-to-buffer'                 \\[fit-window-to-buffer]\n  `shrink-window-if-larger-than-buffer'  \\[shrink-window-if-larger-than-buffer]\n\n  `winsav-rotate'                        \\[winsav-rotate]\n\n  `winsize-move-border-up'      \\[winsize-move-border-up]\n  `winsize-move-border-down'    \\[winsize-move-border-down]\n  `winsize-move-border-left'    \\[winsize-move-border-left]\n  `winsize-move-border-right'   \\[winsize-move-border-right]\n\n  `winsize-to-border-or-window-left'    \\[winsize-to-border-or-window-left]\n  `winsize-to-border-or-window-up'      \\[winsize-to-border-or-window-up]\n  `winsize-to-border-or-window-right'   \\[winsize-to-border-or-window-right]\n  `winsize-to-border-or-window-down'    \\[winsize-to-border-or-window-down]\n\n   Note that you can also use your normal keys for\n   `forward-char', `backward-char', `next-line', `previous-line'\n   and what you have on HOME and END to move in the windows. That\n   might sometimes be necessary to directly select a\n   window. (You may however also use `other-window' or click\n   with the mouse, see below.)\n\n  `delete-window'                \\[delete-window]\n  `delete-other-windows'         \\[delete-other-windows]\n  `split-window-vertically'      \\[split-window-vertically]\n  `split-window-horizontally'    \\[split-window-horizontally]\n  `other-window'                 \\[other-window]\n\n  `winsize-save-window-configuration'       \\[winsize-save-window-configuration]\n  `winsize-next-window-configuration'       \\[winsize-next-window-configuration]\n  `winsize-previous-window-configuration'   \\[winsize-previous-window-configuration]\n\n  `mouse-set-point'   \\[mouse-set-point]\n\n  `winsize-quit'               \\[winsize-quit]\n  `winsize-stop-go-back'       \\[winsize-stop-go-back]\n  `winsize-stop'               \\[winsize-stop]\n  `winsize-stop-and-execute'   \\[winsize-stop-and-execute]\n\n  `winsize-help'          \\[winsize-help]\n  `describe-key'          \\[describe-key]\n  `describe-key-briefly'  \\[describe-key-briefly]\n  (All the normal help keys work, and at least those above will\n  play well with resizing.)\n\nNearly all other keys exits window resizing and they are also\nexecuted.  However, the key sequences in `winsize-let-me-use' and\ndito for commands there are also executed without exiting\nresizing.\n\nThe colors of the modelines are changed to those given in\n`winsize-mode-line-colors' to indicate that you are resizing\nwindows.  To make this indication more prominent the text in the\nselected window is marked with the face hold in the variable\n`winsize-selected-window-face'.\n\nThe option `winsize-juris-way' decides how the borders to move\nare selected. If this option is non-nil then the right or bottom\nborder are the ones that are moved with the arrow keys and the\nopposite border with shift arrow keys.\n\nIf `winsize-juris-way' is nil then the following apply:\n\nAs you select other borders or move to new a window the mouse\npointer is moved inside the selected window to show which borders\nare beeing moved. The mouse jumps a little bit to make its\nposition more visible. You can turn this off by customizing\n`winsize-make-mouse-prominent'.\n\nWhich borders initially are choosen are controlled by the\nvariable `winsize-autoselect-borders'.\n\n** Example: Border selection, movements and windows.\n\n  Suppose you have a frame divided into windows like in the\n  figure below.  If window B is selected when you start resizing\n  then (with default settings) the borders marked with 'v' and\n  'h' will be the ones that the arrow keys moves. To indicate\n  this the mouse pointer is placed in the right lower corner of\n  the selected window B.\n\n    +----------+-----------+--------+\n    |          |           v        |\n    |          |           v        |\n    |    A     |    _B_    v        |\n    |          |           v        |\n    |          |           v        |\n    |          |         x v        |\n    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    +----------+---------+----------+\n\n  Now if you press M-<left> then the picture below shows what has\n  happened. Note that the selected vertical border is now the one\n  between A and B. The mouse pointer has moved to the\n  corresponding corner in the window B, which is still selected.\n\n    +----------+-----------+--------+\n    |          v           |        |\n    |          v           |        |\n    |    A     v    _B_    |        |\n    |          v           |        |\n    |          v           |        |\n    |          v x         |        |\n    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    +----------+---------+----------+\n\n  Press M-<left> once again. This gives this picture:\n\n    +----------+-----------+--------+\n    |          v           |        |\n    |          v           |        |\n    |   _A_    v     B     |        |\n    |          v           |        |\n    |          v           |        |\n    |        x v           |        |\n    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    +----------+---------+----------+\n\n  Note that the window A is now selected. However there is no\n  border that could be moved to the left of this window (which\n  would otherwise be chosen now) so the border between A and B is\n  still the one that <left> and <right> moves. The mouse has\n  moved to A.\n\n  If we now delete window A the new situation will look like\n  this:\n\n    +----------+-----------+--------+\n    |                      |        |\n    |                      |        |\n    |         _B_          |        |\n    |                      |        |\n    |                      |        |\n    |                    x |        |\n    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    |                    |          |\n    +----------+---------+----------+\n\n\n\n>>>> testing stuff >>>>\n`help-mode-hook'\n`temp-buffer-show-function'\n`view-exit-action'\n<<<<<<<<<<<<<<<<<<<<<<<\n\n(fn)" t nil)

(nxhtml-autoload 'winsize-balance-siblings `(lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir) "\
Make current window siblings the same height or width.\nIt works the same way as `balance-windows', but only for the\ncurrent window and its siblings.\n\n(fn)" t nil)

(nxhtml-autoload 'winsize-save-window-configuration `(lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir) "\
Not documented\n\n(fn)" t nil)

(nxhtml-autoload 'winsize-set-mode-line-colors `(lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir) "\
Turn mode line colors on if ON is non-nil, otherwise off.\n\n(fn ON)" nil nil)

(nxhtml-autoload 'winsav-rotate `(lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir) "\
Rotate window configuration on selected frame.\nMIRROR should be either 'mirror-left-right, 'mirror-top-bottom or\nnil.  In the first case the window configuration is mirrored\nvertically and in the second case horizontally.  If MIRROR is nil\nthe configuration is not mirrored.\n\nIf TRANSPOSE is non-nil then the window structure is transposed\nalong the diagonal from top left to bottom right (in analogy with\nmatrix transosition).\n\nIf called interactively MIRROR will is 'mirror-left-right by\ndefault, but 'mirror-top-bottom if called with prefix.  TRANSPOSE\nis t. This mean that the window configuration will be turned one\nquarter clockwise (or counter clockwise with prefix).\n\n(fn MIRROR TRANSPOSE)" t nil)

;;;***

;;;### (autoloads (wrap-to-fill-column-mode wrap-to-fill-left-marg-modes
;;;;;;  wrap-to-fill-left-marg wrap-to-fill) "wrap-to-fill" "util/wrap-to-fill.el"
;;;;;;  (19306 28912))
;;; Generated autoloads from util/wrap-to-fill.el
(web-autoload-require 'wrap-to-fill 'lp '(nxhtml-download-root-url nil) "util/wrap-to-fill" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'wrap-to-fill 'custom-loads))) (if (member '"wrap-to-fill" loads) nil (put 'wrap-to-fill 'custom-loads (cons '"wrap-to-fill" loads))))

(defvar wrap-to-fill-left-marg nil "\
Left margin handling for `wrap-to-fill-column-mode'.\nUsed by `wrap-to-fill-column-mode'. If nil then center the\ndisplay columns. Otherwise it should be a number which will be\nthe left margin.")

(nxhtml-custom-autoload 'wrap-to-fill-left-marg 'wrap-to-fill t)

(defvar wrap-to-fill-left-marg-modes '(text-mode fundamental-mode) "\
Major modes where `wrap-to-fill-left-margin' may be nil.")

(nxhtml-custom-autoload 'wrap-to-fill-left-marg-modes 'wrap-to-fill t)

(nxhtml-autoload 'wrap-to-fill-column-mode `(lp '(nxhtml-download-root-url nil) "util/wrap-to-fill" nxhtml-install-dir) "\
Use `fill-column' display columns in buffer windows.\nBy default the display columns are centered, but see the option\n`wrap-to-fill-left-marg'.\n\nFix-me:\nNote 1: When turning this on `visual-line-mode' is also turned on. This\nis not reset when turning off this mode.\n\nNote 2: The text properties 'wrap-prefix and 'wrap-to-fill-prefix\nis set by this mode to indent continuation lines.\n\nKey bindings added by this minor mode:\n\n\\{wrap-to-fill-column-mode-map}\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (xhtml-help xhtml-help-show-tag-ref xhtml-help-tag-at-point
;;;;;;  xhtml-help-show-css-ref) "xhtml-help" "nxhtml/xhtml-help.el"
;;;;;;  (19364 34616))
;;; Generated autoloads from nxhtml/xhtml-help.el
(web-autoload-require 'xhtml-help 'lp '(nxhtml-download-root-url nil) "nxhtml/xhtml-help" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'xhtml-help-show-css-ref `(lp '(nxhtml-download-root-url nil) "nxhtml/xhtml-help" nxhtml-install-dir) "\
Show CSS reference for CSS property name at point.\n\n(fn)" t nil)

(nxhtml-autoload 'xhtml-help-tag-at-point `(lp '(nxhtml-download-root-url nil) "nxhtml/xhtml-help" nxhtml-install-dir) "\
Get xhtml tag name at or before point.\n\n(fn)" nil nil)

(nxhtml-autoload 'xhtml-help-show-tag-ref `(lp '(nxhtml-download-root-url nil) "nxhtml/xhtml-help" nxhtml-install-dir) "\
Show xhtml reference for tag name at or before point.\n\n(fn)" t nil)

(let ((loads (get 'xhtml-help 'custom-loads))) (if (member '"xhtml-help" loads) nil (put 'xhtml-help 'custom-loads (cons '"xhtml-help" loads))))

;;;***

;;;### (autoloads (tidy-build-menu tidy) "tidy-xhtml" "nxhtml/tidy-xhtml.el"
;;;;;;  (19364 34616))
;;; Generated autoloads from nxhtml/tidy-xhtml.el
(web-autoload-require 'tidy-xhtml 'lp '(nxhtml-download-root-url nil) "nxhtml/tidy-xhtml" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'tidy 'custom-loads))) (if (member '"tidy-xhtml" loads) nil (put 'tidy 'custom-loads (cons '"tidy-xhtml" loads))))

(nxhtml-autoload 'tidy-build-menu `(lp '(nxhtml-download-root-url nil) "nxhtml/tidy-xhtml" nxhtml-install-dir) "\
Set up the tidy menu in MAP.\nUsed to set up a Tidy menu in your favourite mode.\n\n(fn &optional MAP)" t nil)

;;;***

;;;### (autoloads (rngalt-set-validation-header) "rngalt" "nxhtml/rngalt.el"
;;;;;;  (19365 12162))
;;; Generated autoloads from nxhtml/rngalt.el
(web-autoload-require 'rngalt 'lp '(nxhtml-download-root-url nil) "nxhtml/rngalt" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'rngalt-set-validation-header `(lp '(nxhtml-download-root-url nil) "nxhtml/rngalt" nxhtml-install-dir) "\
Not documented\n\n(fn START-OF-DOC)" nil nil)

;;;***

;;;### (autoloads (nxml-where-global-mode nxml-where-mode nxml-where)
;;;;;;  "nxml-where" "nxhtml/nxml-where.el" (19365 12162))
;;; Generated autoloads from nxhtml/nxml-where.el
(web-autoload-require 'nxml-where 'lp '(nxhtml-download-root-url nil) "nxhtml/nxml-where" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'nxml-where 'custom-loads))) (if (member '"nxml-where" loads) nil (put 'nxml-where 'custom-loads (cons '"nxml-where" loads))))

(nxhtml-autoload 'nxml-where-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxml-where" nxhtml-install-dir) "\
Shows path in mode line.\n\n(fn &optional ARG)" t nil)

(defvar nxml-where-global-mode nil "\
Non-nil if Nxml-Where-Global mode is enabled.\nSee the command `nxml-where-global-mode' for a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `nxml-where-global-mode'.")

(nxhtml-custom-autoload 'nxml-where-global-mode 'nxml-where nil)

(nxhtml-autoload 'nxml-where-global-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxml-where" nxhtml-install-dir) "\
Toggle Nxml-Where mode in every possible buffer.\nWith prefix ARG, turn Nxml-Where-Global mode on if and only if ARG is positive.\nNxml-Where mode is enabled in all buffers where `nxml-where-turn-on-in-nxml-child' would do it.\nSee `nxml-where-mode' for more information on Nxml-Where mode.\n\n(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (nxhtml-features-check nxhtml-customize nxhtml)
;;;;;;  "nxhtml" "nxhtml/nxhtml.el" (19412 8766))
;;; Generated autoloads from nxhtml/nxhtml.el
(web-autoload-require 'nxhtml 'lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'nxhtml 'custom-loads))) (if (member '"nxhtml" loads) nil (put 'nxhtml 'custom-loads (cons '"nxhtml" loads))))

(nxhtml-autoload 'nxhtml-customize `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml" nxhtml-install-dir) "\
Customize nXhtml.\n\n(fn)" t nil)

(nxhtml-autoload 'nxhtml-features-check `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml" nxhtml-install-dir) "\
Check if external modules used by nXhtml are found.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (mako-nxhtml-mumamo-mode asp-nxhtml-mumamo-mode
;;;;;;  eruby-nxhtml-mumamo-mode jsp-nxhtml-mumamo-mode gsp-nxhtml-mumamo-mode
;;;;;;  smarty-nxhtml-mumamo-mode mjt-nxhtml-mumamo-mode genshi-nxhtml-mumamo-mode
;;;;;;  mason-nxhtml-mumamo-mode django-nxhtml-mumamo-mode embperl-nxhtml-mumamo-mode
;;;;;;  nxhtml-mumamo-mode) "nxhtml-mumamo" "nxhtml/nxhtml-mumamo.el"
;;;;;;  (19390 8016))
;;; Generated autoloads from nxhtml/nxhtml-mumamo.el
(web-autoload-require 'nxhtml-mumamo 'lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for (X)HTML with main mode `nxhtml-mode'.\nThis covers inlined style and javascript and PHP.\n\nSee also `mumamo-alt-php-tags-mode'." t)

(nxhtml-autoload 'embperl-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Embperl files with main mode `nxhtml-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'django-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Django with main mode `nxhtml-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'mason-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Mason using main mode `nxhtml-mode'.\nThis covers inlined style and javascript." t)

(nxhtml-autoload 'genshi-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Genshi with main mode `nxhtml-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'mjt-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for MJT with main mode `nxhtml-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'smarty-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Smarty with main mode `nxhtml-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'gsp-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for GSP with main mode `nxhtml-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'jsp-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for JSP with main mode `nxhtml-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'eruby-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for eRuby with main mode `nxhtml-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'asp-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for ASP with main mode `nxhtml-mode'.\nThis also covers inlined style and javascript." t)

(nxhtml-autoload 'mako-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Mako with main mode `nxhtml-mode'.\nThis also covers inlined style and javascript." t)

;;;***

;;;### (autoloads (nxhtml-validation-header-mode nxhtml-short-tag-help
;;;;;;  nxhtml-mode) "nxhtml-mode" "nxhtml/nxhtml-mode.el" (19412
;;;;;;  8766))
;;; Generated autoloads from nxhtml/nxhtml-mode.el
(web-autoload-require 'nxhtml-mode 'lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(when (fboundp 'nxml-mode)
(nxhtml-autoload 'nxhtml-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mode" nxhtml-install-dir) "\
Major mode for editing XHTML documents.\nIt is based on `nxml-mode' and adds some features that are useful\nwhen editing XHTML files.\\<nxhtml-mode-map>\n\nThe XML menu contains functionality added by `nxml-mode' (on\nwhich this major mode is based).  There is also a popup menu\nadded to the [apps] key.\n\nThe most important features are probably completion and\nvalidation, which is inherited from `nxml-mode' with some small\naddtions.  In very many situation you can use completion. To\naccess it type \\[nxml-complete]. Completion has been enhanced in\nthe following way:\n\n- If region is active and visible then completion will surround the\n  region with the chosen tag's start and end tag.  However only the\n  starting point is checked for validity. If something is wrong after\n  insertion you will however immediately see it if you have validation\n  on.\n- It can in some cases give assistance with attribute values.\n- Completion can be customized, see the menus XHTML - Completion:\n  * You can use a menu popup style completion.\n  * You can have alternatives grouped.\n  * You can get a short help text shown for each alternative.\n- There does not have to be a '<' before point for tag name\n  completion. (`nxml-mode' requires a '<' before point for tag name\n  completion.)\n- Completes xml version and encoding.\n- Completes in an empty buffer, ie inserts a skeleton.\n\nHere are all key bindings in nxhtml-mode itself:\n\n\\{nxhtml-mode-map}\n\nNotice that other minor mode key bindings may also be active, as\nwell as emulation modes. Do \\[describe-bindings] to get a list\nof all active key bindings. Also, *VERY IMPORTANT*, if mumamo is\nused in the buffer each mumamo chunk has a different major mode\nwith different key bindings. You can however still see all\nbindings with \\[describe-bindings], but you have to do that with\npoint in the mumamo chunk you want to know the key bindings in.\n\n(fn)" t nil))

(nxhtml-autoload 'nxhtml-short-tag-help `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mode" nxhtml-install-dir) "\
Display description of tag TAG.  If TAG is omitted, try tag at point.\n\n(fn TAG)" t nil)

(when (fboundp 'nxml-mode)
(nxhtml-autoload 'nxhtml-validation-header-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mode" nxhtml-install-dir) "\
If on use a Fictive XHTML Validation Header for the buffer.\nSee `nxhtml-set-validation-header' for information about Fictive XHTML Validation Headers.\n\nThis mode may be turned on automatically in two ways:\n- If you try to do completion of a XHTML tag or attribute then\n  `nxthml-mode' may ask you if you want to turn this mode on if\n  needed.\n- You can also choose to have it turned on automatically whenever\n  a mumamo multi major mode is used, see\n  `nxhtml-validation-header-if-mumamo' for further information.\n\n(fn &optional ARG)" t nil))

;;;***

;;;### (autoloads (nxhtml-overview nxhtml-menu-mode nxhtml-browse-region
;;;;;;  nxhtml-browse-file nxhtml-edit-with-gimp) "nxhtml-menu" "nxhtml/nxhtml-menu.el"
;;;;;;  (19412 8766))
;;; Generated autoloads from nxhtml/nxhtml-menu.el
(web-autoload-require 'nxhtml-menu 'lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtml-edit-with-gimp `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir) "\
Edit with GIMP buffer or file at point.\n\n(fn)" t nil)

(nxhtml-autoload 'nxhtml-browse-file `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir) "\
View file in web browser.\n\n(fn FILE)" t nil)

(nxhtml-autoload 'nxhtml-browse-region `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir) "\
View region in web browser.\n\n(fn)" t nil)

(defvar nxhtml-menu-mode nil "\
Non-nil if Nxhtml-Menu mode is enabled.\nSee the command `nxhtml-menu-mode' for a description of this minor mode.")

(nxhtml-custom-autoload 'nxhtml-menu-mode 'nxhtml-menu nil)

(nxhtml-autoload 'nxhtml-menu-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir) "\
Minor mode to turn on some key and menu bindings.\nSee `nxhtml-mode' for more information.\n\nThis minor mode adds the entry 'nXhtml' to the menu bar.  This\nsubmenu gives easy access to most of the important features of\nnXhtml.\n\nTo see an (incomplete) overview in html format do\n\\[nxhtml-overview].\n\n* Note: Please observe that when loading nXhtml some file\n  associations are done, see `nxhtml-setup-file-assoc'.\n\nHere are some important features:\n\n- multiple major modes, see `define-mumamo-multi-major-mode'\n- easy uploading and viewing of files, see for example\n  `html-upl-upload-file'\n\n- validation in XHTML part for php etc, see\n  `nxhtml-validation-header-mode' (you probably also want to know\n  about `nxhtml-toggle-visible-warnings' for this!)\n\n- converting of html to xhtml, see `tidy-buffer'\n\nSome smaller, useful, but easy-to-miss features:\n\n* Following links. The href and src attribute names are\n  underlined and a special keymap is bound to\n  them:\\<mlinks-mode-map>\n\n    \\[mlinks-backward-link], \\[mlinks-forward-link] Move\n        between underlined href/src attributes\n\n    \\[mlinks-goto], Mouse-1 Follow link inside Emacs\n        (if possible)\n\n  It is even a little bit quicker when the links are in an active\n  state (marked with the face `isearch'):\\<mlinks-active-hilight-keymap>\n\n    \\[mlinks-backward-link], \\[mlinks-forward-link] Move\n        between underlined href/src attributes\n    \\[mlinks-goto], Mouse-1  Follow link inside Emacs (if possible)\n\n  If the link is not into a file that you can edit (a mailto link\n  for example) you will be prompted for an alternative action.\n\n* Creating links. To make it easier to create links to id/name\n  attribute in different files there are two special\n  functions:\\<nxhtml-mode-map>\n\n    \\[nxhtml-save-link-to-here] copy link to id/name (you must\n        be in the tag to get the link)\n    \\[nxhtml-paste-link-as-a-tag] paste this as an a-tag.\n\nThis minor mode also adds some bindings:\n\n\\{nxhtml-menu-mode-map}\n\n---------\n* Note: Some of the features supported are optional and available\n  only if other Emacs modules are found.  Use\n  \\[nxhtml-features-check] to get a list of these optional\n  features and modules needed. You should however have no problem\n  with this if you have followed the installation instructions\n  for nXhtml.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'nxhtml-overview `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir) "\
Show a HTML page with an overview of nXhtml.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (nxhtml-report-bug) "nxhtml-bug" "nxhtml/nxhtml-bug.el"
;;;;;;  (19277 43756))
;;; Generated autoloads from nxhtml/nxhtml-bug.el
(web-autoload-require 'nxhtml-bug 'lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-bug" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtml-report-bug `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-bug" nxhtml-install-dir) "\
Report a bug in nXhtml.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (html-wtoc) "html-wtoc" "nxhtml/html-wtoc.el" (19364
;;;;;;  34616))
;;; Generated autoloads from nxhtml/html-wtoc.el
(web-autoload-require 'html-wtoc 'lp '(nxhtml-download-root-url nil) "nxhtml/html-wtoc" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-wtoc 'custom-loads))) (if (member '"html-wtoc" loads) nil (put 'html-wtoc 'custom-loads (cons '"html-wtoc" loads))))

;;;***

;;;### (autoloads (html-upl-ediff-file html-upl-edit-remote-file-with-toc
;;;;;;  html-upl-edit-remote-file html-upl-upload-file html-upl-remote-dired
;;;;;;  html-upl-upload-site html-upl-upload-site-with-toc html-upl)
;;;;;;  "html-upl" "nxhtml/html-upl.el" (19364 34616))
;;; Generated autoloads from nxhtml/html-upl.el
(web-autoload-require 'html-upl 'lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-upl 'custom-loads))) (if (member '"html-upl" loads) nil (put 'html-upl 'custom-loads (cons '"html-upl" loads))))

(nxhtml-autoload 'html-upl-upload-site-with-toc `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Not documented\n\n(fn)" t nil)

(nxhtml-autoload 'html-upl-upload-site `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Not documented\n\n(fn)" t nil)

(nxhtml-autoload 'html-upl-remote-dired `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Start dired for remote directory or its parent/ancestor.\n\n(fn DIRNAME)" t nil)

(nxhtml-autoload 'html-upl-upload-file `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Upload a single file in a site.\nFor the definition of a site see `html-site-current'.\n\n(fn FILENAME)" t nil)

(nxhtml-autoload 'html-upl-edit-remote-file `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Not documented\n\n(fn)" t nil)

(nxhtml-autoload 'html-upl-edit-remote-file-with-toc `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Not documented\n\n(fn)" t nil)

(nxhtml-autoload 'html-upl-ediff-file `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Run ediff on local and remote file.\nFILENAME could be either the remote or the local file.\n\n(fn FILENAME)" t nil)

;;;***

;;;### (autoloads (html-toc) "html-toc" "nxhtml/html-toc.el" (19364
;;;;;;  34616))
;;; Generated autoloads from nxhtml/html-toc.el
(web-autoload-require 'html-toc 'lp '(nxhtml-download-root-url nil) "nxhtml/html-toc" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-toc 'custom-loads))) (if (member '"html-toc" loads) nil (put 'html-toc 'custom-loads (cons '"html-toc" loads))))

(defconst html-toc-menu-map (let ((map (make-sparse-keymap))) (define-key map [html-toc-browse-frames-file] (list 'menu-item "Browse Frames File" 'html-toc-browse-frames-file)) (define-key map [html-toc-write-frames-file] (list 'menu-item "Write Frames File" 'html-toc-write-frames-file)) (define-key map [html-toc-write-toc-file] (list 'menu-item "Write TOC File for Frames" 'html-toc-write-toc-file)) (define-key map [html-toc-sep1] (list 'menu-item "--")) (define-key map [html-toc-edit-pages-file] (list 'menu-item "Edit List of Pages for TOC" 'html-site-edit-pages-file)) (define-key map [html-toc-create-pages-file] (list 'menu-item "Write List of Pages for TOC" 'html-toc-create-pages-file)) map))

;;;***

;;;### (autoloads (html-site-query-replace html-site-rgrep html-site-find-file
;;;;;;  html-site-dired-current html-site-set-site html-site-buffer-or-dired-file-name
;;;;;;  html-site) "html-site" "nxhtml/html-site.el" (19364 34616))
;;; Generated autoloads from nxhtml/html-site.el
(web-autoload-require 'html-site 'lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-site 'custom-loads))) (if (member '"html-site" loads) nil (put 'html-site 'custom-loads (cons '"html-site" loads))))

(nxhtml-autoload 'html-site-buffer-or-dired-file-name `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Return buffer file name or file pointed to in dired.\n\n(fn)" nil nil)

(nxhtml-autoload 'html-site-set-site `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Not documented\n\n(fn NAME)" t nil)

(nxhtml-autoload 'html-site-dired-current `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Open `dired' in current site top directory.\n\n(fn)" t nil)

(nxhtml-autoload 'html-site-find-file `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Find file in current site.\n\n(fn)" t nil)

(nxhtml-autoload 'html-site-rgrep `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Search current site's files with `rgrep'.\nSee `rgrep' for the arguments REGEXP and FILES.\n\n(fn REGEXP FILES)" t nil)

(nxhtml-autoload 'html-site-query-replace `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Query replace in current site's files.\n\n(fn FROM TO FILE-REGEXP DELIMITED)" t nil)

;;;***

;;;### (autoloads (html-pagetoc-rebuild-toc html-pagetoc-insert-toc
;;;;;;  html-pagetoc) "html-pagetoc" "nxhtml/html-pagetoc.el" (19364
;;;;;;  34616))
;;; Generated autoloads from nxhtml/html-pagetoc.el
(web-autoload-require 'html-pagetoc 'lp '(nxhtml-download-root-url nil) "nxhtml/html-pagetoc" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-pagetoc 'custom-loads))) (if (member '"html-pagetoc" loads) nil (put 'html-pagetoc 'custom-loads (cons '"html-pagetoc" loads))))

(nxhtml-autoload 'html-pagetoc-insert-toc `(lp '(nxhtml-download-root-url nil) "nxhtml/html-pagetoc" nxhtml-install-dir) "\
Inserts a table of contents for the current html file.\nThe html header tags h1-h6 found in the file are inserted into\nthis table.  MIN-LEVEL and MAX-LEVEL specifies the minimum and\nmaximum level of h1-h6 to include.  They should be integers.\n\n(fn &optional MIN-LEVEL MAX-LEVEL)" t nil)

(nxhtml-autoload 'html-pagetoc-rebuild-toc `(lp '(nxhtml-download-root-url nil) "nxhtml/html-pagetoc" nxhtml-install-dir) "\
Update the table of contents inserted by `html-pagetoc-insert-toc'.\n\n(fn)" t nil)

(defconst html-pagetoc-menu-map (let ((map (make-sparse-keymap))) (define-key map [html-pagetoc-rebuild-toc] (list 'menu-item "Update Page TOC" 'html-pagetoc-rebuild-toc)) (define-key map [html-pagetoc-insert-style-guide] (list 'menu-item "Insert CSS Style for Page TOC" 'html-pagetoc-insert-style-guide)) (define-key map [html-pagetoc-insert-toc] (list 'menu-item "Insert Page TOC" 'html-pagetoc-insert-toc)) map))

;;;***

;;;### (autoloads (html-chklnk) "html-chklnk" "nxhtml/html-chklnk.el"
;;;;;;  (19364 34616))
;;; Generated autoloads from nxhtml/html-chklnk.el
(web-autoload-require 'html-chklnk 'lp '(nxhtml-download-root-url nil) "nxhtml/html-chklnk" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-chklnk 'custom-loads))) (if (member '"html-chklnk" loads) nil (put 'html-chklnk 'custom-loads (cons '"html-chklnk" loads))))

;;;***

;;;### (autoloads (web-vcs-investigate-elisp-file web-vcs-byte-compile-file
;;;;;;  web-vcs-message-with-face web-vcs-get-files-from-root web-vcs-log-edit
;;;;;;  web-vcs-default-download-directory) "web-vcs" "web-vcs.el"
;;;;;;  (19412 8766))
;;; Generated autoloads from web-vcs.el
(web-autoload-require 'web-vcs 'lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'web-vcs-default-download-directory `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Try to find a suitable place.\nConsiders site-start.el, site-\n\n(fn)" nil nil)

(nxhtml-autoload 'web-vcs-log-edit `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Open log file.\n\n(fn)" t nil)

(nxhtml-autoload 'web-vcs-get-files-from-root `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Download a file tree from VCS system using the web interface.\nUse WEB-VCS entry in variable `web-vcs-links-regexp' to download\nfiles via http from URL to directory DL-DIR.\n\nShow URL first and offer to visit the page.  That page will give\nyou information about version control system (VCS) system used\netc.\n\n(fn WEB-VCS URL DL-DIR)" nil nil)

(nxhtml-autoload 'web-vcs-message-with-face `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Display a colored message at the bottom of the string.\nFACE is the face to use for the message.\nFORMAT-STRING and ARGS are the same as for `message'.\n\nAlso put FACE on the message in *Messages* buffer.\n\n(fn FACE FORMAT-STRING &rest ARGS)" nil nil)

(nxhtml-autoload 'web-vcs-byte-compile-file `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Byte compile FILE in a new Emacs sub process.\nEXTRA-LOAD-PATH is added to the front of `load-path' during\ncompilation.\n\nFILE is set to `buffer-file-name' when called interactively.\nIf LOAD\n\n(fn FILE &optional LOAD EXTRA-LOAD-PATH COMP-DIR)" t nil)

(nxhtml-autoload 'web-vcs-investigate-elisp-file `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Not documented\n\n(fn FILE-OR-BUFFER)" t nil)

;;;***

;;;### (autoloads (nxhtmlmaint-byte-uncompile-all nxhtmlmaint-byte-recompile
;;;;;;  nxhtmlmaint-start-byte-compilation) "nxhtmlmaint" "nxhtmlmaint.el"
;;;;;;  (19378 27812))
;;; Generated autoloads from nxhtmlmaint.el
(web-autoload-require 'nxhtmlmaint 'lp '(nxhtml-download-root-url nil) "nxhtmlmaint" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtmlmaint-start-byte-compilation `(lp '(nxhtml-download-root-url nil) "nxhtmlmaint" nxhtml-install-dir) "\
Start byte compilation of nXhtml in new Emacs instance.\nByte compiling in general makes elisp code run 5-10 times faster\nwhich is quite noticeable when you use nXhtml.\n\nThis will also update the file nxhtml-loaddefs.el.\n\nYou must restart Emacs to use the byte compiled files.\n\nIf for some reason the byte compiled files does not work you can\nremove then with `nxhtmlmaint-byte-uncompile-all'.\n\n(fn)" t nil)

(nxhtml-autoload 'nxhtmlmaint-byte-recompile `(lp '(nxhtml-download-root-url nil) "nxhtmlmaint" nxhtml-install-dir) "\
Recompile or compile all nXhtml files in current Emacs.\n\n(fn)" t nil)

(nxhtml-autoload 'nxhtmlmaint-byte-uncompile-all `(lp '(nxhtml-download-root-url nil) "nxhtmlmaint" nxhtml-install-dir) "\
Delete byte compiled files in nXhtml.\nThis will also update the file nxhtml-loaddefs.el.\n\nSee `nxhtmlmaint-start-byte-compilation' for byte compiling.\n\n(fn)" t nil)

;;;***

;;;### (autoloads (zencoding-preview zencoding-expand-yas zencoding-mode
;;;;;;  zencoding-expand-line zencoding) "zencoding-mode" "util/zencoding-mode.el"
;;;;;;  (19275 41782))
;;; Generated autoloads from util/zencoding-mode.el
(web-autoload-require 'zencoding-mode 'lp '(nxhtml-download-root-url nil) "util/zencoding-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'zencoding 'custom-loads))) (if (member '"zencoding-mode" loads) nil (put 'zencoding 'custom-loads (cons '"zencoding-mode" loads))))

(nxhtml-autoload 'zencoding-expand-line `(lp '(nxhtml-download-root-url nil) "util/zencoding-mode" nxhtml-install-dir) "\
Replace the current line's zencode expression with the corresponding expansion.\nIf prefix ARG is given or region is visible call `zencoding-preview' to start an\ninteractive preview.\n\nOtherwise expand line directly.\n\nFor more information see `zencoding-mode'.\n\n(fn ARG)" t nil)

(nxhtml-autoload 'zencoding-mode `(lp '(nxhtml-download-root-url nil) "util/zencoding-mode" nxhtml-install-dir) "\
Minor mode for writing HTML and CSS markup.\nWith zen coding for HTML and CSS you can write a line like\n\n  ul#name>li.item*2\n\nand have it expanded to\n\n  <ul id=\"name\">\n    <li class=\"item\"></li>\n    <li class=\"item\"></li>\n  </ul>\n\nThis minor mode defines keys for quick access:\n\n\\{zencoding-mode-keymap}\n\nHome page URL `http://www.emacswiki.org/emacs/ZenCoding'.\n\nSee also `zencoding-expand-line'.\n\n(fn &optional ARG)" t nil)

(nxhtml-autoload 'zencoding-expand-yas `(lp '(nxhtml-download-root-url nil) "util/zencoding-mode" nxhtml-install-dir) "\
Not documented\n\n(fn)" t nil)

(nxhtml-autoload 'zencoding-preview `(lp '(nxhtml-download-root-url nil) "util/zencoding-mode" nxhtml-install-dir) "\
Expand zencode between BEG and END interactively.\nThis will show a preview of the expanded zen code and you can\naccept it or skip it.\n\n(fn BEG END)" t nil)

;;;***

;;;### (autoloads nil nil ("autostart.el" "autostart22.el" "etc/schema/schema-path-patch.el"
;;;;;;  "nxhtml-base.el" "nxhtml/html-imenu.el" "nxhtml/html-move.el"
;;;;;;  "nxhtml/html-quote.el" "nxhtml/nxhtml-autoload.el" "nxhtml/nxhtml-strval.el"
;;;;;;  "nxhtml/nxhtmljs.el" "nxhtml/outline-magic.el" "nxhtml/wtest.el"
;;;;;;  "related/flymake-helpers.el" "related/flymakemsg.el" "related/flymu.el"
;;;;;;  "related/php-imenu.el" "tests/angus77-setup-jde.el" "tests/emacstest-suites.el"
;;;;;;  "tests/ert2.el" "tests/hfy-test.el" "tests/inemacs/bug1013.el"
;;;;;;  "tests/mumamo-test.el" "tests/nxhtmltest-helpers.el" "util/appmenu-fold.el"
;;;;;;  "util/css-simple-completion.el" "util/custsets.el" "util/ecb-batch-compile.el"
;;;;;;  "util/fupd.el" "util/idn.el" "util/key-cat.el" "util/mumamo-aspnet.el"
;;;;;;  "util/mumamo-trace.el" "util/new-key-seq-widget.el" "util/nxml-mode-os-additions.el"
;;;;;;  "util/org-panel.el" "util/rxi.el" "util/useful-commands.el"
;;;;;;  "web-autoload.el") (19995 62999 280548))

;;;***

;;;### (autoloads (nxhtml-byte-recompile-file nxhtml-byte-compile-file
;;;;;;  nxhtml-get-missing-files nxhtml-update-existing-files nxhtml-setup-download-all
;;;;;;  nxhtml-setup-auto-download nxhtml-setup-install) "nxhtml-web-vcs"
;;;;;;  "nxhtml-web-vcs.el" (19412 8766))
;;; Generated autoloads from nxhtml-web-vcs.el
(web-autoload-require 'nxhtml-web-vcs 'lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtml-setup-install `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Setup and start nXhtml installation.\n\nThis is for installation and updating directly from the nXhtml\ndevelopment sources.\n\nThere are two different ways to install:\n\n  (1) Download all at once: `nxhtml-setup-download-all'\n  (2) Automatically download part by part: `nxhtml-setup-auto-download'\n\nYou can convert between those ways by calling this function again.\nYou can also do this by setting the option `nxhtml-autoload-web' yourself.\n\nWhen you have nXhtml installed you can update it:\n\n  (3) Update new files in nXhtml: `nxhtml-update-existing-files'\n\nTo learn more about nXhtml visit its home page at URL\n`http://www.emacswiki.com/NxhtmlMode/'.\n\nIf you want to test auto download (but not use it further) there\nis a special function for that, you answer T here:\n\n   (T) Test automatic download part by part: `nxhtml-setup-test-auto-download'\n\n======\n*Note*\nIf you want to download a zip file with latest released version instead then\nplease see URL `http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html'.\n\n(fn WAY)" t nil)

(nxhtml-autoload 'nxhtml-setup-auto-download `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Set up to autoload nXhtml files from the web.\n\nThis function will download some initial files and then setup to\ndownload the rest when you need them.\n\nFiles will be downloaded under the directory root you specify in\nDL-DIR.\n\nNote that files will not be upgraded automatically.  The auto\ndownloading is just for files you are missing. (This may change a\nbit in the future.) If you want to upgrade those files that you\nhave downloaded you can just call `nxhtml-update-existing-files'.\n\nYou can easily switch between this mode of downloading or\ndownloading the whole of nXhtml by once.  To switch just call the\ncommand `nxhtml-setup-install'.\n\nSee also the command `nxhtml-setup-download-all'.\n\nNote: If your nXhtml is to old you can't use this function\n      directly.  You have to upgrade first, se the function\n      above. Version 2.07 or above is good for this.\n\n(fn DL-DIR)" t nil)

(nxhtml-autoload 'nxhtml-setup-download-all `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Download or update all of nXhtml.\n\nYou can download all if nXhtml with this command.\n\nTo update existing files use `nxhtml-update-existing-files'.\n\nIf you want to download only those files you are actually using\nthen call `nxhtml-setup-auto-download' instead.\n\nSee the command `nxhtml-setup-install' for a convenient way to\ncall these commands.\n\nFor more information about auto download of nXhtml files see\n`nxhtml-setup-auto-download'.\n\n(fn DL-DIR)" t nil)

(nxhtml-autoload 'nxhtml-update-existing-files `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Update existing nXhtml files from the development sources.\nOnly files you already have will be updated.\n\nNote that this works both if you have setup nXhtml to auto\ndownload files as you need them or if you have downloaded all of\nnXhtml at once.\n\nFor more information about installing and updating nXhtml see the\ncommand `nxhtml-setup-install'.\n\n(fn)" t nil)

(nxhtml-autoload 'nxhtml-get-missing-files `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Not documented\n\n(fn SUB-DIR FILE-NAME-LIST)" nil nil)

(nxhtml-autoload 'nxhtml-byte-compile-file `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Not documented\n\n(fn FILE &optional LOAD)" nil nil)

(nxhtml-autoload 'nxhtml-byte-recompile-file `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Byte recompile FILE file if necessary.\nFor more information see `nxhtml-byte-compile-file'.\nLoading is done if recompiled and LOAD is t.\n\n(fn FILE &optional LOAD)" t nil)

;;;***
