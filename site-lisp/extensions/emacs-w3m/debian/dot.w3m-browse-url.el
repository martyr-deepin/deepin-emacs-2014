; from TIPS.ja
;   The following emacsen configuration makes `C-x m' on the string looking 
;   like URI browse the page by w3m.
;
(setq browse-url-browser-function 'w3m-browse-url)
(global-set-key "\C-xm" 'browse-url-at-point)
