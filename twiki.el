;;; twiki.el --- mode for editing Twiki wiki files for emacs

;; Copyright (C) 2010-2012 Christopher J. White

;; Author: Christopher J. White <twiki@grierwhite.com>
;; Maintainer: Christopher J. White <twiki@grierwhite.com>
;; Keywords: twiki, wiki
;; Last-modified: 2012-08-01
;; Version 1.2

;; GNU General Public License v3 (GNU GPL v3),
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; The license text: <http://www.gnu.org/licenses/gpl-3.0.html>

;;; Commentary:
;;
;; This package is a major mode for editing TWiki pages
;; (http://twiki.org).  Twiki formatted pages are reformatted on
;; import into a twiki buffer to enhance readibility and friendlier
;; editing.  After editing, the pages are "exported" back to the twiki
;; format.
;; 
;; The key benefits to twiki mode:
;;
;;   * Keyboard shortcuts for common operations 
;;   * Auto-numbering of headings, numbered lists
;;   * Better bullet list management
;;   * orgtbl-minor mode for table editing
;;   * table numbering
;;   * font-lock support for syntax highlighting
;;
;; Typical usage:
;;
;;   1. In Web Browser, click edit in Twiki on the page to edit
;;
;;   2. Select all and copy to clipboard
;;   
;;   3. In emacs, twiki-import-for-edit - assuming you are not already
;;      in a buffer in twiki-mode, this will:
;;         * create a temp file
;;         * create buffer visiting that file,
;;         * set the major mode to twiki-mode
;;         * render the buffer for "editing", making bullet/lists more readable
;;
;;   4. Edit as necessary
;;
;;   5. Export to clipboard using C-c e (twiki-export-to-clipboard)
;;
;;   6. Back in Web Browser, Select All, and Paste from clipboard
;;      (replacing old text)
;;   
;;   7. Optionally save the file elsewhere
;;
;; Interesting twiki-mode keys:
;;    C-c h      Make current line a header line (asks for level, or prefix arg)
;;    C-c 1-6    Make a Header 1-6 line
;;    C-c C-h    Renumber all headers using 1.4.3 notation
;;    C-c C-r    Renumber all headers and tables
;;    C-c C-t    Renumber all tables
;;    C-c i      Import clipboard
;;    C-c e      Export to clipboard, rendering bullets/lists back to 
;;               twiki format
;; Bullet lists:
;;    Tab        Indent bullet list
;;    S-Tab      Unindent bullet list
;;
;; Headings:
;;    Tab        Hide/show direct subtree
;;    S-Tab      Hide/show all subtrees
;;
;; Server:
;;    C-c t p    Upload to the server for a preview
;;    C-c t s    Save as a new revision on the server
;;    C-c t u    Update the local copy from the latest on the server
;;    C-c t t    Return the status of the current document
;;    C-c t d    Diff the current buffer against the server version
;;    
;; When saving the buffer to a file, the format is saved as
;; twiki-format (the buffer is rendered for export, then saved).
;; 
;;
;;; INSTALLATION:
;;
;; Put this in your .emacs to associate "*.twiki" files with twiki-mode:
;;
;;   (add-to-list 'auto-mode-alist'("\\.twiki$" . twiki-mode))
;;
;;; COMMAND LINE TOOL:
;;
;; Install the command line tool 'twikish' in a location in the default path,
;; or set `twiki-shell-cmd' to the proper location.
;;
;; This enables the following integrations from within emacs.  See the
;; Server section in the section above.
;;
;; Additional command `twiki-open-topic' can be called to download 
;; a topic that hasn't yet been stored locally.
;; 
;;   
;;; FIREFOX ITS-ALL-TEXT ADDON INTEGRATION
;;
;; Firefox supports "It's All Text!", a plug-in that will allow using emacs (or
;; any editor) to edit the contents of text areas.
;;
;;   1. Install the add-on from:
;;      https://addons.mozilla.org/en-US/firefox/addon/its-all-text/
;;
;;   2. Setup the auto-mode-alist for ".twiki" above
;;
;;   3. Right-click in a text area in Firefox, select It's All Text ->
;;      Preferences
;;
;;   4. Configure the editor to be "emacsclient", whereever that is on
;;      your system
;; 
;;   5. Add ".twiki" to the list of file extensions.  If you put it
;;      first, all text areas will default to .twiki when pressing the
;;      hot key or clicking the "Edit" button
;;
;; Note, on Windows if you have emacs installed and you get errors
;; about not finding the socket, you may need to specify the full path
;; to the server socket file that is used by the emacs server you're using.  
;; This would have to go into a ".bat" file that in turn calls emacsclient
;; with the additional "--server-file=<>" argument, as that can't be put in
;; the editor command in the Its-All-Text preferences.
;;
;; To get your server-file, try: (process-get server-process :server-file)
;;
;;
;;; CUSTOMIZATION
;;
;; M-x customize-group twiki
;;
;;
;;; AUTO-NUMBERING OF HEADINGS
;;
;; On import or load of a twiki file, if headings were previously
;; numbered, the numbering and min-level will automatically be
;; detected.  If not, the user must call 'twiki-setup-heading-numbers'
;; to setup the minimum and maximum heading levels to be numbered.
;; This sets the variables 'twiki-min-heading-level and
;; twiki-max-heading-level.  All subsequent renumbering will only
;; affecting headings between min and max inclusive.
;;
;; All headings are number using "1.2.3" notation, one additional level
;; of decimal notation for each level deeper than 'twiki-min-heading level:
;;
;;   1. First level
;;   1.1. Next level
;;   1.2. Same level as above
;;   2. Back to first level
;;
;;
;;; TABLES USING ORGTBL
;;
;; Table editing is handled by orgtbl-mode (see Info topic of Org-mode).  Tables
;; are added by simply inserting vertical bars as the beginning of the line, one
;; vertical bar on each end of a row:
;;
;;    | *Header 1* | *Header 2* |
;;
;; With the cursor in a table line, the basic keys are as follows:
;;
;;   Tab       next cell, or if at the end of the row, advance to
;;             next row, creating a new row if necessary
;;
;;   S-Tab     prev cell
;;
;;   Ret       move to next row in the same column, creating a row
;;             if necessary
;;
;;   C-c C-c   Realign the table
;;   
;; Just type away in any cell.  On tab, realignment occurs,
;; "beautifying" the table so all rows have the same columns.
;;
;;
;;; COLSPAN
;;
;; Twiki tables support colspan by interpreting two adjacent vertical bars 
;; as an extention of the previous column:
;;
;;   |  Col 1    | Col 2     |
;;   |  Colspan 2 & 2       ||
;;
;; Unfortunately, orgtbl does not really do colspan and if the table
;; is realigned (on Tab, for example), the table will be reformatted
;; as follows:
;;
;;   |  Col 1         | Col 2     |
;;   |  Colspan 2 & 2 |           |
;;
;; To get around this, put "<<" in the column that should be merged
;; with the previous column.  On export, any columns that have only
;; "<<" as the cell text will get turned back into "||" so that Twiki
;; performs the appropriate colspan:
;;
;;   |  Col 1         | Col 2     |
;;   |  Colspan 2 & 2 | <<        |
;;
;;
;;; COLUMN WIDTH SPECIFIERS 
;;
;; It's possible to specify column-width in orgbl as follows:
;;
;;   | <10>     |       | <20>               |
;;   | *Name*   | *Age* | *Address*          |
;;   | Christo=>| 25    | 1234 Really Long=> |
;;
;; (See org-table for more info on how this works and editing such fields)
;;
;; On export, this descriptor row is put in a comment block so it's hidden
;; from view on the page.  On import again, the row is reinstated.
;;
;;
;;; AUTO-NUMBERING OF TABLES
;;
;; Table number simply looks for any line that matches "| *Table [0-9]+:".  The
;; number matched is replaced with a simple sequence from the start of the file.
;;
;;; KNOWN ISSUES:
;;
;;   - Two lists that are separated by a blank line in twiki syntax get munged
;;     together as a single list by twiki-render-for-edit
;;
;;; TODO:
;;
;;   - Handle heading / table number references on renumber
;;
;;   - Cell-alignment with orgtbl is different than twiki as twiki
;;     uses amount of spaces on either side of the content to align
;;     the cell.  orgtbl is a little smarter about numbers vs not, and
;;     supports left or right alignment.  Unclear the best way to
;;     combine the two.
;;
;;
;;; CHANGE LOG
;; 
;; 2011-07-19  (chris)
;;   - First public release
;;
;; 2011-07-21  (chris)
;;   - Eliminated the question to add numbering on import, added function
;;     "twiki-setup-heading-numbers"
;;   - Fixed twiki-electric-space so that it does not break table lines
;;
;; 2011-07-22  (chris)
;;   - Fixed bugs with renumbering and non-TOC headers containing "!!"
;;   - Added supported for file-save to write twiki-format
;;
;; 2011-07-22  (chris)
;;   - Fixed hang on export on a bullet list with an empty bullet
;;
;; 2011-07-22  (chris)
;;   - Fixed a bug with list numbering of alphanumeric lists (1., A.)
;;
;; 2011-07-29  (chris)
;;   - Added support for orgtbl column-descriptor row, commenting out the line
;;     on export, reinstating it on import
;;
;; 2011-07-31  (chris) -- Version 1.0.0
;;   - cleaned up the code, made a 'twiki' customozation group
;;   - added boolean for disabling orgtbl
;;   - fixed faces so they work for light and dark background modes
;;
;; 2011-09-03  (chris) -- Version 1.0.1
;;   - fixed twiki-electric-space not to break links
;;     of the form [[ref][title]] or [[ref]]
;;
;; 2012-05-14  (chris) -- Version 1.0.2
;;   - Fixed twiki-electric-space so that it does not break headings
;;
;; 2012-05-19  (chris)
;;   - Fixed handling of links so that they are not broken on save
;;   - Added support for C-u C-c <num> to set a heading as invisible to the TOC
;;   - Turn on outline-minor-mode and support [tab] and [S-tab] to toggle
;;     visiblity of subtrees
;;
;; 2012-06-20  (chris) -- Version 1.1.0
;;   - Better line break support when links are in a line
;;   - Added `twiki-verbatim' [C-c v] to add <verbatim></verbatim> 
;;     around a block
;;
;; 2012-08-01  (chris) -- Version 1.2
;;   - Drop '-' as bullet marker, as this causes problems when '-' 
;;     in a bullet line happens to end up after a line break
;;   - Fixed logic regarding import and overwriting the buffer
;;   - Fixed twiki-electric-space so that it does not break incomplete 
;;     table lines (ie, regonized a table line as starting with |, but
;;     not necessarily ending with |)
;;   - Added support for twikish command line tool
;; 
;; 2012-11-05  (chris) -- Version 1.3
;;   - Added font-lock for bumpy case links
;;   - Fixed buffer showing modified after save
;;   - Fixed problems with longlines-mode

(provide 'twiki)

(require 'info)
(require 'cl)

(defconst twiki-version "1.1.0")

;;; ------------------------------------------------------------
;;; Configurable stuff
;;; ------------------------------------------------------------

(defcustom twiki-tab-width 3
  "Spacing for bullet lists, etc.  Probably best to leave at 3 to match what 
twiki expects."
  :group 'twiki
  :type '(integer)
  )

(defcustom twiki-min-heading-level 1
  "Min heading level to include for heading renumbering."
  :group 'twiki
  :type '(integer)
  :options '(1 2 3 4 5 6)
)

(defcustom twiki-max-heading-level 6
  "*Max heading level to include for heading renumbering."
  :group 'twiki
  :type '(boolean)
  :options '(1 2 3 4 5 6)
)

(defcustom twiki-use-orgtbl t
  "Set to nil to disable the use of orgtbl-minor mode for tables"
  :group 'twiki
  :type '(boolean)
)

(defcustom twiki-silent-import nil
  "If non-nil, import into a buffer already in twiki-mode will not ask about overwrite"
  :group 'twiki
  :type '(boolean)
  )

(defcustom twiki-shell-cmd "twikish"
  "Shell command that supports commands that interact with the twiki server"
  :group 'twiki
  :type 'string
  )

;;
;; Internal variables
;;


(defvar twiki-heading-base-string ""
  "*Base string to use for heading renumbering")

(defvar twiki-start-heading-number ""
  "Staring heading number.  Computed automatically")

(defvar twiki-block-tags
  '("php" "file" "script" "code" "verbatim")
  "List of tags that delineate blocks that should not be parsed, such as <file> </file>"
)

(defvar twiki-kill-export-buffer t
  "*If non-null, kill the export buffer when done exporting")

(defvar twiki-font-lock-keywords
  (list
   (list "^\\(---\\+!* .*\\)"
	 '(1 'twiki-heading-1))

   (list "^\\(---\\+\\+!* .*\\)"
	 '(1 'twiki-heading-2))

   (list "^\\(---\\+\\+\\+!* .*\\)"
	 '(1 'twiki-heading-3))

   (list "^\\(---\\+\\+\\+\\+!* .*\\)"
	 '(1 'twiki-heading-4))

   (list "^\\(---\\+\\+\\+\\+\\+!* .*\\)"
	 '(1 'twiki-heading-5))

   (list "\\(:\\?:\\)"
	 '(1 'highlight))

   (list "\\(\\*\\b.*?\\b\\*\\)"
	 '(1 'bold))
   
   (list "\\(\\b__.*?__\\b\\)"
	 '(1 'bold-italic))
   
   (list "\\(\\b_.*?_\\b\\)"
	 '(1 'italic))
   
   (list "\\(\\[\\[.*\\]\\]\\)"
	 '(1 'highlight))

   (list "\\({{.*}}\\)"
	 '(1 'highlight))
   
   (list "\\(^\\|[^!]\\)\\(\\b[A-Z]+[a-z]+[A-Z]+[a-zA-Z0-9]*\\b\\)" 
         '(2 'highlight))
   )
  )

(defvar twiki-debug nil
  "Set to non-null to generate debug messages when rendering, etc.")

(defconst twiki-bullet-regex
  "\\([*]\\|[0-9]+\\.\\|[a-zA-Z]\\.\\|i+\\.\\|iv\\.\\|v\\.\\)")

(defconst twiki-bullet-renumber-regex
  "\\([*-]\\|[0-9]+\\.\\|[a-zA-Z]\\.\\|i+\\.\\|iv\\.\\|v\\.\\)")

(defvar twiki-importing nil)

(defvar twiki-subtree-closed nil)

(defvar twiki-mode-map nil
  "The keymap that is used in this mode.")

(if twiki-mode-map
    ()
  (setq twiki-mode-map (make-sparse-keymap))

  ;; set up the keymap
  (define-key twiki-mode-map (read-kbd-macro "C-c h") 'twiki-heading)
  (define-key twiki-mode-map (read-kbd-macro "C-c b") 'twiki-bold)
  (define-key twiki-mode-map (read-kbd-macro "C-c /") 'twiki-italic)
  (define-key twiki-mode-map (read-kbd-macro "C-c C-h") 'twiki-renumber-headings)
  (define-key twiki-mode-map (read-kbd-macro "C-c C-r") 'twiki-renumber-all)
  (define-key twiki-mode-map (read-kbd-macro "C-c C-t") 'twiki-renumber-tables)
  (define-key twiki-mode-map (read-kbd-macro "C-c 1") 'twiki-heading-1)
  (define-key twiki-mode-map (read-kbd-macro "C-c 2") 'twiki-heading-2)
  (define-key twiki-mode-map (read-kbd-macro "C-c 3") 'twiki-heading-3)
  (define-key twiki-mode-map (read-kbd-macro "C-c 4") 'twiki-heading-4)
  (define-key twiki-mode-map (read-kbd-macro "C-c 5") 'twiki-heading-5)

  (define-key twiki-mode-map (read-kbd-macro "C-c i") 'twiki-import-for-edit)
  (define-key twiki-mode-map (read-kbd-macro "C-c e") 'twiki-export-to-clipboard)

  (define-key twiki-mode-map (read-kbd-macro "C-c t p") 'twiki-preview)
  (define-key twiki-mode-map (read-kbd-macro "C-c t u") 'twiki-update)
  (define-key twiki-mode-map (read-kbd-macro "C-c t s") 'twiki-save)
  (define-key twiki-mode-map (read-kbd-macro "C-c t t") 'twiki-status)
  (define-key twiki-mode-map (read-kbd-macro "C-c t d") 'twiki-diff)

  (define-key twiki-mode-map (read-kbd-macro "C-c v") 'twiki-verbatim)

  (define-key twiki-mode-map " " 'twiki-electric-space)

  (define-key twiki-mode-map [tab] 'twiki-indent-line)
  (define-key twiki-mode-map [S-tab] 'twiki-unindent-line)
  )

(defvar twiki-mode-syntax-table nil)

(defvar twiki-mode-hook '())

(unless twiki-mode-syntax-table
  (setq twiki-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()    " twiki-mode-syntax-table)
  (modify-syntax-entry ?\) ")(    " twiki-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}    " twiki-mode-syntax-table)  
  (modify-syntax-entry ?\} "){    " twiki-mode-syntax-table))

;;;###autoload

(defgroup twiki nil "Group for twiki-mode related elements."
  :prefix "twiki-"
  :group 'hypermedia)

(defface twiki-heading-1
  '( (((class color) (background dark)) :foreground "yellow" :height 1.7 :inherit 'variable-pitch)
     (((class color) (background light)) :foreground "OrangeRed" :height 1.7 :inherit 'variable-pitch) )
  "Face for Twiki heading level 1"
  :group 'twiki)

(defface twiki-heading-2
  '( (((class color) (background dark)) :foreground "yellow" :height 1.6 :inherit 'variable-pitch)
     (((class color) (background light)) :foreground "OrangeRed" :height 1.6 :inherit 'variable-pitch) )
  "Face for Twiki heading level 2"
  :group 'twiki)
    
(defface twiki-heading-3
  '( (((class color) (background dark)) :foreground "yellow" :height 1.4 :inherit 'variable-pitch)
     (((class color) (background light)) :foreground "OrangeRed" :height 1.4 :inherit 'variable-pitch) )
  "Face for Twiki heading level 3"
  :group 'twiki)

(defface twiki-heading-4
  '( (((class color) (background dark)) :foreground "yellow" :height 1.2 :inherit 'variable-pitch)
     (((class color) (background light)) :foreground "OrangeRed" :height 1.2 :inherit 'variable-pitch) )
  "Face for Twiki heading level 4"
  :group 'twiki)

(defface twiki-heading-5
  '( (((class color) (background dark)) :foreground "yellow" :underline t :inherit 'variable-pitch)
     (((class color) (background light)) :foreground "OrangeRed" :underline t :inherit 'variable-pitch) )
  "Face for Twiki heading level 5"
  :group 'twiki)

(defun twiki-mode ()
  "Major mode for editing Twiki files.
This function ends by invoking the function(s) `twiki-mode-hook'.

\\{twiki-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'twiki-min-heading-level)
  (make-local-variable 'twiki-max-heading-level)
  (make-local-variable 'twiki-heading-base-string)
  (make-local-variable 'twiki-start-heading-number)

  (outline-minor-mode)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "---\\++")

  (setq twiki-start-heading-number nil)

  (setq font-lock-defaults '(twiki-font-lock-keywords))

  ;; become the current major mode
  (setq major-mode 'twiki-mode)
  (setq mode-name "Twiki")

  ;; Activate keymap and syntax table.
  (use-local-map twiki-mode-map)
  (set-syntax-table twiki-mode-syntax-table)
  
  (setq indent-tabs-mode nil)
  (setq tab-width twiki-tab-width)

  (twiki-render-for-edit)

  (add-hook 'before-save-hook
            (lambda () (when (eq major-mode 'twiki-mode) 
                         (twiki-render-for-export)
                         )))
  (add-hook 'after-save-hook
            (lambda () (when (eq major-mode 'twiki-mode) 
                         (twiki-render-for-edit)
                         (set-buffer-modified-p nil)
                         )))

  (when twiki-use-orgtbl (turn-on-orgtbl))

  (run-hooks 'twiki-mode-hook)
  )

;;
;; twiki-heading
;;
(defun twiki-heading (level)
  "Insert a heading at the given level"
  (interactive "NLevel: ")
  (beginning-of-line)
  (let ((hs (make-string (abs level) ?+)))
    (cond 
     ((looking-at (twiki-heading-regex))
      (let ((text (match-string 6))
            (base-num (if (< level 0) "" (match-string 3)))
            (suppress (if (< level 0) "!!" (match-string 2))))
        (replace-match (format "---%s%s%s %s" hs suppress base-num text))
        ))
     
     ((looking-at "^\\(.*\\)$")
      (let ((s (or (match-string 1) "")))
        (replace-match (format "---%s%s %s" hs (if (< level 0) "!!" "") s))
        ))
      )
    (beginning-of-line)
    (forward-char (+ (if (< level 0) 6 4) (abs level))))
  (twiki-renumber-headings)
  )
  
(defun twiki-heading-1 (hide)
  (interactive "P")
  (twiki-heading (if hide -1 1)))

(defun twiki-heading-2 (hide)
  (interactive "P")
  (twiki-heading (if hide -2 2)))

(defun twiki-heading-3 (hide)
  (interactive "P")
  (twiki-heading (if hide -3 3)))

(defun twiki-heading-4 (hide)
  (interactive "P")
  (twiki-heading (if hide -4 4)))

(defun twiki-heading-5 (hide)
  (interactive "P")
  (twiki-heading (if hide -5 5)))

;;
;; twiki-bold
;;
(defun twiki-bold (start end)
  "Makes the selected text bold"
  (interactive "r")
  (goto-char end)
  (insert "*")
  (goto-char start)
  (insert "*")
  )

;;
;; twiki-italic
;;
(defun twiki-italic (start end)
  "Makes the selected text italic"
  (interactive "r")
  (goto-char end)
  (insert "_")
  (goto-char start)
  (insert "_")
  )
  
;;
;; twiki-verbatim
;;
(defun twiki-verbatim (start end)
  "Makes the selected text italic"
  (interactive "r")
  (if mark-active
      (progn 
        (goto-char end)
        (insert "</verbatim>")
        (goto-char start)
        (insert "<verbatim>\n")
        )
    (insert "\n</verbatim>")
    (beginning-of-line)
    (forward-line -1)
    (insert "<verbatim>\n")
    )
  )
  
;;
;; twiki-export-to-clipboard ()
;;
(defun twiki-export-to-clipboard ()
  (interactive)
  "Render the buffer as a twiki-document, putting the result in the
clipboard suitable for pasting into the editing window of the
twiki-page.

In addition to just copying the text in the buffer, rendering performs
the following translations on the buffer contents:

Bullets and Ordered Lists

For viewing and editing, bullets may take the form:

   * bullet one

   * bullet two

   * bullet three
     continuation of bullet three

This will be reformatted to:
  
   * bullet one
   * bullet tow
   * bullet three continuation of bullet three
  
This makes sure that Twiki sees the list as a contiguous list
\(particularly import for ordered lists\) as opposed to multiple 1 item
lists.  In addition, Twiki does not allow indentation of continuation lines.
"

  ;; Things to do in the active buffer that should persist
  (twiki-renumber-headings)

  ;; Things to do only in the temp buffer that is relevant to twiki format only
  (let ((buf (get-buffer-create (format "*twiki-%s*" (buffer-name))))
        (text (filter-buffer-substring (point-min) (point-max) nil t))
        )
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert text)
      (twiki-render-for-export)
      (set-buffer buf)
      (goto-char (point-min))
      (clipboard-kill-ring-save (point-min) (point-max))
      (if twiki-kill-export-buffer
          (kill-buffer buf)
        (pop-to-buffer buf))
      )
    )
  (message "Exported Twiki text to clipboard")
  )

;;
;; twiki-render-for-edit ()
;;
(defun twiki-render-for-edit ()
  (interactive)
  "Render the current buffer for editing.  This beautifies bullets and ordered
lists, making them more readable for display and editing."

  (when twiki-debug (message "Rendering for twiki edit"))

  (let ((mod (buffer-modified-p)))

    (when twiki-debug (message "-- modified: %S" mod))
  
    ;; auto-detect min/max header levels
    (save-excursion
      (goto-char (point-min))
      (let ((found-min 20)
            found-number-str
            (found-max 0))
        (when twiki-debug (message "Searching for headings"))

        ;; Search for all instances of "---+++ 1. Title" at any level
        ;; Level is the number of "+", save the min level in 'found-min
        (while (re-search-forward 
                "^---\\(\++\\)!*\\( *\\([.0-9]+\\. *\\)?\\)\\(.*\\)"
                ;;twiki-heading-base-string  used to be in a format...
                nil t)
          (let ((level (length (match-string 1)))
                (number-str (match-string 2)))
            (when twiki-debug (message "Found level %d header" level))

            (cond 
             ;; This header *has* a number-str
             ((string-match "[^ ]" number-str)
              (when (< level found-min) 
                (setq found-min level)
                (if (null found-number-str) (setq found-number-str number-str))))
             )
            )
          )

        (when (not (= found-min 20))
          (setq twiki-min-heading-level found-min)
          (when twiki-debug (message "Start number-str: %s" found-number-str))
          (when (string-match 
                 "^ *\\(\\(.*\\)\\.\\)?\\(\\([0-9]+\\)\\.\\) *$" 
                 found-number-str)
            (setq twiki-heading-base-string (match-string 1 found-number-str))
            (setq twiki-start-heading-number 
                  (string-to-number (match-string 4 found-number-str)))
            )
          )
        )
      
      (when twiki-debug (message "Processing local variabls"))
      ;; Read any local-variables that may be specified in the text
      (hack-local-variables)
      
      ;; If were already in twiki-mode, renumber to twiki, then back to 
      ;; numbers
      (when twiki-debug (message "Twiki - Renumbering lists..."))
      (if (and (eq major-mode 'twiki-mode)
               (not twiki-importing))
          (twiki-renumber-list nil))
      (twiki-renumber-list t)
      
      ;; Renumber headings
      (when twiki-debug (message "Twiki - Renumbering heading..."))
      (twiki-renumber-headings)
      
      (when twiki-debug (message "Twiki - Renumbering tables..."))
      (twiki-renumber-tables)
      
      (when twiki-use-orgtbl
        
        ;; Tables in twiki support colspan by adjacent bars "||".  This
        ;; doesn't work well with orgtbl minor mode because it wants to
        ;; reformat all columns to the same width.  So, reformat colspan
        ;; cells with "| << |" to mark them as spanned with the cell to
        ;; the left.  On export, these cells will be converted back to "||"
        
        (goto-char (point-min))
        (when twiki-debug (message "Twiki - Reformatting tables..."))
        (while (re-search-forward "||" nil t)
          (replace-match "| << |")
          (backward-char) ; to ensure we find multiple colspan cells: "|||"
          )
        
        ;; Recognized rows that declare table alignment and column width rows
        ;;   <10>   no alignment defined, 10 chars width
        ;;   <r10>  for right aligned, 10 chars width
        ;;   <l10>  for lef aligned, 10 chars width
        (goto-char (point-min))
        (while (re-search-forward 
                "^<!--\\( *|\\( *<[rl]?[0-9]+> *|\\| *|\\)+ *\\) orgtbl.*-->$" nil t)
          (replace-match "\\1")
          )
        )
      )
    (set-buffer-modified-p mod)
    )
  (when twiki-debug (message "Twiki - Done"))
  )


;;
;; twiki-render-for-export - render the current buffer as twiki export format
;;
(defun twiki-render-for-export ()
  (save-excursion
    (goto-char (point-min))
    (when twiki-debug (message "Twiki export - renumbering lists"))
    (twiki-renumber-list nil)

    (when twiki-use-orgtbl

      ;; Convert "| << | " form back to "||" to represent colspan
      ;; (See above in render-for-edit)
      
      (when twiki-debug (message "Twiki export - reformatting tables"))
      (goto-char (point-min))
      (while (re-search-forward "| *<< *|" nil t)
        (replace-match "||")
        (backward-char) ; be to ensure we find multiple colspan cells: "| << | << |"
        )
      
      ;; Recognized rows that declare table alignment and column width rows
      ;;   <10>   no alignment defined, 10 chars width
      ;;   <r10>  for right aligned, 10 chars width
      ;;   <l10>  for lef aligned, 10 chars width
      (goto-char (point-min))
      (while (re-search-forward "^\\( *|\\( *<[rl]?[0-9]+> *|\\| *|\\)+ *\\)$" nil t)
        (replace-match "<!--\\1 orgtbl column formatting hints -->")
        )
      )
    )
  (when twiki-debug (message "Twiki export - Done"))
  )

;;
;; twiki-skip-past-blocks
;; 
(defun twiki-skip-past-blocks ()
  (let ((matches (match-data))
        result)
    (when twiki-debug (message "twiki-skip-past-blocks @ %d" (point)))
    (if (eq 'twiki-syntax-block (car (twiki-line-syntax)))
        (if (re-search-forward (format "</%s.*>" (regexp-opt twiki-block-tags t)) nil t)
            (setq result t)
          (goto-char (point-max))
          (setq result t)
          )
      )
    (when twiki-debug (message "twiki-skip-past-blocks done: result %S" result))
    (set-match-data matches)
    result
    )
  )


;; 
;; twiki-renumber-list
;;
(defun twiki-renumber-list (&optional to-numbers)
  (interactive)
  "Renumber all bullets and lists in the buffer.  If TO-NUMBERS is non-null, 
numbered lists and '-' lists are given numbers relative to their level (suitable
for display).  Otherwise list numbers are stripped to '-' for twiki synatax.
"
  (save-excursion
    ;;
    ;; Compress continuation lines
    ;;
    (when twiki-debug (message "twiki-renumber-list: compressing continuation lines"))
    (goto-char (point-min))
    
    (while (re-search-forward
            ;; Looks for bullet / list item line
            (format "^\\(\\(%s\\)+\\(%s\\) \\).*$" 
                    (make-string twiki-tab-width ? )
                    twiki-bullet-renumber-regex)
            nil t)
      
      (unless (twiki-skip-past-blocks)
        ;; Make indent-str a space string same length up to first char or 
        ;; text of bullet
        
        (let ((indent-str (make-string (- (match-end 1) (match-beginning 1)) ? )))
          ;; while the next line has this indent-str and is not a nested bullet
          ;; collapse the line
          (while (and (looking-at (format "\n\\(%s\\)\\S +" indent-str))
                      (not (looking-at (format "\n\\(%s\\)%s " indent-str twiki-bullet-renumber-regex))))
              (delete-char (- (match-end 1) (match-beginning 1)))
              (end-of-line)
              )
          )
        )
      )
    
    ;;
    ;; Collapse blank lines between list items
    ;; Only collapse a single blank line...multiple blank
    ;; lines separates multiple lists
    ;;
    (when twiki-debug (message "twiki-renumber-list: collapsing blank lines"))
    (goto-char (point-min))
    (while (re-search-forward
            ;; Looks for any amount of white space followed by twiki-bullet-renumber-regex
              (format "^ +%s.*$" twiki-bullet-renumber-regex) 
              nil t)
      
      (unless (twiki-skip-past-blocks)
        ;; while the next-line is blank followed by a bullet line, collapse
        (cond 
         ((looking-at (format "\\(\n *\\)\n\\( \\)+%s " twiki-bullet-renumber-regex))
          (replace-match (if to-numbers "\n" "") nil nil nil 1)
          ;;(replace-match (if (or (not (eq major-mode 'twiki-mode))
          ;;                       (not twiki-importing))
          ;;                   "" "\n") 
          ;; nil nil nil 1)
          (end-of-line)
          )

         ((looking-at (format "\\(\\(\n *\\)\\{2,\\}\\)\n +%s " twiki-bullet-renumber-regex))
          (replace-match "\n" nil nil nil 1)
          (end-of-line))
         )
        )
      )

    ;; Renumber
    (when twiki-debug (message "Renumbering"))
    (goto-char (point-min))
    
    (while (re-search-forward
            ;; Looks for any amount of white space followed by twiki-bullet-renumber-regex
            (format "^\\(\\(%s\\)+\\(%s\\)\\( \\)\\).*$" 
                    (make-string twiki-tab-width ? )
                    twiki-bullet-renumber-regex)
            nil t)
    
      (when twiki-debug (message "twiki-renumber-list, continuing renumber"))
      (unless (twiki-skip-past-blocks)
        (beginning-of-line)
        (twiki-renumber-cur-list to-numbers)
        )
      )
    )
  )

;;
;; twiki-renumber-cur-list
;;
;; Assumes that the list is in "twiki" format
;;
(defun twiki-renumber-cur-list (to-numbers &optional parent-depth parent-indent add-newline)
  (interactive "sFormat: ")
  "Renumber the current list starting at point until the end of nested lists."

  ;; Renumber
  (when twiki-debug (message "twiki-renumber-cur-list %s %s %s %s" 
                             to-numbers parent-depth parent-indent add-newline))
  (beginning-of-line)
  (let ((cur-indent 0)
        cur-depth
        (num 1)
        (bulletstr nil)
        (start t)
        )
    (if (not parent-depth)
        (setq parent-depth 0
              parent-indent 0))
    
    (setq cur-depth (1+ parent-depth))

    (beginning-of-line)
    
    (when twiki-debug
      (looking-at "^.*$")
      (message "twiki-renumber-cur-list: %s" (match-string 0)))

    (while (looking-at
            ;; Looks for parent-depth followed by twiki-bullet-renumber-regex
            (format "^\\(%s\\(?:%s\\)+\\)%s\\( +\\)\\S *.*$" 
                    (make-string parent-indent ? ) 
                    (make-string twiki-tab-width ? )
                    twiki-bullet-renumber-regex))
      
      (when twiki-debug (message "looking-at %s" (match-string 0)))

      (when (and (not start) 
                 to-numbers)
        (setq add-newline t))
      
      (setq start nil)

      (let ((this-indent (- (match-end 1) (match-beginning 1)))
            (matches (match-data)) 
            indent-str)

        (if (= cur-indent 0) (setq cur-indent this-indent))

        (if (null bulletstr) (setq bulletstr (match-string 2)))

        (when twiki-debug (message "cur-indent %s, this-indent %s" cur-indent this-indent))

        (if (> this-indent cur-indent)
            ;; Handle sub-list
            (twiki-renumber-cur-list to-numbers cur-depth cur-indent t)

          ;; Renumber active bullet / list item
          (when twiki-debug (message "bulletstr: %s" bulletstr))

          (let ((case-fold-search nil))
            (setq bulletstr
                  (cond
                   ((string= "*" bulletstr) "*")
                     
                   ((string-match "^[0-9]+" bulletstr)
                    (if to-numbers (twiki-format-number num "1") "1."))
                  
                   ((string-match "^[xvi]+." bulletstr)
                    (if to-numbers (twiki-format-number num "i") "i."))
                   
                   ((string-match "^[a-h]." bulletstr)
                    (if to-numbers (twiki-format-number num "a") "a."))
                   
                   ((string-match "^[A-H]." bulletstr)
                    (if to-numbers (twiki-format-number num "A") "A."))

                   (t
                    (twiki-format-number num (substring "1Aai1111" parent-depth 
                                                       (1+ parent-depth))))))
            )
            
          (set-match-data matches)

          ;; reduce space after bullet marker to one space
          (replace-match " " nil nil nil 3)

          ;; put in bullet marker
          (replace-match bulletstr nil nil nil 2)

          (setq indent-str 
                (make-string (* (if to-numbers twiki-tab-width 3) cur-depth) ? ))

          (replace-match (format "%s%s" 
                                 (if (and add-newline to-numbers) "\n" "")
                                 indent-str)
                         nil nil nil 1)

          ;; Split long lines
          (when to-numbers
            (let ((bol (point-at-bol)) ;; beginning of line
                  eol (done nil)       ;; end of line
                  fc                   ;; fill-column
                  (ic (+ 1 (length bulletstr) (length indent-str))))
              (end-of-line)
              (setq eol (point-marker))
              (while (and (not done) 
                          (> (current-column) fill-column))
                (backward-char (- (current-column) fill-column))

                ;; Cursor is now at the fill-column. 
                (let ((fc (point))
                      (lsf (save-excursion (re-search-forward "\\[\\[" eol t))) ;; link start forward
                      (lef (save-excursion (re-search-forward "\\]\\]" eol t))) ;; link end forward
                      (lsb (save-excursion (re-search-backward " *\\[\\[" bol t))) ;; link start backward
                      (web (save-excursion (re-search-backward " +" bol t))) ;; word end backward
                      (wef (save-excursion (re-search-forward " +" eol t))) ;; word end forward
                      )
                  (cond

                   ;; Test for middle of a link
                   ((and lef ;; end of link in front of us...
                         (or (not lsf) ;; and there is no link start, 
                             (> lsf lef)) ;; or its after the link end (a diff link)
                         lsb) ;; and there's is a link start behind us

                    ;; If the link start is at the indent column go to the end of the link
                    ;; else go to the beginning of the link
                    (cond
                     ((> lsb (+ bol ic))
                      (goto-char lsb)
                      (re-search-forward " *" eol t))
                     
                     ((< lef eol)
                      (goto-char lef)
                      (re-search-forward " *" eol t))
                     
                     (t (setq done t))))
                   
                   ;; Go to end of word backward as long as it's not the first workd
                   ((> web (+ bol ic))
                    (goto-char web)
                    (re-search-forward " *" eol t))

                   (wef
                    (goto-char wef)
                    (re-search-forward " *" eol t))

                   (t (setq done t))
                   )
                  
                  (unless done
                    (replace-match 
                     (format "\n%s" (make-string ic ? ))
                     eol)
                    (end-of-line)
                    (setq eol (point-marker))
                    (setq bol (point-at-bol)))))

              (end-of-line)

              ;; Blank line between bullets
              ;;(insert "\n")
              )
            )

          ;; Reset to next item
          (let ((line (line-number-at-pos)))
            (forward-line)
            (if (= (line-number-at-pos) line)
                (progn 
                  (end-of-line)
                  (insert "\n")
                  )
              )
            )
          (beginning-of-line)
          (setq num (1+ num))
          )
        )
      )
    )
  )

(defun twiki-skip-past-links (&optional rev)
  "Move point to the end of a link if the point only if the point
is in the middle of a link"
  (let* ((p (point))
        (le (re-search-forward "\\]\\]" (point-at-eol) t))
        (be (re-search-backward "\\[\\[" p t)))
    (if (and le (not be))
        (if rev (goto-char be) (goto-char le))
      (goto-char p))))



;;
;; twiki-format-number 
;;
(defun twiki-format-number (num fmt)
  (cond
   ((string= fmt "-") "-")

   ((string= fmt "1") 
    (format "%d." num))

   ((string= fmt "i") 
    (cond 
     ((= num 1) "i.")
     ((= num 2) "ii.")
     ((= num 3) "iii.")
     ((= num 4) "iv.")
     ((= num 5) "v.")
     ((= num 6) "vi.")
     ((= num 7) "vii.")
     ((= num 8) "viii.")
     ((= num 9) "ix.")
     ((= num 10) "x.")))
   
   ((string= fmt "a")
    (format "%c." (+ num ?a -1)))
   
   ((string= fmt "A")
    (format "%c." (+ num ?A -1)))
   
   (t "-"))
  )

;;
;; twiki-import-for-edit ()
;;
(defun twiki-import-for-edit (make-buf)
  "Import the text in the clipboard into current buffer, render for editing,
and turn on twiki-mode.  This obliterates the contents of the buffer."
  (interactive 
   (list (cond
          ((not (eq major-mode 'twiki-mode)) t)
          (twiki-silent-import nil)
          ((y-or-n-p "Use current buffer (will overwrite contents)? ") nil)
          (t t))))

  (when twiki-debug (message "twiki-import-for-edit"))
  (let ((twiki-importing t))
    (if make-buf (switch-to-buffer (generate-new-buffer "twiki")))

    (when twiki-debug (message "twiki-import buffer: %S" (buffer-file-name)))
    (erase-buffer)
    (clipboard-yank)
    (if (eq major-mode 'twiki-mode)
        (twiki-render-for-edit)
      (twiki-mode)
      )

    (if make-buf 
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "\\(-+\\) \\(.*\\) \\1" nil t)
              (write-file (format "/tmp/%s.twiki" 
                                  (replace-regexp-in-string "[^a-zA-Z0-9]+" "_" 
                                                            (match-string 2))))
            (write-file (format "%s.twiki" (make-temp-file "/tmp/")))
            )
          )
      )
    )
  (current-buffer)
  )

(defun twiki-prev-line-syntax ()
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (twiki-line-syntax)))

;;
;; twiki-line-syntax ()
;; 
;; Return a 3-tuple of the (syntax line-start text-start)
;;
;; For a bullet / list item, line-start is the bullet, text-start is
;; the beginning of text after the bullet.  Otherwise they are the same
;;
(defun twiki-line-syntax ()
  (save-excursion
    (let ((syntax 'twiki-syntax-other)
          start
          text-start)
      
      ;; Look at current line
      (beginning-of-line)
      (cond
       ;; Heading
       ((looking-at (twiki-heading-regex))
        (setq 
         start 0
         text-start (- (match-beginning 6) (match-beginning 0))
         syntax 'twiki-syntax-heading)
        )

       ;; Link
       ((looking-at "^\\({{.*}}\\)$")
        (setq 
         start (match-beginning 1)
         text-start (match-beginning 1)
         syntax 'twiki-syntax-link)
        )

       ;; Bullet
       ((looking-at (format "^\\( *\\)%s \\(.*\\)$" twiki-bullet-regex))
        (setq 
         start (- (match-end 1) (match-beginning 1))
         text-start (- (match-beginning 3) (match-beginning 0))
         syntax 'twiki-syntax-bullet)
        )

       ;; Table
       ((looking-at (format "^ *\\([|].*\\) *$"))
        (setq
         start 0
         text-start (- (match-end 1) (match-beginning 0))
         syntax 'twiki-syntax-table-line))

       ;; Just whitespace 
       ((looking-at (format "^\\( *\\)$"))
        (setq start (- (match-end 1) (match-beginning 1)))
        (setq text-start start
              syntax 'twiki-syntax-whitespace
              )
        )
       
       ;; Just text with whitespace in front
       ((looking-at (format "^ *\\(.*\\)$"))
        (setq start (- (match-beginning 1) (match-beginning 0)))
        (setq text-start start)
        )
       )

      (when twiki-debug (message "Cur-line syntax (before block-tags): %S" syntax))

      ;; Just in case, reverse search for anything that might 
      ;; be a block
      (if (re-search-backward (format "^<\\(/?\\)%s.*>" (regexp-opt twiki-block-tags t)) nil t)
          ;; If the first block tag found does *not* contain a '/', we must still be
          ;; in the block -- ie <php> vs </php>
          (if (string= (match-string 1) "")
              (setq syntax 'twiki-syntax-block)))

      (when twiki-debug (message "Cur-line syntax (after block-tags): %S" syntax))

      (list syntax start text-start)
      )
    )
  )
      
;;
;; Indent the current line according to context
;;  
(defun twiki-indent-line (&optional unindent)
  "Indent the current line"
  (interactive)
  (let ( (cur-line (point-marker))
         cur-line-info cur-syn cur-start cur-text 
         prev-line-info prev-syn prev-start prev-text
         (prev-blank-lines -1)
         (m 0)
         cur-line-str
         )
    
    ;; Look at current line
    (beginning-of-line)
    (looking-at "^.*$")
    (setq cur-line-str (match-string 0))
    (when twiki-debug (message "twiki-indent-line: '%s'" cur-line-str))
    (beginning-of-line)
    (setq cur-line-info (twiki-line-syntax))

    ;; Look for prev line
    (let ((done nil))
      (while (not done)
        (if (= (forward-line -1) -1)
            (setq done t)
          (beginning-of-line)
          (incf prev-blank-lines)
          (setq done (not (looking-at "^ *$"))))
        )
      )
    
    (setq prev-line-info (twiki-line-syntax))

    ;; Calculate indent of current line
      
    (setq cur-syn (car cur-line-info))
    (setq cur-start (cadr cur-line-info))

    (setq prev-syn (car prev-line-info))
      
    ;; Look at syntax of current / prev line to decide what to do
    (cond
     ((eq cur-syn 'twiki-syntax-heading)
      (goto-char cur-line)
      (setq twiki-subtree-closed 
            (save-excursion 
              (beginning-of-line 2)
              (if (get-char-property (point) 'invisible)
                  t nil)))
      (if unindent
          (outline-toggle-children)
        (if twiki-subtree-closed
            (show-subtree)
          (hide-subtree))
        )
      )

     ;; If current-line is in the middle of a block, do nothing but
     ;; tab-stops
     ((eq cur-syn 'twiki-syntax-block)
      (when twiki-debug (message "cur syntax block"))
      (setq m (twiki-calc-indent 
               cur-start unindent
               (list 0) twiki-tab-width fill-column))
      )
     
     ((eq cur-syn 'twiki-syntax-bullet)
      (when twiki-debug (message "cur syntax bullet"))
      ;; Bullet, just move to next / prev tab stop
      (setq m (twiki-calc-indent 
               cur-start unindent
               (list 0) twiki-tab-width 
               (+ (cadr prev-line-info) twiki-tab-width)))
      )

     ((eq prev-syn 'twiki-syntax-bullet)
      (when twiki-debug (message "prev syntax bullet"))
      ;; Current line is not a bullet, but prev was
      
      ;; Indent works like continuation of bullet,
      ;; Unindent moves backward
      (let ((stops
             (if (or (eq cur-syn 'twiki-syntax-whitespace)
                     (eq cur-syn 'twiki-syntax-other))
                 (list 0 (caddr prev-line-info))
               (list 0 (caddr prev-line-info)))))
        (setq m (twiki-calc-indent cur-start unindent stops 0))
        )
      )

     (t
      (when twiki-debug (message "default case"))
      (setq m (twiki-calc-indent cur-start unindent (list 0 (cadr prev-line-info)) 0))
      )
     )

    (when twiki-debug (message "twiki-indent-line: %S,%S => %d" prev-line-info cur-line-info m))

    (goto-char cur-line)
    (beginning-of-line)
    (if (looking-at "^\\( +\\)")
        (replace-match "" 1))
      
    (if (> m 0)
        (insert (make-string m ? )))

    (if (> (marker-position cur-line) (point))
        (goto-char cur-line))
    )
  
  )

;;
;; twiki-unindent-line
;;
(defun twiki-unindent-line ()
  (interactive)
  (twiki-indent-line t))

;;
;; twiki-calc-indent-from-stops
;;
(defun twiki-calc-indent-from-stops (col unindent stops)
  (cond
   ((= (length stops) 1) 
    (car stops))

   (unindent
    (if (> col (cadr stops))
        (twiki-calc-indent-from-stops col unindent (cdr stops))
      (car stops)))

   (t
    (if (< col (car stops))
        (car stops)
      (twiki-calc-indent-from-stops col unindent (cdr stops))))
   )
  )
      
;;
;; twiki-calc-indent
;;
(defun twiki-calc-indent (col unindent hard-stops tab-stop &optional max-tab-stop)
  (when twiki-debug (message "twiki-calc-indent: col(%d) hard(%S) tab(%S)" col hard-stops tab-stop))
  (let ((all-stops hard-stops) (s tab-width) r)
    (while (and (> tab-stop 0) 
                (<= s (+ col tab-stop))
                (or (null max-tab-stop) (<= s max-tab-stop)))
      (setq all-stops (append all-stops (list s)))
      (setq s (+ s tab-stop)))
    (setq r (twiki-calc-indent-from-stops col unindent (sort all-stops '<)))
    (when twiki-debug (message "stops: %d @ %S => %d" col (sort all-stops '<) r))
    r
    )
  )

;;
;; twiki-number-headings
;;
(defun twiki-number-headings (min-level base-str start-num)
  "Number headings in the current document.

The first argument MIN-LEVEL represents the minimun heading level
to number.  Headings above (smaller number) than this level will
not be touched.  All headings at this level and lower (greater than
or equal by number) will be numbered."

  (interactive "nFirst heading level to number : 
sHeading Prefix String (usually empty): 
nFirst heading number (typically 1): ")

  (setq twiki-min-heading-level min-level)
  (setq twiki-start-heading-number start-num)
  (setq twiki-heading-base-string base-str)

  (twiki-renumber-headings)
)

;;  
;; twiki-setup-heading-numbers
;;
(defun twiki-setup-heading-numbers (&optional ask)
  (interactive "p")
  "Set the minimum and maximum levels for renumbering headings"
  (if (not twiki-min-heading-level)
      (setq twiki-min-heading-level 1))
  (setq twiki-min-heading-level
        (string-to-number (read-string "Min heading level [1-6]: " 
                                       (int-to-string twiki-min-heading-level))))
  (if (not twiki-max-heading-level)
      (setq twiki-max-heading-level 6))
  (setq twiki-max-heading-level
        (string-to-number (read-string "Max heading level [1-6]: " 
                                       (int-to-string twiki-max-heading-level))))

  (setq twiki-start-heading-number 1)
  (when (or (not ask) (y-or-n-p "Renumber the document now? "))
    (twiki-renumber-headings))
  )
  
;;
;; twiki-renumber-headings
;;
(defun twiki-renumber-headings (&optional ask)
  (interactive "p")
  "Renumber all headings in the document up to and including twiki-max-heading-level. 
Headings are prefixed with 1.1.1 notation."

  (if (null twiki-start-heading-number)
      (if ask (twiki-setup-heading-numbers))

    (save-excursion
      (goto-char (point-min))
      (when twiki-debug (message "twiki-renumber-headings"))
      
      (when (null twiki-heading-base-string)
        (setq twiki-heading-base-string ""))
      (let ((is-first t))
        (twiki-renumber-headings-internal nil twiki-heading-base-string)
        )
      )
    )
  )

;;
;; twiki-renumber-all
;; 
(defun twiki-renumber-all ()
  (interactive)
  "Renumber all headings and tables in the document."
  (twiki-renumber-headings)
  (twiki-renumber-tables)
)

;;
;; twiki-renumber-headings-internal
;;
(defun twiki-renumber-headings-internal (&optional level parent-base)
  (let ((num 1)
        (num-str parent-base)
        (done nil))
    
    (if (null level) 
        (setq level 1))
              
    (when twiki-debug (message "renumbering: level %d, num %d, base %S" level num parent-base))

    (if (null num-str) (setq num-str ""))

    ;; !!!
    (while (and (not done)
                (re-search-forward (twiki-heading-regex) nil t)
                )
      (when twiki-debug (message "line: %s (!! match: %s)" (match-string 0) (match-string 2)))
      (let ((match-level (length (match-string 1))))
        (cond
         ((> (length (match-string 2)) 0)
          (when twiki-debug (message "skipping header line with !!"))
          )

         ((> match-level level)
          
          (when twiki-debug (message "descending to deeper level"))
          (beginning-of-line)
          (twiki-renumber-headings-internal  (1+ level) num-str))
         
         ((< match-level level)
          (beginning-of-line)
          (setq done t))
         
         (t
          (when twiki-debug (message "heading: %d - %s" match-level (match-string 6)))
          
          (cond 
           ((or (< level twiki-min-heading-level)
                (> level twiki-max-heading-level))
            (replace-match " " nil nil nil 3)
            )
           
           (t
            (if (null twiki-start-heading-number)
                (setq twiki-start-heading-number 1))
            (if is-first
                (setq is-first nil
                      num twiki-start-heading-number)) 
            (when twiki-debug (message "renumbering to %S %S" parent-base num))
            (setq num-str (format "%s%d." parent-base num))
            (replace-match (format " %s " num-str)  nil nil nil 3))
           )
          
          (end-of-line)
          (if (> (forward-line 1) 0)
              (progn 
                (when twiki-debug (message "end-of-buffer detected"))
                (end-of-line)
                (insert "\n")))
          (beginning-of-line)
          (incf num)
          )
         )
        )
      )
    )
  )

;;
;; twiki-electric-space
;;
(defun twiki-electric-space ()
  (interactive)

  (let ((syntax (twiki-line-syntax))
        (prev-syntax (twiki-prev-line-syntax)))
    (cond
     ((or (eq (car syntax) 'twiki-syntax-block)
          (eq (car syntax) 'twiki-syntax-table-line)
          (eq (car syntax) 'twiki-syntax-heading)
          )
      (call-interactively 'self-insert-command)
      )

     ;; User is at the end of a line
     ((and (looking-at "\\( *\\)$")
           (save-excursion
             (beginning-of-line)
             (looking-at (format "^\\( *\\)\\(%s\\) *$" twiki-bullet-regex))))
    
      ;; starting a new bullet, indent to match the previous bullet,
      ;; or if none, then just a new bullet list
      (save-excursion
        (let ((bullet-mark (match-string 2))
              (matches (match-data))
              (indent-len 0)
              (add-newline "\n"))
          (beginning-of-line)
          (forward-line -1)
          (while (looking-at "^ *$")
            (setq add-newline "")
            (forward-line -1))
          (if (looking-at (format "^\\( *\\(%s\\)? *\\)" twiki-bullet-regex))
              (setq indent-len (- (length (match-string 1)) (length bullet-mark) 1)))
          (if (< indent-len twiki-tab-width)
              (setq indent-len twiki-tab-width))
          (set-match-data matches)
          (replace-match (format "%s%s" add-newline (make-string indent-len ? )) nil nil nil 1)
          )
        )
      (call-interactively 'self-insert-command)
      )
   
     ;; Check if cursor is in the middle of a link [[linkref][text]], and if so
     ;; just insert a space
     ((save-excursion
        (let ((c (current-column)))
          (beginning-of-line)
          (if (looking-at ".*\\(\\[\\[.*\\]\\]\\)")
              (progn 
                (goto-char (match-end 1))
                (< c (current-column)))
            nil)))
      (call-interactively 'self-insert-command)
      )
     
     ;; Make sure user did not just insert a bullet that happens to be past the 
     ;; fill column.  
     ((save-excursion 
        (if (looking-at "[^\n ]*")
            (goto-char (match-end 0)))
        (> (current-column) fill-column))
      (insert "\n")
      (twiki-indent-line)
      )
   
     (t
      (call-interactively 'self-insert-command)
      )
     )
    )
  )

;;
;; twiki-debug
;;
(defun twiki-debug ()
  "Toggle twiki debug messages" 
  (interactive)
  (setq twiki-debug (not twiki-debug))
  (message "Twiki debug %s" (if twiki-debug "enabled" "disabled")))

(defun twiki-renumber-tables ()
  "Renumber tables in the entire file.  This looks for the following text:
     *Table 1:
  "
  (interactive)
  (save-excursion
    (let ((num 1))
      (goto-char (point-min))
      (while (re-search-forward "\\*Table \\([0-9]+\\):" nil t)
        (replace-match (format "%d" num) nil t nil 1)
        (setq num (1+ num))
        )
      )
    )
  )

(defun twiki-heading-regex ()
  "Returns a regex that matches a heading with the following groups defined:
  1 - All '+' symbols representing heading depth
  2 - '!!' or '' representing TOC suppression
  3 - groups 4 + 5
  4 - heading base string, if used
  5 - heading numbering
  6 - heading text"
  (format "^---\\(\\++\\)\\(!*\\)\\( *\\(%s\\)?\\([.0-9]+\\. *\\)?\\)\\(.*\\) *"
          twiki-heading-base-string))

(defun twiki-preview ()
  "Invokes the external shell command to preview this file"
  (interactive)
  (save-buffer)
  (message "Executing twiki preview...")
  (shell-command (format "%s -n preview %s" twiki-shell-cmd buffer-file-name))
  )

(defun twiki-save ()
  "Invokes the external shell command to save this file"
  (interactive)
  (when (y-or-n-p "Save file as new version without preview? ")
    (save-buffer)
    (message "Executing twiki save...")
    (let ((r (shell-command (format "%s -n save %s" twiki-shell-cmd buffer-file-name))))
      (if (= r 0)
          (revert-buffer t t)
        )
      )
    )
  )

(defun twiki-update ()
  "Invokes the external shell command to update this file"
  (interactive)
  (message "Executing twiki update...")
  (let ((r (shell-command (format "%s -n update %s" twiki-shell-cmd buffer-file-name))))
    (if (= r 0)
        (revert-buffer t t))
    )
  )

(defun twiki-status ()
  "Invokes the external shell command to return the status of this file"
  (interactive)
  (message "Executing twiki status...")
  (shell-command (format "%s -n status %s" twiki-shell-cmd buffer-file-name)))

(defun twiki-diff ()
  "Invokes the external shell command to diff this file"
  (interactive)
  (message "Executing twiki diff...")
  (let ((f buffer-file-name)
        (buf (get-buffer-create (format "*%s diff*" (buffer-name)))))
    (pop-to-buffer buf)
    (erase-buffer)
    (shell-command (format "%s -n diff %s" twiki-shell-cmd f) t)
    (diff-mode)))


(defun twiki-open-topic (filename)
  "Retrieve a new topic from the server"
  (interactive "FTopic file: ")
  (message "Executing twiki update...")
  (let ((r (shell-command (format "%s -n update %s" twiki-shell-cmd filename)))
        (twiki-filename filename))
    (unless (string-match "\.twiki$" filename)
      (setq twiki-filename (format "%s.twiki" filename)))
    (if (= r 0)
        (find-file twiki-filename))
    )
  )
