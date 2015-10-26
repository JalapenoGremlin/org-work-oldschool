;;; ox-tracwiki.el --- Org Export Backend to Trac WikiFormat  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Brian J. Carlson

;; Author: Brian J. Carlson <bcarlson@paradigm4.com>
;; Keywords: hypermedia, docs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Trac WikiFormat back-end for
;; Org exporter, based on `md' back-end.  See Org manual for more
;; information. 

;;; Code:



(eval-when-compile (require 'cl))
(require 'ox-md)
;;(require 'ox-ascii)
(require 'ox-publish)
(require 'rx)


;;; User-Configurable Variables

(defgroup org-export-tracwiki nil
  "Options specific to Tracwiki export back-end."
  :tag "Org Markdown"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-tracwiki-headline-style 'atx
  "Style used to format headlines.
This variable can be set to either `atx' or `setext'."
  :group 'org-export-tracwiki
  :type '(choice
	  (const :tag "Use \"atx\" style" atx)
	  (const :tag "Use \"Setext\" style" setext)))

(defcustom org-tracwiki-lang-alist '(("emacs-lisp" . "elisp") )
  "Alist of languages that are not recognized by Github, to
  languages that are. Emacs lisp is a good example of this, where
  we can use lisp as a nice replacement."
  :group 'org-export-tracwiki)

(defcustom org-tracwiki-footnote-separator "^, ^"
  "Text used to separate footnotes."
  :group 'org-export-tracwiki
  :type 'string)


;;; Define Back-End



(org-export-define-derived-backend
 'tracwiki 'ascii
 ;; :export-block '("MD" "MARKDOWN")
 :filters-alist '((:filter-parse-tree . org-tracwiki-separate-elements))
 :menu-entry
 '(?T "Export to Tracwiki Wiki Formatting"
      ((?T "To temporary buffer"
           (lambda (a s v b) (org-tracwiki-export-as-markdown a s v)))
       (?t "To file" (lambda (a s v b) (org-tracwiki-export-to-markdown a s v)))
       (?o "To file and open"
           (lambda (a s v b)
             (if a (org-tracwiki-export-to-markdown t s v)
               (org-open-file (org-tracwiki-export-to-markdown nil s v)))))))
 :translate-alist
 '((bold . org-tracwiki-bold)
   ;; center-block
   ;; clock
   (code . org-tracwiki-verbatim)
   ;; drawer
   ;; dynamic-block
   (entity . org-tracwiki-entity)
   (example-block . org-tracwiki-example-block)
   (export-block . org-tracwiki-export-block)
   ;; (export-snippet . org-tracwiki-export-snippet)
   (fixed-width . org-tracwiki-example-block)
   (footnote-definition . org-tracwiki-footnote-definition)
;;  (footnote-definition . org-html-footnote-definition)
   (footnote-reference . org-tracwiki-footnote-reference)
   ;;(footnote-reference . org-html-footnote-reference)
   (headline . org-tracwiki-headline)
   ;;(horizontal-rule . org-tracwiki-horizontal-rule)
   (inline-src-block . org-tracwiki-verbatim)
   ;; (inlinetask . org-tracwiki-inlinetask)
   (inner-template . org-tracwiki-inner-template)
   (italic . org-tracwiki-italic)
   (item . org-tracwiki-item)
   (keyword . org-tracwiki-keyword)
   ;; (latex-environment . org-tracwiki-latex-environment)
   (latex-fragment . org-tracwiki-latex-fragment)
   (line-break . org-tracwiki-line-break)
   (link . org-tracwiki-link)
   (node-property . org-tracwiki-node-property)
   (paragraph . org-tracwiki-paragraph)
   (plain-list . org-tracwiki-plain-list)
   (plain-text . org-tracwiki-plain-text)
   (property-drawer . org-tracwiki-property-drawer)
   (quote-block . org-tracwiki-quote-block)
   ;; (radio-target . org-tracwiki-radio-target)
   (section . org-tracwiki-section)
   ;; (special-block . org-tracwiki-special-block)
   (src-block . org-tracwiki-src-block)
   ;; (statistics-cookie . org-tracwiki-statistics-cookie)
   ;; (strike-through . org-tracwiki-strike-through)
   (subscript . org-tracwiki-subscript)
   (superscript . org-tracwiki-superscript)
   (table . org-tracwiki-table)
   (table-cell . org-tracwiki-table-cell)
   (table-row . org-tracwiki-table-row)
   ;; (target . org-tracwiki-target)
   (template . org-tracwiki-template)
   ;; (timestamp . org-tracwiki-timestamp)
   (underline . org-tracwiki-underline)
   (verbatim . org-tracwiki-verbatim)
   ;; (verse-block . org-tracwiki-verse-block)
   )
 :options-alist
 '((:tracwiki-headline-style nil nil org-tracwiki-headline-style)
   (:tracwiki-footnote-separator nil nil org-tracwiki-footnote-separator)))


;;; Filters

(defun org-tracwiki-separate-elements (tree backend info)
  "Fix blank lines between elements.

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Enforce a blank line between elements.  There are two exceptions
to this rule:

  1. Preserve blank lines between sibling items in a plain list,

  2. In an item, remove any blank line before the very first
     paragraph and the next sub-list when the latter ends the
     current item.

Assume BACKEND is `tracwiki'."
  (org-element-map tree (remq 'item org-element-all-elements)
    (lambda (e)
      (org-element-put-property
       e :post-blank
       (if (or (and (eq (org-element-type e) 'paragraph)
		(eq (org-element-type (org-element-property :parent e)) 'item)
		(org-export-first-sibling-p e info)
		(let ((next (org-export-get-next-element e info)))
		  (and (eq (org-element-type next) 'plain-list)
		       (not (org-export-get-next-element next info)))))
               (eq (org-element-type e) 'table-row)
               )
	   0
	 1))))
  ;; Return updated tree.
  tree)



;;; Transcode Functions

;;;; Bold

(defun org-tracwiki-bold (bold contents info)
  "Transcode BOLD object into Markdown format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "**%s**" contents))

(defun org-tracwiki-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to TRACWIKI.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "^%s^" contents))

(defun org-tracwiki-subscript(subscript contents info)
  "Transcode a SUBSCRIPT object from Org to TRACWIKI
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format ",,%s,," contents))
  

;;;; Code and Verbatim

(defun org-tracwiki-verbatim (verbatim contents info)
  "Transcode VERBATIM object into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value verbatim)))
    (format (cond ((not (string-match "`" value)) "`%s`")
		  ((or (string-match "\\``" value)
		       (string-match "`\\'" value))
		   "`` %s ``")
		  (t "``%s``"))
	    value)))

;;; Entity
(defun org-tracwiki-entity (entity contents info)
  "Transcode an ENTITY object from Org to TracWiki.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (concat "\\(" (org-element-property :latex entity) "\\)" ))


;;;; Example Block, Src Block and export Block

(defun org-tracwiki-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  ;; (concat (replace-regexp-in-string
  ;;          "^" "{{{\n"
  ;;          (org-remove-indentation
  ;;           (org-export-format-code-default example-block info))) "\n}}}")
  (concat  "{{{\n" (org-remove-indentation
                    (org-export-format-code-default example-block info)) "\n}}}")
  )

(defun org-tracwiki-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (member (org-element-property :type export-block) '("MARKDOWN" "MD"))
      (org-remove-indentation (org-element-property :value export-block))
    ;; Also include HTML export blocks.
    (org-export-with-backend 'html export-block contents info)))


;;;; Headline

(defun org-tracwiki-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (format "     :%s:"
				     (mapconcat 'identity tag-list ":"))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   (anchor
	    (and (or (plist-get info :with-toc)
                     (org-element-property :CUSTOM_ID headline))
		 (format " #%s"
			 (or (org-element-property :CUSTOM_ID headline)
			     (org-export-get-reference headline info)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title))
	   (style (plist-get info :tracwiki-headline-style)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
	    (not (memq style '(atx setext)))
	    (and (eq style 'atx) (> level 6))
	    (and (eq style 'setext) (> level 2)))
	(let ((bullet
	       (if (not (org-export-numbered-headline-p headline info)) "-"
		 (concat (number-to-string
			  (car (last (org-export-get-headline-number
				      headline info))))
			 "."))))
	  (concat bullet (make-string (- 4 (length bullet)) ?\s) heading tags
		  "\n\n"
		  (and contents
		       (replace-regexp-in-string "^" "    " contents)))))
       ;; Use "Setext" style.
       ((eq style 'setext)
	(concat heading tags anchor "\n"
		(make-string (length heading) (if (= level 1) ?= ?-))
		"\n"
		contents))
       ;; Use "atx" style.
       (t (concat (make-string level ?=) " " heading tags anchor "\n"
		  contents))))))


;;;; Horizontal Rule

;; (defun org-tracwiki-horizontal-rule (horizontal-rule contents info)
;;   "Transcode HORIZONTAL-RULE element into Markdown format.
;; CONTENTS is the horizontal rule contents.  INFO is a plist used
;; as a communication channel."
;;   "---")


;;;; Italic

(defun org-tracwiki-italic (italic contents info)
  "Transcode ITALIC object into Markdown format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "//%s//" contents))


;;;; Item

(defun org-tracwiki-item (item contents info)
  "Transcode ITEM element into Markdown format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
	 (struct (org-element-property :structure item))
	 (bullet (if (not (eq type 'ordered)) "*"
		   (concat (number-to-string
			    (car (last (org-list-get-item-number
					(org-element-property :begin item)
					struct
					(org-list-prevs-alist struct)
					(org-list-parents-alist struct)))))
			   ".")))
         (bullet-length (length bullet)))
    ;; TODO This is ugly. 'descriptive was barnacled onto this
    (cond ((eq type 'descriptive)
           (let ((tag (org-element-property :tag item)))
             (concat " " (or (org-export-data tag info) "(no_tag)") ":: "
                   (and contents
                        (org-trim (replace-regexp-in-string "^" (make-string (+ 3 (length tag)) ? ) contents))))))
                     
          (t 
           (concat bullet
                   " "
                   (case (org-element-property :checkbox item)
                     (on "[X] ")
                     (trans "[-] ")
                     (off "[ ] "))
                   (let ((tag (org-element-property :tag item)))
                     (and tag (format "**%s:** "(org-export-data tag info))))
                   (and contents
                        (org-trim (replace-regexp-in-string "^" (make-string (1+ bullet-length) ? ) contents))))))))
;;;; Keyword

(defun org-tracwiki-keyword (keyword contents info)
  "Transcode a KEYWORD element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (if (member (org-element-property :key keyword) '("MARKDOWN" "MD"))
      (org-element-property :value keyword)
    (org-export-with-backend 'html keyword contents info)))


;;;; Line Break

(defun org-tracwiki-line-break (line-break contents info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  ;;(org-element-remove-indentation (format "\\\\(%s %S) \n" contents (org-get-indentation))))
  "\\\\\n")


;;;; Link
(defun org-tracwiki-link (link contents info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  ;; TODO: So FAR, this is Very rudimentary: slightly modified
  ;; org-md-link by only changing order of description and path
  (let ((link-org-files-as-tracwiki
	 (lambda (raw-path)
	   ;; Treat links to `file.org' as links to `file.md'.
	   (if (string= ".org" (downcase (file-name-extension raw-path ".")))
	       (concat (file-name-sans-extension raw-path) ".tracwiki")
	     raw-path)))
	(type (org-element-property :type link)))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link contents 'md))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  (plain-text			; External file.
	   (let ((path (funcall link-org-files-as-tracwiki destination)))
	     (if (not contents) (format "<%s>" path)
	       (format "[[%s|%s]]" path contents))))
	  (headline
	   (format
	    "[[#%s|%s]]"

            ;; Reference.
	    (or (org-element-property :CUSTOM_ID destination)
		(org-export-get-reference destination info))
            ;; Description.
	    (cond ((org-string-nw-p contents))
		  ((org-export-numbered-headline-p destination info)
		   (mapconcat #'number-to-string
			      (org-export-get-headline-number destination info)
			      "."))
		  (t (org-export-data (org-element-property :title destination)
				      info)))


            ))
	  (t
	   (let ((description
		  (or (org-string-nw-p contents)
		      (let ((number (org-export-get-ordinal destination info)))
			(cond
			 ((not number) nil)
			 ((atom number) (number-to-string number))
			 (t (mapconcat #'number-to-string number ".")))))))
	     (when description
	       (format "[[%s|#%s]]"
		       (org-export-get-reference destination info)
                       description)))))))
     ;; ((org-export-inline-image-p link org-html-inline-image-rules)
     ;;  (let ((path (let ((raw-path (org-element-property :path link)))
     ;;    	    (if (not (file-name-absolute-p raw-path)) raw-path
     ;;    	      (expand-file-name raw-path))))
     ;;        (caption (org-export-data
     ;;    	      (org-export-get-caption
     ;;    	       (org-export-get-parent-element link)) info)))
     ;;    (format "![img](%s)"
     ;;    	(if (not (org-string-nw-p caption)) path
     ;;    	  (format "%s \"%s\"" path caption)))))
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
	(format (org-export-get-coderef-format ref contents)
		(org-export-resolve-coderef ref info))))
     ((equal type "radio") contents)
     (t (let* ((raw-path (org-element-property :path link))
	       (path
		(cond
		 ((member type '("http" "https" "ftp"))
		  (concat type ":" raw-path))
		 ((string= type "file")
		  (org-export-file-uri (funcall link-org-files-as-tracwiki raw-path)))
		 (t raw-path))))
	  (if (not contents) (format "<%s>" path)
	    (format "[[%s|%s]]" path contents)))))))


;;;; Node Property

(defun org-tracwiki-node-property (node-property contents info)
  "Transcode a NODE-PROPERTY element into Markdown syntax.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))


;;;; Paragraph

(defun org-tracwiki-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (let ((first-object (car (org-element-contents paragraph))))
    ;; If paragraph starts with a #, protect it.
    (if (and (stringp first-object) (string-match "\\`#" first-object))
	(replace-regexp-in-string "\\`#" "\\#" contents nil t)
      contents)))


;;;; Plain List

(defun org-tracwiki-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element into Markdown format.
CONTENTS is the plain-list contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Plain Text

(defun org-tracwiki-plain-text (text info)
  "Transcode a TEXT string into Markdown format.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (setq text (replace-regexp-in-string "\\^" "!^" text))
  (let ((case-fold-search nil))
    (setq text (replace-regexp-in-string   (rx bow (group (and  upper (one-or-more lower) (+  (and upper (one-or-more lower))))) eow)
                                        "!\\1" text)))
  text)



;;;; Property Drawer

(defun org-tracwiki-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element into Markdown format.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (replace-regexp-in-string "^" "    " contents)))


;;;; Quote Block

(defun org-tracwiki-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK element into Markdown format.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (replace-regexp-in-string
   "^" "     "
   (replace-regexp-in-string "\n +'" " " contents)))


;;;; Section

(defun org-tracwiki-section (section contents info)
  "Transcode SECTION element into Markdown format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)

;;; Source Block
(defun org-tracwiki-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Github Flavored Markdown
format. CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((lang (org-element-property :language src-block))
         (lang (or (assoc-default lang org-tracwiki-lang-alist) lang))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "{{{\n#!" lang "\n"))
         (suffix "\n}}}"))
    (concat prefix code suffix)))


;;;; Template
(defun org-tracwiki--translate (s info)
  "Translate string S according to specified language.
INFO is a plist used as a communication channel."
  (org-export-translate s :default info))


(defun org-tracwiki--anchor (id desc attributes info)
  "Format a HTML anchor."
  ;; (let* ((name (and (plist-get info :html-allow-name-attribute-in-anchors) id))
  ;;        (attributes (concat (and id (format " id=\"%s\"" id))
  ;;       		     (and name (format " name=\"%s\"" name))
  ;;       		     attributes)))
  ;;   (format "<a%s>%s</a>" attributes (or desc "")))
    (format "[=%s][#%s %s]" id attributes desc)
    )

;; TODO
(defun org-tracwiki-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :tracwiki-footnote-separator)))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
	  (id (format "#fnr.%d%s"
		      n
		      (if (org-export-footnote-first-reference-p
			   footnote-reference info)
			  ""
			".100"))))
     (format
      "^%s^" ;; TODO fix me
      ;;(plist-get info :html-footnote-format)
      (org-tracwiki--anchor
       id n (format "fn.%d" n) info)))))

(defun org-tracwiki-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."

    (let* ((fn-alist (org-export-collect-footnote-definitions info))
	 (fn-alist
	  (loop for (n type raw) in fn-alist collect
		(cons n (if (eq (org-element-type raw) 'org-data)
			    (org-trim (org-export-data raw info))
			  (format "<div class=\"footpara\">%s</div>"
				  (org-trim (org-export-data raw info))))))))
    (when fn-alist
      (format
       ;;(plist-get info :html-footnotes-section)
       "= %s %s"
       (org-tracwiki--translate "Footnotes" info)
       (format
	"\n%s\n"
	(mapconcat
	 (lambda (fn)
	   (let ((n (car fn)) (def (cdr fn)))
	     (format
	      "<div class=\"footdef\">%s %s</div>\n"
	      (format
	       ;; (plist-get info :html-footnote-format)
               "^%s^"
	       (org-html--anchor
		(format "fn.%d" n)
		n
		(format " class=\"footnum\" href=\"#fnr.%d\"" n)
		info))
	      def)))
	 fn-alist
	 "\n"))))))

  

(defun org-tracwiki-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth "[[PageOutline]]\n"))
   ;; Document contents.
   "\n"
   contents
   "\n"
   ;; Footnotes section.
   ;; TODO
   ;;   (org-tracwiki-footnote-section info)
   (org-tracwiki-footnote-section info)
   ))

(defun org-tracwiki-template (contents info)
  "Return complete document string after Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)



;;; Interactive function

;;;###autoload
(defun org-tracwiki-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracwikiting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Tracwiki Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'tracwiki "*Org Trackwiki Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-tracwiki-convert-region-to-tracwiki ()
  "Assume the current region has org-mode syntax, and convert it to Markdown.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'md))


;;;###autoload
(defun org-tracwiki-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracwikiting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tracwiki" subtreep)))
    (org-export-to-file 'tracwiki outfile async subtreep visible-only)))

;;;###autoload
(defun org-tracwiki-publish-to-tracwiki (plist filename pub-dir)
  "Publish an org file to Markdown.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'md filename ".md" plist pub-dir))


(defun org-tracwiki-underline (underline contents info)
  "Transcode UNDERLINE from Org to HTML.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  ;; (format (or (cdr (assq 'underline (plist-get info :html-text-markup-alist)))
  ;;             "%s")
  ;;         contents)
  (format  "__%s__" contents)
  )

(defun org-tracwiki-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-element-property :value latex-fragment)))
    (setq value (replace-regexp-in-string "\\$\\([^$]+\\)\\$" "\\\\(\\1\\\\)"  value))))

(defun org-tracwiki-table (table contents info)
  (concat ;; "[TABLE]"
          contents
          ;;"[/TABLE]"
          ))

(defun org-tracwiki-table-row  (table-row contents info)
  (concat "||" contents)
  ;; (concat ;; "[TABLE_ROW]"
  ;;                   (if (org-string-nw-p contents) (format "|%s" contents)
  ;;                     "||")
  ;;                   (when (org-export-table-row-ends-header-p table-row info)
  ;;                     "||")
  ;;                   ;;"[/TABLE_ROW]\n\n\n"
  ;;                   )
  )

(defun org-tracwiki-table-cell  (table-cell contents info)
  (let ((table-row (org-export-get-parent table-cell)))
    (concat ;;"[TABLE_CELL]"
     (if (org-export-table-row-starts-header-p table-row info)
         "="
       )
     (if (eq (length contents) 0)
       " ")
     contents
     (if (org-export-table-row-starts-header-p table-row info)
         "=||" "||")
     ;;     "[/TABLE_CELL]"
     )))


(provide 'ox-tracwiki)
;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-tracwikiwiki.el ends here

