;;; ox-mrkup.el --- surround org export filters with helpful values  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Brian J. Carlson

;; Author: Brian J. Carlson <bcarlson@paradigm4.com>
;; Keywords: 

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

;; 

;;; Code:
(require 'ox-md)
(org-export-define-derived-backend 'md2 'md
  :menu-entry
  '(?B "Export to Tracwiki Wiki Formatting"
       ((?t "To temporary buffer"
	    (lambda (a s v b) (org-md-export-as-markdown a s v)))
	(?b "To file" (lambda (a s v b) (org-md-export-to-markdown a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-md-export-to-markdown t s v)
		(org-open-file (org-md-export-to-markdown nil s v)))))))
  :filters-alist
  '((:filter-body . ox-mrkup-filter-body)
    (:filter-bold . ox-mrkup-filter-bold)
    (:filter-babel-call . ox-mrkup-filter-babel-call)
    (:filter-center-block . ox-mrkup-filter-center-block)
    (:filter-clock . ox-mrkup-filter-clock)
    (:filter-code . ox-mrkup-filter-code)
    (:filter-comment . ox-mrkup-filter-comment)
    (:filter-comment-block . ox-mrkup-filter-comment-block)
    (:filter-diary-sexp . ox-mrkup-filter-diary-sexp)
    (:filter-drawer . ox-mrkup-filter-drawer)
    (:filter-dynamic-block . ox-mrkup-filter-dynamic-block)
    (:filter-entity . ox-mrkup-filter-entity)
    (:filter-example-block . ox-mrkup-filter-example-block)
    (:filter-export-block . ox-mrkup-filter-export-block)
    (:filter-export-snippet . ox-mrkup-filter-export-snippet)
    (:filter-final-output . ox-mrkup-filter-final-output)
    (:filter-fixed-width . ox-mrkup-filter-fixed-width)
    (:filter-footnote-definition . ox-mrkup-filter-footnote-definition)
    (:filter-footnote-reference . ox-mrkup-filter-footnote-reference)
    (:filter-headline . ox-mrkup-filter-headline)
    (:filter-horizontal-rule . ox-mrkup-filter-horizontal-rule)
    (:filter-inline-babel-call . ox-mrkup-filter-inline-babel-call)
    (:filter-inline-src-block . ox-mrkup-filter-inline-src-block)
    (:filter-inlinetask . ox-mrkup-filter-inlinetask)
    (:filter-italic . ox-mrkup-filter-italic)
    (:filter-item . ox-mrkup-filter-item)
    (:filter-keyword . ox-mrkup-filter-keyword)
    (:filter-latex-environment . ox-mrkup-filter-latex-environment)
    (:filter-latex-fragment . ox-mrkup-filter-latex-fragment)
    (:filter-line-break . ox-mrkup-filter-line-break)
    (:filter-link . ox-mrkup-filter-link)
    (:filter-node-property . ox-mrkup-filter-node-property)
    ;;   omit filter with different args
    ;;   (:filter-options . ox-mrkup-filter-options)
    (:filter-paragraph . ox-mrkup-filter-paragraph)
    ;;   omit filter with different args
    ;;   (:filter-parse-tree . ox-mrkup-filter-parse-tree)
    (:filter-plain-list . ox-mrkup-filter-plain-list)
    (:filter-plain-text . ox-mrkup-filter-plain-text)
    (:filter-planning . ox-mrkup-filter-planning)
    (:filter-property-drawer . ox-mrkup-filter-property-drawer)
    (:filter-quote-block . ox-mrkup-filter-quote-block)
    (:filter-radio-target . ox-mrkup-filter-radio-target)
    (:filter-section . ox-mrkup-filter-section)
    (:filter-special-block . ox-mrkup-filter-special-block)
    (:filter-src-block . ox-mrkup-filter-src-block)
    (:filter-statistics-cookie . ox-mrkup-filter-statistics-cookie)
    (:filter-strike-through . ox-mrkup-filter-strike-through)
    (:filter-subscript . ox-mrkup-filter-subscript)
    (:filter-superscript . ox-mrkup-filter-superscript)
    (:filter-table . ox-mrkup-filter-table)
    (:filter-table-cell . ox-mrkup-filter-table-cell)
    (:filter-table-row . ox-mrkup-filter-table-row)
    (:filter-target . ox-mrkup-filter-target)
    (:filter-timestamp . ox-mrkup-filter-timestamp)
    (:filter-underline . ox-mrkup-filter-underline)
    (:filter-verbatim . ox-mrkup-filter-verbatim)
    (:filter-verse-block . ox-mrkup-filter-verse-block)))

(defun ox-mrkup-filter-body
  (text back-end info)
  (format "<filter-body>%s</filter-body" text))
(defun ox-mrkup-filter-bold
  (text back-end info)
  "Markup TEXT as <bold>TEXT</bold>. Ignore BACK-END and INFO."
  (format "<filter-bold>%s</filter-bold>" text))
(defun ox-mrkup-filter-babel-call
  (text back-end info)
  (format "<filter-babel-call>%s</filter-babel-call>" text))
(defun ox-mrkup-filter-center-block
  (text back-end info)
  (format "<filter-center-block>%s</filter-center-block>" text))
(defun ox-mrkup-filter-clock
  (text back-end info)
  (format "<filter-clock>%s</filter-clock>" text))
(defun ox-mrkup-filter-code
  (text back-end info)
  (format "<filter-code>%s</filter-code>" text))
(defun ox-mrkup-filter-comment
  (text back-end info)
  (format "<filter-comment>%s</filter-comment>" text))
(defun ox-mrkup-filter-comment-block
  (text back-end info)
  (format "<filter-comment-block>%s</filter-comment-block>" text))
(defun ox-mrkup-filter-diary-sexp
  (text back-end info)
  (format "<filter-diary-sexp>%s</filter-diary-sexp>" text))
(defun ox-mrkup-filter-drawer
  (text back-end info)
  (format "<filter-drawer>%s</filter-drawer>" text))
(defun ox-mrkup-filter-dynamic-block
  (text back-end info)
  (format "<filter-dynamic-block>%s</filter-dynamic-block>" text))
(defun ox-mrkup-filter-entity
  (text back-end info)
  (format "<filter-entity>%s</filter-entity>" text))
(defun ox-mrkup-filter-example-block
  (text back-end info)
  (format "<filter-example-block>%s</filter-example-block>" text))
(defun ox-mrkup-filter-export-block
  (text back-end info)
  (format "<filter-export-block>%s</filter-export-block>" text))
(defun ox-mrkup-filter-export-snippet
  (text back-end info)
  (format "<filter-export-snippet>%s</filter-export-snippet>" text))
(defun ox-mrkup-filter-final-output
  (text back-end info)
  (format "<filter-final-output>%s</filter-final-output>" text))
(defun ox-mrkup-filter-fixed-width
  (text back-end info)
  (format "<filter-fixed-width>%s</filter-fixed-width>" text))
(defun ox-mrkup-filter-footnote-definition
  (text back-end info)
  (format "<filter-footnote-definition>%s</filter-footnote-definition>" text))
(defun ox-mrkup-filter-footnote-reference
  (text back-end info)
  (format "<filter-footnote-reference>%s</filter-footnote-reference>" text))
(defun ox-mrkup-filter-headline
  (text back-end info)
  (format "<filter-headline>%s</filter-headline>" text))
(defun ox-mrkup-filter-horizontal-rule
  (text back-end info)
  (format "<filter-horizontal-rule>%s</filter-horizontal-rule>" text))
(defun ox-mrkup-filter-inline-babel-call
  (text back-end info)
  (format "<filter-inline-babel-call>%s</filter-inline-babel-call>" text))
(defun ox-mrkup-filter-inline-src-block
  (text back-end info)
  (format "<filter-inline-src-block>%s</filter-inline-src-block>" text))
(defun ox-mrkup-filter-inlinetask
  (text back-end info)
  (format "<filter-inlinetask>%s</filter-inlinetask>" text))
(defun ox-mrkup-filter-italic
  (text back-end info)
  (format "<filter-italic>%s</filter-italic>" text))
(defun ox-mrkup-filter-item
  (text back-end info)
  (format "<filter-item>%s</filter-item>" text))
(defun ox-mrkup-filter-keyword
  (text back-end info)
  (format "<filter-keyword>%s</filter-keyword>" text))
(defun ox-mrkup-filter-latex-environment
  (text back-end info)
  (format "<filter-latex-environment>%s</filter-latex-environment>" text))
(defun ox-mrkup-filter-latex-fragment
  (text back-end info)
  (format "<filter-latex-fragment>%s</filter-latex-fragment>" text))
(defun ox-mrkup-filter-line-break
  (text back-end info)
  (format "<filter-line-break>%s</filter-line-break>" text))
(defun ox-mrkup-filter-link
  (text back-end info)
  (format "<filter-link>%s</filter-link>" text))
(defun ox-mrkup-filter-node-property
  (text back-end info)
  (format "<filter-node-property>%s</filter-node-property>" text))
;; dont (defun ox-mrkup-filter-options ...)
(defun ox-mrkup-filter-paragraph
  (text back-end info)
  (format "<filter-paragraph>)>%s</filter-paragraph>" text))
;; dont (defun ox-mrkup-filter-parse-tree ...)
(defun ox-mrkup-filter-plain-list
  (text back-end info)
  (format "<filter-plain-list>>%s</filter-plain-list>" text))
(defun ox-mrkup-filter-plain-text
  (text back-end info)
  (format "<filter-plain-text>%s</filter-plain-text>" text))
(defun ox-mrkup-filter-planning
  (text back-end info)
  (format "<filter-planning>%s</filter-planning>" text))
(defun ox-mrkup-filter-property-drawer
  (text back-end info)
  (format "<filter-property-drawer>%s</filter-property-drawer>" text))
(defun ox-mrkup-filter-quote-block
  (text back-end info)
  (format "<filter-quote-block>%s</filter-quote-block>" text))
(defun ox-mrkup-filter-radio-target
  (text back-end info)
  (format "<filter-radio-target>%s</filter-radio-target>" text))
(defun ox-mrkup-filter-section
  (text back-end info)
  (format "<filter-section>%s</filter-section>" text))
(defun ox-mrkup-filter-special-block
  (text back-end info)
  (format "<filter-special-block>%s</filter-special-block>" text))
(defun ox-mrkup-filter-src-block
  (text back-end info)
  (format "<filter-src-block>%s</filter-src-block>" text))
(defun ox-mrkup-filter-statistics-cookie
  (text back-end info)
  (format "<filter-statistics-cookie>%s</filter-statistics-cookie>" text))
(defun ox-mrkup-filter-strike-through
  (text back-end info)
  (format "<filter-strike-through>%s</filter-strike-through>" text))
(defun ox-mrkup-filter-subscript
  (text back-end info)
  (format "<filter-subscript>%s</filter-subscript>" text))
(defun ox-mrkup-filter-superscript
  (text back-end info)
  (format "<filter-superscript>%s</filter-superscript>" text))
(defun ox-mrkup-filter-table
  (text back-end info)
  (format "<filter-table>%s</filter-table>" text))
(defun ox-mrkup-filter-table-cell
  (text back-end info)
  (format "<filter-table-cell>%s</filter-table-cell>" text))
(defun ox-mrkup-filter-table-row
  (text back-end info)
  (format "<filter-table-row>%s</filter-table-row>" text))
(defun ox-mrkup-filter-target
  (text back-end info)
  (format "<filter-target>%s</filter-target>" text))
(defun ox-mrkup-filter-timestamp
  (text back-end info)
  (format "<filter-timestamp>%s</filter-timestamp>" text))
(defun ox-mrkup-filter-underline
  (text back-end info)
  (format "<filter-underline>%s</filter-underline>" text))
(defun ox-mrkup-filter-verbatim
  (text back-end info)
  (format "<filter-verbatim>%s</filter-verbatim>" text))
(defun ox-mrkup-filter-verse-block
  (text back-end info)
  (format "<filter-verse-block>%s</filter-verse-block>" text))

(provide 'ox-mrkup)
;;; ox-mrkup.el ends here
