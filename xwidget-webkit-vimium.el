;;; xwidget-webkit-vimium.el --- Link navigation in `xwidget-webkit' -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 JadeStrong
;;
;; Author: JadeStrong <jadestrong@163.com>
;; Maintainer: JadeStrong <jadestrong@163.com>
;; Created: April 27, 2022
;; Modified: April 27, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jadestrong/xwidget-webkit-vimium
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Add support for navigation web pages in `xwidget-webkit' sessions useing the
;;  minibuffer completion.
;;
;;; Code:

(require 'xwidget)

(defvar xwidget-webkit-vimium-js-scripts '() "An  alist of list of javascript function.")

(defun xwidget-webkit-vimium-css-make-class (class style)
  "Generate a css CLASS definition from the STYLE alist."
  (format ".%s { %s }\\n" class (mapconcat (lambda (v) (format "%s: %s;" (car v) (cdr v))) style " ")))

(defmacro xwidget-webkit-vimium--js (js _ &rest replacements)
  "Apply `format' on JS with REPLACEMENTS  providing MMM mode delimiters.

This file has basic support for javascript using MMM mode and
local variables (see at the end of the file)."
  (declare (indent 2))
  `(format ,js ,@replacements))

(defun xwidget-webkit-vimium-js-string-escape (string)
  "Escape STRING for injection."
  (replace-regexp-in-string "\n" "\\\\n" (replace-regexp-in-string "'" "\\\\'" string)))

(defun xwidget-webkit-vimium-html-inject-head-element (xwidget tag id type content)
  "Insert TAG element under XWIDGET head with ID TYPE and CONTENT."
  (let* ((id (xwidget-webkit-vimium-js-string-escape id))
         (tag (xwidget-webkit-vimium-js-string-escape tag))
         (type (xwidget-webkit-vimium-js-string-escape type))
         (content (xwidget-webkit-vimium-js-string-escape content))
         (script (xwidget-webkit-vimium--js "
__xwidget_id = '%s';
if (!document.getElementById(__xwidget_id)) {
    var e = document.createElement('%s');
    e.type = '%s';
    e.id = __xwidget_id;
    e.innerHTML = '%s';
    document.getElementsByTagName('head')[0].appendChild(e);
};
null;
" js-- id tag type content)))
    (xwidget-webkit-execute-script xwidget script)))

(defun xwidget-webkit-vimium-html-inject-script (xwidget id script)
  "Inject javascript SCRIPT in XWIDGET session using a script element with ID."
  (xwidget-webkit-vimium-html-inject-head-element xwidget "script" id "text/javascript" script))

(defun xwidget-webkit-vimium-html-inject-style (xwidget id style)
  "Inject css STYLE in XWIDGET session using a style element with ID."
  (xwidget-webkit-vimium-html-inject-head-element xwidget "style" id "text/css" style))

(defun xwidget-webkit-vimium-js-lisp-to-js (identifier)
  "Convert IDENTIFIER from Lisp style to javascript style."
  (replace-regexp-in-string "-" "_" (if (symbolp identifier) (symbol-name identifier) identifier)))

(defun xwidget-webkit-vimium-js-register-function (ns-name name js-script)
  "Register javascript function NAME in namespace NS-NAME with body JS-SCRIPT."
  (let* ((namespace (assoc ns-name xwidget-webkit-vimium-js-scripts))
         (fun (when namespace (assoc name (cdr namespace)))))
    (cond (fun
           (delete fun namespace)
           (xwidget-webkit-vimium-js-register-function ns-name name js-script))
          ((not namespace)
           (push (cons ns-name '()) xwidget-webkit-vimium-js-scripts)
           (xwidget-webkit-vimium-js-register-function ns-name name js-script))
          (t
           (push (cons name js-script) (cdr namespace))))
    (cons ns-name name)))

(defun xwidget-webkit-vimium-js-funcall (xwidget namespace name &rest arguments)
  "Invoke javascript function NAME in XWIDGET instance.
Passing ARGUMENTS witch CALLBACK in NAMESPACE."
  ;;; Try to be smart
  (let* ((callback (car (last arguments)))
         (arguments (if (functionp callback) (reverse (cdr (reverse arguments))) arguments))
         (json-args (seq-map #'json-encode arguments))
         (arg-string (string-join json-args ", "))
         (namespace (xwidget-webkit-vimium-js-lisp-to-js namespace))
         (name (xwidget-webkit-vimium-js-lisp-to-js name))
         (script (format "__xwidget_%s_%s(%s)" namespace name arg-string)))
    (xwidget-webkit-execute-script xwidget script (and (functionp callback) callback))))

(defmacro xwidget-webkit-vimium-js-def (namespace name arguments docstring js-body)
  "Create a function NAME with ARGUMENTS, DOCSTRING and JS-BODY.
This will define a javascript function in the namespace NAMESPACE
and a Lisp function to call it."
  (declare (indent 3) (doc-string 4))
  (let* ((js-arguments (seq-map #'xwidget-webkit-vimium-js-lisp-to-js arguments))
         (js-name (xwidget-webkit-vimium-js-lisp-to-js name))
         (js-namespace (xwidget-webkit-vimium-js-lisp-to-js namespace))
         (lisp-arguments (append '(xwidget) arguments '(&optional callback)))
         (script (xwidget-webkit-vimium--js "function __xwidget_%s_%s(%s) {%s};" js--
                   js-namespace js-name (string-join js-arguments ", ") (eval js-body)))
         (lisp-def  `(defun ,(intern (format "xwidget-webkit-vimium-%s-%s" namespace name)) ,lisp-arguments
                       ,docstring
                       (xwidget-webkit-vimium-js-funcall xwidget (quote ,namespace) (quote ,name) ,@arguments callback)))
         (lisp-store `(xwidget-webkit-vimium-js-register-function (quote ,namespace) (quote ,name) ,script)))
    `(progn ,lisp-def ,lisp-store)))

(defun xwidget-webkit-vimium-js-inject (xwidget ns-name)
  "Inject the functions defined in NS-NAME into XWIDGET session."
  (let* ((namespace (assoc ns-name xwidget-webkit-vimium-js-scripts))
         (script (mapconcat #'cdr (cdr namespace) "\n")))
    ;; (message "namespace %s script %s" namespace script)
    (xwidget-webkit-vimium-html-inject-script xwidget (format "--xwidget-webkit-vimium-%s" (symbol-name ns-name)) script)))


(xwidget-webkit-vimium-js-def vimium get-xpath-to (element)
  "Get a xpath of the ELEMENT.""
if (element.id !== '')
  return `id('${element.id}')`;
if (element===document.body)
  return element.tagName;
var ix= 0;
var siblings= element.parentNode.childNodes;
for (var i= 0; i<siblings.length; i++) {
  var sibling= siblings[i];
  if (sibling===element)
    return window.__xwidget_vimium_get_xpath_to(element.parentNode)+'/'+element.tagName+'['+(ix+1)+']';
  if (sibling.nodeType===1 && sibling.tagName===element.tagName)
    ix++;
}
")

(xwidget-webkit-vimium-js-def vimium lookup-element-by-xpath (xpath)
  "Get element by the XPATH.""
var evaluator = new XPathEvaluator();
var result = evaluator.evaluate(xpath, document.documentElement, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null);
return result.singleNodeValue;
")

(xwidget-webkit-vimium-js-def vimium get-candidates ()
  "Fetch all visible links/button/input from the current page.""
window.__xwidget_vimium_candidate_items = [];
var r = {};
Array.from(document.querySelectorAll('a, button, input, textarea, select')).forEach((a, i) => {
    if (a.offsetWidth || a.offsetHeight || a.getClientRects().length) {
        r[i] = window.__xwidget_vimium_get_xpath_to(a);
    }
});
return r;
")

(xwidget-webkit-vimium-js-def vimium highlight-candidate (key xpath)
  "Highlight the XPATH candidate by KEY.""
function createLinkItem (link, rect, key) {
  var item = document.createElement('span')
  item.setAttribute('style', 'position: absolute; padding: 1px 3px 0px 3px; background-color: yellow; color: black; z-index: 9999; font-family: Helvetica, Arial, sans-serif;font-weight: bold;font-size: 12px; background: linear-gradient(to bottom, #FFF785 0%,#FFC542 100%); border: solid 1px #C38A22; border-radius: 3px; box-shadow: 0px 3px 7px 0px rgba(0, 0, 0, 0.3);')
  item.textContent = key
  item.style.top = (window.scrollY + rect.top) + 'px'
  item.style.left = (window.scrollX + rect.left) + 'px'
  return item
}
var link = window.__xwidget_vimium_lookup_element_by_xpath(xpath);
var rect = link.getBoundingClientRect();
var item = createLinkItem(link, rect, key);
window.__xwidget_vimium_candidate_items.push(item)
document.body.appendChild(item);
")

(xwidget-webkit-vimium-js-def vimium cleanup-highlight ()
  "Cleanup the highlight item.""
(window.__xwidget_vimium_candidate_items || []).forEach(item => {
   item.remove();
});
")

(xwidget-webkit-vimium-js-def vimium goto-candidate (xpath)
  "Go to the candidate of XPATH.""
const link = window.__xwidget_vimium_lookup_element_by_xpath(xpath);
const tag = link.tagName;
if (tag === 'A') {
  window.open(link.href);
} else if (tag === 'BUTTON') {
    link.click();
} else if (['INPUT', 'TEXTAREA', 'SELECT'].includes(tag) || link.isContentEditable) {
    link.focus();
    if (['checkbox', 'radio'].includes(link.getAttribute('type').toLowerCase())) {
        link.click();
        document.activeElement.blur();
    } else if (tag === 'SELECT') {
        link.click();
    }
} else {
   console.log(tag);
}
")

(defun xwidget-webkit-vimium--goto (xpath)
  "Goto the XPATH element."
  (xwidget-webkit-vimium-vimium-goto-candidate (xwidget-webkit-current-session) xpath))

(defun xwidget-webkit-vimium--overlay-fn (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally (NUM . XPATH)."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (apply #'string (reverse path)))
         (xpath (cdr leaf)))
    (xwidget-webkit-vimium-vimium-highlight-candidate (xwidget-webkit-current-session) str xpath)))

(defun xwidget-webkit-vimium--cleanup-fn ()
  "Cleanup the sign items in current page."
  (xwidget-webkit-vimium-vimium-cleanup-highlight (xwidget-webkit-current-session)))

(defun xwidget-webkit-vimium--process (candidates)
  "Process the CANDIDATES."
  (let ((res (unwind-protect
                 (avy-read (avy-tree (append candidates nil) avy-keys) #'xwidget-webkit-vimium--overlay-fn #'xwidget-webkit-vimium--cleanup-fn))))
    (cond
     ((null res)
      (message "zero candidates"))
     (t
      ;; (funcall avy-pre-action res)
      (setq res (cdr res))
      (funcall 'xwidget-webkit-vimium--goto
               (if (consp res)
                   (car res)
                 res))
      res))))

(defun xwidget-webkit-vimium-get-candidates ()
  "Test."
  (interactive)
  (xwidget-webkit-vimium-js-inject (xwidget-webkit-current-session) 'vimium)
  (xwidget-webkit-vimium-vimium-get-candidates (xwidget-webkit-current-session) #'xwidget-webkit-vimium--process))

(provide 'xwidget-webkit-vimium)
;;; xwidget-webkit-vimium.el ends here
