;;; xwidget-webkit-vimium.el --- Description -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;;

(require 'xwidget)

(defvar xwwv-js-scripts '() "An  alist of list of javascript function.")

(defun xwwv-css-make-class (class style)
  "Generate a css CLASS definition from the STYLE alist."
  (format ".%s { %s }\\n" class (mapconcat (lambda (v) (format "%s: %s;" (car v) (cdr v))) style " ")))

(defmacro xwwv--js (js _ &rest replacements)
  "Apply `format' on JS with REPLACEMENTS  providing MMM mode delimiters.

This file has basic support for javascript using MMM mode and
local variables (see at the end of the file)."
  (declare (indent 2))
  `(format ,js ,@replacements))

(defun xwwv-js-string-escape (string)
  "Escape STRING for injection."
  (replace-regexp-in-string "\n" "\\\\n" (replace-regexp-in-string "'" "\\\\'" string)))

(defun xwwv-html-inject-head-element (xwidget tag id type content)
  "Insert TAG element under XWIDGET head with ID TYPE and CONTENT."
  (let* ((id (xwwv-js-string-escape id))
         (tag (xwwv-js-string-escape tag))
         (type (xwwv-js-string-escape type))
         (content (xwwv-js-string-escape content))
         (script (xwwv--js "
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

(defun xwwv-html-inject-script (xwidget id script)
  "Inject javascript SCRIPT in XWIDGET session using a script element with ID."
  (xwwv-html-inject-head-element xwidget "script" id "text/javascript" script))

(defun xwwv-html-inject-style (xwidget id style)
  "Inject css STYLE in XWIDGET session using a style element with ID."
  (xwwv-html-inject-head-element xwidget "style" id "text/css" style))

(defun xwwv-js-lisp-to-js (identifier)
  "Convert IDENTIFIER from Lisp style to javascript style."
  (replace-regexp-in-string "-" "_" (if (symbolp identifier) (symbol-name identifier) identifier)))

(defun xwwv-js-register-function (ns-name name js-script)
  "Register javascript function NAME in namespace NS-NAME with body JS-SCRIPT."
  (let* ((namespace (assoc ns-name xwwv-js-scripts))
         (fun (when namespace (assoc name (cdr namespace)))))
    (cond (fun
           (delete fun namespace)
           (xwwv-js-register-function ns-name name js-script))
          ((not namespace)
           (push (cons ns-name '()) xwwv-js-scripts)
           (xwwv-js-register-function ns-name name js-script))
          (t
           (push (cons name js-script) (cdr namespace))))
    (cons ns-name name)))

(defun xwwv-js-funcall (xwidget namespace name &rest arguments)
  "Invoke javascript function NAME in XWIDGET instance passing ARGUMENTS witch CALLBACK in NAMESPACE."
  ;;; Try to be smart
  (let* ((callback (car (last arguments)))
         (arguments (if (functionp callback) (reverse (cdr (reverse arguments))) arguments))
         (json-args (seq-map #'json-encode arguments))
         (arg-string (string-join json-args ", "))
         (namespace (xwwv-js-lisp-to-js namespace))
         (name (xwwv-js-lisp-to-js name))
         (script (format "__xwidget_%s_%s(%s)" namespace name arg-string)))
    (xwidget-webkit-execute-script xwidget script (and (functionp callback) callback))))

(defmacro xwwv-js-def (namespace name arguments docstring js-body)
  "Create a function NAME with ARGUMENTS, DOCSTRING and JS-BODY.
This will define a javascript function in the namespace NAMESPACE
and a Lisp function to call it."
  (declare (indent 3) (doc-string 4))
  (let* ((js-arguments (seq-map #'xwwv-js-lisp-to-js arguments))
         (js-name (xwwv-js-lisp-to-js name))
         (js-namespace (xwwv-js-lisp-to-js namespace))
         (lisp-arguments (append '(xwidget) arguments '(&optional callback)))
         (script (xwwv--js "function __xwidget_%s_%s(%s) {%s};" js--
                   js-namespace js-name (string-join js-arguments ", ") (eval js-body)))
         (lisp-def  `(defun ,(intern (format "xwwv-%s-%s" namespace name)) ,lisp-arguments
                       ,docstring
                       (xwwv-js-funcall xwidget (quote ,namespace) (quote ,name) ,@arguments callback)))
         (lisp-store `(xwwv-js-register-function (quote ,namespace) (quote ,name) ,script)))
    `(progn ,lisp-def ,lisp-store)))

(defun xwwv-js-inject (xwidget ns-name)
  "Inject the functions defined in NS-NAME into XWIDGET session."
  (let* ((namespace (assoc ns-name xwwv-js-scripts))
         (script (mapconcat #'cdr (cdr namespace) "\n")))
    ;; (message "namespace %s script %s" namespace script)
    (xwwv-html-inject-script xwidget (format "--xwwv-%s" (symbol-name ns-name)) script)))


(xwwv-js-def vimium get-xpath-to (element)
  "Get a xpath of the element.""
if (element.id!=='')
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

(xwwv-js-def vimium lookup-element-by-xpath (xpath)
  "Get element by the xpath.""
var evaluator = new XPathEvaluator();
var result = evaluator.evaluate(xpath, document.documentElement, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null);
return result.singleNodeValue;
")

(xwwv-js-def vimium get-candidates ()
  "Fetch all visible links/button/input from the current page.""
window.__xwidget_vimium_candidate_items = [];
var r = {};
Array.from(document.querySelectorAll('a')).forEach((a, i) => {
    if (a.offsetWidth || a.offsetHeight || a.getClientRects().length) {
        r[i] = window.__xwidget_vimium_get_xpath_to(a);
    }
});
return r;
")

(xwwv-js-def vimium highlight-candidate (key xpath)
  "Highlight the candidate by key""
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

(xwwv-js-def vimium cleanup-highlight ()
  "Cleanup the highlight item.""
(window.__xwidget_vimium_candidate_items || []).forEach(item => {
   item.remove();
});
")

(xwwv-js-def vimium goto-candidate (xpath)
  "Go to the candidate by select.""
const link = window.__xwidget_vimium_lookup_element_by_xpath(xpath);
const tag = link.tagName;
if (tag === 'A') {
  window.open(link.href);
} else {
  console.log(link);
}
")

(defun xwwv--goto (xpath)
  "Goto the xpath element."
  (xwwv-vimium-goto-candidate (xwidget-webkit-current-session) xpath))

(defun xwwv--overlay-fn (path leaf)
  "My overlay fn."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (apply #'string (reverse path)))
         (xpath (cdr leaf)))
    (xwwv-vimium-highlight-candidate (xwidget-webkit-current-session) str xpath))
  )

(defun xwwv--cleanup-fn ()
  (xwwv-vimium-cleanup-highlight (xwidget-webkit-current-session)))

(defun xwwv--process (candidates)
  "Process the candidates."
  (let ((res (unwind-protect
                 (avy-read (avy-tree (append candidates nil) avy-keys) #'xwwv--overlay-fn #'xwwv--cleanup-fn))))
    (cond
     ((null res)
      (message "zero candidates"))
     (t
      ;; (funcall avy-pre-action res)
      (setq res (cdr res))
      (funcall 'xwwv--goto
               (if (consp res)
                   (car res)
                 res))
      res))))

(defun xwwv-get-candidates ()
  "Test."
  (interactive)
  (xwwv-js-inject (xwidget-webkit-current-session) 'vimium)
  (xwwv-vimium-get-candidates (xwidget-webkit-current-session) #'xwwv--process))

(provide 'xwidget-webkit-vimium)
;;; xwidget-webkit-vimium.el ends here
