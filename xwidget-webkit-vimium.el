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
(require 'cl-lib)

;;* Customization
(defgroup xwidget-webkit-vimium nil
  "Jump to things tree-style."
  :group 'convenience
  :prefix "xwidget-webkit-vimium-")

(defvar xwidget-webkit-vimium-js-scripts '() "An  alist of list of javascript function.")

(defvar xwidget-webkit-vimium-key-to-char-alist '((left . ?◀)
                                (right . ?▶)
                                (up . ?▲)
                                (down . ?▼)
                                (prior . ?△)
                                (next . ?▽))
  "An alist from non-character keys to printable chars used in xwidget overlays.
This alist must contain all keys used in `xwidget-webkit-vimium-keys' which are not
self-inserting keys and thus aren't read as characters.")

(defcustom xwidget-webkit-vimium-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Default keys for jumping.
Any key is either a character representing a self-inserting
key (letters, digits, punctuation, etc.) or a symbol denoting a
non-printing key like an arrow key (left, right, up, down).  For
non-printing keys, a corresponding entry in
`xwidget-webkit-vimium-key-to-char-alist' must exist in order to visualize
 the key in the xwidget-webkit overlays."
  :type '(repeat :tag "Keys" (choice
                              (character :tag "char")
                              (symbol :tag "non-printing key"))))

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

(defun xwidget-webkit-vimium--key-to-char (c)
  "If C is no character, translate it using `xwidget-webkit-vimium-key-to-char-alist'."
  (cond ((characterp c) c)
        ((cdr (assoc c xwidget-webkit-vimium-key-to-char-alist)))
        ((mouse-event-p c) c)
        (t
         (error "Unknown key %s" c))))

(defun xwidget-webkit-vimium--read (tree display-fn cleanup-fn)
  "Select a leaf from TREE using consecutive `read-key'.

DISPLAY-FN should take CHAR and LEAF and signify that LEAFs
associated with CHAR will be selected if CHAR is pressed.  This is
commonly done by adding a CHAR overlay at LEAF position.

CLEANUP-FN should take no arguments and remove the effects of
multiple DISPLAY-FN invocations."
  (catch 'done
    (while tree
      (let ((xwidget-webkit-vimium-leafs nil))
        (xwidget-webkit-vimium--traverse tree
                      (lambda (path leaf)
                        (push (cons path leaf) xwidget-webkit-vimium-leafs)))
        (dolist (x xwidget-webkit-vimium-leafs)
          (funcall display-fn (car x) (cdr x))))
      (let ((char (funcall 'identity (read-key)))
            branch)
        (funcall cleanup-fn)
        (if (setq branch (assoc char tree))
            (if (eq (car (setq tree (cdr branch))) 'leaf)
                    (throw 'done (cdr tree)))
          (xwidget-webkit-vimium-handler-default char))))))

(defun xwidget-webkit-vimium--tree (lst keys)
  "Coerce LST into a balanced tree.
The degree of the tree is the length of KEYS.
KEYS are placed appropriately on internal nodes."
  (let* ((len (length keys)))
    (cl-labels
        ((rd (ls)
             (let ((ln (length ls)))
               (if (< ln len)
                   (cl-pairlis keys
                               (mapcar (lambda (x) (cons 'leaf x)) ls))
                 (let ((ks (copy-sequence keys))
                       res)
                   (dolist (s (xwidget-webkit-vimium--subdiv ln len))
                     (push (cons (pop ks)
                                 (if (eq s 1)
                                     (cons 'leaf (pop ls))
                                   (rd (xwidget-webkit-vimium--multipop ls s))))
                           res))
                   (nreverse res))))))
      (rd lst))))

(defun xwidget-webkit-vimium--subdiv (n b)
  "Distribute N in B terms in a balanced way."
  (let* ((p (1- (floor (+ (log n b) 1e-6))))
         (x1 (expt b p))
         (x2 (* b x1))
         (delta (- n x2))
         (n2 (/ delta (- x2 x1)))
         (n1 (- b n2 1)))
    (append
     (make-list n1 x1)
     (list
      (- n (* n1 x1) (* n2 x2)))
     (make-list n2 x2))))

(defmacro xwidget-webkit-vimium--multipop (lst n)
  "Remove LST's first N elements and return them."
  `(if (<= (length ,lst) ,n)
       (prog1 ,lst
         (setq ,lst nil))
     (prog1 ,lst
       (setcdr
        (nthcdr (1- ,n) (prog1 ,lst (setq ,lst (nthcdr ,n ,lst))))
        nil))))

(defun xwidget-webkit-vimium--traverse (tree walker &optional recur-key)
  "Traverse TREE generated by `xwidget-webkit-vimium--tree'.
WALKER is a function that takes KEYS and LEAF.

RECUR-KEY is used in recursion.

LEAF is a member of LST argument of `xwidget-webkit-vimium--tree'.

KEYS is the path from the root of `xwidget-webkit-vimium--tree' to LEAF."
  (dolist (br tree)
    (let ((key (cons (car br) recur-key)))
      (if (eq (cadr br) 'leaf)
          (funcall walker key (cddr br))
        (xwidget-webkit-vimium--traverse (cdr br) walker key)))))

(defun xwidget-webkit-vimium-handler-default (char)
  "The default handler for a bar CHAR."
  (message "char %s" char)
  (cond ((memq char '(27 7))
         ;; exit sliently
         (throw 'done 'abort))
        ((mouse-event-p char)
         (signal 'user-error (list "Mouse event not handled" char)))
        (t
         (message "No such candidate: %s, hit `C-g' to quit."
                  (if (characterp char) (string char) char)))))




(defun xwidget-webkit-vimium--goto (xpath)
  "Goto the XPATH element."
  (xwidget-webkit-vimium-vimium-goto-candidate (xwidget-webkit-current-session) xpath))

(defun xwidget-webkit-vimium--overlay-fn (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally (NUM . XPATH)."
  (let* ((path (mapcar #'xwidget-webkit-vimium--key-to-char path))
         (str (apply #'string (reverse path)))
         (xpath (cdr leaf)))
    (xwidget-webkit-vimium-vimium-highlight-candidate (xwidget-webkit-current-session) str xpath)))

(defun xwidget-webkit-vimium--cleanup-fn ()
  "Cleanup the sign items in current page."
  (xwidget-webkit-vimium-vimium-cleanup-highlight (xwidget-webkit-current-session)))

(defun xwidget-webkit-vimium--process (candidates)
  "Process the CANDIDATES."
  (let ((res (unwind-protect
                 (xwidget-webkit-vimium--read (xwidget-webkit-vimium--tree (append candidates nil) xwidget-webkit-vimium-keys)
                  #'xwidget-webkit-vimium--overlay-fn
                  #'xwidget-webkit-vimium--cleanup-fn))))
    (cond
     ((null res)
      (message "zero candidates"))
     ((eq res 'exit))
     ((eq res 'abort)
      nil)
     (t
      (setq res (cdr res))
      (funcall 'xwidget-webkit-vimium--goto
               (if (consp res)
                   (car res)
                 res))
      res)))
  ;; The `read-key' in xwidget-webkit-execute-script callback cannot catch the last key input.
  (keyboard-quit))

(defun xwidget-webkit-vimium-get-candidates ()
  "Test."
  (interactive)
  (xwidget-webkit-vimium-js-inject (xwidget-webkit-current-session) 'vimium)
  (xwidget-webkit-vimium-vimium-get-candidates (xwidget-webkit-current-session) #'xwidget-webkit-vimium--process))

(defvar xwidget-webkit-vimium-mode-map (make-sparse-keymap))

(define-key xwidget-webkit-vimium-mode-map "f" 'xwidget-webkit-vimium-get-candidates)

;;;###autoload
(define-minor-mode xwidget-webkit-vimium-mode
  "Enable vimium shortcuts for xwidget-webkit."
  :keymap xwidget-webkit-vimium-mode-map)

;;;###autoload
(add-hook 'xwidget-webkit-mode-hook 'xwidget-webkit-vimium-mode)

(provide 'xwidget-webkit-vimium)
;;; xwidget-webkit-vimium.el ends here
