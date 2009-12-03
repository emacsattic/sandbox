;;; sandbox.el --- Help run untrusted code

;; Copyright (C) 2000 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: extensions, lisp, internal
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is for calling untrusted code without letting it do anything
;; risky, should it be so inclined.  It is a sort of first step
;; towards a Java-like sandbox.

;; I DO NOT GUARANTEE that it will surely make untrusted code harmless
;; even if properly used.  It disables dangerous functions, which
;; should accomplish that in theory, but in practice should not be
;; treated as a guarantee.  The long-term scrutiny of the user
;; community is needed before this code should be considered secure.

;; Note that sbx-other-safe-functions is NOT well fleshed out.  There
;; are surely many other perfectly safe functions that I just didn't
;; think of that need to be added.  Feel free to add them.

;;; Installation:

;; You will definitely want to compile this, otherwise
;; sbx-with-sandbox adds noticeable time (2 to 6 seconds interpreted
;; on my 100Mhz Pentium, depending on what's already loaded) to the
;; call.  Even compiled, expect it to add time.
;; 

;;; Entry points:

;; sbx-with-sandbox, a macro that runs list-forms with only certain safe
;; functions defined.  Be careful of this one.  This was the last one
;; I finished debugging, and when it locks up, it's a doozy.

;; sbx-with-excluding-sandbox, a macro that runs list-forms with
;; specific functions disabled.

;; sbx-functions-called, a macro that returns a list of the functions
;; that were called by a block of code, excluding those it already
;; knows about.  Basically for developrs.

;;; Non-features: Things that could be done, but I haven't done.


;; sbx-watch-function could handle watching commands (commandp) by
;; adding an interactive spec so the commands may be called normally.
;; But it would only be useful when sbx-functions-called was run and
;; contained interactive code, which I expect will be rare.  If you
;; want it, add it and please send me a patch.

;; The cl requirement could be removed by expanding some stuff.

;; We could allow smarter sandboxing, eg variants of delete-file etc
;; that know what files a package "owns", but that's getting way ahead
;; of this. 

;; We could define a better dummy function, perhaps one that reports a
;; sandbox error.

;; sbx-with-excluding-sandbox could protect some crucial functions
;; like quote, setq and fset, even if the code says to stop them.
;; Otherwise it can crash emacs.

;;; Code:

(require 'cl)

;;;;;;;;;;

(defconst sbx-protected-functions
  '(
     ;;Crucial.
     quote byte-code error

     ;;Whatever we use as sbx-dummy-function must be preserved, along
     ;;with whatever it calls.  
     undefined ding

     ;;Used for final setting-up, execution, and restoring
     sbx-restore-functions
     sbx-swap-functions-to-list
     unwind-protect progn let let* while
     setq fset setcar setcdr
     consp car cdr
     get put
     symbol-function
     
     ;;Important for watching functions.
     apply funcall eval

     ;;Timer functions. The timer must always be in working condition.
     timer-event-handler timerp cancel-timer timer-activate-when-idle
     timer-relative-time
     )

  "Functions that sandbox should never change, because it can't avoid
calling them for various reasons." )


;;This list needs to be extended.  There surely must be many more.
(defconst sbx-other-safe-functions
  '( 
     nth aset aref
     prog1 prog2
     condition-case catch throw
     + - * /
     = /= string= string-lessp
     not null 
     and or if cond 
     point point-min point-max 
     point-marker point-min-marker point-max-marker
     push-mark

     message format
     mapcar mapconcat
     append cons nreverse reverse

     fboundp 
     make-symbol 
     numberp number-or-marker-p number-to-string
     )

  "Basic functions that AFAICT can't harm anything." )


(defconst sbx-own-functions 
  '(
     apropos-macrop sbx-mark-functions-as-protected
     sbx-symbol-function-is-protected-p sbx-symbol-collectable-p
     sbx-stop-function-cell sbx-watch-function-cell
     sbx-include-only-safe-functions sbx-exclude-functions
     sbx-watch-all-functions sbx-map-to-called-functions
     sbx-swap-functions-to-list sbx-restore-functions
     sbx-with-excluding-sandbox-aux sbx-with-sandbox
     sbx-with-excluding-sandbox sbx-functions-called sbx-foo sbx-bar
     sbx-mac sbx-bogus-func sbx-watch-function-test
     )

  "Functions from sbx itself, only needed if we ever use nested sandboxes." )


(defconst sbx-dummy-function #'undefined 
  "The function that is assigned to stopped functions, so they can't
do anything." )

;;;;;;;;;;;;;;;;;;
;;Utility functions

;;Borrowed from apropos.  No reason to duplicate names.
(unless (fboundp 'apropos-macrop)
  (defun apropos-macrop (symbol)
    "T if SYMBOL is a Lisp macro."
    (and (fboundp symbol)
      (consp (setq symbol
	       (symbol-function symbol)))
      (or (eq (car symbol) 'macro)
	(if (eq (car symbol) 'autoload)
	  (memq (nth 4 symbol)
	    '(macro t)))))))

;;;;;;;;;;;;;;;
;;Functions to deal with properties of functions.

(defun sbx-mark-functions-as-protected (include-only-functions)
  "Mark the given functions as safe.
Used in conjunction with sbx-include-only-safe-functions.
This must be called with the functions that sbx needs before starting."

  (loop
    for s in include-only-functions
    do
    (put s 'safe-function t)))


(defsubst sbx-symbol-function-is-protected-p (s)
  "Non-nil if the function is protected."
  (get s 'safe-function))


(defun sbx-symbol-collectable-p (s)
  "Non-nil if the symbol is collectable."
  
  (and
    (fboundp s)
    (not (sbx-symbol-function-is-protected-p s))))

;;;;;;;;;;
;;Functions to create cells wrt specific function symbols.

;; All cells are of the form (SYMBOL . ORIGINAL-FUNCTION)


(defsubst sbx-stop-function-cell (s)
  "Return a cell to stop function S."
  (cons s sbx-dummy-function))


(defun sbx-watch-function-cell (s)
  "Return a cell to watch function S."
  
  (let* 
    ( 
      (func (symbol-function s))

      ;;A forwarding function that calls the function normally but
      ;;remembers it was called.
      (new-func
	(cond
	  ((apropos-macrop s)
	    `(macro .
	       (lambda (&rest args)
		 (put ',s 'sbx-was-called t)
		 (apply ',(cdr func) args))))

	  ;;We could also handle (commandp s) specially, by giving it
	  ;;(interactive), but for now we won't.
	  (t
	    `(lambda (&rest args)
	       (put ',s 'sbx-was-called t)
	       (apply ',func args)))
	  )))
    
    ;;Note that it wasn't called yet
    (put s 'sbx-was-called nil)
    (cons s new-func)))

;;;;;;;;;;;;;;;
;;Collect lists of cells

;; All cells are of the form (SYMBOL . ORIGINAL-FUNCTION)

(defun sbx-include-only-safe-functions ()
  "Return a list of cells to stop everything except protected functions.

See sbx-mark-functions-as-protected."

  (loop
    for s being the symbols
    if (sbx-symbol-collectable-p s)
    collect (sbx-stop-function-cell s)))


(defun sbx-exclude-functions (excluded-functions)
  "Return a list of cells to stop the given functions."
  (loop
    for s in excluded-functions
    if (fboundp s)
    collect (sbx-stop-function-cell s)))


(defun sbx-watch-all-functions ()
  "Return a list of cells to watch everything except protected functions.

See sbx-mark-functions-as-protected."

  (loop
    for s being the symbols
    
    if
    (sbx-symbol-collectable-p s)
    collect (sbx-watch-function-cell s)))


;;;;;;;;;;;

(defun sbx-map-to-called-functions (function-list)
  "Return a list of functions on FUNCTION-LIST that have been called.

FUNCTION-LIST is a list in the form \(symbol . etc \).  Lists of cells
will suffice.  

We assume the function properties have been set up by
sbx-watch-all-functions or similar."

  (let
    (
      ;;List only the functions that were called.
      (called-functions
	(remove-if-not
	  #'(lambda (s)
	      (get (car s) 'sbx-was-called))
	  function-list)))
       
    ;;Return their symbols.
    (mapcar #'car called-functions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Careful functions.  

;;These are the only functions, besides the innards of macros, that
;;should have to deal with the functions while in transition from
;;sandboxed to unsandboxed.

;;This exists to make sbx-functions-called more similar to
;;sbx-with-sandbox, and to make the whole business safer.
;;Since the list is so big, we don't mapcar.


(defun sbx-swap-functions-to-list (stored-functions)
  "Swap the symbol's functions and the cdrs of STORED-FUNCTIONS' cells."
  (let
    ( (function-list stored-functions)
      (cell nil)
      the-func)
    (while
      (consp function-list)
      (setq cell
	(car function-list))
      (setq the-func
	(symbol-function (car cell)))
      (fset
	(car cell)
	(cdr cell))
      (setcdr cell the-func)
      (setq function-list
	(cdr function-list)))
    stored-functions))

;;;;
;;Generated from:
;; (defun sbx-restore-functions (stored-functions)
;;   ""

;;   (loop
;;     for cell in stored-functions
;;     do (setf (symbol-function (car cell)) (cdr cell))))

(defun sbx-restore-functions (stored-functions)
  "Set the symbol's functions to the given values."
  (let
    ((cell nil))
    (while
      (consp stored-functions)
      (setq cell
	(car stored-functions))
      (fset
	(car cell)
	(cdr cell))
      (setq stored-functions
	(cdr stored-functions)))
    nil))

;;

;; sbx-with-excluding-sandbox-aux is a defun, not a defmacro, because
;; it isn't directly used as a macro but is the workhorse of the entry
;; macros.

;; It's separate from the macros for 3 reasons:
;;
;; Code re-use of course.
;;
;; To make sbx-functions-called more closely mimic sbx-with-sandbox
;; and sbx-with-excluding-sandbox, so what it collects does not
;; diverge from what sbx-with-sandbox and sbx-with-excluding-sandbox
;; actually see.
;;
;; To minimize the exposure of arbitrary function-collection code.
;;      


(defun sbx-with-excluding-sandbox-aux  (function-swaps body)
  "Return a list-form that will execute BODY in a sandbox.

FUNCTION-SWAPS is a list of cells.
BODY is a list of list-forms.

NB this is *not* a macro."
  
  `(let
     ((stored-functions ,function-swaps))
     ;;Swap the functions into place.
     (sbx-swap-functions-to-list stored-functions)

     (unwind-protect
       ;;Now do the work.  The value will get returned.  The functions
       ;;will get restored even if there's an error.
       (progn
	 ,@body)
       
       ;;Restore the functions to what they were.
       (sbx-restore-functions stored-functions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Entry points

;;Should this be called `in-sandbox'?
(defmacro sbx-with-sandbox (&rest body)
  "Run BODY with only safe functions allowed.

Usage:

    \(sbx-with-sandbox \(call\) \(some untrusted\) \(code\)\)
" 

  (sbx-with-excluding-sandbox-aux 
    '(sbx-include-only-safe-functions)
    body))



;;Should this be called `in-excluding-sandbox'?
(defmacro sbx-with-excluding-sandbox  (unsafe-functions &rest body)
  "Run BODY with UNSAFE-FUNCTIONS disallowed.

Usage:

    \(sbx-with-excluding-sandbox \(call\) \(some untrusted\) \(code\)\)"

  (sbx-with-excluding-sandbox-aux
    `(sbx-exclude-functions ,unsafe-functions)
    body))


(defmacro sbx-functions-called (&rest body)
  "Return a list of the functions that were actually called.
The return value of BODY is discarded.

Usage:

    \(sbx-functions-called \(call\) \(some\) \(code\)\)

This is useful for developing."

  
  `(let
     ((stored-functions (sbx-watch-all-functions)))
     ,(sbx-with-excluding-sandbox-aux
       'stored-functions
	body)

     (sbx-map-to-called-functions stored-functions)))



;;;;;;;;;;;;;;;;
;;Mark some functions as needed or safe immediately.

(sbx-mark-functions-as-protected sbx-protected-functions)
(sbx-mark-functions-as-protected sbx-other-safe-functions)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Testing

(eval-when-compile
  (setf
    (get 'sandbox-regress 'rtest-setup)
    '(progn
       (defun sbx-foo ()
	 ""
	 t)

       (defun sbx-bar ()
	 ""
  
	 131)

       (defmacro sbx-mac ()
	 ""
    
	 `(+ 100 43))

  
       (defun sbx-bogus-func ()
	 "For tests that play with its symbol-function arbitrarily.  Don't
ever call it.") 

       (defun sbx-watch-function-test (s)
	 ""

	 (let
	   ((cell (sbx-watch-function-cell s)))
	   (sbx-swap-functions-to-list (list cell))
	   cell))

       
       ))

  (setf
    (get 'sandbox-regress 'rtest-suite)

    '("sandbox-regress
Beeps are normal when testing this."

       ;;sbx-swap-functions-to-list
       ;;Use a trivial list with one item, using bogus "functions" for
       ;;easy comparison.

       ("Gets the new value from the swap-list." 
	 (let
	   ((the-list (list (cons 'sbx-bogus-func 121))))

	   ;;Set the value to one thing.
	   (setf (symbol-function 'sbx-bogus-func) 234 )

	   ;;Swap it to another.
	   (sbx-swap-functions-to-list the-list)

	   ;;Has it got the new value?
	   (symbol-function 'sbx-bogus-func))
	
	 121)


       ("Gives the old value to the swap-list." 
	 (let
	   ((the-list (list (cons 'sbx-bogus-func 121))))

	   ;;Set the value to one thing.
	   (setf (symbol-function 'sbx-bogus-func) 234 )

	   ;;Swap it to another.
	   (sbx-swap-functions-to-list the-list)

	   (cdr (car the-list)))
	
	 234)

       ("Two swaps undo each other"
	 (let
	   ((the-list (list (cons 'sbx-bogus-func 121))))

	   ;;Set the value to one thing.
	   (setf (symbol-function 'sbx-bogus-func) 234 )

	   ;;Swap it to another.
	   (sbx-swap-functions-to-list the-list)

	   ;;Swap it back
	   (sbx-swap-functions-to-list the-list)

	   ;;It should have the original value
	   (symbol-function 'sbx-bogus-func))
	
	 234)

       ;;sbx-with-excluding-sandbox
       ("Test that a result gets properly returned"
	 (sbx-with-excluding-sandbox '(sbx-foo)
	   57)

	 57)

       ("Test that we can exclude a function."
	 (sbx-with-excluding-sandbox '(sbx-foo)
	   (sbx-foo))

	 nil)

       ("Test that we can exclude a list of functions."
	 (sbx-with-excluding-sandbox '(sbx-foo bar baz baf)
	   (sbx-foo))

	 nil)

       ("Don't exclude that function now when we don't mean to."
	 (sbx-with-excluding-sandbox '(bar)
	   (sbx-foo))

	 t)

       ("An error inside the sandbox shouldn't mess us up."
	 (sbx-with-excluding-sandbox '(bar)
	   (error "An error"))
	 '(error "An error"))
      
       ("After everything, that function should still work outside."
	 (sbx-foo)
	 t)

       ;;sbx-watch-function.  
       ("A watched function should return the same thing as always."
	 (let
	   ()
	   (eval
	     (sbx-with-excluding-sandbox-aux 
	       '(list (sbx-watch-function-cell 'sbx-bar)) 
	       '(
		  (sbx-bar)
		  ))))
	 131)

       ("A watched function informs its sbx-was-called property."
	 (let
	   (val-1 val-3)
	   (eval
	     (sbx-with-excluding-sandbox-aux 
	       '(list (sbx-watch-function-cell 'sbx-bar)) 
	       '(
		  (setq val-1 (get 'sbx-bar 'sbx-was-called))
		  (sbx-bar)
		  (setq val-3 (get 'sbx-bar 'sbx-was-called))
		  )))
	   (list val-1 val-3))
	
	 '(nil t))

       ;;Repeat those tests with sbx-mac
       ("A watched macro should return the same thing as always."
	 (let
	   ()
	   (eval
	     (sbx-with-excluding-sandbox-aux 
	       '(list (sbx-watch-function-cell 'sbx-mac)) 
	       '(
		  (sbx-mac)
		  ))))
	 143)

       ("A watched macro informs its sbx-was-called property."
	 (let
	   (val-1 val-3)
	   (eval
	     (sbx-with-excluding-sandbox-aux 
	       '(list (sbx-watch-function-cell 'sbx-mac)) 
	       '(
		  (setq val-1 (get 'sbx-mac 'sbx-was-called))
		  (sbx-mac)
		  (setq val-3 (get 'sbx-mac 'sbx-was-called))
		  )))
	   (list val-1 val-3))
	
	 '(nil t))

       )))




;;WARNING: These tests are very slow.  Also, bad failures can crash
;;your emacs, so save your buffers first.  The tests work for me, but
;;I can offer no guarantees.


(eval-when-compile
  (setf
    (get 'sandbox-regress-slow 'rtest-suite)
    '("sandbox-regress-slow
Beeps are normal when testing this."

       ;;sbx-functions-called
       ("Test that sbx-functions-called works and yields nothing for
an empty list."
	 (sbx-functions-called)
	 '())
      
       ("Test that it yields foo when we only call sbx-foo which in turn
calls nothing." 
	 (sbx-functions-called (sbx-foo))
	 '(sbx-foo))
      

       ;;sbx-with-sandbox
       ("An empty sandbox works OK."
	 (sbx-with-sandbox)
	 nil)
      
       ("Results are returned normally."
	 (sbx-with-sandbox 87)
	 87)

       ("Excluded functions don't fire"
	 (sbx-with-sandbox (sbx-foo))
	 nil)


       )))



;;; sandbox.el ends here
