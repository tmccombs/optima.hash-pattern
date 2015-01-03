optima.hash-pattern - Hash Table Patterns for Optima
====================================================

optima.hash-pattern is an extension to [Optima](https://github.com/m2ym/optima)
that supplies some patterns to match against
common lisp hash-tables. There is one constructor pattern, and one
dervied pattern.

hash-property
-------------

The `hash-property` pattern is similar to the `property` pattern,
but matches against a hash-table rather than a plist.

### Syntax:

    (hash-property KEY PATTERN)

### Examples:

    (let ((tab (make-hashtable)))
      (setf (gethash :a tab) 1)
        (match tab
        ((hash-property :a x) x)))
    => 1

hashtable
---------

The `hashtable` pattern is similar to the `plist` pattern,
but matches agains a hash-table rather than a plist

### Syntax:

	(hastable {KEY PATTERN}*)

### Expansion:

	(hashtable {k p}* => (and (hash-property k p)*)

### Example:

	(let ((tab (make-hashtable)))
	  (setf (gethash :name tab) \"John\")
	  (setf (gethash :age tab) 23)
	  (match tab
		((hashtable :name \"John\" :age age) age)))
	=> 23
