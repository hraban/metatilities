(in-package #:metatilities)

(export-exported-symbols 'metabang.bind 'metatilities)

(eval-when (:compile-toplevel :load-toplevel)
  ;;?? Gary King 2005-07-12: not quite sure about this one.
  (shadowing-import '(containers:root) '#:metatilities)
  (shadowing-import '(containers:move) '#:metatilities))

(export-exported-symbols '#:containers '#:metatilities)

(make-load-form* containers::bst-node)
(make-load-form* containers::quad-tree-node)

(eval-when (:compile-toplevel :load-toplevel)
  (let ((symbols '(make-sorter
	    string->symbol
	    curry
	    file-package
	    list->formatted-string
	    delete-directory
	    ensure-filename-safe-for-os
	    tokenize-string
	    
	    update-dialog-ui
	    dialog-item-value 

	    note-view-settings-changed

	    view-scale
	    view-x/view-y->x/y
	    view-rect->rect
	    x/y->view-x/view-y
	    view-x/view-y->point
	    distance-x/distance-y->x/y
	    x/y->distance-x/distance-y
	    adjust-point-for-scaling
	    scale-x
	    scale-y
	    view-x/view-y->point

	    view-requiring-cleanup-mixin
	    clean-up-view

	    redraw

	    firstn
	    mapappend
	    find-all
	    partition
	    tree-find
	    tree-find-if
	    tree-remove-if
	    tree-map
	    push-end

	    compact-array
    
	    nmerge-list
	    circular-list
	    make-initialized-array
	    fixnump
	    object->string
	    float->integer
	    sort-using-list-order
	    unused-variables
	    lambda-list->args
	    car-safe
	    cdr-safe

	    curry
	    curry-after
	    compose
	    conjoin
	    disjoin

	    argmax
	    argmin
	    best-item		    ; see also u:minimize, u:reduce-if
	    very-small-number-p
	    +very-small-number+

	    remove-leading-quote

	    nth-elt-of-cross-product
	    nth-elt-of-cross-product-as-multiple-values

	    constant-expression-p

	    cleanup-parsed-parameter
	    convert-clauses-into-lists
	    convert-clauses-into-lists*
	    add-class-if-necessary
	    add-classes-if-necessary

	    spy
	    spyx
   
	    binary-search
	    graph-search

	    make-obsolete

	    named-lambda
	    deletef
	    removef

	    doplist
	    group
	    gensym0
	    assert*
	    maxf
	    minf
	    multf
	    some*
	    handler-case-if

	    gensym*

	    rebinding

	    \\	     ; yes, this is actually the name of a macro. ---L

	    ensure-type
	    with-slot-bindings

	    allf
	    nilf
	    tf
	    toggle!			; renamed toggle to toggle!
	    filter-values
	    with-array
	    with-matrix
	    with-struct
	    match
	    if-match

	    most
	    best
	    mostn

	    map0-n
	    map1-n
	    mapa-b
	    map->
	    mapcars

	    ;; fn
	    compose
	    concf
 
	    file-to-list

	    unique-file-name-from-date
	    pretty-namestring-from-date
	    eos-namestring-from-date
	    short-eos-namestring-from-date
	    rename-file-if-present
	    uniquify-file-name
	    remove-illegal-filename-characters
	    shorten-filename-for-os
	    map-files
	    map-forms-in-file
	    map-lines-in-file 
	    next-line
	    file-package
	    ensure-filename-safe-for-os

	    delete-directory
	    directory-name-p
	    string->month

	    print-date date-string 
	    date-string-brief 
	    print-time 
	    print-universal-time
	    time-string
	    time-string-with-no-colons
	    date-and-time-string 
	    parse-date-and-time-string
	    parse-date-and-time 
	    print-brief-ut 
	    print-ut
	    print-time-interval 
	    print-brief-time-interval
	    parse-interval-or-never

	    leap-year-p

	    string-before
	    string-after
	    string-contains-p
	    string->symbol
	    symbol->string
	    substring
	    tokenize-string
	    list->formatted-string

	    reduce-if
	    minimize

	    flatten
	    power-set
	    all-pairs
	    map-combinations
	    combinations
	    remove-members
	    permute

	    transpose
	    transpose2

	    length-exactly-p
	    same-length-p
	    percent-overlap

	    tree-search)))
    (mapc (lambda (s)
	    (intern (symbol-name s) 
		    (load-time-value (find-package :metatilities))))
	  symbols)
    (mapc (lambda (s)
	    (export s (load-time-value (find-package :metatilities))))
	  symbols)))
 
