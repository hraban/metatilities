
(in-package #:metatilities)

(export-exported-symbols 'metabang.bind 'metatilities)

(eval-when (:compile-toplevel :load-toplevel)
  (shadowing-import '(#:copy-file) '#:cl-fad)

  ;;?? Gary King 2005-07-12: not quite sure about this one.
  (shadowing-import '(containers:root) '#:metatilities)
  (shadowing-import '(containers:move) '#:metatilities)

  (use-package '#:cl-fad '#:metatilities))

(export-exported-symbols '#:containers '#:metatilities)

(make-load-form* containers::bst-node)
(make-load-form* containers::quad-tree-node)
