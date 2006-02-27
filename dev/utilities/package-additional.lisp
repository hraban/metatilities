
(in-package metatilities)

(export-exported-symbols "METABANG.BIND" "METATILITIES")

(eval-when (:compile-toplevel :load-toplevel)
  (shadowing-import '(copy-file) "CL-FAD")

  ;;?? Gary King 2005-07-12: not quite sure about this one.
  (shadowing-import '(containers:root) "METATILITIES")
  (shadowing-import '(containers:move) "METATILITIES")

  (use-package "CL-FAD" "METATILITIES"))

(metatilities:export-exported-symbols "CONTAINERS" "METATILITIES")

(make-load-form* containers:abstract-container)
(make-load-form* containers::bst-node)
(make-load-form* containers::quad-tree-node)
