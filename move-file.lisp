;;; A simple planning problem expressed in SHOP2 syntax - not
;;; hierarchical but shows usage of basic concepts
;; links are to shop2.pdf included in the distribution

;; To run:
;; from SBCL repl
;; (require 'asdf)
;; (push #p"/path/to/shop2-2.9.0/" asdf:*central-registry*)
;; (asdf:load-system :shop2)
;; (load "/path/to/move-file.lisp")
;; -> to see plan tree output
;; (in-package :shop2-user)
;; (find-plans 'test-prob :verbose :plans :plan-tree t)

(in-package :shop2-user)

(defdomain move-file-domain
  ; operators
  (
   ;; c.f.: 4.10 Operators
   (:operator
    ; h - head
    (!move-file ?source-fs ?source-file ?dest-fs)
    ; P - precondition c.f.: 4.6 Logical Precondition
    ((free-space ?dest-fs ?dest-free)
     (free-space ?source-fs ?source-free)
     (size-of ?source-file ?source-size)
     (call >= ?dest-free ?source-size)
     (writable ?dest-fs)
     (assign ?dest-free-after (call - ?dest-free ?source-size))
     (assign ?source-free-after (call + ?source-free ?source-size)))
    ; D
    ((free-space ?dest-fs ?dest-free)
     (free-space ?source-fs ?source-free)
     (location ?source-file ?source-fs))
    ; A
    ((location ?source-file ?dest-fs)
     (free-space ?dest-fs ?dest-free-after)
     (free-space ?source-fs ?source-free-after))))
  )

(defproblem test-prob move-file-domain
  ((free-space tagore-hdd 227)
   (free-space tagore-ssd 128)
   (size-of my-file 13)
   (location my-file tagore-hdd)
   (writable tagore-ssd))
  ((!move-file tagore-hdd my-file tagore-ssd)))

; run this in the repl to see the plan tree output
(find-plans 'test-prob :verbose :plans :plan-tree t)
