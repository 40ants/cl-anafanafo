(defsystem "anafanafo-docs"
  :build-pathname "docs/build/"
  :class :package-inferred-system
  :pathname "docs/source/"
  :depends-on ("anafanafo-docs/index"))
