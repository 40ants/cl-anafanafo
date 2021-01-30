(defsystem "anafanafo" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description "Calculates text width as if it be rendered by a web browser."
  :defsystem-depends-on ("mgl-pax-minimal")
  :depends-on ("anafanafo/core"))
