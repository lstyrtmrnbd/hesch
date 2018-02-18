;;;hesch.asd

(asdf:defsystem #:hesch
    :description "Henderson's Escher language"
    :long-description "An OpenGL implementation of Peter Henderson's functional language to describe Escher's Square Limit"
    :author "undisclosed"
    :license "GPLv3"
    :serial t
    :depends-on (#:cepl.sdl2 #:dirt)
    :components ((:file "hesch")))
