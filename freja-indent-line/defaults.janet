(import freja/default-hotkeys :as dh)

(import ./freja-indent-line :as fil)

(dh/set-key dh/gb-binds
            [:tab]
            (comp dh/reset-blink fil/indent-current-line!))

# XXX: any better way?  also use set-key?
'(put dh/gb-binds
     :enter
     (comp dh/reset-blink fil/newline-and-indent!))

(dh/set-key dh/gb-binds
            [:shift :enter]
            (comp dh/reset-blink fil/newline-and-indent!))

