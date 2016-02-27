;;; init.scm --- Shepherd init file

;; Copyright © 2015, 2016 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In the following code, 'service' may mean a service object, a service
;; name (symbol) or even (for more confusion) a base-name of a "display
;; service".

;; "Display service" is a service that should be started on a particular
;; display (X server and what should be run on it).  Its name (symbol
;; for '#:provides' service slot) consists of 2 parts: base-name and
;; display; for example: 'x:0', 'xterm:1', etc.

(use-modules
 (srfi srfi-1)
 (srfi srfi-26)
 (al files)
 (al plists)
 (al places)
 (al utils))

;; (use-modules (shepherd service) (oop goops))

(define %dbus-address
  (format #f "unix:path=/tmp/dbus-~a" (getuid)))


;;; Miscellaneous auxiliary code

(define (->symbol string-or-symbol)
  (if (symbol? string-or-symbol)
      string-or-symbol
      (string->symbol string-or-symbol)))

(define (display->vt display)
  "Convert DISPLAY string into a string with VT number.
Use 'vt7' for display ':0', vt8 for ':1', etc."
  (let ((display-num (string->number
                      (substring display
                                 (+ 1 (string-index display #\:))))))
    (string-append "vt" (number->string (+ 7 display-num)))))

(define (env-replace env name val)
  "Return environment by adding new variable NAME/VALUE to ENV.
If the variable NAME already exists in ENV, it will be replaced."
  (let* ((rx (make-regexp (string-append "^" name "=")))
         (new-env (filter (negate (cut regexp-exec rx <>))
                          env)))
    (cons (string-append name "=" val)
          new-env)))

(define* (environ* #:optional display)
  "Return environment with some additional things.
If DISPLAY is specified, add it to the environment."
  (let ((env (env-replace (environ)
                          "DBUS_SESSION_BUS_ADDRESS"
                          %dbus-address)))
    (if display
        (env-replace env "DISPLAY" display)
        env)))

;; Override `make-system-constructor' to make it similar to
;; `make-forkexec-constructor', i.e. make it support a list of strings
;; for `system*' procedure as a COMMAND (in original
;; `make-system-constructor', COMMAND is a "rest" argument and it is a
;; list of strings for `system' procedure).

(define (run-command command)
  (zero? (status:exit-val (apply system* command))))

(define (make-system-constructor command)
  (lambda _
    (run-command command)))

(define (make-system-destructor command)
  (lambda _
    (not (run-command command))))

(define* (make-system-constructor-with-env command #:key display)
  ;; FIXME Is there a better way to start COMMAND synchronously with
  ;; some environment?
  (lambda _
    (let ((old-env (environ))
          (new-env (environ* display)))
      (environ new-env)
      (let ((res (run-command command)))
        (environ old-env)
        res))))

(define* (make-forkexec-constructor-with-env command #:key display)
  (make-forkexec-constructor
   command
   #:environment-variables (environ* display)))


;;; Auxiliary code for services

(define* (starter #:key (base-services '()) (default-services '())
                  user-transformer final-transformer)
  "Return procedure for starting services.

Services are started in a direct order; at first BASE-SERVICES and then
USER-SERVICES or DEFAULT-SERVICES.

The procedure returns a list of all services if they have been started
successfully.  Otherwise it returns #f."
  (define* (transform services #:optional transformer
                      (fallback '()))
    (if transformer
        (map transformer services)
        fallback))

  (lambda user-services
    (let* ((user-services (transform user-services user-transformer))
           (services (append base-services
                             (if (null? user-services)
                                 default-services
                                 user-services)))
           (services (transform services final-transformer services)))
      (and (every start services)
           services))))

(define (stop-services services . _)
  "Stop SERVICES in a reverse order and return #f."
  (for-each stop (reverse services))
  #f)

(define (make-service . args)
  (apply make <service> args))

(define* (make-target #:key (maker make-service)
                      (services '())
                      (start (starter #:base-services services))
                      (stop stop-services)
                      #:allow-other-keys
                      #:rest args)
  "Return service to start/stop a list of SERVICES.
SERVICES are being started in the direct order and stopped in the
reverse order."
  (apply maker
         #:start start
         #:stop stop
         args))

(define (display-service-name display base-name)
  "Return name of a 'display service' BASE-NAME for DISPLAY."
  (symbol-append base-name (string->symbol display)))

(define (display-services-names display base-names)
  "Return list of 'display services' names by BASE-NAMES and DISPLAY."
  (map (cut display-service-name display <>)
       base-names))

(define (display-service-description display base-description)
  "Return description of a 'display service' by BASE-DESCRIPTION and DISPLAY."
  (format #f "~a (DISPLAY=~a)" base-description display))

;; `make-display-service' procedure uses `plist-new', because ARGS (the
;; #:rest argument) contains all keyword arguments (e.g., #:docstring),
;; that needs to be shadowed, otherwise `make-service' will be called
;; with 2 #:docstring arguments and may (and surely will) take the wrong
;; one.  Illustration of the problem:
;;
;; (define* (p1 #:key str . args)
;;   (values str args))
;; (define* (p2 #:key str . args)
;;   (apply p1 #:str (string-append str "-bar") args))
;;
;; (p2 #:str "foo")  =>  "foo"
;;                   =>  (#:str "foo-bar" #:str "foo")
;;
;; The same takes place for `make-display-target'.

(define* (make-display-service #:key display
                               (docstring "Unknown")
                               (provides '())
                               (requires '())
                               #:allow-other-keys
                               #:rest args)
  (apply make-service
         (plist-new args
           #:docstring (display-service-description display docstring)
           #:provides (display-services-names display provides)
           #:requires (display-services-names display requires))))

(define* (make-display-target #:key display
                              (services '())
                              #:allow-other-keys
                              #:rest args)
  (apply make-target
         (plist-new args
           #:maker make-display-service
           #:services (display-services-names display services))))

(define (make-simple-display-service display . args)
  (apply make-display-service
         #:display display

         ;; I changed my mind: do not require 'x' because if there is
         ;; some running X server not managed by shepherd, I still want to be
         ;; able to 'start <something>:<N>' there.

         ;; #:requires '(x)
         args))

(define* (make-simple-forkexec-display-service display #:key command
                                               #:allow-other-keys
                                               #:rest args)
  (apply make-simple-display-service display
         #:start (make-forkexec-constructor-with-env
                  command
                  #:display display)
         #:stop (make-kill-destructor)
         args))

(define* (make-simple-system-display-service display #:key command
                                             #:allow-other-keys
                                             #:rest args)
  (apply make-simple-display-service display
         #:start (make-system-constructor-with-env
                  command
                  #:display display)
         args))


;;; Daemons

(define dbus
  (make-service
    #:docstring "D-Bus Session Daemon"
    #:provides '(dbus)
    #:start (make-forkexec-constructor
             (list "dbus-daemon" "--session" "--nofork"
                   "--address" %dbus-address))
    #:stop (make-kill-destructor)))

(define gpg-agent
  (make-service
    #:docstring "GPG Agent"
    #:provides '(gpg-agent)
    ;; For some reason gpg-agent won't start without "--daemon" option,
    ;; so 'make-forkexec-constructor' is not used.
    #:start
    (make-system-constructor
     (let ((pinentry (guix-user-profile-file "bin/pinentry")))
       `("gpg-agent" "--daemon"
         ,@(if (not (file-exists? pinentry))
               '()
               (list "--pinentry-program" pinentry)))))
    #:stop
    (make-system-destructor
     '("gpg-connect-agent" "killagent" "/bye"))))

(define irexec
  (make-service
    #:docstring "IR Exec Daemon"
    #:provides '(irexec)
    #:start (make-forkexec-constructor
             '("irexec"))
    #:stop (make-kill-destructor)))

(define emacs-daemon
  (make-service
    #:docstring "Emacs daemon"
    #:provides '(emacsd)
    #:start
    (make-system-constructor-with-env
     '("emacs" "--no-site-file" "--daemon"))
    #:stop
    (make-system-destructor
     '("emacsclient" "--eval" "(let (kill-emacs-hook) (kill-emacs))"))))

(define daemons
  (list dbus gpg-agent irexec emacs-daemon))


;;; Misc services

(define daemons-target
  (make-target
    #:docstring "Daemons target.
Start 'dbus', 'gpg-agent', 'irexec' and additional specified services."
    #:provides '(daemons)
    #:start
    (starter #:base-services '(dbus gpg-agent irexec)
             #:user-transformer ->symbol)))

(define eval-service
  (let ((module (current-module)))
    (make-service
      #:docstring "Evaluate specified scheme expressions.
This service is intended to perform 'batch' starts/stops, e.g.:

  deco start eval \"(for-each stop daemons)\"
  deco start eval \"(stop 'wm:0)\" \"(start 'stumpwm:0)\"

This service always fails, so that it is always ready to be started
again."
      #:provides '(eval)
      #:start (lambda strings
                (for-each (cut eval-string <> module)
                          strings)
                #f))))

(define amixer-service
  (let ((aset (lambda args
                (run-command (append '("amixer" "set" "-q")
                                     args)))))
    (make-service
      #:docstring "Set sound parameters."
      #:provides '(amixer)
      #:start (lambda _
                (and (aset "Master" "50%")
                     ;; (aset "PCM" "80%")
                     (aset "Line" "10%" "mute"))))))

(define misc-services
  (list daemons-target eval-service amixer-service))


;;; Display services

(define (sudo-command . args)
  "Return a sudo command for running command indicated by ARGS."
  (cons* "sudo" "--non-interactive" "--" args))

(define* (xorg-command #:key (display ":0") (vt "vt7"))
  (let* ((config-dir     (config-file "X/xorg.conf"))
         (module-dir     (let ((modules "lib/xorg/modules"))
                           (first-existing-file
                            (guix-system-profile-file modules)
                            (guix-user-profile-file modules))))
         (x-font-dir     (guix-profile-file "fonts" "share/fonts/X11"))
         (x-font-subdirs (if (file-exists? x-font-dir)
                             (find-files x-font-dir ".")
                             '()))
         (user-font-dir  (home-file ".local/share/fonts"))
         (font-dirs      (if (file-exists? user-font-dir)
                             (cons user-font-dir x-font-subdirs)
                             x-font-subdirs)))
    `("Xdaemon" ,display ,vt
      "-nolisten" "tcp" "-logverbose" "-noreset"
      "-configdir" ,config-dir
      ,@(if (null? x-font-subdirs)
            '()
            (list "-fp" (apply comma-separated font-dirs)))
      ,@(if (not module-dir)
            '()
            (list "-modulepath" module-dir)))))

(define* (xorg-service #:key display vt)
  (make-display-service
    #:display display
    #:docstring "Xorg server"
    #:provides '(x)
    #:start (make-system-constructor
             (apply sudo-command (xorg-command #:display display
                                               #:vt vt)))
    #:stop (make-system-destructor
            (sudo-command "Xkill" display))))

(define (xorg-service* display)
  (xorg-service #:display display
                #:vt (display->vt display)))

(define (xset-service display)
  (make-simple-system-display-service display
    #:docstring "Xset"
    #:provides '(xset)
    #:command (list "xset" "r" "rate" "193" "43" "b" "off")))

(define (xsetroot-service display)
  (make-simple-system-display-service display
    #:docstring "Xsetroot"
    #:provides '(xsetroot)
    #:command (list "xsetroot" "-solid" "gray25"
                    "-xcf" (home-file ".icons/default/cursors/cell") "1")))

(define (setxkbmap-service display)
  (make-simple-system-display-service display
    #:docstring "setxkbmap"
    #:provides '(setxkbmap)
    #:command '("setxkbmap" "us,ru,us" "dvorak,,")))

(define (xmodmap-service display)
  (make-simple-system-display-service display
    #:docstring "Xmodmap"
    #:provides '(xmodmap)
    #:command (list "xmodmap" (config-file "X/Xmodmap"))))

(define (xrdb-service display)
  (make-simple-system-display-service display
    #:docstring "Xrdb (X resource database)"
    #:provides '(xrdb)
    #:command (list "xrdb" "-merge" (config-file "X/Xresources"))))

(define (mosd-service display)
  (make-simple-display-service display
    #:docstring "mOSD"
    #:provides '(mosd)
    #:start (make-forkexec-constructor-with-env
             (list "mosdd" "--script" (config-file "mosd/rc"))
             #:display display)
    #:stop (make-system-destructor
            '("mosdctl" "--exit"))))

(define (unclutter-service display)
  (make-simple-forkexec-display-service display
    #:docstring "Unclutter (hide idle cursor)"
    #:provides '(unclutter)
    #:command '("unclutter" "-root" "-jitter" "5")))

(define (openbox-service display)
  (make-simple-forkexec-display-service display
    #:docstring "Openbox"
    #:provides '(openbox wm)
    #:command '("openbox")
    #:actions
    (make-actions
     (reload
      "Reload configuration file."
      (make-system-constructor-with-env
       '("openbox" "--reconfigure")
       #:display display)))))

(define (stumpwm-service display)
  (make-simple-forkexec-display-service display
    #:docstring "Stumpwm"
    #:provides '(stumpwm wm)
    #:command '("stumpwm")))

(define (xterm-service display)
  (make-simple-forkexec-display-service display
    #:docstring "Xterm"
    #:provides '(xterm)
    #:command '("xterm")))

(define (emacs-service display)
  (make-simple-forkexec-display-service display
    #:docstring "Emacs"
    #:provides '(emacs)
    #:command '("emacs" "--no-site-file")))

(define (conkeror-service display)
  (make-simple-forkexec-display-service display
    #:docstring "Conkeror"
    #:provides '(conkeror)
    #:command '("conkeror")))

(define (xsettings-target display)
  (make-display-target
    #:display display
    #:services '(xset xsetroot setxkbmap xmodmap xrdb)
    #:docstring "Xsettings target"
    #:provides '(xsettings)))

(define (gui-target display)
  "Target service to start GUI session on DISPLAY."
  (make-display-target
    #:display display
    #:docstring "GUI target.
Start X server with some settings and additional services or 'xterm' if
none are specified."
    #:provides '(gui)
    #:start
    (starter #:base-services '(x xsettings)
             #:default-services '(xterm)
             #:user-transformer ->symbol
             #:final-transformer (cut display-service-name display <>))))

(define (make-display-services display)
  "Return list of all 'display services' for DISPLAY."
  (map (cut <> display)
       (list xorg-service*
             xset-service
             xsetroot-service
             setxkbmap-service
             xmodmap-service
             xrdb-service
             mosd-service
             unclutter-service
             openbox-service
             stumpwm-service
             xterm-service
             emacs-service
             conkeror-service
             xsettings-target
             gui-target)))


;;; Let's go!

(apply register-services
       (append daemons
               misc-services
               (make-display-services ":0")
               (make-display-services ":1")
               (make-display-services ":2")))

;; Do not start services if SHEPHERD_SERVICES is 0 or empty.
(let ((env (getenv "SHEPHERD_SERVICES")))
  (unless (and env
               (or (string-null? env)
                   (string= "0" env)))
    (start 'daemons)
    (start amixer-service)))

(action 'shepherd 'daemonize)

;;; init.scm ends here
