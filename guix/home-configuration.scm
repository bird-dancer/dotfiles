;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells)
	     (gnu home services desktop)
	     (gnu home services syncthing))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages (specifications->packages (list "font-fantasque-sans"
                                           "emacs"
;;                                            "redshift-wayland"
                                           "htop"
                                           "icecat"
					   "mpv"
					   "stow")))

 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 (services
  (list (service home-bash-service-type
                 (home-bash-configuration
                  (aliases '(("grep" . "grep --color=auto")
			     ("ll" . "ls -hla")
			     ("b" . "cd ..")
			     ("la" . "ls -a")
                             ("ls" . "ls -p --color=auto")))
                  (bashrc (list (local-file
                                 "/home/vm/src/guix-config/.bashrc" "bashrc")))
                  (bash-profile (list (local-file
                                       "/home/vm/src/guix-config/.bash_profile"
                                       "bash_profile")))))
;; 	(service home-redshift-service-type
;; 		 (home-redshift-configuration
;; 		  (location-provider 'manual)
;; 		  (latitude 52.54)
;; 		  (longitude 13.35)
;; 		  (nighttime-temperature 1900)
;; 		  (daytime-temperature 3100)))
	(service home-syncthing-service-type
		 (for-home
		  (syncthing-configuration (logflags 5))))
					; mail https://guix.gnu.org/manual/devel/en/html_node/Mail-Home-Services.html
	)))
