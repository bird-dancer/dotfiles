;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu))
(use-service-modules cups desktop networking ssh xorg)


(operating-system
 (locale "en_US.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout (keyboard-layout "us"
				   #:options '("ctrl:nocaps")))
 (host-name "guix-xcfce-vm")

 ;; The list of user accounts ('root' is implicit).
 (users (cons* (user-account
                (name "vm")
                (comment "Vm")
                (group "users")
                (home-directory "/home/vm")
                (supplementary-groups '("wheel" "netdev" "audio" "video" "kvm")))
               %base-user-accounts))

 ;; Packages installed system-wide.  Users can also install packages
 ;; under their own account: use 'guix search KEYWORD' to search
 ;; for packages and 'guix install PACKAGE' to install a package.
 (packages (append (list (specification->package "nss-certs"))
                   %base-packages))

 ;; Below is the list of system services.  To search for available
 ;; services, run 'guix system search KEYWORD' in a terminal.
 (services
  (append (list (service xfce-desktop-service-type)
                (set-xorg-configuration
                 (xorg-configuration (keyboard-layout keyboard-layout))))

          ;; This is the default list of services we
          ;; are appending to.
          %desktop-services))
 (bootloader (bootloader-configuration
              (bootloader grub-bootloader)
              (targets (list "/dev/sda"))
              (keyboard-layout keyboard-layout)))
 (swap-devices (list (swap-space
                      (target (uuid
                               "663e06da-f738-4993-8d47-923e8e3a7068")))))

 ;; The list of file systems that get "mounted".  The unique
 ;; file system identifiers there ("UUIDs") can be obtained
 ;; by running 'blkid' in a terminal.
 (file-systems (cons* (file-system
                       (mount-point "/")
                       (device (uuid
                                "0a51f917-2f41-47e3-a87d-92fc399d4c99"
                                'ext4))
                       (type "ext4")) %base-file-systems)))
