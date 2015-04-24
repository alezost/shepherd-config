;; Local variables for Emacs.

((scheme-mode
  .
  ((eval . (put 'make-service
                'scheme-indent-function 0))
   (eval . (put 'make-target
                'scheme-indent-function 0))
   (eval . (put 'make-display-service
                'scheme-indent-function 0))
   (eval . (put 'make-display-target
                'scheme-indent-function 0))
   (eval . (put 'make-simple-display-service
                'scheme-indent-function 1))
   (eval . (put 'make-simple-forkexec-display-service
                'scheme-indent-function 1))
   (eval . (put 'make-simple-system-display-service
                'scheme-indent-function 1)))))
