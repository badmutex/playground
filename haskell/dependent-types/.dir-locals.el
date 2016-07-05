((haskell-mode .
               ((haskell-process-wrapper-function
                 . (lambda (args)
                     (append
                      (list "nix-shell" "-I" "." "--pure" "--command"
                            (mapconcat 'identity args " "))))))))
