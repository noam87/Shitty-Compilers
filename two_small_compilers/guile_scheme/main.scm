(use-modules ((parser)
              #:select (parse-readline)))

(display "\n\nSTARTING SIMPLE REPL...\n\n")

(while #t
  (parse-readline))
