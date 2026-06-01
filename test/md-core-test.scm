(import (scheme base)
        (srfi 78)
        (md core))

(check (markdown-string->sxml "# Title")
       => '(*TOP* (h1 "Title")))

(check (markdown-string->sxml "Hello **world** and `code`.")
       => '(*TOP*
            (p "Hello " (strong "world") " and " (code "code") ".")))

(check (markdown-string->sxml "[site](https://example.com)\n\n![alt](image.png)")
       => '(*TOP*
            (p (a (@ (href "https://example.com")) "site"))
            (p (img (@ (src "image.png") (alt "alt"))))))

(check (markdown-string->sxml "- one\n- two\n\n1. first\n2. second")
       => '(*TOP*
            (ul (li "one") (li "two"))
            (ol (li "first") (li "second"))))

(check (markdown-string->sxml "> quote\n> line")
       => '(*TOP*
            (blockquote (p "quote line"))))

(check (markdown-string->sxml "```scheme\n(+ 1 2)\n```")
       => '(*TOP*
            (pre (code (@ (class "language-scheme")) "(+ 1 2)"))))

(check-report)
