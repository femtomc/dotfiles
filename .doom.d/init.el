;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :completion
       company           ; the ultimate code completion backend
       vertico           ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides     ; highlighted indent columns
       ligatures         ; ligatures and symbols to make your code pretty again
       minimap           ; show a map of the code on the side
       modeline        ; snazzy, Atom-inspired modeline, plus API
       ophints           ; highlight the region an operation acts on
       (popup +defaults) ; tame sudden yet inevitable temporary windows
       ;;tabs            ; a tab bar for Emacs
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax +childframe)  ; tasing you for every semicolon you forget
       (spell +flyspell) ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       biblio            ; Writes a PhD for you (citation needed)
       debugger          ; FIXME stepping through code, to help you add bugs
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       lsp               ; M-x vscode
       magit             ; a git porcelain for Emacs
       rgb               ; creating color strings

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS

       :lang
       yaml
       (cc +lsp)         ; C > C++ == 1
       coq               ; proofs-as-programs
       emacs-lisp        ; drown in parentheses
       fstar             ; (dependent types)
       (haskell +lsp)    ; a language that's lazier than I am
       json              ; At least it ain't XML
       julia             ; a better, faster MATLAB
       latex             ; writing papers in Emacs has never been so fun
       markdown          ; writing docs for people to ignore
       (org +roam2)      ; organize your plain life in plain text
       (racket +lsp +xp)
       (rust +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       sh                ; she sells {ba,z,fi}sh shells on the C xor

       :email
       ; (mu4e +org +gmail)

       :app
       (rss +org)

       :config
       (default +bindings +smartparens))
