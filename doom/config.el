(setq doom-theme 'doom-gruvbox)
(setq doom-gruvbox-dark-variant "soft")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq org-directory "~/org/")

;; Beacon Mode allows you to see where the cursor is when scrolling
(beacon-mode 1)

;; Bookmarks
(setq bookmark-default-file "~/.doom.d/bookmarks")

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks"                          "L" #'list-bookmarks
       :desc "Set bookmark"                            "m" #'bookmark-set
       :desc "Delete bookmark"                         "M" #'bookmark-set
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))


;; Buffers
;; Keeps buffers getting out of sync if that file has been changed by another program
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Keybindings within ibuffer mode
(evil-define-key 'normal ibuffer-mode-map
  (kbd "f c") 'ibuffer-filter-by-content
  (kbd "f d") 'ibuffer-filter-by-directory
  (kbd "f f") 'ibuffer-filter-by-filename
  (kbd "f m") 'ibuffer-filter-by-mode
  (kbd "f n") 'ibuffer-filter-by-name
  (kbd "f x") 'ibuffer-filter-disable
  (kbd "g h") 'ibuffer-do-kill-lines
  (kbd "g H") 'ibuffer-update)


;; Calender
(defun dt/year-calendar (&optional year)
  (interactive)
  (require 'calendar)
  (let* (
      (current-year (number-to-string (nth 5 (decode-time (current-time)))))
      (month 0)
      (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (dotimes (j 4)
      ;; vertical columns
      (dotimes (i 3)
        (calendar-generate-month
          (setq month (+ month 1))
          year
          ;; indentation / spacing between months
          (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun dt/scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by year in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 0))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let* (
              (year (+ displayed-year arg)))
        (dt/year-calendar year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun dt/scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by year in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (dt/scroll-year-calendar-forward (- (or arg 1)) event))

(map! :leader
      :desc "Scroll year calendar backward" "<left>" #'dt/scroll-year-calendar-backward
      :desc "Scroll year calendar forward" "<right>" #'dt/scroll-year-calendar-forward)

(defalias 'year-calendar 'dt/year-calendar)


;; Centaur Tabs

(setq centaur-tabs-set-bar 'over
      centaur-tabs-set-icons t
      centaur-tabs-gray-out-icons 'buffer
      centaur-tabs-height 24
      centaur-tabs-set-modified-marker t
      centaur-tabs-style "bar"
      centaur-tabs-modified-marker "•")
(map! :leader
      :desc "Toggle tabs globally" "t c" #'centaur-tabs-mode
      :desc "Toggle tabs local display" "t C" #'centaur-tabs-local-mode)
(evil-define-key 'normal centaur-tabs-mode-map (kbd "g <right>") 'centaur-tabs-forward        ; default Doom binding is 'g t'
                                               (kbd "g <left>")  'centaur-tabs-backward       ; default Doom binding is 'g T'
                                               (kbd "g <down>")  'centaur-tabs-forward-group
                                               (kbd "g <up>")    'centaur-tabs-backward-group)


;; Clippy

(map! :leader
      (:prefix ("c h" . "Help info from Clippy")
       :desc "Clippy describes function under point" "f" #'clippy-describe-function
       :desc "Clippy describes variable under point" "v" #'clippy-describe-variable))

;; Smudge: Spotify for emacs

(load "~/.doom.d/secrets.el" t)

(use-package! smudge
  :custom
  (smudge-transport 'connect)
  (smudge-player-status-refresh-interval 10)
  (smudge-status-location 'mode-line)
  :config
  (setq smudge-redirect-uri "http://127.0.0.1:8080/callback")  ;; ✅ valid placement
  ;; (global-smudge-remote-mode)
  )
(defun my/smudge-show-user-playlists ()
  "Display the current user's playlists using Smudge."
  (interactive)
  (smudge-playlist-user-playlists))



(map! :leader
      (:prefix ("m" . "Smudge (Spotify)")
       :desc "Play/Pause" "p" #'smudge-controller-toggle-play
       :desc "Next track" "n" #'smudge-controller-next-track
       :desc "Previous track" "b" #'smudge-controller-previous-track
       :desc "Search tracks" "s" #'smudge-track-search
       :desc "Your playlists" "l" #'smudge-playlist-user-playlists))

(use-package! org-roam
  :after org
  :init
  (setq org-roam-directory (file-truename "~/org/roam/"))
  :config
  (org-roam-db-autosync-mode))

(map! :leader
      :prefix ("n r" . "org-roam")
      :desc "Find node"    "f" #'org-roam-node-find
      :desc "Capture node" "c" #'org-roam-capture        ; ← use this
      :desc "Daily note"   "p" #'org-roam-dailies-capture-today  ; optional
      :desc "Insert link"  "i" #'org-roam-node-insert
      :desc "Show graph"   "g" #'org-roam-graph
      :desc "Go back link" "h" #'org-mark-ring-goto
      :desc "Backlinks buf" "l" #'org-roam-buffer-toggle)

;; ─────────────────────────────────────────────────────────────────────────────
;; Org-Roam + Dailies + Capture Templates
;; ─────────────────────────────────────────────────────────────────────────────

(after! org-roam
  ;; Set variables
  (setq org-roam-directory "~/org/roam/"
        org-roam-dailies-directory "daily/")

  ;; Function to insert [[roam:Title]] links
  (defun my/org-insert-roam-link ()
    "Insert a roam:title link instead of ID."
    (interactive)
    (let* ((node (org-roam-node-read))
           (title (org-roam-node-title node)))
      (insert (format "[[roam:%s]]" title))))

  ;; Keybinding for roam:title link
  (map! :leader
        :desc "Insert roam:title link"
        "n r R" #'my/org-insert-roam-link)

  ;; Now set the templates in a new `setq`
  (setq
   org-roam-dailies-capture-templates
   '(("o" "default"
      entry
      "* %<%H:%M> %?"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n#+roam_alias: ${alias}\n#+filetags: :daily:\n")))

   org-roam-capture-templates
   '(("c" "Course Note"    plain "%?"
      :target (file+head "courses/${slug}.org"
                         "#+title: ${title}\n#+roam_alias: ${alias}\n#+hugo_section: .\n#+filetags: :course:\n")
      :unnarrowed t)

     ("r" "Blank Note"     plain "%?"
      :target (file+head "${slug}.org"
                         "#+title: ${title}\n#+roam_alias: ${alias}\n#+hugo_section: .\n#+filetags:\n")
      :unnarrowed t))))


;; ─────────────────────────────────────────────────────────────────────────────
;; Org Capture: Todos + Run Log
;; ─────────────────────────────────────────────────────────────────────────────
(after! org
  (setq org-capture-templates
        '(("t" "Todo Inbox" entry
           (file+headline "~/org/todos.org" "Inbox")
           "* TODO %?\n%U\n"
           :empty-lines 1)

          ("s" "Study Task" entry
           (file+headline "~/org/todos.org" "🧠 Study")
           "* TODO %?\n:PROPERTIES:\n:CATEGORY: study\n:END:\n%U\n"
           :empty-lines 1)

          ("p" "Project Task" entry
           (file+headline "~/org/todos.org" "🛠 Projects")
           "* TODO %?\n:PROPERTIES:\n:CATEGORY: projects\n:END:\n%U\n"
           :empty-lines 1)

          ("x" "Misc Task" entry
           (file+headline "~/org/todos.org" "🗃 Misc")
           "* TODO %?\n:PROPERTIES:\n:CATEGORY: misc\n:END:\n%U\n"
           :empty-lines 1)

          ("l" "Personal Task" entry
           (file+headline "~/org/todos.org" "☕ Personal")
           "* TODO %?\n:PROPERTIES:\n:CATEGORY: personal\n:END:\n%U\n"
           :empty-lines 1)

          ("r" "Run log" entry
           (file+datetree "~/org/runs.org")
           "* 🏃 Run: %^{Distance} km
:PROPERTIES:
:Time:      %U
:Location:  %^{Location}
:HR_Avg:    %^{Heart Rate (avg)} bpm
:Pace:      %^{Pace}
:END:
"
           :empty-lines 1)

          ("m" "Mood Log" entry
           (file+datetree "~/org/mood.org")
           "Mood (0–5): "
           :empty-lines 1))))



(defun my/org-roam-capture-template-first ()
  "Bypass the node list: pick an Org-roam template by key and description, then create a new node."
  (interactive)
  (let* ((templates org-roam-capture-templates)
         ;; Build choices like "c) Course Note"
         (choices (mapcar (lambda (tpl)
                            (format "%s) %s" (car tpl) (nth 1 tpl)))
                          templates))
         ;; Prompt on those choices
         (selection (completing-read "Roam template: " choices nil t))
         ;; Extract the key (the char before the ')')
         (key (substring selection 0 (string-match-p ")" selection)))
         ;; Find the full template by that key
         (template (cl-find key templates :key #'car :test #'string=)))
    ;; Now call org-roam-capture with only that one template
    (org-roam-capture
     nil               ; no prefix arg
     key               ; template key
     :filter-fn (lambda (_node) nil)
     :templates (list template))))

;; Bind it wherever you like—here we make SPC n r C jump straight into it:
(map! :leader
      :prefix ("n i" . "org-roam")
      :desc  "Template-first roam capture"
      "C"    #'my/org-roam-capture-template-first)

(use-package! org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "◎" "◌")
        org-superstar-item-bullet-alist '((?* . ?•)
                                          (?+ . ?➤)
                                          (?- . ?–))
        org-superstar-leading-bullet " "          ;; indent spacing
        org-superstar-remove-leading-stars t      ;; let superstar hide stars
        org-hide-leading-stars nil))              ;; don't double-hide

;; ❌ This line is now redundant and can be removed:
;; (add-hook 'org-mode-hook #'org-superstar-mode)

                                        ;
                                        ;Custom header colours
(after! doom-themes
  (custom-set-faces!
    '(default :foreground "#E5E9F0"))) ;; Lightest Nord color


(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (C . t)
     (cpp . t)))
  ;; Optional: Disable confirmation prompt when evaluating code blocks
  (setq org-confirm-babel-evaluate nil))


(setq org-journal-date-format "%A, %d %B %Y"
      org-journal-dir "~/org/journal/"
      org-journal-file-type 'daily
      org-journal-enable-agenda-integration t

      ;; This is the template that gets inserted on new journal entries
      org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "*** "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-find-file 'find-file

      org-journal-carryover-items nil) ;; don't carry over unfinished todos
(defun my/cbt-journal-template ()
  (goto-char (point-max))
  (insert "
* 🧠 CBT Journal Template
Use this daily or when your mood noticeably shifts. Each question is a separate area to reflect clearly. Keep responses raw, honest, and non-judgemental.

** 🧩 What happened?
What happened externally today? Keep this factual and judgment-free. Think of it like a camera recording what you did or what occurred — not what you felt or thought.
Group into categories like:
- Events you did
- Things you skipped or avoided
- Social interactions

-

** 🧠 What were my automatic thoughts?
Write down the raw, unfiltered thoughts that went through your mind. These are often critical, irrational, or emotionally charged. Avoid saying “I think…” — just write the thought the way it felt in your head.

-

** 💥 What emotions did I feel? (0–100%) [[id:849ac602-9736-406b-85d1-04329db7ab0b][List of Emotions]])
Label each emotion you felt and give it a percentage intensity (0–100%). You can include what triggered each emotion if helpful. Include both positive and negative emotions if they were present.

-

** ⚡ What did I do or feel like doing?
This section helps map the link between your emotions/thoughts and your actions. Break it down into:
- Action / Urge: What you did or felt like doing as a reaction
- Avoidance: What you didn’t do, possibly to avoid discomfort or emotion
- Coping: What you did to manage how you were feeling (healthy or unhealthy)

- Action / Urge:
  -
- Avoidance:
  -
- Coping:
  -

** ❗ What thinking distortions were present?
Identify distorted thinking patterns [[id:a822de35-7b0d-4f4c-88bc-0b4e6db770db][Common Thought Distortions]]. These include mind reading, catastrophising, black-and-white thinking, emotional reasoning, labelling, should statements, etc. Refer to your linked distortions page.

-

** 🔄 What’s a more balanced thought?
Challenge your automatic thoughts with a rational, compassionate response. The goal is not fake positivity — just accuracy, fairness, and self-kindness.

-

** 📉 How do I feel now? (0–100%)
After journaling and reframing, reflect on how you feel now. Rate the intensity and note any shifts in mood.

-

** 🌱 What can I try tomorrow?
Write down a few simple intentions or experiments to try tomorrow. These can be related to thoughts, behaviours, or emotional awareness. Keep them small and specific.

-

** 🌞 What went well today?
Acknowledge any positive moments or small wins from the day. This helps reinforce healthy patterns and trains your brain to notice positives.

-
"))

(add-hook 'org-journal-after-entry-create-hook #'my/cbt-journal-template)

(add-to-list 'default-frame-alist '(width . 127))   ;; columns (characters)
(add-to-list 'default-frame-alist '(height . 44));; rows (lines)
(add-to-list 'default-frame-alist '(top . 8))
(add-to-list 'default-frame-alist '(left . 180))
(map! :leader
      (:prefix ("n j" . "journal")
       :desc "New journal entry" "j" #'org-journal-new-entry))


(setq doom-font (font-spec :family "Lekton Nerd Font Mono" :size 26 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Lekton Nerd Font" :size 26 :weight 'normal)
      doom-big-font (font-spec :family "Lekton Nerd Font Mono" :size 28 :weight 'normal)
      doom-serif-font (font-spec :family "Lekton Nerd Font Mono" :weight 'normal))
;; ── Clean Org Headings for Terminal ─────────────────────────
(after! org

  (setq org-hide-emphasis-markers t)  ;; prevent bold parsing bugs in headings
  (setq org-hide-leading-stars nil)
  (setq org-startup-indented t)
  (setq org-ellipsis "…")
  (setq org-startup-folded 'content)
  (setq org-id-link-to-org-use-id t))

 (after! org
  ;; Agenda files and archive setup
  (setq org-agenda-files
        (append '("~/org/todos.org"
                  "~/org/habits.org"
                  "~/org/journal/")
                (directory-files-recursively "~/org/roam/" "\\.org$")))
  (setq org-archive-location "~/org/archive.org::")

  ;; Agenda display settings
  (setq org-agenda-span 30)                         ;; default view span
  (setq org-agenda-phases-of-moon t)
  (setq org-agenda-include-deadlines t)
  (setq org-agenda-include-diary t)

  ;; TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; TODO keyword colors
  (setq org-todo-keyword-faces
        '(("TODO"      . (:foreground "#ff6c6b" :weight bold))
          ("WAITING"   . (:foreground "#da8548" :weight bold))
          ("BLOCKED"   . (:foreground "#dcaeea" :weight bold))
          ("DONE"      . (:foreground "#98be65" :weight bold))
          ("CANCELLED" . (:foreground "#5B6268" :weight bold))))

  ;; Priority faces
  (setq org-priority-faces
        '((?A . (:foreground "#ff6c6b" :weight bold))
          (?B . (:foreground "#ECBE7B" :weight bold))
          (?C . (:foreground "#46D9FF" :weight bold))))

;; Enable org-super-agenda for grouping in agenda view
(use-package! org-super-agenda
  :after org
  :config
  (org-super-agenda-mode))

(use-package! ox-md)

(require 'cl-lib)


(set-face-attribute 'mode-line nil :font "Lekton Nerd Font")
(setq doom-modeline-height 30     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))
(map! :leader
      :desc "Toggle neotree file viewer" "t n" #'neotree-toggle
      :desc "Open directory in neotree"  "d n" #'neotree-dir)

;;(defun my/autosave-and-minimize (_prompt)
;;  "Autosave all buffers and minimize Emacs instead of quitting.
;;The PROMPT argument is ignored but required for compatibility."
;;  (save-some-buffers t)
;;  (message "Autosaved. Minimising instead of quitting.")
 ;; (iconify-frame)
 ;; nil) ;; block quit

;; Replace quit behavior with autosave + minimize
;;(setq confirm-kill-emacs #'my/autosave-and-minimize)


(map! :leader
      :prefix ("n r d" . "safe-daily")
      "b" nil)  ;; Unbind it so Doom doesn't crash

;; ─── LaTeX Setup ───────────────────────────────────────────────

;; Use pdf-tools for in-Emacs PDF viewing
(setq +latex-viewers '(pdf-tools))

(after! tex
  ;; Don't prompt to save before compile
  (setq TeX-save-query nil
        TeX-show-compilation t
        TeX-command-extra-options "-shell-escape")
  ;; Use latexmk as the default compile command
  (setq TeX-command-default "LatexMk")
  ;; Use default engine unless overridden
  (setq TeX-engine 'default))

;; Enable AUCTeX auto stuff
(use-package! tex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

;; Org-mode LaTeX preview and scaling (AFTER org is loaded!)
(after! org
  ;; Enable inline LaTeX previews at startup
  (setq org-startup-with-latex-preview t)

  ;; Increase LaTeX preview scale (default is 1.0)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0)))

;; Auto-preview LaTeX when cursor enters/exits it
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))


;; 📸 ORG-DOWNLOAD CONFIGURATION
(use-package! org-download
  :after org
  :config
  ;; Create image dir based on current file, only if file is saved
  (defun my/org-setup-download-dir ()
    (when buffer-file-name
      (setq-local org-download-image-dir
                  (concat (file-name-base buffer-file-name) "-imgs"))))

  ;; Setup image dir automatically when:
  (add-hook 'org-mode-hook #'my/org-setup-download-dir)         ;; opening org file
  (add-hook 'after-save-hook #'my/org-setup-download-dir)       ;; saving new file

  (after! org-roam
    (add-hook 'org-roam-capture-new-node-hook #'my/org-setup-download-dir)) ;; new roam node

  ;; Paste image from clipboard with: SPC m i
  (map! :map org-mode-map
        :leader
        :prefix "m"
        :desc "📋 Paste image from clipboard" "i" #'org-download-clipboard)

  ;; Drag-and-drop support (optional)
  (setq org-download-method 'directory
        org-download-image-org-width 600                             ;; scale image previews
        org-download-screenshot-method "screencapture -i %s")       ;; macOS screenshot tool
)

(remove-hook 'org-mode-hook #'org-modern-mode)

;; Coding in C

(setq lsp-clients-clangd-executable "/opt/homebrew/opt/llvm/bin/clangd")

(after! org
  (setq org-agenda-custom-commands
        '(("d" "📆 Daily Agenda + Grouped Tasks"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-start-day nil)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'regexp ":STYLE:.*habit"))))
            (tags-todo "*"
                       ((org-agenda-overriding-header "📝 Tasks")
                        (org-super-agenda-groups
                         '((:name "🧠 Study" :category "study")
                           (:name "🛠 Projects" :category "projects")
                           (:name "☕ Personal" :category "personal")
                           (:name "🗃 Misc" :category "misc")))))))
          ("h" "🌱 Habit Tracker"
           ((agenda ""
                ((org-agenda-span 'day)
                 (org-agenda-start-day nil)
                 (org-habit-show-habits-only-for-today nil)
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'notregexp ":STYLE:.*habit"))))))

          ("s" "🧠 Study"
           ((tags-todo "+CATEGORY=\"study\"")))
          ("p" "🛠 Projects"
           ((tags-todo "+CATEGORY=\"projects\"")))
          ("x" "🗃 Misc"
           ((tags-todo "+CATEGORY=\"misc\"")))
          ("l" "☕ Personal"
           ((tags-todo "+CATEGORY=\"personal\"")))
          ("r" "📋 Weekly Review"
           ((agenda ""
                    ((org-agenda-span 14)
                     (org-agenda-start-day "-1d")
                     (org-agenda-overriding-header "🧾 Last 7 Days")))))))))

;; (after! org
;;   (use-package! org-notify
;;     :config
;;     ;; Define notification styles
;;     (org-notify-add 'default
;;       '(:time "-15m" :period "2m" :duration 10 :actions -notify))
;;     (org-notify-add 'habit
;;       '(:time "-10m" :period "5m" :duration 10 :actions -notify))

;;     ;; Use terminal-notifier (via libnotify backend on macOS)
;;     (setq org-notify-default-style 'libnotify)

;;     ;; Start the org-notify daemon
;;     (org-notify-start)

;;     ;; Check notifications every 60 seconds
;;     (run-at-time "00:01" 60 'org-notify-check)

;;     ;; Optional: Auto-add NOTIFY property for habits
;;     (defun my/org-habit-auto-notify ()
;;       (when (string= (org-entry-get nil "STYLE") "habit")
;;         (org-entry-put nil "NOTIFY" "habit")))
;;     (add-hook 'org-after-todo-state-change-hook #'my/org-habit-auto-notify)))

;; (after! org
;;   (require 'org-notify)

;;   ;; Define a default notification style (e.g., system notifications)
;;   (org-notify-add 'default
;;     '(:time "10m" :period "2m" :duration 60 :actions -notify))

;;   ;; Check every 60 seconds for upcoming notifications
;;   (run-at-time "1 min" 60 #'org-notify-check))
;; ;

;; ======================================
;; Embedded C/C++ Development Configuration
;; For ENCE464 Fitness Monitor Project
;; ======================================
(setq projectile-project-search-path '(("~/Developer/" . 2))) ; depth 2
;; CMake support
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Project management with CMake support
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Register CMake as a project type
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                  :compile "cmake --build build"
                                  :test "ctest"
                                  :configure "cmake -B build"))

;; LSP Mode for intelligent code completion
(use-package lsp-mode
  :hook ((cmake-mode . lsp-deferred))  ; Fixed: cmake-mode (not cmake.mode)
  :ensure t
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-completion-provider :capf
        ;; Clangd settings for embedded development
        lsp-clients-clangd-args
        '("--header-insertion-decorators=0"
          "--compile-commands-dir=build"
          "--background-index")))

;; LSP UI enhancements
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t))

;; Company for auto-completion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  ;; Disable other checkers in favor of LSP
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))

;; DAP Mode for debugging
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (require 'dap-gdb-lldb)
  (dap-auto-configure-mode 1)

  ;; Template for ARM debugging with OpenOCD
  (dap-register-debug-template
   "ARM Embedded Debug"
   (list :type "gdb"
         :request "attach"
         :name "ARM Debug"
         :gdbpath "arm-none-eabi-gdb"
         :target ":3333"
         :remote t
         :cwd "${workspaceFolder}"
         :program "${workspaceFolder}/build/fitness-tracker.elf")))

;; ======================================
;; Custom functions for embedded development
;; ======================================

(defun embedded-compile ()
  "Compile the project using CMake."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "cmake --build build")))

(defun embedded-configure ()
  "Configure CMake build."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "cmake -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")))

(defun embedded-flash ()
  "Flash firmware to device."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "openocd -f board/ti_stellaris_launchpad.cfg -c \"program build/fitness-tracker.elf verify reset exit\"")))

(defun embedded-clean ()
  "Clean build directory."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "cmake --build build --target clean")))

;; OpenOCD management
(defvar embedded--openocd-process nil)

(defun embedded-openocd-start ()
  "Start OpenOCD server."
  (interactive)
  (if (and embedded--openocd-process (process-live-p embedded--openocd-process))
      (message "OpenOCD already running")
    (let ((default-directory (projectile-project-root)))
      (setq embedded--openocd-process
            (start-process "openocd" "*openocd*" "openocd" "-f" "board/ti_stellaris_launchpad.cfg"))
      (message "OpenOCD started"))))

(defun embedded-openocd-stop ()
  "Stop OpenOCD server."
  (interactive)
  (when (and embedded--openocd-process (process-live-p embedded--openocd-process))
    (kill-process embedded--openocd-process)
    (setq embedded--openocd-process nil)
    (message "OpenOCD stopped")))

;; ======================================
;; Key bindings
;; ======================================

(global-set-key (kbd "C-c e c") 'embedded-compile)
(global-set-key (kbd "C-c e C") 'embedded-configure)
(global-set-key (kbd "C-c e f") 'embedded-flash)
(global-set-key (kbd "C-c e x") 'embedded-clean)
(global-set-key (kbd "C-c e o") 'embedded-openocd-start)
(global-set-key (kbd "C-c e k") 'embedded-openocd-stop)
(global-set-key (kbd "C-c e d") 'dap-debug)

;; ======================================
;; C/C++ style settings
;; ======================================

(setq c-default-style "linux"
      c-basic-offset 4)

(defun my-c-mode-hook ()
  "Custom C/C++ mode settings."
  (setq tab-width 4
        indent-tabs-mode nil)
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Transparency
(defun my/set-transparency ()
  (set-frame-parameter (selected-frame) 'alpha '(90 . 60))
  (add-to-list 'default-frame-alist '(alpha . (90 . 60))))

(add-hook 'window-setup-hook #'my/set-transparency)

(use-package! gptel
  :config
  (setq
   ;; Choose your default DeepSeek model:
   gptel-model 'deepseek-r1:latest

   ;; Register the Ollama backend and expose your DeepSeek models
   gptel-backend
   (gptel-make-ollama "Ollama"
     :host   "http://localhost:11434"
     :stream t
     :models '(deepseek-r1:latest
               deepseek-coder:latest
               deepseekcoder:latest))))

(defun my/ollama-server-running-p ()
  "Return non-nil if an Ollama serve process is already running."
  (seq-some
   (lambda (proc)
     (when-let ((cmd (car (process-command proc))))  ; guard nil
       (string-match-p "^ollama serve" cmd)))
   (process-list)))

(defun my/ensure-ollama-serve (&rest _args)
  "Start `ollama serve` in the background if it isn’t already running."
  (unless (my/ollama-server-running-p)
    (message "🚀 Starting Ollama server…")
    (start-process
     "ollama-server"           ; process name
     "*ollama-server*"         ; buffer
     "ollama" "serve")))

;; Advise before any gptel calls—even if they pass args now.
(advice-add #'gptel         :before #'my/ensure-ollama-serve)
(advice-add #'gptel-rewrite :before #'my/ensure-ollama-serve)

;; *** Keybinding ***
(map! :leader
      :desc "Chat with DeepSeek (gptel)" "c d" #'gptel)

(after! lsp-mode
  ;; Turn off all file-watcher ignores and disable watcher startup entirely
  (setq lsp-enable-file-watchers nil
        lsp-file-watch-ignored-directories '()))
(setq org-hugo-base-dir "~/Developer/personal/KieranDigitalNotes/")
(setq org-export-with-broken-links t)

(defun my/org-hugo-export-non-private-notes ()
  "Export Org-roam notes (including courses) that are not tagged :private: to Hugo/Quartz, with numeric tag quoting and date patching."
  (interactive)
  (let ((note-dirs '("~/org/roam" "~/org/roam/courses")))
    (dolist (dir note-dirs)
      (dolist (file (directory-files-recursively dir "\\.org$"))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (unless (re-search-forward ":private:" nil t)
            ;; 🔧 Quote numeric filetags
            (let* ((tags (org-get-tags))
                   (safe-tags (mapcar (lambda (tag)
                                        (if (string-match-p "^[0-9]+$" tag)
                                            (format "\"%s\"" tag)
                                          tag))
                                      tags)))
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^#\\+filetags:" nil t)
                  (replace-match (concat "#+filetags: :" (mapconcat #'identity safe-tags ":") ":")))))
            ;; 📆 Fix date format if needed
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "^#\\+date: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)$" nil t)
                (replace-match "\\1T00:00:00+12:00" nil nil nil 1)))
            ;; ➕ Ensure hugo_section exists
            (save-excursion
              (goto-char (point-min))
              (unless (re-search-forward "^#\\+hugo_section:" nil t)
                (forward-line 1)
                (insert "#+hugo_section: .\n")))
            ;; 📝 Export
            (org-hugo-export-to-md)))))))
`

(defun my/org-add-hugo-section-to-all ()
  "Add #+hugo_section: . to all Org-roam notes (including courses) that don't already have it."
  (interactive)
  (let ((note-dirs '("~/org/roam" "~/org/roam/courses"))) ;; add more if needed
    (dolist (dir note-dirs)
      (dolist (file (directory-files-recursively dir "\\.org$"))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (unless (re-search-forward "^#\\+hugo_section:" nil t)
            (goto-char (point-min))
            (forward-line)
            (insert "#+hugo_section: .\n")
            (save-buffer)))))))


(map! :leader
      :prefix ("m q" . "Quartz export")
      :desc "Export all non-private notes to Quartz"
      "p" #'my/org-hugo-export-non-private-notes
      :desc "Add hugo_section to all notes"
      "s" #'my/org-add-hugo-section-to-all)
