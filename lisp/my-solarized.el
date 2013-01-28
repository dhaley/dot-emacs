;;; my-solarized.el --- Personalization of emacs24 solarized theme

;;; Commentary:
;;

(require 'solarized)

;;; Code:

(defun my-solarized-childtheme ()
  "My solarized child theme."

  (custom-theme-set-faces
   theme-name

   ;; notes -
   ;; file =      fg:normal
   ;; directory = fg:blue
   ;; hlite =
   ;;       bg:hl
   ;;       bg:hl+fg:emph+bold
   ;;       bg:hl+fg:yellow+bold
   ;;       bg:hl+fg:green+(maybe)bold

   ;; mark = fg:magenta+bold


   ;; kite, very much quick hacky
   `(bg:kite-dataReceived ((,class (:background ,magenta))))
   `(bg:kite-receiveHeadersEnd ((,class (:background ,green))))
   `(bg:kite-requestStart ((,class (:background ,red))))
   `(bg:kite-sendEnd ((,class (:background ,cyan))))
   `(bg:kite-table-head ((,class (:background ,solarized-hl))))
   `(bg:kite-tick ((,class (:background ,solarized-hl))))
   `(kite-css-computed-proprietary-unused-property ((,class (:inherit kite-css-proprietary-property :foreground ,blue))))
   `(kite-css-computed-unused-property ((,class (:inherit kite-css-property :foreground ,blue))))
   `(kite-css-value-widget-error ((,class (:background ,orange-lc :foreground ,orange-hc))))
   `(kite-css-value-widget-modified ((,class (:background ,solarized-hl :foreground ,yellow))))
   `(kite-delimited-data-face ((,class (:foreground ,green))))
   `(kite-delimiter-face ((,class (:foreground ,solarized-emph))))
   `(kite-modified-attribute-local-name-face ((,class (:inherit kite-attribute-local-name-face :background ,solarized-hl))))
   `(kite-modified-attribute-value-face ((,class (:inherit kite-attribute-value-face :background ,solarized-hl))))
   `(kite-modified-element-local-name-face ((,class (:inherit kite-element-local-name-face :background ,solarized-hl))))
   `(kite-name-face ((,class (:foreground ,blue))))
   `(kite-proto-property-name ((,class (:inherit default :foreground ,solarized-hl))))
   `(kite-ref-face ((,class (:foreground ,cyan))))
   `(kite-session-closed ((,class (:inherit default :background ,red))))
   `(kite-text-face ((,class (:background nil :foreground ,solarized-comments))))
   `(kite-node-highlight-face ((,class (:background ,solarized-hl))))

   `(bg:kite-pageStart ((,class nil)))
   `(kite-attribute-colon-face ((,class (:inherit kite-name-face))))
   `(kite-attribute-local-name-face ((,class (:inherit kite-name-face))))
   `(kite-attribute-prefix-face ((,class (:inherit kite-name-face))))
   `(kite-attribute-value-delimiter-face ((,class (:inherit kite-delimiter-face))))
   `(kite-attribute-value-face ((,class (:inherit kite-delimited-data-face))))
   `(kite-boolean ((,class (:inherit nxml-char-ref-number))))
   `(kite-cdata-section-CDATA-face ((,class (:inherit kite-name-face))))
   `(kite-cdata-section-content-face ((,class (:inherit kite-text-face))))
   `(kite-cdata-section-delimiter-face ((,class (:inherit kite-delimiter-face))))
   `(kite-char-ref-delimiter-face ((,class (:inherit kite-ref-face))))
   `(kite-char-ref-number-face ((,class (:inherit kite-ref-face))))
   `(kite-comment-content-face ((,class (:slant italic))))
   `(kite-comment-delimiter-face ((,class (:inherit kite-delimiter-face))))
   `(kite-console-prompt-face ((,class (:inherit default))))
   `(kite-css-property ((,class (:inherit css-property))))
   `(kite-css-proprietary-property ((,class (:inherit css-proprietary-property))))
   `(kite-css-selected-overlay ((,class (:inherit secondary-selection))))
   `(kite-css-selector ((,class (:inherit css-selector))))
   `(kite-element-colon-face ((,class (:inherit kite-name-face))))
   `(kite-element-local-name-face ((,class (:inherit kite-name-face))))
   `(kite-element-prefix-face ((,class (:inherit kite-name-face))))
   `(kite-entity-ref-delimiter-face ((,class (:inherit kite-ref-face))))
   `(kite-entity-ref-name-face ((,class (:inherit kite-ref-face))))
   `(kite-hash-face ((,class (:inherit kite-name-face))))
   `(kite-link-face ((,class (:inherit change-log-file))))
   `(kite-loading ((,class (:inherit font-lock-comment))))
   `(kite-log-debug ((,class (:inherit font-lock-comment))))
   `(kite-log-error ((,class (:inherit error))))
   `(kite-log-log ((,class (:inherit default))))
   `(kite-log-warning ((,class (:inherit warning))))
   `(kite-markup-declaration-delimiter-face ((,class (:inherit kite-delimiter-face))))
   `(kite-namespace-attribute-colon-face ((,class (:inherit kite-name-face))))
   `(kite-namespace-attribute-prefix-face ((,class (:inherit kite-name-face))))
   `(kite-namespace-attribute-value-delimiter-face ((,class (:inherit kite-attribute-value-delimiter-face))))
   `(kite-namespace-attribute-value-face ((,class (:inherit kite-attribute-value-face))))
   `(kite-namespace-attribute-xmlns-face ((,class (:inherit kite-name-face))))
   `(kite-null ((,class (:inherit nxml-char-ref-number))))
   `(kite-number ((,class (:inherit nxml-char-ref-number))))
   `(kite-object ((,class (:inherit font-lock-variable-name))))
   `(kite-processing-instruction-content-face ((,class (:inherit kite-delimited-data-face))))
   `(kite-processing-instruction-delimiter-face ((,class (:inherit kite-delimiter-face))))
   `(kite-processing-instruction-target-face ((,class (:inherit kite-name-face))))
   `(kite-prolog-keyword-face ((,class (:inherit kite-name-face))))
   `(kite-prolog-literal-content-face ((,class (:inherit kite-delimited-data-face))))
   `(kite-prolog-literal-delimiter-face ((,class (:inherit kite-delimiter-face))))
   `(kite-property-name ((,class (:inherit default))))
   `(kite-quote ((,class (:inherit font-lock-keyword))))
   `(kite-stack-column-number ((,class (:inherit kite-number))))
   `(kite-stack-error-message ((,class (:inherit default))))
   `(kite-stack-error-type ((,class (:inherit error))))
   `(kite-stack-file-name ((,class (:inherit link))))
   `(kite-stack-function-name ((,class (:inherit font-lock-function-name-face))))
   `(kite-stack-line-number ((,class (:inherit kite-number))))
   `(kite-stack-pseudo-file-name ((,class (:inherit default))))
   `(kite-string ((,class (:inherit font-lock-string))))
   `(kite-table-head ((,class (:inherit highlight))))
   `(kite-tag-delimiter-face ((,class (:inherit kite-delimiter-face))))
   `(kite-tag-slash-face ((,class (:inherit kite-name-face))))
   `(kite-undefined ((,class (:inherit nxml-char-ref-number))))


    ;; (NEW) flymake faces
   ;; `(flymake-errline
   ;;   ((,class (:background nil :underline ,orange :weight bold))))
   ;; `(flymake-infoline ((,class (:background nil :underline ,green :weight bold))))
   ;; `(flymake-warnline
   ;;   ((,class (:background nil :underline ,yellow :weight bold))))


   ;; my extreme whitespace-mode - not sure i actually want this...
   ;; `(whitespace-space ((,class (:background ,solarized-bg :foreground ,yellow-lc
   ;;                                          :inverse-video t))))
   ;; `(whitespace-hspace ((,class (:background ,solarized-bg :foreground ,red-lc
   ;;                                           :inverse-video t))))
   ;; `(whitespace-tab ((,class (:background ,solarized-bg :foreground ,orange-lc
   ;;                                        :inverse-video t))))
   ;; `(whitespace-newline ((,class (:foreground ,solarized-comments))))
   ;; `(whitespace-trailing ((,class (:foreground ,blue-lc :background ,solarized-bg
   ;;                                             :inverse-video t))))
   ;;                                      ; removing inverse video on this
   ;; `(whitespace-line ((,class (:background ,solarized-bg :foreground ,magenta
   ;;                                         :inverse-video nil))))
   ;; `(whitespace-space-before-tab ((,class (:background ,solarized-bg :foreground ,green-lc
   ;;                                                     :inverse-video t))))
   ;; `(whitespace-indentation ((,class (:background ,solarized-bg :foreground ,magenta-lc
   ;;                                                :inverse-video t))))
   ;; `(whitespace-empty ((,class (:background ,solarized-fg :foreground ,red-lc :inverse-video t))))
   ;; `(whitespace-space-after-tab ((,class (:background ,solarized-bg  :foreground ,violet-lc
   ;;                                                    :inverse-video t))))


   ;; a tad more extreme coloring than I would expect from the default theme
   `(mode-line
     ((,class (:inverse-video nil
                              :underline nil
                              :foreground ,solarized-fg
                              :background ,solarized-bg
                              :box (:line-width 2 :color ,green :style nil)))))
   `(mode-line-buffer-id ((,class (:foreground ,green :weight bold))))
   `(mode-line-inactive
     ((,class (:inverse-video nil
                              :underline nil
                              :foreground ,solarized-comments
                              :background ,solarized-bg
                              :box (:line-width 2 :color ,solarized-hl :style nil)))))
   `(header-line
     ((,class (:inverse-video nil
                              :underline nil
                              :foreground ,solarized-emph
                              :background ,solarized-hl
                              :box (:line-width 2 :color ,solarized-hl :style nil)))))
   )

  (custom-theme-set-variables
   theme-name



   ;; `(pos-tip-foreground-color ,solarized-emph)
   ;; `(pos-tip-background-color ,solarized-hl)

   `(org-todo-keyword-faces
     (quote (("TODO" :foreground ,cyan :weight bold :slant italic :underline  t)
             ("NEXT" :foreground ,blue :weight bold :slant italic :underline t)
             ("ACTIVE" :foreground ,blue :weight bold :slant italic :underline t :inverse-video t)
             ("DONE" :foreground ,green :weight bold :slant italic :underline t)
             ("WAITING" :foreground ,orange :weight bold :slant italic :underline t)
             ("HOLD" :foreground ,magenta :weight bold :slant italic :underline t)
             ("NOTE" :foreground ,magenta :weight bold :slant italic :underline t)
             ("CANCELLED" :foreground ,green :weight bold :slant italic :underline t)
             ("PHONE" :foreground ,green :weight bold :slant italic :underline t))))

   `(org-tag-faces
     (quote (("@home" :slant italic)
             ("@office" :slant italic)
             ("@errand" :slant italic :inverse-video t)
             ("task" :slant italic :foreground ,yellow)
             ("bug" :slant italic :foreground ,yellow)
             ("enhancement" :slant italic :foreground ,yellow)
             ("work" :slant italic)
             ("personal" :slant italic)
             ("bulk" :foreground ,solarized-comments)
             ("hold" :foreground ,green :slant italic)
             ("note" :foreground ,magenta :slant italic)
             ("waiting" :foreground ,orange :slant italic :inverse-video t)
             ("cancelled" :foreground ,green :slant italic :slant italic)
             ("flagged" :foreground ,red :slant italic :inverse-video t))))


   ))


(provide 'my-solarized)

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-solarized.el ends here
