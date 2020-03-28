;; -*- lexical-binding: t -*-

;; Disable package system since I configure it myself
(setq package-enable-at-startup nil)

;; Configure default frame properties
(push '(width . 120) default-frame-alist)
(push '(height . 80) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
