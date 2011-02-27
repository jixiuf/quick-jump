;;; quick-jump.el ---Remember current position,and jump back cyclely.

;; Filename: quick-jump.el
;; Description:Remember current position,and jump back cyclely.
;; Author: Joseph <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; Copyright (C) 2011~, Joseph, all rights reserved.
;; Created: 2011-02-22
;; Version: 0.1.0
;; URL: 
;; Keywords: quick jump marker
;; Compatibility: (Test on GNU Emacs 23.2.1).
;;
;;
;;; This file is NOT part of GNU Emacs
;;
;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `quick-jump-push-marker'
;;    push current marker in ring. you can jump back
;;  `quick-jump-history-go-back'
;;    Go back in `qj-marker-ring'.
;;  `quick-jump-history-go-forward'
;;    Go forward in `qj-marker-ring'.
;;  `quick-jump-clear-all-marker'
;;    clear all marker in `qj-marker-ring'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;此段代码用于快速跳转,主要用于代码浏览或代码编写时在多个大跨度的位置间进行切换时使用
;;以我的键绑定为例
;;
;; (global-set-key (kbd "C-.") 'quick-jump-push-marker)
;;
;; (global-set-key (kbd "C-,") 'quick-jump-history-go-back)
;; (global-set-key (kbd "C-<") 'quick-jump-history-go-forward)
;;
;;最简单的使用方法
;;`C-.' 会记录当前光标的位置
;;`C-,' 跳回到`C-.' 所记录的位置
;;当然你可以在文件不同的位置多处按下`C-.' ,而`C-,'则会按照先后顺序遍历这些位置
;;
;;
;;但上面的使用方法有个弊端,必须先经`C-.' 保存位置后,才可以`C-,' 进行跳转,有时会忘掉`C-.',以
;; 致无法跳转到这个位置.
;;为了解觉这个问题 ,`C-,'不但可以进行跳转,也会根据当前的位置在特定的情况下自动将当前位置加入到
;;循环队列中.
;;(print qj-marker-ring)
(require 'ring)

(defvar qj-current-marker nil)
(defvar qj-marker-ring (make-ring 5))
(defvar qj-previous-action-flag nil)
(defvar qj-line-count 10)

;;; util func
(defun qj-is-marker-available(marker)
  "return nil if marker is nil or  in dead buffer ,
   return marker if it is live"
  (if (and marker
           (markerp marker)
           (marker-buffer marker))
      marker))

(defmacro qj-if+ (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it'
  is the result of test-form.(borrowed from anything.el)"
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
;;; funcs
(defun qj-history-init()
  "remove #<marker in no buffer> from `qj-marker-ring'."
      (let ((tmp-marker-ring))
        (while (not (ring-empty-p qj-marker-ring))
          (qj-if+ (qj-is-marker-available (ring-remove qj-marker-ring 0))
              (setq tmp-marker-ring (append tmp-marker-ring (list it)));;new item first
              (while (not (ring-empty-p qj-marker-ring));;remove all old marker
              (ring-remove qj-marker-ring))))
        ;;reinsert all available marker to `qj-marker-ring'
        (mapc (lambda(marker) (ring-insert-at-beginning qj-marker-ring marker)) tmp-marker-ring))
      ;;add (point-marker) to marker-ring, when ...
        (when (and (not (ring-empty-p qj-marker-ring))
                   (not (ring-member qj-marker-ring (point-marker)))
                    (or (not (equal (marker-buffer qj-current-marker) (current-buffer)))
                       (>  (count-lines  (point) (marker-position qj-current-marker)) qj-line-count)))
          (ring-insert qj-marker-ring (point-marker)))
        
        (when (ring-empty-p qj-marker-ring)
          (message "please push marker before jumping. using `quick-jump-push-marker'")))



(defun qj-history-action-go(marker)
  "Go to location."
  (let ((buf (marker-buffer marker))
        (pos (marker-position marker)))
    (when buf
      (switch-to-buffer buf)
      (set-buffer buf)
      (goto-char pos)))
  (setq qj-current-marker marker))

(defun quick-jump-push-marker()
  "push current marker in ring. you can jump back
by `quick-jump-history-go-back'"
  (interactive)
  (when (not (ring-member qj-marker-ring (point-marker)))
    (ring-insert qj-marker-ring (point-marker)))
  (setq qj-current-marker (point-marker))
  (message "a marker is pushed."))

(defun quick-jump-history-go-back()
  "Go back in `qj-marker-ring'."
  (interactive)
  (qj-history-init)
  (when (and
         (qj-is-marker-available qj-current-marker)
         (ring-member qj-marker-ring qj-current-marker))
    (when (and (not (equal qj-previous-action-flag "back"))
               (equal (current-buffer) (marker-buffer qj-current-marker))
               (<  (count-lines  (point) (marker-position qj-current-marker)) qj-line-count))
      (setq qj-current-marker (ring-next qj-marker-ring qj-current-marker)))
    
    (message (concat  "jump to " (prin1-to-string qj-current-marker)))
    (qj-history-action-go qj-current-marker)
    
    (setq qj-current-marker (ring-next qj-marker-ring qj-current-marker))
    (setq qj-previous-action-flag "back")))

(defun quick-jump-history-go-forward()
  "Go forward in `qj-marker-ring'."
  (interactive)
  (qj-history-init)
  (when (and
         (qj-is-marker-available qj-current-marker)
         (ring-member qj-marker-ring qj-current-marker))
    (when (and (not (equal qj-previous-action-flag "forward"))
               (equal (current-buffer) (marker-buffer qj-current-marker))
               (<  (count-lines  (point) (marker-position qj-current-marker)) qj-line-count)) 
      (setq qj-current-marker (ring-previous qj-marker-ring qj-current-marker)))
    
    (message (concat  "jump to " (prin1-to-string qj-current-marker)))
    (qj-history-action-go qj-current-marker)
    
    (setq qj-current-marker (ring-previous qj-marker-ring qj-current-marker))
    (setq qj-previous-action-flag "forward")))


(defun quick-jump-clear-all-marker()
  "clear all marker in `qj-marker-ring'."
  (interactive)
  (message "clear all marker for joseph-quick-jump.")
  (setq qj-previous-action-flag nil)
  (setq qj-current-marker nil)
  (while (not (ring-empty-p qj-marker-ring))
    (ring-remove qj-marker-ring)))


(global-set-key (kbd "C-,") 'quick-jump-history-go-back)
(global-set-key (kbd "C-.") 'quick-jump-push-marker)
(global-set-key (kbd "C-<") 'quick-jump-history-go-forward)
(global-set-key (kbd "C->") 'quick-jump-clear-all-marker)

(provide 'quick-jump)

;;;joseph-quick-jump.el ends here.

