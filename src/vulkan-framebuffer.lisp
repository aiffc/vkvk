;; MIT License

;; Copyright (c) 2021 aiffc

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(in-package :vkvk)

(defun create-framebuffer (device render-pass &key
						 (next +vk-null-ptr+)
						 (flags 0)
						 (image-views nil)
						 (width 600)
						 (height 600)
						 (layer 1)
						(allocator +vk-null-ptr+))
  (let ((image-view-count (length image-views)))
    (with-foreign-objects ((p-info '(:struct VkFramebufferCreateInfo))
			   (p-frame-buffer 'VkFramebuffer)
			   (p-image-view 'VkImageView image-view-count))
      (dotimes (i image-view-count)
	(setf (mem-aref p-image-view 'VkImageView i) (nth i image-views)))
      (setf (mem-ref p-info '(:struct VkFramebufferCreateInfo))
	    (list :sType +structure-type-framebuffer-create-info+
		  :pNext next
		  :flags flags
		  :renderPass render-pass
		  :attachmentCount image-view-count
		  :pAttachments p-image-view
		  :width width
		  :height height
		  :layers layer))
      (check-vk-result (vkCreateFramebuffer device p-info allocator p-frame-buffer))
      (mem-ref p-frame-buffer 'VkFramebuffer))))

(defun destroy-framebuffer (device framebuffer &optional (allocator +vk-null-ptr+))
  (vkDestroyFramebuffer device framebuffer allocator))
