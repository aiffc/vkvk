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

(defun create-buffer (device queue-family-indices &key
						    (next +vk-null-ptr+)
						    (flags 0)
						    (size 0)
						    (usage +buffer-usage-index-buffer-bit+)
						    (sharing-mode +sharing-mode-exclusive+)
						    (allocator +vk-null-ptr+))
  (let ((queue-count (length queue-family-indices)))
    (with-foreign-objects ((p-info '(:struct VkBufferCreateInfo))
			   (p-queue-family-indices :uint32 queue-count)
			   (p-buffer 'VkBuffer))
      (dotimes (i queue-count)
	(setf (mem-aref p-queue-family-indices :uint32 i) (nth i queue-family-indices)))
      (setf (mem-ref p-info '(:struct VkBufferCreateInfo))
	    (list :sType +structure-type-buffer-create-info+
		  :pNext next
		  :flags flags
		  :size size 
		  :usage usage 
		  :sharingMode sharing-mode 
		  :queueFamilyIndexCount queue-count
		  :pQueueFamilyIndices queue-family-indices))
      (check-vk-result (vkCreateBuffer device p-info allocator p-buffer))
      (mem-ref p-buffer 'VkBuffer))))

(defun create-buffer-view (device buffer &key
					   (next +vk-null-ptr+)
					   (flags 0)
					   (format +format-r8g8b8a8-srgb+)
					   (offset 0)
					   (range 0)
					   (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkBufferViewCreateInfo))
			 (p-buffer-view 'VkBufferView))
    (setf (mem-ref p-info '(:struct VkBufferViewCreateInfo))
	  (list :sType +structure-type-buffer-view-create-info+
		:pNext next
		:flags flags
		:buffer buffer
		:format format
		:offset offset
		:range range))
    (check-vk-result (vkCreateBufferView device p-info allocator p-buffer-view))
    (mem-ref p-buffer-view 'VkBufferView)))

(defun destroy-buffer (device buffer &optional (allocator +vk-null-ptr+))
  (vkDestroyBuffer device buffer allocator))

(defun destroy-buffer-view (device buffer-view &optional (allocator +vk-null-ptr+))
  (vkDestroyBufferView device buffer-view allocator))
