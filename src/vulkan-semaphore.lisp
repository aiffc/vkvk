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

(defun create-semaphore (device &key
				  (next +vk-null-ptr+)
				  (flags 0)
				  (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkSemaphoreCreateInfo))
			 (p-semaphore 'VkSemaphore))
    (setf (mem-ref p-info '(:struct VkSemaphoreCreateInfo))
	  (list :sType +structure-type-semaphore-create-info+
		:pNext next
		:flags flags))
    (check-vk-result (vkCreateSemaphore device p-info allocator p-semaphore))
    (mem-ref p-semaphore 'VkSemaphore)))

(defun destroy-semaphore (device semaphore &optional (allocator +vk-null-ptr+))
  (vkDestroySemaphore device semaphore allocator))
