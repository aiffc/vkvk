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

(defun create-fence (device &key
			      (next +vk-null-ptr+)
			      (flags 0)
			      (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkFenceCreateInfo))
			 (p-fence 'VkFence))
    (setf (mem-ref p-info '(:struct VkFenceCreateInfo))
	  (list :sType +structure-type-fence-create-info+
		:pNext next
		:flags flags))
    (check-vk-result (vkCreateFence device p-info allocator p-fence))
    (mem-ref p-fence 'VkFence)))

(defun destroy-fence (device fence &optional (allocator +vk-null-ptr+))
  (vkDestroyFence device fence allocator))

(defun wait-for-fences (device &key
				 (fences nil)
				 (wait-all vk_false)
				 (timeout 1000))
  (let ((fence-count (length fences)))
    (with-foreign-object (p-fence 'VkFence fence-count)
      (dotimes (i fence-count)
	(setf (mem-aref p-fence 'VkFence i) (nth i fences)))
      (check-vk-result (vkWaitForFences device fence-count p-fence wait-all timeout)))))

(defun reset-fences (device &key (fences nil))
  (let ((fence-count (length fences)))
    (with-foreign-object (p-fence 'VkFence fence-count)
      (dotimes (i fence-count)
	(setf (mem-aref p-fence 'VkFence i) (nth i fences)))
      (check-vk-result (vkResetFences device fence-count p-fence)))))

(defun get-fence-status (device fence)
  (check-vk-result (vkGetFenceStatus device fence)))
