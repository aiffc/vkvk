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


(defun bind-buffer-memory (device buffer memory size)
  (check-vk-result (vkBindBufferMemory device buffer memory size)))

(defun bind-image-memory (device image memory size)
  (check-vk-result (vkBindImageMemory device image memory size)))

#|
(defun bind-buffer-memory2-khx (device infos)
  (let ((count (length infos)))
    (with-foreign-object (p-info '(:struct VkBindBufferMemoryInfoKHX) count)
      (dotimes (i count)
	(setf (mem-aref p-info '(:struct VkBindBufferMemoryInfoKHX) i)
	      (nth i infos)))
      (check-vk-result (vkBindBufferMemory2KHX device device count p-info))
      (free-allocated-objs))))


(defun bind-image-memroy2-khx (device infos)
  (let ((count (length infos)))
    (with-foreign-object (p-info '(:struct VkBindImageMemoryInfoKHX))
      (dotimes (i count)
	(setf (mem-aref p-info '(:struct VkBindImageMemoryInfoKHX) i)
	      (nth i infos)))
      (check-vk-result (vkBindImageMemory2KHX device device count p-info))
      (free-allocated-objs))))
|#


(defun unmap-memory (device memory)
  (vkUnmapMemory device memory))

(defun get-buffer-memory-requirements (device buffer)
  (with-foreign-object (p-memory-requirements '(:struct VkMemoryRequirements))
    (vkGetBufferMemoryRequirements device buffer p-memory-requirements)
    (mem-ref p-memory-requirements '(:struct VkMemoryRequirements))))

(defun get-invalidate-mapped-memroy-ranges (device)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkInvalidateMappedMemoryRanges device p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-info '(:struct VkMappedMemoryRange) count)
	  (check-vk-result (vkInvalidateMappedMemoryRanges device p-count p-info))
	  (ptr->list p-info '(:struct VkMappedMemoryRange) count))))))

(defun map-memory (device memory offset size flags data-ptr)
  (check-vk-result (vkMapMemory device memory offset size flags data-ptr)))

(defun flush-mapped-memory-ranges (device ranges)
  (let* ((count (length ranges))
	 (p-ranges (foreign-alloc 'mapped-memory-range :count count :initial-contents ranges)))
    (check-vk-result (vkFlushMappedMemoryRanges device count p-ranges))
    (foreign-free p-ranges)))
