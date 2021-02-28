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

(defun allocate-command-buffers (pool device &key
					       (next +vk-null-ptr+)
					       (count 1)
					       (level +command-buffer-level-primary+))
  (with-foreign-objects ((p-info '(:struct VkCommandBufferAllocateInfo))
			 (p-buffers 'VkCommandBuffer count))
    (setf (mem-ref p-info '(:struct VkCommandBufferAllocateInfo))
	  (list :sType +structure-type-command-buffer-allocate-info+
		:pNext next
		:commandPool pool
		:level level
		:commandBufferCount count))
    (check-vk-result (vkAllocateCommandBuffers device p-info p-buffers))
    (ptr->list p-buffers 'VkCommandBuffer count)))

(defun allocate-descriptor-sets (device descriptor-pool set-layouts &key
							  (next +vk-null-ptr+))
  (let ((layout-count (length set-layouts)))
    (with-foreign-objects ((p-layouts 'VkDescriptorSetLayout layout-count)
			   (p-info '(:struct VkDescriptorSetAllocateInfo))
			   (p-set 'VkDescriptorSet layout-count))
      (when set-layouts
	(dotimes (i layout-count)
	  (setf (mem-aref p-layouts 'VkDescriptorSetLayout i) (nth i set-layouts))))
      (setf (mem-ref p-info '(:struct VkDescriptorSetAllocateInfo))
	    (list :sType +structure-type-descriptor-set-allocate-info+
		  :pNext next
		  :descriptorPool descriptor-pool
		  :descriptorSetCount layout-count
		  :pSetLayouts p-layouts))
      (check-vk-result (vkAllocateDescriptorSets device p-info p-set))
      (ptr->list p-set 'VkDescriptorSet layout-count))))

(defun allocate-memory (device allocation-size memory-type-index &key
								   (next +vk-null-ptr+)
								   (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkMemoryAllocateInfo))
			 (p-memory 'VkDeviceMemory))
    (setf (mem-ref p-info '(:struct VkMemoryAllocateInfo))
	  (list :sType +structure-type-memory-allocate-info+
		:pNext next
		:allocationSize allocation-size
		:memoryTypeIndex memory-type-index))
    (check-vk-result (vkAllocateMemory device p-info allocator p-memory))))

(defun free-command-buffers (device pool cmds)
  (let ((count (length cmds)))
    (with-foreign-object (p-cmd 'VkCommandBuffer count)
      (dotimes (i count)
	(setf (mem-ref p-cmd 'VkCommandBuffer) (nth i cmds)))
      (vkFreeCommandBuffers device pool count p-cmd))))

(defun free-descriptor-sets (device pool sets)
  (let ((count (length sets)))
    (with-foreign-object (p-set 'VkDescriptorSet count)
      (dotimes (i count)
	(setf (mem-ref p-set 'VkDescriptorSet) (nth i sets)))
      (check-vk-result (vkFreeDescriptorSets device pool count p-set)))))

(defun free-memory (device memory &optional (allocator +vk-null-ptr+))
  (vkFreeMemory device memory allocator))
