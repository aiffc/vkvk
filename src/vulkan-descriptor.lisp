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

(defun create-descriptor-pool (device &key
					(next +vk-null-ptr+)
					(flags 0)
					(max-sets 0)
					(pool-size-ptr nil)
					(allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkDescriptorPoolCreateInfo))
			 (p-pool 'VkDescriptorPool))
    (let* ((count (length pool-size-ptr))
	   (p-pool-size (set-null-ptr pool-size-ptr (foreign-alloc 'VkDescriptorPoolSize
								   :count count
								   :initial-contents pool-size-ptr))))
      (setf (mem-ref p-info '(:struct VkDescriptorPoolCreateInfo))
	  (list :sType +structure-type-descriptor-pool-create-info+
		:pNext next
		:flags flags 
		:maxSets max-sets
		:poolSizeCount count
		:pPoolSizes p-pool-size))
      (check-vk-result (vkCreateDescriptorPool device p-info allocator p-pool))
      (foreign-free p-pool-size)
      (mem-ref p-pool 'VkDescriptorPool))))

(defun create-descriptor-set-layout (device &key
					      (next +vk-null-ptr+)
					      (flags 0)
					      (binding-ptr nil)
					      (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkDescriptorSetLayoutCreateInfo))
			 (p-layout 'VkDescriptorSetLayout))
    (let* ((binding-count (length binding-ptr))
	   (p-binding (set-null-ptr binding-ptr
				    (foreign-alloc 'descriptor-set-layout-binding :count binding-count
										 :initial-contents binding-ptr))))
      (setf (mem-ref p-info '(:struct VkDescriptorSetLayoutCreateInfo))
	    (list :sType +structure-type-descriptor-set-layout-create-info+
		  :pNext next
		  :flags flags
		  :bindingCount binding-count
		  :pBindings p-binding))
      (check-vk-result (vkCreateDescriptorSetLayout device p-info allocator p-layout))
      (free-descriptor-set-layout-binding p-binding)
      (mem-ref p-layout 'VkDescriptorSetLayout))))

(defun destroy-descriptor-pool (device pool &optional (allocator +vk-null-ptr+))
  (vkDestroyDescriptorPool device pool allocator))

(defun destroy-descriptor-set-layout (device layout &optional (allocator +vk-null-ptr+))
  (vkDestroyDescriptorSetLayout device layout allocator))

(defun update-descriptor-sets (device write-ptr copy-ptr)
  (let* ((write-count (length write-ptr))
	 (copy-count (length copy-ptr))
	 (p-write (foreign-alloc 'write-descriptor-set :count write-count :initial-contents write-ptr))
	 (p-copy (foreign-alloc 'copy-descriptor-set :count copy-count :initial-contents copy-ptr)))
    (vkUpdateDescriptorSets device write-count p-write copy-count p-copy)))
