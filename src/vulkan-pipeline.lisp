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

(defun create-shader-module (device path &key
					   (next +vk-null-ptr+)
					   (flags 0)
					   (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkShaderModuleCreateInfo))
			 (p-module 'VkShaderModule))
    (multiple-value-bind (p-code code-size) (read-shader-file path)
      (setf (mem-ref p-info '(:struct VkShaderModuleCreateInfo))
		(list :sType +structure-type-shader-module-create-info+
		      :pNext next
		      :flags flags
		      :codeSize code-size
		      :pCode p-code))
      (check-vk-result (vkCreateShaderModule device p-info allocator p-module))
      (foreign-free p-code)
      (mem-ref p-module 'VkShaderModule))))

(defun destroy-shader-module (device module &optional (allocator +vk-null-ptr+))
  (vkDestroyShaderModule device module allocator))

(defun create-pipeline-layout (device &key
					(next +vk-null-ptr+)
					(flags 0)
					(layouts nil)
					(constant-range-ptr nil)
					(allocator +vk-null-ptr+))
  (let* ((layout-count (length layouts))
	 (p-set-layouts (set-null-ptr layouts (foreign-alloc 'VkDescriptorSetLayout :count layout-count :initial-contents layouts)))
	 (constant-range-count (length constant-range-ptr))
	 (p-constant-range (set-null-ptr constant-range-ptr (foreign-alloc 'push-constant-range
									   :count constant-range-count
									   :initial-contents constant-range-ptr))))
    (with-foreign-objects ((p-info '(:struct VkPipelineLayoutCreateInfo))
			   (p-layout 'VkPipelineLayout))
      (setf (mem-ref p-info '(:struct VkPipelineLayoutCreateInfo))
	    (list :sType +structure-type-pipeline-layout-create-info+
		  :pNext next
		  :flags flags
		  :setLayoutCount layout-count
		  :pSetLayouts p-set-layouts
		  :pushConstantRangeCount constant-range-count
		  :pPushConstantRanges p-constant-range))
      (check-vk-result (vkCreatePipelineLayout device p-info allocator p-layout))
      (free-if-not-null p-set-layouts)
      (free-if-not-null p-constant-range)
      (mem-ref p-layout 'VkPipelineLayout))))

(defun destroy-pipeline-layout (device layout &optional (allocator +vk-null-ptr+))
  (vkDestroyPipelineLayout device layout allocator))

(defun create-render-pass (device &key
				    (next +vk-null-ptr+)
				    (flags 0)
				    (attachment-ptr +vk-null-ptr+)
				    (subpass-ptr +vk-null-ptr+)
				    (subpass-dependency-ptr +vk-null-ptr+)
				    (allocator +vk-null-ptr+))
  (let* ((attachment-count (length attachment-ptr))
	 (subpass-count (length subpass-ptr))
	 (subpass-dependency-count (length subpass-dependency-ptr))
	 (p-attachment (set-null-ptr attachment-ptr (foreign-alloc 'attachment-description
								   :count attachment-count
								   :initial-contents attachment-ptr)))
	 (p-subpass (set-null-ptr subpass-ptr (foreign-alloc 'subpass-description
							     :count subpass-count
							     :initial-contents subpass-ptr)))
	 (p-subpass-dependency (set-null-ptr subpass-dependency-ptr (foreign-alloc 'subpass-dependency-description
										   :count subpass-dependency-count
										   :initial-contents subpass-dependency-ptr))))
    (with-foreign-objects ((p-info '(:struct VkRenderPassCreateInfo))
			   (p-render-pass 'VkRenderPass))
      (setf (mem-ref p-info '(:struct VkRenderPassCreateInfo))
	    (list :sType +structure-type-render-pass-create-info+
		  :pNext next
		  :flags flags
		  :attachmentCount attachment-count
		  :pAttachments p-attachment
		  :subpassCount subpass-count
		  :pSubpasses p-subpass
		  :dependencyCount subpass-dependency-count
		  :pDependencies p-subpass-dependency))
      (check-vk-result (vkCreateRenderPass device p-info allocator p-render-pass))
      (free-if-not-null p-attachment)
      (free-subpass-description p-subpass)
      (free-if-not-null p-subpass-dependency)
      (mem-ref p-render-pass 'VkRenderPass))))

(defun destroy-render-pass (device render-pass &optional (allocator +vk-null-ptr+))
  (vkDestroyRenderPass device render-pass allocator))

(defun create-graphics-pipeline (device cache pipeline-ptr &key (allocator +vk-null-ptr+))
  (let* ((pipeline-count (length pipeline-ptr))
	 (p-pipeline-info (foreign-alloc 'graphics-pipeline :count pipeline-count
							    :initial-contents pipeline-ptr)))
    (with-foreign-object (p-pipeline 'VkPipeline pipeline-count)
      (check-vk-result (vkCreateGraphicsPipelines device cache pipeline-count p-pipeline-info allocator p-pipeline))
      (mapc #'free-graphics-pipeline pipeline-ptr)
      (ptr->list p-pipeline 'VkPipeline pipeline-count))))

(defun create-compute-pipeline (device cache pipeline-ptr &key (allocator +vk-null-ptr+))
  (let* ((pipeline-count (length pipeline-ptr))
	 (p-pipeline-info (foreign-alloc 'compute-pipeline :count pipeline-count
							   :initial-contents pipeline-ptr)))
    (with-foreign-object (p-pipeline 'VkPipeline pipeline-count)
      (check-vk-result (vkCreateComputePipelines device cache pipeline-count p-pipeline-info allocator p-pipeline))
      (mapc #'free-compute-pipeline pipeline-ptr)
      (ptr->list p-pipeline 'VkPipeline pipeline-count))))

(defun destroy-pipeline (device pipeline &optional (allocator +vk-null-ptr+))
  (vkDestroyPipeline device pipeline allocator))

(defun get-render-area-granularity (device render-pass)
  (with-foreign-object (p-granularity '(:struct VkExtent2D))
    (vkGetRenderAreaGranularity device render-pass p-granularity)
    (mem-ref p-granularity '(:struct VkExtent2D))))

(defun create-pipeline-cache (device &key
				       (next +vk-null-ptr+)
				       (flags 0)
				       (data-size 0)
				       (init-data +vk-null-ptr+)
				       (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkPipelineCacheCreateInfo))
			 (p-cache 'VkPipelineCache))
    (setf (mem-ref p-info '(:struct VkPipelineCacheCreateInfo))
	  (list :sType +structure-type-pipeline-cache-create-info+
		:pNext next
		:flags flags
		:initialDataSize data-size
		:pInitialData init-data))
    (check-vk-result (vkCreatePipelineCache device p-info allocator p-cache))
    (mem-ref p-cache 'VkPipelineCache)))

(defun destroy-pipeline-cache (device cache &optional (allocator +vk-null-ptr+))
  (vkDestroyPipelineCache device cache allocator))

(defun get-pipeline-cache-data (device cache)
  (with-foreign-object (p-size 'size-t)
    (check-vk-result (vkGetPipelineCacheData device cache p-size +vk-null-ptr+))
    (let ((size (mem-ref p-size 'size-t)))
      (unless (zerop size)
	(with-foreign-object (p-data :char size)    ;;may wrong
	  (check-vk-result (vkGetPipelineCacheData device cache p-size p-data))
	  (foreign-string-to-lisp p-data))))))

(defun merge-pipeline-caches (device dst-cache src-cache)
  (let* ((count (length src-cache))
	 (p-src-cache (foreign-alloc 'VkPipelinecache :count count
						      :initial-contents src-cache)))
    (check-vk-result (vkMergePipelineCaches device dst-cache count p-src-cache))
    (foreign-free p-src-cache)))
