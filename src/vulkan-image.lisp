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

(defun create-image-view (device image &key
					 (next +vk-null-ptr+)
					 (flags 0)
					 (view-type +image-view-type-2d+)
					 (format +format-r8g8b8a8-srgb+)
					 (r +component-swizzle-identity+)
					 (g +component-swizzle-identity+)
					 (b +component-swizzle-identity+)
					 (a +component-swizzle-identity+)
					 (aspect-mask +image-aspect-color-bit+)
					 (mip-level 0)
					 (level-count 1)
					 (array-layer 0)
					 (layer-count 1)
					 (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkImageViewCreateInfo))
			 (p-image-view 'VkImageView))
    (setf (mem-ref p-info '(:struct VkImageViewCreateInfo))
	  (list :sType +structure-type-image-view-create-info+
		:pNext next
		:flags flags
		:image image 
		:viewType view-type
		:format format)
	  (mem-ref (foreign-slot-pointer p-info '(:struct VkImageViewCreateInfo) :components)
		   '(:struct VkComponentMapping))
	  (list :r r
		:g g
		:b b
		:a a)
	  (mem-ref (foreign-slot-pointer p-info '(:struct VkImageViewCreateInfo) :subresourceRange)
		   '(:struct VkImageSubresourceRange))
	  (list :aspectMask aspect-mask
		:baseMipLevel mip-level
		:levelCount level-count
		:baseArrayLayer array-layer
		:layerCount layer-count))
    (check-vk-result (vkCreateImageView device p-info allocator p-image-view))
    (mem-ref p-image-view 'VkImageView)))

(defun destroy-image-view (device image-view &optional (allocator +vk-null-ptr+))
  (vkDestroyImageView device image-view allocator))

(defun get-image-memory-requirements (device image)
  (with-foreign-object (p-info '(:struct VkMemoryRequirements))
    (vkGetImageMemoryRequirements device image p-info)
    (mem-ref p-info '(:struct VkMemoryRequirements))))

(defun get-image-sparse-memory-requirements (device image)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetImageSparseMemoryRequirements device image p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-info '(:struct VkSparseImageMemoryRequirements) count)
	  (check-vk-result (vkGetImageSparseMemoryRequirements device image p-count p-info))
	  (ptr->list p-info '(:struct VkSparseImageMemoryRequirements) count))))))

(defun get-image-subresource-layout (device image aspect-mask mip-level array-layer)
  (with-foreign-objects ((p-subresource '(:struct VkImageSubresource))
			 (p-layout '(:struct VkSubresourceLayout)))
    (setf (mem-ref p-subresource '(:struct VkImageSubresource))
	  (list :aspectMask aspect-mask
		:mipLevel mip-level
		:arrayLayer array-layer))
    (vkGetImageSubresourceLayout device image p-subresource p-layout)
    (mem-ref p-layout '(:struct VkImageSubresource))))

(defun create-image (device &key
			      (next +vk-null-ptr+)
			      (flags 0)
			      (image-type +image-type-1d+)
			      (format +format-r8g8b8a8-srgb+)
			      (extent-width 600)
			      (extent-height 600)
			      (extent-depth 1)
			      (mip-level 0)
			      (array-layers 0)
			      (samples +sample-count-1-bit+)
			      (tiling +image-tiling-linear+)
			      (usage +image-usage-color-attachment-bit+)
			      (sharing-mode +sharing-mode-concurrent+)
			      (queue-indices nil)
			      (init-layout +image-layout-color-attachment-optimal+)
			      (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkImageCreateInfo))
			 (p-image 'VkImage))
    (let* ((indices-count (length queue-indices))
	   (p-indices (foreign-alloc :uint32 :count indices-count :initial-contents queue-indices)))
      (setf (mem-ref (foreign-slot-pointer p-info '(:struct VkImageCreateInfo) :extent) '(:struct VkExtent3D))
	    (list :width extent-width :height extent-height :depth extent-depth)
	    (mem-ref p-info '(:struct VkImageCreateInfo))
	    (list :sType +structure-type-image-create-info+
		  :pNext next
		  :flags flags
		  :imageType image-type
		  :format format
		  :mipLevels mip-level
		  :arrayLayers array-layers 
		  :samples samples
		  :tiling tiling
		  :usage usage
		  :sharingMode sharing-mode
		  :queueFamilyIndexCount indices-count
		  :pQueueFamilyIndices p-indices
		  :initialLayout init-layout))
      (check-vk-result (vkCreateImage device p-info allocator p-image))
      (foreign-free p-indices)
      (mem-ref p-image 'VkImage))))

(defun destroy-image (device image &optional (allocator +vk-null-ptr+))
  (vkDestroyImage device image allocator))
