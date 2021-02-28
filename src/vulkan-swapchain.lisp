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

(defun create-swapchain-khr (device surface &key
					      (next +vk-null-ptr+)
					      (flags 0)
					      (min-image-count 1)
					      (format +FORMAT-R8G8B8A8-SINT+)
					      (color-space +COLOR-SPACE-SRGB-NONLINEAR-KHR+)
					      (image-width 600)
					      (image-height 600)
					      (image-array-layers 0)
					      (usage 0)
					      (sharing-mode +SHARING-MODE-EXCLUSIVE+)
					      (indices nil)
					      (pretransform +SURFACE-TRANSFORM-IDENTITY-BIT-KHR+)
					      (composite-alpha +COMPOSITE-ALPHA-OPAQUE-BIT-KHR+)
					      (present-mode +PRESENT-MODE-FIFO-KHR+)
					      (clipped VK_FALSE)
					      (old-swapchain +vk-null-ptr+)
					      (allocator +vk-null-ptr+))
  (let ((indices-count (length indices)))
    (with-foreign-objects ((p-info '(:struct VkSwapchainCreateInfoKHR))
			   (p-swapchain 'VkSwapchainKHR)
			   (p-indices :uint32 indices-count))
      (dotimes (i indices-count)
	(setf (mem-aref p-indices :int32) (nth i indices)))
      (setf (mem-ref p-info '(:struct VkSwapchainCreateInfoKHR))
	    (list :sType +structure-type-swapchain-create-info-khr+
		  :pNext next 
		  :flags flags
		  :surface surface
		  :minImageCount min-image-count
		  :imageFormat format 
		  :imageColorSpace color-space
		  :imageArrayLayers image-array-layers
		  :imageUsage usage
		  :imageSharingMode sharing-mode
		  :queueFamilyIndexCount indices-count
		  :pQueueFamilyIndices (set-null-ptr indices p-indices)
		  :preTransform pretransform
		  :compositeAlpha composite-alpha
		  :presentMode present-mode
		  :clipped clipped
		  :oldSwapchain old-swapchain))
      (setf (mem-aref (foreign-slot-pointer p-info '(:struct VkSwapchainCreateInfoKHR) :imageExtent)
		      '(:struct VkExtent2D))
	    (list :width image-width :height image-height))
      (check-vk-result (vkCreateSwapchainKHR device p-info allocator p-swapchain))
      (mem-ref p-swapchain 'VkSwapchainKHR))))

(defun destroy-swapchain-khr (device swapchain &optional (allocator +vk-null-ptr+))
  (vkDestroySwapchainKHR device swapchain allocator))

(defun get-swapchain-images-khr (device swapchain)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetSwapchainImagesKHR device swapchain p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-images 'VkImage count)
	  (check-vk-result (vkGetSwapchainImagesKHR device swapchain p-count p-images))
	  (ptr->list p-images 'VkImage count))))))

(defun acquire-next-image-khr (device swapchain time-count semaphore fence)
  (with-foreign-object (p-index :uint32)
    (check-vk-result (vkAcquireNextImageKHR device swapchain time-count semaphore fence p-index))
    (mem-ref p-index :uint32)))

(defun get-display-mode-properties-khr (gpu display)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetDisplayModePropertiesKHR gpu display p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-info '(:struct VkDisplayModePropertiesKHR) count)
	  (check-vk-result (vkGetDisplayModePropertiesKHR gpu display p-count p-info))
	  (ptr->list p-info '(:struct VkDisplayModePropertiesKHR) count))))))

(defun get-display-plane-capabilities-khr (gpu mode)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetDisplayPlaneCapabilitiesKHR gpu mode p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-info '(:struct VkDisplayPlaneCapabilitiesKHR) count)
	  (check-vk-result (vkGetDisplayPlaneCapabilitiesKHR gpu mode p-count p-info))
	  (ptr->list p-info '(:struct VkDisplayPlaneCapabilitiesKHR) count))))))

(defun get-display-plane-supported-displays-khr (gpu plane-index)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetDisplayPlaneSupportedDisplaysKHR gpu plane-index p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-info 'VkDisplayKHR count)
	  (check-vk-result (vkGetDisplayPlaneSupportedDisplaysKHR gpu plane-index p-count p-info))
	  (ptr->list p-info 'VkDisplayKHR count))))))

(defun create-display-mode-khr (gpu display &key
					      (next +vk-null-ptr+)
					      (flags 0)
					      (region-width 600)
					      (region-height 600)
					      (refresh-rate 0)
					      (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkDisplayModeCreateInfoKHR))
			(p-mode 'VkDisplayModeKHR))
    (let* ((p-parameter (foreign-slot-pointer p-info '(:struct VkDisplayModeCreateInfoKHR) :parameters))
	   (p-region (foreign-slot-pointer p-parameter '(:struct VkDisplayModeParametersKHR) :visibleRegion)))
      (setf (mem-ref p-region '(:struct VkExtent2D))
	    (list :width region-width :height region-height)
	    (mem-ref p-parameter '(:struct VkDisplayModeParametersKHR))
	    (list :refreshRate refresh-rate)
	    (mem-ref p-info '(:struct VkDisplayModeCreateInfoKHR))
	    (list :sType +structure-type-display-mode-create-info-khr+
		  :pNext next
		  :flags flags))
      (check-vk-result (vkCreateDisplayModeKHR gpu display p-info allocator p-mode))
      (mem-ref p-mode 'VkDisplayModeKHR))))

(defun create-shader-swapchain-khr (device swapchain-ptr &key (allocator +vk-null-ptr+))
  (let* ((swapchain-count (length swapchain-ptr))
	 (p-info (foreign-alloc 'swapchain-create-info :count swapchain-count
						       :initial-contents swapchain-ptr)))
    (with-foreign-object (p-swapchain 'VkSwapchainKHR swapchain-count)
      (check-vk-result (vkCreateSharedSwapchainsKHR device swapchain-count p-info allocator p-swapchain))
      (ptr->list p-swapchain 'VkSwapchainKhr swapchain-count))))
