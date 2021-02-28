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

(defun enumerate-physical-devices (instance)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkEnumeratePhysicalDevices instance p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-physical-devices 'VkPhysicalDevice count)
	  (check-vk-result (vkEnumeratePhysicalDevices instance p-count p-physical-devices))
	  (ptr->list p-physical-devices 'VkPhysicalDevice count))))))

(defun enumerate-physical-device-groups-khx (instance)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkEnumeratePhysicalDeviceGroupsKHX instance instance p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-groups '(:struct VkPhysicalDeviceGroupPropertiesKHX) count)
	  (check-vk-result (vkEnumeratePhysicalDeviceGroupsKHX instance instance p-count p-groups))
	  (ptr->list p-groups '(:struct VkPhysicalDeviceGroupPropertiesKHX) count))))))

(defun get-physical-device-display-plane-properties-khr (gpu)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetPhysicalDeviceDisplayPlanePropertiesKHR gpu p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-properties '(:struct VkDisplayPlanePropertiesKHR) count)
	  (check-vk-result (vkGetPhysicalDeviceDisplayPlanePropertiesKHR gpu p-count p-properties))
	  (ptr->list p-properties '(:struct VkDisplayPlanePropertiesKHR) count))))))

(defun get-physical-device-display-properties-khr (gpu)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetPhysicalDeviceDisplayPropertiesKHR gpu p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-properties '(:struct VkDisplayPropertiesKHR) count)
	  (check-vk-result (vkGetPhysicalDeviceDisplayPropertiesKHR gpu p-count p-properties))
	  (ptr->list p-properties '(:struct VkDisplayPropertiesKHR) count))))))

(defun get-physical-device-features (gpu)
  (with-foreign-object (p-features '(:struct VkPhysicalDeviceFeatures))
    (vkGetPhysicalDeviceFeatures gpu p-features)
    (mem-ref p-features '(:struct VkPhysicalDeviceFeatures))))

(defun get-physical-device-format-properties (gpu &key (format +format-undefined+))
  (with-foreign-object (p-properties '(:struct VkFormatProperties))
    (vkGetPhysicalDeviceFormatProperties gpu format '(:struct VkFormatProperties))
    (mem-ref p-properties '(:struct VkFormatProperties))))

(defun get-physical-device-image-format-properties (gpu &key
							  (format +FORMAT-UNDEFINED+)
							  (type +IMAGE-TYPE-2D+)
							  (tiling +IMAGE-TILING-LINEAR+)
							  (usage 0)
							  (flags 0))
  (with-foreign-object (p-properties '(:struct VkImageFormatProperties))
    (check-vk-result (vkGetPhysicalDeviceImageFormatProperties gpu format type tiling usage flags p-properties))
    (mem-ref p-properties '(:struct VkImageFormatProperties))))

(defun get-physical-device-memory-properties (gpu)
  (with-foreign-object (p-properties 'physical-device-memory-properties)
    (vkGetPhysicalDeviceMemoryProperties gpu p-properties)
    (mem-ref p-properties 'physical-device-memory-properties)))

(defun get-physical-device-properties (gpu)
  (with-foreign-object (p-properties '(:struct VkPhysicalDeviceProperties))
    (vkGetPhysicalDeviceProperties gpu p-properties)
    (mem-ref p-properties '(:struct VkPhysicalDeviceProperties))))

(defun get-physical-device-queue-family-properties (gpu)
  (with-foreign-object (p-count :uint32)
    (vkGetPhysicalDeviceQueueFamilyProperties gpu p-count +vk-null-ptr+)
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-properties '(:struct VkQueueFamilyProperties) count)
	  (vkGetPhysicalDeviceQueueFamilyProperties gpu p-count p-properties)
	  (ptr->list p-properties '(:struct VkQueueFamilyProperties) count))))))

(defun get-physical-device-sparse-image-format-properties (gpu &key
								 (format +FORMAT-UNDEFINED+)
								 (type +IMAGE-TYPE-2D+)
								 (sample +sample-count-1-bit+)
								 (usage 0)
								 (tiling +IMAGE-TILING-LINEAR+))
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetPhysicalDeviceSparseImageFormatProperties gpu format type sample usage tiling p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-properties '(:struct VkSparseImageFormatProperties) count)
	  (check-vk-result (vkGetPhysicalDeviceSparseImageFormatProperties gpu format type sample usage tiling p-count p-properties))
	  (ptr->list p-properties '(:struct VkSparseImageFormatProperties) count))))))

(defun get-physical-device-surface-capabilities-khr (gpu surface)
  (with-foreign-object (p-cap '(:struct VkSurfaceCapabilitiesKHR))
    (check-vk-result (vkGetPhysicalDeviceSurfaceCapabilitiesKHR gpu surface p-cap))
    (mem-ref p-cap '(:struct VkSurfaceCapabilitiesKHR))))

(defun get-physical-device-surface-present-mode-khr (gpu surface)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetPhysicalDeviceSurfacePresentModesKHR gpu surface p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-mode 'VkPresentModeKHR count)
	  (check-vk-result (vkGetPhysicalDeviceSurfacePresentModesKHR gpu surface p-count p-mode))
	  (ptr->list p-mode 'VkPresentModeKHR count))))))

(defun get-physical-device-surface-formats-khr (gpu surface)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetPhysicalDeviceSurfaceFormatsKHR gpu surface p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-formats '(:struct VkSurfaceFormatKHR) count)
	  (check-vk-result (vkGetPhysicalDeviceSurfaceFormatsKHR gpu surface p-count p-formats))
	  (ptr->list p-formats '(:struct VkSurfaceFormatKHR) count))))))

(defun get-device-group-surface-present-mode-khr (gpu surface)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetDeviceGroupSurfacePresentModesKHX gpu surface p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-mode 'VkDeviceGroupPresentModeFlagsKHX count)
	  (check-vk-result (vkGetDeviceGroupSurfacePresentModesKHX gpu surface p-count p-mode))
	  (ptr->list p-mode 'VkDeviceGroupPresentModeFlagsKHX count))))))

(defun get-physical-device-surface-support-khr (gpu index surface)
  (with-foreign-object (p-support 'VkBool32)
    (check-vk-result (vkGetPhysicalDeviceSurfaceSupportKHR gpu index surface p-support))
    (if (zerop (mem-ref p-support 'VkBool32)) nil t)))

(defun get-physical-device-win32-presentation-support-khr (gpu index)
  (if (zerop (vkGetPhysicalDeviceWin32PresentationSupportKHR gpu index)) nil t))

(defun get-physical-device-external-image-format-properties-nv (instance gpu &key
										(format +FORMAT-UNDEFINED+)
										(type +IMAGE-TYPE-2D+)
										(tiling +IMAGE-TILING-LINEAR+)
										(usage 0)
										(flags 0)
										(external-handle 0))
  (with-foreign-objects ((p-fun :pointer)
			 (p-properties '(:struct VkExternalImageFormatPropertiesNV)))
    (setf (mem-ref p-fun :pointer)
	  (vkGetInstanceProcAddr instance "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"))
    (unless (null-pointer-p p-fun)
      (check-vk-result (foreign-funcall-pointer (mem-ref p-fun :pointer) ()
						VkPhysicalDevice gpu
						VkFormat format
						VkImageType type
						VkImageTiling tiling
						VkImageUsageFlags usage
						VkImageCreateFlags flags
						VkExternalMemoryHandleTypeFlagsNV external-handle
						(:pointer (:struct VkExternalImageFormatPropertiesNV)) p-properties
						VkResult))
      (mem-ref p-properties '(:struct VkExternalImageFormatPropertiesNV)))))

(defun get-physical-device-features2-khr (instance gpu)
  (with-foreign-object (p-properties '(:struct VkPhysicalDeviceFeatures2KHR))
    (vkGetPhysicalDeviceFeatures2KHR instance gpu p-properties)
    (mem-ref p-properties '(:struct VkPhysicalDeviceFeatures2KHR))))

(defun get-physical-device-format-properties2-khr (instance gpu &key (format +FORMAT-UNDEFINED+))
  (with-foreign-object (p-properties '(:struct VkFormatProperties2KHR))
    (vkGetPhysicalDeviceFormatProperties2KHR instance gpu format p-properties)
    (mem-ref p-properties '(:struct VkFormatProperties2KHR))))

(defun get-physical-device-memory-properties2-khr (instance gpu)
  (with-foreign-object (p-properties '(:struct VkPhysicalDeviceMemoryProperties2KHR))
    (vkGetPhysicalDeviceMemoryProperties2KHR instance gpu p-properties)
    (mem-ref p-properties '(:struct VkPhysicalDeviceMemoryProperties2KHR))))

(defun get-physical-device-mir-presentation-support-khr (instance gpu index)
  (with-foreign-object (p-mir-connection :pointer)
    (vkGetPhysicalDeviceMirPresentationSupportKHR instance gpu index p-mir-connection)
    p-mir-connection))

(defun get-physical-device-present-rectangles-khx (instance gpu surface)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetPhysicalDevicePresentRectanglesKHX instance gpu surface p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-rects '(:struct VkRect2D) count)
	  (check-vk-result (vkGetPhysicalDevicePresentRectanglesKHX instance gpu surface p-count p-rects))
	  (ptr->list p-rects '(:struct VkRect2D) count))))))

(defun get-physical-device-properties2-khr (instance gpu)
  (with-foreign-object (p-properties '(:struct VkPhysicalDeviceProperties2KHR))
    (vkGetPhysicalDeviceProperties2KHR instance gpu p-properties)
    (mem-ref p-properties '(:struct VkPhysicalDeviceProperties2KHR))))

(defun get-physical-device-queue-family-properties2-khr (instance gpu)
  (with-foreign-object (p-count :uint32)
    (vkGetPhysicalDeviceQueueFamilyProperties2KHR instance gpu p-count +vk-null-ptr+)
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-properties '(:struct VkQueueFamilyProperties2KHR) count)
	  (vkGetPhysicalDeviceQueueFamilyProperties2KHR instance gpu p-count p-properties)
	  (ptr->list p-properties '(:struct VkQueueFamilyProperties2KHR) count))))))

(defun get-physical-device-surface-capabilities2-ext (instance gpu surface)
  (with-foreign-object (p-cap '(:struct VkSurfaceCapabilities2EXT))
    (check-vk-result (vkGetPhysicalDeviceSurfaceCapabilities2EXT instance gpu surface p-cap))
    (mem-ref p-cap '(:struct VkSurfaceCapabilities2EXT))))

(defun get-physical-device-external-buffer-properties-khx (instance gpu &key
									  (next +vk-null-ptr+)
									  (create-flags 0)
									  (usage-flags 0)
									  (handle-type +EXTERNAL-MEMORY-HANDLE-TYPE-OPAQUE-FD-BIT-KHX+))
  (with-foreign-objects ((p-info '(:struct VkPhysicalDeviceExternalBufferInfoKHX))
			 (p-properties '(:struct VkExternalBufferPropertiesKHX)))
    (setf (mem-ref p-info '(:struct VkPhysicalDeviceExternalBufferInfoKHX))
	  (list :sType +STRUCTURE-TYPE-PHYSICAL-DEVICE-EXTERNAL-BUFFER-INFO-KHX+
		:pNext next
		:flags create-flags
		:usage usage-flags
		:handleType handle-type))
    (vkGetPhysicalDeviceExternalBufferPropertiesKHX instance gpu p-info p-properties)
    (mem-ref p-properties '(:struct VkExternalBufferPropertiesKHX))))

(defun get-physical-device-external-semaphore-properties-khx (instance gpu &key
									     (next +vk-null-ptr+)
									     (handle-type +EXTERNAL-SEMAPHORE-HANDLE-TYPE-OPAQUE-FD-BIT-KHX+))
  (with-foreign-objects ((p-info '(:struct VkPhysicalDeviceExternalSemaphoreInfoKHX))
			 (p-properties '(:struct VkExternalSemaphorePropertiesKHX)))
    (setf (mem-ref p-info '(:struct VkPhysicalDeviceExternalSemaphoreInfoKHX))
	  (list :sType +STRUCTURE-TYPE-PHYSICAL-DEVICE-EXTERNAL-SEMAPHORE-INFO-KHX+
		:pNext next
		:handleType handle-type))
    (vkGetPhysicalDeviceExternalSemaphorePropertiesKHX instance gpu p-info p-properties)
    (mem-ref p-properties '(:struct VkExternalSemaphorePropertiesKHX))))

(defun get-physical-device-generated-commands-properties-nvx (instance gpu &key
									     (next +vk-null-ptr+)
									     (support VK_FALSE))
  (with-foreign-objects ((p-info '(:struct VkDeviceGeneratedCommandsFeaturesNVX))
			 (p-limits '(:struct VkDeviceGeneratedCommandsLimitsNVX)))
    (setf (mem-ref p-info '(:struct VkDeviceGeneratedCommandsFeaturesNVX))
	  (list :sType +STRUCTURE-TYPE-DEVICE-GENERATED-COMMANDS-FEATURES-NVX+
		:pNext next 
		:computeBindingPointSupport support))
    (vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX instance gpu p-info p-limits)
    (mem-ref p-limits '(:struct VkDeviceGeneratedCommandsLimitsNVX))))

(defun get-physical-device-image-format-properties2-khr (instance gpu &key
									(next +vk-null-ptr+)
									(format +FORMAT-R8G8B8A8-UINT+)
									(type +IMAGE-TYPE-2D+)
									(tiling +IMAGE-TILING-OPTIMAL+)
									(usage 0)
									(flags 0))
  (with-foreign-objects ((p-info '(:struct VkPhysicalDeviceImageFormatInfo2KHR))
			 (p-properties '(:struct VkImageFormatProperties2KHR)))
    (setf (mem-ref p-info '(:struct VkPhysicalDeviceImageFormatInfo2KHR))
	  (list :sType +STRUCTURE-TYPE-PHYSICAL-DEVICE-IMAGE-FORMAT-INFO-2-KHR+
		:pNext next 
		:format format 
		:type type
		:tiling tiling
		:usage usage
		:flags flags))
    (vkGetPhysicalDeviceImageFormatProperties2KHR instance gpu p-info p-properties)
    (mem-ref p-properties '(:struct VkImageFormatProperties2KHR))))

(defun get-physical-device-surface-formats2-khr (instance gpu surface &key
									(next +vk-null-ptr+))
  (with-foreign-objects ((p-count :uint32)
			 (p-info '(:struct VkPhysicalDeviceSurfaceInfo2KHR)))
    (setf (mem-ref p-info '(:struct VkPhysicalDeviceSurfaceInfo2KHR))
	  (list :sType +STRUCTURE-TYPE-PHYSICAL-DEVICE-SURFACE-INFO-2-KHR+
		:pNext next
		:surface surface))
    (check-vk-result (vkGetPhysicalDeviceSurfaceFormats2KHR instance gpu p-info p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-formats '(:struct VkSurfaceFormat2KHR) count)
	  (check-vk-result (vkGetPhysicalDeviceSurfaceFormats2KHR instance gpu p-info p-count p-formats))
	  (ptr->list p-formats '(:struct VkSurfaceFormat2KHR) count))))))

(defun get-physical-device-sparse-image-format-properties2-khr (instance gpu &key
									       (next +vk-null-ptr+)
									       (format +FORMAT-R8G8B8A8-UINT+)
									       (type +IMAGE-TYPE-2D+)
									       (samples +SAMPLE-COUNT-1-BIT+)
									       (usage 0)
									       (tiling +IMAGE-TILING-OPTIMAL+))
  (with-foreign-objects ((p-count :uint32)
			 (p-info '(:struct VkPhysicalDeviceSparseImageFormatInfo2KHR)))
    (setf (mem-ref p-info '(:struct VkPhysicalDeviceSparseImageFormatInfo2KHR))
	  (list :sType +STRUCTURE-TYPE-PHYSICAL-DEVICE-SPARSE-IMAGE-FORMAT-INFO-2-KHR+
		:pNext next 
		:format format
		:type type 
		:samples samples
		:usage usage
		:tiling tiling))
    (vkGetPhysicalDeviceSparseImageFormatProperties2KHR instance gpu p-info p-count +vk-null-ptr+)
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-formats '(:struct VkSparseImageFormatProperties2KHR) count)
	  (vkGetPhysicalDeviceSparseImageFormatProperties2KHR instance gpu p-info p-count p-formats)
	  (ptr->list p-formats '(:struct VkSparseImageFormatProperties2KHR) count))))))

(defun get-physical-device-surface-capabilities2-khr (instance gpu surface &key (next +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkPhysicalDeviceSurfaceInfo2KHR))
			 (p-cap '(:struct VkSurfaceCapabilities2KHR)))
    (setf (mem-ref p-info '(:struct VkPhysicalDeviceSurfaceInfo2KHR))
	  (list :sType +STRUCTURE-TYPE-PHYSICAL-DEVICE-SURFACE-INFO-2-KHR+
		:pNext next
		:surface surface))
    (check-vk-result (vkGetPhysicalDeviceSurfaceCapabilities2KHR instance gpu p-info p-cap))
    (mem-ref p-cap '(:struct VkSurfaceCapabilities2KHR))))

(defun get-physical-device-xlib-presentation-support-khr (instance gpu index display id) ;;unknow
  (vkGetPhysicalDeviceXlibPresentationSupportKHR instance gpu index display id))

(defun acquire-xlib-display-ext (instance gpu dpy display)
  (check-vk-result (vkAcquireXlibDisplayEXT instance gpu dpy display)))
;;unsupported
;; (defvkinstextfun ("vkGetPhysicalDeviceWaylandPresentationSupportKHR" vkGetPhysicalDeviceWaylandPresentationSupportKHR) VkBool32
;;   (physicalDevice VkPhysicalDevice)
;;   (queueFamilyIndex :uint32)
;;   (display (:pointer (:struct wl-display))))
