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

(defun enumerate-device-extension-properties (gpu layer-name)
  (let ((p-layer-name (if (null layer-name) +vk-null-ptr+ (foreign-string-alloc layer-name))))
    (with-foreign-object (p-count :uint32)
      (check-vk-result (vkEnumerateDeviceExtensionProperties gpu p-layer-name p-count +vk-null-ptr+))
      (let ((count (mem-ref p-count :uint32)))
	(unless (zerop count)
	  (with-foreign-object (p-properties '(:struct VkExtensionProperties) count)
	    (check-vk-result (vkEnumerateDeviceExtensionProperties gpu p-layer-name p-count p-properties))
	    (unless (null layer-name)
	      (foreign-string-free p-layer-name))
	    (ptr->list p-properties 'extension-properties count)))))))

(defun enumerate-device-layer-properties (gpu)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkEnumerateDeviceLayerProperties gpu p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-properties 'layer-properties count)
	  (check-vk-result (vkEnumerateDeviceLayerProperties gpu p-count p-properties))
	  (ptr->list p-properties 'layer-properties count))))))

(defun available-device-extensions (gpu)
  (remove-duplicates (append (mapcar #'(lambda (property) (getf property :extensionName))
				     (apply #'append (mapcar #'(lambda (layer)
								 (enumerate-device-extension-properties gpu layer))
							     (available-device-layers gpu))))
			     (mapcar #'(lambda (property) (getf property :extensionName))
				     (enumerate-device-extension-properties gpu nil)))
		     :test #'string=))

(defun available-device-layers (gpu)
  (remove-duplicates (mapcar #'(lambda (property) (getf property :layerName))
			     (enumerate-device-layer-properties gpu))
		     :test #'string=))

(defun create-device (gpu &key
			    (next +vk-null-ptr+)
			    (flags 0)
			    (queue-create-ptr nil) ;;list of device-queue-create-ptr
			    (layer-names nil)
			    (extension-names nil)
			    (features nil)
			    (allocator +vk-null-ptr+))
  (when *vk-debug*
    (pushnew "VK_LAYER_KHRONOS_validation" layer-names))
  (let* ((queue-count (length queue-create-ptr))
	 (p-device-queue-create-info (set-null-ptr queue-create-ptr
						   (foreign-alloc 'device-queue-create-info :count queue-count
											    :initial-contents queue-create-ptr))))
    (let* ((usable-extensions (intersection (available-device-extensions gpu) extension-names :test #'string=))
	   (usable-layers (intersection (available-device-layers gpu) layer-names :test #'string=))
	   (ext-count (length usable-extensions))
	   (lay-count (length usable-layers)))
      (with-foreign-objects ((p-features '(:struct VkPhysicalDeviceFeatures))
			     (p-exts '(:pointer :char) ext-count)
			     (p-lays '(:pointer :char) lay-count)
			     (p-info '(:struct VkDeviceCreateInfo))
			     (p-device 'VkDevice))
	;;set features
	(zero-struct p-features '(:struct VkPhysicalDeviceFeatures))
	(unless (null features)
	  (setf (mem-ref p-features '(:struct VkPhysicalDeviceFeatures)) features))
	;;set extensions
	(when extension-names
	  (dotimes (i ext-count)
	    (setf (mem-aref p-exts '(:pointer :char) i)
		  (foreign-string-alloc (nth i usable-extensions)))))
	;;set layers
	(when layer-names
	  (dotimes (i lay-count)
	    (setf (mem-aref p-lays '(:pointer :char) i)
		  (foreign-string-alloc (nth i usable-layers)))))
	
	(setf (mem-ref p-info '(:struct VkDeviceCreateInfo))
	      (list :sType +STRUCTURE-TYPE-DEVICE-CREATE-INFO+
		    :pNext next
		    :flags flags
		    :queueCreateInfoCount queue-count
		    :pQueueCreateInfos p-device-queue-create-info
		    :enabledLayerCount lay-count
		    :ppEnabledLayerNames (set-null-ptr layer-names p-lays)
		    :enabledExtensionCount ext-count
		    :ppEnabledExtensionNames (set-null-ptr extension-names p-exts)
		    :pEnabledFeatures p-features))
	(check-vk-result (vkCreateDevice gpu p-info allocator p-device))
	(free-device-queue-ptr p-device-queue-create-info queue-count)
	(mem-ref p-device 'VkDevice)))))

(defun destroy-device (device &optional (allocator +vk-null-ptr+))
  (vkDestroyDevice device allocator))

(defun get-device-queue (device queue-family-index queue-index)
  (with-foreign-object (p-queue 'VkQueue)
    (vkGetDeviceQueue device queue-family-index queue-index p-queue)
    (mem-ref p-queue 'VkQueue)))

(defun device-wait-idle (device)
  (vkDeviceWaitIdle device))

(defun queue-submit (queue queue-count fence &key
					       (next +vk-null-ptr+)
					       (wait-semaphores nil)       
					       (wait-dst-stage-mask nil)   
					       (cmds nil)
					       (single-semaphore nil))
  (let ((wait-semaphore-count (length wait-semaphores))
	(wait-dst-stage-mask-count (length wait-dst-stage-mask))
	(cmd-count (length cmds))
	(single-semaphore-count (length single-semaphore)))
    (with-foreign-objects ((p-wait-semaphore 'VkSemaphore wait-semaphore-count)
			   (p-wait-dst-stage-mask 'VkPipelineStageFlags wait-dst-stage-mask-count)
			   (p-cmd 'VkCommandBuffer cmd-count)
			   (p-single-semaphore 'VkSemaphore single-semaphore-count)
			   (p-info '(:struct VkSubmitInfo)))
      (when wait-semaphores
	  (dotimes (i wait-semaphore-count)
	    (setf (mem-aref p-wait-semaphore 'VkSemaphore i) (nth i wait-semaphores))))
      (when wait-dst-stage-mask
	  (dotimes (i wait-dst-stage-mask-count)
	    (setf (mem-aref p-wait-dst-stage-mask 'VkPipelineStageFlags i) (nth i wait-dst-stage-mask))))
      (when cmds
	  (dotimes (i cmd-count)
	    (setf (mem-aref p-cmd 'VkCommandBuffer i) (nth i cmds))))
      (when single-semaphore
	  (dotimes (i single-semaphore-count)
	    (setf (mem-aref p-single-semaphore 'VkSemaphore i) (nth i single-semaphore))))
      (setf (mem-ref p-info '(:struct VkSubmitInfo))
	    (list :sType +structure-type-submit-info+
		  :pNext next
		  :waitSemaphoreCount wait-semaphore-count
		  :pWaitSemaphores (set-null-ptr wait-semaphores p-wait-semaphore)
		  :pWaitDstStageMask (set-null-ptr wait-dst-stage-mask p-wait-dst-stage-mask)
		  :commandBufferCount cmd-count
		  :pCommandBuffers (set-null-ptr cmds p-cmd)
		  :signalSemaphoreCount single-semaphore-count
		  :pSignalSemaphores (set-null-ptr single-semaphore p-single-semaphore)))
      (check-vk-result (vkQueueSubmit queue queue-count p-info fence)))))

(defun queue-present-khr (present-queue &key
					  (next +vk-null-ptr+)
					  (wait-semaphores nil)
					  (swapchains nil)
					  (image-indices nil)
					  (results nil))
  (let ((wait-semaphore-count (length wait-semaphores))
	(swapchain-count (length swapchains))
	(indices-count (length image-indices))
	(result-count (length results)))
    (with-foreign-objects ((p-wait-semaphore 'VkSemaphore wait-semaphore-count)
			   (p-swapchain 'VkSwapchainKHR swapchain-count)
			   (p-indices :uint32 indices-count)
			   (p-results 'VkResult result-count)
			   (p-info '(:struct VkPresentInfoKHR)))
      (when wait-semaphores
	(dotimes (i wait-semaphore-count)
	  (setf (mem-aref p-wait-semaphore 'VkSemaphore i) (nth i wait-semaphores))))
      (when swapchains
	(dotimes (i swapchain-count)
	  (setf (mem-aref p-swapchain 'VkSwapchainKHR i) (nth i swapchains))))
      (when image-indices
	(dotimes (i indices-count)
	  (setf (mem-aref p-indices :uint32 i) (nth i image-indices))))
      (when results
	(dotimes (i result-count)
	  (setf (mem-aref p-results 'VkResult i) (nth i results))))
      (setf (mem-ref p-info '(:struct VkPresentInfoKHR))
	    (list :sType +structure-type-present-info-khr+
		  :pNext next
		  :waitSemaphoreCount wait-semaphore-count
		  :pWaitSemaphores (set-null-ptr wait-semaphores p-wait-semaphore)
		  :swapchainCount swapchain-count
		  :pSwapchains (set-null-ptr swapchains p-swapchain)
		  :pImageIndices (set-null-ptr image-indices p-indices)
		  :pResults (set-null-ptr results p-results)))
      (check-vk-result (vkQueuePresentKHR present-queue p-info)))))

(defun queue-wait-idle (queue)
  (check-vk-result (vkQueueWaitIdle queue)))

(defun get-device-memory-commitment (device memory)
  (with-foreign-object (p-bytes 'VkDeviceSize)
    (vkGetDeviceMemoryCommitment device memory p-bytes)
    (mem-ref p-bytes 'VkDeviceSize)))

(defun queue-bind-sparse (queue bind-info-ptr fence)
  (let* ((info-count (length bind-info-ptr))
	 (p-info (set-null-ptr bind-info-ptr
			       (foreign-alloc 'bind-sparse-info
					      :count info-count
					      :initial-contents bind-info-ptr))))
    (check-vk-result (vkQueueBindSparse queue info-count p-info fence))
    (dotimes (i info-count)
      (free-bind-sparse-info (mem-aptr p-info 'bind-sparse-info i)))))
