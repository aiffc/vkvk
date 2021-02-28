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

(in-package #:vkvk)

(defun enumerate-instance-extension-properties (layer-name)
  (let ((p-layer-name (if (null layer-name) +vk-null-ptr+ (foreign-string-alloc layer-name))))
    (with-foreign-object (p-count :uint32)
      (check-vk-result (vkEnumerateInstanceExtensionProperties p-layer-name p-count +vk-null-ptr+))
      (let ((count (mem-ref p-count :uint32)))
	(unless (zerop count)
	  (with-foreign-object (p-properties '(:struct VkExtensionProperties) count)
	    (check-vk-result (vkEnumerateInstanceExtensionProperties p-layer-name p-count p-properties))
	    (unless (null layer-name)
	      (foreign-string-free p-layer-name))
	    (ptr->list p-properties 'extension-properties count)))))))

(defun enumerate-instance-layer-properties  ()
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkEnumerateInstanceLayerProperties p-count +vk-null-ptr+))
    (let ((count (mem-ref p-count :uint32)))
      (unless (zerop count)
	(with-foreign-object (p-properties '(:struct VkLayerProperties) count)
	  (check-vk-result (vkEnumerateInstanceLayerProperties p-count p-properties))
	  (ptr->list p-properties 'layer-properties count))))))

(defun available-instance-extensions ()
  (remove-duplicates (append (mapcar #'(lambda (property) (getf property :extensionName))
				     (apply #'append (mapcar #'enumerate-instance-extension-properties
							     (available-instance-layers))))
			     (mapcar #'(lambda (property) (getf property :extensionName))
				     (enumerate-instance-extension-properties nil)))
		     :test #'string=))

(defun available-instance-layers ()
  (remove-duplicates (mapcar #'(lambda (property) (getf property :layerName))
			     (enumerate-instance-layer-properties))
		     :test #'string=))

(defun create-instance (&key
			  ;;VkApplicationInfo
			  (app-next +vk-null-ptr+)                         ;;pNext
			  (app-name "vkvk test")                           ;;pApplicationName
			  (app-version (make-vulkan-version 0 0 0))        ;;applicationVersion
			  (engine-name "vkvk test")                        ;;pEngineName
			  (engine-version (make-vulkan-version 0 0 0))     ;;engineVersion
			  (api-version (make-vulkan-version 1 2 0))        ;;apiVersion
			  ;;VkInstanceCreateInfo
			  (instance-next +vk-null-ptr+)                    ;;pNext
			  (instance-flags 0)                               ;;flags
			  (layer-names nil)                                ;;ppEnabledLayerNames
			  (extension-names nil)                            ;;ppEnabledExtensionNames
			  (allocator +vk-null-ptr+))
  (when *vk-debug*
    (pushnew "VK_LAYER_KHRONOS_validation" layer-names)
    (pushnew "VK_EXT_debug_report" extension-names)
    
    (setf instance-next (foreign-alloc '(:struct VkDebugReportCallbackCreateInfoEXT))
	  (mem-ref instance-next '(:struct VkDebugReportCallbackCreateInfoEXT))
	  (list :sType +STRUCTURE-TYPE-DEBUG-REPORT-CALLBACK-CREATE-INFO-EXT+
		:pNext +vk-null-ptr+
		:flags (logior +DEBUG-REPORT-INFORMATION-BIT-EXT+
			       +DEBUG-REPORT-WARNING-BIT-EXT+
			       +DEBUG-REPORT-PERFORMANCE-WARNING-BIT-EXT+
			       +DEBUG-REPORT-ERROR-BIT-EXT+
			       +DEBUG-REPORT-ERROR-BIT-EXT+)
		:pfnCallback (callback debug-message)
		:pUserData +vk-null-ptr+)))
  (let* ((usable-layers (intersection (available-instance-layers) layer-names :test #'string=))
	 (usable-extensions (intersection (available-instance-extensions) extension-names :test #'string=))
	 (layer-count (length usable-layers))
	 (extension-count (length usable-extensions)))
    (with-foreign-strings ((p-app-name app-name)
			   (p-engine-name engine-name))
      (with-foreign-objects ((p-application-info '(:struct VkApplicationInfo))
			     (p-create-info '(:struct VkInstanceCreateInfo))
			     (p-lay '(:pointer :char) layer-count)
			     (p-ext '(:pointer :char) extension-count)
			     (p-instance 'VkInstance))
	(when extension-names
	  (dotimes (i extension-count)
	    (setf (mem-aref p-ext '(:pointer :char) i)
		  (foreign-string-alloc (nth i usable-extensions)))))
	(when layer-names
	  (dotimes (i layer-count)
	    (setf (mem-aref p-lay '(:pointer :char) i)
		  (foreign-string-alloc (nth i usable-layers)))))
	
	(setf (mem-ref p-application-info '(:struct VkApplicationInfo))
	      (list :sType +STRUCTURE-TYPE-APPLICATION-INFO+
		    :pNext app-next 
		    :pApplicationName p-app-name
		    :applicationVersion app-version
		    :pEngineName p-engine-name
		    :engineVersion engine-version
		    :apiVersion api-version)
	      (mem-ref p-create-info '(:struct VkInstanceCreateInfo))
	      (list :sType +STRUCTURE-TYPE-INSTANCE-CREATE-INFO+
		    :pNext instance-next
		    :flags instance-flags
		    :pApplicationInfo p-application-info
		    :enabledLayerCount layer-count
		    :ppEnabledLayerNames (set-null-ptr layer-names p-lay)
		    :enabledExtensionCount extension-count
		    :ppEnabledExtensionNames (set-null-ptr extension-names p-ext)))
	(check-vk-result (vkCreateInstance p-create-info allocator p-instance))
	(let ((ret-ist (mem-ref p-instance 'VkInstance)))
	  (when *vk-debug*
	    (create-debug-report-callback-ext ret-ist instance-next)
	    (foreign-free instance-next))
	  ret-ist)))))

(defun destroy-instance (instance &optional (allocator +vk-null-ptr+))
  (when *vk-debug*
    (destroy-debug-report-callback-ext instance)
    (setf *vk-debug-callback* +vk-null-ptr+))
  (vkDestroyInstance instance allocator))

