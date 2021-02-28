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

(defparameter *vk-debug* t)
(defparameter *vk-debug-callback* nil)

(defcallback debug-message VkBool32
    ((flags VkFlags)
     (obj-type VkDebugReportObjectTypeExt)
     (src-obj :uint64)
     (location :unsigned-int)
     (msg-code :int32)
     (layer-prefix :string)
     (msg :string)
     (user-data (:pointer :void)))
  (declare (ignore obj-type src-obj location user-data msg-code layer-prefix))
  (cond ((not (zerop (logand flags #.+DEBUG-REPORT-ERROR-BIT-EXT+)))
	 (format t "[debug error] -> message: ~a~2%" msg))
	
	((not (zerop (logand flags #.+DEBUG-REPORT-WARNING-BIT-EXT+)))
	 (format t "[debug waring] -> message: ~a~2%" msg))
	
	((not (zerop (logand flags #.+DEBUG-REPORT-INFORMATION-BIT-EXT+)))
	 (format t "[debug information] -> message: ~a~2%" msg))
	
	((not (zerop (logand flags #.+DEBUG-REPORT-PERFORMANCE-WARNING-BIT-EXT+)))
	 (format t "[debug performance waring] -> message: ~a~2%" msg))
	
	((not (zerop (logand flags #.+DEBUG-REPORT-DEBUG-BIT-EXT+)))
	 (format t "[debug] -> message: ~a~2%" msg)))
  VK_FALSE)

(defun create-debug-report-callback-ext (instance dbg-ptr &optional (allocator +vk-null-ptr+))
  (setf *vk-debug-callback* (foreign-alloc 'VkDebugReportCallbackEXT))
  (with-foreign-object (create-fun 'PFN_vkDebugReportCallbackEXT)
    (with-foreign-string (fun-name "vkCreateDebugReportCallbackEXT")
      (setf (mem-ref create-fun 'PFN_vkDebugReportCallbackEXT)
	    (vkGetInstanceProcAddr instance fun-name))
      (unless (null-pointer-p create-fun)
	(check-vk-result (foreign-funcall-pointer (mem-ref create-fun 'PFN_vkDebugReportCallbackEXT) ()
						  VkInstance instance
						  (:pointer (:struct VkDebugReportCallbackCreateInfoEXT)) dbg-ptr
						  (:pointer (:struct VkAllocationCallbacks)) allocator 
						  VkDebugReportCallbackEXT *vk-debug-callback*
						  VkResult))))))

(defun destroy-debug-report-callback-ext (instance &optional (allocator +vk-null-ptr+))
  (with-foreign-object (destroy-fun 'PFN_vkDebugReportCallbackEXT)
    (with-foreign-string (fun-name "vkDestroyDebugReportCallbackEXT")
      (setf (mem-ref destroy-fun 'PFN_vkDebugReportCallbackEXT)
	    (vkGetInstanceProcAddr instance fun-name))
      (unless (null-pointer-p destroy-fun)
	(foreign-funcall-pointer (mem-ref destroy-fun 'PFN_vkDebugReportCallbackEXT) ()
				 VkInstance instance
				 VkDebugReportCallbackEXT (mem-ref *vk-debug-callback* 'VkDebugReportCallbackEXT)
				 (:pointer (:struct VkAllocationCallbacks)) allocator)))))
