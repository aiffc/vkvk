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

(defparameter +vk-null-ptr+ (null-pointer))

(declaim (inline set-null-ptr free-if-not-null))

(defun check-vk-result (result)
  (case result
    (#.+SUCCESS+ (values))
    (#.+NOT-READY+   (warn "A fence or query has not yet completed.") (values))
    (#.+TIMEOUT+     (warn "A wait operation has not completed in the specified time.") (values))
    (#.+EVENT-SET+   (format *error-output* "An event is signaled.") (values))
    (#.+EVENT-RESET+ (format *error-output* "An even is unsignaled.") (values))
    (#.+INCOMPLETE+  (warn "A return array was too small for the result.") (values))

    (#.+ERROR-OUT-OF-HOST-MEMORY+
     (error "A host memory allocation has failed."))
    (#.+ERROR-OUT-OF-DEVICE-MEMORY+
     (error "A device memory allocation has failed."))
    (#.+ERROR-INITIALIZATION-FAILED+
     (error "Initialization of an object has failed."))
    (#.+ERROR-DEVICE-LOST+
     (error "The logical device has been lost."))
    (#.+ERROR-MEMORY-MAP-FAILED+
     (error "Mapping of a memory object has failed."))
    (#.+ERROR-LAYER-NOT-PRESENT+
     (error "Layer specified does not exist."))
    (#.+ERROR-EXTENSION-NOT-PRESENT+
     (error "Extension specified does not exist."))
    (#.+ERROR-FEATURE-NOT-PRESENT+
     (error "Requested feature is not available on this device."))
    (#.+ERROR-INCOMPATIBLE-DRIVER+
     (error "Unable to find vulkan driver."))
    (#.+ERROR-TOO-MANY-OBJECTS+
     (error "Too many objects of this type have already been created."))
    (#.+ERROR-FORMAT-NOT-SUPPORTED+
     (error "Requested format is not supported on this device."))
    (#.+ERROR-FRAGMENTED-POOL+
     (error "A requested pool allocation has failed due to fragmentation of the pool's memory."))
    (#.+ERROR-SURFACE-LOST-KHR+
     (error "KHR-surface: ERROR-SURFACE-LOST-KHR"))
    (#.+ERROR-NATIVE-WINDOW-IN-USE-KHR+
     (error "KHR-surface: ERROR-NATIVE-WINDOW-IN-USE"))
    (#.+SUBOPTIMAL-KHR+
     (warn "KHR-swapchain: SUBOPTIMAL-KHR") (values))
    (#.+ERROR-OUT-OF-DATE-KHR+
     (error "KHR-swapchain: ERROR-OUT-OF-DATE-KHR"))
    (#.+ERROR-INCOMPATIBLE-DISPLAY-KHR+
     (error "KHR-display-swapchain: INCOMPATIBLE-DISPLAY"))
    (#.+ERROR-VALIDATION-FAILED-EXT+
     (error "EXT-debug-report: ERROR-VALIDATION-FAILED"))
    (#.+ERROR-INVALID-SHADER-NV+
     (error "NV-glsl-shader: ERROR-INVALID-SHADER-NV"))
    (#.+NV-EXTENSION-1-ERROR+
     (error "NV-extension-1: NV-EXTENSION-1-ERROR"))
    (#.+ERROR-OUT-OF-POOL-MEMORY-KHR+
     (error "KHR-maintenance1: ERROR-OUT-OF-POOL-MEMORY-KHR"))
    (#.+ERROR-INVALID-EXTERNAL-HANDLE-KHX+
     (error "KHX-external-memory: ERROR-INVALID-EXTERNAL-HANDLE-KHX"))
    (t (error "Unknown VkResult: ~S" result))))

(defun ptr->list (ptr type count)
  "convert ptr to list by count,count is a number"
  (loop for i from 0 below count
	collect (mem-aref ptr type i)))

(defun make-vulkan-version (&optional (major 1) (minor 1) (patch 0))
  (logior (ash major 22)
	  (ash minor 12)
	  patch))

(defun show-vulkan-version (version)
  (list (ash (logand #xFFC00000 version) -22) 
	(ash (logand #x003FF000 version) -12)
	(logand #x00000FFF version)))

(defun read-shader-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
	  (byte))
      (loop while (setq byte (read-byte stream nil))
	 do (vector-push-extend byte buffer))
      (let* ((size (fill-pointer buffer))
	     (binary (foreign-alloc (list :array :uint8 size))))
	(loop for b across buffer for i from 0
	   do (setf (mem-aref binary :uint8 i) b))
	(values binary size)))))

(defun set-null-ptr (val ptr)
  (if val ptr +vk-null-ptr+))

(defun loop-clear-ptr (ptr count type &optional (stype nil) (next nil) (flags nil))
  (loop for i from 0 below count
	for p = (mem-aptr ptr type i)
	do (zero-struct p type)
	when stype do
	  (setf (mem-ref p type) (list :sType stype))
	when next do
	  (setf (mem-ref p type) (list :pNext next))
	when flags do
	  (setf (mem-ref p type) (list :flags flags))))

(defun free-if-not-null (ptr)
  (unless (null-pointer-p ptr) (foreign-free ptr)))
