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

#|
(defvkinstextfun ("vkCreateAndroidSurfaceKHR" vkCreateAndroidSurfaceKHR) VkResult
  (instance VkInstance)
  (pCreateInfo (:pointer (:struct VkAndroidSurfaceCreateInfoKHR)))
  (pAllocator (:pointer (:struct VkAllocationCallbacks))) ;; :OPTIONAL "true"
  (pSurface (:pointer VkSurfaceKHR)))

(cffi:defcfun ("vkCreateDisplayPlaneSurfaceKHR" vkCreateDisplayPlaneSurfaceKHR) VkResult
  (instance VkInstance)
  (pCreateInfo (:pointer (:struct VkDisplaySurfaceCreateInfoKHR)))
  (pAllocator (:pointer (:struct VkAllocationCallbacks))) ;; :OPTIONAL "true"
  (pSurface (:pointer VkSurfaceKHR)))

(defvkinstextfun ("vkCreateMirSurfaceKHR" vkCreateMirSurfaceKHR) VkResult
  (instance VkInstance)
  (pCreateInfo (:pointer (:struct VkMirSurfaceCreateInfoKHR)))
  (pAllocator (:pointer (:struct VkAllocationCallbacks))) ;; :OPTIONAL "true"
  (pSurface (:pointer VkSurfaceKHR)))

(defvkinstextfun ("vkCreateViSurfaceNN" vkCreateViSurfaceNN) VkResult
  (instance VkInstance)
  (pCreateInfo (:pointer (:struct VkViSurfaceCreateInfoNN)))
  (pAllocator (:pointer (:struct VkAllocationCallbacks))) ;; :OPTIONAL "true"
  (pSurface (:pointer VkSurfaceKHR)))

(defvkinstextfun ("vkCreateWaylandSurfaceKHR" vkCreateWaylandSurfaceKHR) VkResult
  (instance VkInstance)
  (pCreateInfo (:pointer (:struct VkWaylandSurfaceCreateInfoKHR)))
  (pAllocator (:pointer (:struct VkAllocationCallbacks))) ;; :OPTIONAL "true"
  (pSurface (:pointer VkSurfaceKHR)))
|#

(defun create-ios-surface-mvk (instance p-view &key
						 (next +vk-null-ptr+)
						 (flags 0)
						 (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkIOSSurfaceCreateInfoMVK))
			 (p-surface 'VkSurfaceKHR))
    (setf (mem-ref p-info '(:struct VkIOSSurfaceCreateInfoMVK))
	  (list :sType +structure-type-ios-surface-create-info-mvk+
		:pNext next 
		:flags flags
		:pView p-view)) ;;unknow
    (check-vk-result (vkCreateIOSSurfaceMVK instance instance p-info allocator p-surface))
    (mem-ref p-surface 'VkSurfaceKHR)))

(defun create-mac-os-surface-mvk (instance p-view &key
						    (next +vk-null-ptr+)
						    (flags 0)
						    (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkMacOSSurfaceCreateInfoMVK))
			 (p-surface 'VkSurfaceKHR))
    (setf (mem-ref p-info '(:struct VkMacOSSurfaceCreateInfoMVK))
	  (list :sType +STRUCTURE-TYPE-MACOS-SURFACE-CREATE-INFO-MVK+
		:pNext next 
		:flags flags
		:pView p-view))
    (check-vk-result (vkCreateMacOSSurfaceMVK instance instance p-info allocator p-surface))
    (mem-ref p-surface 'VkSurfaceKHR)))

(defun create-win32-surface-khr (instance hinstance hwnd &key
							   (next +vk-null-ptr+)
							   (flags 0)
							   (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkWin32SurfaceCreateInfoKHR))
			 (p-surface 'VkSurfaceKHR))
    (setf (mem-ref p-info '(:struct VkWin32SurfaceCreateInfoKHR))
	  (list :sType +STRUCTURE-TYPE-WIN32-SURFACE-CREATE-INFO-KHR+
		:pNext next 
		:flags flags
		:hinstance hinstance
		:hwnd hwnd))
    (check-vk-result (vkCreateWin32SurfaceKHR instance p-info allocator p-surface))
    (mem-ref p-surface 'VkSurfaceKHR)))

(defun create-xcb-surface-khr (instance connection window &key
							    (next +vk-null-ptr+)
							    (flags 0)
							    (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkXcbSurfaceCreateInfoKHR))
			 (p-surface 'VkSurfaceKHR))
    (setf (mem-ref p-info '(:struct VkXcbSurfaceCreateInfoKHR))
	  (list :sType +STRUCTURE-TYPE-XCB-SURFACE-CREATE-INFO-KHR+
		:pNext next 
		:flags flags
		:connection connection
		:window window))
    (check-vk-result (vkCreateXcbSurfaceKHR instance instance p-info allocator p-surface))
    (mem-ref p-surface 'VkSurfaceKHR)))

(defun create-xlib-surface-khr (instance dpy window &key
						      (next +vk-null-ptr+)
						      (flags 0)
						      (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkXlibSurfaceCreateInfoKHR))
			 (p-surface 'VkSurfaceKHR))
    (setf (mem-ref p-info '(:struct VkXlibSurfaceCreateInfoKHR))
	  (list :sType +STRUCTURE-TYPE-XLIB-SURFACE-CREATE-INFO-KHR+
		:pNext next 
		:flags flags
		:dpy dpy
		:window window))
    (check-vk-result (vkCreateXlibSurfaceKHR instance instance p-info allocator p-surface))
    (mem-ref p-surface 'VkSurfaceKHR)))


(defun destroy-surface-khr (instance surface &key (allocator +vk-null-ptr+))
  (vkDestroySurfaceKHR instance surface allocator))
