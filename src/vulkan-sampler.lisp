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

(defun create-sampler (device &key
				(next +vk-null-ptr+)
				(flags 0)
				(mag-filter +filter-linear+)
				(min-filter +filter-linear+)
				(mipmap-mode +sampler-mipmap-mode-linear+)
				(mode-u +sampler-address-mode-clamp-to-border+)
				(mode-v +sampler-address-mode-clamp-to-border+)
				(mode-w +sampler-address-mode-clamp-to-border+)
				(mip-lod-bias 0.0)
				(anisotropy-enable vk_false)
				(max-anisotropy 0.0)
				(compare-enable vk_false)
				(compare-op +compare-op-never+)
				(min-lod 0.0)
				(max-lod 0.0)
				(border-color +border-color-float-opaque-black+)
				(unnormalized-cordinates vk_false)
				(allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkSamplerCreateInfo))
			 (p-sampler 'VkSampler))
    (setf (mem-ref p-info '(:struct VkSamplerCreateInfo))
	  (list :sType +structure-type-sampler-create-info+
		:pNext next
		:flags flags
		:magFilter mag-filter
		:minFilter min-filter
		:mipmapMode mipmap-mode
		:addressModeU mode-u
		:addressModeV mode-v
		:addressmodew mode-w
		:mipLodBias mip-lod-bias
		:anisotropyEnable anisotropy-enable
		:maxAnisotropy max-anisotropy
		:compareEnable compare-enable
		:compareOp compare-op
		:minLod min-lod
		:maxLod max-lod
		:borderColor border-color
		:unnormalizedCoordinates unnormalized-cordinates))
    (check-vk-result (vkCreateSampler device p-info allocator p-sampler))
    (mem-ref p-sampler 'VkSampler)))

(defun destroy-sampler (device sampler &optional (allocator +vk-null-ptr+))
  (vkDestroySampler device sampler allocator))
