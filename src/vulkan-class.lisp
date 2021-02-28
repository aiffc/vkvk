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

(defclass viewport-ptr ()
  ((x :initarg :x)
   (y :initarg :y)
   (width :initarg :width)
   (height :initarg :height)
   (min-depth :initarg :min-depth)
   (max-depth :initarg :max-depth)))
(defun make-viewport-ptr (x y width height min-depth max-depth)
  (make-instance 'viewport-ptr :x x :y y :width width :height height :min-depth min-depth :max-depth max-depth))

(defclass rect2d-ptr ()
  ((x :initarg :x)
   (y :initarg :y)
   (width :initarg :width)
   (height :initarg :height)))
(defun make-rect2d-ptr (x y width height)
  (make-instance 'rect2d-ptr :x x :y y :width width :height height))

(defclass clear-color-value-ptr ()
  ((r :initarg r)
   (g :initarg g)
   (b :initarg b)
   (a :initarg a)))
(defun make-clear-color-value-ptr (r g b a)
  (make-instance 'clear-color-valueptr :r r :g g :b b :a a))

(defclass clear-value-ptr (clear-color-value-ptr) ())
(defun make-clear-value-ptr (r g b a)
  (make-instance 'clear-value-ptr :r r :g g :b b :a a))

(defclass device-queue-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (family-index :initarg :family-index)
   (properties :initarg :properties)))
(defun make-device-queue-ptr (family-index properties &key (next +vk-null-ptr+) (flags 0))
  "family-uint32 -> uint32 properties -> list"
  (make-instance 'device-queue-ptr :next next :flags flags :family-index family-index :properties properties))

(defclass specialization-map-entry ()
  ((id :initarg id)
   (offset :initarg offset)
   (size :initarg size)))
(defun make-specialization-map-entry-ptr (id offset size)
  (make-instance 'specialization-map-entry :id id :offset offset :size size))

(defclass specialization-ptr ()
  ((map-entries :initarg :map-encties)
   (size :initarg :size)
   (p-data :initarg :p-data)))
(defun make-specialization-ptr (map-entry-ptr size p-data)
  (make-instance 'specialization-ptr :map-encties map-entry-ptr :size size :p-data p-data))

(defclass vertex-input-binding-descriptor-ptr ()
  ((binding :initarg :binding)
   (stride :initarg :stride)
   (rate :initarg :rate)))
(defun make-vertex-input-binding-descriptor-ptr (binding stride rate)
  (make-instance 'vertex-input-binding-descriptor-ptr :binding binding :stride stride :rate rate))

(defclass vertex-input-attribute-desription-ptr ()
  ((location :initarg :location)
   (binding :initarg :binding)
   (format :initarg :format)
   (offset :initarg :offset)))
(defun make-vertex-input-attribute-desription-ptr (location binding offset &optional (format +format-r8g8b8a8-srgb+))
  (make-instance 'vertex-input-attribute-desription-ptr :location location
							:binding binding
							:format format
							:offset offset))

(defclass pipeline-shader-stage-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (stage :initarg :stage)
   (module :initarg :module)
   (name :initarg :name)
   (specialization :initarg :specialization-ptr)))
(defun make-pipeline-shader-stage-ptr (module &key
						(next +vk-null-ptr+)
						(flags 0)
						(stage +shader-stage-vertex-bit+)
						(name "main")
						(specialization-ptr nil))
  (make-instance 'pipeline-shader-stage-ptr :next next
					    :flags flags
					    :stage stage
					    :module module
					    :name name
					    :specialization-ptr specialization-ptr))

(defclass pipeline-vertex-input-state ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (binding-ptr :initarg :binding)
   (attribute-ptr :initarg :attribute)))
(defun make-pipeline-vertex-input-state (binding-ptr attribute-ptr &key (next +vk-null-ptr+) (flags 0))
  (make-instance 'pipeline-vertex-input-state :next next
					      :flags flags
					      :binding binding-ptr
					      :attribute attribute-ptr))

(defclass pipeline-input-assembly-state-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (topology :initarg :topology)
   (restart-enable :initarg :restart-enable)))
(defun make-pipeline-input-assembly-state-ptr (topology restart-enable &key (next +vk-null-ptr+) (flags 0))
  (make-instance 'pipeline-input-assembly-state-ptr :next next :flags flags :topology topology :restart-enable restart-enable))

(defclass pipeline-tessellation-state-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (points :initarg :points)))
(defun make-pipeline-tessellation-state-ptr (points &key (next +vk-null-ptr+) (flags 0))
  (make-instance 'pipeline-tessellation-state-ptr :next next :flags flags :points points))

(defclass pipeline-viewport-state-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (viewports :initarg :viewports)
   (scissors :initarg :scissors)))
(defun make-pipeline-viewport-state-ptr (viewports scissors &key (next +vk-null-ptr+) (flags 0))
  (make-instance 'pipeline-viewport-state-ptr :next next :flags flags
					      :viewports viewports :scissors scissors))

(defclass pipeline-rasterization-state-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (clamp-enable :initarg :clamp-enable)
   (discard-enable :initarg :discard-enable)
   (polygon-mode :initarg :polygon-mode)
   (cull-mode :initarg :cull-mode)
   (front-face :initarg :front-face)
   (bias-enable :initarg :bias-enable)
   (constant-factor :initarg :constant-factor)
   (clamp :initarg :clamp)
   (slope-factor :initarg :slope-factor)
   (line-width :initarg :line-width)))
(defun make-pipeline-rasterization-state-ptr (line-width &key
							   (next +vk-null-ptr+)
							   (flags 0)
							   (clamp-enable vk_false)
							   (discard-enable vk_false)
							   (polygon-mode +polygon-mode-fill+)
							   (cull-mode +cull-mode-back-bit+)
							   (front-face +front-face-clockwise+)
							   (bias-enable vk_false)
							   (constant-factor 0.0)
							   (clamp 0.0)
							   (slope-factor 0.0))
  (make-instance 'pipeline-rasterization-state-ptr :next next :flags flags
						   :clamp-enable clamp-enable
						   :discard-enable discard-enable
						   :polygon-mode polygon-mode
						   :cull-mode cull-mode
						   :front-face front-face
						   :bias-enable bias-enable
						   :constant-factor constant-factor
						   :clamp clamp
						   :slope-factor slope-factor
						   :line-width line-width))

(defclass pipeline-multisample-state-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (samples :initarg :samples)
   (sample-shading-enable :initarg :sample-shading-enable)
   (min-sample-shading :initarg :min-sample-shading)
   (sample-mask :initarg :sample-mask)
   (alpha-to-coverage-enable :initarg :alpha-to-coverage-enable)
   (alpha-to-one-enable :initarg :alpha-to-one-enable)))
(defun make-pipeline-multisample-state-ptr (&key
					      (next +vk-null-ptr+)
					      (flags 0)
					      (samples +sample-count-1-bit+)
					      (sample-shading-enable vk_false)
					      (min-sample-shading 0.0)
					      (sample-mask nil)
					      (alpha-to-coverage-enable vk_false)
					      (alpha-to-one-enable vk_false))
  (make-instance 'pipeline-multisample-state-ptr :next next :flags flags
						 :samples samples
						 :sample-shading-enable sample-shading-enable
						 :min-sample-shading min-sample-shading
						 :sample-mask sample-mask
						 :alpha-to-coverage-enable alpha-to-coverage-enable
						 :alpha-to-one-enable alpha-to-one-enable))

(defclass pipeline-depth-stencil-state-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (depth-test-enable :initarg :depth-test-enable)
   (depth-write-enable :initarg :depth-write-enable)
   (compare-op :initarg :compare-op)
   (depth-bounds-test-enable :initarg :depth-bounds-test-enable)
   (stencil-test-enable :initarg :stencil-test-enable)
   (front-fail-op :initarg :front-fail-op)
   (front-pass-op :initarg :front-pass-op)
   (front-depth-fail-op :initarg :front-depth-fail-op)
   (front-compare-op :initarg :front-compare-op)
   (front-compare-mask :initarg :front-compare-mask)
   (front-write-mask :initarg :front-write-mask)
   (front-reference :initarg :front-reference)
   (back-fail-op :initarg :back-fail-op)
   (back-pass-op :initarg :back-pass-op)
   (back-depth-fail-op :initarg :back-depth-fail-op)
   (back-compare-op :initarg :back-compare-op)
   (back-compare-mask :initarg :back-compare-mask)
   (back-write-mask :initarg :back-write-mask)
   (back-reference :initarg :back-reference)
   (min-depth-bounds :initarg :min-depth-bounds)
   (max-depth-bounds :initarg :max-depth-bounds)))
(defun make-pipeline-depth-stencil-state-ptr (&key
						(next +vk-null-ptr+)
						(flags 0)
						(depth-test-enable vk_false)
						(depth-write-enable vk_false)
						(compare-op +compare-op-never+)
						(depth-bounds-test-enable vk_false)
						(stencil-test-enable vk_false)
						(front-fail-op +stencil-op-keep+)
						(front-pass-op +stencil-op-keep+)
						(front-depth-fail-op +stencil-op-keep+)
						(front-compare-op +compare-op-never+)
						(front-compare-mask 0)
						(front-write-mask 0)
						(front-reference 0)
						(back-fail-op +stencil-op-keep+)
						(back-pass-op +stencil-op-keep+)
						(back-depth-fail-op +stencil-op-keep+)
						(back-compare-op +compare-op-never+)
						(back-compare-mask 0)
						(back-write-mask 0)
						(back-reference 0)
						(min-depth-bounds 0.0)
						(max-depth-bounds 0.0))
  (make-instance 'pipeline-depth-stencil-state
		 :next next
		 :flags flags
		 :depth-test-enable depth-test-enable
		 :depth-write-enable depth-write-enable
		 :compare-op compare-op
		 :depth-bounds-test-enable depth-bounds-test-enable
		 :stencil-test-enable stencil-test-enable
		 :front-fail-op front-fail-op
		 :front-pass-op front-pass-op
		 :front-depth-fail-op front-depth-fail-op
		 :front-compare-op front-compare-op
		 :front-compare-mask front-compare-mask
		 :front-write-mask front-write-mask
		 :front-reference front-reference
		 :back-fail-op back-fail-op
		 :back-pass-op back-pass-op
		 :back-depth-fail-op back-depth-fail-op
		 :back-compare-op back-compare-op
		 :back-compare-mask back-compare-mask
		 :back-write-mask back-write-mask
		 :back-reference back-reference
		 :min-depth-bounds min-depth-bounds
		 :max-depth-bounds max-depth-bounds))

(defclass pipeline-color-blend-attachment-state-ptr ()
  ((blend-enable :initarg :blend-enable)
   (src-color-blend :initarg :src-color-blend)
   (dst-color-blend :initarg :dst-color-blend)
   (color-blend-op :initarg :color-blend-op)
   (src-alpha-blend :initarg :src-alpha-blend)
   (dst-alpha-blend :initarg :dst-alpha-blend)
   (alpha-blend-op :initarg :alpha-blend-op)
   (write-mask :initarg :write-mask)))
(defun make-pipeline-color-blend-attachment-state-ptr (&key
							 (blend-enable vk_false)
							 (src-color-blend +blend-factor-one+)
							 (dst-color-blend +blend-factor-zero+)
							 (color-blend-op +blend-op-add+)
							 (src-alpha-blend +blend-factor-one+)
							 (dst-alpha-blend +blend-factor-zero+)
							 (alpha-blend-op +blend-op-add+)
							 (write-mask +color-component-r-bit+))
  (make-instance 'pipeline-color-blend-attachment-state-ptr
		 :blend-enable blend-enable
		 :src-color-blend src-color-blend
		 :dst-color-blend dst-color-blend
		 :color-blend-op color-blend-op
		 :src-alpha-blend src-alpha-blend
		 :dst-alpha-blend dst-alpha-blend
		 :alpha-blend-op alpha-blend-op
		 :write-mask write-mask))

(defclass pipeline-color-blend-state-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (logic-op-enable :initarg :logic-op-enable)
   (logic-op :initarg :logic-op)
   (attachments :initarg :attachments)
   (r :initarg :r)
   (g :initarg :g)
   (b :initarg :b)
   (a :initarg :a)))
(defun make-pipeline-color-blend-state-ptr (attachments &key
							  (next +vk-null-ptr+)
							  (flags 0)
							  (logic-op-enable vk_false)
							  (logic-op +logic-op-copy+)
							  (r 0.0)
							  (g 0.0)
							  (b 0.0)
							  (a 0.0))
  (make-instance 'pipeline-color-blend-attachment-state-ptr :next next :flags flags
							    :logic-op-enable logic-op-enable :logic-op logic-op
							    :attachments attachments
							    :r r :g g :b b :a a))

(defclass pipeline-dynamic-state-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (states :initarg :states)))
(defun make-pipeline-dynamic-state-ptr (&key
					  (next +vk-null-ptr+)
					  (flags 0)
					  (states nil))
  (make-instance 'pipeline-dynamic-state-ptr :next next :flags flags :states states))

(defclass graphics-pipeline-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (shader-stage :initarg :shader-stage)
   (vertex-state :initarg :vertex-state)
   (assembly-state :initarg :assembly-state)
   (tessellation-state :initarg :tessellation-state)
   (viewport-state :initarg :viewport-state)
   (rasterization-state :initarg :rasterization-state)
   (multisample-state :initarg :multisample-state)
   (depth-stencil-state :initarg :depth-stencil-state)
   (color-blend-state :initarg :color-blend-state)
   (dynamic-state :initarg :dynamic-state)
   (layout :initarg :layout)
   (render-pass :initarg :render-pass)
   (subpass :initarg :subpass)
   (handle :initarg :handle)
   (index :initarg :index)))
(defun make-graphics-pipeline-ptr (layout render-pass subpass handle index shader-stage &key
											 (next +vk-null-ptr+)
											 (flags 0)
											 (vertex-state nil)
											 (assembly-state nil)
											 (tessellation-state nil)
											 (viewport-state nil)
											 (rasterization-state nil)
											 (multisample-state nil)
											 (depth-stencil-state nil)
											 (color-blend-state nil)
											 (dynamic-state nil))
  (make-instance 'graphics-pipeline-ptr :next next
					:flags flags
					:shader-stage shader-stage
					:vertex-state vertex-state
					:assembly-state assembly-state
					:tessellation-state tessellation-state
					:viewport-state viewport-state
					:rasterization-state rasterization-state
					:multisample-state multisample-state
					:depth-stencil-state depth-stencil-state
					:color-blend-state color-blend-state
					:dynamic-state dynamic-state
					:layout layout
					:render-pass render-pass
					:subpass subpass
					:handle handle
					:index index))

(defclass compute-pipeline-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (stage :initarg :stage)
   (layout :initarg :layout)
   (handle :initarg :handle)
   (index :initarg :index)))
(defun make-compute-pipeline-ptr (layout handle index stage &key
							      (next +vk-null-ptr+)
							      (flags 0))
  (make-instance 'compute-pipeline-ptr :next next :flags flags
				       :stage (if (listp stage) (first stage) stage)
				       :layout layout
				       :handle handle
				       :index index))

(defclass push-constant-range-ptr ()
  ((flags :initarg :flags)
   (offset :initarg :offset)
   (size :initarg :size)))
(defun make-push-constant-range-ptr (flags offset size)
  (make-instance 'push-constant-range-ptr :flags flags :offset offset :size size))

(defclass attachment-description-ptr ()
  ((flags :initarg :flags)
   (format :initarg :format)
   (samples :initarg :samples)
   (load-op :initarg :load-op)
   (store-op :initarg :store-op)
   (stencil-load-op :initarg :stencil-load-op)
   (stencil-store-op :initarg :stencil-store-op)
   (initial-layout :initarg :initial-layout)
   (final-layout :initarg :final-layout)))
(defun make-attachment-description-ptr (&key
					  (flags 0)
					  (format +format-r8g8b8a8-srgb+)
					  (samples +sample-count-1-bit+)
					  (load-op +attachment-load-op-clear+)
					  (store-op +attachment-store-op-store+)
					  (stencil-load-op +attachment-load-op-dont-care+)
					  (stencil-store-op +attachment-store-op-dont-care+)
					  (initial-layout +image-layout-undefined+)
					  (final-layout +image-layout-present-src-khr+))
  (make-instance 'attachment-description-ptr
		 :flags flags
		 :format format
		 :samples samples
		 :load-op load-op
		 :store-op store-op
		 :stencil-load-op stencil-load-op
		 :stencil-store-op stencil-store-op
		 :initial-layout initial-layout
		 :final-layout final-layout))

(defclass subpass-dependency-description-ptr ()
  ((src-subpass :initarg :src-subpass)
   (dst-subpass :initarg :dst-subpass)
   (src-stage-mask :initarg :src-stage-mask)
   (dst-stage-mask :initarg :dst-stage-mask)
   (src-access-mask :initarg :src-access-mask)
   (dst-access-mask :initarg :dst-access-mask)
   (flags :initarg :flags)))
(defun make-subpass-dependency-description-ptr (&key
						  (src-subpass 1)
						  (dst-subpass 0)
						  (src-stage-mask +pipeline-stage-color-attachment-output-bit+)
						  (dst-stage-mask 0)
						  (src-access-mask +pipeline-stage-color-attachment-output-bit+)
						  (dst-access-mask +access-color-attachment-write-bit+)
						  (dependency-flags 0))
  (make-instance 'subpass-dependency-description-ptr
		 :src-subpass src-subpass
		 :dst-subpass dst-subpass
		 :src-stage-mask src-stage-mask
		 :dst-stage-mask dst-stage-mask
		 :src-access-mask src-access-mask
		 :dst-access-mask dst-access-mask
		 :flags dependency-flags))

(defclass attachment-reference-ptr ()
  ((attachment :initarg :attachment)
   (layout :initarg :layout)))
(defun make-attachment-reference-ptr (attachment layout)
  (make-instance 'attachment-reference
		 :attachment attachment
		 :layout layout))

(defclass subpass-description-ptr ()
  ((flags :initarg :flags)
   (pipeline-bind-point :initarg :pipeline-bind-point)
   (input-attachments :initarg :input-attachments)
   (color-attachments :initarg :color-attachments)
   (resolve-attachments :initarg :resolve-attachments)
   (depth-stencil-attachments :initarg :depth-stencil-attachments)
   (preserve-attachments :initarg :preserve-attachments)))
(defun make-subpass-description-ptr (&key
				       (flags 0)
				       (bind-point +pipeline-bind-point-graphics+)
				       (input-attachments nil)
				       (color-attachments nil)
				       (resolve-attachments nil)
				       (depth-stencil-attachments nil)
				       (preserve-attachments nil))
  (make-instance 'subpass-description-ptr
		 :flags flags
		 :pipeline-bind-point bind-point
		 :input-attachments input-attachments
		 :color-attachments color-attachments
		 :resolve-attachments resolve-attachments
		 :depth-stencil-attachments depth-stencil-attachments
		 :preserve-attachments preserve-attachments))

(defclass command-buffer-inheritance-ptr ()
  ((next :initarg :next)
   (render-pass :initarg :render-pass)
   (subpass :initarg :subpass)
   (framebuffer :initarg :framebuffer)
   (query-enable :initarg :query-enable)
   (query-flags :initarg :query-flags)
   (pipeline-statistics :initarg :pipeline-statistics)))
(defun make-command-buffer-inheritance-ptr (render-pass framebuffer &key
								      (next +vk-null-ptr+)
								      (subpass 0)
								      (query-enable vk_false)
								      (query-flags 0)
								      (pipeline-flags 0))
  (make-instance 'command-buffer-inheritance-ptr
		 :next next
		 :render-pass render-pass
		 :subpass subpass
		 :framebuffer framebuffer
		 :query-enable query-enable
		 :query-flags query-flags
		 :pipeline-statistics pipeline-flags))

(defclass image-blit-ptr ()
  ((src-aspect-mask :initarg :src-aspect-mask)
   (src-mip-level :initarg :src-mip-level)
   (src-base-array-layer :initarg :src-base-array-layer)
   (src-layer-count :initarg :src-layer-count)
   (src-offset-x1 :initarg :src-offset-x1)
   (src-offset-y1 :initarg :src-offset-y1)
   (src-offset-z1 :initarg :src-offset-z1)
   (src-offset-x2 :initarg :src-offset-x2)
   (src-offset-y2 :initarg :src-offset-y2)
   (src-offset-z2 :initarg :src-offset-z2)
   (dst-aspect-mask :initarg :dst-aspect-mask)
   (dst-mip-level :initarg :dst-mip-level)
   (dst-base-array-layer :initarg :dst-base-array-layer)
   (dst-layer-count :initarg :dst-layer-count)
   (dst-offset-x1 :initarg :dst-offset-x1)
   (dst-offset-y1 :initarg :dst-offset-y1)
   (dst-offset-z1 :initarg :dst-offset-z1)
   (dst-offset-x2 :initarg :dst-offset-x2)
   (dst-offset-y2 :initarg :dst-offset-y2)
   (dst-offset-z2 :initarg :dst-offset-z2)))
(defun make-image-blit-ptr (&key
			      (src-aspect-mask 0)
			      (src-mip-level 0)
			      (src-base-array-layer 0)
			      (src-layer-count 0)
			      (src-offset-x1 0)
			      (src-offset-y1 0)
			      (src-offset-z1 0)
			      (src-offset-x2 0)
			      (src-offset-y2 0)
			      (src-offset-z2 0)				       
			      (dst-aspect-mask 0)
			      (dst-mip-level 0)
			      (dst-base-array-layer 0)
			      (dst-layer-count 0)
			      (dst-offset-x1 0)
			      (dst-offset-y1 0)
			      (dst-offset-z1 0)
			      (dst-offset-x2 0)
			      (dst-offset-y2 0)
			      (dst-offset-z2 0))
  (make-instance 'image-blit-ptr
		 :src-aspect-mask src-aspect-mask
		 :src-mip-level src-mip-level
		 :src-base-array-layer src-base-array-layer
		 :src-layer-count src-layer-count
		 :src-offset-x1 src-offset-x1
		 :src-offset-y1 src-offset-y1
		 :src-offset-z1 src-offset-z1
		 :src-offset-x2 src-offset-x2
		 :src-offset-y2 src-offset-y2
		 :src-offset-z2 src-offset-z2
		 :dst-aspect-mask dst-aspect-mask
		 :dst-mip-level dst-mip-level
		 :dst-base-array-layer dst-base-array-layer
		 :dst-layer-count dst-layer-count
		 :dst-offset-x1 dst-offset-x1
		 :dst-offset-y1 dst-offset-y1
		 :dst-offset-z1 dst-offset-z1
		 :dst-offset-x2 dst-offset-x2
		 :dst-offset-y2 dst-offset-y2
		 :dst-offset-z2 dst-offset-z2))

(defclass memory-barrier-ptr ()
  ((next :initarg :next)
   (src-mask :initarg :src-mask)
   (dst-mask :initarg :dst-mask)))
(defun make-memory-barrier-ptr (src-mask dst-mask &key (next +vk-null-ptr+))
  (make-instance 'memory-barrier-ptr :next next :src-mask src-mask :dst-mask dst-mask))

(defclass buffer-memory-barrier-ptr ()
  ((next :initarg :next)
   (src-mask :initarg :src-mask)
   (dst-mask :initarg :dst-mask)
   (src-queue-family-index :initarg :src-queue-family-index)
   (dst-queue-family-index :initarg :dst-queue-family-index)
   (buffer :initarg :buffer)
   (offset :initarg :offset)
   (size :initarg :size)))
(defun make-buffer-memory-barrier-ptr (src-mask dst-mask
				       src-queue-family-index dst-queue-family-index
				       buffer size &key (next +vk-null-ptr+) (offset 0))
  (make-instance 'buffer-memory-barrier-ptr
		 :next next
		 :src-mask src-mask
		 :dst-mask dst-mask
		 :src-queue-family-index src-queue-family-index
		 :dst-queue-family-index dst-queue-family-index
		 :buffer buffer
		 :offset offset
		 :size size))

(defclass image-memory-barrier-ptr ()
  ((next :initarg :next)
   (src-mask :initarg :src-mask)
   (dst-mask :initarg :dst-mask)
   (old-layout :initarg :old-layout)
   (new-layout :initarg :new-layout)
   (src-queue-family-index :initarg :src-queue-family-index)
   (dst-queue-family-index :initarg :dst-queue-family-index)
   (image :initarg :image)
   (aspect-mask :initarg :aspect-mask)
   (base-mip-level :initarg :base-mip-level)
   (level-count :initarg :level-count)
   (base-array-layer :initarg :base-array-layer)
   (layer-count :initarg :layout-count)))
(defun make-image-memory-barrier-ptr (image
				      src-mask dst-mask
				      old-layout new-layout
				      src-queue-family-index dst-queue-family-index
				      &key
					(next +vk-null-ptr+)
					(aspect-mask 0)
					(base-mip-level 0)
					(level-count 0)
					(base-array-layer 0)
					(layer-count 0))
  (make-instance 'image-memory-barrier-ptr
		 :next next
		 :src-mask src-mask
		 :dst-mask dst-mask
		 :old-layout old-layout
		 :new-layout new-layout
		 :src-queue-family-index src-queue-family-index
		 :dst-queue-family-index dst-queue-family-index
		 :image image
		 :aspect-mask aspect-mask
		 :base-mip-level base-mip-level
		 :level-count level-count
		 :base-array-layer base-array-layer
		 :layer-count layer-count))

(defclass image-reslove-ptr ()
  ((src-aspect-mask :initarg :src-aspect-mask)
   (src-mip-level :initarg :src-mip-level)
   (src-base-array-layer :initarg :src-base-array-layer)
   (src-layer-count :initarg :src-layer-count)
   (src-offset-x :initarg :src-offset-x)
   (src-offset-y :initarg :src-offset-y)
   (src-offset-z :initarg :src-offset-z)
   (dst-aspect-mask :initarg :dst-aspect-mask)
   (dst-mip-level :initarg :dst-mip-level)
   (dst-base-array-layer :initarg :dst-base-array-layer)
   (dst-layer-count :initarg :dst-layer-count)
   (dst-offset-x :initarg :dst-offset-x)
   (dst-offset-y :initarg :dst-offset-y)
   (dst-offset-z :initarg :dst-offset-z)
   (width :initarg :width)
   (height :initarg :height)
   (depth :initarg :depth)))
(defun make-image-reslove-ptr (&key
				 (src-aspect-mask 0)
				 (src-mip-level 0)
				 (src-base-array-layer 0)
				 (src-layer-count 0)
				 (src-offset-x 0)
				 (src-offset-y 0)
				 (src-offset-z 0)
				 (dst-aspect-mask 0)
				 (dst-mip-level 0)
				 (dst-base-array-layer 0)
				 (dst-layer-count 0)
				 (dst-offset-x 0)
				 (dst-offset-y 0)
				 (dst-offset-z 0)
				 (width 0)
				 (height 0)
				 (depth 0))
  (make-instance 'image-reslove-ptr
		 :src-aspect-mask src-aspect-mask
		 :src-mip-level src-mip-level
		 :src-base-array-layer src-base-array-layer
		 :src-layer-count src-layer-count
		 :src-offset-x src-offset-x
		 :src-offset-y src-offset-y
		 :src-offset-z src-offset-z
		 :dst-aspect-mask dst-aspect-mask
		 :dst-mip-level dst-mip-level
		 :dst-base-array-layer dst-base-array-layer
		 :dst-layer-count dst-layer-count
		 :dst-offset-x dst-offset-x
		 :dst-offset-y dst-offset-y
		 :dst-offset-z dst-offset-z
		 :width width
		 :height height
		 :depth depth))

(defclass clear-attachment-ptr ()
  ((mask :initarg :mask)
   (color-attachment :initarg :color-attachment)
   (clear-value :initarg :clear-value)))
(defun make-clear-attachment-ptr (mask color-attachment &key (clear-value (make-clear-value-ptr 0.0
												0.0
												0.0
												0.0)))
  (make-instance 'clear-attachment-ptr :mask mask :color-attachment color-attachment :clear-value clear-value))

(defclass clear-rect-ptr (rect2d-ptr)
  ((base-array-layer :initarg base-array-layer)
   (layer-count :initarg layer-count)))
(defun make-clear-rect-ptr (x y width height base-array-layer layer-count)
  (make-instance 'clear-rect-ptr
		 :x x :y y
		 :width width :height height
		 :base-array-layer base-array-layer
		 :layer-count layer-count))

(defclass image-subresource-range-ptr ()
  ((aspect-mask :initarg :aspect-mask)
   (base-mip-level :initarg :bae-mip-level)
   (level-count :initarg :level-count)
   (base-array-layer :initarg :base-array-layer)
   (layer-count :initarg :layer-count)))
(defun make-image-subresource-range-ptr (aspect-mask base-mip-level level-count base-array-layer layer-count)
  (make-instance 'image-subresource-range
		 :aspect-mask aspect-mask
		 :base-mip-level base-mip-level
		 :level-count level-count
		 :base-array-layer base-array-layer
		 :layer-count layer-count))

(defclass clear-depth-stencil-value-ptr ()
  ((depth :initarg :depth)
   (stencil :initarg :stencil)))
(defun make-clear-depth-stencil-value-ptr (depth stencil)
  (make-instance 'clear-depth-stencil-value-ptr :depth depth
						:stencil stencil))

(defclass buffer-copy-ptr ()
  ((src-offset :initarg :src-offset)
   (dst-offset :initarg :dst-offset)
   (size :initarg :size)))
(defun make-buffer-copy-ptr (src-offset dst-offset size)
  (make-instance 'buffer-copy-ptr
		 :src-offset src-offset
		 :dst-offset dst-offset
		 :size size))

(defclass buffer-image-copy-ptr ()
  ((buffer-offset :initarg :buffer-offset)
   (buffer-row-length :initarg :buffer-row-length)
   (buffer-image-height :initarg :buffer-image-height)
   (image-aspect-mask :initarg :image-aspect-mask)
   (image-mip-level :initarg :image-mip-level)
   (image-base-array-layer :initarg :image-base-array-layer)
   (image-layer-count :initarg :image-layer-count)
   (image-x :initarg :image-x)
   (image-y :initarg :image-y)
   (image-z :initarg :image-z)
   (image-width :initarg :image-width)
   (image-height :initarg :image-height)
   (image-depth :initarg :image-depth)))
(defun make-buffer-image-copy-ptr (&key
				     (buffer-offset 0)
				     (buffer-row-length 0)
				     (buffer-image-height 0)
				     (image-aspect-mask 0)
				     (image-mip-level 0)
				     (image-base-array-layer 0)
				     (image-layer-count 0)
				     (image-x 0)
				     (image-y 0)
				     (image-z 0)
				     (image-width 0)
				     (image-height 0)
				     (image-depth 0))
  (make-instance 'buffer-image-copy-ptr
		 :buffer-offset buffer-offset
		 :buffer-row-length buffer-row-length
		 :buffer-image-height buffer-image-height
		 :image-aspect-mask image-aspect-mask
		 :image-mip-level image-mip-level
		 :image-base-array-layer image-base-array-layer
		 :image-layer-count image-layer-count
		 :image-x image-x
		 :image-y image-y
		 :image-z image-z
		 :image-width image-width
		 :image-height image-height
		 :image-depth image-depth))

(defclass image-copy-ptr ()
  ((src-image-aspect-mask :initarg :src-image-aspect-mask)
   (src-image-mip-level :initarg :src-image-mip-level)
   (src-image-base-array-layer :initarg :src-image-base-array-layer)
   (src-image-layer-count :initarg :src-image-layer-count)
   (src-image-x :initarg :src-image-x)
   (src-image-y :initarg :src-image-y)
   (src-image-z :initarg :src-image-z)
   (dst-image-aspect-mask :initarg :dst-image-aspect-mask)
   (dst-image-mip-level :initarg :dst-image-mip-level)
   (dst-image-base-array-layer :initarg :dst-image-base-array-layer)
   (dst-image-layer-count :initarg :dst-image-layer-count)
   (dst-image-x :initarg :dst-image-x)
   (dst-image-y :initarg :dst-image-y)
   (dst-image-z :initarg :dst-image-z)
   (width :initarg :width)
   (height :initarg :height)
   (depth :initarg :depth)))
(defun make-image-copy-ptr (&key
			      (src-image-aspect-mask 0)
			      (src-image-mip-level 0)
			      (src-image-base-array-layer 0)
			      (src-image-layer-count 0)
			      (src-image-x 0)
			      (src-image-y 0)
			      (src-image-z 0)
			      (dst-image-aspect-mask 0)
			      (dst-image-mip-level 0)
			      (dst-image-base-array-layer 0)
			      (dst-image-layer-count 0)
			      (dst-image-x 0)
			      (dst-image-y 0)
			      (dst-image-z 0)
			      (width 0)
			      (height 0)
			      (depth 0))
  (make-instance 'image-copy-ptr
		 :src-image-aspect-mask src-image-aspect-mask
		 :src-image-mip-level src-image-mip-level
		 :src-image-base-array-layer src-image-base-array-layer
		 :src-image-layer-count src-image-layer-count
		 :src-image-x src-image-x
		 :src-image-y src-image-y
		 :src-image-z src-image-z
		 :dst-image-aspect-mask dst-image-aspect-mask
		 :dst-image-mip-level dst-image-mip-level
		 :dst-image-base-array-layer dst-image-base-array-layer
		 :dst-image-layer-count dst-image-layer-count
		 :dst-image-x dst-image-x
		 :dst-image-y dst-image-y
		 :dst-image-z dst-image-z
		 :width width
		 :height height
		 :depth depth))

(defclass descriptor-pool-size-ptr ()
  ((type :initarg :type)
   (count :initarg :count)))
(defun make-descriptor-pool-size-ptr (type count)
  (make-instance 'descriptor-pool-size-ptr :type type :count count))

(defclass descriptor-set-layout-binding-ptr ()
  ((binding :initarg :binding)
   (type :initarg :type)
   (samplers :initarg :samplers)
   (flags :initarg :flags)))
(defun make-descriptor-set-layout-binding-ptr (binding type &key (samplers nil) (flags 0))
  (make-instance 'descriptor-set-layout-binding-ptr :binding binding
						    :type type
						    :samplers samplers
						    :flags flags))

(defclass swapchain-create-info-ptr ()
  ((next :initarg :next)
   (flags :initarg :flags)
   (min-image-count :initarg :min-image-count)
   (format :initarg :format)
   (color-space :initarg :color-space)
   (image-width :initarg :image-width)
   (image-height :initarg :image-height)
   (image-array-layers :initarg :image-array-layers)
   (usage :initarg :usage)
   (sharing-mode :initarg :sharing-mode)
   (indices :initarg :indices)
   (pretransform :initarg :pretransform)
   (composite-alpha :initarg :composite-alpha)
   (present-mode :initarg :present-mode)
   (clipped :initarg :clipped)
   (old-swapchain :initarg :old-swapchain)
   (surface :initarg :surface)))
(defun make-swapchain-create-info-ptr (surface &key
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
						 (old-swapchain +vk-null-ptr+))
  (make-instance 'swapchain-create-info-ptr
		 :next next
		 :flags flags
		 :min-image-count min-image-count
		 :format format
		 :color-space color-space
		 :image-width image-width
		 :image-height image-height
		 :image-array-layers image-array-layers
		 :usage usage
		 :sharing-mode sharing-mode
		 :indices indices
		 :pretransform pretransform
		 :composite-alpha composite-alpha
		 :present-mode present-mode
		 :clipped clipped
		 :old-swapchain old-swapchain
		 :surface surface))

(defclass mapped-memory-range-ptr ()
  ((next :initarg :next)
   (memory :initarg :memory)
   (offset :initarg :offset)
   (size :initarg :size)))
(defun make-mapped-memory-range-ptr (memory offset size &key (next +vk-null-ptr+))
  (make-instance 'mapped-memory-range-ptr
		 :next next
		 :memory memory
		 :offset offset
		 :size size))

(defclass sparse-memory-bind-ptr ()
  ((offset :initarg :offset)
   (size :initarg :size)
   (memory :initarg :memory)
   (memory-offset :initarg :memory-offset)
   (flags :initarg :flags)))
(defun make-sparse-memory-bind-ptr (offset size memory memory-offset flags)
  (make-instance 'sparse-memory-bind-ptr :offset offset :siez size :memory memory :memory-offset memory-offset :flags flags))

(defclass sparse-buffer-memory-bind-ptr ()
  ((buffer :initarg :buffer)
   (binds :initarg :binds)))
(defun make-sparse-buffer-memory-bind-ptr (buffer binds)
  (make-instance 'sparse-buffer-memory-bind-ptr :buffer buffer :binds binds))

(defclass sparse-image-opaque-memory-bind-ptr ()
  ((image :initarg :image)
   (binds :initarg :binds)))
(defun make-sparse-image-opaque-memory-bind-ptr (image binds)
  (make-instance 'sparse-image-opaque-memory-bind-ptr :image image :binds binds))

(defclass sparse-image-memory-bind-ptr ()
  ((aspect-mask :initarg :aspect-mask)
   (mip-level :initarg :mip-level)
   (array-layer :initarg :array-layer)
   (x :initarg :x)
   (y :initarg :y)
   (z :initarg :z)
   (width :initarg :width)
   (height :initarg :height)
   (depth :initarg :depth)
   (memory :initarg :memory)
   (offset :initarg :offset)
   (flags :initarg :flags)))
(defun make-sparse-image-memory-bind-ptr (&key
					    (aspect-mask 0)
					    (mip-level 0)
					    (array-layer 0)
					    (x 0)
					    (y 0)
					    (z 0)
					    (width 0)
					    (height 0)
					    (depth 0)
					    (memory +vk-null-ptr+)
					    (offset 0)
					    (flags 0))
  (make-instance 'sparse-image-memory-bind-ptr
		 :aspect-mask aspect-mask
		 :mip-level mip-level
		 :array-layer array-layer
		 :x x
		 :y y
		 :z z
		 :width width
		 :height height
		 :depth depth
		 :memory memory
		 :offset offset
		 :flags flags))

(defclass sparse-image-memory-bind-info-ptr ()
  ((image :initarg :image)
   (binds :initarg :binds)))
(defun make-sparse-image-memory-bind-info-ptr (image binds)
  (make-instance 'sparse-image-memory-bind-info-ptr :image image :binds binds))

(defclass bind-sparse-info-ptr ()
  ((next :initarg :next)
   (wait-semaphores  :initarg :wait-semaphores)
   (buffer-binds :initarg :buffer-binds)
   (image-opaque-binds :initarg :image-opaque-binds)
   (image-binds :initarg :image-binds)
   (single-semaphores :initarg :single-semaphores)))
(defun make-bind-sparse-info-ptr (wait-semaphores buffer-binds image-opaque-binds image-binds single-semaphores &key (next +vk-null-ptr+))
  (make-instance 'bind-sparse-info-ptr
		 :next next
		 :wait-semaphores wait-semaphores
		 :buffer-binds buffer-binds
		 :image-opaque-binds image-opaque-binds
		 :image-binds image-binds
		 :single-semaphores single-semaphores))

(defclass desciptor-image-ptr ()
  ((sampler :initarg :sampler)
   (image-view :initarg :image-views)
   (image-layout :initarg :image-layout)))
(defun make-descriptor-image-ptr (sampler image-view image-layout)
  (make-instance 'descriptor-image-ptr :sampler sampler :imaeg-view image-view :image-layout image-layout))

(defclass descriptor-buffer-ptr ()
  ((buffer :initarg :buffer)
   (offset :initarg :offset)
   (range :initarg :range)))
(defun make-descriptor-buffer-ptr (buffer offset range)
  (make-instance 'descriptor-buffer-ptr :buffer buffer :offset offset :range range))

(defclass write-descriptor-set-ptr ()
  ((next :initarg :next)
   (dst-set :initarg :dst-set)
   (dst-binding :initarg :dst-binding)
   (dst-array-element :initarg :dst-array-element)
   (type :initarg :type)
   (image-info :initarg :image-info)
   (buffer-info :initarg :buffer-info)
   (texel-buffer-view :initarg :texel-buffer-view)))
(defun make-write-descriptor-set-ptr (dst-set dst-binding dst-array-element type image-info-ptr buffer-info-ptr texel-buffer-view &key (next +vk-null-ptr+))
  (make-instance 'write-descriptor-set-ptr :next next
					   :dst-set dst-set
					   :dst-binding dst-binding
					   :dst-array-element dst-array-element
					   :type type
					   :image-info image-info-ptr
					   :buffer-info buffer-info-ptr
					   :texel-buffer-view texel-buffer-view))

(defclass copy-descriptor-set-ptr ()
  ((next :initarg :next)
   (src-set :initarg :src-set)
   (src-binding :initarg :src-binding)
   (src-array-element :initarg :src-array-element)
   (dst-set :initarg :dst-set)
   (dst-binding :initarg :dst-binding)
   (dst-array-element :initarg :dst-array-element)
   (descriptor-count :initarg :descriptor-count)))
(defun make-copy-descriptor-set-ptr (src-set src-binding src-array-element dst-set dst-binding dst-array-element descriptor-count &key (next +vk-null-ptr+))
  (make-instance 'copy-descriptor-set-ptr
		 :next next
		 :src-set src-set
		 :src-binding src-binding
		 :src-array-element src-array-element
		 :dst-set dst-set
		 :dst-binding dst-binding
		 :dst-array-element dst-array-element
		 :descriptor-count descriptor-count))
