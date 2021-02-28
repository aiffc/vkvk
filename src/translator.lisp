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

(define-foreign-type s-layer-properties ()
  ()
  (:actual-type :struct VkLayerProperties)
  (:simple-parser layer-properties))
(defmethod translate-from-foreign (ptr (type s-layer-properties))
  (list :description
	(foreign-string-to-lisp (foreign-slot-value ptr '(:struct VkLayerProperties) :description))
	:implementationVersion
	(show-vulkan-version (foreign-slot-value ptr '(:struct VkLayerProperties) :implementationVersion))
	:specVersion
	(show-vulkan-version (foreign-slot-value ptr '(:struct VkLayerProperties) :specVersion))
	:layerName
	(foreign-string-to-lisp (foreign-slot-value ptr '(:struct VkLayerProperties) :layerName))))

(define-foreign-type s-extension-properties ()
  ()
  (:actual-type :struct VkExtensionProperties)
  (:simple-parser extension-properties))
(defmethod translate-from-foreign (ptr (type s-extension-properties))
  (list :extensionName
	(foreign-string-to-lisp (foreign-slot-value ptr '(:struct VkExtensionProperties) :extensionName))
	:specVersion
	(show-vulkan-version (foreign-slot-value ptr '(:struct VkExtensionProperties) :specVersion))))

(define-foreign-type s-physical-device-memory-properties ()
  ()
  (:actual-type :struct VkPhysicalDeviceMemoryProperties)
  (:simple-parser physical-device-memory-properties))
(defmethod translate-from-foreign (ptr (type s-physical-device-memory-properties))
  (let ((type-count (foreign-slot-value ptr '(:struct VkPhysicalDeviceMemoryProperties) :memoryTypeCount))
	(heap-count (foreign-slot-value ptr '(:struct VkPhysicalDeviceMemoryProperties) :memoryHeapCount))
	(p-types (foreign-slot-pointer ptr '(:struct VkPhysicalDeviceMemoryProperties) :memoryTypes))
	(p-heaps (foreign-slot-pointer ptr '(:struct VkPhysicalDeviceMemoryProperties) :memoryHeaps)))
    (list :memory-type-count type-count
	  :memory-types (ptr->list p-types '(:struct VkMemoryType) type-count)
	  :memory-heap-count heap-count
	  :memory-heaps (ptr->list p-heaps '(:struct VkMemoryHeap) heap-count))))

(define-foreign-type s-viewport ()
  ()
  (:actual-type :struct VkViewport)
  (:simple-parser viewport))
(defmethod translate-into-foreign-memory (class-val (type s-viewport) ptr)
  (with-slots (x y width height min-depth max-depth) class-val
    (setf (mem-ref ptr '(:struct VkViewport))
	  `(:x ,x :y ,y :width ,width :height ,height :min-depth ,min-depth :max-depth ,max-depth))))

(define-foreign-type s-rect2d ()
  ()
  (:actual-type :struct VkRect2D)
  (:simple-parser rect2d))
(defmethod translate-into-foreign-memory (class-val (type s-rect2d) ptr)
  (with-slots (x y width height) class-val
    (let ((p-offset (foreign-slot-pointer ptr '(:struct VkRect2D) :offset))
	  (p-extent (foreign-slot-pointer ptr '(:struct VkRect2D) :extent)))
      (setf (mem-ref p-offset '(:struct VkOffset2D))
	    `(list :x ,x :y ,y)
	    (mem-ref p-extent '(:struct VkExtent2D))
	    `(list :width ,width :height ,height)))))

(define-foreign-type u-clear-value ()
  ()
  (:actual-type :union VkClearValue)
  (:simple-parser clear-value))
(defmethod translate-into-foreign-memory (class-val (type u-clear-value) ptr)
  (with-slots (r g b a) class-val
    (setf (mem-aref :float ptr 0) r
	  (mem-aref :float ptr 1) g 
	  (mem-aref :float ptr 2) b
	  (mem-aref :float ptr 3) a)))

(define-foreign-type s-device-queue-create-info ()
  ()
  (:actual-type :struct VkDeviceQueueCreateInfo)
  (:simple-parser device-queue-create-info))
(defmethod translate-into-foreign-memory (class-val (type s-device-queue-create-info) ptr)
  (with-slots (next flags family-index properties) class-val
    (let* ((properties-count (length properties))
	   (p-properties (foreign-alloc :float :count properties-count :initial-contents properties)))
      (setf (mem-ref ptr '(:struct VkDeviceQueueCreateInfo))
	    `(:sType ,+structure-type-device-queue-create-info+
	      :pNext ,next
	      :flags ,flags
	      :queueFamilyIndex ,family-index
	      :queueCount ,properties-count
	      :pQueuePriorities ,p-properties)))))
(defun free-device-queue-ptr (ptr count)
  (loop for i from 0 below count
	for p = (mem-aptr ptr '(:struct VkDeviceQueueCreateInfo) i)
	for slot-p = (foreign-slot-value p '(:struct VkDeviceQueueCreateInfo) :pQueuePriorities)
	unless (null-pointer-p slot-p)
	  do (foreign-free slot-p)) 
  (foreign-free ptr))

;; ;; for pipeline
(define-foreign-type s-specialization-map-entry ()
  ()
  (:actual-type :struct VkSpecializationMapEntry)
  (:simple-parser specialization-map-entry))
(defmethod translate-into-foreign-memory (class-val (type s-specialization-map-entry) ptr)
  (with-slots (id offset size) class-val
    (setf (mem-ref ptr '(:struct VkSpecializationMapEntry))
	`(:constantID id
	  :offset offset
	  :size size))))

(define-foreign-type s-specialization ()
  ()
  (:actual-type :struct VkSpecializationInfo)
  (:simple-parser specialization))
(defmethod translate-into-foreign-memory (class-val (type s-specialization) ptr)
  (with-slots (map-entries size p-data) class-val
    (let* ((map-count (length map-entries))
	   (map-ptr (foreign-alloc 's-specialization-map-entry :count map-count
							       :initial-contents map-entries)))
      (setf (mem-ref ptr '(:struct VkSpecializationInfo))
	    `(:mapEntryCount ,map-count
	      :pMapEntries ,map-ptr
	      :dataSize ,size
	      :pData ,p-data)))))

(define-foreign-type s-pipeline-shader-state ()
  ()
  (:actual-type :struct VkPipelineShaderStageCreateInfo)
  (:simple-parser pipeline-shader-stage))
(defmethod translate-into-foreign-memory (class-val (type s-pipeline-shader-state) ptr)
  (with-slots (next flags stage module name specialization) class-val
    (let ((p-name (foreign-string-alloc name))
	  (p-specialization (set-null-ptr specialization
					  (foreign-alloc 'specialization))))
      (when specialization
	(setf (mem-ref p-specialization 'specialization) specialization))
      (setf (mem-ref ptr '(:struct VkPipelineShaderStageCreateInfo))
	    `(:sType ,+structure-type-pipeline-shader-stage-create-info+
	      :pNext ,next
	      :flags ,flags
	      :stage ,stage
	      :module ,module
	      :pName ,p-name
	      :pSpecializationInfo ,p-specialization)))))
(defun free-shader-stage (ptr)
  (let ((p-entries (foreign-slot-value ptr '(:struct VkSpecializationInfo) :pMapEntries)))
    (free-if-not-null p-entries)
    (free-if-not-null p-entries)))
(defun free-pipeline-shader-stage (pipeline-ptr)
  (let* ((p-stage (foreign-slot-value pipeline-ptr '(:struct VkGraphicsPipelineCreateInfo) :pStages))
	 (count (foreign-slot-value pipeline-ptr '(:struct VkGraphicsPipelineCreateInfo) :stageCount)))
    (unless (null-pointer-p p-stage)
      (loop for i from 0 below count
	    for p-s = (mem-aptr p-stage '(:struct VkPipelineShaderStageCreateInfo) i)
	    for str = (foreign-slot-value p-s '(:struct VkPipelineShaderStageCreateInfo) :pName)
	    for p-sp-info = (foreign-slot-value p-s '(:struct VkPipelineShaderStageCreateInfo) :pSpecializationInfo)
	    unless (null-pointer-p str)
	      do (foreign-string-free str)
	    unless (null-pointer-p p-sp-info)
	      do (free-shader-stage p-sp-info))
      (foreign-free p-stage))))

(define-foreign-type s-vertex-input-binding-descriptor ()
  ()
  (:actual-type :struct VkVertexInputBindingDescription)
  (:simple-parser vertex-input-binding-descriptor))
(defmethod translate-into-foreign-memory (class-val (type s-vertex-input-binding-descriptor) ptr)
  (with-slots (binding stride rate) class-val
    (setf (mem-ref ptr '(:struct VkVertexInputBindingDescription))
	  `(:binding ,binding
	    :stride ,stride
	    :inputRate ,rate))))

(define-foreign-type s-vertex-input-attribute-desription ()
  ()
  (:actual-type :struct VkVertexInputAttributeDescription)
  (:simple-parser vertex-input-attribute-desription))
(defmethod translate-into-foreign-memory (class-val (type s-vertex-input-attribute-desription) ptr)
  (with-slots (location binding format offset) class-val
    (setf (mem-ref ptr '(:struct VkVertexInputAttributeDescription))
	  `(:location ,location
	    :binding ,binding
	    :format ,format
	    :offset ,offset))))

(define-foreign-type s-pipeline-vertex-input-state ()
  ()
  (:actual-type :struct VkPipelineVertexInputStateCreateInfo)
  (:simple-parser pipeline-vertex-input-state))
(defmethod translate-into-foreign-memory (class-val (type s-pipeline-vertex-input-state) ptr)
  (with-slots (next flags binding-ptr attribute-ptr) class-val
    (let* ((binding-count (length binding-ptr))
	   (attribute-count (length attribute-ptr))
	   (p-binding (set-null-ptr binding-ptr (foreign-alloc 'vertex-input-binding-descriptor
							       :count binding-count
							       :initial-contents binding-ptr)))
	   (p-attribute (set-null-ptr attribute-ptr
				      (foreign-alloc 'vertex-input-attribute-desription
						     :count attribute-count
						     :initial-contents attribute-ptr))))
      (setf (mem-ref ptr '(:struct VkPipelineVertexInputStateCreateInfo))
	    `(:sType ,+structure-type-pipeline-vertex-input-state-create-info+
	      :pNext ,next
	      :flags ,flags
	      :vertexBindingDescriptionCount ,binding-count
	      :pVertexBindingDescriptions ,p-binding
	      :vertexAttributeDescriptionCount ,attribute-count
	      :pVertexAttributeDescriptions ,p-attribute)))))
(defun free-pipeline-vertex-input-state (pipeline-ptr)
  (let* ((p-info (foreign-slot-value pipeline-ptr '(:struct VkGraphicsPipelineCreateInfo) :pVertexInputState))
	 (p-binding (foreign-slot-value p-info '(:struct VkPipelineVertexInputStateCreateInfo) :pVertexBindingDescriptions))
	 (p-attribute (foreign-slot-value p-info '(:struct VkPipelineVertexInputStateCreateInfo) :pVertexAttributeDescriptions)))
    (free-if-not-null p-binding)
    (free-if-not-null p-attribute)
    (free-if-not-null p-info)))

(define-foreign-type s-pipeline-input-assembly-state ()
  ()
  (:actual-type :struct VkPipelineInputAssemblyStateCreateInfo)
  (:simple-parser pipeline-input-assembly-state))
(defmethod translate-into-foreign-memory (class-val (type s-pipeline-input-assembly-state) ptr)
  (with-slots (next flags topology restart-enable) class-val
    `( :sType ,+structure-type-pipeline-input-assembly-state-create-info+
       :pNext ,next
       :flags ,flags
       :topology ,topology
       :primitiveRestartEnable ,restart-enable)))
(defun free-pipeline-input-assembly-state (pipeline-ptr)
  (let ((p (foreign-slot-value pipeline-ptr '(:struct VkGraphicsPipelineCreateInfo) :pInputAssemblyState)))
    (free-if-not-null p)))

(define-foreign-type s-pipeline-tessellation-state ()
  ()
  (:actual-type :struct VkPipelineTessellationStateCreateInfo)
  (:simple-parser pipeline-tessellation-state-ptr))
(defmethod translate-into-foreign-memory (class-val (type s-pipeline-tessellation-state) ptr)
  (with-slots (next flags points) class-val
    (setf (mem-ref ptr '(:struct VkPipelineTessellationStateCreateInfo))
	  `(:sType ,+structure-type-pipeline-tessellation-state-create-info+
	    :pNext ,next
	    :flags ,flags
	    :patchControlPoints ,points))))
(defun free-pipeline-tessellation-state (pipeline-ptr)
  (let ((p (foreign-slot-value pipeline-ptr '(:struct VkGraphicsPipelineCreateInfo) :pTessellationState)))
    (free-if-not-null p)))

(define-foreign-type s-pipeline-viewport-state ()
  ()
  (:actual-type :struct VkPipelineViewportStateCreateInfo)
  (:simple-parser pipeline-viewport-state))
(defmethod translate-into-foreign-memory (class-val (type s-pipeline-viewport-state) ptr)
  (with-slots (next flags viewports scissors) class-val
    (let* ((viewport-count (length viewports))
	   (scissor-count (length scissors))
	   (viewport-ptr (foreign-alloc 'viewport :count viewport-count :initial-contents viewports))
	   (scissor-ptr (foreign-alloc 'rect2d :count scissor-count :initial-contents scissors)))
      `(:sType ,+structure-type-pipeline-viewport-state-create-info+
	:pNext ,next
	:flags ,flags
	:viewportCount ,viewport-count
	:pViewports ,viewport-ptr
	:scissorCount ,scissor-count
	:pScissors ,scissor-ptr))))
(defun free-pipeline-viewport-state (pipeline-ptr)
  (let* ((p-state (foreign-slot-value pipeline-ptr '(:struct VkGraphicsPipelineCreateInfo) :pViewportState))
	 (p-viewport (foreign-slot-value p-state '(:struct VkPipelineViewportStateCreateInfo) :pViewports))
	 (p-rect (foreign-slot-value p-state '(:struct VkPipelineViewportStateCreateInfo) :pScissors)))
    (free-if-not-null p-viewport)
    (free-if-not-null p-rect)
    (free-if-not-null p-state)))

(define-foreign-type s-pipeline-rasterization-state ()
  ()
  (:actual-type :struct VkPipelineRasterizationStateCreateInfo)
  (:simple-parser pipeline-rasterization-state))
(defmethod translate-into-foreign-memory (class-val (type s-pipeline-rasterization-state) ptr)
  (with-slots (next flags clamp-enable discard-enable polygon-mode cull-mode front-face bias-enable constant-factor clamp slope-factor line-width) class-val
    (setf (mem-ref ptr '(:struct VkPipelineRasterizationStateCreateInfo))
	`(:sType ,+structure-type-pipeline-rasterization-state-create-info+
	  :pNext ,next
	  :flags ,flags
	  :depthClampEnable ,clamp-enable
	  :rasterizerDiscardEnable ,discard-enable
	  :polygonMode ,polygon-mode
	  :cullMode ,cull-mode
	  :frontFace ,front-face
	  :depthBiasEnable ,bias-enable
	  :depthBiasConstantFactor ,constant-factor
	  :depthBiasClamp ,clamp 
	  :depthBiasSlopeFactor ,slope-factor
	  :lineWidth ,line-width))))
(defun free-pipeline-rasterization-state (pipeline-ptr)
  (let ((p (foreign-slot-value pipeline-ptr '(:struct VkGraphicsPipelineCreateInfo) :pRasterizationState)))
    (free-if-not-null p)))

(define-foreign-type s-pipeline-multisample-state ()
  ()
  (:actual-type :struct VkPipelineMultisampleStateCreateInfo)
  (:simple-parser pipeline-multisample-state))
(defmethod translate-into-foreign-memory (class-val (type s-pipeline-multisample-state) ptr)
  (with-slots (next flags samples sample-shading-enable min-sample-shading sample-mask alpha-to-coverage-enable alpha-to-one-enable) class-val
    (let ((p-sample-mask (set-null-ptr sample-mask
				       (foreign-alloc 'VkSampleMask :count (length sample-mask)
								    :initial-contents sample-mask))))
      (setf (mem-ref ptr '(:struct VkPipelineMultisampleStateCreateInfo))
	    `(:sType ,+structure-type-pipeline-multisample-state-create-info+
	      :pNext ,next
	      :flags ,flags
	      :rasterizationSamples ,samples
	      :sampleShadingEnable ,sample-shading-enable
	      :minSampleShading ,min-sample-shading
	      :pSampleMask ,p-sample-mask
	      :alphaToCoverageEnable ,alpha-to-coverage-enable
	      :alphaToOneEnable ,alpha-to-one-enable)))))
(defun free-pipeline-multisample-state (pipeline-ptr)
  (let* ((p (foreign-slot-value pipeline-ptr '(:struct VkGraphicsPipelineCreateInfo) :pMultisampleState))
	 (p-sample-mask (foreign-slot-value p '(:struct VkPipelineMultisampleStateCreateInfo) :pSampleMask)))
    (free-if-not-null p-sample-mask)
    (free-if-not-null p)))

(define-foreign-type s-pipeline-depth-stencil-state ()
  ()
  (:actual-type :struct VkPipelineDepthStencilStateCreateInfo)
  (:simple-parser pipeline-depth-stencil-state))
(defmethod translate-into-foreign-memory (class-val (type s-pipeline-depth-stencil-state) ptr)
  (with-slots (next 
	       flags 
	       depth-test-enable 
	       depth-write-enable 
	       compare-op 
	       depth-bounds-test-enable 
	       stencil-test-enable 
	       front-fail-op 
	       front-pass-op 
	       front-depth-fail-op 
	       front-compare-op 
	       front-compare-mask 
	       front-write-mask 
	       front-reference 
	       back-fail-op 
	       back-pass-op 
	       back-depth-fail-op 
	       back-compare-op 
	       back-compare-mask 
	       back-write-mask 
	       back-reference 
	       min-depth-bounds 
	       max-depth-bounds) class-val
    (setf (mem-ref ptr '(:struct VkPipelineDepthStencilStateCreateInfo))
	  `(:sType ,+structure-type-pipeline-depth-stencil-state-create-info+
	    :pNext ,next
	    :flags ,flags
	    :depthTestEnable ,depth-test-enable
	    :depthWriteEnable ,depth-write-enable
	    :depthCompareOp ,compare-op
	    :depthBoundsTestEnable ,depth-bounds-test-enable
	    :stencilTestEnable ,stencil-test-enable
	    :minDepthBounds ,min-depth-bounds
	    :maxDepthBounds ,max-depth-bounds)
	  (mem-ref (foreign-slot-pointer ptr '(:struct VkPipelineDepthStencilStateCreateInfo) :front)
		   '(:struct VkStencilOpState))
	  `(:failOp ,front-fail-op
	    :passOp ,front-pass-op
	    :depthFailOp ,front-depth-fail-op
	    :compareOp ,front-compare-op
	    :compareMask ,front-compare-mask
	    :writeMask ,front-write-mask
	    :reference ,front-reference)
	  (mem-ref (foreign-slot-pointer ptr '(:struct VkPipelineDepthStencilStateCreateInfo) :back)
		   '(:struct VkStencilOpState))
	  `(:failOp ,back-fail-op
	    :passOp ,back-pass-op
	    :depthFailOp ,back-depth-fail-op
	    :compareOp ,back-compare-op
	    :compareMask ,back-compare-mask
	    :writeMask ,back-write-mask
	    :reference ,back-reference))))
(defun free-pipeline-depth-stencil-state (pipeline-ptr)
  (let* ((p (foreign-slot-value pipeline-ptr '(:struct VkGraphicsPipelineCreateInfo) :pDepthStencilState)))
    (free-if-not-null p)))

(define-foreign-type s-pipeline-color-blend-attachment-state ()
  ()
  (:actual-type :struct VkPipelineColorBlendAttachmentState)
  (:simple-parser pipeline-color-blend-attachment-state))
(defmethod translate-into-foreign-memory (class-val (type s-pipeline-color-blend-attachment-state) ptr)
  (with-slots (blend-enable 
	       src-color-blend 
	       dst-color-blend 
	       color-blend-op 
	       src-alpha-blend 
	       dst-alpha-blend 
	       alpha-blend-op 
	       write-mask) class-val
    (setf (mem-aref ptr '(:struct VkPipelineColorBlendAttachmentState))
	  `(:blendEnable ,blend-enable
	    :srcColorBlendFactor ,src-color-blend
	    :dstColorBlendFactor ,dst-color-blend
	    :colorBlendOp ,color-blend-op
	    :srcAlphaBlendFactor ,src-alpha-blend
	    :dstAlphaBlendFactor ,dst-alpha-blend
	    :alphaBlendOp ,alpha-blend-op
	    :colorWriteMask ,write-mask))))

(define-foreign-type s-pipeline-color-blend-state ()
  ()
  (:actual-type :struct VkPipelineColorBlendStateCreateInfo)
  (:simple-parser pipeline-color-blend-state))
(defmethod translate-into-foreign-memory (class-val (type s-pipeline-color-blend-state) ptr)
  (with-slots (next flags logic-op-enable logic-op attachments r g b a) class-val
    (let* ((p-constants (foreign-slot-pointer ptr '(:struct VkPipelineColorBlendStateCreateInfo) :blendConstants))
	   (attachment-count (length attachments))
	   (p-attachments (set-null-ptr attachments
					(foreign-alloc 'pipeline-color-blend-attachment-state :count attachment-count
											      :initial-contents attachments))))
      (setf (mem-ref ptr '(:struct VkPipelineColorBlendStateCreateInfo))
	    `(:sType ,+structure-type-pipeline-color-blend-state-create-info+
	      :pNext ,next
	      :flags ,flags
	      :logicOpEnable ,logic-op-enable
	      :logicOp ,logic-op
	      :attachmentCount ,attachment-count
	      :pAttachments ,p-attachments)
	    (mem-aref p-constants :float 0) r
	    (mem-aref p-constants :float 1) g
	    (mem-aref p-constants :float 2) b
	    (mem-aref p-constants :float 3) a))))
(defun free-pipeline-color-blend-attachment-state (pipeline-ptr)
  (let* ((p-state (foreign-slot-value pipeline-ptr '(:struct VkGraphicsPipelineCreateInfo) :pColorBlendState))
	 (p-attachments (foreign-slot-value p-state '(:struct VkPipelineColorBlendStateCreateInfo) :pAttachments)))
    (free-if-not-null p-attachments)
    (free-if-not-null p-state)))

(define-foreign-type s-pipeline-dynamic-state ()
  ()
  (:actual-type :struct VkPipelineDynamicStateCreateInfo)
  (:simple-parser pipeline-dynamic-state))
(defmethod translate-into-foreign-memory (class-val (type s-pipeline-dynamic-state) ptr)
  (with-slots (next flags states) class-val
    (let* ((state-count (length states))
	   (p-state (set-null-ptr states
				  (foreign-alloc 'VkDynamicState :count state-count
								 :initial-contents states))))
      (setf (mem-ref ptr '(:struct VkPipelineDynamicStateCreateInfo))
	    `(:sType ,+structure-type-pipeline-dynamic-state-create-info+
	      :pNext ,next
	      :flags ,flags
	      :dynamicStateCount ,state-count
	      :pDynamicStates ,p-state)))))
(defun free-pipeline-dynamic-state (pipeline-ptr)
  (let* ((p-state (foreign-slot-value pipeline-ptr '(:struct VkGraphicsPipelineCreateInfo) :pDynamicState))
	 (p-dynamic-states (foreign-slot-value p-state '(:struct VkPipelineDynamicStateCreateInfo) :pDynamicStates)))
    (free-if-not-null p-dynamic-states)
    (free-if-not-null p-state)))

(define-foreign-type s-graphics-pipeline ()
  ()
  (:actual-type :struct VkGraphicsPipelineCreateInfo)
  (:simple-parser graphics-pipeline))
(defmethod translate-into-foreign-memory (class-val (type s-graphics-pipeline) ptr)
  (with-slots (next
	       flags
	       shader-stage
	       vertex-state
	       assembly-state
	       tessellation-state
	       viewport-state
	       rasterization-state
	       multisample-state
	       depth-stencil-state
	       color-blend-state
	       dynamic-state
	       layout
	       render-pass
	       subpass
	       handle
	       index) class-val
    (let* ((shader-stage-count (length shader-stage))
	   (p-shader-stage (set-null-ptr shader-stage (foreign-alloc 'pipeline-shader-stage :count shader-stage-count
								     :initial-contents shader-stage)))
	   (p-vertex-state (set-null-ptr vertex-state (foreign-alloc 'pipeline-vertex-input-state)))
	   (p-assembly-state (set-null-ptr assembly-state (foreign-alloc 'pipeline-input-assembly-state)))
	   (p-tessellation-state (set-null-ptr tessellation-state (foreign-alloc 'pipeline-tessellation-state-ptr)))
	   (p-viewport-state (set-null-ptr viewport-state (foreign-alloc 'pipeline-viewport-state)))
	   (p-rasterization-state (set-null-ptr rasterization-state (foreign-alloc 'pipeline-rasterization-state)))
	   (p-multisample-state (set-null-ptr multisample-state (foreign-alloc 'pipeline-multisample-state)))
	   (p-depth-stencil-state (set-null-ptr depth-stencil-state (foreign-alloc 'pipeline-depth-stencil-state)))
	   (p-color-blend-state (set-null-ptr color-blend-state (foreign-alloc 'pipeline-color-blend-state)))
	   (p-dynamic-state (set-null-ptr dynamic-state (foreign-alloc 'pipeline-dynamic-state))))

      (when vertex-state
	(setf (mem-ref p-vertex-state 'pipeline-vertex-input-state) vertex-state)) 
      (when assembly-state
	(setf (mem-ref p-assembly-state 'pipeline-input-assembly-state) assembly-state))
      (when tessellation-state
	(setf (mem-ref p-tessellation-state 'pipeline-tessellation-state-ptr) tessellation-state))
      (when viewport-state
	(setf (mem-ref p-viewport-state 'pipeline-viewport-state) viewport-state))
      (when rasterization-state
	(setf (mem-ref p-rasterization-state 'pipeline-rasterization-state) rasterization-state))
      (when multisample-state
	(setf (mem-ref p-multisample-state 'pipeline-multisample-state) multisample-state))
      (when depth-stencil-state
	(setf (mem-ref p-depth-stencil-state 'pipeline-depth-stencil-state) depth-stencil-state))
      (when color-blend-state
	(setf (mem-ref p-color-blend-state 'pipeline-color-blend-state) color-blend-state))
      (when dynamic-state
	(setf (mem-ref p-dynamic-state 'pipeline-dynamic-state) dynamic-state))

      (setf (mem-ref ptr '(:struct VkGraphicsPipelineCreateInfo))
	    `(:sType ,+structure-type-graphics-pipeline-create-info+
	      :pNext ,next 
	      :flags ,flags
	      :stageCount ,shader-stage-count
	      :pStages ,p-shader-stage
	      :pVertexInputState ,p-vertex-state
	      :pInputAssemblyState ,p-assembly-state
	      :pTessellationState ,p-tessellation-state
	      :pViewportState ,p-viewport-state
	      :pRasterizationState ,p-rasterization-state
	      :pMultisampleState ,p-multisample-state
	      :pDepthStencilState ,p-depth-stencil-state
	      :pColorBlendState ,p-color-blend-state
	      :pDynamicState ,p-dynamic-state
	      :layout ,layout
	      :renderPass ,render-pass
	      :subpass ,subpass
	      :basePipelineHandle ,handle
	      :basePipelineIndex ,index)))))
(defun free-graphics-pipeline (pipeline)
  (free-pipeline-shader-stage pipeline)
  (free-pipeline-vertex-input-state pipeline)
  (free-pipeline-input-assembly-state pipeline)
  (free-pipeline-viewport-state pipeline)
  (free-pipeline-tessellation-state pipeline)
  (free-pipeline-rasterization-state pipeline)
  (free-pipeline-multisample-state pipeline)
  (free-pipeline-depth-stencil-state pipeline)
  (free-pipeline-color-blend-attachment-state pipeline)
  (free-pipeline-dynamic-state pipeline)
  (foreign-free pipeline))

(define-foreign-type s-compute-pipeline ()
  ()
  (:actual-type :struct VkComputePipelineCreateInfo)
  (:simple-parser compute-pipeline))
(defmethod translate-into-foreign-memory (class-val (type s-compute-pipeline) ptr)
  (with-slots (next flags stage layout handle index) class-val
    (setf (mem-ref ptr '(:struct VkComputePipelineCreateInfo))
	  `(:sType ,+structure-type-compute-pipeline-create-info+
	    :pNext ,next
	    :flags ,flags
	    :layout ,layout
	    :basePipelineHandle ,handle
	    :basePipelineIndex ,index)
	  (mem-ref (foreign-slot-pointer ptr '(:struct VkComputePipelineCreateInfo) :stage) 'pipeline-shader-stage)
	  stage)))
(defun free-compute-pipeline (pipeline-ptr)
  (let* ((p-stage (foreign-slot-pointer pipeline-ptr '(:struct VkComputePipelineCreateInfo) :stage))
	 (p-str (foreign-slot-value p-stage '(:struct VkPipelineShaderStageCreateInfo) :pName))
	 (p-spec (foreign-slot-value p-stage '(:struct VkPipelineShaderStageCreateInfo) :pSpecializationInfo)))
    (foreign-string-free p-str)
    (free-if-not-null p-spec)
    (foreign-free pipeline-ptr)))

(define-foreign-type s-push-constant-range ()
  ()
  (:actual-type :struct VkPushConstantRange)
  (:simple-parser push-constant-range))
(defmethod translate-into-foreign-memory (class-val (type s-push-constant-range) ptr)
  (with-slots (flags offset size) class-val
    (setf (mem-ref ptr '(:struct VkPushConstantRange))
	  `(:stageFlags ,flags
	    :offset offset
	    :size ,size))))

(define-foreign-type attachment-description ()
  ()
  (:actual-type :struct VkAttachmentDescription)
  (:simple-parser attachment-description))
(defmethod translate-into-foreign-memory (class-val (type attachment-description) ptr)
  (with-slots (flags
	       format
	       samples
	       load-op
	       store-op
	       stencil-load-op
	       stencil-store-op
	       initial-layout
	       final-layout) class-val
    (setf (mem-ref ptr '(:struct VkAttachmentDescription))
	  `(:flags ,flags
	    :format ,format
	    :samples ,samples
	    :loadOp ,load-op
	    :storeOp ,store-op
	    :stencilLoadOp ,stencil-load-op
	    :stencilStoreOp ,stencil-store-op
	    :initialLayout ,initial-layout
	    :finalLayout ,final-layout))))

(define-foreign-type s-subpass-dependency-description ()
  ()
  (:actual-type :struct VkSubpassDependency)
  (:simple-parser subpass-dependency-description))
(defmethod translate-into-foreign-memory (class-val (type s-subpass-dependency-description) ptr)
  (with-slots (src-subpass dst-subpass
	       src-stage-mask dst-stage-mask
	       src-access-mask dst-access-mask
	       flags) class-val
    (setf (mem-ref ptr '(:struct VkSubpassDependency))
	  `(:srcSubpass ,src-subpass
	    :dstSubpass ,dst-subpass
	    :srcStageMask ,src-stage-mask
	    :dstStageMask ,dst-stage-mask
	    :srcAccessMask ,src-access-mask
	    :dstAccessMask ,dst-access-mask
	    :dependencyFlags ,flags))))

(define-foreign-type s-attachment-reference ()
  ()
  (:actual-type :struct VkAttachmentReference)
  (:simple-parser attachment-reference))
(defmethod translate-into-foreign-memory (class-val (type s-attachment-reference) ptr)
  (with-slots (attachment layout) class-val
    (setf (mem-ref ptr '(:struct VkAttachmentReference))
	  `(:attachment ,attachment
	    :layout ,layout))))

(define-foreign-type s-subpass-description ()
  ()
  (:actual-type :struct VkSubpassDescription)
  (:simple-parser subpass-description))
(defmethod translate-into-foreign-memory (class-val (type s-subpass-description) ptr)
  (with-slots (flags pipeline-bind-point input-attachments color-attachments resolve-attachments depth-stencil-attachments preserve-attachments) class-val
    (let* ((input-attachment-count (length input-attachments))
	   (p-input-attachment (set-null-ptr input-attachments
					     (foreign-alloc 'attachment-reference :count input-attachment-count
										  :initial-contents input-attachments)))
	   (color-attachment-count (length color-attachments))
	   (p-color-attachemnt (set-null-ptr color-attachments
					     (foreign-alloc 'attachment-reference :count color-attachment-count
										  :initial-contents color-attachments)))
	   (p-resolve-attachment (set-null-ptr resolve-attachments
					       (foreign-alloc 'attachment-reference :count color-attachment-count
										    :initial-contents resolve-attachments)))
	   (p-depth-stencil-state (set-null-ptr depth-stencil-attachments
						(foreign-alloc 'attachment-reference :count color-attachment-count
										     :initial-contents depth-stencil-attachments)))
	   (preserve-attachment-count (length preserve-attachments))
	   (p-preserve-attachment (set-null-ptr preserve-attachments
						(foreign-alloc :uint32 :count preserve-attachment-count
								       :initial-contents preserve-attachments))))
      (setf (mem-ref ptr '(:struct VkSubpassDescription))
	    `(:flags ,flags 
	      :pipelineBindPoint ,pipeline-bind-point
	      :inputAttachmentCount ,input-attachment-count
	      :pInputAttachments ,p-input-attachment
	      :colorAttachmentCount ,color-attachment-count
	      :pColorAttachments ,p-color-attachemnt
	      :pResolveAttachments ,p-resolve-attachment
	      :pDepthStencilAttachment ,p-depth-stencil-state
	      :preserveAttachmentCount ,preserve-attachment-count
	      :pPreserveAttachments ,p-preserve-attachment)))))
(defun free-subpass-description (ptr)
  (let* ((p-input-attachment (foreign-slot-value ptr '(:struct VkSubpassDescription) :pInputAttachments))
	 (p-color-attachemnt (foreign-slot-value ptr '(:struct VkSubpassDescription) :pColorAttachments))
	 (p-resolve-attachment (foreign-slot-value ptr '(:struct VkSubpassDescription) :pResolveAttachments))
	 (p-depth-stencil-state (foreign-slot-value ptr '(:struct VkSubpassDescription) :pDepthStencilAttachment))
	 (p-preserve-attachment (foreign-slot-value ptr '(:struct VkSubpassDescription) :pPreserveAttachments)))
    (free-if-not-null p-input-attachment)
    (free-if-not-null p-color-attachemnt)
    (free-if-not-null p-resolve-attachment)
    (free-if-not-null p-depth-stencil-state)
    (free-if-not-null p-preserve-attachment)))

(define-foreign-type s-command-buffer-inheritance ()
  ()
  (:actual-type :struct VkCommandBufferInheritanceInfo)
  (:simple-parser command-buffer-inheritance))
(defmethod translate-into-foreign-memory (class-val (type s-command-buffer-inheritance) ptr)
  (with-slots (next render-pass subpass framebuffer query-enable query-flags pipeline-statistics) class-val
    (setf (mem-ref ptr '(:struct VkCommandBufferInheritanceInfo))
	  `(:pNext ,next
	    :renderPass ,render-pass
	    :subpass ,subpass
	    :framebuffer, framebuffer
	    :occlusionQueryEnable ,query-enable
	    :queryFlags ,query-flags
	    :pipelineStatistics ,pipeline-statistics))))

(define-foreign-type s-image-blit ()
  ()
  (:actual-type :struct VkImageBlit)
  (:simple-parser image-blit))
(defmethod translate-into-foreign-memory (class-val (type s-image-blit) ptr)
  (with-slots (src-aspect-mask 
	       src-mip-level 
	       src-base-array-layer 
	       src-layer-count 
	       src-offset-x1 
	       src-offset-y1 
	       src-offset-z1 
	       src-offset-x2 
	       src-offset-y2 
	       src-offset-z2 
	       dst-aspect-mask 
	       dst-mip-level 
	       dst-base-array-layer 
	       dst-layer-count 
	       dst-offset-x1 
	       dst-offset-y1 
	       dst-offset-z1 
	       dst-offset-x2 
	       dst-offset-y2 
	       dst-offset-z2) class-val
    (let ((p-src-subresource (foreign-slot-pointer ptr '(:struct VkImageBlit) :srcSubresource))
	  (p-dst-subresource (foreign-slot-pointer ptr '(:struct VkImageBlit) :dstSubresource))
	  (p-src-offset (foreign-slot-pointer ptr '(:struct VkImageBlit) :srcOffsets))
	  (p-dst-offset (foreign-slot-pointer ptr '(:struct VkImageBlit) :dstOffsets)))
      (setf (mem-ref p-src-subresource '(:struct VkImageSubresourceLayers))
	    `(:aspectMask ,src-aspect-mask
	      :mipLevel ,src-mip-level
	      :baseArrayLayer ,src-base-array-layer
	      :layerCount ,src-layer-count)
	    (mem-ref p-dst-subresource '(:struct VkImageSubresourceLayers))
	    `(:aspectMask ,dst-aspect-mask
	      :mipLevel ,dst-mip-level
	      :baseArrayLayer ,dst-base-array-layer
	      :layerCount ,dst-layer-count)
	    (mem-aref p-src-offset '(:struct VkOffset3D) 0)
	    `(:x ,src-offset-x1 :y ,src-offset-y1 :z ,src-offset-z1)
	    (mem-aref p-src-offset '(:struct VkOffset3D) 1)
	    `(:x ,src-offset-x1 :y ,src-offset-y1 :z ,src-offset-z1)
	    (mem-aref p-dst-offset '(:struct VkOffset3D) 0)
	    `(:x ,dst-offset-x1 :y ,dst-offset-y1 :z ,dst-offset-z1)
	    (mem-aref p-dst-offset '(:struct VkOffset3D) 1)
	    `(:x ,dst-offset-x1 :y ,dst-offset-y1 :z ,dst-offset-z1)))))

(define-foreign-type s-memory-barrier ()
  ()
  (:actual-type :struct VkBufferMemoryBarrier)
  (:simple-parser memory-barrier))
(defmethod translate-into-foreign-memory (class-val (type s-memory-barrier) ptr)
  (with-slots (next src-mask dst-mask) class-val
    (setf (mem-ref ptr '(:struct VkMemoryBarrier))
	  `(:pNext ,next
	    :srcAccessMask ,src-mask
	    :dstAccessMask ,dst-mask))))

(define-foreign-type s-buffer-memory-barrier ()
  ()
  (:actual-type :struct VkBufferMemoryBarrier)
  (:simple-parser buffer-memory-barrier))
(defmethod translate-into-foreign-memory (class-val (type s-buffer-memory-barrier) ptr)
  (with-slots (next
	       src-mask dst-mask
	       src-queue-family-index dst-queue-family-index
	       buffer offset size) class-val
    (setf (mem-ref ptr '(:struct VkBufferMemoryBarrier))
	  `(:pNext ,next
	    :srcAccessMask ,src-mask
	    :dstAccessMask ,dst-mask
	    :srcQueueFamilyIndex ,src-queue-family-index
	    :dstQueueFamilyIndex ,dst-queue-family-index
	    :buffer ,buffer 
	    :offset ,offset
	    :size ,size))))

(define-foreign-type s-image-memory-barrier ()
  ()
  (:actual-type :struct VkImageMemoryBarrier)
  (:simple-parser image-memory-barrier))
(defmethod translate-into-foreign-memory (class-val (type s-image-memory-barrier) ptr)
  (with-slots (next 
	       src-mask dst-mask 
	       old-layout new-layout 
	       src-queue-family-index  dst-queue-family-index 
	       image 
	       aspect-mask 
	       base-mip-level 
	       level-count 
	       base-array-layer 
	       layer-count) class-val
    (let ((p-range (foreign-slot-pointer ptr '(:struct VkImageMemoryBarrier) :subresourceRange)))
      (setf (mem-ref ptr '(:struct VkImageMemoryBarrier))
	    `(:pNext ,next
	      :srcAccessMask ,src-mask
	      :dstAccessMask ,dst-mask
	      :oldLayout ,old-layout
	      :newLayout ,new-layout
	      :srcQueueFamilyIndex ,src-queue-family-index
	      :dstQueueFamilyIndex ,dst-queue-family-index
	      :image ,image)
	    (mem-ref p-range '(:struct VkImageSubresourceRange))
	    `(:aspectMask ,aspect-mask
	      :baseMipLevel ,base-mip-level
	      :levelCount ,level-count
	      :baseArrayLayer ,base-array-layer
	      :layerCount ,layer-count)))))

(define-foreign-type s-image-reslove ()
  ()
  (:actual-type :struct VkImageResolve)
  (:simple-parser image-reslove))
(defmethod translate-into-foreign-memory (class-val (type s-image-reslove) ptr)
  (with-slots (src-aspect-mask 
	       src-mip-level 
	       src-base-array-layer 
	       src-layer-count 
	       src-offset-x src-offset-y src-offset-z 
	       dst-aspect-mask 
	       dst-mip-level 
	       dst-base-array-layer 
	       dst-layer-count dst-offset-x dst-offset-y dst-offset-z 
	       width height depth) class-val
    (let ((p-src-subresource (foreign-slot-pointer ptr '(:struct VkImageResolve) :srcSubresource))
	  (p-src-offset (foreign-slot-pointer ptr '(:struct VkImageResolve) :srcOffset))
	  (p-dst-subresource (foreign-slot-pointer ptr '(:struct VkImageResolve) :dstSubresource))
	  (p-dst-offset (foreign-slot-pointer ptr '(:struct VkImageResolve) :dstOffset))
	  (p-extent (foreign-slot-pointer ptr '(:struct VkImageResolve) :extent)))
      (setf (mem-ref p-src-subresource '(:struct VkImageSubresourceLayers))
	    `(:aspectMask ,src-aspect-mask
	      :mipLevel ,src-mip-level
	      :baseArrayLayer ,src-base-array-layer
	      :layerCount ,src-layer-count)
	    (mem-ref p-src-offset '(:struct VkOffset3D))
	    `(list :x ,src-offset-x :y ,src-offset-y :z ,src-offset-z)
	    (mem-ref p-dst-subresource '(:struct VkImageSubresourceLayers))
	    `(list :aspectMask ,dst-aspect-mask
		   :mipLevel ,dst-mip-level
		   :baseArrayLayer ,dst-base-array-layer
		   :layerCount ,dst-layer-count)
	    (mem-ref p-dst-offset '(:struct VkOffset3D))
	    `(list :x ,dst-offset-x :y ,dst-offset-y :z ,dst-offset-z)
	    (mem-ref p-extent '(:struct VkExtent3D))
	    `(list :width ,width :height ,height :depth ,depth)))))

(define-foreign-type s-clear-attachment ()
  ()
  (:actual-type :struct VkClearAttachment)
  (:simple-parser clear-attachment))
(defmethod translate-into-foreign-memory (class-val (type s-clear-attachment) ptr)
  (with-slots (mask color-attachment clear-value) class-val
    (let ((p-clear-value (foreign-slot-pointer ptr '(:struct VkClearAttachment) :clearValue)))
      (setf (mem-ref p-clear-value 'clear-value) clear-value
	    (mem-ref ptr '(:struct VkClearAttachment))
	    `(:aspectMask ,mask
	      :colorAttachment ,color-attachment)))))

(define-foreign-type s-clear-rect ()
  ()
  (:actual-type :struct VkClearRect)
  (:simple-parser clear-rect))
(defmethod translate-into-foreign-memory (class-val (type s-clear-rect) ptr)
  (with-slots (x y width height base-array-layer layer-count) class-val
    (let* ((p-rect (foreign-slot-pointer ptr '(:struct VkClearRect) :rect))
	   (p-rect-offset (foreign-slot-pointer p-rect '(:struct VkRect2D) :offset))
	   (p-rect-extent (foreign-slot-pointer p-rect '(:struct VkRect2D) :extent)))
      (setf (mem-ref p-rect-offset '(:struct VkOffset2D))
	    `(:x ,x :y ,y)
	    (mem-ref p-rect-extent '(:struct VkExtent2D))
	    `(:width ,width :height ,height)
	    (mem-ref ptr '(:struct VkClearRect))
	    `(:baseArrayLayer ,base-array-layer
	      :layerCount ,layer-count)))))

(define-foreign-type u-clear-color-value ()
  ()
  (:actual-type :union VkClearColorValue)
  (:simple-parser clear-color-value))
(defmethod translate-into-foreign-memory (class-val (type u-clear-color-value) ptr)
  (with-slots (r g b a) class-val
    (setf (mem-aref ptr :float 0) r
	  (mem-aref ptr :float 1) g
	  (mem-aref ptr :float 2) b
	  (mem-aref ptr :float 3) a)))

(define-foreign-type s-image-subresource-range ()
  ()
  (:actual-type :struct VkImageSubresourceRange)
  (:simple-parser image-subresource-range))
(defmethod translate-into-foreign-memory (class-val (type s-image-subresource-range) ptr)
  (with-slots (aspect-mask 
	       base-mip-level 
	       level-count 
	       base-array-layer 
	       layer-count) class-val
    (setf (mem-ref ptr '(:struct VkImageSubresourceRange))
	  `(:aspectMask ,aspect-mask
	    :baseMipLevel ,base-mip-level
	    :levelCount ,level-count 
	    :baseArrayLayer ,base-array-layer
	    :layerCount ,layer-count))))

(define-foreign-type s-clear-depth-stencil-value ()
  ()
  (:actual-type :struct VkClearDepthStencilValue)
  (:simple-parser clear-depth-stencil-value))
(defmethod translate-into-foreign-memory (class-val (type s-clear-depth-stencil-value) ptr)
  (with-slots (depth stencil) class-val
    (setf (mem-ref ptr '(:struct VkClearDepthStencilValue))
	  `(:depth ,depth
	    :stencil ,stencil))))

(define-foreign-type s-buffer-copy ()
  ()
  (:actual-type :struct VkBufferCopy)
  (:simple-parser buffer-copy))
(defmethod translate-into-foreign-memory (class-val (type s-buffer-copy) ptr)
  (with-slots (src-offset dst-offset size) class-val
    (setf (mem-ref ptr '(:struct VkBufferCopy))
	  `(:srcOffset ,src-offset
	    :dstOffset ,dst-offset
	    :size ,size))))

(define-foreign-type s-buffer-image-copy ()
  ()
  (:actual-type :struct VkBufferImageCopy)
  (:simple-parser buffer-image-copy))
(defmethod translate-into-foreign-memory (class-val (type s-buffer-image-copy) ptr)
  (with-slots (buffer-offset 
	       buffer-row-length 
	       buffer-image-height 
	       image-aspect-mask 
	       image-mip-level 
	       image-base-array-layer 
	       image-layer-count 
	       image-x 
	       image-y 
	       image-z 
	       image-width 
	       image-height 
	       image-depth) class-val
    (let ((p-subresource (foreign-slot-pointer ptr '(:struct VkBufferImageCopy) :imageSubresource))
	  (p-offset (foreign-slot-pointer ptr '(:struct VkBufferImageCopy) :imageOffset))
	  (p-extent (foreign-slot-pointer ptr '(:struct VkBufferImageCopy) :imageOffset)))
      (setf (mem-ref ptr '(:struct VkBufferImageCopy))
	    `(:bufferOffset ,buffer-offset
	      :bufferRowLength ,buffer-row-length
	      :bufferImageHeight ,buffer-image-height)
	    (mem-ref p-subresource '(:struct VkImageSubresourceLayers))
	    `(:aspectMask ,image-aspect-mask
	      :mipLevel ,image-mip-level
	      :baseArrayLayer ,image-base-array-layer
	      :layerCount ,image-layer-count)
	    (mem-ref p-offset '(:struct VkOffset3D))
	    `(:x ,image-x :y ,image-y :z ,image-z)
	    (mem-ref p-extent '(:struct VkExtent3D))
	    `(:width ,image-width :height ,image-height :depth ,image-depth)))))

(define-foreign-type s-image-copy ()
  ()
  (:actual-type VkImageCopy)
  (:simple-parser image-copy))
(defmethod translate-into-foreign-memory (class-val (type s-image-copy) ptr)
  (with-slots (src-image-aspect-mask src-image-mip-level src-image-base-array-layer src-image-layer-count 
	       src-image-x src-image-y src-image-z 
	       dst-image-aspect-mask dst-image-mip-level dst-image-base-array-layer dst-image-layer-count 
	       dst-image-x dst-image-y dst-image-z 
	       width height depth) class-val
    (let ((p-src-subresource (foreign-slot-value ptr '(:struct VkImageCopy) :srcSubresource))
	  (p-dst-subresource (foreign-slot-value ptr '(:struct VkImageCopy) :srcOffset))
	  (p-src-offset (foreign-slot-value ptr '(:struct VkImageCopy) :dstSubresource))
	  (p-dst-offset (foreign-slot-value ptr '(:struct VkImageCopy) :dstOffset))
	  (p-extent (foreign-slot-value ptr '(:struct VkImageCopy) :extent)))
      (setf (mem-ref p-src-subresource '(:struct VkImageSubresourceLayers))
	    `(:aspectMask ,src-image-aspect-mask
	      :mipLevel ,src-image-mip-level
	      :baseArrayLayer ,src-image-base-array-layer
	      :layerCount ,src-image-layer-count)
	    (mem-ref p-src-offset '(:struct VkOffset3D))
	    `(:x ,src-image-x :y ,src-image-y :z ,src-image-z)
	    (mem-ref p-dst-subresource '(:struct VkImageSubresourceLayers))
	    `(:aspectMask ,dst-image-aspect-mask
	      :mipLevel ,dst-image-mip-level
	      :baseArrayLayer ,dst-image-base-array-layer
	      :layerCount ,dst-image-layer-count)
	    (mem-ref p-dst-offset '(:struct VkOffset3D))
	    `(:x ,dst-image-x :y ,dst-image-y :z ,dst-image-z)
	    (mem-ref p-extent '(:struct VkExtent3D))
	    `(:width ,width :height ,height :depth ,depth)))))

(define-foreign-type s-descriptor-pool-size ()
  ()
  (:actual-type :struct VkDescriptorPoolSize)
  (:simple-parser descriptor-pool-size))
(defmethod translate-into-foreign-memory (class-val (type s-descriptor-pool-size) ptr)
  (with-slots (type count) class-val
    (setf (mem-ref ptr '(:struct VkDescriptorPoolSize))
	  `(:type ,type
	    :descriptorCount ,count))))

(define-foreign-type s-descriptor-set-layout-binding ()
  ()
  (:actual-type :struct VkDescriptorSetLayoutBinding)
  (:simple-parser descriptor-set-layout-binding))
(defmethod translate-into-foreign-memory (class-val (type s-descriptor-set-layout-binding) ptr)
  (with-slots (binding type samplers flags) class-val
    (let* ((sampler-count (length samplers))
	   (p-samplers (set-null-ptr samplers
				     (foreign-alloc 'VkSampler :count sampler-count
							       :initial-contents samplers))))
      (setf (mem-ref ptr '(:struct VkDescriptorSetLayoutBinding))
	    `(:binding ,binding
	      :descriptorType ,type 
	      :descriptorCount ,sampler-count
	      :stageFlags ,flags
	      :pImmutableSamplers ,p-samplers)))))
(defun free-descriptor-set-layout-binding (ptr)
  (let ((p-samplers (foreign-slot-value ptr '(:struct VkDescriptorSetLayoutBinding) :pImmutableSamplers)))
    (free-if-not-null p-samplers)))

(define-foreign-type s-swapchain-create-info ()
  ()
  (:actual-type :struct VkSwapchainCreateInfoKHR)
  (:simple-parser swapchain-create-info))
(defmethod translate-into-foreign-memory (class-val (type s-swapchain-create-info) ptr)
  (with-slots (next 
	       flags 
	       min-image-count 
	       format 
	       color-space 
	       image-width 
	       image-height 
	       image-array-layers 
	       usage 
	       sharing-mode 
	       indices 
	       pretransform 
	       composite-alpha 
	       present-mode 
	       clipped 
	       old-swapchain 
	       surface) class-val
    (let* ((indices-count (length indices))
	   (p-indices (set-null-ptr indices
				    (foreign-alloc :uint32 :count indices-count
							   :initial-contents indices))))
      (setf (mem-ref ptr '(:struct VkSwapchainCreateInfoKHR))
	  `(:sType ,+structure-type-swapchain-create-info-khr+
	    :pNext ,next 
	    :flags ,flags
	    :surface ,surface
	    :minImageCount ,min-image-count
	    :imageFormat ,format 
	    :imageColorSpace ,color-space
	    :imageArrayLayers ,image-array-layers
	    :imageUsage ,usage
	    :imageSharingMode ,sharing-mode
	    :queueFamilyIndexCount ,indices-count
	    :pQueueFamilyIndices ,p-indices
	    :preTransform ,pretransform
	    :compositeAlpha ,composite-alpha
	    :presentMode ,present-mode
	    :clipped ,clipped
	    :oldSwapchain ,old-swapchain)
	  (mem-ref (foreign-slot-value ptr '(:struct VkSwapchainCreateInfoKHR) :imageExtent)
		   '(:struct VkExtent2D))
	  `(:width ,image-width :height ,image-height)))))

(define-foreign-type s-mapped-memory-range ()
  ()
  (:actual-type :struct VkMappedMemoryRange)
  (:simple-parser mapped-memory-range))
(defmethod translate-into-foreign-memory (class-val (type s-mapped-memory-range) ptr)
  (with-slots (next memory offset size) class-val
    (setf (mem-ref ptr '(:struct VkMappedMemoryRange))
	  `(:sType ,+structure-type-mapped-memory-range+
	    :pNext ,next
	    :memory ,memory
	    :offset ,offset
	    :size ,size))))

(define-foreign-type s-sparse-memory-bind ()
  ()
  (:actual-type :struct VkSparseMemoryBind)
  (:simple-parser sparse-memory-bind))
(defmethod translate-into-foreign-memory (class-val (type s-sparse-memory-bind) ptr)
  (with-slots (offset size memory memory-offset flags) class-val
    (setf (mem-ref ptr '(:struct VkSparseMemoryBind))
	  `(:resourceOffset ,offset
	    :size ,size
	    :memory ,memory
	    :memoryOffset ,memory-offset
	    :flags ,flags))))

(define-foreign-type s-sparse-buffer-memory-bind ()
  ()
  (:actual-type :struct VkSparseBufferMemoryBindInfo)
  (:simple-parser sparse-buffer-memory-bind))
(defmethod translate-into-foreign-memory (class-val (type s-sparse-buffer-memory-bind) ptr)
  (with-slots (buffer binds) class-val
    (let* ((count (length binds))
	   (p-binds (set-null-ptr binds (foreign-alloc 'sparse-memory-bind :count count
									   :initial-contents binds))))
      (setf (mem-ref ptr '(:struct VkSparseBufferMemoryBindInfo))
	    `(:buffer ,buffer
	      :bindCount ,count
	      :pBinds ,p-binds)))))
(defun free-sparse-buffer-memory-bind (ptr)
  (free-if-not-null (foreign-slot-value ptr '(:struct VkSparseBufferMemoryBindInfo) :pBinds))
  (foreign-free ptr))

(define-foreign-type s-sparse-image-opaque-memory-bind ()
  ()
  (:actual-type :struct VkSparseImageOpaqueMemoryBindInfo)
  (:simple-parser sparse-image-opaque-memory-bind))
(defmethod translate-into-foreign-memory (class-val (type s-sparse-image-opaque-memory-bind) ptr)
  (with-slots (image binds) class-val
    (let* ((count (length binds))
	   (p-binds (set-null-ptr binds (foreign-alloc 'sparse-memory-bind :count count
									   :initial-contents binds))))
      (setf (mem-ref ptr '(:struct VkSparseImageOpaqueMemoryBindInfo))
	    `(:image ,image
	      :bindCount ,count
	      :pBinds ,p-binds)))))
(defun free-sparse-image-opaque-memory (ptr)
  (free-if-not-null (foreign-slot-value ptr '(:struct VkSparseImageOpaqueMemoryBindInfo) :pBinds))
  (foreign-free ptr))

(define-foreign-type s-sparse-image-memory-bind ()
  ()
  (:actual-type :struct VkSparseImageMemoryBind)
  (:simple-parser sparse-image-memory-bind))
(defmethod translate-into-foreign-memory (class-val (type s-sparse-image-memory-bind) ptr)
  (with-slots (aspect-mask mip-level array-layer 
	       x y z 
	       width height depth 
	       memory 
	       offset 
	       flags) class-val
    (setf (mem-ref (foreign-slot-pointer ptr '(:struct VkSparseImageMemoryBind) :subresource) '(:struct VkImageSubresource))
	  `(:aspectMask ,aspect-mask :mipLevel ,mip-level :arrayLayer ,array-layer)
	  (mem-ref (foreign-slot-pointer ptr '(:struct VkSparseImageMemoryBind) :offset) '(:struct VkOffset3D))
	  `(:x ,x :y ,y :z ,z)
	  (mem-ref (foreign-slot-pointer ptr '(:struct VkSparseImageMemoryBind) :extent) '(:struct VkExtent3D))
	  `(:width ,width :height ,height :depth ,depth)
	  (mem-ref ptr '(:struct VkSparseImageMemoryBind))
	  `(:memory ,memory
	    :memoryOffset ,offset 
	    :flags ,flags))))

(define-foreign-type s-sparse-image-memory-bind-info ()
  ()
  (:actual-type :struct VkSparseImageMemoryBindInfo)
  (:simple-parser sparse-image-memory-bind-info))
(defmethod translate-into-foreign-memory (class-val (type s-sparse-image-memory-bind-info) ptr)
  (with-slots (image binds) class-val
    (let* ((count (length binds))
	   (p-binds (set-null-ptr binds (foreign-alloc 'sparse-image-memory-bind
						       :count count
						       :initial-contents binds))))
      (setf (mem-ref ptr '(:struct VkSparseImageMemoryBindInfo))
	    `(:image ,image
	      :bindCount ,count
	      :pBinds ,p-binds)))))
(defun free-sparse-image-memory-bind-info (ptr)
  (free-if-not-null (foreign-slot-value ptr '(:struct VkSparseImageMemoryBindInfo) :pBinds))
  (foreign-free ptr))

(define-foreign-type s-bind-sparse-info ()
  ()
  (:actual-type :struct VkBindSparseInfo)
  (:simple-parser bind-sparse-info))
(defmethod translate-into-foreign-memory (class-val (type s-bind-sparse-info) ptr)
  (with-slots (next wait-semaphores buffer-binds image-opaque-binds image-binds single-semaphores) class-val
    (let* ((wait-semaphore-count (length wait-semaphores))
	   (buffer-bind-count (length buffer-binds))
	   (image-opaque-bind-count (length image-opaque-binds))
	   (image-bind-count (length image-binds))
	   (single-semaphore-count (length single-semaphores))

	   (p-wait-semaphore (set-null-ptr wait-semaphores
					   (foreign-alloc 'VkSemaphore
							  :count wait-semaphore-count
							  :initial-contents wait-semaphores)))
	   (p-buffer-bind (set-null-ptr buffer-binds
					(foreign-alloc 'sparse-buffer-memory-bind
						       :count buffer-bind-count
						       :initial-contents buffer-binds)))
	   (p-image-opaque-bind (set-null-ptr image-opaque-binds
					      (foreign-alloc 'sparse-image-opaque-memory-bind
							     :count image-opaque-bind-count
							     :initial-contents image-opaque-binds)))
	   (p-image-bind (set-null-ptr image-binds
				       (foreign-alloc 'sparse-image-memory-bind 
						      :count image-bind-count
						      :initial-contents image-binds)))
	   (p-single-semaphore (set-null-ptr single-semaphores
					     (foreign-alloc 'VkSemaphore
							    :count single-semaphore-count
							    :initial-contents single-semaphores))))
      (setf (mem-ref ptr '(:struct VkBindSparseInfo))
	    `(:sType ,+structure-type-bind-sparse-info+
	      :pNext ,next
	      :waitSemaphoreCount ,wait-semaphore-count
	      :pWaitSemaphores ,p-wait-semaphore
	      :bufferBindCount ,buffer-bind-count
	      :pBufferBinds ,p-buffer-bind
	      :imageOpaqueBindCount ,image-opaque-bind-count
	      :pImageOpaqueBinds ,p-image-opaque-bind
	      :imageBindCount ,image-bind-count
	      :pImageBinds ,p-image-bind
	      :signalSemaphoreCount ,single-semaphore-count
	      :pSignalSemaphores ,p-single-semaphore)))))
(defun free-bind-sparse-info (ptr)
  (free-if-not-null (foreign-slot-value ptr '(:struct VkBindSparseInfo) :pWaitSemaphores))
  (free-if-not-null (foreign-slot-value ptr '(:struct VkBindSparseInfo) :pBufferBinds))
  (free-if-not-null (foreign-slot-value ptr '(:struct VkBindSparseInfo) :pImageOpaqueBinds))
  (free-if-not-null (foreign-slot-value ptr '(:struct VkBindSparseInfo) :pImageBinds))
  (free-if-not-null (foreign-slot-value ptr '(:struct VkBindSparseInfo) :pSignalSemaphores))
  (free-if-not-null ptr))

(define-foreign-type s-descriptor-image ()
  ()
  (:actual-type :struct VkDescriptorImageInfo)
  (:simple-parser descriptor-image))
(defmethod translate-into-foreign-memory (class-val (type s-descriptor-image) ptr)
  (with-slots (sampler image-views image-layout) class-val
    (setf (mem-ref ptr '(:struct VkDescriptorImageInfo))
	  `(:sampler ,sampler
	    :imageView ,image-views
	    :imageLayout ,image-layout))))

(define-foreign-type s-descriptor-buffer ()
  ()
  (:actual-type :struct VkDescriptorBufferInfo)
  (:simple-parser descriptor-buffer))
(defmethod translate-into-foreign-memory (class-val (type s-descriptor-buffer) ptr)
  (with-slots (buffer offset range) class-val
    (setf (mem-ref ptr '(:struct VkDescriptorBufferInfo))
	  `(:buffer ,buffer
	    :offset ,offset
	    :range ,range))))

(define-foreign-type s-write-descriptor-set ()
  ()
  (:actual-type :struct VkWriteDescriptorSet)
  (:simple-parser write-descriptor-set))
(defmethod translate-into-foreign-memory (class-val (type s-write-descriptor-set) ptr)
  (with-slots (next dst-set dst-binding dst-array-element type image-info buffer-info texel-buffer-view) class-val
    (let* ((count (max (length image-info) (length buffer-info) (length texel-buffer-view)))
	   (p-image-info (set-null-ptr image-info (foreign-alloc 's-descriptor-image :initial-contents image-info)))
	   (p-buffer-info (set-null-ptr buffer-info (foreign-alloc 's-descriptor-buffer :initial-contents buffer-info)))
	   (p-texel-buffer-view (set-null-ptr texel-buffer-view (foreign-alloc 'VkBufferView :initial-contents texel-buffer-view))))
      (setf (mem-ref ptr '(:struct VkWriteDescriptorSet))
	    `(:sType ,+structure-type-write-descriptor-set+
	      :pNext ,next 
	      :dstSet ,dst-set
	      :dstBinding ,dst-binding
	      :dstArrayElement ,dst-array-element
	      :descriptorCount ,count
	      :descriptorType type
	      :pImageInfo ,p-image-info
	      :pBufferInfo ,p-buffer-info
	      :pTexelBufferView ,p-texel-buffer-view)))))
(defun free-write-descriptor (ptr)
  (free-if-not-null (foreign-slot-value ptr '(:struct VkWriteDescriptorSet) :pImageInfo))
  (free-if-not-null (foreign-slot-value ptr '(:struct VkWriteDescriptorSet) :pBufferInfo))
  (free-if-not-null (foreign-slot-value ptr '(:struct VkWriteDescriptorSet) :pTexelBufferView))
  (free-if-not-null ptr))


(define-foreign-type s-copy-descriptor-set ()
  ()
  (:actual-type :struct VkCopyDescriptorSet)
  (:simple-parser copy-descriptor-set))
(defmethod translate-into-foreign-memory (class-val (type s-copy-descriptor-set) ptr)
  (with-slots (next src-set src-binding src-array-element dst-set dst-binding dst-array-element descriptor-count) class-val
    (setf (mem-ref ptr '(:struct VkCopyDescriptorSet))
	  `(:sType ,+structure-type-copy-descriptor-set+
	    :pNext ,next
	    :srcSet ,src-set
	    :srcBinding ,src-binding
	    :srcArrayElement ,src-array-element
	    :dstSet ,dst-set
	    :dstBinding ,dst-binding
	    :dstArrayElement ,dst-array-element
	    :descriptorCount ,descriptor-count))))


