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

(defun create-command-pool (device queue-family-index &key (next +vk-null-ptr+) (flags 0) (allocator +vk-null-ptr+))
  (with-foreign-objects ((p-info '(:struct VkCommandPoolCreateInfo))
			 (p-pool 'VkCommandPool))
    (setf (mem-ref p-info '(:struct VkCommandPoolCreateInfo))
	  (list :sType +structure-type-command-pool-create-info+
		:pNext next
		:flags flags
		:queueFamilyIndex queue-family-index))
    (check-vk-result (vkCreateCommandPool device p-info allocator p-pool))
    (mem-ref p-pool 'VkCommandPool)))

(defun destroy-command-pool (device pool &optional (allocator +vk-null-ptr+))
  (vkDestroyCommandPool device pool allocator))

(defun begin-command-buffer (cmd &key
				   (next +vk-null-ptr+)
				   (flags 0)
				   (inheritance-ptr nil))
  (with-foreign-object (p-info '(:struct VkCommandBufferBeginInfo))
    (let ((p-inheritance (foreign-alloc 'command-buffer-inheritance)))
      (setf (mem-ref p-inheritance 'command-buffer-inheritance) inheritance-ptr
	    (mem-ref p-info '(:struct VkCommandBufferBeginInfo))
	    (list :sType +structure-type-command-buffer-begin-info+
		  :pNext next
		  :flags flags
		  :pInheritanceInfo inheritance-ptr))
      (check-vk-result (vkBeginCommandBuffer cmd p-info))
      (foreign-free p-inheritance))))

(defun end-command-buffer (cmd)
  (check-vk-result (vkEndCommandBuffer cmd)))

(defun cmd-begin-render-pass (cmd render-pass framebuffer &key
							    (next +vk-null-ptr+)
							    (offset-x 0)
							    (offset-y 0)
							    (extent-width 600)
							    (extent-height 600)
							    (clear-color-ptr nil)
							    (content +subpass-contents-inline+))
  (with-foreign-object (p-info '(:struct VkRenderPassBeginInfo))
    (let* ((p-rect (foreign-slot-pointer p-info '(:struct VkRenderPassBeginInfo) :renderArea))
	   (clear-color-count (length clear-color-ptr))
	   (p-clear-color (set-null-ptr clear-color-ptr
					(foreign-alloc 'clear-color-value :count clear-color-count
									  :initial-contents clear-color-ptr))))
      (setf (mem-ref p-info '(:struct VkRenderPassBeginInfo))
	    (list :sType +structure-type-render-pass-begin-info+
		  :pNext next
		  :renderPass render-pass
		  :framebuffer framebuffer
		  :clearValueCount clear-color-count
		  :pClearValues p-clear-color)
	    (mem-ref (foreign-slot-pointer p-rect '(:struct VkRect2D) :offset) '(:struct VkOffset2D))
	    (list :x offset-x :y offset-y)
	    (mem-ref (foreign-slot-pointer p-rect '(:struct VkRect2D) :extent) '(:struct VkExtent2D))
	    (list :width extent-width :height extent-height))
      (vkCmdBeginRenderPass cmd p-info content)
      (foreign-free p-clear-color))))

(defun cmd-end-render-pass (cmd)
  (vkCmdEndRenderPass cmd))

(defun cmd-bind-pipeline (cmd bind-point pipeline)
  (vkCmdBindPipeline cmd bind-point pipeline))

(defun cmd-draw (cmd vertex-count instance-count first-vertex first-instance)
  (vkCmdDraw cmd vertex-count instance-count first-vertex first-instance))

(defun cmd-begin-query (cmd query-pool query flags)
  (vkCmdBeginQuery cmd query-pool query flags))

(defun cmd-bind-descriptor-sets (cmd pipeline-bind-point layout first-set descriptor-sets &optional offsets)
  (let ((set-count (length descriptor-sets))
	(offset-count (length offsets)))
    (with-foreign-objects ((p-set 'VkDescriptorSet set-count)
			   (p-offset :uint32 offset-count))
      (dotimes (i set-count)
	(setf (mem-aref p-set 'VkDescriptorSet i) (nth i descriptor-sets)))
      (when offsets
	(dotimes (i offset-count)
	  (setf (mem-aref p-offset :uint32 i) (nth i offsets))))
      (vkCmdBindDescriptorSets cmd pipeline-bind-point layout first-set set-count p-set offset-count p-offset))))

(defun cmd-bind-index-buffer (cmd buffer offset index-type)
  (vkCmdBindIndexBuffer cmd buffer offset index-type))

(defun cmd-bind-vertex-buffers (cmd first-binding binding-count buffers offsets)
  (unless (= binding-count (length buffers) (length offsets))
    (error "length of buffers and offsets must be equal to binding-count"))
  (with-foreign-objects ((p-buffers 'VkBuffer binding-count)
			 (p-offsets 'VkDeviceSize binding-count))
    (dotimes (i binding-count)
      (setf (mem-aref p-buffers 'VkBuffer i) (nth i buffers)
	    (mem-aref p-offsets 'VkDeviceSize i) (nth i offsets)))
    (vkCmdBindVertexBuffers cmd first-binding binding-count p-buffers p-offsets)))

(defun cmd-blit-image (cmd src-image src-layout dst-image dst-layout region-ptr filter)
  (let* ((region-count (length region-ptr))
	 (p-region (foreign-alloc 'image-blit :count region-count
					      :initial-contents region-ptr)))
    (vkCmdBlitImage cmd src-image src-layout dst-image dst-layout region-count p-region filter)))

(defun cmd-dispatch (cmd x y z)
  (vkCmdDispatch cmd x y z))

(defun cmd-dispatch-indirect (cmd buffer offset)
  (vkCmdDispatchIndirect cmd buffer offset))

(defun cmd-draw-indexed (cmd index-count instance-count first-index vertex-offset first-instance)
  (vkCmdDrawIndexed cmd index-count instance-count first-index vertex-offset first-instance))

(defun cmd-draw-indexed-indirect (cmd buffer offset draw-count stride)
  (vkCmdDrawIndexedIndirect cmd buffer offset draw-count stride))

(defun cmd-draw-indirect (cmd buffer offset draw-count stride)
  (vkCmdDrawIndirect cmd buffer offset draw-count stride))

(defun cmd-end-query (cmd pool query)
  (vkCmdEndQuery cmd pool query))

(defun cmd-execute-commands (buffer cmds)
  (let ((count (length cmds)))
    (with-foreign-object (p-cmd 'VkCommandBuffer count)
      (dotimes (i count)
	(setf (mem-aref p-cmd 'VkCommandBuffer i) (nth i cmds)))
      (vkCmdExecuteCommands buffer count (set-null-ptr cmds p-cmd)))))

(defun cmd-fill-buffer (cmd dst-buffer dst-offset size data)
  (vkCmdFillBuffer cmd dst-buffer dst-offset size data))

(defun cmd-next-subpass (cmd contents)
  (vkCmdNextSubpass cmd contents))

(defun cmd-pipeline-barrier (cmd src-stage-mask dst-stage-mask &key
								 (dependency-flags 0)
								 (memory-barriers-ptr nil)
								 (buffer-memory-barriers-ptr nil)
								 (image-memory-barriers-ptr nil))
  (let* ((memory-barrier-count (length memory-barriers-ptr))
	 (buffer-memory-barrier-count (length buffer-memory-barriers-ptr))
	 (image-memory-barriers-count (length image-memory-barriers-ptr))
	 (p-memory-barrier (set-null-ptr memory-barriers-ptr
					 (foreign-alloc 'memory-barrier :count memory-barrier-count
									:initial-contents memory-barriers-ptr)))
	 (p-buffer-memory-barrier (set-null-ptr buffer-memory-barriers-ptr
						(foreign-alloc 'buffer-memory-barrier :count buffer-memory-barrier-count
										      :initial-contents buffer-memory-barriers-ptr)))
	 (p-image-memroy-barrier (set-null-ptr image-memory-barriers-ptr
					       (foreign-alloc 'image-memory-barrier :count image-memory-barriers-count
							      :initial-contents image-memory-barriers-ptr))))
    (vkCmdPipelineBarrier cmd src-stage-mask dst-stage-mask dependency-flags
			  memory-barrier-count p-memory-barrier
			  buffer-memory-barrier-count p-buffer-memory-barrier
			  image-memory-barriers-count p-image-memroy-barrier)
    (free-if-not-null p-memory-barrier)
    (free-if-not-null p-buffer-memory-barrier)
    (free-if-not-null p-image-memroy-barrier)))

(defun cmd-push-constants (cmd layout flags offset size val-ptr)
  (vkCmdPushConstants cmd layout flags offset size val-ptr))

(defun cmd-reset-event (cmd event mask)
  (vkCmdResetEvent cmd event mask))

(defun cmd-reset-query-pool (cmd pool first-query query-count)
  (vkCmdResetQueryPool cmd pool first-query query-count ))

(defun cmd-set-blend-constants (cmd constants)
  (vkCmdSetBlendConstants cmd constants))

(defun cmd-reslove-image (cmd src-image src-image-layout dst-image dst-image-layout region-ptr)
  (let* ((region-count (length region-ptr))
	 (p-region (set-null-ptr region-ptr
				 (foreign-alloc 'image-reslove :count region-count
							       :initial-contents region-ptr))))
    (vkCmdResolveImage cmd
		     src-image src-image-layout
		     dst-image dst-image-layout
		     region-count p-region)
    (free-if-not-null p-region)))

(defun cmd-set-depth-bias (cmd bias-constant-factor bias-clamp base-slope-factor)
  (vkCmdSetDepthBias cmd bias-constant-factor bias-clamp base-slope-factor))

(defun cmd-set-depth-bounds (cmd min-depth-bounds max-depth-bounds)
  (vkCmdSetDepthBounds cmd min-depth-bounds max-depth-bounds))

(defun cmd-set-event (cmd event mask)
  (vkCmdSetEvent cmd event mask))

(defun cmd-set-line-width (cmd line-width)
  (vkCmdSetLineWidth cmd line-width))

(defun cmd-set-scissor (cmd first-scissor rect-ptr)
  "use with-rect-2d and free here"
  (let* ((rect-count (length rect-ptr))
	 (p-rect (foreign-alloc 'rect2d :count rect-count
					:initial-contents rect-ptr)))
    (vkCmdSetScissor cmd first-scissor rect-count p-rect)
    (foreign-free p-rect)))

(defun cmd-set-stencil-compare-mask (cmd flags mask)
  (vkCmdSetStencilCompareMask cmd flags mask))

(defun cmd-set-stencil-reference (cmd mask reference)
  (vkCmdSetStencilReference cmd mask reference))

(defun cmd-set-stencil-write-mask (cmd flags mask)
  (vkCmdSetStencilWriteMask cmd flags mask))

(defun cmd-set-viewport (cmd first-viewport viewport-ptr)
  "use with-viewports free pointer here"
  (let* ((viewport-count (length viewport-ptr))
	(p-viewport (foreign-alloc 'viewport :count viewport-count
				   :initial-contents viewport-ptr)))
    (vkCmdSetViewport cmd first-viewport viewport-count viewport-ptr)
    (foreign-free p-viewport)))

(defun cmd-update-buffer (cmd buffer offset size data-ptr)
  ;;should data-ptr free here??
  (vkCmdUpdateBuffer cmd buffer offset size data-ptr))

(defun cmd-wait-events (cmd events src-stage-mask dst-stage-mask
			memory-barrier-count memory-barrier-ptr
			buffer-memory-barrier-count buffer-memory-barrier-ptr
			image-memory-barrier-count image-memory-barrier-ptr)
  (let ((event-count (length events)))
    (with-foreign-object (p-event 'VkEvent event-count)
      (dotimes (i event-count)
	(setf (mem-aref p-event 'VkEvent i) (nth i events)))
      (vkCmdWaitEvents cmd event-count p-event
		       src-stage-mask dst-stage-mask
		       memory-barrier-count memory-barrier-ptr
		       buffer-memory-barrier-count buffer-memory-barrier-ptr
		       image-memory-barrier-count image-memory-barrier-ptr))))

(defun cmd-write-timestamp (buffer flags pool query)
  (vkCmdWriteTimestamp buffer flags pool query))

(defun cmd-clear-attachments (cmd attachment-ptr rect-ptr)
  (let* ((attachment-count (length attachment-ptr))
	 (rect-count (length rect-ptr))
	 (p-attachemnt (set-null-ptr attachment-ptr
				     (foreign-alloc 'clear-attachment
						    :initial-contents attachment-ptr)))
	 (p-rect (set-null-ptr rect-ptr
			       (foreign-alloc 'rect2d :count rect-count
						      :initial-contents rect-ptr))))
    (vkCmdClearAttachments cmd attachment-count p-attachemnt rect-count p-rect)
    (free-if-not-null p-attachemnt)
    (free-if-not-null p-rect)))

(defun cmd-copy-query-pool-results (cmd pool first-query query-count dst-buffer dst-offset stride flags)
  (vkCmdCopyQueryPoolResults cmd pool first-query query-count dst-buffer dst-offset stride flags))

(defun cmd-clear-color-image (cmd image layout color-ptr range-ptr)
  (let* ((count (length range-ptr))
	 (p-color (set-null-ptr color-ptr (foreign-alloc 'clear-color-value :count count :initial-contents color-ptr)))
	 (p-range (set-null-ptr range-ptr (foreign-alloc 'image-subresource-range :count count :initial-contents range-ptr))))
    (vkCmdClearColorImage cmd image layout p-color count p-range)
    (free-if-not-null p-color)
    (free-if-not-null p-range)))

(defun cmd-clear-depth-stencil-image (cmd image layout clear-ptr range-ptr)
  (let* ((count (length range-ptr))
	 (p-clear (set-null-ptr clear-ptr (foreign-alloc 'clear-depth-stencil-value :count count :initial-contents clear-ptr)))
	 (p-range (set-null-ptr range-ptr (foreign-alloc 'image-subresource-range :count count :initial-contents range-ptr))))
    (vkCmdClearDepthStencilImage cmd image layout p-clear count p-range)
    (free-if-not-null p-clear)
    (free-if-not-null p-range)))

(defun cmd-copy-buffer (cmd src-buffer dst-buffer region-ptr)
  (let* ((region-count (length region-ptr))
	 (p-region (foreign-alloc 'buffer-copy :count region-count :initial-contents region-ptr)))
    (vkCmdCopyBuffer cmd src-buffer dst-buffer region-count p-region)
    (foreign-free p-region)))

(defun cmd-copy-buffer-to-image (cmd buffer image layout region-ptr)
  (let* ((region-count (length region-ptr))
	 (p-region (foreign-alloc 'buffer-image-copy :count region-count :initial-contents region-ptr)))
    (vkCmdCopyBufferToImage cmd buffer image layout region-count p-region)
    (foreign-free p-region)))

(defun cmd-copy-image (cmd src-image src-layout dst-image dst-layout region-ptr)
  (let* ((region-count (length region-ptr))
	 (p-region (foreign-alloc 'image-copy :count region-count :initial-contents region-ptr)))
    (vkCmdCopyImage cmd src-image src-layout dst-image dst-layout region-count p-region)
    (foreign-free p-region)))

(defun cmd-copy-image-to-buffer (cmd src-image src-layout dst-buffer region-ptr)
  (let* ((region-count (length region-ptr))
	 (p-region (foreign-alloc 'buffer-image-copy :count region-count :initial-contents region-ptr)))
    (vkCmdCopyImageToBuffer cmd src-image src-layout dst-buffer region-count region-ptr)
    (foreign-free p-region)))

(defun reset-command-buffer (cmds flags)
  (check-vk-result (vkResetCommandBuffer cmds flags)))

(defun reset-command-pool (device pool flags)
  (check-vk-result (vkResetCommandPool device pool flags)))

(defun reset-descriptor-pool (device pool flags)
  (check-vk-result (vkResetDescriptorPool device pool flags)))
