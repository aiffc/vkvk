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

(defpackage #:vkvk
  (:use #:cl #:cffi #:%vk)
  (:export *vk-debug*
	   +vk-null-ptr+
	   vk_false
	   vk_true

	   make-viewport-ptr
	   make-rect2d-ptr
	   make-clear-color-value-ptr
	   make-clear-value-ptr
	   make-device-queue-ptr
	   make-specialization-map-entry-ptr
	   make-specialization-ptr
	   make-vertex-input-binding-descriptor-ptr
	   make-vertex-input-attribute-desription-ptr
	   make-pipeline-shader-stage-ptr
	   make-pipeline-vertex-input-state
	   make-pipeline-input-assembly-state-ptr
	   make-pipeline-tessellation-state-ptr
	   make-pipeline-viewport-state-ptr
	   make-pipeline-rasterization-state-ptr
	   make-pipeline-multisample-state-ptr
	   make-pipeline-depth-stencil-state-ptr
	   make-pipeline-color-blend-attachment-state-ptr
	   make-pipeline-color-blend-state-ptr
	   make-pipeline-dynamic-state-ptr
	   make-graphics-pipeline-ptr
	   make-compute-pipeline-ptr
	   make-push-constant-range-ptr
	   make-attachment-description-ptr
	   make-subpass-dependency-description-ptr
	   make-attachment-reference-ptr
	   make-subpass-description-ptr
	   make-command-buffer-inheritance-ptr
	   make-clear-value-ptr
	   make-image-blit-ptr
	   make-memory-barrier-ptr
	   make-buffer-memory-barrier-ptr
	   make-image-memory-barrier-ptr
	   make-image-reslove-ptr
	   make-clear-attachment-ptr
	   make-clear-rect-ptr
	   make-image-subresource-range-ptr
	   make-clear-depth-stencil-value-ptr
	   make-buffer-image-copy-ptr
	   make-image-copy-ptr
	   make-descriptor-pool-size-ptr
	   make-descriptor-set-layout-binding-ptr
	   make-swapchain-create-info-ptr
	   make-mapped-memory-range-ptr
	   make-sparse-memory-bind-ptr
	   make-sparse-buffer-memory-bind-ptr
	   make-sparse-image-opaque-memory-bind-ptr
	   make-sparse-image-memory-bind-ptr
	   make-sparse-image-memory-bind-info-ptr
	   make-bind-sparse-info-ptr
	   make-descriptor-image-ptr
	   make-descriptor-buffer-ptr
	   make-write-descriptor-set-ptr
	   make-copy-descriptor-set-ptr
	   
	   make-vulkan-version
	   
	   enumerate-instance-extension-properties
	   enumerate-instance-layer-properties
	   available-instance-extensions
	   available-instance-layers
	   create-instance
	   destroy-instance
	   enumerate-physical-devices
	   enumerate-physical-device-groups-khx
	   get-physical-device-display-plane-properties-khr
	   get-physical-device-display-properties-khr
	   get-physical-device-features
	   get-physical-device-format-properties
	   get-physical-device-image-format-properties
	   get-physical-device-memory-properties
	   get-physical-device-properties
	   get-physical-device-queue-family-properties
	   get-physical-device-sparse-image-format-properties
	   get-physical-device-surface-capabilities-khr
	   get-physical-device-surface-present-mode-khr
	   get-physical-device-surface-formats-khr
	   get-device-group-surface-present-mode-khr
	   get-physical-device-surface-support-khr
	   get-physical-device-win32-presentation-support-khr
	   get-physical-device-external-image-format-properties-nv
	   get-physical-device-features2-khr
	   get-physical-device-format-properties2-khr
	   get-physical-device-memory-properties2-khr
	   get-physical-device-mir-presentation-support-khr
	   get-physical-device-present-rectangles-khx
	   get-physical-device-properties2-khr
	   get-physical-device-queue-family-properties2-ext
	   get-physical-device-external-buffer-properties-khx
	   get-physical-device-external-semaphore-properties-khx
	   get-physical-device-generated-commands-properties-nvx
	   get-physical-device-image-format-properties2-khr
	   get-physical-device-surface-formats2-khr
	   get-physical-device-sparse-image-format-properties2-khr
	   get-physical-device-surface-capabilities2-khr
	   get-physical-device-xlib-presentation-support-khr
	   acquire-xlib-display-ext
	   
	   enumerate-device-extension-properties
	   enumerate-device-layer-properties
	   available-device-extensions
	   available-device-layers
	   create-device
	   destroy-device
	   get-device-queue
	   device-wait-idle
	   queue-submit
	   queue-present-khr
	   queue-wait-idle
	   get-device-memory-commitment
	   
	   create-ios-surface-mvk
	   create-mac-os-surface-mvk
	   create-win32-surface-khr
	   create-xcb-surface-khr
	   create-xlib-surface-khr
	   destroy-surface-khr

	   bind-buffer-memory
	   bind-buffer-memory2-khx
	   bind-image-memory
	   bind-image-memroy2-khx
	   unmap-memory
	   get-buffer-memory-requirements
	   get-invalidate-mapped-memroy-ranges
	   map-memory
	   flush-mapped-memory-range

	   create-swapchain-khr
	   destroy-swapchain-khr
	   get-swapchain-images-khr
	   acquire-next-image-khr
	   get-display-mode-properties-khr
	   get-display-plane-capabilities-khr
	   get-display-plane-supported-displays-khr
	   create-display-mode-khr
	   create-shader-swapchain-khr
	   
	   create-image-view
	   destroy-image-view
	   get-image-memory-requirements
	   get-image-subresource-layout
	   get-image-sparse-memory-requirements
	   create-image
	   destroy-image
	   
	   create-shader-module
	   destroy-shader-module
	   create-pipeline-layout
	   destroy-pipeline-layout
	   create-render-pass
	   destroy-render-pass
	   create-graphics-pipeline
	   destroy-pipeline
	   get-render-area-granularity
	   create-pipeline-cache
	   destroy-pipeline-cache
	   get-pipeline-cache-data
	   merge-pipeline-caches

	   create-framebuffer
	   destroy-framebuffer

	   create-command-pool
	   destroy-command-pool
	   begin-command-buffer
	   end-command-buffer
	   cmd-begin-render-pass
	   cmd-end-render-pass
	   cmd-bind-pipeline
	   cmd-draw
	   cmd-draw-indexed
	   cmd-draw-indexed-indirect
	   cmd-draw-indirect
	   cmd-begin-query
	   cmd-end-query
	   cmd-bind-descriptor-sets
	   cmd-bind-index-buffer
	   cmd-bind-vertex-buffers
	   cmd-bind-pipeline
	   cmd-dispatch
	   cmd-dispatch-indirect
	   cmd-execute-commands
	   cmd-fill-buffer
	   cmd-next-subpass
	   cmd-pipeline-barrier
	   cmd-push-constants
	   cmd-reset-event
	   cmd-reset-query-pool
	   cmd-reslove-image
	   cmd-set-depth-bias
	   cmd-set-depth-bounds
	   cmd-set-event
	   cmd-set-line-width
	   cmd-set-scissor
	   cmd-set-stencil-compare-mask
	   cmd-set-stencil-reference
	   cmd-set-stencil-write-mask
	   cmd-set-viewport
	   cmd-update-buffer
	   cmd-wait-events
	   cmd-write-timestamp
	   cmd-clear-attachments
	   cmd-clear-color-image
	   cmd-clear-depth-stencil-image
	   cmd-copy-buffer
	   cmd-copy-buffer-to-image
	   cmd-copy-image
	   cmd-copy-image-to-buffer
	   reset-command-buffer
	   reset-command-pool
	   reset-descriptor-pool
	   
	   create-semaphore
	   destroy-semaphore

	   create-fence
	   destroy-fence
	   wait-for-fences
	   reset-fences
	   get-fence-status

	   allocate-command-buffers
	   allocate-descriptor-sets
	   allocate-memory
	   free-command-buffers
	   free-descriptor-sets
	   free-memory

	   create-buffer
	   destroy-buffer
	   create-buffer-view
	   destroy-buffer-view

	   create-descriptor-pool
	   destroy-descriptor-pool
	   create-descriptor-set-layout
	   destroy-descriptor-set-layout

	   create-event
	   destroy-event
	   get-event-status
	   reset-event
	   set-event

	   create-query-pool
	   destroy-query-pool
	   get-query-pool-result

	   create-sampler
	   destroy-sampler
	   ;;types
	   +access-indirect-command-read-bit+
	   +access-index-read-bit+
	   +access-vertex-attribute-read-bit+
	   +access-uniform-read-bit+
	   +access-input-attachment-read-bit+
	   +access-shader-read-bit+
	   +access-shader-write-bit+
	   +access-color-attachment-read-bit+
	   +access-color-attachment-write-bit+
	   +access-depth-stencil-attachment-read-bit+
	   +access-depth-stencil-attachment-write-bit+
	   +access-transfer-read-bit+
	   +access-transfer-write-bit+
	   +access-host-read-bit+
	   +access-host-write-bit+
	   +access-memory-read-bit+
	   +access-memory-write-bit+
	   +access-command-process-read-bit-nvx+
	   +access-command-process-write-bit-nvx+
	   +attachment-description-may-alias-bit+
	   +buffer-create-sparse-binding-bit+
	   +buffer-create-sparse-residency-bit+
	   +buffer-create-sparse-aliased-bit+
	   +buffer-usage-transfer-src-bit+
	   +buffer-usage-transfer-dst-bit+
	   +buffer-usage-uniform-texel-buffer-bit+
	   +buffer-usage-storage-texel-buffer-bit+
	   +buffer-usage-uniform-buffer-bit+
	   +buffer-usage-storage-buffer-bit+
	   +buffer-usage-index-buffer-bit+
	   +buffer-usage-vertex-buffer-bit+
	   +buffer-usage-indirect-buffer-bit+
	   +color-component-r-bit+
	   +color-component-g-bit+
	   +color-component-b-bit+
	   +color-component-a-bit+
	   +command-buffer-reset-release-resources-bit+
	   +command-buffer-usage-one-time-submit-bit+
	   +command-buffer-usage-render-pass-continue-bit+
	   +command-buffer-usage-simultaneous-use-bit+
	   +command-pool-create-transient-bit+
	   +command-pool-create-reset-command-buffer-bit+
	   +command-pool-reset-release-resources-bit+
	   +composite-alpha-opaque-bit-khr+
	   +composite-alpha-pre-multiplied-bit-khr+
	   +composite-alpha-post-multiplied-bit-khr+
	   +composite-alpha-inherit-bit-khr+
	   +cull-mode-none+
	   +cull-mode-front-bit+
	   +cull-mode-back-bit+
	   +cull-mode-front-and-back+
	   +debug-report-information-bit-ext+
	   +debug-report-warning-bit-ext+
	   +debug-report-performance-warning-bit-ext+
	   +debug-report-error-bit-ext+
	   +debug-report-debug-bit-ext+
	   +dependency-by-region-bit+
	   +dependency-view-local-bit-khx+
	   +dependency-device-group-bit-khx+
	   +descriptor-pool-create-free-descriptor-set-bit+
	   +descriptor-set-layout-create-push-descriptor-bit-khr+
	   +device-group-present-mode-local-bit-khx+
	   +device-group-present-mode-remote-bit-khx+
	   +device-group-present-mode-sum-bit-khx+
	   +device-group-present-mode-local-multi-device-bit-khx+
	   +display-plane-alpha-opaque-bit-khr+
	   +display-plane-alpha-global-bit-khr+
	   +display-plane-alpha-per-pixel-bit-khr+
	   +display-plane-alpha-per-pixel-premultiplied-bit-khr+
	   +external-memory-feature-dedicated-only-bit-khx+
	   +external-memory-feature-exportable-bit-khx+
	   +external-memory-feature-importable-bit-khx+
	   +external-memory-feature-dedicated-only-bit-nv+
	   +external-memory-feature-exportable-bit-nv+
	   +external-memory-feature-importable-bit-nv+
	   +external-memory-handle-type-opaque-fd-bit-khx+
	   +external-memory-handle-type-opaque-win32-bit-khx+
	   +external-memory-handle-type-opaque-win32-kmt-bit-khx+
	   +external-memory-handle-type-d3d11-texture-bit-khx+
	   +external-memory-handle-type-d3d11-texture-kmt-bit-khx+
	   +external-memory-handle-type-d3d12-heap-bit-khx+
	   +external-memory-handle-type-d3d12-resource-bit-khx+
	   +external-memory-handle-type-opaque-win32-bit-nv+
	   +external-memory-handle-type-opaque-win32-kmt-bit-nv+
	   +external-memory-handle-type-d3d11-image-bit-nv+
	   +external-memory-handle-type-d3d11-image-kmt-bit-nv+
	   +external-semaphore-feature-exportable-bit-khx+
	   +external-semaphore-feature-importable-bit-khx+
	   +external-semaphore-handle-type-opaque-fd-bit-khx+
	   +external-semaphore-handle-type-opaque-win32-bit-khx+
	   +external-semaphore-handle-type-opaque-win32-kmt-bit-khx+
	   +external-semaphore-handle-type-d3d12-fence-bit-khx+
	   +external-semaphore-handle-type-fence-fd-bit-khx+
	   +fence-create-signaled-bit+
	   +format-feature-sampled-image-bit+
	   +format-feature-storage-image-bit+
	   +format-feature-storage-image-atomic-bit+
	   +format-feature-uniform-texel-buffer-bit+
	   +format-feature-storage-texel-buffer-bit+
	   +format-feature-storage-texel-buffer-atomic-bit+
	   +format-feature-vertex-buffer-bit+
	   +format-feature-color-attachment-bit+
	   +format-feature-color-attachment-blend-bit+
	   +format-feature-depth-stencil-attachment-bit+
	   +format-feature-blit-src-bit+
	   +format-feature-blit-dst-bit+
	   +format-feature-sampled-image-filter-linear-bit+
	   +format-feature-sampled-image-filter-cubic-bit-img+
	   +format-feature-transfer-src-bit-khr+
	   +format-feature-transfer-dst-bit-khr+
	   +image-aspect-color-bit+
	   +image-aspect-depth-bit+
	   +image-aspect-stencil-bit+
	   +image-aspect-metadata-bit+
	   +image-create-sparse-binding-bit+
	   +image-create-sparse-residency-bit+
	   +image-create-sparse-aliased-bit+
	   +image-create-mutable-format-bit+
	   +image-create-cube-compatible-bit+
	   +image-create-bind-sfr-bit-khx+
	   +image-create-2d-array-compatible-bit-khr+
	   +image-usage-transfer-src-bit+
	   +image-usage-transfer-dst-bit+
	   +image-usage-sampled-bit+
	   +image-usage-storage-bit+
	   +image-usage-color-attachment-bit+
	   +image-usage-depth-stencil-attachment-bit+
	   +image-usage-transient-attachment-bit+
	   +image-usage-input-attachment-bit+
	   +indirect-commands-layout-usage-unordered-sequences-bit-nvx+
	   +indirect-commands-layout-usage-sparse-sequences-bit-nvx+
	   +indirect-commands-layout-usage-empty-executions-bit-nvx+
	   +indirect-commands-layout-usage-indexed-sequences-bit-nvx+
	   +memory-allocate-device-mask-bit-khx+
	   +memory-heap-device-local-bit+
	   +memory-heap-multi-instance-bit-khx+
	   +memory-property-device-local-bit+
	   +memory-property-host-visible-bit+
	   +memory-property-host-coherent-bit+
	   +memory-property-host-cached-bit+
	   +memory-property-lazily-allocated-bit+
	   +object-entry-usage-graphics-bit-nvx+
	   +object-entry-usage-compute-bit-nvx+
	   +peer-memory-feature-copy-src-bit-khx+
	   +peer-memory-feature-copy-dst-bit-khx+
	   +peer-memory-feature-generic-src-bit-khx+
	   +peer-memory-feature-generic-dst-bit-khx+
	   +pipeline-create-disable-optimization-bit+
	   +pipeline-create-allow-derivatives-bit+
	   +pipeline-create-derivative-bit+
	   +pipeline-create-view-index-from-device-index-bit-khx+
	   +pipeline-create-dispatch-base-khx+
	   +pipeline-stage-top-of-pipe-bit+
	   +pipeline-stage-draw-indirect-bit+
	   +pipeline-stage-vertex-input-bit+
	   +pipeline-stage-vertex-shader-bit+
	   +pipeline-stage-tessellation-control-shader-bit+
	   +pipeline-stage-tessellation-evaluation-shader-bit+
	   +pipeline-stage-geometry-shader-bit+
	   +pipeline-stage-fragment-shader-bit+
	   +pipeline-stage-early-fragment-tests-bit+
	   +pipeline-stage-late-fragment-tests-bit+
	   +pipeline-stage-color-attachment-output-bit+
	   +pipeline-stage-compute-shader-bit+
	   +pipeline-stage-transfer-bit+
	   +pipeline-stage-bottom-of-pipe-bit+
	   +pipeline-stage-host-bit+
	   +pipeline-stage-all-graphics-bit+
	   +pipeline-stage-all-commands-bit+
	   +pipeline-stage-command-process-bit-nvx+
	   +query-control-precise-bit+
	   +query-pipeline-statistic-input-assembly-vertices-bit+
	   +query-pipeline-statistic-input-assembly-primitives-bit+
	   +query-pipeline-statistic-vertex-shader-invocations-bit+
	   +query-pipeline-statistic-geometry-shader-invocations-bit+
	   +query-pipeline-statistic-geometry-shader-primitives-bit+
	   +query-pipeline-statistic-clipping-invocations-bit+
	   +query-pipeline-statistic-clipping-primitives-bit+
	   +query-pipeline-statistic-fragment-shader-invocations-bit+
	   +query-pipeline-statistic-tessellation-control-shader-patches-bit+
	   +query-pipeline-statistic-tessellation-evaluation-shader-invocations-bit+
	   +query-pipeline-statistic-compute-shader-invocations-bit+
	   +query-result-64-bit+
	   +query-result-wait-bit+
	   +query-result-with-availability-bit+
	   +query-result-partial-bit+
	   +queue-graphics-bit+
	   +queue-compute-bit+
	   +queue-transfer-bit+
	   +queue-sparse-binding-bit+
	   +sample-count-1-bit+
	   +sample-count-2-bit+
	   +sample-count-4-bit+
	   +sample-count-8-bit+
	   +sample-count-16-bit+
	   +sample-count-32-bit+
	   +sample-count-64-bit+
	   +shader-stage-vertex-bit+
	   +shader-stage-tessellation-control-bit+
	   +shader-stage-tessellation-evaluation-bit+
	   +shader-stage-geometry-bit+
	   +shader-stage-fragment-bit+
	   +shader-stage-compute-bit+
	   +shader-stage-all-graphics+
	   +shader-stage-all+
	   +sparse-image-format-single-miptail-bit+
	   +sparse-image-format-aligned-mip-size-bit+
	   +sparse-image-format-nonstandard-block-size-bit+
	   +sparse-memory-bind-metadata-bit+
	   +stencil-face-front-bit+
	   +stencil-face-back-bit+
	   +stencil-front-and-back+
	   +subpass-description-per-view-attributes-bit-nvx+
	   +subpass-description-per-view-position-x-only-bit-nvx+
	   +surface-counter-vblank-ext+
	   +surface-transform-identity-bit-khr+
	   +surface-transform-rotate-90-bit-khr+
	   +surface-transform-rotate-180-bit-khr+
	   +surface-transform-rotate-270-bit-khr+
	   +surface-transform-horizontal-mirror-bit-khr+
	   +surface-transform-horizontal-mirror-rotate-90-bit-khr+
	   +surface-transform-horizontal-mirror-rotate-180-bit-khr+
	   +surface-transform-horizontal-mirror-rotate-270-bit-khr+
	   +surface-transform-inherit-bit-khr+
	   +swapchain-create-bind-sfr-bit-khx+
	   +access-indirect-command-read-bit +
	   +access-index-read-bit +
	   +access-vertex-attribute-read-bit +
	   +access-uniform-read-bit +
	   +access-input-attachment-read-bit +
	   +access-shader-read-bit +
	   +access-shader-write-bit +
	   +access-color-attachment-read-bit +
	   +access-color-attachment-write-bit +
	   +access-depth-stencil-attachment-read-bit +
	   +access-depth-stencil-attachment-write-bit +
	   +access-transfer-read-bit +
	   +access-transfer-write-bit +
	   +access-host-read-bit +
	   +access-host-write-bit +
	   +access-memory-read-bit +
	   +access-memory-write-bit +
	   +access-command-process-read-bit-nvx +
	   +access-command-process-write-bit-nvx +
	   +attachment-description-may-alias-bit +
	   +attachment-load-op-load+
	   +attachment-load-op-clear+
	   +attachment-load-op-dont-care+
	   +attachment-store-op-store+
	   +attachment-store-op-dont-care+
	   +blend-factor-zero+
	   +blend-factor-one+
	   +blend-factor-src-color+
	   +blend-factor-one-minus-src-color+
	   +blend-factor-dst-color+
	   +blend-factor-one-minus-dst-color+
	   +blend-factor-src-alpha+
	   +blend-factor-one-minus-src-alpha+
	   +blend-factor-dst-alpha+
	   +blend-factor-one-minus-dst-alpha+
	   +blend-factor-constant-color+
	   +blend-factor-one-minus-constant-color+
	   +blend-factor-constant-alpha+
	   +blend-factor-one-minus-constant-alpha+
	   +blend-factor-src-alpha-saturate+
	   +blend-factor-src1-color+
	   +blend-factor-one-minus-src1-color+
	   +blend-factor-src1-alpha+
	   +blend-factor-one-minus-src1-alpha+
	   +blend-op-add+
	   +blend-op-subtract+
	   +blend-op-reverse-subtract+
	   +blend-op-min+
	   +blend-op-max+
	   +border-color-float-transparent-black+
	   +border-color-int-transparent-black+
	   +border-color-float-opaque-black+
	   +border-color-int-opaque-black+
	   +border-color-float-opaque-white+
	   +border-color-int-opaque-white+
	   +buffer-create-sparse-binding-bit +
	   +buffer-create-sparse-residency-bit +
	   +buffer-create-sparse-aliased-bit +
	   +buffer-usage-transfer-src-bit +
	   +buffer-usage-transfer-dst-bit +
	   +buffer-usage-uniform-texel-buffer-bit +
	   +buffer-usage-storage-texel-buffer-bit +
	   +buffer-usage-uniform-buffer-bit +
	   +buffer-usage-storage-buffer-bit +
	   +buffer-usage-index-buffer-bit +
	   +buffer-usage-vertex-buffer-bit +
	   +buffer-usage-indirect-buffer-bit +
	   +color-component-r-bit+
	   +color-component-g-bit+
	   +color-component-b-bit+
	   +color-component-a-bit+
	   +color-space-srgb-nonlinear-khr+
	   +color-space-display-p3-nonlinear-ext +
	   +color-space-extended-srgb-linear-ext +
	   +color-space-dci-p3-linear-ext +
	   +color-space-dci-p3-nonlinear-ext +
	   +color-space-bt709-linear-ext +
	   +color-space-bt709-nonlinear-ext +
	   +color-space-bt2020-linear-ext +
	   +color-space-hdr10-st2084-ext +
	   +color-space-dolbyvision-ext +
	   +color-space-hdr10-hlg-ext +
	   +color-space-adobergb-linear-ext +
	   +color-space-adobergb-nonlinear-ext +
	   +color-space-pass-through-ext +
	   +command-buffer-level-primary+
	   +command-buffer-level-secondary+
	   +command-buffer-reset-release-resources-bit +
	   +command-buffer-usage-one-time-submit-bit+
	   +command-buffer-usage-render-pass-continue-bit+
	   +command-buffer-usage-simultaneous-use-bit +
	   +command-pool-create-transient-bit +
	   +command-pool-create-reset-command-buffer-bit +
	   +command-pool-reset-release-resources-bit +
	   +compare-op-never+
	   +compare-op-less+
	   +compare-op-equal+
	   +compare-op-less-or-equal+
	   +compare-op-greater+
	   +compare-op-not-equal+
	   +compare-op-greater-or-equal+
	   +compare-op-always+
	   +component-swizzle-identity+
	   +component-swizzle-zero+
	   +component-swizzle-one+
	   +component-swizzle-r+
	   +component-swizzle-g+
	   +component-swizzle-b+
	   +component-swizzle-a+
	   +composite-alpha-opaque-bit-khr+
	   +composite-alpha-pre-multiplied-bit-khr+
	   +composite-alpha-post-multiplied-bit-khr+
	   +composite-alpha-inherit-bit-khr+
	   +cull-mode-none+
	   +cull-mode-front-bit+
	   +cull-mode-back-bit+
	   +cull-mode-front-and-back+
	   +debug-report-information-bit-ext+
	   +debug-report-warning-bit-ext+
	   +debug-report-performance-warning-bit-ext+
	   +debug-report-error-bit-ext+
	   +debug-report-debug-bit-ext+
	   +debug-report-object-type-unknown-ext+
	   +debug-report-object-type-instance-ext+
	   +debug-report-object-type-physical-device-ext+
	   +debug-report-object-type-device-ext+
	   +debug-report-object-type-queue-ext+
	   +debug-report-object-type-semaphore-ext+
	   +debug-report-object-type-command-buffer-ext+
	   +debug-report-object-type-fence-ext+
	   +debug-report-object-type-device-memory-ext+
	   +debug-report-object-type-buffer-ext+
	   +debug-report-object-type-image-ext+
	   +debug-report-object-type-event-ext+
	   +debug-report-object-type-query-pool-ext+
	   +debug-report-object-type-buffer-view-ext+
	   +debug-report-object-type-image-view-ext+
	   +debug-report-object-type-shader-module-ext+
	   +debug-report-object-type-pipeline-cache-ext+
	   +debug-report-object-type-pipeline-layout-ext+
	   +debug-report-object-type-render-pass-ext+
	   +debug-report-object-type-pipeline-ext+
	   +debug-report-object-type-descriptor-set-layout-ext+
	   +debug-report-object-type-sampler-ext+
	   +debug-report-object-type-descriptor-pool-ext+
	   +debug-report-object-type-descriptor-set-ext+
	   +debug-report-object-type-framebuffer-ext+
	   +debug-report-object-type-command-pool-ext+
	   +debug-report-object-type-surface-khr-ext+
	   +debug-report-object-type-swapchain-khr-ext+
	   +debug-report-object-type-debug-report-callback-ext-ext+
	   +debug-report-object-type-display-khr-ext+
	   +debug-report-object-type-display-mode-khr-ext+
	   +debug-report-object-type-object-table-nvx-ext+
	   +debug-report-object-type-indirect-commands-layout-nvx-ext+
	   +debug-report-object-type-descriptor-update-template-khr-ext +
	   +dependency-by-region-bit +
	   +dependency-view-local-bit-khx +
	   +dependency-device-group-bit-khx +
	   +descriptor-pool-create-free-descriptor-set-bit +
	   +descriptor-set-layout-create-push-descriptor-bit-khr +
	   +descriptor-type-sampler+
	   +descriptor-type-combined-image-sampler+
	   +descriptor-type-sampled-image+
	   +descriptor-type-storage-image+
	   +descriptor-type-uniform-texel-buffer+
	   +descriptor-type-storage-texel-buffer+
	   +descriptor-type-uniform-buffer+
	   +descriptor-type-storage-buffer+
	   +descriptor-type-uniform-buffer-dynamic+
	   +descriptor-type-storage-buffer-dynamic+
	   +descriptor-type-input-attachment+
	   +descriptor-update-template-type-descriptor-set-khr +
	   +descriptor-update-template-type-push-descriptors-khr +
	   +device-event-type-display-hotplug-ext+
	   +device-group-present-mode-local-bit-khx +
	   +device-group-present-mode-remote-bit-khx +
	   +device-group-present-mode-sum-bit-khx +
	   +device-group-present-mode-local-multi-device-bit-khx +
	   +discard-rectangle-mode-inclusive-ext+
	   +discard-rectangle-mode-exclusive-ext+
	   +display-event-type-first-pixel-out-ext+
	   +display-plane-alpha-opaque-bit-khr+
	   +display-plane-alpha-global-bit-khr+
	   +display-plane-alpha-per-pixel-bit-khr+
	   +display-plane-alpha-per-pixel-premultiplied-bit-khr+
	   +display-power-state-off-ext+
	   +display-power-state-suspend-ext+
	   +display-power-state-on-ext+
	   +dynamic-state-viewport+
	   +dynamic-state-scissor+
	   +dynamic-state-line-width+
	   +dynamic-state-depth-bias+
	   +dynamic-state-blend-constants+
	   +dynamic-state-depth-bounds+
	   +dynamic-state-stencil-compare-mask+
	   +dynamic-state-stencil-write-mask+
	   +dynamic-state-stencil-reference+
	   +dynamic-state-viewport-w-scaling-nv +
	   +dynamic-state-discard-rectangle-ext +
	   +dynamic-state-line-stipple-ext+
	   +external-memory-feature-dedicated-only-bit-khx+
	   +external-memory-feature-exportable-bit-khx+
	   +external-memory-feature-importable-bit-khx+
	   +external-memory-feature-dedicated-only-bit-nv+
	   +external-memory-feature-exportable-bit-nv+
	   +external-memory-feature-importable-bit-nv+
	   +external-memory-handle-type-opaque-fd-bit-khx+
	   +external-memory-handle-type-opaque-win32-bit-khx+
	   +external-memory-handle-type-opaque-win32-kmt-bit-khx+
	   +external-memory-handle-type-d3d11-texture-bit-khx+
	   +external-memory-handle-type-d3d11-texture-kmt-bit-khx+
	   +external-memory-handle-type-d3d12-heap-bit-khx+
	   +external-memory-handle-type-d3d12-resource-bit-khx+
	   +external-memory-handle-type-opaque-win32-bit-nv+
	   +external-memory-handle-type-opaque-win32-kmt-bit-nv+
	   +external-memory-handle-type-d3d11-image-bit-nv+
	   +external-memory-handle-type-d3d11-image-kmt-bit-nv+
	   +external-semaphore-feature-exportable-bit-khx+
	   +external-semaphore-feature-importable-bit-khx+
	   +external-semaphore-handle-type-opaque-fd-bit-khx+
	   +external-semaphore-handle-type-opaque-win32-bit-khx+
	   +external-semaphore-handle-type-opaque-win32-kmt-bit-khx+
	   +external-semaphore-handle-type-d3d12-fence-bit-khx+
	   +external-semaphore-handle-type-fence-fd-bit-khx+
	   +fence-create-signaled-bit+
	   +filter-nearest+
	   +filter-linear+
	   +filter-cubic-img +
	   +format-undefined+
	   +format-r4g4-unorm-pack8+
	   +format-r4g4b4a4-unorm-pack16+
	   +format-b4g4r4a4-unorm-pack16+
	   +format-r5g6b5-unorm-pack16+
	   +format-b5g6r5-unorm-pack16+
	   +format-r5g5b5a1-unorm-pack16+
	   +format-b5g5r5a1-unorm-pack16+
	   +format-a1r5g5b5-unorm-pack16+
	   +format-r8-unorm+
	   +format-r8-snorm+
	   +format-r8-uscaled+
	   +format-r8-sscaled+
	   +format-r8-uint+
	   +format-r8-sint+
	   +format-r8-srgb+
	   +format-r8g8-unorm+
	   +format-r8g8-snorm+
	   +format-r8g8-uscaled+
	   +format-r8g8-sscaled+
	   +format-r8g8-uint+
	   +format-r8g8-sint+
	   +format-r8g8-srgb+
	   +format-r8g8b8-unorm+
	   +format-r8g8b8-snorm+
	   +format-r8g8b8-uscaled+
	   +format-r8g8b8-sscaled+
	   +format-r8g8b8-uint+
	   +format-r8g8b8-sint+
	   +format-r8g8b8-srgb+
	   +format-b8g8r8-unorm+
	   +format-b8g8r8-snorm+
	   +format-b8g8r8-uscaled+
	   +format-b8g8r8-sscaled+
	   +format-b8g8r8-uint+
	   +format-b8g8r8-sint+
	   +format-b8g8r8-srgb+
	   +format-r8g8b8a8-unorm+
	   +format-r8g8b8a8-snorm+
	   +format-r8g8b8a8-uscaled+
	   +format-r8g8b8a8-sscaled+
	   +format-r8g8b8a8-uint+
	   +format-r8g8b8a8-sint+
	   +format-r8g8b8a8-srgb+
	   +format-b8g8r8a8-unorm+
	   +format-b8g8r8a8-snorm+
	   +format-b8g8r8a8-uscaled+
	   +format-b8g8r8a8-sscaled+
	   +format-b8g8r8a8-uint+
	   +format-b8g8r8a8-sint+
	   +format-b8g8r8a8-srgb+
	   +format-a8b8g8r8-unorm-pack32+
	   +format-a8b8g8r8-snorm-pack32+
	   +format-a8b8g8r8-uscaled-pack32+
	   +format-a8b8g8r8-sscaled-pack32+
	   +format-a8b8g8r8-uint-pack32+
	   +format-a8b8g8r8-sint-pack32+
	   +format-a8b8g8r8-srgb-pack32+
	   +format-a2r10g10b10-unorm-pack32+
	   +format-a2r10g10b10-snorm-pack32+
	   +format-a2r10g10b10-uscaled-pack32+
	   +format-a2r10g10b10-sscaled-pack32+
	   +format-a2r10g10b10-uint-pack32+
	   +format-a2r10g10b10-sint-pack32+
	   +format-a2b10g10r10-unorm-pack32+
	   +format-a2b10g10r10-snorm-pack32+
	   +format-a2b10g10r10-uscaled-pack32+
	   +format-a2b10g10r10-sscaled-pack32+
	   +format-a2b10g10r10-uint-pack32+
	   +format-a2b10g10r10-sint-pack32+
	   +format-r16-unorm+
	   +format-r16-snorm+
	   +format-r16-uscaled+
	   +format-r16-sscaled+
	   +format-r16-uint+
	   +format-r16-sint+
	   +format-r16-sfloat+
	   +format-r16g16-unorm+
	   +format-r16g16-snorm+
	   +format-r16g16-uscaled+
	   +format-r16g16-sscaled+
	   +format-r16g16-uint+
	   +format-r16g16-sint+
	   +format-r16g16-sfloat+
	   +format-r16g16b16-unorm+
	   +format-r16g16b16-snorm+
	   +format-r16g16b16-uscaled+
	   +format-r16g16b16-sscaled+
	   +format-r16g16b16-uint+
	   +format-r16g16b16-sint+
	   +format-r16g16b16-sfloat+
	   +format-r16g16b16a16-unorm+
	   +format-r16g16b16a16-snorm+
	   +format-r16g16b16a16-uscaled+
	   +format-r16g16b16a16-sscaled+
	   +format-r16g16b16a16-uint+
	   +format-r16g16b16a16-sint+
	   +format-r16g16b16a16-sfloat+
	   +format-r32-uint+
	   +format-r32-sint+
	   +format-r32-sfloat+
	   +format-r32g32-uint+
	   +format-r32g32-sint+
	   +format-r32g32-sfloat+
	   +format-r32g32b32-uint+
	   +format-r32g32b32-sint+
	   +format-r32g32b32-sfloat+
	   +format-r32g32b32a32-uint+
	   +format-r32g32b32a32-sint+
	   +format-r32g32b32a32-sfloat+
	   +format-r64-uint+
	   +format-r64-sint+
	   +format-r64-sfloat+
	   +format-r64g64-uint+
	   +format-r64g64-sint+
	   +format-r64g64-sfloat+
	   +format-r64g64b64-uint+
	   +format-r64g64b64-sint+
	   +format-r64g64b64-sfloat+
	   +format-r64g64b64a64-uint+
	   +format-r64g64b64a64-sint+
	   +format-r64g64b64a64-sfloat+
	   +format-b10g11r11-ufloat-pack32+
	   +format-e5b9g9r9-ufloat-pack32+
	   +format-d16-unorm+
	   +format-x8-d24-unorm-pack32+
	   +format-d32-sfloat+
	   +format-s8-uint+
	   +format-d16-unorm-s8-uint+
	   +format-d24-unorm-s8-uint+
	   +format-d32-sfloat-s8-uint+
	   +format-bc1-rgb-unorm-block+
	   +format-bc1-rgb-srgb-block+
	   +format-bc1-rgba-unorm-block+
	   +format-bc1-rgba-srgb-block+
	   +format-bc2-unorm-block+
	   +format-bc2-srgb-block+
	   +format-bc3-unorm-block+
	   +format-bc3-srgb-block+
	   +format-bc4-unorm-block+
	   +format-bc4-snorm-block+
	   +format-bc5-unorm-block+
	   +format-bc5-snorm-block+
	   +format-bc6h-ufloat-block+
	   +format-bc6h-sfloat-block+
	   +format-bc7-unorm-block+
	   +format-bc7-srgb-block+
	   +format-etc2-r8g8b8-unorm-block+
	   +format-etc2-r8g8b8-srgb-block+
	   +format-etc2-r8g8b8a1-unorm-block+
	   +format-etc2-r8g8b8a1-srgb-block+
	   +format-etc2-r8g8b8a8-unorm-block+
	   +format-etc2-r8g8b8a8-srgb-block+
	   +format-eac-r11-unorm-block+
	   +format-eac-r11-snorm-block+
	   +format-eac-r11g11-unorm-block+
	   +format-eac-r11g11-snorm-block+
	   +format-astc-4x4-unorm-block+
	   +format-astc-4x4-srgb-block+
	   +format-astc-5x4-unorm-block+
	   +format-astc-5x4-srgb-block+
	   +format-astc-5x5-unorm-block+
	   +format-astc-5x5-srgb-block+
	   +format-astc-6x5-unorm-block+
	   +format-astc-6x5-srgb-block+
	   +format-astc-6x6-unorm-block+
	   +format-astc-6x6-srgb-block+
	   +format-astc-8x5-unorm-block+
	   +format-astc-8x5-srgb-block+
	   +format-astc-8x6-unorm-block+
	   +format-astc-8x6-srgb-block+
	   +format-astc-8x8-unorm-block+
	   +format-astc-8x8-srgb-block+
	   +format-astc-10x5-unorm-block+
	   +format-astc-10x5-srgb-block+
	   +format-astc-10x6-unorm-block+
	   +format-astc-10x6-srgb-block+
	   +format-astc-10x8-unorm-block+
	   +format-astc-10x8-srgb-block+
	   +format-astc-10x10-unorm-block+
	   +format-astc-10x10-srgb-block+
	   +format-astc-12x10-unorm-block+
	   +format-astc-12x10-srgb-block+
	   +format-astc-12x12-unorm-block+
	   +format-astc-12x12-srgb-block+
	   +format-pvrtc1-2bpp-unorm-block-img +
	   +format-pvrtc1-4bpp-unorm-block-img +
	   +format-pvrtc2-2bpp-unorm-block-img +
	   +format-pvrtc2-4bpp-unorm-block-img +
	   +format-pvrtc1-2bpp-srgb-block-img +
	   +format-pvrtc1-4bpp-srgb-block-img +
	   +format-pvrtc2-2bpp-srgb-block-img +
	   +format-pvrtc2-4bpp-srgb-block-img +
	   +format-feature-sampled-image-bit +
	   +format-feature-storage-image-bit +
	   +format-feature-storage-image-atomic-bit +
	   +format-feature-uniform-texel-buffer-bit +
	   +format-feature-storage-texel-buffer-bit +
	   +format-feature-storage-texel-buffer-atomic-bit +
	   +format-feature-vertex-buffer-bit +
	   +format-feature-color-attachment-bit +
	   +format-feature-color-attachment-blend-bit +
	   +format-feature-depth-stencil-attachment-bit +
	   +format-feature-blit-src-bit +
	   +format-feature-blit-dst-bit +
	   +format-feature-sampled-image-filter-linear-bit +
	   +format-feature-sampled-image-filter-cubic-bit-img +
	   +format-feature-transfer-src-bit-khr +
	   +format-feature-transfer-dst-bit-khr +
	   +front-face-counter-clockwise+
	   +front-face-clockwise+
	   +image-aspect-color-bit+
	   +image-aspect-depth-bit+
	   +image-aspect-stencil-bit+
	   +image-aspect-metadata-bit+
	   +image-create-sparse-binding-bit +
	   +image-create-sparse-residency-bit +
	   +image-create-sparse-aliased-bit +
	   +image-create-mutable-format-bit +
	   +image-create-cube-compatible-bit +
	   +image-create-bind-sfr-bit-khx +
	   +image-create-2d-array-compatible-bit-khr +
	   +image-layout-undefined +
	   +image-layout-general +
	   +image-layout-color-attachment-optimal +
	   +image-layout-depth-stencil-attachment-optimal +
	   +image-layout-depth-stencil-read-only-optimal +
	   +image-layout-shader-read-only-optimal +
	   +image-layout-transfer-src-optimal +
	   +image-layout-transfer-dst-optimal +
	   +image-layout-preinitialized +
	   +image-layout-present-src-khr +
	   +image-layout-shared-present-khr +
	   +image-tiling-optimal+
	   +image-tiling-linear+
	   +image-type-1d+
	   +image-type-2d+
	   +image-type-3d+
	   +image-usage-transfer-src-bit +
	   +image-usage-transfer-dst-bit +
	   +image-usage-sampled-bit +
	   +image-usage-storage-bit +
	   +image-usage-color-attachment-bit +
	   +image-usage-depth-stencil-attachment-bit +
	   +image-usage-transient-attachment-bit +
	   +image-usage-input-attachment-bit +
	   +image-view-type-1d+
	   +image-view-type-2d+
	   +image-view-type-3d+
	   +image-view-type-cube+
	   +image-view-type-1d-array+
	   +image-view-type-2d-array+
	   +image-view-type-cube-array+
	   +index-type-uint16+
	   +index-type-uint32+
	   +indirect-commands-layout-usage-unordered-sequences-bit-nvx+
	   +indirect-commands-layout-usage-sparse-sequences-bit-nvx+
	   +indirect-commands-layout-usage-empty-executions-bit-nvx+
	   +indirect-commands-layout-usage-indexed-sequences-bit-nvx+
	   +indirect-commands-token-type-pipeline-nvx+
	   +indirect-commands-token-type-descriptor-set-nvx+
	   +indirect-commands-token-type-index-buffer-nvx+
	   +indirect-commands-token-type-vertex-buffer-nvx+
	   +indirect-commands-token-type-push-constant-nvx+
	   +indirect-commands-token-type-draw-indexed-nvx+
	   +indirect-commands-token-type-draw-nvx+
	   +indirect-commands-token-type-dispatch-nvx+
	   +internal-allocation-type-executable+
	   +logic-op-clear+
	   +logic-op-and+
	   +logic-op-and-reverse+
	   +logic-op-copy+
	   +logic-op-and-inverted+
	   +logic-op-no-op+
	   +logic-op-xor+
	   +logic-op-or+
	   +logic-op-nor+
	   +logic-op-equivalent+
	   +logic-op-invert+
	   +logic-op-or-reverse+
	   +logic-op-copy-inverted+
	   +logic-op-or-inverted+
	   +logic-op-nand+
	   +logic-op-set+
	   +memory-allocate-device-mask-bit-khx +
	   +memory-heap-device-local-bit +
	   +memory-heap-multi-instance-bit-khx +
	   +memory-property-device-local-bit +
	   +memory-property-host-visible-bit +
	   +memory-property-host-coherent-bit +
	   +memory-property-host-cached-bit +
	   +memory-property-lazily-allocated-bit +
	   +object-entry-type-descriptor-set-nvx+
	   +object-entry-type-pipeline-nvx+
	   +object-entry-type-index-buffer-nvx+
	   +object-entry-type-vertex-buffer-nvx+
	   +object-entry-type-push-constant-nvx+
	   +object-entry-usage-graphics-bit-nvx+
	   +object-entry-usage-compute-bit-nvx+
	   +object-type-unknown+
	   +object-type-instance +
	   +object-type-physical-device +
	   +object-type-device +
	   +object-type-queue +
	   +object-type-semaphore +
	   +object-type-command-buffer +
	   +object-type-fence +
	   +object-type-device-memory +
	   +object-type-buffer +
	   +object-type-image +
	   +object-type-event +
	   +object-type-query-pool +
	   +object-type-buffer-view +
	   +object-type-image-view +
	   +object-type-shader-module +
	   +object-type-pipeline-cache +
	   +object-type-pipeline-layout +
	   +object-type-render-pass +
	   +object-type-pipeline +
	   +object-type-descriptor-set-layout +
	   +object-type-sampler +
	   +object-type-descriptor-pool +
	   +object-type-descriptor-set +
	   +object-type-framebuffer +
	   +object-type-command-pool +
	   +object-type-surface-khr +
	   +object-type-swapchain-khr +
	   +object-type-display-khr +
	   +object-type-display-mode-khr +
	   +object-type-debug-report-callback-ext +
	   +object-type-descriptor-update-template-khr +
	   +object-type-object-table-nvx +
	   +object-type-indirect-commands-layout-nvx +
	   +peer-memory-feature-copy-src-bit-khx +
	   +peer-memory-feature-copy-dst-bit-khx +
	   +peer-memory-feature-generic-src-bit-khx +
	   +peer-memory-feature-generic-dst-bit-khx +
	   +physical-device-type-other+
	   +physical-device-type-integrated-gpu+
	   +physical-device-type-discrete-gpu+
	   +physical-device-type-virtual-gpu+
	   +physical-device-type-cpu+
	   +pipeline-bind-point-graphics+
	   +pipeline-bind-point-compute+
	   +pipeline-cache-header-version-one+
	   +pipeline-create-disable-optimization-bit+
	   +pipeline-create-allow-derivatives-bit+
	   +pipeline-create-derivative-bit+
	   +pipeline-create-view-index-from-device-index-bit-khx +
	   +pipeline-create-dispatch-base-khx +
	   +pipeline-stage-top-of-pipe-bit +
	   +pipeline-stage-draw-indirect-bit +
	   +pipeline-stage-vertex-input-bit +
	   +pipeline-stage-vertex-shader-bit +
	   +pipeline-stage-tessellation-control-shader-bit +
	   +pipeline-stage-tessellation-evaluation-shader-bit +
	   +pipeline-stage-geometry-shader-bit +
	   +pipeline-stage-fragment-shader-bit +
	   +pipeline-stage-early-fragment-tests-bit +
	   +pipeline-stage-late-fragment-tests-bit +
	   +pipeline-stage-color-attachment-output-bit +
	   +pipeline-stage-compute-shader-bit +
	   +pipeline-stage-transfer-bit +
	   +pipeline-stage-bottom-of-pipe-bit +
	   +pipeline-stage-host-bit +
	   +pipeline-stage-all-graphics-bit +
	   +pipeline-stage-all-commands-bit +
	   +pipeline-stage-command-process-bit-nvx +
	   +polygon-mode-fill+
	   +polygon-mode-line+
	   +polygon-mode-point+
	   +present-mode-immediate-khr+
	   +present-mode-mailbox-khr+
	   +present-mode-fifo-khr+
	   +present-mode-fifo-relaxed-khr+
	   +present-mode-shared-demand-refresh-khr +
	   +present-mode-shared-continuous-refresh-khr +
	   +primitive-topology-point-list+
	   +primitive-topology-line-list+
	   +primitive-topology-line-strip+
	   +primitive-topology-triangle-list+
	   +primitive-topology-triangle-strip+
	   +primitive-topology-triangle-fan+
	   +primitive-topology-line-list-with-adjacency+
	   +primitive-topology-line-strip-with-adjacency+
	   +primitive-topology-triangle-list-with-adjacency+
	   +primitive-topology-triangle-strip-with-adjacency+
	   +primitive-topology-patch-list+
	   +query-control-precise-bit +
	   +query-pipeline-statistic-input-assembly-vertices-bit +
	   +query-pipeline-statistic-input-assembly-primitives-bit +
	   +query-pipeline-statistic-vertex-shader-invocations-bit +
	   +query-pipeline-statistic-geometry-shader-invocations-bit +
	   +query-pipeline-statistic-geometry-shader-primitives-bit +
	   +query-pipeline-statistic-clipping-invocations-bit +
	   +query-pipeline-statistic-clipping-primitives-bit +
	   +query-pipeline-statistic-fragment-shader-invocations-bit +
	   +query-pipeline-statistic-tessellation-control-shader-patches-bit +
	   +query-pipeline-statistic-tessellation-evaluation-shader-invocations-bit +
	   +query-pipeline-statistic-compute-shader-invocations-bit +
	   +query-result-64-bit +
	   +query-result-wait-bit +
	   +query-result-with-availability-bit +
	   +query-result-partial-bit +
	   +query-type-occlusion+
	   +query-type-pipeline-statistics +
	   +query-type-timestamp+
	   +queue-graphics-bit +
	   +queue-compute-bit +
	   +queue-transfer-bit +
	   +queue-sparse-binding-bit +
	   +rasterization-order-strict-amd+
	   +rasterization-order-relaxed-amd+
	   +success +
	   +not-ready +
	   +timeout +
	   +event-set +
	   +event-reset +
	   +incomplete +
	   +sample-count-1-bit +
	   +sample-count-2-bit +
	   +sample-count-4-bit +
	   +sample-count-8-bit +
	   +sample-count-16-bit +
	   +sample-count-32-bit +
	   +sample-count-64-bit +
	   +sampler-address-mode-repeat+
	   +sampler-address-mode-mirrored-repeat+
	   +sampler-address-mode-clamp-to-edge+
	   +sampler-address-mode-clamp-to-border+
	   +sampler-address-mode-mirror-clamp-to-edge +
	   +sampler-mipmap-mode-nearest +
	   +sampler-mipmap-mode-linear +
	   +shader-stage-vertex-bit+
	   +shader-stage-tessellation-control-bit+
	   +shader-stage-tessellation-evaluation-bit+
	   +shader-stage-geometry-bit+
	   +shader-stage-fragment-bit+
	   +shader-stage-compute-bit+
	   +shader-stage-all-graphics+
	   +shader-stage-all+
	   +sharing-mode-exclusive+
	   +sharing-mode-concurrent+
	   +sparse-image-format-single-miptail-bit +
	   +sparse-image-format-aligned-mip-size-bit +
	   +sparse-image-format-nonstandard-block-size-bit +
	   +sparse-memory-bind-metadata-bit +
	   +stencil-face-front-bit +
	   +stencil-face-back-bit +
	   +stencil-front-and-back +
	   +stencil-op-keep+
	   +stencil-op-zero+
	   +stencil-op-replace+
	   +stencil-op-increment-and-clamp+
	   +stencil-op-decrement-and-clamp+
	   +stencil-op-invert+
	   +stencil-op-increment-and-wrap+
	   +stencil-op-decrement-and-wrap+
	   +subpass-contents-inline+
	   +subpass-contents-secondary-command-buffers+
	   +subpass-description-per-view-attributes-bit-nvx +
	   +subpass-description-per-view-position-x-only-bit-nvx +
	   +surface-counter-vblank-ext+
	   +surface-transform-identity-bit-khr+
	   +surface-transform-rotate-90-bit-khr+
	   +surface-transform-rotate-180-bit-khr+
	   +surface-transform-rotate-270-bit-khr+
	   +surface-transform-horizontal-mirror-bit-khr+
	   +surface-transform-horizontal-mirror-rotate-90-bit-khr+
	   +surface-transform-horizontal-mirror-rotate-180-bit-khr+
	   +surface-transform-horizontal-mirror-rotate-270-bit-khr+
	   +surface-transform-inherit-bit-khr+
	   +swapchain-create-bind-sfr-bit-khx +
	   +system-allocation-scope-command+
	   +system-allocation-scope-object+
	   +system-allocation-scope-cache+
	   +system-allocation-scope-device+
	   +system-allocation-scope-instance+
	   +validation-check-all-ext+
	   +validation-check-shaders-ext+
	   +vertex-input-rate-vertex+
	   +vertex-input-rate-instance+
	   +viewport-coordinate-swizzle-positive-x-nv+
	   +viewport-coordinate-swizzle-negative-x-nv+
	   +viewport-coordinate-swizzle-positive-y-nv+
	   +viewport-coordinate-swizzle-negative-y-nv+
	   +viewport-coordinate-swizzle-positive-z-nv+
	   +viewport-coordinate-swizzle-negative-z-nv+
	   +viewport-coordinate-swizzle-positive-w-nv+
	   +viewport-coordinate-swizzle-negative-w-nv+))

(in-package #:vkvk)
