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

(defconstant +access-indirect-command-read-bit+ #x1)
(defconstant +access-index-read-bit+ #x2)
(defconstant +access-vertex-attribute-read-bit+ #x4)
(defconstant +access-uniform-read-bit+ #x8)
(defconstant +access-input-attachment-read-bit+ #x10)
(defconstant +access-shader-read-bit+ #x20)
(defconstant +access-shader-write-bit+ #x40)
(defconstant +access-color-attachment-read-bit+ #x80)
(defconstant +access-color-attachment-write-bit+ #x100)
(defconstant +access-depth-stencil-attachment-read-bit+ #x200)
(defconstant +access-depth-stencil-attachment-write-bit+ #x400)
(defconstant +access-transfer-read-bit+ #x800)
(defconstant +access-transfer-write-bit+ #x1000)
(defconstant +access-host-read-bit+ #x2000)
(defconstant +access-host-write-bit+ #x4000)
(defconstant +access-memory-read-bit+ #x8000)
(defconstant +access-memory-write-bit+ #x10000)
(defconstant +access-command-process-read-bit-nvx+ #x20000)
(defconstant +access-command-process-write-bit-nvx+ #x40000)

(defconstant +attachment-description-may-alias-bit+ #x1)

(defconstant +buffer-create-sparse-binding-bit+ #x1)
(defconstant +buffer-create-sparse-residency-bit+ #x2)
(defconstant +buffer-create-sparse-aliased-bit+ #x4)

(defconstant +buffer-usage-transfer-src-bit+ #x1)
(defconstant +buffer-usage-transfer-dst-bit+ #x2)
(defconstant +buffer-usage-uniform-texel-buffer-bit+ #x4)
(defconstant +buffer-usage-storage-texel-buffer-bit+ #x8)
(defconstant +buffer-usage-uniform-buffer-bit+ #x10)
(defconstant +buffer-usage-storage-buffer-bit+ #x20)
(defconstant +buffer-usage-index-buffer-bit+ #x40)
(defconstant +buffer-usage-vertex-buffer-bit+ #x80)
(defconstant +buffer-usage-indirect-buffer-bit+ #x100)

(defconstant +color-component-r-bit+ #x1)
(defconstant +color-component-g-bit+ #x2)
(defconstant +color-component-b-bit+ #x4)
(defconstant +color-component-a-bit+ #x8)

(defconstant +command-buffer-reset-release-resources-bit+ #x1)

(defconstant +command-buffer-usage-one-time-submit-bit+ #x1)
(defconstant +command-buffer-usage-render-pass-continue-bit+ #x2)
(defconstant +command-buffer-usage-simultaneous-use-bit+ #x4)

(defconstant +command-pool-create-transient-bit+ #x1)
(defconstant +command-pool-create-reset-command-buffer-bit+ #x2)

(defconstant +command-pool-reset-release-resources-bit+ #x1)

(defconstant +composite-alpha-opaque-bit-khr+ #x1)
(defconstant +composite-alpha-pre-multiplied-bit-khr+ #x2)
(defconstant +composite-alpha-post-multiplied-bit-khr+ #x4)
(defconstant +composite-alpha-inherit-bit-khr+ #x8)

(defconstant +cull-mode-none+ #x0)
(defconstant +cull-mode-front-bit+ #x1)
(defconstant +cull-mode-back-bit+ #x2)
(defconstant +cull-mode-front-and-back+ #x3)

(defconstant +debug-report-information-bit-ext+ #x1)
(defconstant +debug-report-warning-bit-ext+ #x2)
(defconstant +debug-report-performance-warning-bit-ext+ #x4)
(defconstant +debug-report-error-bit-ext+ #x8)
(defconstant +debug-report-debug-bit-ext+ #x10)

(defconstant +dependency-by-region-bit+ #x1)
(defconstant +dependency-view-local-bit-khx+ #x2)
(defconstant +dependency-device-group-bit-khx+ #x4)

(defconstant +descriptor-pool-create-free-descriptor-set-bit+ #x1)

(defconstant +descriptor-set-layout-create-push-descriptor-bit-khr+ #x1)

(defconstant +device-group-present-mode-local-bit-khx+ #x1)
(defconstant +device-group-present-mode-remote-bit-khx+ #x2)
(defconstant +device-group-present-mode-sum-bit-khx+ #x4)
(defconstant +device-group-present-mode-local-multi-device-bit-khx+ #x8)

(defconstant +display-plane-alpha-opaque-bit-khr+ #x1)
(defconstant +display-plane-alpha-global-bit-khr+ #x2)
(defconstant +display-plane-alpha-per-pixel-bit-khr+ #x4)
(defconstant +display-plane-alpha-per-pixel-premultiplied-bit-khr+ #x8)

(defconstant +external-memory-feature-dedicated-only-bit-khx+ #x1)
(defconstant +external-memory-feature-exportable-bit-khx+ #x2)
(defconstant +external-memory-feature-importable-bit-khx+ #x4)

(defconstant +external-memory-feature-dedicated-only-bit-nv+ #x1)
(defconstant +external-memory-feature-exportable-bit-nv+ #x2)
(defconstant +external-memory-feature-importable-bit-nv+ #x4)

(defconstant +external-memory-handle-type-opaque-fd-bit-khx+ #x1)
(defconstant +external-memory-handle-type-opaque-win32-bit-khx+ #x2)
(defconstant +external-memory-handle-type-opaque-win32-kmt-bit-khx+ #x4)
(defconstant +external-memory-handle-type-d3d11-texture-bit-khx+ #x8)
(defconstant +external-memory-handle-type-d3d11-texture-kmt-bit-khx+ #x10)
(defconstant +external-memory-handle-type-d3d12-heap-bit-khx+ #x20)
(defconstant +external-memory-handle-type-d3d12-resource-bit-khx+ #x40)

(defconstant +external-memory-handle-type-opaque-win32-bit-nv+ #x1)
(defconstant +external-memory-handle-type-opaque-win32-kmt-bit-nv+ #x2)
(defconstant +external-memory-handle-type-d3d11-image-bit-nv+ #x4)
(defconstant +external-memory-handle-type-d3d11-image-kmt-bit-nv+ #x8)

(defconstant +external-semaphore-feature-exportable-bit-khx+ #x1)
(defconstant +external-semaphore-feature-importable-bit-khx+ #x2)

(defconstant +external-semaphore-handle-type-opaque-fd-bit-khx+ #x1)
(defconstant +external-semaphore-handle-type-opaque-win32-bit-khx+ #x2)
(defconstant +external-semaphore-handle-type-opaque-win32-kmt-bit-khx+ #x4)
(defconstant +external-semaphore-handle-type-d3d12-fence-bit-khx+ #x8)
(defconstant +external-semaphore-handle-type-fence-fd-bit-khx+ #x10)

(defconstant +fence-create-signaled-bit+ #x1)

(defconstant +format-feature-sampled-image-bit+ #x1)
(defconstant +format-feature-storage-image-bit+ #x2)
(defconstant +format-feature-storage-image-atomic-bit+ #x4)
(defconstant +format-feature-uniform-texel-buffer-bit+ #x8)
(defconstant +format-feature-storage-texel-buffer-bit+ #x10)
(defconstant +format-feature-storage-texel-buffer-atomic-bit+ #x20)
(defconstant +format-feature-vertex-buffer-bit+ #x40)
(defconstant +format-feature-color-attachment-bit+ #x80)
(defconstant +format-feature-color-attachment-blend-bit+ #x100)
(defconstant +format-feature-depth-stencil-attachment-bit+ #x200)
(defconstant +format-feature-blit-src-bit+ #x400)
(defconstant +format-feature-blit-dst-bit+ #x800)
(defconstant +format-feature-sampled-image-filter-linear-bit+ #x1000)
(defconstant +format-feature-sampled-image-filter-cubic-bit-img+ #x2000)
(defconstant +format-feature-transfer-src-bit-khr+ #x4000)
(defconstant +format-feature-transfer-dst-bit-khr+ #x8000)

(defconstant +image-aspect-color-bit+ #x1)
(defconstant +image-aspect-depth-bit+ #x2)
(defconstant +image-aspect-stencil-bit+ #x4)
(defconstant +image-aspect-metadata-bit+ #x8)

(defconstant +image-create-sparse-binding-bit+ #x1)
(defconstant +image-create-sparse-residency-bit+ #x2)
(defconstant +image-create-sparse-aliased-bit+ #x4)
(defconstant +image-create-mutable-format-bit+ #x8)
(defconstant +image-create-cube-compatible-bit+ #x10)
(defconstant +image-create-bind-sfr-bit-khx+ #x40)
(defconstant +image-create-2d-array-compatible-bit-khr+ #x20)

(defconstant +image-usage-transfer-src-bit+ #x1)
(defconstant +image-usage-transfer-dst-bit+ #x2)
(defconstant +image-usage-sampled-bit+ #x4)
(defconstant +image-usage-storage-bit+ #x8)
(defconstant +image-usage-color-attachment-bit+ #x10)
(defconstant +image-usage-depth-stencil-attachment-bit+ #x20)
(defconstant +image-usage-transient-attachment-bit+ #x40)
(defconstant +image-usage-input-attachment-bit+ #x80)

(defconstant +indirect-commands-layout-usage-unordered-sequences-bit-nvx+ #x1)
(defconstant +indirect-commands-layout-usage-sparse-sequences-bit-nvx+ #x2)
(defconstant +indirect-commands-layout-usage-empty-executions-bit-nvx+ #x4)
(defconstant +indirect-commands-layout-usage-indexed-sequences-bit-nvx+ #x8)

(defconstant +memory-allocate-device-mask-bit-khx+ #x1)

(defconstant +memory-heap-device-local-bit+ #x1)
(defconstant +memory-heap-multi-instance-bit-khx+ #x2)

(defconstant +memory-property-device-local-bit+ #x1)
(defconstant +memory-property-host-visible-bit+ #x2)
(defconstant +memory-property-host-coherent-bit+ #x4)
(defconstant +memory-property-host-cached-bit+ #x8)
(defconstant +memory-property-lazily-allocated-bit+ #x10)

(defconstant +object-entry-usage-graphics-bit-nvx+ #x1)
(defconstant +object-entry-usage-compute-bit-nvx+ #x2)

(defconstant +peer-memory-feature-copy-src-bit-khx+ #x1)
(defconstant +peer-memory-feature-copy-dst-bit-khx+ #x2)
(defconstant +peer-memory-feature-generic-src-bit-khx+ #x4)
(defconstant +peer-memory-feature-generic-dst-bit-khx+ #x8)

(defconstant +pipeline-create-disable-optimization-bit+ #x1)
(defconstant +pipeline-create-allow-derivatives-bit+ #x2)
(defconstant +pipeline-create-derivative-bit+ #x4)
(defconstant +pipeline-create-view-index-from-device-index-bit-khx+ #x8)
(defconstant +pipeline-create-dispatch-base-khx+ #x10)

(defconstant +pipeline-stage-top-of-pipe-bit+ #x1)
(defconstant +pipeline-stage-draw-indirect-bit+ #x2)
(defconstant +pipeline-stage-vertex-input-bit+ #x4)
(defconstant +pipeline-stage-vertex-shader-bit+ #x8)
(defconstant +pipeline-stage-tessellation-control-shader-bit+ #x10)
(defconstant +pipeline-stage-tessellation-evaluation-shader-bit+ #x20)
(defconstant +pipeline-stage-geometry-shader-bit+ #x40)
(defconstant +pipeline-stage-fragment-shader-bit+ #x80)
(defconstant +pipeline-stage-early-fragment-tests-bit+ #x100)
(defconstant +pipeline-stage-late-fragment-tests-bit+ #x200)
(defconstant +pipeline-stage-color-attachment-output-bit+ #x400)
(defconstant +pipeline-stage-compute-shader-bit+ #x800)
(defconstant +pipeline-stage-transfer-bit+ #x1000)
(defconstant +pipeline-stage-bottom-of-pipe-bit+ #x2000)
(defconstant +pipeline-stage-host-bit+ #x4000)
(defconstant +pipeline-stage-all-graphics-bit+ #x8000)
(defconstant +pipeline-stage-all-commands-bit+ #x10000)
(defconstant +pipeline-stage-command-process-bit-nvx+ #x20000)

(defconstant +query-control-precise-bit+ #x1)

(defconstant +query-pipeline-statistic-input-assembly-vertices-bit+ #x1)
(defconstant +query-pipeline-statistic-input-assembly-primitives-bit+ #x2)
(defconstant +query-pipeline-statistic-vertex-shader-invocations-bit+ #x4)
(defconstant +query-pipeline-statistic-geometry-shader-invocations-bit+ #x8)
(defconstant +query-pipeline-statistic-geometry-shader-primitives-bit+ #x10)
(defconstant +query-pipeline-statistic-clipping-invocations-bit+ #x20)
(defconstant +query-pipeline-statistic-clipping-primitives-bit+ #x40)
(defconstant +query-pipeline-statistic-fragment-shader-invocations-bit+ #x80)
(defconstant +query-pipeline-statistic-tessellation-control-shader-patches-bit+ #x100)
(defconstant +query-pipeline-statistic-tessellation-evaluation-shader-invocations-bit+ #x200)
(defconstant +query-pipeline-statistic-compute-shader-invocations-bit+ #x400)

(defconstant +query-result-64-bit+ #x1)
(defconstant +query-result-wait-bit+ #x2)
(defconstant +query-result-with-availability-bit+ #x4)
(defconstant +query-result-partial-bit+ #x8)

(defconstant +queue-graphics-bit+ #x1)
(defconstant +queue-compute-bit+ #x2)
(defconstant +queue-transfer-bit+ #x4)
(defconstant +queue-sparse-binding-bit+ #x8)

(defconstant +sample-count-1-bit+ #x1)
(defconstant +sample-count-2-bit+ #x2)
(defconstant +sample-count-4-bit+ #x4)
(defconstant +sample-count-8-bit+ #x8)
(defconstant +sample-count-16-bit+ #x10)
(defconstant +sample-count-32-bit+ #x20)
(defconstant +sample-count-64-bit+ #x40)

(defconstant +shader-stage-vertex-bit+ #x1)
(defconstant +shader-stage-tessellation-control-bit+ #x2)
(defconstant +shader-stage-tessellation-evaluation-bit+ #x4)
(defconstant +shader-stage-geometry-bit+ #x8)
(defconstant +shader-stage-fragment-bit+ #x10)
(defconstant +shader-stage-compute-bit+ #x20)
(defconstant +shader-stage-all-graphics+ #x1f)
(defconstant +shader-stage-all+ #x7fffffff)

(defconstant +sparse-image-format-single-miptail-bit+ #x1)
(defconstant +sparse-image-format-aligned-mip-size-bit+ #x2)
(defconstant +sparse-image-format-nonstandard-block-size-bit+ #x4)

(defconstant +sparse-memory-bind-metadata-bit+ #x1)

(defconstant +stencil-face-front-bit+ #x1)
(defconstant +stencil-face-back-bit+ #x2)
(defconstant +stencil-front-and-back+ #x3)

(defconstant +subpass-description-per-view-attributes-bit-nvx+ #x1)
(defconstant +subpass-description-per-view-position-x-only-bit-nvx+ #x2)

(defconstant +surface-counter-vblank-ext+ #x1)

(defconstant +surface-transform-identity-bit-khr+ #x1)
(defconstant +surface-transform-rotate-90-bit-khr+ #x2)
(defconstant +surface-transform-rotate-180-bit-khr+ #x4)
(defconstant +surface-transform-rotate-270-bit-khr+ #x8)
(defconstant +surface-transform-horizontal-mirror-bit-khr+ #x10)
(defconstant +surface-transform-horizontal-mirror-rotate-90-bit-khr+ #x20)
(defconstant +surface-transform-horizontal-mirror-rotate-180-bit-khr+ #x40)
(defconstant +surface-transform-horizontal-mirror-rotate-270-bit-khr+ #x80)
(defconstant +surface-transform-inherit-bit-khr+ #x100)

(defconstant +swapchain-create-bind-sfr-bit-khx+ #x1)

(defconstant +access-indirect-command-read-bit+ #x1)+ ;;+ controls+ coherency+ of+ indirect+ command+ reads
(defconstant +access-index-read-bit+ #x2)+ ;;+ controls+ coherency+ of+ index+ reads
(defconstant +access-vertex-attribute-read-bit+ #x4)+ ;;+ controls+ coherency+ of+ vertex+ attribute+ reads
(defconstant +access-uniform-read-bit+ #x8)+ ;;+ controls+ coherency+ of+ uniform+ buffer+ reads
(defconstant +access-input-attachment-read-bit+ #x10)+ ;;+ controls+ coherency+ of+ input+ attachment+ reads
(defconstant +access-shader-read-bit+ #x20)+ ;;+ controls+ coherency+ of+ shader+ reads
(defconstant +access-shader-write-bit+ #x40)+ ;;+ controls+ coherency+ of+ shader+ writes
(defconstant +access-color-attachment-read-bit+ #x80)+ ;;+ controls+ coherency+ of+ color+ attachment+ reads
(defconstant +access-color-attachment-write-bit+ #x100)+ ;;+ controls+ coherency+ of+ color+ attachment+ writes
(defconstant +access-depth-stencil-attachment-read-bit+ #x200)+ ;;+ controls+ coherency+ of+ depth/stencil+ attachment+ reads
(defconstant +access-depth-stencil-attachment-write-bit+ #x400)+ ;;+ controls+ coherency+ of+ depth/stencil+ attachment+ writes
(defconstant +access-transfer-read-bit+ #x800)+ ;;+ controls+ coherency+ of+ transfer+ reads
(defconstant +access-transfer-write-bit+ #x1000)+ ;;+ controls+ coherency+ of+ transfer+ writes
(defconstant +access-host-read-bit+ #x2000)+ ;;+ controls+ coherency+ of+ host+ reads
(defconstant +access-host-write-bit+ #x4000)+ ;;+ controls+ coherency+ of+ host+ writes
(defconstant +access-memory-read-bit+ #x8000)+ ;;+ controls+ coherency+ of+ memory+ reads
(defconstant +access-memory-write-bit+ #x10000)+ ;;+ controls+ coherency+ of+ memory+ writes
(defconstant +access-command-process-read-bit-nvx+ #x20000)+ ;;+ "nvx-device-generated-commands"
(defconstant +access-command-process-write-bit-nvx+ #x40000)+ ;;+ "nvx-device-generated-commands"

(defconstant +attachment-description-may-alias-bit+ #x1)+ ;;+ the+ attachment+ may+ alias+ physical+ memory+ of+ another+ attachment+ in+ the+ same+ render+ pass

(defconstant +attachment-load-op-load+ #x0)
(defconstant +attachment-load-op-clear+ #x1)
(defconstant +attachment-load-op-dont-care+ #x2)

(defconstant +attachment-store-op-store+ #x0)
(defconstant +attachment-store-op-dont-care+ #x1)

(defconstant +blend-factor-zero+ #x0)
(defconstant +blend-factor-one+ #x1)
(defconstant +blend-factor-src-color+ #x2)
(defconstant +blend-factor-one-minus-src-color+ #x3)
(defconstant +blend-factor-dst-color+ #x4)
(defconstant +blend-factor-one-minus-dst-color+ #x5)
(defconstant +blend-factor-src-alpha+ #x6)
(defconstant +blend-factor-one-minus-src-alpha+ #x7)
(defconstant +blend-factor-dst-alpha+ #x8)
(defconstant +blend-factor-one-minus-dst-alpha+ #x9)
(defconstant +blend-factor-constant-color+ #xa)
(defconstant +blend-factor-one-minus-constant-color+ #xb)
(defconstant +blend-factor-constant-alpha+ #xc)
(defconstant +blend-factor-one-minus-constant-alpha+ #xd)
(defconstant +blend-factor-src-alpha-saturate+ #xe)
(defconstant +blend-factor-src1-color+ #xf)
(defconstant +blend-factor-one-minus-src1-color+ #x10)
(defconstant +blend-factor-src1-alpha+ #x11)
(defconstant +blend-factor-one-minus-src1-alpha+ #x12)

(defconstant +blend-op-add+ #x0)
(defconstant +blend-op-subtract+ #x1)
(defconstant +blend-op-reverse-subtract+ #x2)
(defconstant +blend-op-min+ #x3)
(defconstant +blend-op-max+ #x4)

(defconstant +border-color-float-transparent-black+ #x0)
(defconstant +border-color-int-transparent-black+ #x1)
(defconstant +border-color-float-opaque-black+ #x2)
(defconstant +border-color-int-opaque-black+ #x3)
(defconstant +border-color-float-opaque-white+ #x4)
(defconstant +border-color-int-opaque-white+ #x5)

(defconstant +buffer-create-sparse-binding-bit+ #x1)+ ;;+ buffer+ should+ support+ sparse+ backing
(defconstant +buffer-create-sparse-residency-bit+ #x2)+ ;;+ buffer+ should+ support+ sparse+ backing+ with+ partial+ residency
(defconstant +buffer-create-sparse-aliased-bit+ #x4)+ ;;+ buffer+ should+ support+ constent+ data+ access+ to+ physical+ memory+ ranges+ mapped+ into+ multiple+ locations+ of+ sparse+ buffers

(defconstant +buffer-usage-transfer-src-bit+ #x1)+ ;;+ can+ be+ used+ as+ a+ source+ of+ transfer+ operations
(defconstant +buffer-usage-transfer-dst-bit+ #x2)+ ;;+ can+ be+ used+ as+ a+ destination+ of+ transfer+ operations
(defconstant +buffer-usage-uniform-texel-buffer-bit+ #x4)+ ;;+ can+ be+ used+ as+ tbo
(defconstant +buffer-usage-storage-texel-buffer-bit+ #x8)+ ;;+ can+ be+ used+ as+ ibo
(defconstant +buffer-usage-uniform-buffer-bit+ #x10)+ ;;+ can+ be+ used+ as+ ubo
(defconstant +buffer-usage-storage-buffer-bit+ #x20)+ ;;+ can+ be+ used+ as+ ssbo
(defconstant +buffer-usage-index-buffer-bit+ #x40)+ ;;+ can+ be+ used+ as+ source+ of+ fixed-function+ index+ fetch+ (index+ buffer)
(defconstant +buffer-usage-vertex-buffer-bit+ #x80)+ ;;+ can+ be+ used+ as+ source+ of+ fixed-function+ vertex+ fetch+ (vbo)
(defconstant +buffer-usage-indirect-buffer-bit+ #x100)+ ;;+ can+ be+ the+ source+ of+ indirect+ parameters+ (e.g.+ indirect+ buffer,+ parameter+ buffer)

(defconstant +color-component-r-bit+ #x1)
(defconstant +color-component-g-bit+ #x2)
(defconstant +color-component-b-bit+ #x4)
(defconstant +color-component-a-bit+ #x8)

(defconstant +color-space-srgb-nonlinear-khr+ #x0)
(defconstant +color-space-display-p3-nonlinear-ext+ #x3b9c6041)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-extended-srgb-linear-ext+ #x3b9c6042)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-dci-p3-linear-ext+ #x3b9c6043)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-dci-p3-nonlinear-ext+ #x3b9c6044)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-bt709-linear-ext+ #x3b9c6045)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-bt709-nonlinear-ext+ #x3b9c6046)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-bt2020-linear-ext+ #x3b9c6047)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-hdr10-st2084-ext+ #x3b9c6048)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-dolbyvision-ext+ #x3b9c6049)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-hdr10-hlg-ext+ #x3b9c604a)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-adobergb-linear-ext+ #x3b9c604b)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-adobergb-nonlinear-ext+ #x3b9c604c)+ ;;+ "ext-swapchain-colorspace"
(defconstant +color-space-pass-through-ext+ #x3b9c604d)+ ;;+ "ext-swapchain-colorspace"

(defconstant +command-buffer-level-primary+ #x0)
(defconstant +command-buffer-level-secondary+ #x1)

(defconstant +command-buffer-reset-release-resources-bit+ #x1)+ ;;+ release+ resources+ owned+ by+ the+ buffer

(defconstant +command-buffer-usage-one-time-submit-bit+ #x1)
(defconstant +command-buffer-usage-render-pass-continue-bit+ #x2)
(defconstant +command-buffer-usage-simultaneous-use-bit+ #x4)+ ;;+ command+ buffer+ may+ be+ submitted/executed+ more+ than+ once+ simultaneously

(defconstant +command-pool-create-transient-bit+ #x1)+ ;;+ command+ buffers+ have+ a+ short+ lifetime
(defconstant +command-pool-create-reset-command-buffer-bit+ #x2)+ ;;+ command+ buffers+ may+ release+ their+ memory+ individually

(defconstant +command-pool-reset-release-resources-bit+ #x1)+ ;;+ release+ resources+ owned+ by+ the+ pool

(defconstant +compare-op-never+ #x0)
(defconstant +compare-op-less+ #x1)
(defconstant +compare-op-equal+ #x2)
(defconstant +compare-op-less-or-equal+ #x3)
(defconstant +compare-op-greater+ #x4)
(defconstant +compare-op-not-equal+ #x5)
(defconstant +compare-op-greater-or-equal+ #x6)
(defconstant +compare-op-always+ #x7)

(defconstant +component-swizzle-identity+ #x0)
(defconstant +component-swizzle-zero+ #x1)
(defconstant +component-swizzle-one+ #x2)
(defconstant +component-swizzle-r+ #x3)
(defconstant +component-swizzle-g+ #x4)
(defconstant +component-swizzle-b+ #x5)
(defconstant +component-swizzle-a+ #x6)

(defconstant +composite-alpha-opaque-bit-khr+ #x1)
(defconstant +composite-alpha-pre-multiplied-bit-khr+ #x2)
(defconstant +composite-alpha-post-multiplied-bit-khr+ #x4)
(defconstant +composite-alpha-inherit-bit-khr+ #x8)

(defconstant +cull-mode-none+ #x0)
(defconstant +cull-mode-front-bit+ #x1)
(defconstant +cull-mode-back-bit+ #x2)
(defconstant +cull-mode-front-and-back+ #x3)

(defconstant +debug-report-information-bit-ext+ #x1)
(defconstant +debug-report-warning-bit-ext+ #x2)
(defconstant +debug-report-performance-warning-bit-ext+ #x4)
(defconstant +debug-report-error-bit-ext+ #x8)
(defconstant +debug-report-debug-bit-ext+ #x10)

(defconstant +debug-report-object-type-unknown-ext+ #x0)
(defconstant +debug-report-object-type-instance-ext+ #x1)
(defconstant +debug-report-object-type-physical-device-ext+ #x2)
(defconstant +debug-report-object-type-device-ext+ #x3)
(defconstant +debug-report-object-type-queue-ext+ #x4)
(defconstant +debug-report-object-type-semaphore-ext+ #x5)
(defconstant +debug-report-object-type-command-buffer-ext+ #x6)
(defconstant +debug-report-object-type-fence-ext+ #x7)
(defconstant +debug-report-object-type-device-memory-ext+ #x8)
(defconstant +debug-report-object-type-buffer-ext+ #x9)
(defconstant +debug-report-object-type-image-ext+ #xa)
(defconstant +debug-report-object-type-event-ext+ #xb)
(defconstant +debug-report-object-type-query-pool-ext+ #xc)
(defconstant +debug-report-object-type-buffer-view-ext+ #xd)
(defconstant +debug-report-object-type-image-view-ext+ #xe)
(defconstant +debug-report-object-type-shader-module-ext+ #xf)
(defconstant +debug-report-object-type-pipeline-cache-ext+ #x10)
(defconstant +debug-report-object-type-pipeline-layout-ext+ #x11)
(defconstant +debug-report-object-type-render-pass-ext+ #x12)
(defconstant +debug-report-object-type-pipeline-ext+ #x13)
(defconstant +debug-report-object-type-descriptor-set-layout-ext+ #x14)
(defconstant +debug-report-object-type-sampler-ext+ #x15)
(defconstant +debug-report-object-type-descriptor-pool-ext+ #x16)
(defconstant +debug-report-object-type-descriptor-set-ext+ #x17)
(defconstant +debug-report-object-type-framebuffer-ext+ #x18)
(defconstant +debug-report-object-type-command-pool-ext+ #x19)
(defconstant +debug-report-object-type-surface-khr-ext+ #x1a)
(defconstant +debug-report-object-type-swapchain-khr-ext+ #x1b)
(defconstant +debug-report-object-type-debug-report-callback-ext-ext+ #x1c)
(defconstant +debug-report-object-type-display-khr-ext+ #x1d)
(defconstant +debug-report-object-type-display-mode-khr-ext+ #x1e)
(defconstant +debug-report-object-type-object-table-nvx-ext+ #x1f)
(defconstant +debug-report-object-type-indirect-commands-layout-nvx-ext+ #x20)
(defconstant +debug-report-object-type-descriptor-update-template-khr-ext+ #x3b9c1608)+ ;;+ "khr-descriptor-update-template"

(defconstant +dependency-by-region-bit+ #x1)+ ;;+ dependency+ is+ per+ pixel+ region
(defconstant +dependency-view-local-bit-khx+ #x2)+ ;;+ "khx-multiview"
(defconstant +dependency-device-group-bit-khx+ #x4)+ ;;+ "khx-device-group"

(defconstant +descriptor-pool-create-free-descriptor-set-bit+ #x1)+ ;;+ descriptor+ sets+ may+ be+ freed+ individually

(defconstant +descriptor-set-layout-create-push-descriptor-bit-khr+ #x1)+ ;;+ "khr-push-descriptor"

(defconstant +descriptor-type-sampler+ #x0)
(defconstant +descriptor-type-combined-image-sampler+ #x1)
(defconstant +descriptor-type-sampled-image+ #x2)
(defconstant +descriptor-type-storage-image+ #x3)
(defconstant +descriptor-type-uniform-texel-buffer+ #x4)
(defconstant +descriptor-type-storage-texel-buffer+ #x5)
(defconstant +descriptor-type-uniform-buffer+ #x6)
(defconstant +descriptor-type-storage-buffer+ #x7)
(defconstant +descriptor-type-uniform-buffer-dynamic+ #x8)
(defconstant +descriptor-type-storage-buffer-dynamic+ #x9)
(defconstant +descriptor-type-input-attachment+ #xa)

(defconstant +descriptor-update-template-type-descriptor-set-khr+ #x0)+ ;;+ create+ descriptor+ update+ template+ for+ descriptor+ set+ updates
(defconstant +descriptor-update-template-type-push-descriptors-khr+ #x1)+ ;;+ create+ descriptor+ update+ template+ for+ pushed+ descriptor+ updates

(defconstant +device-event-type-display-hotplug-ext+ #x0)

(defconstant +device-group-present-mode-local-bit-khx+ #x1)+ ;;+ present+ from+ local+ memory
(defconstant +device-group-present-mode-remote-bit-khx+ #x2)+ ;;+ present+ from+ remote+ memory
(defconstant +device-group-present-mode-sum-bit-khx+ #x4)+ ;;+ present+ sum+ of+ local+ and/or+ remote+ memory
(defconstant +device-group-present-mode-local-multi-device-bit-khx+ #x8)+ ;;+ each+ physical+ device+ presents+ from+ local+ memory

(defconstant +discard-rectangle-mode-inclusive-ext+ #x0)
(defconstant +discard-rectangle-mode-exclusive-ext+ #x1)

(defconstant +display-event-type-first-pixel-out-ext+ #x0)

(defconstant +display-plane-alpha-opaque-bit-khr+ #x1)
(defconstant +display-plane-alpha-global-bit-khr+ #x2)
(defconstant +display-plane-alpha-per-pixel-bit-khr+ #x4)
(defconstant +display-plane-alpha-per-pixel-premultiplied-bit-khr+ #x8)

(defconstant +display-power-state-off-ext+ #x0)
(defconstant +display-power-state-suspend-ext+ #x1)
(defconstant +display-power-state-on-ext+ #x2)

(defconstant +dynamic-state-viewport+ #x0)
(defconstant +dynamic-state-scissor+ #x1)
(defconstant +dynamic-state-line-width+ #x2)
(defconstant +dynamic-state-depth-bias+ #x3)
(defconstant +dynamic-state-blend-constants+ #x4)
(defconstant +dynamic-state-depth-bounds+ #x5)
(defconstant +dynamic-state-stencil-compare-mask+ #x6)
(defconstant +dynamic-state-stencil-write-mask+ #x7)
(defconstant +dynamic-state-stencil-reference+ #x8)
(defconstant +dynamic-state-viewport-w-scaling-nv+ #x3b9c1dd8)+ ;;+ "nv-clip-space-w-scaling"
(defconstant +dynamic-state-discard-rectangle-ext+ #x3b9c4cb8)+ ;;+ "ext-discard-rectangles"
(defconstant +dynamic-state-line-stipple-ext+ 1000259000)

(defconstant +external-memory-feature-dedicated-only-bit-khx+ #x1)
(defconstant +external-memory-feature-exportable-bit-khx+ #x2)
(defconstant +external-memory-feature-importable-bit-khx+ #x4)

(defconstant +external-memory-feature-dedicated-only-bit-nv+ #x1)
(defconstant +external-memory-feature-exportable-bit-nv+ #x2)
(defconstant +external-memory-feature-importable-bit-nv+ #x4)

(defconstant +external-memory-handle-type-opaque-fd-bit-khx+ #x1)
(defconstant +external-memory-handle-type-opaque-win32-bit-khx+ #x2)
(defconstant +external-memory-handle-type-opaque-win32-kmt-bit-khx+ #x4)
(defconstant +external-memory-handle-type-d3d11-texture-bit-khx+ #x8)
(defconstant +external-memory-handle-type-d3d11-texture-kmt-bit-khx+ #x10)
(defconstant +external-memory-handle-type-d3d12-heap-bit-khx+ #x20)
(defconstant +external-memory-handle-type-d3d12-resource-bit-khx+ #x40)

(defconstant +external-memory-handle-type-opaque-win32-bit-nv+ #x1)
(defconstant +external-memory-handle-type-opaque-win32-kmt-bit-nv+ #x2)
(defconstant +external-memory-handle-type-d3d11-image-bit-nv+ #x4)
(defconstant +external-memory-handle-type-d3d11-image-kmt-bit-nv+ #x8)

(defconstant +external-semaphore-feature-exportable-bit-khx+ #x1)
(defconstant +external-semaphore-feature-importable-bit-khx+ #x2)

(defconstant +external-semaphore-handle-type-opaque-fd-bit-khx+ #x1)
(defconstant +external-semaphore-handle-type-opaque-win32-bit-khx+ #x2)
(defconstant +external-semaphore-handle-type-opaque-win32-kmt-bit-khx+ #x4)
(defconstant +external-semaphore-handle-type-d3d12-fence-bit-khx+ #x8)
(defconstant +external-semaphore-handle-type-fence-fd-bit-khx+ #x10)

(defconstant +fence-create-signaled-bit+ #x1)

(defconstant +filter-nearest+ #x0)
(defconstant +filter-linear+ #x1)
(defconstant +filter-cubic-img+ #x3b9b0498)+ ;;+ "img-filter-cubic"

(defconstant +format-undefined+ #x0)
(defconstant +format-r4g4-unorm-pack8+ #x1)
(defconstant +format-r4g4b4a4-unorm-pack16+ #x2)
(defconstant +format-b4g4r4a4-unorm-pack16+ #x3)
(defconstant +format-r5g6b5-unorm-pack16+ #x4)
(defconstant +format-b5g6r5-unorm-pack16+ #x5)
(defconstant +format-r5g5b5a1-unorm-pack16+ #x6)
(defconstant +format-b5g5r5a1-unorm-pack16+ #x7)
(defconstant +format-a1r5g5b5-unorm-pack16+ #x8)
(defconstant +format-r8-unorm+ #x9)
(defconstant +format-r8-snorm+ #xa)
(defconstant +format-r8-uscaled+ #xb)
(defconstant +format-r8-sscaled+ #xc)
(defconstant +format-r8-uint+ #xd)
(defconstant +format-r8-sint+ #xe)
(defconstant +format-r8-srgb+ #xf)
(defconstant +format-r8g8-unorm+ #x10)
(defconstant +format-r8g8-snorm+ #x11)
(defconstant +format-r8g8-uscaled+ #x12)
(defconstant +format-r8g8-sscaled+ #x13)
(defconstant +format-r8g8-uint+ #x14)
(defconstant +format-r8g8-sint+ #x15)
(defconstant +format-r8g8-srgb+ #x16)
(defconstant +format-r8g8b8-unorm+ #x17)
(defconstant +format-r8g8b8-snorm+ #x18)
(defconstant +format-r8g8b8-uscaled+ #x19)
(defconstant +format-r8g8b8-sscaled+ #x1a)
(defconstant +format-r8g8b8-uint+ #x1b)
(defconstant +format-r8g8b8-sint+ #x1c)
(defconstant +format-r8g8b8-srgb+ #x1d)
(defconstant +format-b8g8r8-unorm+ #x1e)
(defconstant +format-b8g8r8-snorm+ #x1f)
(defconstant +format-b8g8r8-uscaled+ #x20)
(defconstant +format-b8g8r8-sscaled+ #x21)
(defconstant +format-b8g8r8-uint+ #x22)
(defconstant +format-b8g8r8-sint+ #x23)
(defconstant +format-b8g8r8-srgb+ #x24)
(defconstant +format-r8g8b8a8-unorm+ #x25)
(defconstant +format-r8g8b8a8-snorm+ #x26)
(defconstant +format-r8g8b8a8-uscaled+ #x27)
(defconstant +format-r8g8b8a8-sscaled+ #x28)
(defconstant +format-r8g8b8a8-uint+ #x29)
(defconstant +format-r8g8b8a8-sint+ #x2a)
(defconstant +format-r8g8b8a8-srgb+ #x2b)
(defconstant +format-b8g8r8a8-unorm+ #x2c)
(defconstant +format-b8g8r8a8-snorm+ #x2d)
(defconstant +format-b8g8r8a8-uscaled+ #x2e)
(defconstant +format-b8g8r8a8-sscaled+ #x2f)
(defconstant +format-b8g8r8a8-uint+ #x30)
(defconstant +format-b8g8r8a8-sint+ #x31)
(defconstant +format-b8g8r8a8-srgb+ #x32)
(defconstant +format-a8b8g8r8-unorm-pack32+ #x33)
(defconstant +format-a8b8g8r8-snorm-pack32+ #x34)
(defconstant +format-a8b8g8r8-uscaled-pack32+ #x35)
(defconstant +format-a8b8g8r8-sscaled-pack32+ #x36)
(defconstant +format-a8b8g8r8-uint-pack32+ #x37)
(defconstant +format-a8b8g8r8-sint-pack32+ #x38)
(defconstant +format-a8b8g8r8-srgb-pack32+ #x39)
(defconstant +format-a2r10g10b10-unorm-pack32+ #x3a)
(defconstant +format-a2r10g10b10-snorm-pack32+ #x3b)
(defconstant +format-a2r10g10b10-uscaled-pack32+ #x3c)
(defconstant +format-a2r10g10b10-sscaled-pack32+ #x3d)
(defconstant +format-a2r10g10b10-uint-pack32+ #x3e)
(defconstant +format-a2r10g10b10-sint-pack32+ #x3f)
(defconstant +format-a2b10g10r10-unorm-pack32+ #x40)
(defconstant +format-a2b10g10r10-snorm-pack32+ #x41)
(defconstant +format-a2b10g10r10-uscaled-pack32+ #x42)
(defconstant +format-a2b10g10r10-sscaled-pack32+ #x43)
(defconstant +format-a2b10g10r10-uint-pack32+ #x44)
(defconstant +format-a2b10g10r10-sint-pack32+ #x45)
(defconstant +format-r16-unorm+ #x46)
(defconstant +format-r16-snorm+ #x47)
(defconstant +format-r16-uscaled+ #x48)
(defconstant +format-r16-sscaled+ #x49)
(defconstant +format-r16-uint+ #x4a)
(defconstant +format-r16-sint+ #x4b)
(defconstant +format-r16-sfloat+ #x4c)
(defconstant +format-r16g16-unorm+ #x4d)
(defconstant +format-r16g16-snorm+ #x4e)
(defconstant +format-r16g16-uscaled+ #x4f)
(defconstant +format-r16g16-sscaled+ #x50)
(defconstant +format-r16g16-uint+ #x51)
(defconstant +format-r16g16-sint+ #x52)
(defconstant +format-r16g16-sfloat+ #x53)
(defconstant +format-r16g16b16-unorm+ #x54)
(defconstant +format-r16g16b16-snorm+ #x55)
(defconstant +format-r16g16b16-uscaled+ #x56)
(defconstant +format-r16g16b16-sscaled+ #x57)
(defconstant +format-r16g16b16-uint+ #x58)
(defconstant +format-r16g16b16-sint+ #x59)
(defconstant +format-r16g16b16-sfloat+ #x5a)
(defconstant +format-r16g16b16a16-unorm+ #x5b)
(defconstant +format-r16g16b16a16-snorm+ #x5c)
(defconstant +format-r16g16b16a16-uscaled+ #x5d)
(defconstant +format-r16g16b16a16-sscaled+ #x5e)
(defconstant +format-r16g16b16a16-uint+ #x5f)
(defconstant +format-r16g16b16a16-sint+ #x60)
(defconstant +format-r16g16b16a16-sfloat+ #x61)
(defconstant +format-r32-uint+ #x62)
(defconstant +format-r32-sint+ #x63)
(defconstant +format-r32-sfloat+ #x64)
(defconstant +format-r32g32-uint+ #x65)
(defconstant +format-r32g32-sint+ #x66)
(defconstant +format-r32g32-sfloat+ #x67)
(defconstant +format-r32g32b32-uint+ #x68)
(defconstant +format-r32g32b32-sint+ #x69)
(defconstant +format-r32g32b32-sfloat+ #x6a)
(defconstant +format-r32g32b32a32-uint+ #x6b)
(defconstant +format-r32g32b32a32-sint+ #x6c)
(defconstant +format-r32g32b32a32-sfloat+ #x6d)
(defconstant +format-r64-uint+ #x6e)
(defconstant +format-r64-sint+ #x6f)
(defconstant +format-r64-sfloat+ #x70)
(defconstant +format-r64g64-uint+ #x71)
(defconstant +format-r64g64-sint+ #x72)
(defconstant +format-r64g64-sfloat+ #x73)
(defconstant +format-r64g64b64-uint+ #x74)
(defconstant +format-r64g64b64-sint+ #x75)
(defconstant +format-r64g64b64-sfloat+ #x76)
(defconstant +format-r64g64b64a64-uint+ #x77)
(defconstant +format-r64g64b64a64-sint+ #x78)
(defconstant +format-r64g64b64a64-sfloat+ #x79)
(defconstant +format-b10g11r11-ufloat-pack32+ #x7a)
(defconstant +format-e5b9g9r9-ufloat-pack32+ #x7b)
(defconstant +format-d16-unorm+ #x7c)
(defconstant +format-x8-d24-unorm-pack32+ #x7d)
(defconstant +format-d32-sfloat+ #x7e)
(defconstant +format-s8-uint+ #x7f)
(defconstant +format-d16-unorm-s8-uint+ #x80)
(defconstant +format-d24-unorm-s8-uint+ #x81)
(defconstant +format-d32-sfloat-s8-uint+ #x82)
(defconstant +format-bc1-rgb-unorm-block+ #x83)
(defconstant +format-bc1-rgb-srgb-block+ #x84)
(defconstant +format-bc1-rgba-unorm-block+ #x85)
(defconstant +format-bc1-rgba-srgb-block+ #x86)
(defconstant +format-bc2-unorm-block+ #x87)
(defconstant +format-bc2-srgb-block+ #x88)
(defconstant +format-bc3-unorm-block+ #x89)
(defconstant +format-bc3-srgb-block+ #x8a)
(defconstant +format-bc4-unorm-block+ #x8b)
(defconstant +format-bc4-snorm-block+ #x8c)
(defconstant +format-bc5-unorm-block+ #x8d)
(defconstant +format-bc5-snorm-block+ #x8e)
(defconstant +format-bc6h-ufloat-block+ #x8f)
(defconstant +format-bc6h-sfloat-block+ #x90)
(defconstant +format-bc7-unorm-block+ #x91)
(defconstant +format-bc7-srgb-block+ #x92)
(defconstant +format-etc2-r8g8b8-unorm-block+ #x93)
(defconstant +format-etc2-r8g8b8-srgb-block+ #x94)
(defconstant +format-etc2-r8g8b8a1-unorm-block+ #x95)
(defconstant +format-etc2-r8g8b8a1-srgb-block+ #x96)
(defconstant +format-etc2-r8g8b8a8-unorm-block+ #x97)
(defconstant +format-etc2-r8g8b8a8-srgb-block+ #x98)
(defconstant +format-eac-r11-unorm-block+ #x99)
(defconstant +format-eac-r11-snorm-block+ #x9a)
(defconstant +format-eac-r11g11-unorm-block+ #x9b)
(defconstant +format-eac-r11g11-snorm-block+ #x9c)
(defconstant +format-astc-4x4-unorm-block+ #x9d)
(defconstant +format-astc-4x4-srgb-block+ #x9e)
(defconstant +format-astc-5x4-unorm-block+ #x9f)
(defconstant +format-astc-5x4-srgb-block+ #xa0)
(defconstant +format-astc-5x5-unorm-block+ #xa1)
(defconstant +format-astc-5x5-srgb-block+ #xa2)
(defconstant +format-astc-6x5-unorm-block+ #xa3)
(defconstant +format-astc-6x5-srgb-block+ #xa4)
(defconstant +format-astc-6x6-unorm-block+ #xa5)
(defconstant +format-astc-6x6-srgb-block+ #xa6)
(defconstant +format-astc-8x5-unorm-block+ #xa7)
(defconstant +format-astc-8x5-srgb-block+ #xa8)
(defconstant +format-astc-8x6-unorm-block+ #xa9)
(defconstant +format-astc-8x6-srgb-block+ #xaa)
(defconstant +format-astc-8x8-unorm-block+ #xab)
(defconstant +format-astc-8x8-srgb-block+ #xac)
(defconstant +format-astc-10x5-unorm-block+ #xad)
(defconstant +format-astc-10x5-srgb-block+ #xae)
(defconstant +format-astc-10x6-unorm-block+ #xaf)
(defconstant +format-astc-10x6-srgb-block+ #xb0)
(defconstant +format-astc-10x8-unorm-block+ #xb1)
(defconstant +format-astc-10x8-srgb-block+ #xb2)
(defconstant +format-astc-10x10-unorm-block+ #xb3)
(defconstant +format-astc-10x10-srgb-block+ #xb4)
(defconstant +format-astc-12x10-unorm-block+ #xb5)
(defconstant +format-astc-12x10-srgb-block+ #xb6)
(defconstant +format-astc-12x12-unorm-block+ #xb7)
(defconstant +format-astc-12x12-srgb-block+ #xb8)
(defconstant +format-pvrtc1-2bpp-unorm-block-img+ #x3b9b9cf0)+ ;;+ "img-format-pvrtc"
(defconstant +format-pvrtc1-4bpp-unorm-block-img+ #x3b9b9cf1)+ ;;+ "img-format-pvrtc"
(defconstant +format-pvrtc2-2bpp-unorm-block-img+ #x3b9b9cf2)+ ;;+ "img-format-pvrtc"
(defconstant +format-pvrtc2-4bpp-unorm-block-img+ #x3b9b9cf3)+ ;;+ "img-format-pvrtc"
(defconstant +format-pvrtc1-2bpp-srgb-block-img+ #x3b9b9cf4)+ ;;+ "img-format-pvrtc"
(defconstant +format-pvrtc1-4bpp-srgb-block-img+ #x3b9b9cf5)+ ;;+ "img-format-pvrtc"
(defconstant +format-pvrtc2-2bpp-srgb-block-img+ #x3b9b9cf6)+ ;;+ "img-format-pvrtc"
(defconstant +format-pvrtc2-4bpp-srgb-block-img+ #x3b9b9cf7)+ ;;+ "img-format-pvrtc"


(defconstant +format-feature-sampled-image-bit+ #x1)+ ;;+ format+ can+ be+ used+ for+ sampled+ images+ (sampled-image+ and+ combined-image-sampler+ descriptor+ types)
(defconstant +format-feature-storage-image-bit+ #x2)+ ;;+ format+ can+ be+ used+ for+ storage+ images+ (storage-image+ descriptor+ type)
(defconstant +format-feature-storage-image-atomic-bit+ #x4)+ ;;+ format+ supports+ atomic+ operations+ in+ case+ it+ is+ used+ for+ storage+ images
(defconstant +format-feature-uniform-texel-buffer-bit+ #x8)+ ;;+ format+ can+ be+ used+ for+ uniform+ texel+ buffers+ (tbos)
(defconstant +format-feature-storage-texel-buffer-bit+ #x10)+ ;;+ format+ can+ be+ used+ for+ storage+ texel+ buffers+ (ibos)
(defconstant +format-feature-storage-texel-buffer-atomic-bit+ #x20)+ ;;+ format+ supports+ atomic+ operations+ in+ case+ it+ is+ used+ for+ storage+ texel+ buffers
(defconstant +format-feature-vertex-buffer-bit+ #x40)+ ;;+ format+ can+ be+ used+ for+ vertex+ buffers+ (vbos)
(defconstant +format-feature-color-attachment-bit+ #x80)+ ;;+ format+ can+ be+ used+ for+ color+ attachment+ images
(defconstant +format-feature-color-attachment-blend-bit+ #x100)+ ;;+ format+ supports+ blending+ in+ case+ it+ is+ used+ for+ color+ attachment+ images
(defconstant +format-feature-depth-stencil-attachment-bit+ #x200)+ ;;+ format+ can+ be+ used+ for+ depth/stencil+ attachment+ images
(defconstant +format-feature-blit-src-bit+ #x400)+ ;;+ format+ can+ be+ used+ as+ the+ source+ image+ of+ blits+ with+ vkcmdblitimage
(defconstant +format-feature-blit-dst-bit+ #x800)+ ;;+ format+ can+ be+ used+ as+ the+ destination+ image+ of+ blits+ with+ vkcmdblitimage
(defconstant +format-feature-sampled-image-filter-linear-bit+ #x1000)+ ;;+ format+ can+ be+ filtered+ with+ filter-linear+ when+ being+ sampled
(defconstant +format-feature-sampled-image-filter-cubic-bit-img+ #x2000)+ ;;+ "img-filter-cubic"
(defconstant +format-feature-transfer-src-bit-khr+ #x4000)+ ;;+ "khr-maintenance1"
(defconstant +format-feature-transfer-dst-bit-khr+ #x8000)+ ;;+ "khr-maintenance1"

(defconstant +front-face-counter-clockwise+ #x0)
(defconstant +front-face-clockwise+ #x1)

(defconstant +image-aspect-color-bit+ #x1)
(defconstant +image-aspect-depth-bit+ #x2)
(defconstant +image-aspect-stencil-bit+ #x4)
(defconstant +image-aspect-metadata-bit+ #x8)

(defconstant +image-create-sparse-binding-bit+ #x1)+ ;;+ image+ should+ support+ sparse+ backing
(defconstant +image-create-sparse-residency-bit+ #x2)+ ;;+ image+ should+ support+ sparse+ backing+ with+ partial+ residency
(defconstant +image-create-sparse-aliased-bit+ #x4)+ ;;+ image+ should+ support+ constent+ data+ access+ to+ physical+ memory+ ranges+ mapped+ into+ multiple+ locations+ of+ sparse+ images
(defconstant +image-create-mutable-format-bit+ #x8)+ ;;+ allows+ image+ views+ to+ have+ different+ format+ than+ the+ base+ image
(defconstant +image-create-cube-compatible-bit+ #x10)+ ;;+ allows+ creating+ image+ views+ with+ cube+ type+ from+ the+ created+ image
(defconstant +image-create-bind-sfr-bit-khx+ #x40)+ ;;+ "khx-device-group"
(defconstant +image-create-2d-array-compatible-bit-khr+ #x20)+ ;;+ "khr-maintenance1"

(defconstant +image-layout-undefined+ #x0)+ ;;+ implicit+ layout+ an+ image+ is+ when+ its+ contents+ are+ undefined+ due+ to+ various+ reasons+ (e.g.+ right+ after+ creation)
(defconstant +image-layout-general+ #x1)+ ;;+ general+ layout+ when+ image+ can+ be+ used+ for+ any+ kind+ of+ access
(defconstant +image-layout-color-attachment-optimal+ #x2)+ ;;+ optimal+ layout+ when+ image+ is+ only+ used+ for+ color+ attachment+ read/write
(defconstant +image-layout-depth-stencil-attachment-optimal+ #x3)+ ;;+ optimal+ layout+ when+ image+ is+ only+ used+ for+ depth/stencil+ attachment+ read/write
(defconstant +image-layout-depth-stencil-read-only-optimal+ #x4)+ ;;+ optimal+ layout+ when+ image+ is+ used+ for+ read+ only+ depth/stencil+ attachment+ and+ shader+ access
(defconstant +image-layout-shader-read-only-optimal+ #x5)+ ;;+ optimal+ layout+ when+ image+ is+ used+ for+ read+ only+ shader+ access
(defconstant +image-layout-transfer-src-optimal+ #x6)+ ;;+ optimal+ layout+ when+ image+ is+ used+ only+ as+ source+ of+ transfer+ operations
(defconstant +image-layout-transfer-dst-optimal+ #x7)+ ;;+ optimal+ layout+ when+ image+ is+ used+ only+ as+ destination+ of+ transfer+ operations
(defconstant +image-layout-preinitialized+ #x8)+ ;;+ initial+ layout+ used+ when+ the+ data+ is+ populated+ by+ the+ cpu
(defconstant +image-layout-present-src-khr+ #x3b9acdea)+ ;;+ "khr-swapchain"
(defconstant +image-layout-shared-present-khr+ #x3b9c7b98)+ ;;+ "khr-shared-presentable-image"

(defconstant +image-tiling-optimal+ #x0)
(defconstant +image-tiling-linear+ #x1)

(defconstant +image-type-1d+ #x0)
(defconstant +image-type-2d+ #x1)
(defconstant +image-type-3d+ #x2)

(defconstant +image-usage-transfer-src-bit+ #x1)+ ;;+ can+ be+ used+ as+ a+ source+ of+ transfer+ operations
(defconstant +image-usage-transfer-dst-bit+ #x2)+ ;;+ can+ be+ used+ as+ a+ destination+ of+ transfer+ operations
(defconstant +image-usage-sampled-bit+ #x4)+ ;;+ can+ be+ sampled+ from+ (sampled-image+ and+ combined-image-sampler+ descriptor+ types)
(defconstant +image-usage-storage-bit+ #x8)+ ;;+ can+ be+ used+ as+ storage+ image+ (storage-image+ descriptor+ type)
(defconstant +image-usage-color-attachment-bit+ #x10)+ ;;+ can+ be+ used+ as+ framebuffer+ color+ attachment
(defconstant +image-usage-depth-stencil-attachment-bit+ #x20)+ ;;+ can+ be+ used+ as+ framebuffer+ depth/stencil+ attachment
(defconstant +image-usage-transient-attachment-bit+ #x40)+ ;;+ image+ data+ not+ needed+ outside+ of+ rendering
(defconstant +image-usage-input-attachment-bit+ #x80)+ ;;+ can+ be+ used+ as+ framebuffer+ input+ attachment

(defconstant +image-view-type-1d+ #x0)
(defconstant +image-view-type-2d+ #x1)
(defconstant +image-view-type-3d+ #x2)
(defconstant +image-view-type-cube+ #x3)
(defconstant +image-view-type-1d-array+ #x4)
(defconstant +image-view-type-2d-array+ #x5)
(defconstant +image-view-type-cube-array+ #x6)

(defconstant +index-type-uint16+ #x0)
(defconstant +index-type-uint32+ #x1)

(defconstant +indirect-commands-layout-usage-unordered-sequences-bit-nvx+ #x1)
(defconstant +indirect-commands-layout-usage-sparse-sequences-bit-nvx+ #x2)
(defconstant +indirect-commands-layout-usage-empty-executions-bit-nvx+ #x4)
(defconstant +indirect-commands-layout-usage-indexed-sequences-bit-nvx+ #x8)

(defconstant +indirect-commands-token-type-pipeline-nvx+ #x0)
(defconstant +indirect-commands-token-type-descriptor-set-nvx+ #x1)
(defconstant +indirect-commands-token-type-index-buffer-nvx+ #x2)
(defconstant +indirect-commands-token-type-vertex-buffer-nvx+ #x3)
(defconstant +indirect-commands-token-type-push-constant-nvx+ #x4)
(defconstant +indirect-commands-token-type-draw-indexed-nvx+ #x5)
(defconstant +indirect-commands-token-type-draw-nvx+ #x6)
(defconstant +indirect-commands-token-type-dispatch-nvx+ #x7)

(defconstant +internal-allocation-type-executable+ #x0)

(defconstant +logic-op-clear+ #x0)
(defconstant +logic-op-and+ #x1)
(defconstant +logic-op-and-reverse+ #x2)
(defconstant +logic-op-copy+ #x3)
(defconstant +logic-op-and-inverted+ #x4)
(defconstant +logic-op-no-op+ #x5)
(defconstant +logic-op-xor+ #x6)
(defconstant +logic-op-or+ #x7)
(defconstant +logic-op-nor+ #x8)
(defconstant +logic-op-equivalent+ #x9)
(defconstant +logic-op-invert+ #xa)
(defconstant +logic-op-or-reverse+ #xb)
(defconstant +logic-op-copy-inverted+ #xc)
(defconstant +logic-op-or-inverted+ #xd)
(defconstant +logic-op-nand+ #xe)
(defconstant +logic-op-set+ #xf)

(defconstant +memory-allocate-device-mask-bit-khx+ #x1)+ ;;+ force+ allocation+ on+ specific+ devices

(defconstant +memory-heap-device-local-bit+ #x1)+ ;;+ if+ set,+ heap+ represents+ device+ memory
(defconstant +memory-heap-multi-instance-bit-khx+ #x2)+ ;;+ "khx-device-group-creation"

(defconstant +memory-property-device-local-bit+ #x1)+ ;;+ if+ otherwise+ stated,+ then+ allocate+ memory+ on+ device
(defconstant +memory-property-host-visible-bit+ #x2)+ ;;+ memory+ is+ mappable+ by+ host
(defconstant +memory-property-host-coherent-bit+ #x4)+ ;;+ memory+ will+ have+ i/o+ coherency.+ if+ not+ set,+ application+ may+ need+ to+ use+ vkflushmappedmemoryranges+ and+ vkinvalidatemappedmemoryranges+ to+ flush/invalidate+ host+ cache
(defconstant +memory-property-host-cached-bit+ #x8)+ ;;+ memory+ will+ be+ cached+ by+ the+ host
(defconstant +memory-property-lazily-allocated-bit+ #x10)+ ;;+ memory+ may+ be+ allocated+ by+ the+ driver+ when+ it+ is+ required

(defconstant +object-entry-type-descriptor-set-nvx+ #x0)
(defconstant +object-entry-type-pipeline-nvx+ #x1)
(defconstant +object-entry-type-index-buffer-nvx+ #x2)
(defconstant +object-entry-type-vertex-buffer-nvx+ #x3)
(defconstant +object-entry-type-push-constant-nvx+ #x4)

(defconstant +object-entry-usage-graphics-bit-nvx+ #x1)
(defconstant +object-entry-usage-compute-bit-nvx+ #x2)

(defconstant +object-type-unknown+ #x0)
(defconstant +object-type-instance+ #x1)+ ;;+ vkinstance
(defconstant +object-type-physical-device+ #x2)+ ;;+ vkphysicaldevice
(defconstant +object-type-device+ #x3)+ ;;+ vkdevice
(defconstant +object-type-queue+ #x4)+ ;;+ vkqueue
(defconstant +object-type-semaphore+ #x5)+ ;;+ vksemaphore
(defconstant +object-type-command-buffer+ #x6)+ ;;+ vkcommandbuffer
(defconstant +object-type-fence+ #x7)+ ;;+ vkfence
(defconstant +object-type-device-memory+ #x8)+ ;;+ vkdevicememory
(defconstant +object-type-buffer+ #x9)+ ;;+ vkbuffer
(defconstant +object-type-image+ #xa)+ ;;+ vkimage
(defconstant +object-type-event+ #xb)+ ;;+ vkevent
(defconstant +object-type-query-pool+ #xc)+ ;;+ vkquerypool
(defconstant +object-type-buffer-view+ #xd)+ ;;+ vkbufferview
(defconstant +object-type-image-view+ #xe)+ ;;+ vkimageview
(defconstant +object-type-shader-module+ #xf)+ ;;+ vkshadermodule
(defconstant +object-type-pipeline-cache+ #x10)+ ;;+ vkpipelinecache
(defconstant +object-type-pipeline-layout+ #x11)+ ;;+ vkpipelinelayout
(defconstant +object-type-render-pass+ #x12)+ ;;+ vkrenderpass
(defconstant +object-type-pipeline+ #x13)+ ;;+ vkpipeline
(defconstant +object-type-descriptor-set-layout+ #x14)+ ;;+ vkdescriptorsetlayout
(defconstant +object-type-sampler+ #x15)+ ;;+ vksampler
(defconstant +object-type-descriptor-pool+ #x16)+ ;;+ vkdescriptorpool
(defconstant +object-type-descriptor-set+ #x17)+ ;;+ vkdescriptorset
(defconstant +object-type-framebuffer+ #x18)+ ;;+ vkframebuffer
(defconstant +object-type-command-pool+ #x19)+ ;;+ vkcommandpool
(defconstant +object-type-surface-khr+ #x3b9aca00)+ ;;+ "khr-surface"
(defconstant +object-type-swapchain-khr+ #x3b9acde8)+ ;;+ "khr-swapchain"
(defconstant +object-type-display-khr+ #x3b9ad1d0)+ ;;+ "khr-display"
(defconstant +object-type-display-mode-khr+ #x3b9ad1d1)+ ;;+ "khr-display"
(defconstant +object-type-debug-report-callback-ext+ #x3b9af4f8)+ ;;+ "ext-debug-report"
(defconstant +object-type-descriptor-update-template-khr+ #x3b9c1608)+ ;;+ "khr-descriptor-update-template"
(defconstant +object-type-object-table-nvx+ #x3b9c19f0)+ ;;+ "nvx-device-generated-commands"
(defconstant +object-type-indirect-commands-layout-nvx+ #x3b9c19f1)+ ;;+ "nvx-device-generated-commands"

(defconstant +peer-memory-feature-copy-src-bit-khx+ #x1)+ ;;+ can+ read+ with+ vkcmdcopy+ commands
(defconstant +peer-memory-feature-copy-dst-bit-khx+ #x2)+ ;;+ can+ write+ with+ vkcmdcopy+ commands
(defconstant +peer-memory-feature-generic-src-bit-khx+ #x4)+ ;;+ can+ read+ with+ any+ access+ type/command
(defconstant +peer-memory-feature-generic-dst-bit-khx+ #x8)+ ;;+ can+ write+ with+ and+ access+ type/command

(defconstant +physical-device-type-other+ #x0)
(defconstant +physical-device-type-integrated-gpu+ #x1)
(defconstant +physical-device-type-discrete-gpu+ #x2)
(defconstant +physical-device-type-virtual-gpu+ #x3)
(defconstant +physical-device-type-cpu+ #x4)

(defconstant +pipeline-bind-point-graphics+ #x0)
(defconstant +pipeline-bind-point-compute+ #x1)

(defconstant +pipeline-cache-header-version-one+ #x1)

(defconstant +pipeline-create-disable-optimization-bit+ #x1)
(defconstant +pipeline-create-allow-derivatives-bit+ #x2)
(defconstant +pipeline-create-derivative-bit+ #x4)
(defconstant +pipeline-create-view-index-from-device-index-bit-khx+ #x8)+ ;;+ "khx-device-group"
(defconstant +pipeline-create-dispatch-base-khx+ #x10)+ ;;+ "khx-device-group"

(defconstant +pipeline-stage-top-of-pipe-bit+ #x1)+ ;;+ before+ subsequent+ commands+ are+ processed
(defconstant +pipeline-stage-draw-indirect-bit+ #x2)+ ;;+ draw/dispatchindirect+ command+ fetch
(defconstant +pipeline-stage-vertex-input-bit+ #x4)+ ;;+ vertex/index+ fetch
(defconstant +pipeline-stage-vertex-shader-bit+ #x8)+ ;;+ vertex+ shading
(defconstant +pipeline-stage-tessellation-control-shader-bit+ #x10)+ ;;+ tessellation+ control+ shading
(defconstant +pipeline-stage-tessellation-evaluation-shader-bit+ #x20)+ ;;+ tessellation+ evaluation+ shading
(defconstant +pipeline-stage-geometry-shader-bit+ #x40)+ ;;+ geometry+ shading
(defconstant +pipeline-stage-fragment-shader-bit+ #x80)+ ;;+ fragment+ shading
(defconstant +pipeline-stage-early-fragment-tests-bit+ #x100)+ ;;+ early+ fragment+ (depth+ and+ stencil)+ tests
(defconstant +pipeline-stage-late-fragment-tests-bit+ #x200)+ ;;+ late+ fragment+ (depth+ and+ stencil)+ tests
(defconstant +pipeline-stage-color-attachment-output-bit+ #x400)+ ;;+ color+ attachment+ writes
(defconstant +pipeline-stage-compute-shader-bit+ #x800)+ ;;+ compute+ shading
(defconstant +pipeline-stage-transfer-bit+ #x1000)+ ;;+ transfer/copy+ operations
(defconstant +pipeline-stage-bottom-of-pipe-bit+ #x2000)+ ;;+ after+ previous+ commands+ have+ completed
(defconstant +pipeline-stage-host-bit+ #x4000)+ ;;+ indicates+ host+ (cpu)+ is+ a+ source/sink+ of+ the+ dependency
(defconstant +pipeline-stage-all-graphics-bit+ #x8000)+ ;;+ all+ stages+ of+ the+ graphics+ pipeline
(defconstant +pipeline-stage-all-commands-bit+ #x10000)+ ;;+ all+ stages+ supported+ on+ the+ queue
(defconstant +pipeline-stage-command-process-bit-nvx+ #x20000)+ ;;+ "nvx-device-generated-commands"

(defconstant +polygon-mode-fill+ #x0)
(defconstant +polygon-mode-line+ #x1)
(defconstant +polygon-mode-point+ #x2)

(defconstant +present-mode-immediate-khr+ #x0)
(defconstant +present-mode-mailbox-khr+ #x1)
(defconstant +present-mode-fifo-khr+ #x2)
(defconstant +present-mode-fifo-relaxed-khr+ #x3)
(defconstant +present-mode-shared-demand-refresh-khr+ #x3b9c7b98)+ ;;+ "khr-shared-presentable-image"
(defconstant +present-mode-shared-continuous-refresh-khr+ #x3b9c7b99)+ ;;+ "khr-shared-presentable-image"

(defconstant +primitive-topology-point-list+ #x0)
(defconstant +primitive-topology-line-list+ #x1)
(defconstant +primitive-topology-line-strip+ #x2)
(defconstant +primitive-topology-triangle-list+ #x3)
(defconstant +primitive-topology-triangle-strip+ #x4)
(defconstant +primitive-topology-triangle-fan+ #x5)
(defconstant +primitive-topology-line-list-with-adjacency+ #x6)
(defconstant +primitive-topology-line-strip-with-adjacency+ #x7)
(defconstant +primitive-topology-triangle-list-with-adjacency+ #x8)
(defconstant +primitive-topology-triangle-strip-with-adjacency+ #x9)
(defconstant +primitive-topology-patch-list+ #xa)

(defconstant +query-control-precise-bit+ #x1)+ ;;+ require+ precise+ results+ to+ be+ collected+ by+ the+ query

(defconstant +query-pipeline-statistic-input-assembly-vertices-bit+ #x1)+ ;;+ optional
(defconstant +query-pipeline-statistic-input-assembly-primitives-bit+ #x2)+ ;;+ optional
(defconstant +query-pipeline-statistic-vertex-shader-invocations-bit+ #x4)+ ;;+ optional
(defconstant +query-pipeline-statistic-geometry-shader-invocations-bit+ #x8)+ ;;+ optional
(defconstant +query-pipeline-statistic-geometry-shader-primitives-bit+ #x10)+ ;;+ optional
(defconstant +query-pipeline-statistic-clipping-invocations-bit+ #x20)+ ;;+ optional
(defconstant +query-pipeline-statistic-clipping-primitives-bit+ #x40)+ ;;+ optional
(defconstant +query-pipeline-statistic-fragment-shader-invocations-bit+ #x80)+ ;;+ optional
(defconstant +query-pipeline-statistic-tessellation-control-shader-patches-bit+ #x100)+ ;;+ optional
(defconstant +query-pipeline-statistic-tessellation-evaluation-shader-invocations-bit+ #x200)+ ;;+ optional
(defconstant +query-pipeline-statistic-compute-shader-invocations-bit+ #x400)+ ;;+ optional

(defconstant +query-result-64-bit+ #x1)+ ;;+ results+ of+ the+ queries+ are+ written+ to+ the+ destination+ buffer+ as+ 64-bit+ values
(defconstant +query-result-wait-bit+ #x2)+ ;;+ results+ of+ the+ queries+ are+ waited+ on+ before+ proceeding+ with+ the+ result+ copy
(defconstant +query-result-with-availability-bit+ #x4)+ ;;+ besides+ the+ results+ of+ the+ query,+ the+ availability+ of+ the+ results+ is+ also+ written
(defconstant +query-result-partial-bit+ #x8)+ ;;+ copy+ the+ partial+ results+ of+ the+ query+ even+ if+ the+ final+ results+ are+ not+ available

(defconstant +query-type-occlusion+ #x0)
(defconstant +query-type-pipeline-statistics+ #x1)+ ;;+ optional
(defconstant +query-type-timestamp+ #x2)

(defconstant +queue-graphics-bit+ #x1)+ ;;+ queue+ supports+ graphics+ operations
(defconstant +queue-compute-bit+ #x2)+ ;;+ queue+ supports+ compute+ operations
(defconstant +queue-transfer-bit+ #x4)+ ;;+ queue+ supports+ transfer+ operations
(defconstant +queue-sparse-binding-bit+ #x8)+ ;;+ queue+ supports+ sparse+ resource+ memory+ management+ operations

(defconstant +rasterization-order-strict-amd+ #x0)
(defconstant +rasterization-order-relaxed-amd+ #x1)

(defconstant +success+ #x0)+ ;;+ command+ completed+ successfully
(defconstant +not-ready+ #x1)+ ;;+ a+ fence+ or+ query+ has+ not+ yet+ completed
(defconstant +timeout+ #x2)+ ;;+ a+ wait+ operation+ has+ not+ completed+ in+ the+ specified+ time
(defconstant +event-set+ #x3)+ ;;+ an+ event+ is+ signaled
(defconstant +event-reset+ #x4)+ ;;+ an+ event+ is+ unsignaled
(defconstant +incomplete+ #x5)+ ;;+ a+ return+ array+ was+ too+ small+ for+ the+ result
(defconstant +error-out-of-host-memory+ -1)+ ;;+ a+ host+ memory+ allocation+ has+ failed
(defconstant +error-out-of-device-memory+ -2)+ ;;+ a+ device+ memory+ allocation+ has+ failed
(defconstant +error-initialization-failed+ -3)+ ;;+ initialization+ of+ a+ object+ has+ failed
(defconstant +error-device-lost+ -4)+ ;;+ the+ logical+ device+ has+ been+ lost.+ see+ <<devsandqueues-lost-device>>
(defconstant +error-memory-map-failed+ -5)+ ;;+ mapping+ of+ a+ memory+ object+ has+ failed
(defconstant +error-layer-not-present+ -6)+ ;;+ layer+ specified+ does+ not+ exist
(defconstant +error-extension-not-present+ -7)+ ;;+ extension+ specified+ does+ not+ exist
(defconstant +error-feature-not-present+ -8)+ ;;+ requested+ feature+ is+ not+ available+ on+ this+ device
(defconstant +error-incompatible-driver+ -9)+ ;;+ unable+ to+ find+ a+ vulkan+ driver
(defconstant +error-too-many-objects+ -10)+ ;;+ too+ many+ objects+ of+ the+ type+ have+ already+ been+ created
(defconstant +error-format-not-supported+ -11)+ ;;+ requested+ format+ is+ not+ supported+ on+ this+ device
(defconstant +error-fragmented-pool+ -12)+ ;;+ a+ requested+ pool+ allocation+ has+ failed+ due+ to+ fragmentation+ of+ the+ pool's+ memory
(defconstant +error-surface-lost-khr+ -1000000000)+ ;;+ "khr-surface"
(defconstant +error-native-window-in-use-khr+ -1000000001)+ ;;+ "khr-surface"
(defconstant +suboptimal-khr+ #x3b9acdeb)+ ;;+ "khr-swapchain"
(defconstant +error-out-of-date-khr+ -1000001004)+ ;;+ "khr-swapchain"
(defconstant +error-incompatible-display-khr+ -1000003001)+ ;;+ "khr-display-swapchain"
(defconstant +error-validation-failed-ext+ -1000011001)+ ;;+ "ext-debug-report"
(defconstant +error-invalid-shader-nv+ -1000012000)+ ;;+ "nv-glsl-shader"
(defconstant +nv-extension-1-error+ -1000013000)+ ;;+ "nv-extension-1"
(defconstant +error-out-of-pool-memory-khr+ -1000069000)+ ;;+ "khr-maintenance1"
(defconstant +error-invalid-external-handle-khx+ -1000072003)+ ;;+ "khx-external-memory"

(defconstant +sample-count-1-bit+ #x1)+ ;;+ sample+ count+ 1+ supported
(defconstant +sample-count-2-bit+ #x2)+ ;;+ sample+ count+ 2+ supported
(defconstant +sample-count-4-bit+ #x4)+ ;;+ sample+ count+ 4+ supported
(defconstant +sample-count-8-bit+ #x8)+ ;;+ sample+ count+ 8+ supported
(defconstant +sample-count-16-bit+ #x10)+ ;;+ sample+ count+ 16+ supported
(defconstant +sample-count-32-bit+ #x20)+ ;;+ sample+ count+ 32+ supported
(defconstant +sample-count-64-bit+ #x40)+ ;;+ sample+ count+ 64+ supported

(defconstant +sampler-address-mode-repeat+ #x0)
(defconstant +sampler-address-mode-mirrored-repeat+ #x1)
(defconstant +sampler-address-mode-clamp-to-edge+ #x2)
(defconstant +sampler-address-mode-clamp-to-border+ #x3)
(defconstant +sampler-address-mode-mirror-clamp-to-edge+ #x4)+ ;;+ "khr-sampler-mirror-clamp-to-edge"

(defconstant +sampler-mipmap-mode-nearest+ #x0)+ ;;+ choose+ nearest+ mip+ level
(defconstant +sampler-mipmap-mode-linear+ #x1)+ ;;+ linear+ filter+ between+ mip+ levels

(defconstant +shader-stage-vertex-bit+ #x1)
(defconstant +shader-stage-tessellation-control-bit+ #x2)
(defconstant +shader-stage-tessellation-evaluation-bit+ #x4)
(defconstant +shader-stage-geometry-bit+ #x8)
(defconstant +shader-stage-fragment-bit+ #x10)
(defconstant +shader-stage-compute-bit+ #x20)
(defconstant +shader-stage-all-graphics+ #x1f)
(defconstant +shader-stage-all+ #x7fffffff)

(defconstant +sharing-mode-exclusive+ #x0)
(defconstant +sharing-mode-concurrent+ #x1)


(defconstant +sparse-image-format-single-miptail-bit+ #x1)+ ;;+ image+ uses+ a+ single+ mip+ tail+ region+ for+ all+ array+ layers
(defconstant +sparse-image-format-aligned-mip-size-bit+ #x2)+ ;;+ image+ requires+ mip+ level+ dimensions+ to+ be+ an+ integer+ multiple+ of+ the+ sparse+ image+ block+ dimensions+ for+ non-tail+ mip+ levels.
(defconstant +sparse-image-format-nonstandard-block-size-bit+ #x4)+ ;;+ image+ uses+ a+ non-standard+ sparse+ image+ block+ dimensions

(defconstant +sparse-memory-bind-metadata-bit+ #x1)+ ;;+ operation+ binds+ resource+ metadata+ to+ memory

(defconstant +stencil-face-front-bit+ #x1)+ ;;+ front+ face
(defconstant +stencil-face-back-bit+ #x2)+ ;;+ back+ face
(defconstant +stencil-front-and-back+ #x3)+ ;;+ front+ and+ back+ faces

(defconstant +stencil-op-keep+ #x0)
(defconstant +stencil-op-zero+ #x1)
(defconstant +stencil-op-replace+ #x2)
(defconstant +stencil-op-increment-and-clamp+ #x3)
(defconstant +stencil-op-decrement-and-clamp+ #x4)
(defconstant +stencil-op-invert+ #x5)
(defconstant +stencil-op-increment-and-wrap+ #x6)
(defconstant +stencil-op-decrement-and-wrap+ #x7)

(defconstant +structure-type-pipeline-rasterization-line-state-create-info-ext+ 1000259001)
(defconstant +structure-type-physical-device-line-rasterization-features-ext+ 1000259000)
(defconstant +structure-type-application-info+ #x0)
(defconstant +structure-type-instance-create-info+ #x1)
(defconstant +structure-type-device-queue-create-info+ #x2)
(defconstant +structure-type-device-create-info+ #x3)
(defconstant +structure-type-submit-info+ #x4)
(defconstant +structure-type-memory-allocate-info+ #x5)
(defconstant +structure-type-mapped-memory-range+ #x6)
(defconstant +structure-type-bind-sparse-info+ #x7)
(defconstant +structure-type-fence-create-info+ #x8)
(defconstant +structure-type-semaphore-create-info+ #x9)
(defconstant +structure-type-event-create-info+ #xa)
(defconstant +structure-type-query-pool-create-info+ #xb)
(defconstant +structure-type-buffer-create-info+ #xc)
(defconstant +structure-type-buffer-view-create-info+ #xd)
(defconstant +structure-type-image-create-info+ #xe)
(defconstant +structure-type-image-view-create-info+ #xf)
(defconstant +structure-type-shader-module-create-info+ #x10)
(defconstant +structure-type-pipeline-cache-create-info+ #x11)
(defconstant +structure-type-pipeline-shader-stage-create-info+ #x12)
(defconstant +structure-type-pipeline-vertex-input-state-create-info+ #x13)
(defconstant +structure-type-pipeline-input-assembly-state-create-info+ #x14)
(defconstant +structure-type-pipeline-tessellation-state-create-info+ #x15)
(defconstant +structure-type-pipeline-viewport-state-create-info+ #x16)
(defconstant +structure-type-pipeline-rasterization-state-create-info+ #x17)
(defconstant +structure-type-pipeline-multisample-state-create-info+ #x18)
(defconstant +structure-type-pipeline-depth-stencil-state-create-info+ #x19)
(defconstant +structure-type-pipeline-color-blend-state-create-info+ #x1a)
(defconstant +structure-type-pipeline-dynamic-state-create-info+ #x1b)
(defconstant +structure-type-graphics-pipeline-create-info+ #x1c)
(defconstant +structure-type-compute-pipeline-create-info+ #x1d)
(defconstant +structure-type-pipeline-layout-create-info+ #x1e)
(defconstant +structure-type-sampler-create-info+ #x1f)
(defconstant +structure-type-descriptor-set-layout-create-info+ #x20)
(defconstant +structure-type-descriptor-pool-create-info+ #x21)
(defconstant +structure-type-descriptor-set-allocate-info+ #x22)
(defconstant +structure-type-write-descriptor-set+ #x23)
(defconstant +structure-type-copy-descriptor-set+ #x24)
(defconstant +structure-type-framebuffer-create-info+ #x25)
(defconstant +structure-type-render-pass-create-info+ #x26)
(defconstant +structure-type-command-pool-create-info+ #x27)
(defconstant +structure-type-command-buffer-allocate-info+ #x28)
(defconstant +structure-type-command-buffer-inheritance-info+ #x29)
(defconstant +structure-type-command-buffer-begin-info+ #x2a)
(defconstant +structure-type-render-pass-begin-info+ #x2b)
(defconstant +structure-type-buffer-memory-barrier+ #x2c)
(defconstant +structure-type-image-memory-barrier+ #x2d)
(defconstant +structure-type-memory-barrier+ #x2e)
(defconstant +structure-type-loader-instance-create-info+ #x2f)
(defconstant +structure-type-loader-device-create-info+ #x30)
(defconstant +structure-type-swapchain-create-info-khr+ #x3b9acde8)+ ;;+ "khr-swapchain"
(defconstant +structure-type-present-info-khr+ #x3b9acde9)+ ;;+ "khr-swapchain"
(defconstant +structure-type-display-mode-create-info-khr+ #x3b9ad1d0)+ ;;+ "khr-display"
(defconstant +structure-type-display-surface-create-info-khr+ #x3b9ad1d1)+ ;;+ "khr-display"
(defconstant +structure-type-display-present-info-khr+ #x3b9ad5b8)+ ;;+ "khr-display-swapchain"
(defconstant +structure-type-xlib-surface-create-info-khr+ #x3b9ad9a0)+ ;;+ "khr-xlib-surface"
(defconstant +structure-type-xcb-surface-create-info-khr+ #x3b9add88)+ ;;+ "khr-xcb-surface"
(defconstant +structure-type-wayland-surface-create-info-khr+ #x3b9ae170)+ ;;+ "khr-wayland-surface"
(defconstant +structure-type-mir-surface-create-info-khr+ #x3b9ae558)+ ;;+ "khr-mir-surface"
(defconstant +structure-type-android-surface-create-info-khr+ #x3b9ae940)+ ;;+ "khr-android-surface"
(defconstant +structure-type-win32-surface-create-info-khr+ #x3b9aed28)+ ;;+ "khr-win32-surface"
(defconstant +structure-type-debug-report-callback-create-info-ext+ #x3b9af4f8)+ ;;+ "ext-debug-report"
(defconstant +structure-type-pipeline-rasterization-state-rasterization-order-amd+ #x3b9b1050)+ ;;+ "amd-rasterization-order"
(defconstant +structure-type-debug-marker-object-name-info-ext+ #x3b9b1ff0)+ ;;+ "ext-debug-marker"
(defconstant +structure-type-debug-marker-object-tag-info-ext+ #x3b9b1ff1)+ ;;+ "ext-debug-marker"
(defconstant +structure-type-debug-marker-marker-info-ext+ #x3b9b1ff2)+ ;;+ "ext-debug-marker"
(defconstant +structure-type-dedicated-allocation-image-create-info-nv+ #x3b9b2f90)+ ;;+ "nv-dedicated-allocation"
(defconstant +structure-type-dedicated-allocation-buffer-create-info-nv+ #x3b9b2f91)+ ;;+ "nv-dedicated-allocation"
(defconstant +structure-type-dedicated-allocation-memory-allocate-info-nv+ #x3b9b2f92)+ ;;+ "nv-dedicated-allocation"
(defconstant +structure-type-texture-lod-gather-format-properties-amd+ #x3b9b6a28)+ ;;+ "amd-texture-gather-bias-lod"
(defconstant +structure-type-render-pass-multiview-create-info-khx+ #x3b9b9908)+ ;;+ "khx-multiview"
(defconstant +structure-type-physical-device-multiview-features-khx+ #x3b9b9909)+ ;;+ "khx-multiview"
(defconstant +structure-type-physical-device-multiview-properties-khx+ #x3b9b990a)+ ;;+ "khx-multiview"
(defconstant +structure-type-external-memory-image-create-info-nv+ #x3b9ba4c0)+ ;;+ "nv-external-memory"
(defconstant +structure-type-export-memory-allocate-info-nv+ #x3b9ba4c1)+ ;;+ "nv-external-memory"
(defconstant +structure-type-import-memory-win32-handle-info-nv+ #x3b9ba8a8)+ ;;+ "nv-external-memory-win32"
(defconstant +structure-type-export-memory-win32-handle-info-nv+ #x3b9ba8a9)+ ;;+ "nv-external-memory-win32"
(defconstant +structure-type-win32-keyed-mutex-acquire-release-info-nv+ #x3b9bac90)+ ;;+ "nv-win32-keyed-mutex"
(defconstant +structure-type-physical-device-features-2-khr+ #x3b9bb078)+ ;;+ "khr-get-physical-device-properties2"
(defconstant +structure-type-physical-device-properties-2-khr+ #x3b9bb079)+ ;;+ "khr-get-physical-device-properties2"
(defconstant +structure-type-format-properties-2-khr+ #x3b9bb07a)+ ;;+ "khr-get-physical-device-properties2"
(defconstant +structure-type-image-format-properties-2-khr+ #x3b9bb07b)+ ;;+ "khr-get-physical-device-properties2"
(defconstant +structure-type-physical-device-image-format-info-2-khr+ #x3b9bb07c)+ ;;+ "khr-get-physical-device-properties2"
(defconstant +structure-type-queue-family-properties-2-khr+ #x3b9bb07d)+ ;;+ "khr-get-physical-device-properties2"
(defconstant +structure-type-physical-device-memory-properties-2-khr+ #x3b9bb07e)+ ;;+ "khr-get-physical-device-properties2"
(defconstant +structure-type-sparse-image-format-properties-2-khr+ #x3b9bb07f)+ ;;+ "khr-get-physical-device-properties2"
(defconstant +structure-type-physical-device-sparse-image-format-info-2-khr+ #x3b9bb080)+ ;;+ "khr-get-physical-device-properties2"
(defconstant +structure-type-memory-allocate-flags-info-khx+ #x3b9bb460)+ ;;+ "khx-device-group"
(defconstant +structure-type-bind-buffer-memory-info-khx+ #x3b9bb461)+ ;;+ "khx-device-group"
(defconstant +structure-type-bind-image-memory-info-khx+ #x3b9bb462)+ ;;+ "khx-device-group"
(defconstant +structure-type-device-group-render-pass-begin-info-khx+ #x3b9bb463)+ ;;+ "khx-device-group"
(defconstant +structure-type-device-group-command-buffer-begin-info-khx+ #x3b9bb464)+ ;;+ "khx-device-group"
(defconstant +structure-type-device-group-submit-info-khx+ #x3b9bb465)+ ;;+ "khx-device-group"
(defconstant +structure-type-device-group-bind-sparse-info-khx+ #x3b9bb466)+ ;;+ "khx-device-group"
(defconstant +structure-type-device-group-present-capabilities-khx+ #x3b9bb467)+ ;;+ "khx-device-group"
(defconstant +structure-type-image-swapchain-create-info-khx+ #x3b9bb468)+ ;;+ "khx-device-group"
(defconstant +structure-type-bind-image-memory-swapchain-info-khx+ #x3b9bb469)+ ;;+ "khx-device-group"
(defconstant +structure-type-acquire-next-image-info-khx+ #x3b9bb46a)+ ;;+ "khx-device-group"
(defconstant +structure-type-device-group-present-info-khx+ #x3b9bb46b)+ ;;+ "khx-device-group"
(defconstant +structure-type-device-group-swapchain-create-info-khx+ #x3b9bb46c)+ ;;+ "khx-device-group"
(defconstant +structure-type-validation-flags-ext+ #x3b9bb848)+ ;;+ "ext-validation-flags"
(defconstant +structure-type-vi-surface-create-info-nn+ #x3b9bbc30)+ ;;+ "nn-vi-surface"
(defconstant +structure-type-physical-device-group-properties-khx+ #x3b9bdb70)+ ;;+ "khx-device-group-creation"
(defconstant +structure-type-device-group-device-create-info-khx+ #x3b9bdb71)+ ;;+ "khx-device-group-creation"
(defconstant +structure-type-physical-device-external-image-format-info-khx+ #x3b9bdf58)+ ;;+ "khx-external-memory-capabilities"
(defconstant +structure-type-external-image-format-properties-khx+ #x3b9bdf59)+ ;;+ "khx-external-memory-capabilities"
(defconstant +structure-type-physical-device-external-buffer-info-khx+ #x3b9bdf5a)+ ;;+ "khx-external-memory-capabilities"
(defconstant +structure-type-external-buffer-properties-khx+ #x3b9bdf5b)+ ;;+ "khx-external-memory-capabilities"
(defconstant +structure-type-physical-device-id-properties-khx+ #x3b9bdf5c)+ ;;+ "khx-external-memory-capabilities"
(defconstant +structure-type-external-memory-buffer-create-info-khx+ #x3b9be340)+ ;;+ "khx-external-memory"
(defconstant +structure-type-external-memory-image-create-info-khx+ #x3b9be341)+ ;;+ "khx-external-memory"
(defconstant +structure-type-export-memory-allocate-info-khx+ #x3b9be342)+ ;;+ "khx-external-memory"
(defconstant +structure-type-import-memory-win32-handle-info-khx+ #x3b9be728)+ ;;+ "khx-external-memory-win32"
(defconstant +structure-type-export-memory-win32-handle-info-khx+ #x3b9be729)+ ;;+ "khx-external-memory-win32"
(defconstant +structure-type-memory-win32-handle-properties-khx+ #x3b9be72a)+ ;;+ "khx-external-memory-win32"
(defconstant +structure-type-import-memory-fd-info-khx+ #x3b9beb10)+ ;;+ "khx-external-memory-fd"
(defconstant +structure-type-memory-fd-properties-khx+ #x3b9beb11)+ ;;+ "khx-external-memory-fd"
(defconstant +structure-type-win32-keyed-mutex-acquire-release-info-khx+ #x3b9beef8)+ ;;+ "khx-win32-keyed-mutex"
(defconstant +structure-type-physical-device-external-semaphore-info-khx+ #x3b9bf2e0)+ ;;+ "khx-external-semaphore-capabilities"
(defconstant +structure-type-external-semaphore-properties-khx+ #x3b9bf2e1)+ ;;+ "khx-external-semaphore-capabilities"
(defconstant +structure-type-export-semaphore-create-info-khx+ #x3b9bf6c8)+ ;;+ "khx-external-semaphore"
(defconstant +structure-type-import-semaphore-win32-handle-info-khx+ #x3b9bfab0)+ ;;+ "khx-external-semaphore-win32"
(defconstant +structure-type-export-semaphore-win32-handle-info-khx+ #x3b9bfab1)+ ;;+ "khx-external-semaphore-win32"
(defconstant +structure-type-d3d12-fence-submit-info-khx+ #x3b9bfab2)+ ;;+ "khx-external-semaphore-win32"
(defconstant +structure-type-import-semaphore-fd-info-khx+ #x3b9bfe98)+ ;;+ "khx-external-semaphore-fd"
(defconstant +structure-type-physical-device-push-descriptor-properties-khr+ #x3b9c0280)+ ;;+ "khr-push-descriptor"
(defconstant +structure-type-present-regions-khr+ #x3b9c1220)+ ;;+ "khr-incremental-present"
(defconstant +structure-type-descriptor-update-template-create-info-khr+ #x3b9c1608)+ ;;+ "khr-descriptor-update-template"
(defconstant +structure-type-object-table-create-info-nvx+ #x3b9c19f0)+ ;;+ "nvx-device-generated-commands"
(defconstant +structure-type-indirect-commands-layout-create-info-nvx+ #x3b9c19f1)+ ;;+ "nvx-device-generated-commands"
(defconstant +structure-type-cmd-process-commands-info-nvx+ #x3b9c19f2)+ ;;+ "nvx-device-generated-commands"
(defconstant +structure-type-cmd-reserve-space-for-commands-info-nvx+ #x3b9c19f3)+ ;;+ "nvx-device-generated-commands"
(defconstant +structure-type-device-generated-commands-limits-nvx+ #x3b9c19f4)+ ;;+ "nvx-device-generated-commands"
(defconstant +structure-type-device-generated-commands-features-nvx+ #x3b9c19f5)+ ;;+ "nvx-device-generated-commands"
(defconstant +structure-type-pipeline-viewport-w-scaling-state-create-info-nv+ #x3b9c1dd8)+ ;;+ "nv-clip-space-w-scaling"
(defconstant +structure-type-surface-capabilities2-ext+ #x3b9c2990)+ ;;+ "ext-display-surface-counter"
(defconstant +structure-type-display-power-info-ext+ #x3b9c2d78)+ ;;+ "ext-display-control"
(defconstant +structure-type-device-event-info-ext+ #x3b9c2d79)+ ;;+ "ext-display-control"
(defconstant +structure-type-display-event-info-ext+ #x3b9c2d7a)+ ;;+ "ext-display-control"
(defconstant +structure-type-swapchain-counter-create-info-ext+ #x3b9c2d7b)+ ;;+ "ext-display-control"
(defconstant +structure-type-present-times-info-google+ #x3b9c3160)+ ;;+ "google-display-timing"
(defconstant +structure-type-physical-device-multiview-per-view-attributes-properties-nvx+ #x3b9c44e8)+ ;;+ "nvx-multiview-per-view-attributes"
(defconstant +structure-type-pipeline-viewport-swizzle-state-create-info-nv+ #x3b9c48d0)+ ;;+ "nv-viewport-swizzle"
(defconstant +structure-type-physical-device-discard-rectangle-properties-ext+ #x3b9c4cb8)+ ;;+ "ext-discard-rectangles"
(defconstant +structure-type-pipeline-discard-rectangle-state-create-info-ext+ #x3b9c4cb9)+ ;;+ "ext-discard-rectangles"
(defconstant +structure-type-hdr-metadata-ext+ #x3b9c6428)+ ;;+ "ext-hdr-metadata"
(defconstant +structure-type-shared-present-surface-capabilities-khr+ #x3b9c7b98)+ ;;+ "khr-shared-presentable-image"
(defconstant +structure-type-physical-device-surface-info-2-khr+ #x3b9c9ad8)+ ;;+ "khr-get-surface-capabilities2"
(defconstant +structure-type-surface-capabilities-2-khr+ #x3b9c9ad9)+ ;;+ "khr-get-surface-capabilities2"
(defconstant +structure-type-surface-format-2-khr+ #x3b9c9ada)+ ;;+ "khr-get-surface-capabilities2"
(defconstant +structure-type-ios-surface-create-info-mvk+ #x3b9ca690)+ ;;+ "mios-surface"
(defconstant +structure-type-macos-surface-create-info-mvk+ #x3b9caa78)+ ;;+ "mmacos-surface"

(defconstant +subpass-contents-inline+ #x0)
(defconstant +subpass-contents-secondary-command-buffers+ #x1)

(defconstant +subpass-description-per-view-attributes-bit-nvx+ #x1)+ ;;+ "nvx-multiview-per-view-attributes"
(defconstant +subpass-description-per-view-position-x-only-bit-nvx+ #x2)+ ;;+ "nvx-multiview-per-view-attributes"

(defconstant +surface-counter-vblank-ext+ #x1)

(defconstant +surface-transform-identity-bit-khr+ #x1)
(defconstant +surface-transform-rotate-90-bit-khr+ #x2)
(defconstant +surface-transform-rotate-180-bit-khr+ #x4)
(defconstant +surface-transform-rotate-270-bit-khr+ #x8)
(defconstant +surface-transform-horizontal-mirror-bit-khr+ #x10)
(defconstant +surface-transform-horizontal-mirror-rotate-90-bit-khr+ #x20)
(defconstant +surface-transform-horizontal-mirror-rotate-180-bit-khr+ #x40)
(defconstant +surface-transform-horizontal-mirror-rotate-270-bit-khr+ #x80)
(defconstant +surface-transform-inherit-bit-khr+ #x100)

(defconstant +swapchain-create-bind-sfr-bit-khx+ #x1)+ ;;+ "khx-device-group"

(defconstant +system-allocation-scope-command+ #x0)
(defconstant +system-allocation-scope-object+ #x1)
(defconstant +system-allocation-scope-cache+ #x2)
(defconstant +system-allocation-scope-device+ #x3)
(defconstant +system-allocation-scope-instance+ #x4)

(defconstant +validation-check-all-ext+ #x0)
(defconstant +validation-check-shaders-ext+ #x1)

(defconstant +vertex-input-rate-vertex+ #x0)
(defconstant +vertex-input-rate-instance+ #x1)

(defconstant +viewport-coordinate-swizzle-positive-x-nv+ #x0)
(defconstant +viewport-coordinate-swizzle-negative-x-nv+ #x1)
(defconstant +viewport-coordinate-swizzle-positive-y-nv+ #x2)
(defconstant +viewport-coordinate-swizzle-negative-y-nv+ #x3)
(defconstant +viewport-coordinate-swizzle-positive-z-nv+ #x4)
(defconstant +viewport-coordinate-swizzle-negative-z-nv+ #x5)
(defconstant +viewport-coordinate-swizzle-positive-w-nv+ #x6)
(defconstant +viewport-coordinate-swizzle-negative-w-nv+ #x7)
