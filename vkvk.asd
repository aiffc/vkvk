(defsystem "vkvk"
  :version "0.1.0"
  :author "yjc18915568435@gmail.com"
  :license "MIT"
  :depends-on (:cffi)
  :serial t
  :components ((:file "vulkan/package")
	       (:file "vulkan/vk-macros")
	       (:file "vulkan/vk-types")
	       (:file "vulkan/vk-funcs")
	       (:file "src/package")
	       (:file "src/vulkan-enum")
	       (:file "src/vk-arg")
	       (:file "src/translator")
	       (:file "src/vulkan-loader")
	       (:file "src/vulkan-class")
	       (:file "src/vulkan-debug")
	       (:file "src/vulkan-instance")
	       (:file "src/vulkan-physical-device")
	       (:file "src/vulkan-device")
	       (:file "src/vulkan-surface")
	       (:file "src/vulkan-swapchain")
	       (:file "src/vulkan-command-buffers")
	       (:file "src/vulkan-memory")
	       (:file "src/vulkan-image")
	       (:file "src/vulkan-pipeline")
	       (:file "src/vulkan-framebuffer")
	       (:file "src/vulkan-semaphore")
	       (:file "src/vulkan-fence")
	       (:file "src/vulkan-allocator")
	       (:file "src/vulkan-buffer")
	       (:file "src/vulkan-descriptor")
	       (:file "src/vulkan-event")
	       (:file "src/vulkan-query")
	       (:file "src/vulkan-sampler")))


