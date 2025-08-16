(use-package speak-buffer
  :ensure t
  :vc (:url "https://github.com/lorniu/speak-buffer.el")
  :config
  ;; See Customization section below
  (setq speak-buffer-language 'zh)
  (setq gt-tts-edge-tts-voice "zh-CN-XiaoxiaoNenral")
  (setq speak-buffer-engine 'edge-tts)
  (setq gt-tts-edge-tts-speed 1.9))
