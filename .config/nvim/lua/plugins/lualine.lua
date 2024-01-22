return {
  "nvim-lualine/lualine.nvim",
  event = "VeryLazy",
  opts = function(_, opts)
    opts.options = {
      theme = "gruvbox",
      component_separators = "|",
      section_separators = { left = "", right = "" },
    }
  end,
}
