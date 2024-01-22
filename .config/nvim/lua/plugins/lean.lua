local function on_attach(_, bufnr)
  local function cmd(mode, lhs, rhs)
    vim.keymap.set(mode, lhs, rhs, { noremap = true, buffer = true })
  end

  -- Autocomplete using the Lean language server
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  -- gd in normal mode will jump to definition
  cmd("n", "gd", vim.lsp.buf.definition)
  -- K in normal mode will show the definition of what's under the cursor
  cmd("n", "K", vim.lsp.buf.hover)

  -- <leader>n will jump to the next Lean line with a diagnostic message on it
  -- <leader>N will jump backwards
  cmd("n", "<leader>n", function()
    vim.diagnostic.goto_next({ popup_opts = { show_header = false } })
  end)
  cmd("n", "<leader>N", function()
    vim.diagnostic.goto_prev({ popup_opts = { show_header = false } })
  end)

  -- <leader>q will load all errors in the current lean file into the location list
  -- (and then will open the location list)
  -- see :h location-list if you don't generally use it in other vim contexts
  cmd("n", "<leader>q", vim.diagnostic.setloclist)
end

return {
  "Julian/lean.nvim",
  event = { "BufReadPre *.lean", "BufNewFile *.lean" },

  dependencies = {
    "neovim/nvim-lspconfig",
    "nvim-lua/plenary.nvim",
  },

  -- see details below for full configuration options
  opts = {
    lsp = {
      on_attach = on_attach,
      init_options = {
        edit_delay = 50,
      },
    },
    mappings = true,
    infoview = {
      separate_tab = false,
      autoopen = true,
      horizontal_position = "bottom",
      width = 50,
      height = 10,
    },
    abbreviations = {
      -- Enable expanding of unicode abbreviations?
      enable = true,
      -- additional abbreviations:
      extra = {
        -- Add a \wknight abbreviation to insert ♘
        --
        -- Note that the backslash is implied, and that you of
        -- course may also use a snippet engine directly to do
        -- this if so desired.
        wknight = "♘",
      },
      -- Change if you don't like the backslash
      -- (comma is a popular choice on French keyboards)
      leader = "\\",
    },
  },
}
