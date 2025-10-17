import { defineConfig } from 'vite'
import { resolve } from 'path'

export default defineConfig({
  appType: 'spa', // default, but explicit is nice
  resolve: {
    alias: {
      '@shared': resolve(__dirname, '../shared'),
    },
  },
  server: {
    fs: {
      allow: [
        '..',
        resolve(__dirname, '../shared'),
      ],
    },
  },
})
