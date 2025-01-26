// @ts-check
import { defineConfig } from "astro/config";

// https://astro.build/config
export default defineConfig({
  server: {
    proxy: {
      "/api": "http://localhost:8080", // Proxy /api to Servant
    },
  },
});
