import { defineConfig } from "vite";

export default defineConfig({
  server: {
    middlewareMode: true, // important for express integration
  },
  appType: "custom", // Vite won't try to serve index.html itself
});

