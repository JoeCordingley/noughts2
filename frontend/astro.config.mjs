// @ts-check
import { defineConfig } from "astro/config";

import node from "@astrojs/node";
import htmx from "astro-htmx";

// https://astro.build/config
export default defineConfig({
  integrations: [htmx()],
  adapter: node({
    mode: "standalone",
  }),
});

