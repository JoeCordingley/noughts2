import express from "express";
import mustache from "mustache";
import fs from "fs";
import { createServer as createViteServer } from "vite";

async function start() {
  const app = express();

  // 1️⃣ Create Vite in middleware mode
  const vite = await createViteServer({
    server: { middlewareMode: true },
    appType: "custom",
  });
  app.use(vite.middlewares);

  app.get("/html/:page", async (req, res) => {
    const filePath = path.join("/", `${req.params.page}.html`);

    if (!fs.existsSync(filePath)) {
      res.status(404).send("HTML page not found");
      return;
    }

    const html = fs.readFileSync(filePath, "utf8");
    const transformed = await vite.transformIndexHtml(req.url, html);

    res.setHeader("Content-Type", "text/html");
    res.send(transformed);
  });

  // 2️⃣ Serve each Mustache template as /:page
  app.get("/mustache/:page?", async (req, res) => {
    const filePath = `../shared/templates/${req.params.page}.mustache`;

    if (!fs.existsSync(filePath)) {
      res.status(404).send("Page not found");
      return;
    }

    const tpl = fs.readFileSync(filePath, "utf8");

    // Mock data for preview (add anything you need)
    const mockData = {
      playerName: "Alice",
      values: [
        { dynasty: "Archer", player: "Alice", taken: true },
        { dynasty: "Bull", player: "Bob", taken: true },
        { dynasty: "Lion", taken: false },
      ],
    };

    const rendered = mustache.render(tpl, mockData);

    // Let Vite handle transforms (so imports + CSS/JS hot reload work)
    const html = await vite.transformIndexHtml(req.url, rendered);

    res.setHeader("Content-Type", "text/html");
    res.send(html);
  });

  app.listen(5173, () =>
    console.log("✨ Preview running at http://localhost:5173")
  );
}

start();

