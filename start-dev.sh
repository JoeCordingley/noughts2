#!/bin/bash

# Navigate to the frontend directory and start Astro
echo "Starting frontend..."
(cd frontend && npm run dev) &

# Navigate to the API directory and start the Haskell backend
echo "Starting API..."
(cd api && ghcid --command "stack ghci" --run) &

# Start the nginx container
echo "Starting nginx (Docker)..."
docker compose -f docker-compose.local.yml up --build

