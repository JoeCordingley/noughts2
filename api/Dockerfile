# Stage 1: Build the Haskell binary
FROM haskell:latest AS builder
WORKDIR /app
COPY . .
RUN stack build --copy-bins --local-bin-path /bin

# Stage 2: Run the binary in a minimal image
FROM ubuntu:latest
WORKDIR /app
COPY --from=builder /bin/tigris-api-exe /app/tigris-api-exe
CMD ["/app/tigris-api-exe"]

