FROM ubuntu:latest
RUN apt-get update && apt-get install --yes libtinfo5 && mkdir /app
WORKDIR /app
COPY artifacts/dhalls /app
ENTRYPOINT ["/app/dhalls"]
