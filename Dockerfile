FROM ubuntu:latest
RUN mkdir /app
WORKDIR /app
COPY artifacts/* /app
ENTRYPOINT ["/app/dhalls"]
