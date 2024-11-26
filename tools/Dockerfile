FROM rust:latest

ENV LLVM_VERSION=18

RUN apt-get update && apt-get install -y \
    lsb-release \
    software-properties-common

RUN wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 18  && \
    apt-get install -y libpolly-18-dev # this package is not available before

WORKDIR /project
