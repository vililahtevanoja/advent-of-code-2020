FROM haskell:stretch

RUN apt update && apt install -y wget fontconfig unzip
RUN wget -c https://dl.google.com/go/go1.14.2.linux-amd64.tar.gz -O - | tar -xz -C /usr/local
RUN export PATH=$PATH:/usr/local/go/bin
RUN /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/JetBrains/JetBrainsMono/master/install_manual.sh)"
