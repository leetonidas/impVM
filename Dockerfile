# this should generate a running Docker container for your challenge
# if your service does not require a server, simply delete this file

FROM debian:stretch

RUN apt-get update
RUN apt-get -y upgrade
RUN apt-get install -y binutils gcc cmake
RUN apt-get install -y wget xz-utils

WORKDIR /root/

RUN wget https://yx7.cc/code/ynetd/ynetd-0.1.2.tar.xz
RUN tar -xvf ynetd-0.1.2.tar.xz

WORKDIR /root/ynetd-0.1.2
RUN make

COPY CMakeLists.txt /root/
COPY src /root/src

WORKDIR /root/
RUN cmake .
RUN make

FROM debian:stretch

RUN useradd --create-home --shell /bin/bash ctf

USER root

# COPY all your files
COPY --from=0 /root/ynetd-0.1.2 /root/impVM /home/ctf/
COPY rev_flag.txt /home/ctf

# RUN chmod on all your files
RUN chmod u=rwx,go=rx /home/ctf && \
    chown -R root:root /home/ctf

# EXPOSE all your ports
EXPOSE 1337

# CMD your challenge
CMD ["/home/ctf/ynetd", "-p", "1337", "-u", "ctf", "-d", "/home/ctf", "-lt", "5", "-lm", "16777216", "/home/ctf/impVM -i"]
