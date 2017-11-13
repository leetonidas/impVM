# this should generate a running Docker container for your challenge
# if your service does not require a server, simply delete this file

FROM debian:stretch

RUN useradd --create-home --shell /bin/bash ctf

USER root

# COPY all your files
COPY ynetd impVM rev_flag.txt /home/ctf/

# RUN chmod on all your files
RUN chmod u=rwx,go=rx /home/ctf && \
    chown -R root:root /home/ctf

# EXPOSE all your ports
EXPOSE 1337

# CMD your challenge
CMD ["/home/ctf/ynetd", "-p", "1337", "-u", "ctf", "-d", "/home/ctf", "-lt", "5", "-lm", "16777216", "/home/ctf/impVM -i"]
