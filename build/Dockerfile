FROM fpco/stack-build:lts-10.2

RUN echo "mysql-server mysql-server/root_password password root" \
    | debconf-set-selections \
  && echo "mysql-server mysql-server/root_password_again password root" \
    | debconf-set-selections \
  && apt-get -y update \
  && apt-get -y install mysql-server

RUN wget https://github.com/mattes/migrate/releases/download/v3.0.1/migrate.linux-amd64.tar.gz \
  && tar -xzvf migrate.linux-amd64.tar.gz \
  && mv migrate.linux-amd64 /bin/migrate

RUN ln -s /usr/bin/nodejs /usr/bin/node \
  && npm install pug-cli -g

ADD ./build.sh /bin/

CMD ["build.sh"]