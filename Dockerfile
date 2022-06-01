
FROM tonyfischetti/risa

WORKDIR /home/marvin

RUN git clone https://github.com/tonyfischetti/mylua-analysis

WORKDIR /home/marvin/mylua-analysis

COPY .env .

RUN npm install && sudo npm install gulp-cli --global

CMD gulp
