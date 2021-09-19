FROM node:lts-buster AS builderbot

COPY package.json .
COPY package-lock.json .
COPY elm.json .
RUN npm install

COPY . .
CMD ["npm", "run", "build"]

## Devserver

FROM php:7.2-apache
COPY --from=builderbot build/ /var/www/html/

EXPOSE 80
EXPOSE 443