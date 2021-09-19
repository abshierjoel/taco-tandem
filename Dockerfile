FROM node:lts-alpine AS build

COPY . .

RUN npm install

RUN npm run build


## Devserver

FROM php:8.1-rc-apache-buster

COPY --from=build /build/ /var/www/html/