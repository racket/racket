#!/bin/sh

openssl genrsa -des3 -out private-key.pem 1024
openssl rsa -in private-key.pem -out private-key.pem
chmod 400 private-key.pem
openssl req -new -x509 -nodes -sha1 -days 365 -key private-key.pem > server-cert.pem
