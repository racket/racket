#!/bin/bash

# This script generates new certificates used for the tests in this
# directory. It overwrites existing certificate files, but it does not
# overwrite existing private key files.

## ------------------------------------------------------------
## Make Root CA

ROOT_CA_CERT_DAYS=3650
ROOT_CA_RSA_BITS=4096

make_root_ca() { # keyfile, certfile, subject
    if [ ! -f "$1" ] ; then
        openssl genrsa -out "$1" "$ROOT_CA_RSA_BITS"
    fi
    openssl req -x509 -new -key "$1" -sha256 -days "$ROOT_CA_CERT_DAYS" -out "$2" \
            -subj "$3" -addext "keyUsage=critical,keyCertSign"
}

## ------------------------------------------------------------
## Make Server

END_CERT_DAYS=3600
END_CERT_RSA_BITS=2048

SERVER_CERT_EXTS="
authorityKeyIdentifier=keyid,issuer
basicConstraints=critical,CA:FALSE
keyUsage=critical,digitalSignature,nonRepudiation,keyEncipherment,dataEncipherment
subjectAltName=@alt_names
[alt_names]
"

make_server() { # 1=keyfile, 2=certfile, 3=subject, 4=altnames, 5=cakeyfile, 6=cacertfile
    if [ ! -f "$1" ] ; then
        openssl genrsa -out "$1" "$END_CERT_RSA_BITS"
    fi
    openssl req -new -key "$1" -out "$2.csr" -subj "$3"
    echo "$SERVER_CERT_EXTS" > "$2.ext"
    echo "$4" >> "$2.ext"
    openssl x509 -req -in "$2.csr" -out "$2" -days "$END_CERT_DAYS" \
            -CAkey "$5" -CA "$6" -CAcreateserial \
            -extfile "$2.ext"
            #-extfile <(echo "$SERVER_CERT_EXTS" ; echo "$4")
    rm "$2.csr" "$2.ext"
}

## ------------------------------------------------------------
## Make Client

CLIENT_CERT_EXTS="
authorityKeyIdentifier=keyid,issuer
basicConstraints=critical,CA:FALSE
keyUsage=critical,digitalSignature,nonRepudiation,keyEncipherment,dataEncipherment
subjectAltName=@alt_names
[alt_names]
"

make_client() { # 1=keyfile, 2=certfile, 3=subject, 4=altnames, 5=cakeyfile, 6=cacertfile
    if [ ! -f "$1" ] ; then
        openssl genrsa -out "$1" "$END_CERT_RSA_BITS"
    fi
    openssl req -new -key "$1" -out "$2.csr" -subj "$3"
    echo "$CLIENT_CERT_EXTS" > "$2.ext"
    echo "$4" >> "$2.ext"
    openssl x509 -req -in "$2.csr" -out "$2" -days "$END_CERT_DAYS" \
            -CAkey "$5" -CA "$6" -CAcreateserial \
            -extfile "$2.ext"
    rm "$2.csr" "$2.ext"
}

## ============================================================

NAME_PREFIX1="/C=US/ST=Racketa/L=Racketville/O=Testing Examples/OU=Testing"
NAME_PREFIX2="/C=US/ST=Racketa/O=Testing Examples/OU=Testing"

make_root_ca "ca_key.pem" "cacert.pem" \
             "$NAME_PREFIX1/CN=example.com/emailAddress=ca@example.com"

make_server "server_key.pem" "server_crt.pem" \
            "$NAME_PREFIX2/CN=server.example.com/emailAddress=server@example.com" \
            "DNS.1=server.example.com" \
            "ca_key.pem" "cacert.pem"

make_server "server_key.pem" "server_crt2.pem" \
            "$NAME_PREFIX1/CN=server2.example.com" \
            "DNS.1=server2.example.com" \
            "ca_key.pem" "cacert.pem"

make_server "server_key.pem" "server_lambda_crt.pem" \
            "$NAME_PREFIX1/CN=lambda" \
            "DNS.1=lambda" \
            "ca_key.pem" "cacert.pem"

make_server "server_ultimate_key.pem" "server_ultimate_crt.pem" \
            "$NAME_PREFIX1/CN=theultimate" \
            "DNS.1=theultimate" \
            "ca_key.pem" "cacert.pem"

CLIENT_ALT_NAMES="
DNS.1=client.example.com
email.1=client@example.com
"

make_client "client_key.pem" "client_crt.pem" \
            "$NAME_PREFIX2/CN=client.example.com/emailAddress=client@example.com" \
            "$CLIENT_ALT_NAMES" \
            "ca_key.pem" "cacert.pem"

# Clean up:
rm "cacert.srl"
