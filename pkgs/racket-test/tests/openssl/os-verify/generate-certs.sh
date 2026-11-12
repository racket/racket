#!/bin/bash

set -eu -o pipefail

# This script generates new certificates used for the tests in this
# directory. It overwrites existing certificate files, but it does not
# overwrite existing private key files.

## ------------------------------------------------------------
## Make Root CA

ROOT_CA_CERT_DAYS=3650
ROOT_CA_RSA_BITS=4096
ROOT_CA_CERT_EXTS="
subjectKeyIdentifier=hash
authorityKeyIdentifier=keyid:always,issuer
basicConstraints=critical,CA:true
keyUsage=critical,digitalSignature,cRLSign,keyCertSign
nameConstraints=critical,permitted;DNS:.racket.example.com
"

make_root_ca() { # keyfile, certfile, subject
    keyfile="$1"
    certfile="$2"
    subject="$3"
    echo "$ROOT_CA_CERT_EXTS" > "$certfile.ext"
    if [[ ! -f "$keyfile" ]] ; then
        openssl genrsa -out "$keyfile" "$ROOT_CA_RSA_BITS"
    fi
    openssl req -new -key "$keyfile" -subj "$subject" -out "$certfile.csr"
    openssl x509 -req -in "$certfile.csr" -out "$certfile" \
            -days "$ROOT_CA_CERT_DAYS" -extfile "$certfile.ext" \
            -signkey "$keyfile"
    rm "$certfile.csr" "$certfile.ext"
}

## ------------------------------------------------------------
## Make Intermediate CA

INT_CA_RSA_BITS=2048
INT_CA_CERT_EXTS="
subjectKeyIdentifier=hash
authorityKeyIdentifier=keyid:always,issuer
basicConstraints=critical,CA:true
keyUsage=critical,digitalSignature,cRLSign,keyCertSign
"

make_int_ca() { # keyfile, certfile, subject, certdays, ekus, cakeyfile, cacertfile
    keyfile="$1"
    certfile="$2"
    subject="$3"
    certdays="$4"
    ekus="$5"
    cakeyfile="$6"
    cacertfile="$7"
    if [[ ! -f "$keyfile" ]] ; then
        openssl genrsa -out "$keyfile" "$INT_CA_RSA_BITS"
    fi
    echo "$INT_CA_CERT_EXTS" > "$certfile.ext"
    if [[ ! -z "$ekus" ]] ; then
        echo "extendedKeyUsage=$ekus" >> "$certfile.ext"
    fi
    openssl req -new -key "$keyfile" -out "$certfile.csr" -subj "$subject"
    openssl x509 -req -in "$certfile.csr" -out "$certfile" \
            -days "$certdays" -extfile "$certfile.ext" \
            -CAkey "$cakeyfile" -CA "$cacertfile"
    rm "$certfile.csr" "$certfile.ext"
}

## ------------------------------------------------------------
## Make Server

END_CERT_DAYS=360
END_CERT_RSA_BITS=2048

SERVER_CERT_EXTS="
authorityKeyIdentifier=keyid,issuer
basicConstraints=critical,CA:FALSE
keyUsage=critical,digitalSignature,nonRepudiation,keyEncipherment,dataEncipherment
extendedKeyUsage=serverAuth
subjectAltName=@alt_names
[alt_names]
"

make_server() { # keyfile, certfile, subject, altnames, cakeyfile, cacertfile
    keyfile="$1"
    certfile="$2"
    subject="$3"
    altnames="$4"
    cakeyfile="$5"
    cacertfile="$6"
    if [[ ! -f "$keyfile" ]] ; then
        openssl genrsa -out "$keyfile" "$END_CERT_RSA_BITS"
    fi
    echo "$SERVER_CERT_EXTS" > "$certfile.ext"
    echo "$altnames" >> "$certfile.ext"
    openssl req -new -key "$keyfile" -out "$certfile.csr" -subj "$subject"
    openssl x509 -req -in "$certfile.csr" -out "$certfile" \
            -days "$END_CERT_DAYS" -extfile "$certfile.ext" \
            -CAkey "$cakeyfile" -CA "$cacertfile"
    rm "$certfile.csr" "$certfile.ext"
}

## ============================================================

# Certificate hierarchy (issuer --> subject):
#
#        +-> subA --+-> int1 ---> end1
#        |          |
# root --+          +-----+
#        |                v
#        +-> subB --+-> int2 ---> end2
#                   |
#                   +-> int3 ---> end3
#
# A certificate corresponds to an *edge* from issuer to subject.
# If subA distrusted: end1 invalid, end[23] valid
# If subB distrusted: end[12] valid, end3 invalid

CA_NAME_PREFIX="/C=US/ST=Racketia/L=Racketville/O=TestRKT/OU=Testing"
END_NAME_PREFIX="/C=US/ST=Racketa/O=Testing Examples/OU=Testing"

make_root_ca "root_key.pem" "root_cert.pem" \
             "$CA_NAME_PREFIX/CN=TestRKT Root CA"

make_int_ca "subA_key.pem" "subA_cert.pem" \
            "$CA_NAME_PREFIX/CN=TestRKT Intermediate CA SubA" \
            2000 "serverAuth" "root_key.pem" "root_cert.pem"

make_int_ca "subB_key.pem" "subB_cert.pem" \
            "$CA_NAME_PREFIX/CN=TestRKT Intermediate CA SubB" \
            2000 "serverAuth" "root_key.pem" "root_cert.pem"

make_int_ca "int1_key.pem" "int1_cert.pem" \
            "$CA_NAME_PREFIX/CN=TestRKT Intermediate CA Int1" \
            1000 "serverAuth" "subA_key.pem" "subA_cert.pem"

make_int_ca "int2_key.pem" "int2_cert.pem" \
            "$CA_NAME_PREFIX/CN=TestRKT Intermediate CA Int2" \
            1000 "serverAuth" "subB_key.pem" "subB_cert.pem"

make_int_ca "int3_key.pem" "int3_cert.pem" \
            "$CA_NAME_PREFIX/CN=TestRKT Intermediate CA Int3" \
            1000 "serverAuth" "subB_key.pem" "subB_cert.pem"

make_int_ca "int2_key.pem" "int2_byA_cert.pem" \
            "$CA_NAME_PREFIX/CN=TestRKT Intermediate CA Int2" \
            1000 "serverAuth" "subA_key.pem" "subA_cert.pem"

make_server "end1_key.pem" "end1_cert.pem" \
            "$END_NAME_PREFIX/CN=server1.racket.example.com" \
            "DNS.1=server1.racket.example.com" \
            "int1_key.pem" "int1_cert.pem"

make_server "end2_key.pem" "end2_cert.pem" \
            "$END_NAME_PREFIX/CN=server2.racket.example.com" \
            "DNS.1=server2.racket.example.com" \
            "int2_key.pem" "int2_cert.pem"

make_server "end3_key.pem" "end3_cert.pem" \
            "$END_NAME_PREFIX/CN=server3.racket.example.com" \
            "DNS.1=server3.racket.example.com" \
            "int3_key.pem" "int3_cert.pem"

openssl x509 -in "subA_cert.pem" -addreject "anyExtendedKeyUsage" \
        -trustout -out "distrust_subA_cert.pem"
openssl x509 -in "subB_cert.pem" -addreject "anyExtendedKeyUsage" \
        -trustout -out "distrust_subB_cert.pem"

# ----------------------------------------
# Self-check

set +e +o pipefail

echo "Checking end1 chain"
openssl verify -trusted root_cert.pem \
        -untrusted subA_cert.pem -untrusted int1_cert.pem \
        -purpose sslserver \
        end1_cert.pem

echo "Checking end2 chain"
openssl verify -trusted root_cert.pem \
        -untrusted subB_cert.pem -untrusted int2_cert.pem \
        -purpose sslserver \
        end2_cert.pem

echo "Checking end2-via-subA chain"
openssl verify -trusted root_cert.pem \
        -untrusted subA_cert.pem -untrusted int2_byA_cert.pem \
        -purpose sslserver \
        end2_cert.pem

echo "Checking end3 chain"
openssl verify -trusted root_cert.pem \
        -untrusted subB_cert.pem -untrusted int3_cert.pem \
        -purpose sslserver \
        end3_cert.pem

# If subA is distrusted:
echo "Checking end1 chain (with subA distrusted)"
openssl verify -trusted root_cert.pem -trusted distrust_subA_cert.pem \
        -untrusted subA_cert.pem -untrusted int1_cert.pem \
        -purpose sslserver \
        end1_cert.pem 2> /dev/null
if [[ "$?" == 0 ]] ; then echo "FAIL" ; else echo "PASS" ; fi

echo "Checking end2 chain (with subA distrusted)"
openssl verify -trusted root_cert.pem -trusted distrust_subA_cert.pem \
        -untrusted subB_cert.pem -untrusted int2_cert.pem \
        -purpose sslserver \
        end2_cert.pem

echo "Checking end2-via-subA chain"
openssl verify -trusted root_cert.pem -trusted distrust_subA_cert.pem \
        -untrusted subA_cert.pem -untrusted int2_byA_cert.pem \
        -purpose sslserver \
        end2_cert.pem 2> /dev/null
if [[ "$?" == 0 ]] ; then echo "FAIL" ; else echo "PASS" ; fi

# If subB is distrusted:
echo "Checking end1 chain (with subB distrusted)"
openssl verify -trusted root_cert.pem -trusted distrust_subB_cert.pem \
        -untrusted subA_cert.pem -untrusted int1_cert.pem \
        -purpose sslserver \
        end1_cert.pem

echo "Checking end2 chain (with subB distrusted)"
openssl verify -trusted root_cert.pem -trusted distrust_subB_cert.pem \
        -untrusted subB_cert.pem -untrusted int2_cert.pem \
        -purpose sslserver \
        end2_cert.pem 2> /dev/null
if [[ "$?" == 0 ]] ; then echo "FAIL" ; else echo "PASS" ; fi

echo "Checking end2-via-subA chain (with subB distrusted)"
openssl verify -trusted root_cert.pem -trusted distrust_subB_cert.pem \
        -untrusted subA_cert.pem -untrusted int2_byA_cert.pem \
        -purpose sslserver \
        end2_cert.pem

echo "Checking end3 chain (with subB distrusted)"
openssl verify -trusted root_cert.pem -trusted distrust_subB_cert.pem \
        -untrusted subB_cert.pem -untrusted int3_cert.pem \
        -purpose sslserver \
        end3_cert.pem 2> /dev/null
if [[ "$?" == 0 ]] ; then echo "FAIL" ; else echo "PASS" ; fi
