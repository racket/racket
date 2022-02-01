/*
  Test server for tls-unique channel binding

  This implements a TLS "server" that accepts a single connection,
  completes the TLS handshake, and then prints the connection's
  tls-unique channel binding information and exits.

  It is adapted from code at https://www.gnutls.org/manual/gnutls.html,
  specifically:
  - 7.2.1 Echo server with X.509 authentication
  - 6.12.8 Channel bindings

  Used by "../test-channel-binding.rkt".
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <string.h>
#include <unistd.h>
#include <gnutls/gnutls.h>
#include <assert.h>

/*
  Note: relative to dynamic current directory, not this source
  file. See "../test-channel-binding.rkt" and files in "../".
*/
#define KEYFILE "server_key.pem"
#define CERTFILE "server_crt.pem"
#define CAFILE "/etc/ssl/certs/ca-certificates.crt"

#define CHECK(x) assert((x)>=0)
#define LOOP_CHECK(rval, cmd) \
        do { \
                rval = cmd; \
        } while(rval == GNUTLS_E_AGAIN || rval == GNUTLS_E_INTERRUPTED)

#define PORT 5556               /* listen to 5556 port */

static void print_channel_binding(gnutls_session_t session);

int main(void)
{
  int listen_sd;
  int sd, ret;
  gnutls_certificate_credentials_t x509_cred;
  gnutls_priority_t priority_cache;
  struct sockaddr_in sa_serv;
  struct sockaddr_in sa_cli;
  socklen_t client_len;
  gnutls_session_t session;
  int optval = 1;

  /* for backwards compatibility with gnutls < 3.3.0 */
  CHECK(gnutls_global_init());
  CHECK(gnutls_certificate_allocate_credentials(&x509_cred));
  CHECK(gnutls_certificate_set_x509_trust_file(x509_cred, CAFILE,
                                               GNUTLS_X509_FMT_PEM));
  CHECK(gnutls_certificate_set_x509_key_file(x509_cred, CERTFILE,
                                             KEYFILE,
                                             GNUTLS_X509_FMT_PEM));
  CHECK(gnutls_priority_init(&priority_cache, NULL, NULL));

  listen_sd = socket(AF_INET, SOCK_STREAM, 0);
  memset(&sa_serv, '\0', sizeof(sa_serv));
  sa_serv.sin_family = AF_INET;
  sa_serv.sin_addr.s_addr = INADDR_ANY;
  sa_serv.sin_port = htons(PORT); /* Server Port number */
  setsockopt(listen_sd, SOL_SOCKET, SO_REUSEADDR, (void *) &optval,
             sizeof(int));
  bind(listen_sd, (struct sockaddr *) &sa_serv, sizeof(sa_serv));
  listen(listen_sd, 1024);

  client_len = sizeof(sa_cli);

  {
    CHECK(gnutls_init(&session, GNUTLS_SERVER));

    /* Channel binding support for TLS 1.3 is currently (v3.7.3) BROKEN; see
     * - https://gitlab.com/gnutls/gnutls/-/issues/1041
     * - https://gitlab.com/gnutls/gnutls/-/merge_requests/1422
     * - https://gitlab.com/gnutls/gnutls/-/merge_requests/1293
     * So disable TLS 1.3 via priority string.
     */
    // CHECK(gnutls_priority_set(session, priority_cache));
    CHECK(gnutls_set_default_priority_append(session, "-VERS-TLS1.3", NULL, 0));

    CHECK(gnutls_credentials_set(session, GNUTLS_CRD_CERTIFICATE,
                                 x509_cred));
    gnutls_certificate_server_set_request(session,
                                          GNUTLS_CERT_IGNORE);
    gnutls_handshake_set_timeout(session,
                                 GNUTLS_DEFAULT_HANDSHAKE_TIMEOUT);

    sd = accept(listen_sd, (struct sockaddr *) &sa_cli,
                &client_len);

    gnutls_transport_set_int(session, sd);

    LOOP_CHECK(ret, gnutls_handshake(session));
    if (ret < 0) {
      close(sd);
      gnutls_deinit(session);
      fprintf(stderr,
              "*** Handshake has failed (%s)\n\n",
              gnutls_strerror(ret));
      exit(-1);
    }

    print_channel_binding(session);
  }

  close(listen_sd);

  gnutls_certificate_free_credentials(x509_cred);
  gnutls_priority_deinit(priority_cache);
  gnutls_global_deinit();
  return 0;
}

static void print_channel_binding(gnutls_session_t session) {
  gnutls_datum_t cb;
  int rc;

  rc = gnutls_session_channel_binding (session,
                                       GNUTLS_CB_TLS_UNIQUE,
                                       &cb);
  if (rc) {
    fprintf (stderr, "Channel binding error: %s\n",
             gnutls_strerror (rc));
  } else {
    size_t i;
    printf ("tls-unique ");
    for (i = 0; i < cb.size; i++)
      printf ("%02x", cb.data[i]);
    printf ("\n");
  }
}
