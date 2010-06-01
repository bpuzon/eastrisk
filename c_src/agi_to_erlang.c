/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 *                                                         *
 *   Asterisk -> Erlang                                    *
 *                                                         *
 * Author: Oscar Hellström <oscar@erlang-consulting.com    *
 * (c) 2010 Erlang Solutions, Ltd                          *
 *                                                         *
 * To be used with Eastrisk.                               *
 * Reads form stdin and passed the information to Eastrisk *
 * via TCP.                                                *
 * Prints whatever it receives from Eastrisk to stdout     *
 *                                                         *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>         /* IPPROTO_TCP */
#include <arpa/inet.h>          /* inet_addr() */
#include <unistd.h>             /* close() */
#include <errno.h>              /* errno */
#include <string.h>


/* Change if you wish to use another port */
#define PORT 6666
#define HOST "127.0.0.1"

int main(void) {
	int sock_fd, length, gotenv = 0;
	struct sockaddr_in address;
	char line[80];

	/* set up stdin/stdout */
	setlinebuf(stdin);
	setlinebuf(stdout);

	/* set up socket and connect */
	address.sin_family = PF_INET;
	address.sin_port = htons(PORT);
	address.sin_addr.s_addr = inet_addr(HOST);
	memset(&(address.sin_zero), '\0', 8);

	if ((sock_fd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) == -1)
        exit(1);

	if (connect(sock_fd, (struct sockaddr *)&address, sizeof(struct sockaddr)) == -1)
        exit(1);

	/* read agi env */
	while (!gotenv) {
		fgets(line, 80, stdin);

		if ('\n' == line[0])
			gotenv = 1;
		else
			send(sock_fd, line, strlen(line), 0);
	}

	send(sock_fd, line, strlen(line), 0);

	/* read commands form Erlang side */
	while(0 < (length = recv(sock_fd, line, 79, 0))) {
		line[length] = '\0';
		fputs(line, stdout);
		fgets(line, 80, stdin);
		send(sock_fd, line, strlen(line) - 1, 0);
	}

	close(sock_fd);

	return 0;
}
