#ifndef REPLIES
#define REPLIES

void send_message(c_server *server, c_user *user, char *reply);

void ping_handler(c_server *server, c_user *user, c_message *message);

void pong_handler(c_server *server, c_user *user, c_message *message);

void motd_handler(c_server *server, c_user *user, c_message *message);

void whois_handler(c_server *server, c_user *user, c_message *message);

void luser_handler(c_server *server, c_user *user, c_message *message);

void nick_handler(c_server *server, c_user *user, c_message *message);

void username_handler(c_server *server, c_user *user, c_message *message);

void quit_handler(c_server *server, c_user *user, c_message *message);

void channel_privmsg(c_server *server, c_user *user, c_message *message);

void privmsg_handler(c_server *server, c_user *user, c_message *message);

void notice_handler(c_server *server, c_user *user, c_message *message);

void join_handler(c_server *server, c_user *user, c_message *message);

void part_handler(c_server *server, c_user *user, c_message *message);

void oper_handler(c_server *server, c_user *user, c_message *message);

#endif