#ifndef PERSON_H
#define PERSON_H

#include <string>
#include <queue>
#include <iostream>
#include <algorithm>

using namespace std;

class Person {

  private:

    string username;
    string firstname;
    string lastname;
    string gender;
    int age;
    string tagline;

    // store received messages
    // ignore classifications
    // when a message is retrieved, delete it immediately
    queue<string> inbox;
  
  public:

    Person();
    Person(string _username, string _firstname, string _lastname, 
           string gender, int _age, string _tagline);

    string get_username();
    string get_firstname();
    string get_lastname();
    string get_gender();
    int get_age();
    string get_tagline();
    string get_info();

    bool set_username(string _username);
    bool set_firstname(string _firstname);
    bool set_lastname(string _lastname);
    bool set_gender(string _gender);
    bool set_age(int _age);
    bool set_tagline(string _tagline);
    bool set_info(string _username, string _firstname, string _lastname,
                  int _age, string _tagline);

    void send_msg(Person &recipient, string msg);
    void get_msg(string msg);
    bool read_msg();
};

bool str_isalpha(const string str);
bool str_isalnum(const string str);

#endif /* COMMUNITY_H */

