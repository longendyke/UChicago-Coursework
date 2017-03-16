#ifndef COMMUNITY_H
#define COMMUNITY_H

#include <string>
#include <list>
#include <map>
#include "person.h"

using namespace std;

typedef pair<string,Person> contact;

class Community {

  private:

    string name;
    map<string,Person> people;

  public:

    Community();
    Community(string _name, map<string,Person> _people);

    string get_name();
    bool set_name(string _name);

    // add an instance of Person, _person, to the people
    // make sure the username of _person is not used yet
    // make sure the affliation of _person is same as organization of this community
    // the return value indicates whether this adding method is successful or not
    bool add_person(Person _person);

    // return all usernames of members as a list
    list<string> get_all_usernames();
    void print_all_usernames();

    // find users with a certain first name
    list<Person> find_member(string firstname);
    
    // find users within certain age range
    list<Person> find_member(int age_lb, int age_ub);

    // return the person object for a given username
    Person get_member(string username);

    bool username_check(string username);
    
    // send msg to a Person addressed by username
    // make sure the username is validated
    bool send_msg(list<string> usernames, string msg);
    
};

#endif /* COMMUNITY_H */

