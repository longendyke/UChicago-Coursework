#include "person.h"

bool str_isalpha(const string str){
    for(int i = 0; i < str.size(); i++)
    	if((isalpha(str[i]) == 0) || (str[i] == ' '))
    		return false;
    return true;
}

bool str_isalnum(const string s)
{
    std::string::const_iterator it = s.begin();
    while (it != s.end() && std::isdigit(*it)) ++it;
    return !s.empty() && it == s.end();
}

Person::Person() 
  : username(""), firstname(""), lastname(""), gender(""), age(0), tagline("") {
}

Person::Person(string _username, string _firstname, string _lastname, 
               string _gender, int _age, string _tagline) 
  : username(_username), firstname(_firstname), lastname(_lastname),
    gender(_gender), age(_age), tagline(_tagline) {
}

string Person::get_username() {
    return username;
}
string Person::get_firstname() {
    return firstname;
}
string Person::get_lastname() {
    return lastname;
}
string Person::get_gender(){
    return gender;
}
int Person::get_age() {
    return age;
}
string Person::get_tagline() {
    return tagline;
}
string Person::get_info() {
	string ret = "";
    return ret;
}

bool Person::set_username(string _username) {
    // TODO
	// write the function for set_username
	return false;
}

bool Person::set_firstname(string _firstname) {
    // TODO
	if (0/* fill in any necessary tests here */) {
        firstname = _firstname;
        return true;
    }
    else {
        return false;
    }
}


bool Person::set_lastname(string _lastname) {
    // TODO
	if (0/* fill in any necessary tests here */) {
        lastname = _lastname;
        return true;
    }
    else {
        return false;
    }
}

bool Person::set_gender(string _gender){
    //TODO
    return false;
}

bool Person::set_age(int _age) {
	// TODO
    if (0/* fill in any necessary tests here */) {
        age = _age;
        return true;
    }
    else {
        return false;
    }
}
bool Person::set_tagline(string _tagline) {
	// TODO
    if (0/* fill in any necessary tests here */) {
        tagline = _tagline;
        return true;
    }
    else {
        return false;
    }
}


bool Person::set_info(string _username, string _firstname, string _lastname,
                      int _age, string _tagline) {
    // TODO
	// set the information for
	// 1) username
	// 2) firstname
	// 3) lastnam3
	// 4) age
	// 5) tagline
	return false;
}

void Person::send_msg(Person &recipient, string msg) {
	// TODO
	// send message
    }

void Person::get_msg(string msg) {
	// TODO
	// get message
    }

bool Person::read_msg() {
	// TODO
	// print the message if there any message inbox
    return false;

}

