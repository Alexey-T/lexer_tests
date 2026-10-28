#include <iostream>
#include <vector>
#include <string>

class Person {
public:
    std::string name;
    int age;

    Person(const std::string& n, int a) : name(n), age(a) {}

    void introduce() const {
        std::cout << "Hello, I'm " << name << " and I'm " << age << " years old." << std::endl;
    }
};

int main() {
    std::vector<Person> people;
    people.emplace_back("Alice", 30);
    people.emplace_back("Bob", 25);

    for (const auto& p : people) {
        p.  // <-- Type here and trigger completion: should suggest 'introduce', 'name', 'age', etc.
    }

    return 0;
}