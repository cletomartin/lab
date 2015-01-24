// -*- coding:utf-8; mode:cpp


/*
  Playing with some different initializers and basic data.
*/

#include <iostream>

using namespace std;

int
main() {

    bool a = true;
    bool b{false};
    bool c(25);

    // bool d{25}; error - extended initializer prevents narrowing
    bool d = a + c;

    cout << "a: " << a << " b: " << b << endl;
    cout << "c: " << c << " d: "  << d << endl;

    int e = 1234;

    // char f {e}; error - narrowing again!
    char f (e);

    cout << "e: " << e << " f: "  << static_cast<int>(f) << endl;

    // float g {e}; error - truncation!
    float g = e + 0.1;
    double h {g + 1}; // ok! no narrowing!.

    cout << "g: " << g << " h: "  << h << endl;

    return 0;
}
