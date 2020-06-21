//
// Created by Ajay on 6/20/20.
//

#include "Poly.h"


class BaseClass {
public:
    virtual void method () = 0;
};

class DerivedClass : public BaseClass{
public:
    void method() override { }
};

// Base class as reference argument to f0
void f0(BaseClass& obj) {
    obj.method();
}

// Derived class can also be passed to f0 since DerivedClass is an extension of BaseClass
void f1() {
    DerivedClass obj;
    f0(obj);
}