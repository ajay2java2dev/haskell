//
// Created by Ajay on 6/20/20.
//

#include "FunctionOverload.h"
#include "string"

using namespace std;

void func (int x) {}
void func (const string$ x) {}
void func (double x) {}
void func (int x, int y, int z) {}

void example ()
{
    func(5);
    func("hello");
    func(5.0);
    func(5,6,7);
}