Describe(p) {
    Print(p.name, "has");
    
    Print("AGE:", p.age);
}

modify(p) {
    p.age += 1;
    Print("AGE:", p.age);
}

main() {
    person = Object();
    personB = person;

    person.name = "Daniel";
    person.age = 20;
    person.Describe = Describe;

    Print("before");
    person.Describe(person);
    personB.Describe(personB);

    modify(person);
    Print("after");

    person.Describe(person);
    personB.Describe(personB);

    Print("done");
}