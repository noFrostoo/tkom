Describe(p) {
    Print(p.name, "has");
    
    for c in p {
        Print(c);
        Print(" ");
    }
}

main() {
    name = Input();
    lastName = Input();

    if( name == "Daniel") {
        Print("Hello Daniel");
    } else {
        Print("Hello", name);
    }

    person = Object();

    person.name = name;
    person.Describe = Describe;

    person.Describe(person);
}