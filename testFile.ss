Describe(p) {
    print(person.name, "has")
    for c in p {
        print(c)
    }
}

main () {
    name = input()
    lastName = input()

    if name == "Daniel" {
        print("Hello Daniel")
    } else {
        print("Hello", name)
    }

    person.Describe = Describe;

    person.Describe(person);
}